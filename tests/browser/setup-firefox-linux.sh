#!/usr/bin/env bash
# Temporary official-release install only. No sudo, package manager, PATH edits,
# user-profile edits, browser registration, or sandbox-disabling flags.
set -euo pipefail

if [[ $# -ne 1 ]]; then
  printf 'Usage: bash %s NEW_EMPTY_TARGET_DIRECTORY\n' "$0" >&2
  exit 2
fi
if [[ "$(uname -s)" != Linux || "$(uname -m)" != x86_64 ]]; then
  printf 'This installer requires Linux x86_64.\n' >&2
  exit 2
fi
for executable in curl python3 ldd; do
  command -v "$executable" >/dev/null || { printf 'Missing prerequisite: %s\n' "$executable" >&2; exit 2; }
done
install_dir=$(python3 - "$1" <<'PY'
import pathlib, sys, tarfile
if not hasattr(tarfile, 'data_filter'):
    raise SystemExit('Python with tarfile.data_filter is required (use Python 3.12+).')
target = pathlib.Path(sys.argv[1]).expanduser().resolve()
if target.exists() and (not target.is_dir() or any(target.iterdir())):
    raise SystemExit('Target must be a new or empty directory: ' + str(target))
target.mkdir(parents=True, exist_ok=True)
print(target)
PY
)
mkdir -p "$install_dir/downloads" "$install_dir/bin"

# Pinned official artifacts, verified reachable 2026-10-14. No third-party mirror.
firefox_url='https://archive.mozilla.org/pub/firefox/releases/158.0/linux-x86_64/en-US/firefox-158.0.tar.xz'
gecko_url='https://github.com/mozilla/geckodriver/releases/download/v0.37.1/geckodriver-v0.37.1-linux64.tar.gz'
for archive in firefox geckodriver; do
  if [[ "$archive" == firefox ]]; then url=$firefox_url; suffix=tar.xz; else url=$gecko_url; suffix=tar.gz; fi
  curl --fail --show-error --silent --location --proto '=https' --proto-redir '=https' \
    --connect-timeout 20 --max-time 300 --retry 3 \
    "$url" --output "$install_dir/downloads/$archive.$suffix"
done

python3 - "$install_dir" <<'PY'
import hashlib, pathlib, tarfile, sys
root = pathlib.Path(sys.argv[1])
# Firefox: official release SHA256SUMS; driver: Mozilla GitHub release asset digest.
# https://archive.mozilla.org/pub/firefox/releases/158.0/SHA256SUMS
# https://api.github.com/repos/mozilla/geckodriver/releases/tags/v0.37.1
artifacts = [
    ('firefox.tar.xz', '9549b054ec4cf85f50f6a19d8c6fb79580312e71ebc4ec2cb217ec9af09dc12a', root, 'firefox'),
    ('geckodriver.tar.gz', 'e815130ea95983e162ae91843b48d3a3ce991735635fce83a647afde21e09f7e', root / 'bin', 'geckodriver'),
]
for filename, expected, destination, top in artifacts:
    archive = root / 'downloads' / filename
    digest = hashlib.sha256(archive.read_bytes()).hexdigest()
    if digest != expected:
        raise SystemExit('SHA256 mismatch; archive was not extracted: ' + filename)
    with tarfile.open(archive, 'r:*') as source:
        members = source.getmembers()
        if not members:
            raise SystemExit('Empty archive: ' + filename)
        for member in members:
            name = pathlib.PurePosixPath(member.name)
            if name.is_absolute() or '..' in name.parts or not name.parts or name.parts[0] != top:
                raise SystemExit('Unexpected archive path: ' + member.name)
            if not (member.isfile() or member.isdir() or member.issym() or member.islnk()):
                raise SystemExit('Unsupported archive member: ' + member.name)
        # The stdlib data filter also rejects links escaping the destination,
        # strips special permission bits, and does not restore archive ownership.
        source.extractall(destination, members=members, filter='data')
PY

# Check runtime linkage without installing or altering system packages.
for library in firefox libxul.so libmozgtk.so; do
  dependency_report=$(LD_LIBRARY_PATH="$install_dir/firefox${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    ldd "$install_dir/firefox/$library")
  if [[ "$dependency_report" == *'not found'* ]]; then
    printf 'Missing Firefox dependencies for %s:\n%s\n' "$library" "$dependency_report" >&2
    printf 'Provide the documented Linux GTK/GLib/X.Org runtime libraries in the runner; this script does not install system packages.\n' >&2
    exit 1
  fi
done

python3 - "$install_dir" <<'PY'
import pathlib, subprocess, sys
root = pathlib.Path(sys.argv[1])
firefox = subprocess.check_output([str(root / 'firefox/firefox'), '--version'], text=True).strip()
gecko = subprocess.check_output([str(root / 'bin/geckodriver'), '--version'], text=True).splitlines()[0]
if firefox != 'Mozilla Firefox 158.0':
    raise SystemExit('Unexpected Firefox version: ' + firefox)
if not gecko.startswith('geckodriver 0.37.1 (') or not gecko.endswith(')'):
    raise SystemExit('Unexpected geckodriver version: ' + gecko)
print(firefox)
print(gecko)
print('Firefox executable: ' + str(root / 'firefox/firefox'))
print('WebDriver executable: ' + str(root / 'bin/geckodriver'))
PY
