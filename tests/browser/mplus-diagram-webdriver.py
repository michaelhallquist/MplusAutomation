#!/usr/bin/env python3
"""Actual Firefox Desktop acceptance via geckodriver; Python standard library only.

Runs real locally generated R HTML, not a synthetic replacement or patched
Playwright Firefox. All browser settings apply to a new throwaway profile.
"""
import argparse
import base64
import json
from pathlib import Path
import socket
import struct
import subprocess
import tempfile
import time
import urllib.error
import urllib.request
import xml.etree.ElementTree as ET

ELEMENT = "element-6066-11e4-a52e-4f735466cecf"


def unused_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


class Driver:
    def __init__(self, port):
        self.base = "http://127.0.0.1:%s" % port
        self.session = None
        self.pointer_checks = []
        self.config_transport_differences = []
        # Browser proxy isolation must not affect local WebDriver HTTP traffic.
        self.http = urllib.request.build_opener(urllib.request.ProxyHandler({}))

    def request(self, method, endpoint, value=None, session=True):
        if session:
            endpoint = "/session/" + self.session + endpoint
        data = None if value is None else json.dumps(value).encode("utf-8")
        req = urllib.request.Request(self.base + endpoint, data=data, method=method,
                                     headers={"Content-Type": "application/json"})
        try:
            with self.http.open(req, timeout=45) as response:
                result = json.load(response)
        except urllib.error.HTTPError as error:
            result = json.load(error)
        value = result.get("value")
        if isinstance(value, dict) and value.get("error"):
            raise RuntimeError(value["error"] + ": " + value.get("message", ""))
        return value

    def js(self, script, *args, asynchronous=False):
        return self.request("POST", "/execute/" + ("async" if asynchronous else "sync"),
                            {"script": script, "args": list(args)})

    def node(self, selector, widget=0):
        result = self.js("return document.querySelectorAll('.mplus-diagram')[arguments[0]]"
                         ".querySelector(arguments[1]);", widget, selector)
        assert result and ELEMENT in result, "Missing control: " + selector
        return result[ELEMENT]

    def click(self, selector, widget=0):
        return self.request("POST", "/element/" + self.node(selector, widget) + "/click", {})

    def node_point(self, selector=".ld-node", widget=0):
        """Find a hit-tested visible SVG shape center; never synthesize an event.

        Firefox's Element Click endpoint can reject SVG container <g> nodes.
        Scroll their HTML stage, then verify actual geometry and the hit target
        before sending standard WebDriver pointer actions to the painted shape.
        """
        geometry = self.js("""
          const root=document.querySelectorAll('.mplus-diagram')[arguments[0]];
          const node=root.querySelector(arguments[1]);
          if (!node) return {error:'Node is absent'};
          const stage=root.querySelector('.ld-stage');
          stage.scrollIntoView({block:'center',inline:'nearest',behavior:'instant'});
          const shape=node.querySelector('.ld-shape');
          if (!shape) return {error:'Painted node shape is absent'};
          const r=shape.getBoundingClientRect(), clip=stage.getBoundingClientRect();
          const x=Math.round(r.x+r.width/2), y=Math.round(r.y+r.height/2);
          const hit=document.elementFromPoint(x,y);
          const hitNode=hit && hit.closest('.ld-node');
          return {
            id:node.getAttribute('data-node-id'), x, y,
            shape:{x:r.x,y:r.y,width:r.width,height:r.height},
            stage:{x:clip.x,y:clip.y,width:clip.width,height:clip.height},
            viewport:{width:innerWidth,height:innerHeight},
            inViewport:x>=0 && y>=0 && x<innerWidth && y<innerHeight,
            inCanvas:x>=clip.left && x<clip.right && y>=clip.top && y<clip.bottom,
            hitMatches:hitNode===node,
            hit:hit ? {tag:hit.localName,node:hitNode && hitNode.getAttribute('data-node-id')} : null
          };
        """, widget, selector)
        self.pointer_checks.append(geometry)
        assert (not geometry.get("error") and geometry["shape"]["width"] > 0
                and geometry["shape"]["height"] > 0 and geometry["inViewport"]
                and geometry["inCanvas"] and geometry["hitMatches"]), (
                    "SVG node is not visibly hit-testable: " + json.dumps(geometry))
        return geometry

    def click_node(self, selector=".ld-node", widget=0):
        point = self.node_point(selector, widget)
        return self.request("POST", "/actions", {"actions": [{
            "type": "pointer", "id": "mouse", "parameters": {"pointerType": "mouse"},
            "actions": [
                {"type": "pointerMove", "origin": "viewport", "x": point["x"], "y": point["y"], "duration": 0},
                {"type": "pointerDown", "button": 0},
                {"type": "pointerUp", "button": 0},
            ],
        }]})

    def config(self, widget=0):
        result = self.js("""
          const index=arguments[0], done=arguments[arguments.length-1];
          const root=document.querySelectorAll('.mplus-diagram')[index];
          const originalURL=URL.createObjectURL, originalClick=HTMLAnchorElement.prototype.click;
          let blob;
          try {
            URL.createObjectURL=function(b){blob=b;return originalURL(b);};
            HTMLAnchorElement.prototype.click=function(){};
            root.querySelector('[data-action="export-config"]').click();
          } finally {
            URL.createObjectURL=originalURL;HTMLAnchorElement.prototype.click=originalClick;
          }
          blob.text().then(text=>done({ok:true,text:text,value:JSON.parse(text)}))
              .catch(error=>done({ok:false,message:error.message}));
        """, widget, asynchronous=True)
        assert result["ok"], result
        # A config download is JSON text. Preserve its original numbers instead
        # of round-tripping them through WebDriver's intermediate JSON encoder.
        exact = json.loads(result["text"])
        def differences(a, b, path=""):
            if isinstance(a, dict) and isinstance(b, dict) and a.keys() == b.keys():
                return [d for k in a for d in differences(a[k], b[k], path + "/" + k)]
            if a != b:
                return [{"path": path, "blob": a, "webdriver": b}]
            return []
        self.config_transport_differences.extend(differences(exact, result["value"]))
        return exact

    def wait_js(self, script, *args, timeout=15):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            value = self.js(script, *args)
            if value:
                return value
            time.sleep(0.05)
        raise AssertionError("Timed out: " + script)

    def import_file(self, file):
        control = self.node('input[type="file"]')
        self.request("POST", "/element/" + control + "/value", {"text": str(file)})

    def screenshot(self, path):
        path.write_bytes(base64.b64decode(self.request("GET", "/screenshot")))


def wait_download(directory, name):
    deadline = time.monotonic() + 20
    path = directory / name
    while time.monotonic() < deadline:
        if path.is_file() and path.stat().st_size and not (directory / (name + ".part")).exists():
            return path
        time.sleep(0.05)
    raise AssertionError("Download did not complete: " + str(path))


def run_html(driver, firefox, html, artifacts):
    artifacts.mkdir(parents=True, exist_ok=True)
    driver.pointer_checks = []
    driver.config_transport_differences = []
    # Keep the would-be proxy port reserved but non-listening for this session.
    # Every external HTTP(S) request must fail; file: and blob: stay usable.
    proxy_socket = socket.socket()
    proxy_socket.bind(("127.0.0.1", 0))
    proxy_port = proxy_socket.getsockname()[1]
    capabilities = {
        "browserName": "firefox",
        "proxy": {"proxyType": "manual", "httpProxy": "127.0.0.1:%s" % proxy_port,
                  "sslProxy": "127.0.0.1:%s" % proxy_port, "noProxy": []},
        "moz:firefoxOptions": {
            "binary": str(firefox), "args": ["-headless"],
            "prefs": {
                "browser.download.folderList": 2,
                "browser.download.dir": str(artifacts),
                "browser.download.useDownloadDir": True,
                "browser.helperApps.neverAsk.saveToDisk": "application/json,image/svg+xml,image/png,application/octet-stream",
                "browser.download.always_ask_before_handling_new_types": False,
                "browser.download.alwaysOpenPanel": False,
                "network.proxy.no_proxies_on": "",
            },
        },
    }
    try:
        created = driver.request("POST", "/session", {"capabilities": {"alwaysMatch": capabilities}}, session=False)
        driver.session = created["sessionId"]
        version = created["capabilities"]["browserVersion"]
        driver.request("POST", "/timeouts", {"implicit": 0, "pageLoad": 15000, "script": 15000})
        driver.request("POST", "/window/rect", {"width": 1450, "height": 1050})
        try:
            driver.request("POST", "/url", {"url": "https://www.mozilla.org/"})
            raise AssertionError("Offline proxy did not block an external navigation")
        except RuntimeError as error:
            assert "proxyConnectFailure" in str(error), str(error)
            offline_evidence = str(error).split("\n")[0]
        started = time.monotonic()
        driver.request("POST", "/url", {"url": html.as_uri()})
        count = driver.wait_js("return document.querySelectorAll('.mplus-diagram .ld-node').length;")
        render_ms = round((time.monotonic() - started) * 1000)
        widgets = driver.js("return document.querySelectorAll('.mplus-diagram').length;")
        assert widgets >= 2, "Use a real report or prototype with at least two widgets"
        driver.js("window.ldTestErrors=[];window.addEventListener('error',e=>ldTestErrors.push(e.message));"
                  "window.addEventListener('unhandledrejection',e=>ldTestErrors.push(String(e.reason)));")
        baseline = driver.config()
        second = driver.config(1)
        driver.click('[data-action="edit"]')
        position = driver.node_point()
        moves = [{"type": "pointerMove", "origin": "viewport", "x": position["x"], "y": position["y"], "duration": 0},
                 {"type": "pointerDown", "button": 0}]
        for n in range(1, 9):
            moves.append({"type": "pointerMove", "origin": "viewport", "x": position["x"] + n * 5,
                          "y": position["y"] + n * 3, "duration": 30})
        moves.append({"type": "pointerUp", "button": 0})
        driver.request("POST", "/actions", {"actions": [{"type": "pointer", "id": "mouse",
                       "parameters": {"pointerType": "mouse"}, "actions": moves}]})
        moved = driver.config()
        assert moved["state"]["views"]["full"]["nodes"] != baseline["state"]["views"]["full"]["nodes"]
        assert driver.config(1) == second
        driver.click('[data-action="undo"]')
        assert driver.config()["state"]["views"] == baseline["state"]["views"]
        driver.click('[data-action="redo"]')
        assert driver.config()["state"]["views"] == moved["state"]["views"]
        driver.click('select[aria-label="Scale"] option[value="unstandardized"]')
        assert driver.js("return document.querySelectorAll('.mplus-diagram .ld-heading').length;") == 0
        assert driver.config()["state"]["options"]["scale"] == "unstandardized"
        driver.click_node()
        alias = '火狐中文变量 <img src=x onerror="alert(1)">'
        alias_control = driver.node('input[aria-label="Display name"]')
        driver.request("POST", "/element/" + alias_control + "/clear", {})
        driver.request("POST", "/element/" + alias_control + "/value", {"text": alias})
        driver.click('[data-action="alias"]')
        assert "火狐中文变量" in driver.js("return document.querySelector('.mplus-diagram .ld-node').textContent;")
        assert driver.js("return document.querySelectorAll('.mplus-diagram img').length;") == 0
        edited = driver.config()
        assert driver.config(1) == second
        driver.click('[data-action="export-config"]')
        config_file = wait_download(artifacts, "model.diagram.json")
        (artifacts / "expected-edited.json").write_text(json.dumps(edited, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
        assert json.loads(config_file.read_text()) == edited, "Downloaded JSON differs from the browser's exported Blob"
        restore_file = artifacts / "baseline.json"
        restore_file.write_text(json.dumps(baseline), encoding="utf-8")
        driver.import_file(restore_file)
        driver.wait_js("return document.querySelector('.mplus-diagram [role=status]').textContent.includes('Configuration imported');")
        assert driver.config() == baseline
        driver.import_file(config_file)
        driver.wait_js("return document.querySelector('.mplus-diagram .ld-node').textContent.includes('火狐中文变量');")
        assert driver.config() == edited
        mismatch = dict(edited)
        mismatch["fingerprint"] = "wrong-model"
        mismatch_file = artifacts / "mismatch.json"
        mismatch_file.write_text(json.dumps(mismatch), encoding="utf-8")
        driver.import_file(mismatch_file)
        driver.wait_js("return document.querySelector('.mplus-diagram [role=status]').textContent.includes('Import rejected');")
        assert driver.config() == edited
        driver.click('select[aria-label="View"] option[value="simplified"]')
        assert driver.config()["state"]["views"] == edited["state"]["views"]
        driver.click('select[aria-label="View"] option[value="full"]')
        driver.click('[data-action="zoom-in"]')
        driver.click('[data-action="export-svg"]')
        svg_file = wait_download(artifacts, "model-diagram.svg")
        svg = svg_file.read_text(encoding="utf-8")
        parsed = ET.fromstring(svg)
        assert "火狐中文变量" in svg and "<marker" in svg
        assert "ld-toolbar" not in svg and "data-editor-only" not in svg
        assert parsed.attrib["width"] and parsed.attrib["height"]
        assert "ld-heading" not in svg and "Double arrows: covariance" not in svg and "Display projection only" not in svg
        driver.click('[data-action="export-png"]')
        png_file = wait_download(artifacts, "model-diagram.png")
        png = png_file.read_bytes()
        assert png[:8] == b"\x89PNG\r\n\x1a\n"
        png_width, png_height = struct.unpack(">II", png[16:24])
        assert png_width > 500 and png_height > 300
        assert driver.js("return window.ldTestErrors;") == []
        driver.screenshot(artifacts / "edited-report.png")
        driver.request("POST", "/refresh", {})
        driver.wait_js("return document.querySelectorAll('.mplus-diagram .ld-node').length;")
        assert driver.config() == baseline
        assert driver.config(1) == second
        # Open the real exported documents as standalone resources in Firefox.
        driver.request("POST", "/url", {"url": svg_file.as_uri()})
        assert driver.js("return document.documentElement.localName;") == "svg"
        assert "火狐中文变量" in driver.js("return document.documentElement.textContent;")
        driver.screenshot(artifacts / "standalone-svg.png")
        driver.request("POST", "/url", {"url": png_file.as_uri()})
        driver.wait_js("return document.images.length && document.images[0].complete && document.images[0].naturalWidth>0;")
        assert driver.js("return document.images[0].naturalWidth;") == png_width
        result = {"html": str(html), "browser": "Firefox Desktop (official release, headless)",
                  "version": version, "geckodriver": created["capabilities"].get("moz:geckodriverVersion"),
                  "widgets": widgets, "renderedNodesAcrossPage": count, "localNavigationAndRenderMs": render_ms,
                  "networkBlockedEvidence": offline_evidence, "pngWidth": png_width, "pngHeight": png_height,
                  "pointerHitChecks": driver.pointer_checks,
                  "configTransportDifferences": driver.config_transport_differences,
                  "artifacts": str(artifacts), "status": "PASS"}
        print(json.dumps(result, ensure_ascii=False), flush=True)
        return result
    except Exception as error:
        (artifacts / "failure.json").write_text(json.dumps({
            "error": str(error), "pointerHitChecks": driver.pointer_checks,
            "configTransportDifferences": driver.config_transport_differences,
        }, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
        if driver.session:
            try:
                driver.screenshot(artifacts / "failure.png")
            except Exception:
                pass  # Keep the original failure if the browser already exited.
        raise
    finally:
        if driver.session:
            try:
                driver.request("DELETE", "")
            finally:
                driver.session = None
        proxy_socket.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--firefox", type=Path, required=True)
    parser.add_argument("--geckodriver", type=Path, required=True)
    parser.add_argument("--html", type=Path, nargs="+", required=True)
    parser.add_argument("--artifacts", type=Path)
    args = parser.parse_args()
    firefox = args.firefox.resolve(strict=True)
    gecko = args.geckodriver.resolve(strict=True)
    html_files = [p.resolve(strict=True) for p in args.html]
    artifacts = (args.artifacts or Path(tempfile.mkdtemp(prefix="lin-firefox-test-"))).resolve()
    if artifacts.exists() and any(artifacts.iterdir()):
        parser.error("--artifacts must be empty, to prevent accepting stale downloads")
    artifacts.mkdir(parents=True, exist_ok=True)
    profiles = artifacts / "profiles"
    profiles.mkdir(exist_ok=True)
    port = unused_port()
    driver = Driver(port)
    with (artifacts / "geckodriver.log").open("wb") as log:
        process = subprocess.Popen([str(gecko), "--host", "127.0.0.1", "--port", str(port), "--profile-root", str(profiles)],
                                   stdout=log, stderr=subprocess.STDOUT)
        try:
            deadline = time.monotonic() + 15
            while True:
                try:
                    driver.request("GET", "/status", session=False)
                    break
                except urllib.error.URLError:
                    if process.poll() is not None or time.monotonic() >= deadline:
                        raise RuntimeError("geckodriver did not start; inspect " + str(artifacts / "geckodriver.log"))
                    time.sleep(0.1)
            results = [run_html(driver, firefox, html, artifacts / (str(i + 1) + "-" + html.stem))
                       for i, html in enumerate(html_files)]
            (artifacts / "results.json").write_text(json.dumps(results, ensure_ascii=False, indent=2) + "\n")
        finally:
            process.terminate()
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()


if __name__ == "__main__":
    main()
