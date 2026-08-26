#!/usr/bin/env python3
"""Second instance of the retarget class: does kimi-k2.6 reject a non-default temperature?

`run_no_scope_kimi.py:184` omits `temperature` on the grounds that "kimi-k3 is reasoning-only and
(like Sonnet-5/Opus-4.7+) rejects a non-default sampling temperature" -- a claim about K3, in a
function whose DEFAULT_MODEL is k2.6. Same shape as the thinking-toggle claim, three lines away.

Two arms + a known-positive: if k2.6 ACCEPTS temperature, the kimi legs' temperature is
inherited-not-specified exactly as the thinking regime was.
"""
import json, os, sys, urllib.request
KEY = os.environ.get("MOONSHOT_API_KEY") or os.environ.get("KIMI_API_KEY")
URL = "https://api.moonshot.ai/v1/chat/completions"
MSG = [{"role": "user", "content": "Reply with exactly one word: ping"}]

def call(extra, label):
    body = {"model": "kimi-k2.6", "messages": MSG, "max_tokens": 64}
    body.update(extra)
    req = urllib.request.Request(URL, data=json.dumps(body).encode(),
        headers={"Authorization": f"Bearer {KEY}", "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=180) as r:
            d = json.load(r)
        print(f"{label}\n    sent: {json.dumps(extra)}\n    HTTP 200 -- ACCEPTED. "
              f"content={d['choices'][0]['message'].get('content','')[:40]!r}")
        return True
    except urllib.error.HTTPError as e:
        print(f"{label}\n    sent: {json.dumps(extra)}\n    HTTP {e.code} -- REJECTED: "
              f"{e.read()[:220].decode(errors='replace')}")
        return False

# CORRECTED 2026-08-26, BEFORE the result was reported anywhere. The first draft sent
# temperature 1.0 as "ARM B -- temperature 1.0" and folded it into `a or b` as evidence that
# a NON-DEFAULT temperature is accepted. It is not: the API's own 400 says "only 1 is allowed
# for this model", so 1.0 IS the default and arm B is a known-good CONTROL, not evidence.
# The label was not true of what it measured -- the exact defect class this sweep exists for,
# committed inside the sweep's own instrument.
print("=" * 74)
a = call({"temperature": 0.2}, "ARM A -- temperature 0.2, genuinely NON-DEFAULT (the claim's subject)")
print()
b = call({"temperature": 1.0}, "ARM B -- temperature 1.0 == THE DEFAULT (known-good control, NOT evidence)")
print()
c = call({}, "ARM C -- no temperature at all (what the driver actually sends; control)")
print("=" * 74)
print("\nVERDICT")
if not (b and c):
    print("  INSTRUMENT FAILURE: a known-good control arm failed, so arm A tells us nothing.")
elif a:
    print("  => k2.6 ACCEPTS a non-default temperature: the :184 claim is FALSE of this file's")
    print("     default -- a second CONSEQUENTIAL instance of the retarget class.")
else:
    print("  => k2.6 REJECTS a non-default temperature (only 1 permitted), so the :184 CLAIM")
    print("     HOLDS for k2.6 even though its JUSTIFICATION names K3. Mis-attributed but")
    print("     correct: no capability is foreclosed, so this is NOT a consequential instance.")
