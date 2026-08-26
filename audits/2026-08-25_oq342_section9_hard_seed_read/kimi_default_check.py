#!/usr/bin/env python3
"""Does kimi-k2.6 reason when sent NO thinking toggle -- i.e. what is its server-side default?

`agent/run_no_scope_kimi.py` `_body/3` has only ever sent {model, messages, max_tokens}. If the
default is thinking-ON, every kimi leg is a thinking-on leg. THREE arms, so the instrument is
witnessed able to distinguish rather than merely producing a number:

  A. NO toggle          -- what every kimi leg was actually generated with (THE QUESTION)
  B. thinking disabled  -- the 2026-08-21 claim, re-witnessed live
  C. thinking enabled   -- positive control: confirms reasoning_content is where we look for it
"""
import json, os, sys, urllib.request

KEY = os.environ.get("MOONSHOT_API_KEY") or os.environ.get("KIMI_API_KEY")
if not KEY:
    sys.exit("no key")
URL = "https://api.moonshot.ai/v1/chat/completions"
MSG = [{"role": "user",
        "content": "A train leaves at 3pm going 60mph; another leaves at 4pm going 80mph on the "
                   "same track. When does the second catch the first? Answer with the time only."}]


def call(extra, label):
    body = {"model": "kimi-k2.6", "messages": MSG, "max_tokens": 2048}
    body.update(extra)
    req = urllib.request.Request(
        URL, data=json.dumps(body).encode(),
        headers={"Authorization": f"Bearer {KEY}", "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=300) as r:
            d = json.load(r)
    except urllib.error.HTTPError as e:
        print(f"{label}: HTTP {e.code} -- {e.read()[:300].decode(errors='replace')}")
        return None
    m = d["choices"][0]["message"]
    reasoning = m.get("reasoning_content") or ""
    content = m.get("content") or ""
    u = d.get("usage", {})
    print(f"{label}")
    print(f"    sent          : {json.dumps(extra) if extra else '(nothing -- bare body)'}")
    print(f"    reasoning_content chars : {len(reasoning)}")
    print(f"    content chars           : {len(content)}")
    print(f"    completion_tokens       : {u.get('completion_tokens')}")
    print(f"    usage keys              : {sorted(u.keys())}")
    if reasoning:
        print(f"    reasoning head          : {reasoning[:110]!r}")
    return {"label": label, "reasoning_chars": len(reasoning),
            "content_chars": len(content), "completion_tokens": u.get("completion_tokens")}


print("=" * 78)
a = call({}, "ARM A -- NO TOGGLE (what every kimi leg was generated with)")
print()
b = call({"thinking": {"type": "disabled"}}, "ARM B -- thinking DISABLED")
print()
c = call({"thinking": {"type": "enabled"}}, "ARM C -- thinking ENABLED (positive control)")
print("=" * 78)

if a and b and c:
    print("\nVERDICT")
    print(f"  no-toggle reasoning chars = {a['reasoning_chars']}")
    print(f"  disabled  reasoning chars = {b['reasoning_chars']}")
    print(f"  enabled   reasoning chars = {c['reasoning_chars']}")
    if c["reasoning_chars"] == 0:
        print("  INSTRUMENT FAILURE: the positive control did not reason either -- this probe "
              "cannot detect reasoning, so ARM A's zero would be uninterpretable.")
    elif a["reasoning_chars"] > 0:
        print("  => k2.6 DEFAULT IS THINKING-ON. Every kimi leg is a thinking-ON leg.")
    else:
        print("  => k2.6 default is thinking-OFF, and arm C shows the probe CAN see reasoning.")
