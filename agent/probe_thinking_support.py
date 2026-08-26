#!/usr/bin/env python3
"""Witness a model's reasoning-toggle support, for the `_THINKING_WITNESSED` whitelist.

WHY THIS IS A COMMITTED SCRIPT AND NOT A RECIPE. The whitelist in `run_no_scope_kimi.py` is a
set of model-keyed capability claims -- the same SHAPE as the `mandatory` comment that cost this
project an experiment that never ran (KNOWN_STATE 2026-08-26). It fails safe (an unlisted model
omits the toggle rather than receiving an untested parameter), so it cannot mislabel a leg. The
way it goes stale instead is that someone ADDS a model without witnessing, because witnessing
would mean reconstructing a three-arm probe from memory. With this script, adding a model is
"run it, paste the printed line", and every whitelist entry carries its own evidence.

THE ARM LABELS ARE TRANSCRIBED FROM THE REQUEST BODY, NOT FROM INTENT. That rule is here because
it was violated twice in one session, both times by an arm whose label asserted what it was FOR
rather than what it SENT -- once calling a default temperature "non-default" and reading
acceptance from it. So every arm prints the exact JSON body it transmitted, and the verdict is
computed from the responses, never from the arm's name.

Usage:
    KIMI_API_KEY=... python3 agent/probe_thinking_support.py kimi-k2.6
    OPENROUTER_API_KEY=... python3 agent/probe_thinking_support.py <model> --api openrouter
"""
import argparse, json, os, sys, urllib.error, urllib.request

APIS = {
    "moonshot":   ("https://api.moonshot.ai/v1/chat/completions", ("MOONSHOT_API_KEY", "KIMI_API_KEY")),
    "openrouter": ("https://openrouter.ai/api/v1/chat/completions", ("OPENROUTER_API_KEY",)),
}
PROMPT = [{"role": "user", "content":
           "A train leaves at 3pm at 60mph; another leaves at 4pm at 80mph on the same track. "
           "When does the second catch the first? Answer with the time only."}]


def call(url, key, body, label):
    """Returns (reasoning_chars, content_chars, completion_tokens, http_ok). Prints the BODY."""
    print(f"\n--- {label}")
    print(f"    REQUEST BODY (transcribed, not described): {json.dumps(body, sort_keys=True)}")
    req = urllib.request.Request(url, data=json.dumps(body).encode(),
                                 headers={"Authorization": f"Bearer {key}",
                                          "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=300) as r:
            d = json.load(r)
    except urllib.error.HTTPError as e:
        print(f"    HTTP {e.code} REJECTED: {e.read()[:240].decode(errors='replace')}")
        return None
    m = d["choices"][0]["message"]
    rc = len(m.get("reasoning_content") or "")
    cc = len(m.get("content") or "")
    ct = (d.get("usage") or {}).get("completion_tokens")
    print(f"    HTTP 200  reasoning_content={rc} chars  content={cc} chars  completion_tokens={ct}")
    return {"reasoning": rc, "content": cc, "tokens": ct}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("model")
    ap.add_argument("--api", choices=sorted(APIS), default="moonshot")
    ap.add_argument("--max-tokens", type=int, default=2048)
    a = ap.parse_args()

    url, envs = APIS[a.api]
    key = next((os.environ[e] for e in envs if os.environ.get(e)), None)
    if not key:
        sys.exit(f"no key: set one of {', '.join(envs)}")

    base = {"model": a.model, "messages": PROMPT, "max_tokens": a.max_tokens}
    print(f"=== probing {a.model} via {a.api} ===")
    # THREE arms. C is the positive control: without it, B's zero cannot be told apart from a
    # probe that simply cannot see reasoning on this API shape.
    arm_a = call(url, key, dict(base), "ARM A -- NO TOGGLE (what an unlisted model would get)")
    arm_b = call(url, key, dict(base, thinking={"type": "disabled"}), "ARM B -- thinking DISABLED")
    arm_c = call(url, key, dict(base, thinking={"type": "enabled"}), "ARM C -- thinking ENABLED (positive control)")

    print("\n" + "=" * 74)
    if arm_c is None or arm_c["reasoning"] == 0:
        print("VERDICT: INSTRUMENT FAILURE — the positive control did not reason (or was rejected),")
        print("  so arm B's result is uninterpretable. Do NOT add this model to the whitelist.")
        sys.exit(1)
    if arm_b is None:
        print(f"VERDICT: {a.model} REJECTS an explicit toggle. Do NOT whitelist it — an unlisted")
        print("  model omits the toggle, which is the correct behaviour here.")
        sys.exit(0)
    default_on = arm_a is not None and arm_a["reasoning"] > 0
    print(f"VERDICT: toggle SUPPORTED. Server-side default is "
          f"thinking-{'ON' if default_on else 'OFF'} "
          f"(arm A reasoning={arm_a['reasoning'] if arm_a else 'n/a'}).")
    print("\nPaste into `_THINKING_WITNESSED` in agent/run_no_scope_kimi.py:\n")
    print(f'    # {a.model}: no toggle -> {arm_a["reasoning"] if arm_a else "n/a"} reasoning chars; '
          f'disabled -> {arm_b["reasoning"]}; enabled -> {arm_c["reasoning"]}.')
    print(f'    # Two-sided with a positive control, witnessed via agent/probe_thinking_support.py.')
    print(f'    "{a.model}": "enabled",')


if __name__ == "__main__":
    main()
