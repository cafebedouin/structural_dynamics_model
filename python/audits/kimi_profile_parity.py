#!/usr/bin/env python3
"""Thinking-parity re-run — within-model reasoning manipulation (2026-07-21).

Closes the battery's regime confound ("kimi's stance sharpness might just be its reasoning tokens").
kimi-k2.6 reasoning cannot be cleanly disabled (enable_thinking:false only ~3x reduces it), so
instead of cross-model parity we manipulate reasoning WITHIN each model and watch the stance move:

  kimi-k2.6:   baseline (heavy)  vs  enable_thinking=false (reduced ~3x)
  gemini-2.5-flash:  thinkingBudget=0 (off)  vs  thinkingBudget=8192 (max)

Probes: P2 memetic_mirror, P3 perspective_diff (the two that differentiated in the battery; P1
folded for everyone). Same prompts as kimi_profile_battery.py.

Decisive read: if kimi stays sharp at REDUCED reasoning AND gemini stays thin at MAX reasoning,
the sharpness/thinness are intrinsic — the structural-vs-stance dissociation is not a thinking
artifact. Analyst: Claude (interpretive).

Writes: battery/parity_responses.json + per-condition .txt
"""
import json
import os
import sys
from pathlib import Path

import requests

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # repo root, so `python.audits` imports
from python.audits.kimi_profile_battery import PROBES, BATTERY, SYSTEM

PROBE_IDS = ["P2_memetic_mirror", "P3_perspective_diff"]


def kimi(prompt, reduced):
    key = os.environ.get("MOONSHOT_API_KEY") or os.environ.get("KIMI_API_KEY")
    base = os.environ.get("MOONSHOT_BASE_URL", "https://api.moonshot.ai/v1")
    body = {"model": "kimi-k2.6", "max_tokens": 16000,
            "messages": [{"role": "system", "content": SYSTEM},
                         {"role": "user", "content": prompt}]}
    if reduced:
        body["enable_thinking"] = False
    r = requests.post(f"{base}/chat/completions",
                      headers={"Authorization": f"Bearer {key}", "Content-Type": "application/json"},
                      json=body, timeout=1200)
    r.raise_for_status()
    b = r.json(); u = b.get("usage") or {}
    reason = (u.get("completion_tokens_details") or {}).get("reasoning_tokens")
    return b["choices"][0]["message"].get("content") or "", u.get("completion_tokens", 0), reason


def gemini(prompt, budget):
    key = os.environ.get("GEMINI_API_KEY") or os.environ.get("GOOGLE_API_KEY")
    url = ("https://generativelanguage.googleapis.com/v1beta/models/"
           f"gemini-2.5-flash:generateContent?key={key}")
    body = {"contents": [{"role": "user", "parts": [{"text": prompt}]}],
            "systemInstruction": {"parts": [{"text": SYSTEM}]},
            "generationConfig": {"maxOutputTokens": 16000, "temperature": 0.2,
                                 "thinkingConfig": {"thinkingBudget": budget,
                                                    "includeThoughts": False}}}
    r = requests.post(url, json=body, timeout=300)
    r.raise_for_status()
    d = r.json()
    cand = (d.get("candidates") or [{}])[0]
    parts = (cand.get("content") or {}).get("parts") or []
    text = "".join(p.get("text", "") for p in parts)
    um = d.get("usageMetadata") or {}
    return text, um.get("candidatesTokenCount", 0), um.get("thoughtsTokenCount")


CONDITIONS = [
    ("kimi-k2.6", "heavy", lambda p: kimi(p, reduced=False)),
    ("kimi-k2.6", "reduced", lambda p: kimi(p, reduced=True)),
    ("gemini-2.5-flash", "think_off", lambda p: gemini(p, budget=0)),
    ("gemini-2.5-flash", "think_max", lambda p: gemini(p, budget=8192)),
]


def main():
    results = []
    for probe_id in PROBE_IDS:
        prompt = PROBES[probe_id]
        for model, cond, fn in CONDITIONS:
            tag = f"{model}/{cond}"
            print(f"[{probe_id} / {tag}] calling...", flush=True)
            try:
                text, out, reason = fn(prompt)
                print(f"  OK out={out} reasoning={reason} chars={len(text)}", flush=True)
                ok = bool(text.strip())
            except Exception as e:
                text, out, reason, ok = f"<ERROR: {type(e).__name__}: {e}>", 0, None, False
                print(f"  FAIL {e}", flush=True)
            results.append({"probe": probe_id, "model": model, "condition": cond,
                            "out_tok": out, "reasoning_tok": reason, "ok": ok, "response": text})
            safe = f"parity__{probe_id}__{model}__{cond}.txt"
            (BATTERY / safe).write_text(
                f"# {probe_id} / {tag}  (out={out} reasoning={reason})\n\n"
                f"## RESPONSE\n{text}\n", encoding="utf-8")
    (BATTERY / "parity_responses.json").write_text(
        json.dumps(results, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"\nwrote {BATTERY / 'parity_responses.json'} ({len(results)} responses)", flush=True)


if __name__ == "__main__":
    main()
