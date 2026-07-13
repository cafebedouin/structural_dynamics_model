"""OQ-219 Datum Stone §6 — THREE cold arms. Each = one fresh call, payload only, no system prompt."""
import sys, pathlib
ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
AUD = pathlib.Path(__file__).resolve().parent
payload = (AUD / "blind_arm_payload_datum_stone.md").read_text(encoding="utf-8")

def anthropic_call(model, extra):
    import anthropic
    c = anthropic.Anthropic()
    kw = dict(model=model, max_tokens=4096,
              messages=[{"role": "user", "content": payload}])
    kw.update(extra)
    r = c.messages.create(**kw)
    return "".join(b.text for b in r.content if getattr(b, "type", "") == "text")

def run_sonnet():
    t = anthropic_call("claude-sonnet-5", {"thinking": {"type": "disabled"}})
    (AUD / "blind_arm_sonnet_datum_stone.md").write_text(t, encoding="utf-8")
    print(f"[sonnet] ok chars={len(t)}")

def run_haiku():
    t = anthropic_call("claude-haiku-4-5-20251001", {})
    (AUD / "blind_arm_haiku_datum_stone.md").write_text(t, encoding="utf-8")
    print(f"[haiku] ok chars={len(t)}")

def run_gemini():
    from google import genai
    r = genai.Client().models.generate_content(model="gemini-2.5-pro", contents=payload)
    t = r.text or ""
    (AUD / "blind_arm_gemini_datum_stone.md").write_text(t, encoding="utf-8")
    print(f"[gemini] ok chars={len(t)}")

if __name__ == "__main__":
    ok = True
    for name, fn in (("sonnet", run_sonnet), ("gemini", run_gemini), ("haiku", run_haiku)):
        try:
            fn()
        except Exception as e:
            ok = False
            print(f"[{name}] FAILED: {type(e).__name__}: {e}")
    sys.exit(0 if ok else 1)
