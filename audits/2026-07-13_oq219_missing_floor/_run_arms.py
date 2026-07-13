"""OQ-219 §6 cold-arm runner. Each arm = one fresh call, payload only, no system prompt."""
import sys, pathlib
ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
AUD = pathlib.Path(__file__).resolve().parent
payload = (AUD / "blind_arm_payload_margins_floor.md").read_text(encoding="utf-8")

def run_sonnet():
    from agent.llm_call import call
    text, tin, tout = call(payload, "claude-sonnet-5", system="", max_tokens=4096)
    (AUD / "blind_arm_sonnet_margins_floor.md").write_text(text, encoding="utf-8")
    print(f"[sonnet] ok  in={tin} out={tout}  -> blind_arm_sonnet_margins_floor.md")

def run_gemini():
    from google import genai
    client = genai.Client()
    resp = client.models.generate_content(model="gemini-2.5-pro", contents=payload)
    text = resp.text or ""
    (AUD / "blind_arm_gemini_margins_floor.md").write_text(text, encoding="utf-8")
    print(f"[gemini] ok  chars={len(text)}  -> blind_arm_gemini_margins_floor.md")

if __name__ == "__main__":
    ok = True
    for name, fn in (("sonnet", run_sonnet), ("gemini", run_gemini)):
        try:
            fn()
        except Exception as e:
            ok = False
            print(f"[{name}] FAILED: {type(e).__name__}: {e}")
    sys.exit(0 if ok else 1)
