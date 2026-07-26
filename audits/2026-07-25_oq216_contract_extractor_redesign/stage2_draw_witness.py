"""OQ-216 witness set: fresh stage-2 draws through the redesigned extractor.

Replays the production _run_stage_2 path (cached stage_1_anon + stage-0
contracts from the run dir, same OQ-219 clause injection) without running
the rest of the pipeline. 3x prometheus + 1x quellcrist per the operator's
verdict (2026-07-25). Each draw is saved; each goes through
_extract_invariant_contract_checked and the verdict is printed.
"""
import sys, pathlib, time

REPO = pathlib.Path("/home/scott/bin/structural_dynamics_model")
sys.path.insert(0, str(REPO))
from agent.uke_narrative_orchestrator import (  # noqa: E402
    UKEOrchestrator, _extract_invariant_contract_checked,
)

OUT = pathlib.Path(__file__).parent / "draws"
OUT.mkdir(exist_ok=True)
UKE = REPO / "agent" / "narrative_transform" / "uke"

RUNS = [
    ("prometheus", UKE / "prometheus_1785030750", 3),
    ("quellcrist", UKE / "quellcrist_1784034874", 1),
]


def main():
    orch = UKEOrchestrator(mode="narrative", skip_engine=True)
    results = []
    for name, run_dir, n_draws in RUNS:
        stage_1_anon = (run_dir / "stage_1_anon_output.md").read_text()
        contract0 = (run_dir / "invariant_contract_stage0_output.md").read_text()
        break0_path = run_dir / "break_contract_output.md"
        break0 = break0_path.read_text() if break0_path.exists() else ""
        for i in range(1, n_draws + 1):
            tag = f"{name}_draw{i}"
            print(f"\n=== {tag} ===")
            t0 = time.time()
            step = orch._run_stage_2(stage_1_anon, contract0, break0)
            dt = time.time() - t0
            if step.status != "success":
                print(f"  API ERROR: {step.error}")
                results.append((tag, "api_error", 0, 0, step.error))
                continue
            out_path = OUT / f"{tag}_stage_2_output.md"
            out_path.write_text(step.data)
            block, err = _extract_invariant_contract_checked(step.data)
            verdict = "PASS" if not err else f"GUARD-FAIL: {err}"
            print(f"  tokens_out={step.tokens_out}  {dt:.0f}s  "
                  f"block={len(block)}ch  -> {verdict}")
            results.append((tag, verdict, step.tokens_out, len(block), err))

    print("\n" + "=" * 60)
    print("SUMMARY")
    for tag, verdict, tout, blen, err in results:
        print(f"  {tag}: tokens_out={tout} block={blen}ch  {verdict}")


if __name__ == "__main__":
    main()
