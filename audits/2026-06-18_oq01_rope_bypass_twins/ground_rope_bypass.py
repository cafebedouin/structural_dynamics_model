"""
OQ-01 grounding: re-run the range sweep (A1 baseline, A2, A3, B3) against the
live-regime TWIN corpora (testsets_haiku, testsets_flash) — not the chimera-era
prolog_v5 the original May-2026 sweep used — to confirm the rope-gate Chi<=0
bypass safe-envelope pattern:

  A3 (sign-flip + compressed ceiling, span 0.85) collapses Jaccard
  B3 (no sign-flip, matched span 0.85)            does NOT collapse

If both twins reproduce the A3-collapse / B3-stable contrast, the boundary note
in logic.md can state the positive safe envelope (sign-consistent Hub weights;
ceiling uncompressed) with cross-corpus grounding.

Variant power_function clauses live in tests/test_battery_variants.pl
(alt_sigmoid_f/3). A1 = default power_function 'sigmoid' (config.pl:165).
"""
import subprocess, json, sys, os, tempfile, time
from pathlib import Path

_here = Path(__file__).resolve()
_root = next(c for c in (_here, *_here.parents) if (c / "pyproject.toml").is_file())
sys.path.insert(0, str(_root / "python"))
from paths import PROLOG_DIR

EVID = _here.parent / "evidence"
EVID.mkdir(parents=True, exist_ok=True)

# (label, power_function variant, sign_flip, span) — A1 is the per-corpus baseline
BASELINE_VARIANT = ("A1", "sigmoid", True, 1.70)
VARIANTS = [
    ("A2", "range_a2", True,  1.20),
    ("A3", "range_a3", True,  0.85),  # sign-flip + compressed ceiling -> expected collapse
    ("B3", "range_b3", False, 0.85),  # no sign-flip, matched span    -> expected stable
]

TWINS = ["testsets_haiku", "testsets_flash"]

OVERLAY = """\
:- use_module(config).
:- ( retract(config:param(power_function, _)) -> true ; true ),
   asserta(config:param(power_function, {variant})).
:- ( retract(config:param(corpus_path, _)) -> true ; true ),
   asserta(config:param(corpus_path, '{corpus}')).
:- [stack].
:- [tests/test_battery_variants].
:- [product_site_export].
:- product_site_export:run_product_export_to('{outpath}'), halt.
"""


def run(variant, corpus, out_json):
    overlay = OVERLAY.format(variant=variant, corpus=corpus, outpath=str(out_json))
    with tempfile.NamedTemporaryFile(mode="w", suffix=".pl", dir=PROLOG_DIR, delete=False) as f:
        f.write(overlay)
        op = f.name
    try:
        cmd = ["swipl", "--stack_limit=4G", "-g", f'["{Path(op).name}"]', "-t", "halt(1)"]
        t0 = time.time()
        r = subprocess.run(cmd, cwd=str(PROLOG_DIR), capture_output=True, text=True, timeout=600)
        dt = time.time() - t0
        for line in r.stderr.strip().splitlines():
            if "[product_export]" in line or "[corpus]" in line:
                print(f"      {line}")
        if r.returncode != 0:
            print(f"    ERROR exit {r.returncode} in {dt:.0f}s")
            print(r.stderr[-1500:])
            return False
        print(f"    done {dt:.0f}s")
        return True
    except subprocess.TimeoutExpired:
        print("    TIMEOUT")
        return False
    finally:
        os.unlink(op)


def presheaf_set(path):
    with open(path) as f:
        d = json.load(f)
    return {c for c, v in d.items() if isinstance(v, dict) and v.get("h0", 0) == 0}


def jaccard(base, var):
    inter = len(base & var); union = len(base | var)
    return (inter / union if union else 0.0, inter, len(var - base), len(base - var))


if __name__ == "__main__":
    summary = {}
    for corpus in TWINS:
        print(f"\n=== {corpus} ===")
        # baseline
        b_out = EVID / f"{corpus}_A1.json"
        if not b_out.exists():
            print(f"  [A1 baseline] {corpus}")
            if not run(BASELINE_VARIANT[1], corpus, b_out):
                summary[corpus] = {"error": "baseline failed"}; continue
        base = presheaf_set(b_out)
        print(f"  A1 baseline N_presheaves={len(base)}")
        rows = {"A1": {"span": 1.70, "flip": True, "N": len(base), "jaccard": 1.0}}
        for label, var, flip, span in VARIANTS:
            out = EVID / f"{corpus}_{label}.json"
            if not out.exists():
                print(f"  [{label} {var}] {corpus}")
                if not run(var, corpus, out):
                    rows[label] = {"error": "run failed"}; continue
            vs = presheaf_set(out)
            jac, inter, sp, ps = jaccard(base, vs)
            rows[label] = {"span": span, "flip": flip, "N": len(vs),
                           "s_to_p": sp, "p_to_s": ps, "jaccard": round(jac, 4)}
            print(f"  {label}: span={span} flip={flip} N={len(vs)} "
                  f"s->p={sp} p->s={ps} Jaccard={jac:.4f}")
        summary[corpus] = rows

    with open(EVID / "summary.json", "w") as f:
        json.dump(summary, f, indent=2)
    print("\n=== SUMMARY ===")
    print(json.dumps(summary, indent=2))
