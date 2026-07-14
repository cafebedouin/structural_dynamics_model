"""Standing positive control for the OQ-214 theme meter (_theme_inventory).

The introduced instrument is itself a claim (CLAUDE.md): a meter that
extracts theme-naming candidates needs a permanent fixture proving it still
fires on the shapes it was built to catch AND that it never arms the gate on
a merit-correlated kind. Precedent: python/tests/test_stage2_dominance_gate.py.

The load-bearing assertion is BUCKET INVARIANT: refrain (and every other
adjudication-only kind) must NOT contribute to density_per_1000. A future
optimizer that "improves determinism" by folding refrain into the gate would
turn the meter into a craft-suppressor (rift3's institutional creed = craft);
this test fails loudly if that happens.

Run: python3 python/tests/test_theme_inventory.py   (exit 0 = all pass)
"""
import sys
import pathlib

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[2]))
from agent.uke_narrative_orchestrator import (  # noqa: E402
    _theme_inventory, _THEME_DENSITY_KINDS, THEME_DENSITY_THRESHOLD,
)

# Consecutive sentence-initial repetition (density-bearing: anaphora).
ANAPHORA = (
    "They do not tell us the truth. They do not tell us the cost. "
    "They do not tell us the way it ends."
)
# Stacked + drawn-out because/therefore syllogism (density-bearing: causal_chain).
CAUSAL = (
    "He stayed because the rent came due, and because the children could "
    "not eat otherwise, therefore he did not sleep.\n\n"
    "The gate held because the seal held. The seal held because the "
    "pressure stayed. The pressure stayed because no one opened the vent."
)
# A normalized sentence recurring (adjudication-only: refrain). The
# recurrence is NON-consecutive on purpose: back-to-back identical sentences
# would legitimately ALSO register as anaphora (they share their initial
# phrase). Here the refrain is interleaved with unrelated sentences that
# share no 3-word prefix with it, so ONLY the refrain kind fires — any
# density it produced would be a bucket leak.
REFRAIN = (
    "The keeping became accounting. Winter arrived in the valley. "
    "The keeping became accounting. High water marked that spring. "
    "The keeping became accounting. Snow buried the north field."
)


def check(name: str, cond: bool, detail: str = "") -> bool:
    print(f"  {'PASS' if cond else 'FAIL'}  {name}"
          + (f"  ({detail})" if detail else ""))
    return cond


def main() -> int:
    fails = []

    # 1. dict shape
    inv = _theme_inventory(ANAPHORA)
    shape_keys = {"word_count", "counts", "density_per_1000", "density_kinds",
                  "threshold", "caveat", "groupings", "entries"}
    if not check("dict shape has all expected keys",
                 shape_keys <= set(inv), f"missing {shape_keys - set(inv)}"):
        fails.append("shape")
    kinds = {"anaphora", "causal_chain", "refrain", "aphorism",
             "resonant_closer", "word_arithmetic"}
    if not check("counts has all six kinds", kinds <= set(inv["counts"])):
        fails.append("counts-keys")
    if not check("density_kinds == the two density-bearing kinds",
                 tuple(inv["density_kinds"]) == _THEME_DENSITY_KINDS,
                 str(inv["density_kinds"])):
        fails.append("density-kinds")
    if not check("threshold surfaced == module constant",
                 inv["threshold"] == THEME_DENSITY_THRESHOLD):
        fails.append("threshold")

    # 2. anaphora is caught AND is density-bearing
    if not check("anaphora x3 caught", inv["counts"]["anaphora"] >= 3,
                 f"count={inv['counts']['anaphora']}"):
        fails.append("anaphora-catch")
    if not check("anaphora moves density", inv["density_per_1000"] > 0):
        fails.append("anaphora-density")

    # 3. causal_chain is caught AND is density-bearing
    cinv = _theme_inventory(CAUSAL)
    if not check("causal_chain caught", cinv["counts"]["causal_chain"] >= 2,
                 f"count={cinv['counts']['causal_chain']}"):
        fails.append("causal-catch")
    if not check("causal_chain moves density", cinv["density_per_1000"] > 0):
        fails.append("causal-density")

    # 4. THE BUCKET INVARIANT — refrain caught but density stays ZERO
    rinv = _theme_inventory(REFRAIN)
    if not check("refrain caught (adjudication list)",
                 rinv["counts"]["refrain"] >= 3,
                 f"count={rinv['counts']['refrain']}"):
        fails.append("refrain-catch")
    if not check("refrain is NOT anaphora/causal",
                 rinv["counts"]["anaphora"] == 0
                 and rinv["counts"]["causal_chain"] == 0):
        fails.append("refrain-clean")
    if not check("BUCKET INVARIANT: refrain does NOT move density_per_1000",
                 rinv["density_per_1000"] == 0.0,
                 f"density={rinv['density_per_1000']}"):
        fails.append("BUCKET-INVARIANT")

    # 5. density formula uses ONLY the density-bearing kinds
    words = rinv["word_count"]
    expected = round(1000.0 * (rinv["counts"]["anaphora"]
                               + rinv["counts"]["causal_chain"]) / max(words, 1), 2)
    if not check("density == 1000*(anaphora+causal)/words",
                 rinv["density_per_1000"] == expected):
        fails.append("density-formula")

    if fails:
        print(f"\nFAILED: {fails}")
        return 1
    print("\nAll theme-inventory fixture cases PASS "
          "(density-bearing kinds fire; refrain never arms the gate).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
