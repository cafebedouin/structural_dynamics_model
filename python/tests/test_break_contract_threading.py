"""
Break-contract threading witness (rides R13/R14 plumbing).

Stage 0 authors the break's ADDRESS (original_break / prior_status /
target_prior, surface-free); the orchestrator extracts it, scrubs it with
the same anonymizer that guards the air gap, and carries it to stages 2,
9, and 10. This test witnesses, with NO API spend:

  1. extraction returns the <break_contract> block verbatim (positive)
     and '' when absent (negative control — a block-free stage 0 must not
     fabricate a contract);
  2. the contract survives anonymization SURFACE-FREE: every character
     name from the stage-0 name map (incl. possessives and "The X"
     shorthand) is scrubbed, the XML structure survives intact;
  3. two-sided control: the scrub is name-map-driven, not a text
     destroyer — a content word absent from the name map survives, so
     assertion 2's "names gone" is a real scrub result;
  4. STAGE_INPUTS threading: break_contract feeds stages 2, 9, 10, and
     stage_9's list matches the review-blind assert exactly (a drifted
     list would crash every narrative run at stage 9);
  5. the stage-9 falsifier extraction stops BEFORE the new BREAK section
     (without the lookahead fix, the break finding is swallowed into the
     D9 adjudication payload — positive control: the falsifier body is
     still captured).

Run: python3 python/tests/test_break_contract_threading.py
"""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

from agent.uke_narrative_orchestrator import (  # noqa: E402
    STAGE_INPUTS,
    UKEOrchestrator,
    _extract_stage0_break_contract,
    _extract_stage9_falsifier,
)

BREAK_BLOCK = """<break_contract>
    <original_break>Santiago's defeat arrives through The Marlin, breaking the first readers' assumption that endurance is rewarded.</original_break>
    <prior_status>DEAD</prior_status>
    <target_prior>A reader still assumes that holding on longer than anyone else earns keeping what was held.</target_prior>
  </break_contract>"""

STAGE_0 = f"""<constraint_manifest>
  <selected>
    <constraint id="C1" name="endurance_trap" generation_order="1">
      <character_classifications>
        <character name="Santiago">
          <type>snare</type>
        </character>
        <character name="The Marlin">
          <type>mountain</type>
        </character>
      </character_classifications>
    </constraint>
  </selected>
  <invariant_contract>
    <untranslatable_real present="yes">A worth that holds only unclaimed.</untranslatable_real>
    <missing_floor present="no">absent</missing_floor>
    <inherent_instrument value="no">Extraction runs through endurance, not a reading.</inherent_instrument>
  </invariant_contract>
  {BREAK_BLOCK}
</constraint_manifest>
"""


def test_extraction_positive_and_negative():
    got = _extract_stage0_break_contract(STAGE_0)
    assert got == BREAK_BLOCK, f"block not returned verbatim:\n{got!r}"
    # Negative control: no block -> '' (never fabricated)
    assert _extract_stage0_break_contract(
        STAGE_0.replace(BREAK_BLOCK, "")) == ""
    print("PASS: extraction — verbatim block + '' on block-free output")


def test_anonymization_surface_free():
    scrubbed = UKEOrchestrator._anonymize_stage_1(STAGE_0, BREAK_BLOCK)
    # Names gone, in every form the anonymizer handles
    for leak in ("Santiago", "Marlin"):
        assert leak not in scrubbed, f"source name survived scrub: {leak!r}"
    # Structural labels took their place
    assert "Agent_A's" in scrubbed, scrubbed
    assert "Agent_B" in scrubbed, scrubbed
    # The XML address survives intact for the downstream consumers
    assert scrubbed.count("<break_contract>") == 1
    assert scrubbed.count("</break_contract>") == 1
    for tag in ("original_break", "prior_status", "target_prior"):
        assert f"<{tag}>" in scrubbed and f"</{tag}>" in scrubbed, tag
    assert "DEAD" in scrubbed
    print("PASS: anonymization — surface-free, structure intact")


def test_scrub_is_map_driven():
    scrubbed = UKEOrchestrator._anonymize_stage_1(STAGE_0, BREAK_BLOCK)
    # Two-sided control: "endurance" is not in the stage-0 name map and
    # must survive — proves the scrub targets the map, so the absence of
    # names above is a scrub result, not text destruction.
    assert "endurance" in scrubbed, "map-driven scrub destroyed content text"
    print("PASS: control — non-mapped content word survives the scrub")


def test_stage_inputs_threading():
    narrative = STAGE_INPUTS["narrative"]
    for stage in ("stage_2", "stage_9", "stage_10"):
        assert "break_contract" in narrative[stage], (
            f"break_contract missing from {stage} inputs: {narrative[stage]}")
    # Must match the review-blind assert in _run_stage_generic exactly,
    # or every narrative run crashes at stage 9.
    assert narrative["stage_9"] == [
        "stage_8", "invariant_contract", "break_contract"]
    print("PASS: STAGE_INPUTS — break_contract feeds stages 2, 9, 10; "
          "stage_9 list matches the blind assert")


def test_s9_falsifier_stops_before_break():
    review = (
        "REVIEW ASSESSMENT\n\n"
        "STRENGTHS:\nAlive.\n\n"
        "BIGGEST WEAKNESS:\nNone.\n\n"
        "INVARIANT FALSIFIER:\n"
        "HOLDS — the kill passage is on page 3.\n\n"
        "BREAK:\n"
        "The story violated the expectation that endurance is rewarded.\n\n"
        "READINESS:\nClose.\n\n"
        "ROUTE: VALIDATION\n"
    )
    finding = _extract_stage9_falsifier(review)
    # Positive control: the falsifier body is captured...
    assert "kill passage" in finding, finding
    # ...and the BREAK section is NOT swallowed into the D9 payload.
    assert "endurance is rewarded" not in finding, (
        f"BREAK section leaked into the D9 falsifier payload:\n{finding}")
    print("PASS: stage-9 falsifier extraction stops before BREAK")


if __name__ == "__main__":
    test_extraction_positive_and_negative()
    test_anonymization_surface_free()
    test_scrub_is_map_driven()
    test_stage_inputs_threading()
    test_s9_falsifier_stops_before_break()
    print("\nAll break-contract threading witnesses PASS")
