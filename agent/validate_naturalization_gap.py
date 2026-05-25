"""Forward validation: naturalization-gap detectors on known seeds.

Generates 4 naturalized-mountain + 3 pure-mountain constraint stories fresh
from seed text only (no pre-existing .pl or .json from the pre-kernel-frame corpus).
Runs both detectors on each:
  - Python detector: authority ∈ {extraction, diffuse_epistemic} AND δ_d flag
  - Prolog detector: cs_pattern=natural_law_constraint AND constraint_beneficiary exists

A naturalized-mountain case PASSES if it trips EITHER detector.
A pure-mountain case PASSES if it trips NEITHER detector.

Results → outputs/naturalization_gap_validation.json

Run: python3 agent/validate_naturalization_gap.py
"""
import json
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
import anthropic

from agent.story_generator_base import (
    REPO_ROOT,
    _SYSTEM_INSTRUCTION,
    build_prompt,
    strip_json_fences,
)

MODEL = "claude-haiku-4-5-20251001"
OUT_PATH = REPO_ROOT / "outputs" / "naturalization_gap_validation.json"
PROLOG_DIR = REPO_ROOT / "prolog"

NATURALIZED_SEEDS = [
    {
        "id": "career_duration_compression",
        "expected": "naturalized",
        "human_readable": (
            "AXIS: career_duration_compression — Mountain-type constraint, FSM candidate "
            "(has identifiable beneficiaries). "
            "Healthcare systems operate under a structural parameter: mean physician career length "
            "has declined from ~57 years (2008 cohort) to ~48 years (2024 cohort). "
            "This decline is presented as self-enforcing — an emergent property of physician "
            "burnout, trainee attrition, and labor market dynamics that no single institution "
            "controls. The constraint appears as a natural law of healthcare workforce economics. "
            "However: clinical_workforce and patient_continuity bear the costs (truncated tenure, "
            "reduced specialist pipeline, interrupted care relationships). "
            "healthcare_systems and administrative_structures benefit from lower long-term "
            "employment obligations and reduced senior-physician leverage. "
            "CS structure: the 'inevitable career arc' is a commitment kernel grounded in "
            "diffuse healthcare labor economics and burnout epidemiology — distributed epistemic "
            "consensus that establishes direction without adjudicating specific instances. "
            "authority_grounding: self_enforcing (no institution controls career duration; "
            "the constraint enforces itself through aggregate market dynamics). "
            "Include cs_structure block. Include both victims and beneficiaries."
        ),
    },
    {
        "id": "age_related_capacity_erosion",
        "expected": "naturalized",
        "human_readable": (
            "AXIS: age_related_capacity_erosion — Mountain-type constraint, FSM candidate "
            "(has identifiable beneficiaries). "
            "Age-related decline in untrained movement capacity is presented as immutable "
            "biological law: the 'aging is inevitable' constraint governs rehabilitation "
            "expectations, insurance reimbursement, and clinical protocols. "
            "The constraint appears self-enforcing — biological in origin, requiring no "
            "institutional adjudicator. "
            "However: aging_individuals bear the costs of restricted rehabilitation scope "
            "and lowered expectations. fitness_industry, pharmaceutical_companies, and "
            "age_management_clinics benefit from the 'inevitable decline' framing, which "
            "drives demand for compensatory products and services. "
            "CS structure: the 'aging constraint' is a commitment kernel grounded in "
            "biological science consensus — the authority that determines clinical expectations "
            "is the scientific-medical community (self_enforcing natural law framing). "
            "authority_grounding: self_enforcing. "
            "Include cs_structure block. Include both victims and beneficiaries."
        ),
    },
    {
        "id": "erasure_before_celebration",
        "expected": "naturalized",
        "human_readable": (
            "AXIS: erasure_before_celebration — Mountain-type constraint, FSM candidate "
            "(has identifiable beneficiaries). "
            "Cultural appropriation follows a recurring structural pattern: marginalized "
            "communities create practices; practices gain mainstream visibility; commercial "
            "interests adopt them; originating communities are displaced. "
            "This pattern is presented as natural cultural diffusion — inevitable, self-enforcing, "
            "how culture 'naturally spreads.' No institution governs which practices get "
            "appropriated; the constraint appears as a law of cultural dynamics. "
            "However: originating_marginalized_communities bear the costs (displacement, "
            "economic exclusion, loss of recognition). mainstream_commercial_interests and "
            "dominant_cultural_institutions benefit from the appropriation cycle. "
            "CS structure: the 'natural diffusion' framing is a commitment kernel — the "
            "inevitability claim is grounded in diffuse cultural theory and mainstream media "
            "consensus (epistemic authority that establishes direction without adjudication). "
            "authority_grounding: self_enforcing (no single institution controls cultural spread). "
            "Include cs_structure block. Include both victims and beneficiaries."
        ),
    },
    {
        "id": "physician_call_reluctance",
        "expected": "naturalized",
        "human_readable": (
            "AXIS: physician_call_reluctance — Mountain-type constraint, FSM candidate "
            "(has identifiable beneficiaries). "
            "Physician after-hours call reluctance is presented as an immutable structural "
            "feature of medical practice — a self-enforcing consequence of cognitive load "
            "limits and burnout risk that no policy can override. "
            "Healthcare institutions treat it as background environmental fact: "
            "staffing protocols, nursing triage scripts, and on-call coverage structures "
            "are all calibrated to physician unavailability as an invariant. "
            "However: nurses and patients_with_urgent_needs bear the costs "
            "(absorbing triage burden, delayed access to care). "
            "physicians and hospital_administration benefit from the institutional acceptance "
            "of call reluctance (reduced physician hours, maintained hierarchy). "
            "CS structure: the 'physician unavailability is inevitable' norm is a commitment "
            "kernel grounded in medical culture and hospital administrative practice. "
            "authority_grounding: self_enforcing (physician cognitive limits treated as "
            "natural law; no institutional authority overrides them). "
            "Include cs_structure block. Include both victims and beneficiaries."
        ),
    },
]

PURE_MOUNTAIN_SEEDS = [
    {
        "id": "fundamental_theorem_of_calculus",
        "expected": "pure",
        "human_readable": (
            "Fundamental Theorem of Calculus (Pure Mathematical Law): "
            "The theorem establishing that differentiation and integration are inverse operations "
            "is a pure mathematical constraint. For continuous f on [a,b]: ∫f dx = F(b)-F(a). "
            "Constraint enforces itself with no adjudicator possible or needed. "
            "No extraction. No victims. No beneficiaries. Do NOT include cs_structure block "
            "(mathematical theorems do not warrant CS fields per schema instructions)."
        ),
    },
    {
        "id": "godel_incompleteness_first_theorem",
        "expected": "pure",
        "human_readable": (
            "Gödel's First Incompleteness Theorem (Pure Mathematical Law): "
            "Any consistent formal system expressive enough to describe arithmetic contains "
            "true-but-unprovable statements. Pure structural constraint on formal systems. "
            "Self-enforcing; no adjudicator; no extraction; no victims; no beneficiaries. "
            "Do NOT include cs_structure block "
            "(mathematical theorems do not warrant CS fields per schema instructions)."
        ),
    },
    {
        "id": "ackermann_function_bounds",
        "expected": "pure",
        "human_readable": (
            "Ackermann Function Bounds (Pure Computability Constraint): "
            "The Ackermann function grows faster than any primitive recursive function. "
            "A(n,n) cannot be bounded by any primitive recursive function — this is a "
            "structural mathematical fact, self-enforcing, with no institutional adjudicator. "
            "No extraction. No victims. No beneficiaries. Do NOT include cs_structure block "
            "(mathematical theorems do not warrant CS fields per schema instructions)."
        ),
    },
]

ALL_SEEDS = NATURALIZED_SEEDS + PURE_MOUNTAIN_SEEDS

NATURALIZED_AUTHORITIES = {"extraction", "diffuse_epistemic"}
DEADWEIGHT_EPSILON_CEILING = 0.15


def generate_story(client: anthropic.Anthropic, seed: dict) -> tuple[dict, str]:
    prompt = build_prompt(seed["human_readable"])
    resp = client.messages.create(
        model=MODEL,
        max_tokens=8192,
        system=_SYSTEM_INSTRUCTION,
        messages=[{"role": "user", "content": prompt}],
    )
    raw = "".join(b.text for b in resp.content if hasattr(b, "text"))
    json_text = strip_json_fences(raw)
    try:
        story = json.loads(json_text)
    except json.JSONDecodeError as e:
        story = {"_parse_error": str(e), "_raw_head": raw[:500]}
    return story, raw


def python_detector(story: dict) -> tuple[bool, str]:
    """Python naturalization-gap detector.

    Returns (fired, reason).
    """
    bp = story.get("base_properties", {})
    cs = story.get("cs_structure", {}) or {}

    claimed_type = bp.get("claimed_type") or bp.get("type", "")
    eps_raw = bp.get("extractiveness")
    try:
        eps = float(eps_raw) if eps_raw is not None else None
    except (TypeError, ValueError):
        eps = None

    victims = bp.get("victims") or bp.get("victim_set") or []
    beneficiaries = bp.get("beneficiaries") or bp.get("beneficiary_set") or []
    authority = cs.get("authority_grounding")

    is_mountain = isinstance(claimed_type, str) and "mountain" in claimed_type.lower()
    low_eps = eps is not None and eps < DEADWEIGHT_EPSILON_CEILING
    has_victims = bool(victims)
    has_beneficiaries = bool(beneficiaries)

    delta_d_flag = is_mountain and low_eps and has_victims and has_beneficiaries

    if not is_mountain:
        return False, f"claimed_type={claimed_type!r} (not mountain)"
    if not low_eps:
        return False, f"ε={eps} (≥{DEADWEIGHT_EPSILON_CEILING})"
    if not has_victims:
        return False, f"no victims"
    if not has_beneficiaries:
        return False, f"no beneficiaries"
    if authority not in NATURALIZED_AUTHORITIES:
        return False, f"authority={authority!r} not in {NATURALIZED_AUTHORITIES}"

    return True, f"mountain, ε={eps}, authority={authority}, victims+bens present"


def build_temp_pl(constraint_id: str, story: dict) -> str:
    """Serialise cs_structure + beneficiaries from a story dict into Prolog facts."""
    cs = story.get("cs_structure", {}) or {}
    bp = story.get("base_properties", {})

    lines = [
        ":- use_module(narrative_ontology).",
        ":- multifile narrative_ontology:cs_kernel_codification/2.",
        ":- multifile narrative_ontology:cs_authority_grounding/2.",
        ":- multifile narrative_ontology:cs_interpretation_layer_present/1.",
        ":- multifile narrative_ontology:constraint_beneficiary/2.",
        ":- multifile narrative_ontology:constraint_metric/3.",
    ]

    cid = constraint_id

    if cs.get("kernel_codification"):
        lines.append(f"narrative_ontology:cs_kernel_codification({cid}, {cs['kernel_codification']}).")
    if cs.get("authority_grounding"):
        lines.append(f"narrative_ontology:cs_authority_grounding({cid}, {cs['authority_grounding']}).")
    if cs.get("interpretation_layer_present"):
        lines.append(f"narrative_ontology:cs_interpretation_layer_present({cid}).")

    beneficiaries = bp.get("beneficiaries") or bp.get("beneficiary_set") or []
    for b in beneficiaries:
        safe_b = str(b).lower().replace(" ", "_").replace("-", "_").replace("'", "")
        lines.append(f"narrative_ontology:constraint_beneficiary({cid}, {safe_b}).")

    eps_raw = bp.get("extractiveness")
    if eps_raw is not None:
        try:
            eps = float(eps_raw)
            lines.append(f"narrative_ontology:constraint_metric({cid}, extractiveness, {eps}).")
        except (TypeError, ValueError):
            pass

    return "\n".join(lines) + "\n"


def prolog_detector(constraint_id: str, story: dict) -> tuple[bool, str]:
    """Prolog false_natural_law_constraint verdict check.

    Writes a temp .pl file, queries swipl for the verdict, returns (fired, reason).
    """
    pl_content = build_temp_pl(constraint_id, story)

    with tempfile.NamedTemporaryFile(mode="w", suffix=".pl", dir=str(PROLOG_DIR),
                                     prefix=f"tmp_ngv_{constraint_id}_",
                                     delete=False, encoding="utf-8") as tf:
        tf.write(pl_content)
        temp_path = tf.name

    try:
        query = (
            f"[stack], [cs_pattern_detection], "
            f"['{Path(temp_path).name}'], "
            f"( cs_verdict({constraint_id}, false_natural_law_constraint) "
            f"-> write(verdict_fired) ; write(verdict_not_fired) ), halt."
        )
        result = subprocess.run(
            ["swipl", "-g", query, "-t", "halt(1)"],
            capture_output=True, text=True, timeout=30,
            cwd=str(PROLOG_DIR),
        )
        if "verdict_fired" in result.stdout and "verdict_not_fired" not in result.stdout:
            cs = story.get("cs_structure", {}) or {}
            return True, f"authority={cs.get('authority_grounding')!r}, beneficiary found"
        else:
            cs = story.get("cs_structure", {}) or {}
            bp = story.get("base_properties", {})
            bens = bp.get("beneficiaries") or bp.get("beneficiary_set") or []
            return False, f"authority={cs.get('authority_grounding')!r}, beneficiaries={bens}, output={result.stdout.strip()[:80]}"
    except subprocess.TimeoutExpired:
        return False, "swipl timeout"
    except Exception as e:
        return False, f"swipl error: {e}"
    finally:
        Path(temp_path).unlink(missing_ok=True)


def evaluate_seed(seed: dict, story: dict) -> dict:
    cid = seed["id"]
    expected = seed["expected"]

    py_fired, py_reason = python_detector(story)
    pl_fired, pl_reason = prolog_detector(cid, story)

    if expected == "naturalized":
        passes = py_fired or pl_fired
        pass_criterion = "EITHER detector must fire"
    else:
        passes = not py_fired and not pl_fired
        pass_criterion = "NEITHER detector must fire"

    detectors_fired = []
    if py_fired:
        detectors_fired.append("python")
    if pl_fired:
        detectors_fired.append("prolog")

    cs = story.get("cs_structure", {}) or {}
    bp = story.get("base_properties", {})

    return {
        "constraint_id": cid,
        "expected": expected,
        "passes": passes,
        "pass_criterion": pass_criterion,
        "detectors_fired": detectors_fired,
        "python_detector": {"fired": py_fired, "reason": py_reason},
        "prolog_detector": {"fired": pl_fired, "reason": pl_reason},
        "story_summary": {
            "claimed_type": bp.get("claimed_type") or bp.get("type"),
            "extractiveness": bp.get("extractiveness"),
            "authority_grounding": cs.get("authority_grounding"),
            "kernel_codification": cs.get("kernel_codification"),
            "victims": bp.get("victims") or bp.get("victim_set") or [],
            "beneficiaries": bp.get("beneficiaries") or bp.get("beneficiary_set") or [],
        },
    }


def main():
    client = anthropic.Anthropic()
    results = []

    print(f"Generating {len(ALL_SEEDS)} constraint stories fresh from seed text...\n")

    for i, seed in enumerate(ALL_SEEDS, 1):
        cid = seed["id"]
        expected = seed["expected"]
        marker = "NATURALIZED" if expected == "naturalized" else "PURE"
        print(f"  [{i}/{len(ALL_SEEDS)}] {cid} ({marker})...", flush=True)

        story, _ = generate_story(client, seed)

        if "_parse_error" in story:
            print(f"    PARSE ERROR: {story['_parse_error']}")
            result = {
                "constraint_id": cid,
                "expected": expected,
                "passes": False,
                "error": story["_parse_error"],
            }
        else:
            result = evaluate_seed(seed, story)
            status = "PASS" if result["passes"] else "FAIL"
            fired = result["detectors_fired"] or ["none"]
            print(f"    [{status}] detectors fired: {fired}")
            print(f"    claimed_type={result['story_summary']['claimed_type']}"
                  f"  ε={result['story_summary']['extractiveness']}"
                  f"  authority={result['story_summary']['authority_grounding']}")

        result["seed_human_readable"] = seed["human_readable"]
        results.append(result)

    passed = sum(1 for r in results if r.get("passes", False))
    failed = sum(1 for r in results if not r.get("passes", False))

    print(f"\n=== SUMMARY ===")
    print(f"  Passed: {passed}/{len(results)}")
    print(f"  Failed: {failed}/{len(results)}")

    naturalized_results = [r for r in results if r.get("expected") == "naturalized"]
    pure_results = [r for r in results if r.get("expected") == "pure"]

    print(f"\n  Naturalized-mountain cases ({len(naturalized_results)}):")
    for r in naturalized_results:
        status = "PASS" if r.get("passes") else "FAIL"
        fired = r.get("detectors_fired", [])
        print(f"    [{status}] {r['constraint_id']}: detectors={fired}")

    print(f"\n  Pure-mountain cases ({len(pure_results)}):")
    for r in pure_results:
        status = "PASS" if r.get("passes") else "FAIL"
        fired = r.get("detectors_fired", [])
        print(f"    [{status}] {r['constraint_id']}: detectors={fired}")

    output = {
        "summary": {
            "total": len(results),
            "passed": passed,
            "failed": failed,
            "naturalized_cases": len(naturalized_results),
            "pure_cases": len(pure_results),
        },
        "results": results,
    }

    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    OUT_PATH.write_text(json.dumps(output, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"\nResults → {OUT_PATH}")


if __name__ == "__main__":
    main()
