#!/usr/bin/env python3
"""Phase-A step-1 witness suite: stakeholder schema declaration (OQ-83).

W1  schema is valid JSON and a valid Draft 2020-12 schema
W2  backward compat: an existing pre-migration story still validates (additive)
W3  a story carrying stakeholders[] + six_questions validates
NC1 stakeholders: [] WITHOUT six_questions            -> must FAIL (Pattern-5 guard)
NC2 stakeholders: [] + disappearance world_rearranges -> must FAIL (authored-empty rule)
NC3 role: "contender"                                 -> must FAIL (dial-set enum; operator ruling)
NC4 founding_problem_status without corroboration     -> must FAIL (R5 provenance rule)

W2/W3 prove the validator accepts; NC1-4 prove the NEW constraints specifically
bite (each is the positive control for its own guard).
"""
import copy
import json
import sys
from pathlib import Path

import jsonschema
# Validate with the validator the PIPELINE actually uses (consumer's exact
# path: story_generator_base.strip_extra_properties uses Draft7Validator,
# falling back to Draft4) — not the newest available draft.
Draft202012Validator = getattr(jsonschema, "Draft7Validator", jsonschema.Draft4Validator)

ROOT = Path("/home/scott/bin/structural_dynamics_model")
SCHEMA = json.loads((ROOT / "schemas/constraint_story_schema.json").read_text())
STORY = json.loads((ROOT / "json/ai_governance_accountability.json").read_text())

results = []

def check(label, expect_valid, instance):
    errs = list(Draft202012Validator(SCHEMA).iter_errors(instance))
    ok = (not errs) == expect_valid
    detail = "" if not errs else f" [@{list(errs[0].absolute_path)}: {errs[0].message[:90]}]"
    results.append((label, "PASS" if ok else "FAIL",
                    f"expect {'valid' if expect_valid else 'INVALID'}, got "
                    f"{'valid' if not errs else 'invalid'}{detail}"))

# W1
Draft202012Validator.check_schema(SCHEMA)
results.append(("W1 schema well-formed", "PASS", "Draft202012Validator.check_schema OK"))

# W2 backward compat
check("W2 pre-migration story validates", True, STORY)

# W3 stakeholder story
s3 = copy.deepcopy(STORY)
s3["stakeholders"] = [
    {"name": "ai_deploying_corporations", "role": "agenda_setter",
     "secondary_role": "beneficiary", "power": "institutional",
     "time_horizon": "immediate", "exit_options": "arbitrage",
     "spatial_scope": "global",
     "situation": "Deploys systems without liability exposure; forum-shops across jurisdictions; writes the voluntary frameworks it is then judged by."},
    {"name": "algorithmic_decision_subjects", "role": "payer", "power": "powerless",
     "time_horizon": "biographical", "exit_options": "trapped",
     "spatial_scope": "national",
     "situation": "Denied employment, credit, services by systems they cannot inspect or appeal; bear the full cost of errors and bias."},
    {"name": "future_decision_subjects", "role": "excluded", "power": "powerless",
     "time_horizon": "generational", "exit_options": "trapped",
     "spatial_scope": "global", "agent": True,
     "situation": "Will live under precedents set now; not present in any current consultation."},
    {"name": "technological_neutrality_doctrine", "role": "beneficiary",
     "power": "institutional", "time_horizon": "civilizational",
     "exit_options": "analytical", "spatial_scope": "global", "agent": False,
     "situation": "Doctrine vindicated by the accountability gap persisting; collects no rents (non-agent)."}
]
s3["six_questions"] = {
    "coordination_function": "Some shared rules for deploying decision systems are genuinely needed for interoperability and trust.",
    "transfer_function": "Liability and error costs move from deploying corporations to decision subjects; attention moves from harms to compliance theater.",
    "absent_voices": "Future decision subjects; non-users harmed by others' automated decisions; workers displaced before any framework binds.",
    "disappearance_verdict": "world_rearranges",
    "disappearance_rationale": "Vendors would face direct liability overnight; insurers, courts, and procurement would all reprice.",
    "founding_problem": "Deployment outpaced institutional adaptation; the gap was tolerated to avoid freezing a nascent technology.",
    "founding_problem_status": "contested",
    "founding_problem_corroboration": "EU AI Act recitals and US state-bill findings attest the founding rationale from outside the deploying-corporation beneficiary set."
}
check("W3 stakeholder story validates", True, s3)

# NC1: authored-empty without six_questions
n1 = copy.deepcopy(STORY); n1["stakeholders"] = []
check("NC1 empty stakeholders, no six_questions", False, n1)

# NC2: authored-empty contradicting Q5
n2 = copy.deepcopy(s3); n2["stakeholders"] = []
check("NC2 empty stakeholders + world_rearranges", False, n2)

# NC3: contender role
n3 = copy.deepcopy(s3); n3["stakeholders"][0]["role"] = "contender"
check("NC3 role=contender rejected", False, n3)

# NC4: status without corroboration
n4 = copy.deepcopy(s3); del n4["six_questions"]["founding_problem_corroboration"]
check("NC4 status without corroboration", False, n4)

w = max(len(r[0]) for r in results)
fails = 0
for label, verdict, detail in results:
    print(f"{label:<{w}}  {verdict}  {detail}")
    fails += verdict == "FAIL"
print(f"\n{len(results)-fails}/{len(results)} checks passed")
sys.exit(1 if fails else 0)
