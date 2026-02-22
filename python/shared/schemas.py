"""JSON schema contracts for the structural dynamics pipeline.

Dataclasses document the per-constraint field contracts for the two main
interchange files.  They are NOT instantiated during normal operation --
they serve as human-readable specifications.

Two contracts:
  PipelineConstraint  -- per-constraint entry in pipeline_output.json
                         (27 fields emitted by Prolog json_report.pl)
  EnrichedConstraint  -- per-constraint entry in enriched_pipeline.json
                         (PipelineConstraint + 12 fields from enrich_pipeline_json.py)

Two public validators:
  validate_pipeline_output(data)   -> list[str]   (empty = valid)
  validate_enriched_pipeline(data) -> list[str]   (empty = valid)

Both accept the full JSON dict (with per_constraint, diagnostic, etc.)
and return a list of human-readable error strings.
"""

from __future__ import annotations

import sys
from dataclasses import dataclass, field

from shared.constants import MAXENT_TYPES

# ===================================================================
# Schema dataclasses (documentation-only, never instantiated)
# ===================================================================


@dataclass
class PipelineConstraint:
    """Contract for a single per_constraint entry in pipeline_output.json.

    27 fields emitted by Prolog json_report.pl.  All fields must be present
    in the JSON.  Fields marked ``| None`` may be null; others must not be.

    Nullable fields fall into two categories:
      - Legitimately optional: purity_score/purity_band (26/1034 null when
        purity is not computed), topic_domain (6/1034 null), resistance
        (always null), resolution_strategy (deferred feature, always null).
      - Incomplete entries: human_readable, claimed_type, base_extractiveness,
        theater_ratio are null for 1-2 constraints with incomplete Prolog
        testset data.  These are data quality issues, not design intent.
    """

    # --- Identity (always present, never null) ---
    id: str                                       # constraint ID (snake_case)

    # --- Classification ---
    human_readable: str | None = None             # narrative display name (null: incomplete entry)
    claimed_type: str | None = None               # one of MAXENT_TYPES (null: incomplete entry)
    perspectives: dict = field(default_factory=dict)  # {powerless, moderate, institutional, analytical -> type str}
    classifications: list = field(default_factory=list)  # [{type, context: {agent_power, time_horizon, exit_options, spatial_scope}}]

    # --- Continuous metrics ---
    base_extractiveness: float | None = None      # [0, 1] (null: incomplete entry)
    suppression: float = 0.0                      # [0, 1]
    theater_ratio: float | None = None            # [0, 1] (null: incomplete entry)
    purity_score: float | None = None             # [0, 1]; null when purity not computed (26/1034)
    maxent_entropy: float = 0.0                   # [0, 1] normalized Shannon entropy

    # --- Categorical metrics ---
    signature: str = ""                           # structural signature label
    purity_band: str | None = None                # "clean", "degraded", etc.; null when purity not computed (26/1034)
    domain: str = ""                              # legacy domain classification
    topic_domain: str | None = None               # topic domain; null for some constraints (6/1034)
    maxent_top_type: str = ""                     # argmax of maxent_probs
    h1_band: int = 0                              # cohomological obstruction band [0..6]

    # --- Structural objects ---
    coupling: dict = field(default_factory=dict)   # {category: str, score: float, boltzmann: str}
    maxent_probs: dict = field(default_factory=dict)  # {mountain, rope, tangled_rope, snare, scaffold, piton -> float}
    raw_maxent_probs: dict = field(default_factory=dict)  # same structure as maxent_probs (pre-override)

    # --- Boolean features ---
    emerges_naturally: bool = False
    requires_active_enforcement: bool = False

    # --- Lists ---
    omegas: list = field(default_factory=list)     # [{id, type, question, severity}]
    gaps: list = field(default_factory=list)        # [{gap_type, powerless_type, institutional_type}]
    beneficiaries: list = field(default_factory=list)  # [str]
    victims: list = field(default_factory=list)     # [str]
    drift_events: list = field(default_factory=list)  # [{type, severity}]

    # --- Diagnostic verdict (per-constraint subsystem synthesis) ---
    diagnostic_verdict: dict | None = None        # {verdict, agreements, expected_conflicts, ...}

    # --- Post-synthesis divergence flags (T12) ---
    post_synthesis_flags: list = field(default_factory=list)  # [{flag_type, details}]

    # --- Always nullable ---
    resistance: float | None = None               # null for all current constraints
    resolution_strategy: str | None = None        # deferred feature, always null


@dataclass
class EnrichedConstraint(PipelineConstraint):
    """Contract for a single per_constraint entry in enriched_pipeline.json.

    Strict superset of PipelineConstraint with 12 additional fields
    computed by enrich_pipeline_json.py.
    """

    # --- Confidence metrics (all None when maxent_probs/claimed_type missing) ---
    confidence: float | None = None               # P(claimed_type) from maxent
    rival_type: str | None = None                 # argmax excluding claimed_type
    rival_prob: float | None = None               # P(rival_type) from maxent
    confidence_margin: float | None = None        # confidence - rival_prob
    confidence_entropy: float | None = None       # Shannon entropy of maxent_probs
    confidence_band: str | None = None            # "deep" | "moderate" | "borderline"
    boundary: str | None = None                   # "{claimed}->{rival}" or None

    # --- Raw rival ---
    raw_rival_prob: float | None = None           # rival prob from raw_maxent_probs

    # --- Coalition (always set, never None) ---
    coalition_type: str = "other"                 # classify_coalition() result

    # --- Tangled rope only (None for non-tangled_rope) ---
    tangled_psi: float | None = None              # psi metric
    tangled_band: str | None = None               # "rope_leaning" | "genuinely_tangled" | "snare_leaning"

    # --- Abductive (always set, defaults to []) ---
    abductive_triggers: list = field(default_factory=list)


# ===================================================================
# Field specification tables (drive the validation engine)
# ===================================================================
# Format: (field_name, expected_type_for_isinstance, nullable)
# Use (int, float) for numeric fields because JSON integers parse as int.

_MAXENT_SET = set(MAXENT_TYPES)

_PERSPECTIVE_KEYS = {"powerless", "moderate", "institutional", "analytical"}

_CONFIDENCE_BANDS = {"deep", "moderate", "borderline"}
_TANGLED_BANDS = {"rope_leaning", "genuinely_tangled", "snare_leaning"}

PIPELINE_FIELDS = [
    # (field_name, expected_type, nullable)
    # --- Always present, never null ---
    ("id",                          str,          False),
    ("perspectives",                dict,         False),
    ("suppression",                 (int, float), False),
    ("signature",                   str,          False),
    ("coupling",                    dict,         False),
    ("omegas",                      list,         False),
    ("gaps",                        list,         False),
    ("beneficiaries",               list,         False),
    ("victims",                     list,         False),
    ("emerges_naturally",           bool,         False),
    ("requires_active_enforcement", bool,         False),
    ("classifications",             list,         False),
    ("domain",                      str,          False),
    ("maxent_probs",                dict,         False),
    ("raw_maxent_probs",            dict,         False),
    ("maxent_entropy",              (int, float), False),
    ("maxent_top_type",             str,          False),
    ("h1_band",                     int,          False),
    ("drift_events",                list,         False),
    # --- Nullable: incomplete entries (1-2 constraints) ---
    ("human_readable",              str,          True),
    ("claimed_type",                str,          True),
    ("base_extractiveness",         (int, float), True),
    ("theater_ratio",               (int, float), True),
    # --- Nullable: legitimately optional ---
    ("purity_score",                (int, float), True),   # 26/1034 null
    ("purity_band",                 str,          True),   # 26/1034 null
    ("topic_domain",                str,          True),   # 6/1034 null
    ("resistance",                  (int, float), True),   # always null
    ("resolution_strategy",         str,          True),   # always null (deferred)
    # --- Diagnostic verdict (per-constraint subsystem synthesis) ---
    ("diagnostic_verdict",          dict,         True),   # null if diagnostic_summary fails
    # --- Post-synthesis divergence flags (T12) ---
    ("post_synthesis_flags",        list,         False),  # [] when no divergence
]

ENRICHED_EXTRA_FIELDS = [
    ("confidence",          (int, float), True),
    ("rival_type",          str,         True),
    ("rival_prob",          (int, float), True),
    ("confidence_margin",   (int, float), True),
    ("confidence_entropy",  (int, float), True),
    ("confidence_band",     str,         True),
    ("boundary",            str,         True),
    ("raw_rival_prob",      (int, float), True),
    ("coalition_type",      str,         False),
    ("tangled_psi",         (int, float), True),
    ("tangled_band",        str,         True),
    ("abductive_triggers",  list,        False),
]

_PIPELINE_FIELD_NAMES = {f[0] for f in PIPELINE_FIELDS}
_ALL_ENRICHED_FIELD_NAMES = _PIPELINE_FIELD_NAMES | {f[0] for f in ENRICHED_EXTRA_FIELDS}


# ===================================================================
# Internal validation helpers
# ===================================================================

def _check_field(entry, field_name, expected_type, nullable, cid):
    """Check presence and type of a single field.  Returns error list."""
    errors = []
    if field_name not in entry:
        errors.append(f"[{cid}] missing required field: {field_name}")
        return errors

    value = entry[field_name]
    if value is None:
        if not nullable:
            errors.append(f"[{cid}] field '{field_name}' is null but required")
        return errors

    if not isinstance(value, expected_type):
        type_label = (expected_type.__name__ if isinstance(expected_type, type)
                      else "/".join(t.__name__ for t in expected_type))
        errors.append(
            f"[{cid}] field '{field_name}': expected {type_label}, "
            f"got {type(value).__name__}"
        )
    return errors


def _check_structure(entry, cid):
    """Structural invariant checks on a pipeline constraint."""
    errors = []

    # claimed_type should be a known type (warn, don't fail — data quality issue)
    claimed = entry.get("claimed_type")
    if isinstance(claimed, str) and claimed not in _MAXENT_SET:
        print(f"  [WARN] [{cid}] claimed_type '{claimed}' not in MAXENT_TYPES",
              file=sys.stderr)

    # perspectives must have exactly the 4 expected keys
    persp = entry.get("perspectives")
    if isinstance(persp, dict):
        missing = _PERSPECTIVE_KEYS - set(persp.keys())
        if missing:
            errors.append(f"[{cid}] perspectives missing keys: {sorted(missing)}")

    # maxent_probs / raw_maxent_probs must have the 6 type keys summing to ~1.0
    for fname in ("maxent_probs", "raw_maxent_probs"):
        mp = entry.get(fname)
        if isinstance(mp, dict):
            missing_types = _MAXENT_SET - set(mp.keys())
            if missing_types:
                errors.append(f"[{cid}] {fname} missing types: {sorted(missing_types)}")
            total = sum(v for v in mp.values() if isinstance(v, (int, float)))
            if abs(total - 1.0) > 0.01:
                errors.append(f"[{cid}] {fname} sum={total:.6f}, expected ~1.0")

    # coupling must have {category, score, boltzmann}
    coupling = entry.get("coupling")
    if isinstance(coupling, dict):
        for k in ("category", "score", "boltzmann"):
            if k not in coupling:
                errors.append(f"[{cid}] coupling missing key: {k}")

    # classifications entries must have {type, context}
    clfs = entry.get("classifications")
    if isinstance(clfs, list):
        for i, clf in enumerate(clfs):
            if not isinstance(clf, dict):
                errors.append(f"[{cid}] classifications[{i}] is not a dict")
                continue
            if "type" not in clf:
                errors.append(f"[{cid}] classifications[{i}] missing 'type'")
            ctx = clf.get("context")
            if not isinstance(ctx, dict):
                errors.append(f"[{cid}] classifications[{i}].context is not a dict")
            else:
                for k in ("agent_power", "time_horizon", "exit_options", "spatial_scope"):
                    if k not in ctx:
                        errors.append(f"[{cid}] classifications[{i}].context missing '{k}'")

    # omegas entries must have {id, type, question, severity}
    omegas = entry.get("omegas")
    if isinstance(omegas, list):
        for i, omega in enumerate(omegas):
            if isinstance(omega, dict):
                for k in ("id", "type", "question", "severity"):
                    if k not in omega:
                        errors.append(f"[{cid}] omegas[{i}] missing '{k}'")

    # gaps entries must have {gap_type, powerless_type, institutional_type}
    gaps = entry.get("gaps")
    if isinstance(gaps, list):
        for i, gap in enumerate(gaps):
            if isinstance(gap, dict):
                for k in ("gap_type", "powerless_type", "institutional_type"):
                    if k not in gap:
                        errors.append(f"[{cid}] gaps[{i}] missing '{k}'")

    # drift_events entries must have {type, severity}
    drift = entry.get("drift_events")
    if isinstance(drift, list):
        for i, de in enumerate(drift):
            if isinstance(de, dict):
                for k in ("type", "severity"):
                    if k not in de:
                        errors.append(f"[{cid}] drift_events[{i}] missing '{k}'")

    # h1_band range
    h1 = entry.get("h1_band")
    if isinstance(h1, int) and not (0 <= h1 <= 6):
        errors.append(f"[{cid}] h1_band={h1}, expected [0..6]")

    # diagnostic_verdict structure
    dv = entry.get("diagnostic_verdict")
    if isinstance(dv, dict):
        _VERDICT_VALUES = {"green", "yellow", "red"}
        v = dv.get("verdict")
        if v is not None and isinstance(v, str) and v not in _VERDICT_VALUES:
            errors.append(
                f"[{cid}] diagnostic_verdict.verdict '{v}' "
                f"not in {sorted(_VERDICT_VALUES)}"
            )
        for dv_key in ("verdict", "agreements", "expected_conflicts",
                        "tensions", "subsystems_available",
                        "subsystems_unavailable"):
            if dv_key not in dv:
                errors.append(
                    f"[{cid}] diagnostic_verdict missing key: {dv_key}"
                )
        if "agreements" in dv and not isinstance(dv["agreements"], list):
            errors.append(
                f"[{cid}] diagnostic_verdict.agreements should be list"
            )
        if "expected_conflicts" in dv and not isinstance(dv["expected_conflicts"], list):
            errors.append(
                f"[{cid}] diagnostic_verdict.expected_conflicts should be list"
            )
        for i, ec in enumerate(dv.get("expected_conflicts", [])):
            if isinstance(ec, dict):
                for k in ("subsystem", "pattern", "explanation"):
                    if k not in ec:
                        errors.append(
                            f"[{cid}] diagnostic_verdict.expected_conflicts"
                            f"[{i}] missing '{k}'"
                        )
        if "tensions" in dv and not isinstance(dv["tensions"], list):
            errors.append(
                f"[{cid}] diagnostic_verdict.tensions should be list"
            )
        for i, t in enumerate(dv.get("tensions", [])):
            if isinstance(t, dict):
                for k in ("subsystem", "signal", "detail"):
                    if k not in t:
                        errors.append(
                            f"[{cid}] diagnostic_verdict.tensions"
                            f"[{i}] missing '{k}'"
                        )
        if "subsystems_available" in dv:
            sa = dv["subsystems_available"]
            if not isinstance(sa, (int, float)):
                errors.append(
                    f"[{cid}] diagnostic_verdict.subsystems_available "
                    f"should be numeric"
                )
        if "subsystems_unavailable" in dv:
            su = dv["subsystems_unavailable"]
            if not isinstance(su, list):
                errors.append(
                    f"[{cid}] diagnostic_verdict.subsystems_unavailable "
                    f"should be list"
                )

    # post_synthesis_flags entries must have {flag_type, details}
    ps_flags = entry.get("post_synthesis_flags")
    if isinstance(ps_flags, list):
        for i, psf in enumerate(ps_flags):
            if isinstance(psf, dict):
                for k in ("flag_type", "details"):
                    if k not in psf:
                        errors.append(
                            f"[{cid}] post_synthesis_flags[{i}] missing '{k}'"
                        )

    return errors


def _check_enriched_structure(entry, cid):
    """Structural invariant checks on enrichment-only fields."""
    errors = []

    # confidence_band must be a known value
    cb = entry.get("confidence_band")
    if cb is not None and isinstance(cb, str) and cb not in _CONFIDENCE_BANDS:
        errors.append(f"[{cid}] confidence_band '{cb}' not in {sorted(_CONFIDENCE_BANDS)}")

    # tangled_band must be a known value
    tb = entry.get("tangled_band")
    if tb is not None and isinstance(tb, str) and tb not in _TANGLED_BANDS:
        errors.append(f"[{cid}] tangled_band '{tb}' not in {sorted(_TANGLED_BANDS)}")

    # tangled_psi/tangled_band must be None for non-tangled_rope
    claimed = entry.get("claimed_type")
    tp = entry.get("tangled_psi")
    tband = entry.get("tangled_band")
    if claimed is not None and claimed != "tangled_rope":
        if tp is not None:
            errors.append(f"[{cid}] tangled_psi={tp} but claimed_type='{claimed}'")
        if tband is not None:
            errors.append(f"[{cid}] tangled_band='{tband}' but claimed_type='{claimed}'")

    # tangled_psi and tangled_band must be both null or both non-null
    if (tp is None) != (tband is None):
        errors.append(
            f"[{cid}] tangled_psi/tangled_band null mismatch: "
            f"psi={'null' if tp is None else tp}, band={'null' if tband is None else tband}"
        )

    # boundary format: "X->Y" when not None
    boundary = entry.get("boundary")
    if boundary is not None and isinstance(boundary, str) and "->" not in boundary:
        errors.append(f"[{cid}] boundary '{boundary}' missing '->' separator")

    # Confidence fields must be all-null or all-non-null together
    conf_fields = ["confidence", "rival_type", "rival_prob",
                    "confidence_margin", "confidence_entropy", "confidence_band"]
    nulls = [entry.get(f) is None for f in conf_fields if f in entry]
    if nulls and any(nulls) and not all(nulls):
        non_null = [f for f in conf_fields if f in entry and entry.get(f) is not None]
        null = [f for f in conf_fields if f in entry and entry.get(f) is None]
        errors.append(
            f"[{cid}] confidence field null-inconsistency: "
            f"non-null={non_null}, null={null}"
        )

    return errors


def _warn_unexpected_fields(entry, known_fields, cid):
    """Print stderr warnings for unexpected fields (schema drift detection).

    Does NOT return errors -- unexpected fields warn but don't fail.
    """
    unexpected = set(entry.keys()) - known_fields
    for f in sorted(unexpected):
        print(f"  [WARN] [{cid}] unexpected field: {f}", file=sys.stderr)


# ===================================================================
# Public validation functions
# ===================================================================

def validate_pipeline_output(data):
    """Validate a full pipeline_output.json dict.

    Returns list of error strings (empty = valid).
    Unexpected fields produce stderr warnings but are not included in errors.
    """
    errors = []

    if not isinstance(data, dict):
        return [f"Expected dict, got {type(data).__name__}"]

    if "per_constraint" not in data:
        return ["Missing top-level 'per_constraint' key"]

    per_constraint = data["per_constraint"]
    if not isinstance(per_constraint, list):
        return [f"'per_constraint' should be list, got {type(per_constraint).__name__}"]

    if len(per_constraint) == 0:
        return ["'per_constraint' is empty"]

    seen_ids = set()
    has_unexpected = False
    for i, entry in enumerate(per_constraint):
        if not isinstance(entry, dict):
            errors.append(f"per_constraint[{i}] is not a dict")
            continue

        cid = entry.get("id", f"<index {i}>")

        # Duplicate ID check
        if cid in seen_ids:
            errors.append(f"[{cid}] duplicate constraint ID")
        seen_ids.add(cid)

        # Field presence and type checks
        for field_name, expected_type, nullable in PIPELINE_FIELDS:
            errors.extend(_check_field(entry, field_name, expected_type, nullable, cid))

        # Structural invariants
        errors.extend(_check_structure(entry, cid))

        # Unexpected fields (warn only, first occurrence triggers header)
        unexpected = set(entry.keys()) - _PIPELINE_FIELD_NAMES
        if unexpected and not has_unexpected:
            print("Schema drift warnings (pipeline_output.json):", file=sys.stderr)
            has_unexpected = True
        if unexpected:
            _warn_unexpected_fields(entry, _PIPELINE_FIELD_NAMES, cid)

    return errors


def validate_enriched_pipeline(data):
    """Validate a full enriched_pipeline.json dict.

    Runs all pipeline_output checks first (enriched is a strict superset),
    then checks the 12 enrichment fields.

    Returns list of error strings (empty = valid).
    """
    # The enriched file has all pipeline fields plus enrichment fields,
    # so we can't use validate_pipeline_output directly (it would warn
    # about the enrichment fields as unexpected).  Instead, replicate
    # the core checks with the enriched known-fields set.
    errors = []

    if not isinstance(data, dict):
        return [f"Expected dict, got {type(data).__name__}"]

    if "per_constraint" not in data:
        return ["Missing top-level 'per_constraint' key"]

    per_constraint = data["per_constraint"]
    if not isinstance(per_constraint, list):
        return [f"'per_constraint' should be list, got {type(per_constraint).__name__}"]

    if len(per_constraint) == 0:
        return ["'per_constraint' is empty"]

    seen_ids = set()
    has_unexpected = False
    for i, entry in enumerate(per_constraint):
        if not isinstance(entry, dict):
            errors.append(f"per_constraint[{i}] is not a dict")
            continue

        cid = entry.get("id", f"<index {i}>")

        # Duplicate ID check
        if cid in seen_ids:
            errors.append(f"[{cid}] duplicate constraint ID")
        seen_ids.add(cid)

        # Pipeline field checks (base contract)
        for field_name, expected_type, nullable in PIPELINE_FIELDS:
            errors.extend(_check_field(entry, field_name, expected_type, nullable, cid))

        # Pipeline structural invariants
        errors.extend(_check_structure(entry, cid))

        # Enrichment field checks
        for field_name, expected_type, nullable in ENRICHED_EXTRA_FIELDS:
            errors.extend(_check_field(entry, field_name, expected_type, nullable, cid))

        # Enrichment structural invariants
        errors.extend(_check_enriched_structure(entry, cid))

        # Unexpected fields (warn only)
        unexpected = set(entry.keys()) - _ALL_ENRICHED_FIELD_NAMES
        if unexpected and not has_unexpected:
            print("Schema drift warnings (enriched_pipeline.json):", file=sys.stderr)
            has_unexpected = True
        if unexpected:
            _warn_unexpected_fields(entry, _ALL_ENRICHED_FIELD_NAMES, cid)

    return errors
