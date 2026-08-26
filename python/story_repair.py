#!/usr/bin/env python3
"""Canonical, deterministic constraint-story repair.

Single source of truth for "make a generated story meet the schema where the fix is
mechanical and lossless-of-meaning." Used by both the generator
(agent/generate_kernel_corpus.py) and the recovery script
(python/recover_historical_seeds.py) so the repair logic cannot fork.

What it repairs (deterministic, non-semantic):
  - strips unknown fields at the known container paths (allowed-set based);
  - defaults a missing required `base_properties.mandatrophy_resolved` (= ε < 0.46);
  - sanitises ids to ^[a-z][a-z0-9_]*$, TRANSLITERATING non-ASCII first
    (e.g. `māori_iwi_hapū` -> `maori_iwi_hapu`) — beneficiaries, victims, omega ids;
  - coerces null/negative measurement time_point/value to 0;
  - clamps directionality d_value and other UNCONDITIONAL [0,1] metric fields to range.

What it deliberately does NOT touch (would fabricate meaning — left to fail/regenerate):
  - conditional schema bounds (allOf/then: e.g. mountain ⇒ suppression ≤ 0.05). A story
    that claims a type but authors out-of-band metrics is internally inconsistent; clamping
    the metric to fit the claim invents data. These stay failures.
  - JSON parse errors (the model emitted invalid JSON) and genuine enum drift.

`repair_story` is idempotent and safe to run on an already-valid story.
"""
import re
import unicodedata

COMMENTARY_ALLOWED = {
    "narrative_context", "key_agents", "logic_rationale",
    "perspectival_gap", "directionality_logic", "mandatrophy_analysis", "kernel_context",
}
OMEGA_ALLOWED = {
    "id", "question", "resolution_mechanism", "impact",
    "confidence", "type_class", "description",
}
PERSPECTIVE_ALLOWED = {
    "classification_type", "agent_power", "time_horizon",
    "exit_options", "spatial_scope", "label", "comment",
}
MEASUREMENT_ALLOWED = {"metric", "time_point", "value", "id_override"}
# FALLBACK only — when the caller passes the schema, the allow-list is DERIVED from its
# top-level `properties` (single source). This static copy rotted once: it was last edited
# 2026-06-02, the stakeholder layer landed 2026-06-07/10, and for 2.5 months repair_story
# silently deleted `stakeholders`/`gain_flow`/`fixing_cost`/`six_questions`/`coercion_grid`
# from EVERY story that entered repair for ANY reason — surfacing downstream as the
# misleading "'stakeholders' is a required property" (witnessed 2026-08-22 on the
# nemotron leg's persisted raw responses: 291 draws, all carrying a stakeholders array).
TOP_LEVEL_ALLOWED = {
    "header", "base_properties", "perspectives", "omegas",
    "measurements", "interval", "commentary", "boltzmann",
    "network", "directionality_overrides", "uke_scope", "cs_structure",
    "stakeholders", "gain_flow", "fixing_cost", "six_questions", "coercion_grid",
    "provenance",
}

# Stakeholder `role` enum drift. The prompt says "victim" ten times (base_properties.victims,
# "must name at least one victim") and names the role enum once, so prose-following models
# (Flash, Nemotron; not Claude/Kimi) author role="victim". The schema defines payer = "bears
# its costs" — the same seat. Mapped like AXIOM_STATUS_REMAP: known drift coerced and COUNTED;
# any other out-of-enum role is left for validation to reject loudly.
STAKEHOLDER_ROLE_REMAP = {"victim": "payer"}
BASE_PROPS_ALLOWED = {
    "extractiveness", "suppression", "theater_ratio", "claimed_type",
    "human_readable", "topic_domain", "requires_active_enforcement",
    "emerges_naturally", "has_sunset_clause", "accessibility_collapse",
    "resistance", "beneficiaries", "victims", "mandatrophy_resolved",
}
# Unconditional [0,1] numeric fields in base_properties (NOT the conditional allOf/then bounds).
BASE_PROPS_UNIT_RANGE = {
    "extractiveness", "suppression", "theater_ratio",
    "accessibility_collapse", "resistance",
}
VALID_ID_RE = re.compile(r"^[a-z][a-z0-9_]*$")

# cs_axiom status enum (schema: holdable | overridden). `foreclosed` is engine-DERIVED
# (cs_axiom_foreclosed/2 from cs_axiom_contradiction/2), NOT an authored status; the
# generation prompt no longer offers it. This map is the repair safety-net for a model
# that emits it (or the non-enum `contested`) anyway.
VALID_AXIOM_STATUS = {"holdable", "overridden"}
# Known out-of-enum drift, mapped to the minimal defensible authored status:
#  - contested:  not an axiom status (founding_problem_status uses it); a contested
#                axiom is still coherently *holdable*.
#  - foreclosed: maps to `holdable`, NOT `overridden`. `overridden` would be harmless
#                ONLY when the constraint also authors a cs_axiom_contradiction for that
#                atom (then the engine derives foreclosure independently); otherwise it
#                over-claims structural displacement. repair_story has no contradiction
#                context — contradictions are authored in the scope manifest and emitted
#                to a separate <kernel>_contradictions.pl, not in the per-story JSON this
#                function sees — so it cannot establish the "harmless" branch and takes
#                the safe fallback (plan pre-commit rule: no contradiction → holdable).
AXIOM_STATUS_REMAP = {"contested": "holdable", "foreclosed": "holdable"}


def _normalize_axiom_status(status, stats):
    """Coerce a cs_axiom status into the {holdable, overridden} enum.

    Known drift (AXIOM_STATUS_REMAP) is mapped silently. Any OTHER out-of-enum value
    is a NOVEL mangle: it is COUNTED in *stats* (when provided), reported to stderr,
    and coerced to `holdable` — deliberately NOT a silent default. A nonzero
    `axiom_status_fallback` count is an escalation signal (a status string nobody
    anticipated; inspect `axiom_status_fallback_values`).
    """
    if status in VALID_AXIOM_STATUS:
        return status
    if status in AXIOM_STATUS_REMAP:
        return AXIOM_STATUS_REMAP[status]
    if stats is not None:
        stats["axiom_status_fallback"] = stats.get("axiom_status_fallback", 0) + 1
        stats.setdefault("axiom_status_fallback_values", []).append(status)
    import sys
    print(f"[story_repair] WARNING: out-of-enum cs_axiom status {status!r} coerced to "
          f"'holdable' (novel mangle — escalate, nonzero fallback count is signal)",
          file=sys.stderr)
    return "holdable"


def sanitize_id(s):
    """Convert a string to a valid constraint/atom id, transliterating non-ASCII first."""
    s = unicodedata.normalize("NFKD", str(s)).encode("ascii", "ignore").decode("ascii")
    s = s.lower()
    s = re.sub(r"[^a-z0-9_]", "_", s)
    s = re.sub(r"_+", "_", s).strip("_")
    if not s:
        s = "id_x"
    if not s[0].isalpha():
        s = "id_" + s
    return s


def _clamp(v, lo, hi):
    try:
        v = float(v)
    except (TypeError, ValueError):
        return v
    return max(lo, min(hi, v))


def repair_story(story, schema=None, stats=None):
    """Apply deterministic, meaning-preserving repairs in place; returns the story.

    *stats* (optional mutable dict): out-of-enum repair signals are accumulated here
    (currently `axiom_status_fallback` count + `axiom_status_fallback_values`). A
    caller that wants to fail-loud on novel mangles passes a dict and inspects it.
    """
    if not isinstance(story, dict):
        return story

    # Strip unknown top-level fields — allow-list derived from the schema when available.
    allowed = TOP_LEVEL_ALLOWED | set((schema or {}).get("properties", {}).keys())
    dropped = [k for k in story if k not in allowed]
    for k in dropped:
        del story[k]
    if dropped:
        import sys
        print(f"[story_repair] dropped top-level key(s) {dropped} (not in schema)", file=sys.stderr)
        if stats is not None:
            stats.setdefault("top_level_dropped", []).extend(dropped)

    # Stakeholder role drift (see STAKEHOLDER_ROLE_REMAP).
    sk = story.get("stakeholders")
    if isinstance(sk, list):
        for item in sk:
            if not isinstance(item, dict):
                continue
            for field in ("role", "secondary_role"):   # both carry the StakeholderRole enum
                if item.get(field) in STAKEHOLDER_ROLE_REMAP:
                    was = item[field]
                    item[field] = STAKEHOLDER_ROLE_REMAP[was]
                    if stats is not None:
                        stats["stakeholder_role_remapped"] = stats.get("stakeholder_role_remapped", 0) + 1
                        # Which value, on which field — the datum OQ-344 needs to say WHICH
                        # models follow prose vs enum, not merely how many did. Same idiom as
                        # axiom_status_fallback_values below.
                        stats.setdefault("stakeholder_role_remapped_values", []).append(
                            f"{field}={was}")

    c = story.get("commentary")
    if isinstance(c, dict):
        for k in list(c.keys()):
            if k not in COMMENTARY_ALLOWED:
                del c[k]

    persps = story.get("perspectives")
    if isinstance(persps, list):
        for p in persps:
            if isinstance(p, dict):
                for k in list(p.keys()):
                    if k not in PERSPECTIVE_ALLOWED:
                        del p[k]

    omegas = story.get("omegas")
    if isinstance(omegas, list):
        for om in omegas:
            if not isinstance(om, dict):
                continue
            for k in list(om.keys()):
                if k not in OMEGA_ALLOWED:
                    del om[k]
            if "id" in om and not VALID_ID_RE.match(str(om["id"])):
                om["id"] = sanitize_id(om["id"])

    meas = story.get("measurements")
    if isinstance(meas, list):
        # Metric-name transliteration (2026-06-05): models pattern-match the
        # base_properties field names ("suppression", "extractiveness") where the
        # measurement enum wants the canonical series names. This renames the
        # SERIES, never touches authored values — same class as id transliteration.
        # Unknown metric names are deliberately NOT mapped (fail-closed: repair must
        # not invent a metric).
        metric_aliases = {
            "suppression": "suppression_requirement",
            "extractiveness": "base_extractiveness",
            "theater": "theater_ratio",
        }
        for m in meas:
            if not isinstance(m, dict):
                continue
            if m.get("metric") in metric_aliases:
                m["metric"] = metric_aliases[m["metric"]]
            for k in list(m.keys()):
                if k not in MEASUREMENT_ALLOWED:
                    del m[k]
            if m.get("time_point") is None or (isinstance(m.get("time_point"), (int, float)) and m["time_point"] < 0):
                m["time_point"] = 0
            if m.get("value") is None:
                m["value"] = 0.0
            elif isinstance(m.get("value"), (int, float)):
                m["value"] = _clamp(m["value"], 0.0, 1.0)
            if m.get("id_override") and not VALID_ID_RE.match(str(m["id_override"])):
                m["id_override"] = sanitize_id(m["id_override"])

    bp = story.get("base_properties")
    if isinstance(bp, dict):
        for k in list(bp.keys()):
            if k not in BASE_PROPS_ALLOWED:
                del bp[k]
        # REMOVED 2026-06-05 (de-leak): the old rule fabricated mandatrophy_resolved from
        # an extractiveness threshold (a band-keyed default writing an AUTHORED field the
        # author never authored — fabricated-default pattern, and the eps>0.70 schema
        # conditional it served was deleted with the bands). Repair never touches authored
        # claims/metrics; authored-vs-computed divergence is signal, read downstream.
        for f in BASE_PROPS_UNIT_RANGE:
            if isinstance(bp.get(f), (int, float)):
                bp[f] = _clamp(bp[f], 0.0, 1.0)
        for field in ("beneficiaries", "victims"):
            if isinstance(bp.get(field), list):
                bp[field] = [
                    v if VALID_ID_RE.match(str(v)) else sanitize_id(v)
                    for v in bp[field] if v is not None
                ]

    # cs_structure id-pattern fields (axiom atoms, reference_frame, drift moment, sibling refs).
    # Sanitisation is deterministic, so axiom atoms that participate in contradiction matching
    # stay aligned across siblings (same input -> same output).
    cs = story.get("cs_structure")
    if isinstance(cs, dict):
        if cs.get("reference_frame") and not VALID_ID_RE.match(str(cs["reference_frame"])):
            cs["reference_frame"] = sanitize_id(cs["reference_frame"])
        ds = cs.get("drift_state")
        if isinstance(ds, dict) and ds.get("moment") and not VALID_ID_RE.match(str(ds["moment"])):
            ds["moment"] = sanitize_id(ds["moment"])
        if isinstance(cs.get("axioms"), list):
            for ax in cs["axioms"]:
                if not isinstance(ax, dict):
                    continue
                if ax.get("atom") and not VALID_ID_RE.match(str(ax["atom"])):
                    ax["atom"] = sanitize_id(ax["atom"])
                if "status" in ax:
                    ax["status"] = _normalize_axiom_status(ax["status"], stats)
        if isinstance(cs.get("reading_relations"), list):
            for rr in cs["reading_relations"]:
                if isinstance(rr, dict) and rr.get("sibling_id") and not VALID_ID_RE.match(str(rr["sibling_id"])):
                    rr["sibling_id"] = sanitize_id(rr["sibling_id"])

    net = story.get("network")
    if isinstance(net, dict) and isinstance(net.get("affects_constraints"), list):
        net["affects_constraints"] = [
            v if VALID_ID_RE.match(str(v)) else sanitize_id(v)
            for v in net["affects_constraints"] if v is not None
        ]

    ov = story.get("directionality_overrides")
    if isinstance(ov, dict):
        story["directionality_overrides"] = ov = [ov]  # schema expects array
    if isinstance(ov, list):
        for o in ov:
            if isinstance(o, dict) and "d_value" in o:
                o["d_value"] = _clamp(o["d_value"], 0.0, 1.0)

    return story
