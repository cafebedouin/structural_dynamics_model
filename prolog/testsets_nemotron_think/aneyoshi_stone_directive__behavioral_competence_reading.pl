% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive — Behavioral Competence Reading
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   In Aneyoshi, Japan, a stone marker erected after the 1933 Shōwa Sanriku
 *   tsunami reads: 'High dwellings are the peace and harmony of our
 *   descendants. Remember the calamity of the great tsunamis. Do not build
 *   any homes below this point.' The village obeyed for 78 years; when the
 *   2011 Tōhoku tsunami struck, Aneyoshi suffered zero fatalities while
 *   neighboring towns were devastated. This reading holds that the stone's
 *   directive remained a behaviorally binding land-use constraint across the
 *   entire inter-catastrophe period — not merely a memorial. The constraint
 *   is the physical geography of tsunami inundation risk, marked and
 *   culturally transmitted by the stone. Extractiveness is near-zero because
 *   compliance avoids a natural hazard, not a human extraction. No party
 *   benefits from compliance; the constraint simply exists.
 *
 * KEY AGENTS:
 *   - aneyoshi_villagers: Primary targets (moderate/constrained) — bear the opportunity cost of not using lower land, but gain survival
 *   - ancestral_elders: Agenda setters (historical, identity_locked) — erected the stone and established the norm
 *   - disaster_anthropologists: Observers (analytical/analytical) — study the transmission mechanism
 *   - geologists: Observers (analytical/analytical) — validate the physical geography referent
 *   - neighboring_communities: Excluded (moderate/trapped) — lacked equivalent markers and suffered catastrophic loss
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.03).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.08).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive — Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, 'e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9').
narrative_ontology:cs_kernel_codification('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9', fixed_text).
narrative_ontology:cs_authority_grounding('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9', lineage).
narrative_ontology:cs_interpretation_layer_present('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9').
narrative_ontology:cs_reading_relation('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9', aneyoshi_stone_directive__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9', foundational, physical_geography_grounds_behavioral_obligation).
narrative_ontology:cs_axiom_status(physical_geography_grounds_behavioral_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9', physical_geography_grounds_behavioral_obligation, empirically_contingent).
narrative_ontology:cs_axiom('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9', secondary, intergenerational_transmission_solves_memory_decay).
narrative_ontology:cs_axiom_status(intergenerational_transmission_solves_memory_decay, holdable).
narrative_ontology:cs_axiom_grounding('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9', intergenerational_transmission_solves_memory_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9', ancestral_tsunami_warning_tradition).
narrative_ontology:cs_drift_state('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9', post_2011_validation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e9f3a5ed-2b3c-432c-ae10-b88a5b2b26e9', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_villagers).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, tsunami_inundation_geography).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, elevation_safety_principle).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, intergenerational_warning_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reside in Aneyoshi and comply with the stone's directive by building only above the marked elevation. They bear the opportunity cost of not using lower, more convenient land near the harbor. Exit is constrained — moving away means leaving ancestral land and community. The 2011 tsunami validated their compliance: zero fatalities in the village.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_villagers, payer,
    moderate, biographical, constrained, local).

% The 1933 survivors who erected the stone and established the norm. Their authority derives from direct catastrophe experience and the intergenerational transmission they initiated. They are historical agents (not current) but their founding act structures the present constraint. Identity-locked because the village's self-concept is constituted through this ancestral warning.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, ancestral_elders, agenda_setter,
    institutional, generational, identity_locked, local).
narrative_ontology:stakeholder_non_agent(aneyoshi_stone_directive__behavioral_competence_reading, ancestral_elders).

% Study the Aneyoshi case as a rare instance of successful intergenerational disaster warning transmission. They analyze whether behavioral compliance persisted continuously or decayed into ritual. Their seat is analytical — they neither bear costs nor collect benefits from the constraint.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% Validate the physical geography referent: the stone's elevation corresponds to the maximum tsunami inundation line for the 1896, 1933, and 2011 events. They confirm the constraint's mountain nature — its referent is a natural law (hydrodynamics), not a human choice.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, geologists, observer,
    analytical, civilizational, analytical, global).

% Villages along the Sanriku coast that lacked equivalent stone markers or lost their transmission. They suffered catastrophic fatalities in 2011. They are excluded from the Aneyoshi constraint's protection — not by active suppression but by the absence of an equivalent cultural-physical marker. Their situation highlights what the constraint makes possible: survival through transmitted knowledge.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, neighboring_communities, excluded,
    moderate, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone marks a physical boundary that coordinates settlement patterns away from tsunami inundation zones. It solves the coordination problem of transmitting intergenerational knowledge about rare, catastrophic events — ensuring each generation knows where not to build without having to experience the tsunami themselves.
% TRANSFER_FUNCTION: No transfer occurs. The constraint moves no resources between agents. Compliance avoids a natural hazard; non-compliance risks death. The stone's inscription is an information standard, not a transfer mechanism.
% ABSENT_VOICES: The 1933 tsunami victims who could not testify to the stone's efficacy — their silence is the founding trauma. Neighboring communities who lacked equivalent markers are structurally excluded from this constraint's protection; they would attest to the cost of its absence.
% DISAPPEARANCE_RATIONALE: If the stone vanished overnight, the physical tsunami risk would remain unchanged. The constraint's referent is the geography, not the marker. However, the cultural transmission mechanism would degrade — future generations might lose the precise elevation knowledge. The world rearranges only in the epistemic dimension, not the physical one.
% FOUNDING_PROBLEM: The 1896 Meiji Sanriku tsunami and 1933 Shōwa Sanriku tsunami killed hundreds in Aneyoshi. Survivors erected the stone to prevent rebuilding in the inundation zone, solving the problem of intergenerational memory decay for rare catastrophic events.
% FOUNDING_PROBLEM_CORROBORATION: Geological records confirm recurring megathrust earthquakes on the Japan Trench (869 Jōgan, 1896 Meiji, 1933 Shōwa, 2011 Tōhoku). Historical demography documents Aneyoshi's fatality counts. Survivor testimonies recorded in municipal archives. The 2011 event empirically corroborated the founding problem's persistence — the same geography produced the same hazard.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.03, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a mountain because its referent is physical geography (tsunami run-up elevation), not human arrangement. The stone is a marker, not the constraint itself. Extractiveness 0.03 reflects only the opportunity cost of forgoing lower-elevation land — a cost imposed by nature, not by a human extractor. Suppression 0.08 reflects minimal social enforcement (community norm adherence), not coercion. Accessibility_collapse 0.92 because physics admits no alternatives: you cannot negotiate with tsunami hydrodynamics. Resistance 0.04 because the constraint meets almost no active opposition — the 2011 event confirmed its validity. Theater_ratio 0.05 because ritual commemoration exists but the behavioral core remains functional. The measurement grid shows remarkable stability across 91 years.
 *
 * DIRECTIONALITY LOGIC:
 *   All human agents are symmetric relative to the physical constraint (d ≈ 0.5): everyone faces the same tsunami risk, everyone gains the same survival benefit from compliance. The stone creates no beneficiaries or victims. Ancestral elders are historical agenda_setters but collect no rents. Villagers bear opportunity costs but gain survival — net benefit. No agent is extracted from by another agent. Directionality derivation from beneficiary/victim arrays (both empty) yields symmetric d for all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing tsunami fatalities) remains live — tsunami risk persists geologically. The arrangement has not atrophied; the 2011 event validated its function. No mandatrophy: the constraint's mandate matches its current operation. The commemorative_husk_reading's claim of behavioral decay is the competing hypothesis, captured in omega.behavioral_force_persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_force_persistence,
    'Did the stone''s directive retain continuous behavioral force across the 78-year inter-catastrophe period (1933–2011), or did compliance decay into commemorative ritual?',
    'Ethnographic reconstruction of settlement patterns, building permits, and oral testimony from 1933–2011; comparison with neighboring villages lacking stone markers.',
    'If behavioral force persisted, the constraint is a mountain (physical geography + cultural transmission). If force decayed and only commemorative residue remained, the constraint is a piton (theatrical maintenance of a depleted function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_force_persistence, empirical, 'Whether the stone''s directive remained a live behavioral constraint or became a commemorative husk.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the aneyoshi_stone_directive kernel a single constraint with contested readings, or two distinct constraints (physical marker vs. cultural practice) conflated by a shared label?',
    'Apply ε-invariance test: measure extractiveness and suppression separately for the physical geography claim (tsunami risk at elevation X) and the cultural transmission claim (ancestral warning obligates compliance). If ε diverges, decompose into two stories.',
    'If decomposable, the behavioral_competence_reading and commemorative_husk_reading are separate constraints linked by network.affects_constraints, not sibling readings of one kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel label ''aneyoshi_stone_directive'' covers one constraint or two structurally distinct ones.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1960, 0.03).
narrative_ontology:measurement(aney_tr_t1985, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1985, 0.04).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2011, 0.05).
narrative_ontology:measurement(aney_tr_t2024, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1933, 0.02).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1960, 0.02).
narrative_ontology:measurement(aney_be_t1985, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1985, 0.03).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2011, 0.03).
narrative_ontology:measurement(aney_be_t2024, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2024, 0.03).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1933, 0.05).
narrative_ontology:measurement(aney_su_t1960, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1960, 0.06).
narrative_ontology:measurement(aney_su_t1985, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1985, 0.07).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 2011, 0.08).
narrative_ontology:measurement(aney_su_t2024, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 2024, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition: behavioral_competence_reading (mountain, ε≈0.03) and commemorative_husk_reading (piton candidate, ε≈0.15 if theatrical maintenance) are sibling readings of the aneyoshi_stone_directive kernel. They share the physical marker but diverge on whether the directive's behavioral force persisted. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
