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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Aneyoshi Stone Directive — Behavioral Competence Reading (Tsunami Inundation Line)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi stone directive ("Do not build your homes below this point")
 *   was erected after the 1896 Meiji-Sanriku tsunami and remained the sole
 *   land-use constraint governing the village's settlement pattern through
 *   the 1933 Showa-Sanriku tsunami and until the 2011 Tohoku earthquake. The
 *   behavioral_competence_reading treats the directive as a physical
 *   geography constraint: the stone marks the empirically verified tsunami
 *   inundation line. Compliance costs are near-zero (building above the line
 *   is topographically natural), no party extracts rents from the
 *   restriction, and the constraint would persist unchanged if all human
 *   institutions vanished — the topography remains. The 78-year persistence
 *   without formal validation reflects the constraint's mountain nature: it
 *   is a physical fact encoded in social memory, not a social fact maintained
 *   by enforcement.
 *
 * KEY AGENTS:
 *   - aneyoshi_households: Primary agents subject to the directive (powerless/identity_locked) — bear no extraction cost, benefit from survival
 *   - aneyoshi_municipal_authority: Agenda setter for land-use formalization (institutional/biographical) — could codify or ignore the directive
 *   - disaster_anthropologists: Observers (analytical/analytical) — study the transmission mechanism
 *   - commemorative_husk_beneficiaries: NOT in this reading — tourism/heritage actors who would benefit from the stone's symbolic status (appear in commemorative_husk_reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.02).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive — Behavioral Competence Reading (Tsunami Inundation Line)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, 'e8b526c4-f644-432b-8a7c-f649f4d02e38').
narrative_ontology:cs_kernel_codification('e8b526c4-f644-432b-8a7c-f649f4d02e38', fixed_text).
narrative_ontology:cs_authority_grounding('e8b526c4-f644-432b-8a7c-f649f4d02e38', practice).
narrative_ontology:cs_reading_relation('e8b526c4-f644-432b-8a7c-f649f4d02e38', aneyoshi_stone_directive__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('e8b526c4-f644-432b-8a7c-f649f4d02e38', foundational, stone_marks_physical_inundation_line).
narrative_ontology:cs_axiom_status(stone_marks_physical_inundation_line, holdable).
narrative_ontology:cs_axiom_grounding('e8b526c4-f644-432b-8a7c-f649f4d02e38', stone_marks_physical_inundation_line, empirically_contingent).
narrative_ontology:cs_axiom('e8b526c4-f644-432b-8a7c-f649f4d02e38', foundational, intergenerational_transmission_without_institution_is_possible).
narrative_ontology:cs_axiom_status(intergenerational_transmission_without_institution_is_possible, holdable).
narrative_ontology:cs_axiom_grounding('e8b526c4-f644-432b-8a7c-f649f4d02e38', intergenerational_transmission_without_institution_is_possible, empirically_contingent).
narrative_ontology:cs_reference_frame('e8b526c4-f644-432b-8a7c-f649f4d02e38', meiji_sanriku_survivor_knowledge).
narrative_ontology:cs_drift_state('e8b526c4-f644-432b-8a7c-f649f4d02e38', post_2011_tohoku_validation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e8b526c4-f644-432b-8a7c-f649f4d02e38', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, topographic_hazard_zoning_principle).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, intergenerational_risk_transmission_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households in Aneyoshi village who have built and lived above the stone's marked line for generations. The directive imposes no cost — the topography makes building below the line physically hazardous. Their compliance is survival-rational, not socially enforced. Exit would mean abandoning ancestral land and community; identity is fused with the place and its survival practice.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households, beneficiary,
    powerless, biographical, identity_locked, local).

% Local government with formal land-use planning authority. Could codify the stone's line into zoning law, ignore it, or contradict it. Bears no extraction cost from the directive's operation. Has institutional incentive to formalize validated survival practices but no structural benefit from the directive itself.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_municipal_authority, agenda_setter,
    institutional, biographical, mobile, regional).

% Researchers studying intergenerational disaster memory transmission. They analyze the Aneyoshi case as a rare instance of behavioral directive persistence across catastrophic intervals. They neither benefit from nor bear costs of the directive; their seat is analytical.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, disaster_anthropologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates settlement location with physical tsunami inundation boundary — a genuine information coordination problem where the cost of error is death. The stone transmits the empirically discovered safe zone across generations without requiring each generation to rediscover it through catastrophe.
% TRANSFER_FUNCTION: Moves no resources between parties. The constraint is a pure information standard: the stone marks the line; households build above it; no transfer occurs.
% ABSENT_VOICES: No excluded voices in this reading — the constraint's operation affects only Aneyoshi households (who comply voluntarily) and the municipal authority (which could formalize it). The commemorative_husk_reading would identify excluded voices: descendants of 1933 tsunami victims who might contest the stone's heritage framing, or would-be developers excluded by the commemorative designation.
% DISAPPEARANCE_RATIONALE: If the stone and its directive vanished overnight, the physical topography would remain — but the intergenerational transmission of the inundation line would be degraded. Without the marker, a future generation might rebuild in the inundation zone before the next tsunami validates the boundary anew. The world rearranges because the information coordination function would be lost, not because extraction would cease.
% FOUNDING_PROBLEM: After the 1896 Meiji-Sanriku tsunami destroyed the original coastal settlement, survivors needed a durable, non-institutional method to transmit the tsunami inundation boundary to future generations — a method that would survive institutional collapse, language change, and the 30-40 year inter-catastrophe interval.
% FOUNDING_PROBLEM_CORROBORATION: The 2011 Tohoku tsunami empirically validated the directive: houses above the stone survived; the inundation line matched the stone's position. This corroboration comes from physical geography, not from the beneficiary set (which is empty in this reading). The founding problem (transmitting the boundary without institutional continuity) remains live because tsunami recurrence intervals exceed institutional memory spans.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-zero (0.02) because the constraint imposes no transfer — building above the inundation line is what topography would dictate regardless. Suppression is negligible (0.05) because no enforcement mechanism exists or is needed; the stone is a marker, not a mandate. Theater ratio is minimal (0.03) — the directive's function (survival) is its only operation. Accessibility collapse is very high (0.92) because the physical geography allows no viable alternative settlement sites below the line. Resistance is near-zero (0.08) because the constraint aligns with survival interest; the 1933 tsunami validated it empirically. These metrics are authored independently of the claimed_type; the engine will compute mountain from them.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from the structural data. For aneyoshi_households (powerless/identity_locked, local scope), directionality derives toward beneficiary (d ≈ 0.1) because the constraint subsidizes survival. For municipal_authority (institutional, regional scope), d ≈ 0.3 — the authority could formalize or ignore but bears no extraction cost. The analytical observer sees the full mountain structure. The commemorative_husk_reading would compute differently because its beneficiary/victim structure differs.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims declared in this reading — the constraint extracts from no one. The directionality derivation chain finds no structural extraction relationship. The stone's physical geography function (marking the inundation line) is the referent for ε, not any social enforcement. If the commemorative_husk_reading identifies heritage/tourism beneficiaries, that reading's ε will differ — confirming they are distinct constraints per ε-invariance.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — the constraint's founding problem (tsunami survival) remains live (2011 validation). The directive has not atrophied; its function was re-validated by the 2011 event. Mandatrophy would apply only if the stone were maintained as a ritual after the inundation risk vanished (e.g., sea wall construction eliminated the hazard).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is the Aneyoshi stone directive a single constraint or two distinct constraints (behavioral land-use restriction vs. commemorative artifact)?',
    'Compare ε values across readings: if behavioral_competence_reading yields ε ≈ 0.02 (physical geography) and commemorative_husk_reading yields substantially higher ε (social extraction), they are distinct constraints per ε-invariance principle. Empirical measurement of compliance cost and enforcement structure across the two framings would resolve.',
    'If the framings produce different ε values, they must be modeled as separate constraint stories linked by network.affects_constraints, not as one story with measurement-dependent classification. The current decomposition into two stories with distinct constraint_ids follows this principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel label ''Aneyoshi stone directive'' conflates structurally distinct constraints.').

omega_variable(
    intergenerational_transmission_mechanism,
    'What mechanism sustained the directive''s behavioral force across 78 years without formal validation — oral tradition, institutional embedding, or repeated near-miss reinforcement?',
    'Ethnographic reconstruction of Aneyoshi community decision-making 1933-2011: household interviews, municipal records, school curricula analysis. Identify whether compliance was enforced by social sanction, internalized norm, or physical salience of the stone itself.',
    'If transmission relied on active social enforcement (sanctions for building below the line), the constraint has a coordination-enforcement structure (rope/tangled_rope). If compliance was self-enforcing through physical geography salience, it remains mountain. The very low ε and suppression scores assume the latter; evidence of the former would require reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'Mechanism of behavioral persistence across the inter-catastrophe period.').

omega_variable(
    commemorative_husk_divergence,
    'Does the commemorative_husk_reading describe the same physical constraint with a different ε, or a genuinely different constraint (the stone as memorial rather than behavioral directive)?',
    'Measure whether the commemorative reading identifies beneficiaries (e.g., tourism operators, heritage agencies, municipal identity entrepreneurs) who extract value from the stone''s symbolic status while the behavioral directive has lapsed. If beneficiaries exist with asymmetric extraction, the commemorative reading is a snare/tangled_rope, not a mountain.',
    'Confirms the ε-invariance decomposition: behavioral_competence_reading = mountain (ε ≈ 0.02), commemorative_husk_reading = snare/tangled_rope (ε > 0.3). The two stories must carry distinct metrics and stakeholder sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commemorative_husk_divergence, empirical, 'Structural distinction between the two kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_tr_t1933, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1933, 0.01).
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_tr_t1960, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1960, 0.02).
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_tr_t1980, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1980, 0.02).
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_tr_t2000, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2000, 0.03).
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_tr_t2011, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2011, 0.03).
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_tr_t2024, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2024, 0.03).

% Extraction over time
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_be_t1933, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1933, 0.01).
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_be_t1960, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1960, 0.01).
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_be_t1980, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1980, 0.02).
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_be_t2000, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_be_t2011, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2011, 0.02).
narrative_ontology:measurement(aneyoshi_stone_directive_bcr_be_t2024, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2024, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__behavioral_competence_reading, 0.02).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint and commemorative_husk_reading form the aneyoshi_stone_directive constraint family. The behavioral_competence_reading models the physical geography constraint (mountain, ε ≈ 0.02, no beneficiaries). The commemorative_husk_reading models the heritage/commemoration constraint (snare/tangled_rope, ε > 0.3, beneficiaries = tourism/heritage actors). They share the same physical artifact but are distinct constraints per ε-invariance: changing the observable (survival compliance vs. heritage designation) changes ε, so they are different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
