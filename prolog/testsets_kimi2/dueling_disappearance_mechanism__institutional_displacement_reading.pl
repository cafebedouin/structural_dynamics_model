% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling Protocol: Institutional Displacement Reading
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   One reading of the contested kernel 'dueling disappearance mechanism.'
 *   This reading holds that dueling functioned as a coordination protocol for
 *   elite male dispute resolution and declined because courts, banking, and
 *   libel law provided superior coordination alternatives that voluntarily
 *   drew participants away. The constraint is the dueling protocol itself,
 *   assessed as a rope: symmetric, voluntary, and obsolete due to
 *   institutional substitution rather than coercion or cultural taboo.
 *   Dueling persisted only in institutional gaps as an
 *   available-but-disfavored option.
 *
 * KEY AGENTS:
 *   - Gentleman participants: symmetric beneficiaries who bore mutual risk in exchange for a terminal honor ritual, with improving exit options as legal and financial institutions matured.
 *   - Seconds and arbiters: beneficiaries who mediated the protocol and lost function as dispute resolution professionalized.
 *   - Excluded populations: women, commoners, and colonial subjects structurally outside the honor framework with no standing to challenge its operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling Protocol: Institutional Displacement Reading").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '7bd622aa-b5c9-48a9-8888-e294903c4b66').
narrative_ontology:cs_kernel_codification('7bd622aa-b5c9-48a9-8888-e294903c4b66', implicit).
narrative_ontology:cs_authority_grounding('7bd622aa-b5c9-48a9-8888-e294903c4b66', practice).
narrative_ontology:cs_interpretation_layer_present('7bd622aa-b5c9-48a9-8888-e294903c4b66').
narrative_ontology:cs_reading_relation('7bd622aa-b5c9-48a9-8888-e294903c4b66', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7bd622aa-b5c9-48a9-8888-e294903c4b66', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('7bd622aa-b5c9-48a9-8888-e294903c4b66', foundational, institutional_substitution_sufficiency).
narrative_ontology:cs_axiom_status(institutional_substitution_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('7bd622aa-b5c9-48a9-8888-e294903c4b66', institutional_substitution_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('7bd622aa-b5c9-48a9-8888-e294903c4b66', foundational, voluntary_exit_premise).
narrative_ontology:cs_axiom_status(voluntary_exit_premise, holdable).
narrative_ontology:cs_axiom_grounding('7bd622aa-b5c9-48a9-8888-e294903c4b66', voluntary_exit_premise, empirically_contingent).
narrative_ontology:cs_reference_frame('7bd622aa-b5c9-48a9-8888-e294903c4b66', aristocratic_honor_dispute_resolution).
narrative_ontology:cs_drift_state('7bd622aa-b5c9-48a9-8888-e294903c4b66', post_institutional_substitution_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7bd622aa-b5c9-48a9-8888-e294903c4b66', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, gentleman_participants).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, seconds_and_arbiters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elite males who used dueling to settle disputes of honor. They faced bodily risk but gained a recognized terminal ritual. Over the nineteenth century, they increasingly turned to courts, banking arrangements, and libel suits as less costly ways to resolve conflicts, and the social expectation to duel gradually relaxed.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, gentleman_participants, beneficiary,
    moderate, biographical, mobile, national).

% Friends or associates who arranged the terms of a duel, chose weapons, and ensured fair conduct. They held trusted positions within elite social networks and saw their mediation role become unnecessary as professional lawyers and bankers took over dispute management.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, seconds_and_arbiters, beneficiary,
    moderate, biographical, mobile, national).

% Women, commoners, and colonial subjects who were entirely outside the honor code. They could not issue or receive challenges and had no standing in the protocol, though they sometimes suffered indirect effects from the violence and impunity of elite males.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, excluded_populations, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates reciprocal dispute resolution among elite males by providing a ritualized, bounded terminal mechanism that replaces indefinite feuding and bypasses slow or class-biased state courts.
% TRANSFER_FUNCTION: Moves bodily risk and honor status between disputing parties; moves social authority over resolution from public institutions to private seconds and the duelists themselves.
% ABSENT_VOICES: Women, commoners, colonial subjects, and pacifist religious voices were excluded from the honor framework. They would have contested the legitimacy of private violence and the exclusionary definition of honor had they been present in the deliberation.
% DISAPPEARANCE_RATIONALE: If the dueling protocol disappeared, elite disputants would immediately reroute into courts, banking guarantees, and libel actions; the social role of the second would dissolve; the entire topology of masculine honor maintenance would shift from violent private ritual to institutional process.
% FOUNDING_PROBLEM: Early modern state courts were slow, corrupt, or inaccessible for personal honor disputes; informal vengeance produced open-ended violence; dueling provided a contained, reciprocal, terminal alternative.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and jurists attesting to expanded court accessibility and libel remedies; economic historians documenting banking and credit-based reputation systems; these sources stand outside the community of gentleman duelists and corroborate that the founding problem has been institutionally resolved.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) because dueling imposed symmetric costs and benefits on participants with no concentrated extractor. Suppression is low (0.12) because persistence relied on social honor enforcement rather than active coercion. Theater ratio rises across the interval (0.10 to 0.48) because late-stage dueling became increasingly performative and vestigial as functional dispute resolution migrated to institutions. Accessibility collapse is moderate (0.30): once courts and banking were understood as available, the dueling alternative lost appeal but did not become physically inaccessible. Resistance is moderate-low (0.25): church and state opposition existed but is not read as the driver of decline. The metrics and the claimed type are authored independently: the rope claim reflects the symmetric voluntary structure, while the theater trajectory records honest functional decay.
 *
 * PERSPECTIVAL GAP:
 *   Participants in the honor culture experienced the protocol as essential masculine maintenance; outside observers and later generations viewed it as archaic violence. Under this reading, both perspectives describe the same coordination structure, but the institutional-alternative reading bridges the gap by showing that participants themselves abandoned the ritual when lower-cost coordination became available. The engine will compute per-seat directionality as near-symmetric for participants and excluded for non-elites.
 *
 * DIRECTIONALITY LOGIC:
 *   Gentleman participants and their seconds sit near the symmetric center (d â 0.5) because costs and benefits were mutual and no party captured a unilateral transfer. As institutional alternatives matured, exit options improved from constrained toward mobile, pushing both seats slightly toward the beneficiary end over time. Excluded populations have no directionality within the constraint because they were not parties to it; their structural relationship is external exclusion rather than targeted extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunreliable state courts and open-ended vengeanceâwas fully absorbed by courts, banking, and libel law. The constraint persisted briefly as a vestigial practice but without active maintenance or concentrated beneficiaries, so it registers as a dead mandate with rope obsolescence rather than mandatrophy requiring enforcement. The rising theater ratio captures the lag between functional death and behavioral disappearance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_relation,
    'Does the institutional substitution reading logically foreclose the cultural contraction reading, or can they coexist within a single historiographic framework?',
    'Archival analysis of whether anti-dueling statutes and banking expansion preceded or followed the internalization of bourgeois dignity norms in gentleman correspondence.',
    'If cultural taboo preceded institutional capacity, the displacement reading is incomplete and the contraction reading gains necessity; if institutions alone explain the timing, the composite reading overcounts causes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_relation, conceptual, 'Structural relation between institutional and cultural disappearance mechanisms').

omega_variable(
    voluntary_vs_coerced_decline,
    'Was the abandonment of dueling driven by voluntary substitution toward superior institutions, or by legal prohibition that created coerced exit?',
    'Prosecution records and gentleman diaries: compare the chronology of dueling''s decline against the chronology of actual anti-dueling enforcement.',
    'Coerced exit would introduce victims and active enforcement, reclassifying the late-stage constraint toward tangled_rope or snare; voluntary exit preserves rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coerced_decline, empirical, 'Whether decline was voluntary substitution or coerced suppression').

omega_variable(
    institutional_gap_persistence,
    'Does dueling persist as a live dispute-resolution option in contemporary institutional gaps?',
    'Ethnographic and criminological study of extralegal communities with low state presence.',
    'Persistence would indicate the coordination function remains live in bounded scopes despite apparent obsolescence in the core domain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_gap_persistence, empirical, 'Contemporary persistence of dueling in extralegal gaps').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(duel_tr_t16, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(duel_tr_t32, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 32, 0.24).
narrative_ontology:measurement(duel_tr_t48, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 48, 0.33).
narrative_ontology:measurement(duel_tr_t64, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 64, 0.41).
narrative_ontology:measurement(duel_tr_t80, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 80, 0.48).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(duel_be_t16, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(duel_be_t32, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 32, 0.16).
narrative_ontology:measurement(duel_be_t48, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 48, 0.15).
narrative_ontology:measurement(duel_be_t64, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 64, 0.14).
narrative_ontology:measurement(duel_be_t80, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 80, 0.13).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dueling_disappearance_mechanism__institutional_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
