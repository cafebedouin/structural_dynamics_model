% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: State Practice Displacement via Endogenous Adoption Pathways (Internalization Reading)
 *   domain: political_history/cultural_formation
 *
 * SUMMARY:
 *   A state issues decrees mandating the adoption of new cultural practices—a
 *   reformed calendar replacing lunar observation, standardized dress codes,
 *   a single administrative language—as part of centralization and
 *   modernization. This constraint models the endogenous_climb_reading: the
 *   claim that imposed practices fail to displace prior ones unless
 *   communities internalize them through their own adoption pathways. The
 *   state achieves nominal compliance (the practice is used in official
 *   contexts) but discovers that enforcement costs rise as communities
 *   develop dual systems (public compliance, private retention). The gap
 *   between the claimed type (tangled_rope: coordination of administrative
 *   uniformity, enforcement of state mandates) and the authored metrics (high
 *   extractiveness, rising theater ratio, high resistance) models the
 *   structural divergence this reading names: the state coordinates a genuine
 *   administrative need but extracts cultural authority, and the extraction
 *   increasingly takes theatrical form as enforcement machinery discovers it
 *   cannot compel internalization itself.
 *
 * KEY AGENTS:
 *   - state_administrative_center: Central authority issuing the decree; discovers enforcement without buy-in fails; benefits from nominal uniformity but loses legitimacy as enforcement costs escalate.
 *   - communities_preserving_autonomy: Organized resistance maintaining dual systems; retains autonomy and cultural continuity; bears cost of hidden compliance.
 *   - local_practice_keepers: Custodians of prior practices; identity-locked into preservation role; central to whether internalization can be avoided.
 *   - urban_assimilationists: Adopters of the new practice for status/pragmatic gain; represent the state's hope that endogenous pull can succeed; located in contexts where adoption is structurally easiest.
 *   - enforcement_apparatus: Discovers that coercion alone cannot achieve internalization; shift toward theatrical displays of enforcement.
 *   - rival_state_authorities: Regional powers excluded from governance of the new practice; their legitimacy claims rested on the prior practice; structurally barred from contesting the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.71).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "State Practice Displacement via Endogenous Adoption Pathways (Internalization Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/cultural_formation").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '85c671d5-e6cf-46e0-b205-5140c6d20a0c').
narrative_ontology:cs_kernel_codification('85c671d5-e6cf-46e0-b205-5140c6d20a0c', fixed_text).
narrative_ontology:cs_authority_grounding('85c671d5-e6cf-46e0-b205-5140c6d20a0c', extraction).
narrative_ontology:cs_interpretation_layer_present('85c671d5-e6cf-46e0-b205-5140c6d20a0c').
narrative_ontology:cs_reading_relation('85c671d5-e6cf-46e0-b205-5140c6d20a0c', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('85c671d5-e6cf-46e0-b205-5140c6d20a0c', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('85c671d5-e6cf-46e0-b205-5140c6d20a0c', foundational, internalization_cannot_be_decreed).
narrative_ontology:cs_axiom_status(internalization_cannot_be_decreed, holdable).
narrative_ontology:cs_axiom_grounding('85c671d5-e6cf-46e0-b205-5140c6d20a0c', internalization_cannot_be_decreed, empirically_contingent).
narrative_ontology:cs_axiom('85c671d5-e6cf-46e0-b205-5140c6d20a0c', foundational, legitimacy_requires_endogenous_adoption).
narrative_ontology:cs_axiom_status(legitimacy_requires_endogenous_adoption, holdable).
narrative_ontology:cs_axiom_grounding('85c671d5-e6cf-46e0-b205-5140c6d20a0c', legitimacy_requires_endogenous_adoption, deontological).
narrative_ontology:cs_reference_frame('85c671d5-e6cf-46e0-b205-5140c6d20a0c', state_decree_sufficiency_for_cultural_uniformity).
narrative_ontology:cs_drift_state('85c671d5-e6cf-46e0-b205-5140c6d20a0c', post_failed_displacement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('85c671d5-e6cf-46e0-b205-5140c6d20a0c', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, local_practice_keepers).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, centralized_administrative_uniformity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics model a constraint that begins as a genuine coordination problem (low extractiveness, low theater at t=0) but increasingly reveals extraction and performance as time passes. Extractiveness rises from 0.35 to 0.58 because the state discovers that nominal compliance does not yield real administrative uniformity—communities maintain private systems, requiring continuous monitoring and punishment. Theater ratio rises from 0.18 to 0.62 (peaking at t=32 before declining) because enforcement becomes increasingly performative: public displays of conformity are maintained while private practice persists. Suppression rises from 0.45 to 0.78 because the constraint's persistence depends not on agreement but on escalating coercive machinery. The slight decline in both extractiveness and theater at t=40 models the turning point where some communities achieve genuine internalization (especially urban cohorts and younger generations), partially releasing enforcement pressure. The one shared time grid spans all three metrics; each is authored at every time point so temporal analysis can detect the simultaneous rise in all three (the signature of a constraint shifting from coordination to extraction to performance).
 *
 * PERSPECTIVAL GAP:
 *   The state agenda-setter and the enforcement apparatus experience this constraint as a coordination problem with persistent compliance deficits; they read failure as incomplete enforcement or insufficient ideological work. Communities preserving autonomy and local practice-keepers experience it as extraction of cultural authority masked as modernization; they read the constraint as a snare using a coordination frame to justify erasure. Urban assimilationists and the state-aligned ideological apparatus experience it as partial success—the imposed practice is adopted in their contexts, internalization is proceeding—but this perception depends on their structural position in contexts where adoption is already economically rational and socially mobile. The engine computes these divergent classifications from the structural data: the state's seat likely computes as rope (genuine coordination with enforcement costs) while the community seats compute as snare (extraction defended as administration), and the urban seats compute as rope (genuine coordination they have autonomously adopted). These per-seat divergences are the measurement this story exists to reveal.
 *
 * DIRECTIONALITY LOGIC:
 *   The state administrative center is the agenda-setter (d near 0.0: sets the rules, enforces them, benefits from nominal uniformity). Communities preserving autonomy are organized victims (d near 1.0: bear enforcement costs, must hide their practices, constrained exit). Local practice-keepers are identity-locked victims (d near 0.95: their social role depends on practice persistence; internalization would erase that role). Urban assimilationists are near-symmetric beneficiaries (d near 0.3: some genuine benefit from adoption, but also structural pressure to conform). The enforcement apparatus is an institutional payer (d near 0.8: bears escalating cost of surveillance and punishment, has limited exit). Rival state authorities are trapped victims (d near 1.0: their legitimacy claims are foreclosed by the new practice, cannot exit but also cannot accept). The directionality logic directly follows from beneficiary/victim declarations and exit options; no overrides are needed—the derivation chain produces defensible d values from the structural data as authored.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy classification by maintaining a clear founding problem and founding_problem_status. The state's mandate—administrative uniformity for bureaucratic efficiency—is live and observable. However, the analysis documents a critical gap: the state conflates two distinct problems: (1) administrative standardization (solvable through translation layers, dual-system management, shared time-keeping protocols) and (2) cultural internalization (which requires bottom-up adoption). This reading claims that (2) cannot be decreed successfully; it can only happen through endogenous pathways. Thus the constraint is NOT mandatrophy (mandate has not outlived its function; the state's need for administrative uniformity is real). However, the rising theater_ratio and the eventual plateau of extractiveness suggest that the state is increasingly using enforcement to maintain a symbolic appearance of uniformity rather than achieving real administrative integration. This is the theater of administration, not mandate obsolescence—the state still needs uniformity but is discovering that coercion cannot supply internalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_mechanism_ambiguity,
    'What constitutes genuine internalization of an imposed practice? Is it behavioral adoption, belief alignment, or social identity fusion? Does internalization require all three or only one?',
    'Longitudinal ethnographic study tracking adoption trajectories: measure behavior (public compliance), expressed belief (private attitudes), and identity markers (social role changes). Compare communities where practices persisted despite enforcement to those where practices were genuinely abandoned.',
    'If internalization requires all three and communities achieve only behavioral compliance, the constraint''s extractiveness is measured correctly and the endogenous claim holds. If any single dimension suffices, the state''s enforcement might be achieving partial internalization faster than the timeline suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_mechanism_ambiguity, empirical, 'The multidimensional structure of internalization and its measurement.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression structural (external barriers, enforcement machinery, identity loss from non-compliance) or internalized (communities believe the new practice is superior, or shame prevents return to prior practice)?',
    'Post-enforcement suppression trajectory: if the state abandoned enforcement, would the prior practice resurface immediately (structural suppression) or remain suppressed through internalized preference (internalized suppression)? Track communities where enforcement relapsed and observe re-adoption speeds.',
    'If suppression is structural, removal of enforcement would quickly restore prior practice, supporting the endogenous claim. If internalized, the state has achieved some genuine internalization despite what enforcement machinery alone would suggest. Measurement would remain accurate but interpretation would shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is enforced from outside or carried internalized by communities post-enforcement.').

omega_variable(
    urban_vs_rural_divergence_in_internalization,
    'Does internalization follow a spatial pattern: urban adoption is genuine and endogenous (mobile, economically rational, status-seeking), while rural persistence reflects structural suppression and identity-lock?',
    'Spatial decomposition: measure adoption rates and extraction levels separately in urban and rural contexts. Track whether urban assimilation is maintained after enforcement fades (genuine internalization) or collapses (urban adoption was performative, dependent on enforcement).',
    'If spatial divergence is real, the constraint is actually two constraints (urban endogenous adoption vs. rural extraction). If the divergence is artifactual (both are equally dependent on enforcement, just with different performance styles), the endogenous-climb reading overstates the heterogeneity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(urban_vs_rural_divergence_in_internalization, empirical, 'Spatial heterogeneity in internalization dynamics: true endogenous adoption vs. enforced performance.').

omega_variable(
    practice_keeper_identity_lock_resistance,
    'How much of the resistance to the imposed practice comes from local practice-keepers'' identity fusion (they cannot accept internalization without ceasing to exist as keepers) versus from genuine community commitment to the prior practice?',
    'Separate practice-keeper resistance from community resistance: measure adoption rates in communities where practice-keepers have died or emigrated versus those where they remain. Assess whether community internalization accelerates after keeper-generation transition.',
    'If keeper resistance is the primary barrier and community adoption would proceed after keeper exit, the constraint''s extractiveness overstates cultural resistance and understates the rate of genuine internalization. If community resistance persists after keeper exit, identity-lock is not the primary driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_keeper_identity_lock_resistance, empirical, 'The role of practice-keeper identity-fusion in sustaining resistance.').

omega_variable(
    kernel_reading_contention_naturalization_vs_constitution,
    'Is this constraint''s classification as one reading of a contested kernel itself contestable? Do the three readings (endogenous, exogenous, hybrid) genuinely carve up one underlying process, or do they describe three genuinely distinct constraints?',
    'Meta-analysis: for each reading, specify what would falsify it. If a finding (e.g., rapid generational turnover in adopting communities) would support both the endogenous and hybrid readings simultaneously, they are not genuinely distinct readings of one kernel but rather different emphasis on different causal stages of one process.',
    'If the readings are genuinely distinct kernels with distinct ε values, the constraint family should be decomposed further. If they are emphasis-variants of one underlying process, the kernel framing is defensible and the reading relations are correctly specified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contention_naturalization_vs_constitution, conceptual, 'Whether the three readings are distinct kernels or variants of a single process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 32, 0.68).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'legitimacy_of_imposed_practice'. The endogenous_climb_reading claims that practice displacement requires bottom-up internalization; exogenous_override claims decree alone suffices; hybrid_scaffolding claims scaffolded messaging accelerates endogenous pull. All three readings address the same empirical domain (state-mandated cultural change) but decompose the causal structure differently. The endogenous reading's ε is higher than the exogenous reading's (because it claims coercion fails without internalization, making extraction visible), and lower than the hybrid reading's (because the hybrid reading names the contradictions between top-down and bottom-up, modeling both simultaneously). The readings are linked by network.affects_constraints to enable contamination analysis: if one reading's empirical support weakens, downstream readings must be reassessed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
