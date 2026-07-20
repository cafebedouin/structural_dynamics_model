% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone, erected after the 1896 Sanriku tsunami and
 *   reinforced after 1933, inscribes a prohibition on building below its
 *   elevation. In the behavioral_competence_reading, this inscription is not
 *   a dead monument but a live land-use rule that was operationally enforced
 *   across 78 years (1933â2011). The constraint is a physical reality
 *   (tsunami inundation physics) transmitted through a commitment system (the
 *   inscribed stone and attendant community practice). There is no
 *   beneficiary structure: no party collects rents or status from
 *   enforcement; survival is diffuse and non-extractive. The reading competes
 *   with the commemorative_husk_reading, which treats the stone as a decayed
 *   symbol. This JSON instantiates ONLY the behavioral competence reading as
 *   a clean, Îµ-invariant constraint.
 *
 * KEY AGENTS:
 *   - Aneyoshi village community: maintains the rule through social practice and informal settlement control
 *   - Tsunami hazard: the irreducible physical constraint that makes low-elevation settlement lethal
 *   - External developers and younger households: absent voices who might prefer low-elevation building
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.04).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.06).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'b362be88-bb48-4eff-bd81-287d2fa6aa71').
narrative_ontology:cs_kernel_codification('b362be88-bb48-4eff-bd81-287d2fa6aa71', fixed_text).
narrative_ontology:cs_authority_grounding('b362be88-bb48-4eff-bd81-287d2fa6aa71', practice).
narrative_ontology:cs_interpretation_layer_present('b362be88-bb48-4eff-bd81-287d2fa6aa71').
narrative_ontology:cs_reading_relation('b362be88-bb48-4eff-bd81-287d2fa6aa71', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('b362be88-bb48-4eff-bd81-287d2fa6aa71', foundational, inscribed_rule_maintains_operational_force).
narrative_ontology:cs_axiom_status(inscribed_rule_maintains_operational_force, holdable).
narrative_ontology:cs_axiom_grounding('b362be88-bb48-4eff-bd81-287d2fa6aa71', inscribed_rule_maintains_operational_force, empirically_contingent).
narrative_ontology:cs_axiom('b362be88-bb48-4eff-bd81-287d2fa6aa71', foundational, community_practice_tracks_natural_hazard).
narrative_ontology:cs_axiom_status(community_practice_tracks_natural_hazard, holdable).
narrative_ontology:cs_axiom_grounding('b362be88-bb48-4eff-bd81-287d2fa6aa71', community_practice_tracks_natural_hazard, empirically_contingent).
narrative_ontology:cs_reference_frame('b362be88-bb48-4eff-bd81-287d2fa6aa71', live_physical_competence).
narrative_ontology:cs_drift_state('b362be88-bb48-4eff-bd81-287d2fa6aa71', post_2011_tohoku_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b362be88-bb48-4eff-bd81-287d2fa6aa71', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates village settlement location to maintain elevation above recurring tsunami inundation, solving the collective problem that individual households would preferentially build on lower, flatter coastal land.
% TRANSFER_FUNCTION: Transfers individual building-site liberty to communal survival; no material rent is extracted, but the constraint moves risk exposure from the collective to zero by prohibiting low-elevation settlement.
% ABSENT_VOICES: Younger households or external developers who might prefer low-elevation construction for economic convenience; their absence from the 1933â2011 enforcement record is notable because the constraint's legitimacy was uncontested enough that formal opposition did not register.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished, the village's settlement pattern would likely shift downhill toward the shore as households sought flatter building sites, rearranging land use and re-exposing the community to tsunami risk.
% FOUNDING_PROBLEM: The 1896 Meiji Sanriku tsunami destroyed Aneyoshi and demonstrated that settlement below a specific elevation was existentially untenable; the stone was erected to permanently fix the village's location above the inundation line.
% FOUNDING_PROBLEM_CORROBORATION: The 2011 TÅhoku tsunami provided external corroboration from geophysical reality itself: Aneyoshi survived while neighboring low-lying settlements were destroyed. This attestation comes from the hazard event, not from a party benefiting from the rule's maintenance.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.04, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.04) because no agent captures a rent; the constraint moves risk to zero without redistribution. Suppression is minimal (0.06) because enforcement is social practice rather than coercive machinery. Theater ratio is negligible (0.02) because the rule's function (survival) is inseparable from its performance. Accessibility collapse is very high (0.92): once the physics is understood, building lower is not a viable alternative. Resistance is negligible (0.03): the hazard itself obviates the need for active resistance. The metrics are flat across the 1933â2011 interval because the underlying geophysics and the community's competent response were stable.
 *
 * PERSPECTIVAL GAP:
 *   From the behavioral competence reading, the constraint appears as a transparent transmission of natural law; from the commemorative husk reading, the same stone would appear as theatrical maintenance of a dead symbol. The engine will compute seat divergences if stakeholders are ever added, but under this reading there are no distinct beneficiary and payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   With no declared beneficiaries or victims, directionality is structurally flat. All village members are symmetrically positioned with respect to the hazard; the constraint subsidizes survival universally. The physical hazard itself is the only 'agent' with non-zero directionality, and it targets those who would build low.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists mandatrophy because the constraint's persistence is justified by the continuing existence of the founding problem (tsunami risk). If the hazard were to disappear, the rule would become a piton; as of 2011, the hazard is live and the rule is functionally competent, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_commemorative_status,
    'Is the Aneyoshi stone a live behavioral rule or a commemorative husk?',
    'Ethnographic observation of actual settlement and construction decisions in Aneyoshi; measurement of whether low-elevation building proposals are actively blocked by community enforcement.',
    'If the rule is live, the constraint reads as mountain/behavioral competence; if decayed, it collapses to piton or snare under the commemorative reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_status, empirical, 'Empirical ambiguity between live rule and commemorative symbol').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Does the prohibition persist because of active social enforcement or because residents independently recognize the physical risk?',
    'Comparative analysis with villages that abandoned similar stones: if physical recognition alone suffices, abandonment should correlate with risk denial; if social enforcement is required, abandonment correlates with community dissolution.',
    'If purely physical recognition, the constraint is a pure mountain; if social enforcement is required, the constraint carries a coordination component that edges toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, conceptual, 'Whether persistence is social or physical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_bc_tr_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aneyoshi_bc_tr_t1946, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1946, 0.02).
narrative_ontology:measurement(aneyoshi_bc_tr_t1959, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1959, 0.02).
narrative_ontology:measurement(aneyoshi_bc_tr_t1972, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1972, 0.02).
narrative_ontology:measurement(aneyoshi_bc_tr_t1985, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1985, 0.02).
narrative_ontology:measurement(aneyoshi_bc_tr_t1998, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1998, 0.02).
narrative_ontology:measurement(aneyoshi_bc_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.02).

% Extraction over time
narrative_ontology:measurement(aneyoshi_bc_be_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1933, 0.04).
narrative_ontology:measurement(aneyoshi_bc_be_t1946, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1946, 0.04).
narrative_ontology:measurement(aneyoshi_bc_be_t1959, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1959, 0.04).
narrative_ontology:measurement(aneyoshi_bc_be_t1972, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1972, 0.04).
narrative_ontology:measurement(aneyoshi_bc_be_t1985, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1985, 0.04).
narrative_ontology:measurement(aneyoshi_bc_be_t1998, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1998, 0.04).
narrative_ontology:measurement(aneyoshi_bc_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.04).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_bc_su_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1933, 0.06).
narrative_ontology:measurement(aneyoshi_bc_su_t1946, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1946, 0.06).
narrative_ontology:measurement(aneyoshi_bc_su_t1959, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1959, 0.06).
narrative_ontology:measurement(aneyoshi_bc_su_t1972, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1972, 0.06).
narrative_ontology:measurement(aneyoshi_bc_su_t1985, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1985, 0.06).
narrative_ontology:measurement(aneyoshi_bc_su_t1998, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1998, 0.06).
narrative_ontology:measurement(aneyoshi_bc_su_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2011, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel decomposes into two structurally distinct constraints: the behavioral_competence_reading (low extraction, live enforcement, mountain) and the commemorative_husk_reading (high theater, decayed function, piton or snare). They share the same stone but instantiate different Îµ values and different stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
