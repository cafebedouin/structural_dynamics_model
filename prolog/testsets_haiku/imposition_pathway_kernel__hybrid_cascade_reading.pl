% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: State-Mandated Fringe Creation & Organic Adoption Cascade
 *   domain: social/historical/institutional
 *
 * SUMMARY:
 *   This reading models commitment displacement as a two-stage process:
 *   top-down imposition (state decree mandating practice adoption within
 *   state and military employment) initiates a manufactured fringe of coerced
 *   practitioners; this artificial fringe then becomes the vector for organic
 *   cascade as state employees and military personnel carry internalized
 *   practice into civilian society and peripheral regions. The Meiji
 *   Restoration's adoption of Western administrative and legal forms
 *   exemplifies this pattern: the decree mandated adoption within government
 *   and military; the fringe of trained state employees then became the model
 *   and teacher for organic civilian adoption over the following decades.
 *   This reading is distinct from both the endogenous-only reading (which
 *   sees all adoption as fringe climb, with the initial mandate as a
 *   compressed fringe stage) and the exogenous-only reading (which treats
 *   top-down override as a completely distinct mechanism requiring separate
 *   M-set classification). The hybrid reading captures the empirical pattern:
 *   override initiates, fringe carries, organic cascade completes.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: institutional agenda-setter; issues the initial decree
 *   - military_institutional_power: institutional beneficiary and primary organic vector; military hierarchy produces internal adoption momentum
 *   - state_employees: moderate-power payers initially, become bifurcated into genuine practitioners (beneficiaries) and displaced traditional practitioners (victims)
 *   - military_conscripts_and_personnel: powerless and trapped; coerced adoption becomes internalized through military training; conscript discharge creates organic carriers
 *   - civilian_early_adopters: powerful beneficiaries; voluntary adoption creates legitimacy cascade
 *   - excluded_traditional_practitioners: identity-locked victims; crowded out by state-induced normalization
 *   - rural_and_peripheral_populations: powerless payers; adopt through cascade pressure with minimal benefit
 *   - institutional_scholars_and_historians: observer seat; provide external corroboration of the mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.68).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.72).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "State-Mandated Fringe Creation & Organic Adoption Cascade").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "social/historical/institutional").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, 'c9390298-3f0f-4478-bc80-98f3d11cc437').
narrative_ontology:cs_kernel_codification('c9390298-3f0f-4478-bc80-98f3d11cc437', formalized).
narrative_ontology:cs_authority_grounding('c9390298-3f0f-4478-bc80-98f3d11cc437', extraction).
narrative_ontology:cs_interpretation_layer_present('c9390298-3f0f-4478-bc80-98f3d11cc437').
narrative_ontology:cs_reading_relation('c9390298-3f0f-4478-bc80-98f3d11cc437', imposition_pathway_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9390298-3f0f-4478-bc80-98f3d11cc437', imposition_pathway_kernel__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('c9390298-3f0f-4478-bc80-98f3d11cc437', foundational, override_fringe_cascade_coupling).
narrative_ontology:cs_axiom_status(override_fringe_cascade_coupling, holdable).
narrative_ontology:cs_axiom_grounding('c9390298-3f0f-4478-bc80-98f3d11cc437', override_fringe_cascade_coupling, empirically_contingent).
narrative_ontology:cs_axiom('c9390298-3f0f-4478-bc80-98f3d11cc437', secondary, internalization_completes_displacement).
narrative_ontology:cs_axiom_status(internalization_completes_displacement, holdable).
narrative_ontology:cs_axiom_grounding('c9390298-3f0f-4478-bc80-98f3d11cc437', internalization_completes_displacement, empirically_contingent).
narrative_ontology:cs_reference_frame('c9390298-3f0f-4478-bc80-98f3d11cc437', state_override_as_initiator).
narrative_ontology:cs_drift_state('c9390298-3f0f-4478-bc80-98f3d11cc437', cascade_completion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c9390298-3f0f-4478-bc80-98f3d11cc437', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, military_institutional_power).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, coerced_adopters).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, excluded_traditional_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high at t=0 (0.85) because the initial decree is coercive, decoupled from beneficiary demand, and dependent on suppression. It falls over the interval (reaching 0.68 at t=40) because as organic adoption spreads, coercive extraction becomes less necessary — the practice becomes normalized and self-perpetuating. However, it does not fall below 0.62 because even in the cascade phase, early adopters and state-aligned actors continue to extract advantage (status, access, authority) from controlling the standard. The rise to 0.68 at t=40 reflects the constraint stabilizing at a new equilibrium where the practice is no longer imposed but remains asymmetrically benefiting state-aligned practitioners. Suppression mirrors this trajectory: highest at imposition (0.95), declining through the cascade phase (0.65 at t=25) because organic adoption reduces enforcement costs, then rising slightly (0.72 at t=40) as the constraint stabilizes and active enforcement becomes periodic maintenance. Theater ratio rises gradually from 0.15 to 0.28 as the constraint ages: the initial legitimate coordination need (unified administrative standards) is real, but by t=40 much of the enforcement activity is theatrical maintenance of state authority rather than functional coordination. The shared time grid ensures every metric is authored at every examined point; measurements capture the compressed climb phase and the organic cascade completion.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus perspective, this is coordination: creating unified administrative capacity across dispersed populations with no prior infrastructure. From the coerced adopter perspective, this is imposition: the timing, mechanism, and practice standard are chosen by the state without consultation. From the excluded traditional practitioner perspective, this is displacement: their practice loses legitimacy through state-induced cascade, not through superior function. From the civilian early adopter perspective, this is opportunity: voluntary adoption of a practice the state signals as authoritative creates status and access advantage. The engine computes per-seat classifications from directionality; the perspectival gap is structural, not subjective.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: d ≈ 0.1 (full beneficiary — captures legitimacy and authority from fringe creation and cascade completion). Military institution: d ≈ 0.15 (strong beneficiary — gains institutional capacity and internal coordination efficiency). State employees: d ≈ 0.6 initially (coerced adopters bear adoption costs), bifurcating to d ≈ 0.3 for internalized practitioners (moderate benefit) and d ≈ 0.8 for displaced traditional practitioners (victims). Military conscripts: d ≈ 0.95 initially (full targets of coercion, trapped exit), declining to d ≈ 0.4 over time as internalization creates genuine practice identification. Early civilian adopters: d ≈ 0.2 (beneficiaries from voluntary adoption and status capture). Peripheral populations: d ≈ 0.65 (coerced through cascade pressure, constrained exit, diffuse costs). The directionality profile captures the hybrid mechanism: initial override is highly coercive (high d for targets), but organic cascade bifurcates the adopter population into genuine practitioners (lower d) and excluded traditionalists (higher d).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (unified administrative standards remain necessary) and the constraint persists (organic adoption is real, not theatrical). However, mandatrophy risk emerges at t=25-40: once organic adoption is complete and the practice is normalized, the constraint's original coordination function is accomplished, but enforcement machinery persists. The rise in theater_ratio (0.15→0.28) signals that enforcement is increasingly defensive (maintaining state control of the practice standard) rather than functional (creating coordination). The stabilization of extractiveness at 0.68 (rather than falling further) indicates that even in steady state, the constraint continues to extract asymmetric advantage for state-aligned practitioners. This is not mandatrophy yet (the founding problem is still live), but the trajectory suggests risk: if organic adoption becomes complete enough that enforcement becomes purely theatrical, and if the founding coordination need is solved, the constraint could degrade into piton (atrophied function, maintained through inertia). The measurement series documents this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_initiates_fringe_cascade,
    'Is the organic cascade that follows the initial override dependent on the artificial fringe created by the override, or would organic adoption have proceeded at the same rate without the mandated fringe stage?',
    'Comparative historical analysis: examine cases where top-down commitment impositions were issued without the artificial fringe creation (pure exogenous override) and compare adoption timelines to cases with manufactured fringe. Natural experiments from failed impositions that did not create enforced fringe populations would be decisive.',
    'If cascade is dependent on artificial fringe, the hybrid reading is correct and override/fringe/cascade form a single mechanism. If cascade proceeds at comparable rates without enforced fringe, the override and cascade are separable mechanisms and the exogenous_override_reading captures the structural truth. This test is decisive for the kernel''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_initiates_fringe_cascade, empirical, 'Whether organic cascade depends on the artificial fringe created by the override, or whether override and cascade are structurally separable.').

omega_variable(
    fringe_internalization_mechanism,
    'When state employees and conscripted military personnel adopt the mandated practice, are they adopting because they internalize the practice''s legitimacy and function, or are they adopting because they are trapped in enforcement regimes and performing conformity?',
    'Post-discharge behavior of military conscripts and retired state employees: do they continue practicing after enforcement ends? High continuation rates (>60%) indicate internalization; low rates indicate performative adoption. Longitudinal cohort studies tracking conscript cohorts across the conscription-to-civilian transition provide this data.',
    'If internalization is real (genuine practice identification), the fringe becomes a true organic vector and the cascade is self-sustaining. If adoption is performative (conformity under enforcement), the constraint''s organic component is weaker than the cascade trajectory suggests, and post-enforcement decay is likely. This informs the cascade''s durability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_internalization_mechanism, empirical, 'Whether the fringe''s adoption of the mandated practice is genuine internalization or performative conformity under enforcement.').

omega_variable(
    coexistence_or_displacement_of_tradition,
    'Does the state-mandated practice displace traditional practice (one replaces the other), or do they coexist in parallel institutional structures?',
    'Ethnographic and archival evidence: document whether traditional practitioners and their practices continue after the state-mandated alternative becomes normative. High coexistence (traditional practice persists in non-state institutional contexts) vs. displacement (traditional practice disappears or becomes marginal). The excluded_traditional_practitioners seat''s trajectory is diagnostic.',
    'If displacement is real, the constraint''s extractiveness is asymmetric (state-aligned practitioners gain, traditional practitioners lose). If coexistence is real, the constraint is less extractive and less suppressive than the metric profile suggests. This informs the constraint''s classification stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coexistence_or_displacement_of_tradition, empirical, 'Whether the state-mandated practice displaces traditional practice or coexists with it.').

omega_variable(
    hybrid_vs_endogenous_cliff_test,
    'Is there a observable discontinuity in adoption rates at the point when the artificial fringe becomes large enough to serve as a normalization vector (the cliff between imposition-driven and cascade-driven adoption)?',
    'Adoption rate time series: plot the rate of new adopters (derivatives of cumulative adoption) and look for a kink or inflection point where adoption accelerates from the mandatory stage to the voluntary stage. A sharp kink indicates the cliff and supports the hybrid reading; a smooth curve without discontinuity supports the endogenous reading (smooth fringe climb throughout).',
    'A detected cliff-point at the fringe size threshold would be strong evidence for the hybrid reading and decisively refute the endogenous_climb reading''s claim that all adoption follows fringe dynamics. This is the most discriminating test between these two readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_vs_endogenous_cliff_test, empirical, 'Whether there is an observable discontinuity in adoption rates when artificial fringe becomes large enough to serve as a normalization vector.').

omega_variable(
    suppression_internalization_split,
    'In the coerced adopters (state employees and military conscripts), how much of the measured suppression is structural (external enforcement) vs. internalized (the adopter believes they deserve the treatment or have fused their identity with the practice)?',
    'Post-enforcement behavior and exit: if suppression is structural, enforcement removal should produce rapid behavioral change and practice abandonment. If suppression is internalized, abandonment should be slower and resistance should be low even when enforcement is removed. Tracking adopters across enforcement-intensive and enforcement-light periods provides this signal.',
    'If internalization is high, the cascade''s organic character is stronger and the constraint''s self-perpetuation is more durable. If suppression is purely structural, removal of enforcement should halt organic adoption and begin decomposition. This informs the constraint''s stability classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Whether suppression of the coerced adopters is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(impo_tr_t5, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(impo_tr_t25, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(impo_be_t5, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 5, 0.79).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 10, 0.73).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(impo_be_t25, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(impo_su_t5, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 5, 0.88).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 10, 0.81).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(impo_su_t25, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__hybrid_cascade_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the imposition_pathway_kernel. The kernel is the contested claim about commitment displacement mechanisms. The hybrid_cascade_reading models top-down imposition as creating artificial fringe (state-mandated adoption within state and military employment) which then becomes the vector for organic adoption. The endogenous_climb_reading interprets all adoption as fringe-based (the initial override is a compressed fringe stage with invisible prior stages). The exogenous_override_reading treats the override as a distinct mechanism independent of fringe dynamics. These are three structurally distinct claims about the same historical process, each generating a different epsilon-invariant constraint with different ε values and different stakeholder structures. All three readings are linked via network.affects_constraints because they compete to explain the same phenomenon and their truth values are mutually constraining.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
