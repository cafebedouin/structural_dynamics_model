% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: State Capacity Exogenous Commitment Override (Meiji Reading)
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   In 1872, the Meiji state decreed the adoption of the Gregorian calendar,
 *   abolishing the lunar calendar that had structured timekeeping,
 *   agriculture, religious observance, and craft production for centuries.
 *   This decree had NO meaningful fringe-adoption pathway preceding it—no
 *   merchant networks or modernizing communities had already shifted to the
 *   Gregorian calendar as a voluntary adoption climb. The change was imposed
 *   top-down through administrative enforcement: state employees and military
 *   conscripts were mandated to comply; rural communities and religious
 *   practitioners faced administrative penalty (tax reassessment,
 *   conscription pressure, denial of services) for noncompliance. This
 *   constraint instantiates the 'exogenous override' reading of the
 *   imposition-pathway kernel: state capacity alone enabled commitment
 *   displacement without bottom-up emergent adoption. The fringe that did
 *   form (military, state employees) was created BY the state as enforcement
 *   apparatus, not as pre-decree voluntary adopters. The constraint is
 *   CLAIMED as tangled_rope (real coordination function: unified state
 *   administration) while the metrics record the high extraction (cultural
 *   authority transfer) and sustained suppression (continuous enforcement
 *   against noncompliance) required to hold the arrangement. The measurement
 *   series spans from pre-decree baseline (1868) through the 1872 edict and
 *   28 years of enforcement, showing the sharp extraction and suppression
 *   spike at decree and the sustained plateau thereafter—the constraint does
 *   not settle into self-sustaining equilibrium.
 *
 * KEY AGENTS:
 *   - meiji_central_state — institutional agenda-setter; controls both the decree and the enforcement machinery that makes alternatives unviable
 *   - rural_communities — powerless payers; bear the extraction (disrupted agricultural timekeeping, administrative pressure) without participation in policy formation
 *   - religious_practitioners — constrained payers; lose authority over ritual timekeeping and suffer cultural marginalization
 *   - state_employees and military — beneficiaries created by the state apparatus itself; their early adoption is mandated, not emergent; they benefit from alignment with state ideology
 *   - merchant_networks — institutional beneficiaries with pre-existing arbitrage options; they were already climbing toward this commitment; the decree removes friction for them without coercing them
 *   - fringe_adoption_theorists — analytical observers; must account for zero observable fringe adoption prior to decree
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.68).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.81).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "State Capacity Exogenous Commitment Override (Meiji Reading)").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, '2196ff80-2821-4b89-9408-041620722aa6').
narrative_ontology:cs_kernel_codification('2196ff80-2821-4b89-9408-041620722aa6', formalized).
narrative_ontology:cs_authority_grounding('2196ff80-2821-4b89-9408-041620722aa6', extraction).
narrative_ontology:cs_interpretation_layer_present('2196ff80-2821-4b89-9408-041620722aa6').
narrative_ontology:cs_reading_relation('2196ff80-2821-4b89-9408-041620722aa6', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('2196ff80-2821-4b89-9408-041620722aa6', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('2196ff80-2821-4b89-9408-041620722aa6', foundational, state_capacity_enables_override).
narrative_ontology:cs_axiom_status(state_capacity_enables_override, holdable).
narrative_ontology:cs_axiom_grounding('2196ff80-2821-4b89-9408-041620722aa6', state_capacity_enables_override, empirically_contingent).
narrative_ontology:cs_axiom('2196ff80-2821-4b89-9408-041620722aa6', foundational, fringe_adoption_not_prerequisite).
narrative_ontology:cs_axiom_status(fringe_adoption_not_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('2196ff80-2821-4b89-9408-041620722aa6', fringe_adoption_not_prerequisite, empirically_contingent).
narrative_ontology:cs_reference_frame('2196ff80-2821-4b89-9408-041620722aa6', unified_administrative_temporality).
narrative_ontology:cs_drift_state('2196ff80-2821-4b89-9408-041620722aa6', contemporary_post_meiji, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('2196ff80-2821-4b89-9408-041620722aa6', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, centralized_governance_ideology).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, subnational_communities).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, religious_practitioners).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, traditional_craft_guilds).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the state appropriates cultural authority from subnational communities; the constraint transfers the legitimacy to set time from religious and local actors to the central apparatus. Suppression is higher still (0.81) because the commitment persists entirely through active enforcement—police and administrative apparatus must continuously enforce compliance, deny services to noncompliers, and reassess taxes on basis of calendar conformity. If enforcement relaxed, rural communities would immediately revert to lunar calendar. Theater is moderate-low (0.28 by 1900): the legitimate coordination function (unified state administration, international legitimacy) is real and functions; but an increasing share of enforcement activity after 1880 is defensive—maintaining the commitment against passive resistance, not solving the original coordination problem. The measurement series show: (1) a sharp transition at the 1872 decree (extractiveness jumps from 0.12 to 0.58); (2) suppression requirement stays high and flat across the interval—the constraint does not naturalize or become self-sustaining; (3) theater ratio rises gradually as the coordination function is consolidated and routine, but suppression remains necessary. This is the structural signature of exogenous override: extraction without gradual climb.
 *
 * PERSPECTIVAL GAP:
 *   From the state institutional seat, the constraint solves a genuine administrative problem (fragmented timekeeping) and the extraction is justified as the cost of coordination. From the powerless community seat, the constraint is coercive cultural displacement with no emergent fringe-adoption history to justify it as 'natural.' The engine should flag this divergence as seat-dependent classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The state is a full beneficiary (d near 0.0): it appropriates cultural authority, consolidates control, gains administrative efficiency. Powerless rural communities are full targets (d near 1.0): they lose autonomy, face administrative penalty, have no exit (trapped). Religious practitioners are high targets (d ~0.85): they lose institutional authority over timekeeping. State employees and military are beneficiaries (d near 0.0): they gain career advancement, ideological alignment, integration into state system. Merchant networks are near-beneficiary (d near 0.2): they benefit from the coordination without coercion—they had arbitrage options and the decree removes transaction-cost friction rather than imposing new burden. The directionality of each seat differs because the constraint's effect differs: for some it is imposed against will, for others it removes friction from pre-existing preference.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative friction from calendar fragmentation) is LIVE—it was real and the decree solved it. But the solution was exogenous override, not emergent climb. The distinction matters for mandatrophy: a commitment that displaces through fringe adoption carries its own reproductive logic (early adopters normalize the practice, climb continues). An exogenously imposed commitment carries no reproductive logic—it persists only through continuous enforcement. If the founding problem decays (if Japan's governance becomes more decentralized, if international pressure for unified timekeeping relaxes), the constraint has no fallback equilibrium; it must be actively maintained or abandoned. The measurement series showing sustained high suppression through 1900 reflects this: the constraint is not settling into self-sustaining coordination; it requires continuous active enforcement to prevent reversion to lunar calendars. This is the structural signature that distinguishes exogenous override from rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    invisible_fringe_adoption,
    'Did merchant networks, urban intellectuals, or any proto-modernizing community begin adopting the Gregorian calendar before the 1872 decree, such that the decree was a compression of an already-climbing fringe adoption?',
    'Archival examination of pre-1872 timekeeping practices in merchant networks, intellectual circles, and urban centers. Check merchant contracts, temple records, guild documents, and intellectual correspondence for voluntary Gregorian adoption.',
    'If fringe adoption is empirically absent, the exogenous_override reading is confirmed and the fringe-adoption framework is incomplete. If fringe adoption is present but small, the hybrid_cascade reading gains traction—the state amplified and accelerated what was already beginning. If fringe adoption is substantial, the endogenous_climb reading is vindicated and the decree was a natural endpoint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(invisible_fringe_adoption, empirical, 'Whether voluntary Gregorian calendar adoption preceded the 1872 state decree.').

omega_variable(
    suppression_mechanism_structure_vs_internalization,
    'Is the measured suppression (0.81) structural (police enforcement, administrative penalty, tax reassessment) or internalized (communities have absorbed the legitimacy of the new calendar, maintain compliance even when surveillance relaxes)?',
    'Post-enforcement relaxation trajectory: if enforcement is withdrawn (e.g., police enforcement of calendar conformity stops), does compliance persist? If suppression is internalized, communities continue the practice. If structural, reversion to lunar calendar is immediate.',
    'If suppression is structural, the constraint is fundamentally coercive and dependent on continuous enforcement—a snare with administrative framing. If partly internalized, the constraint has acquired self-sustaining components and may be creeping toward rope. The distinction determines long-term stability predictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structure_vs_internalization, empirical, 'Whether suppression in Gregorian calendar adoption is structural (external enforcement) or internalized (communities have accepted the new calendar as legitimate).').

omega_variable(
    coordination_framing_vs_cultural_appropriation,
    'Is the state''s framing of the decree as ''coordination enabling modernization'' the dominant reading, or is the alternative framing—''exogenous cultural appropriation by a centralizing state''—equally live in contemporary discourse?',
    'Analysis of Japanese historical scholarship, political rhetoric post-1872, and contemporary educational materials. Identify which reading is institutionalized as the legitimate narrative of the calendar change.',
    'Dominance of the coordination framing supports the state''s vindication claim; dominance of the cultural-appropriation framing supports the victim narrative. The readings coexist in scholarship; which one captures official memory determines whether the constraint''s legitimacy is reinforced or eroded over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_framing_vs_cultural_appropriation, conceptual, 'Whether the Meiji calendar decree is framed as legitimate modernization coordination or as exogenous cultural displacement.').

omega_variable(
    fringe_creation_vs_fringe_recruitment,
    'Were state employees and military conscripts early adopters of the Gregorian calendar (who then joined the state apparatus), or were they created as adopters by state mandate (mandated to adopt as employment/conscription condition)?',
    'Biographical and archival evidence: did individuals adopt the calendar before state employment, or did state employment create the adoption requirement? Check military enrollment documents, state employee hiring records, and personal archives of early state employees.',
    'If created-as-adopters, the state is the fringe generator and the constraint is exogenous override (state capacity creates the fringe). If recruits were pre-adopters, it suggests a voluntary fringe already existed and the state merely consolidated it—supporting the hybrid_cascade reading (fringe exists and state accelerates it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_creation_vs_fringe_recruitment, empirical, 'Whether military and state-employee adoption was mandated (state-created fringe) or pre-existing (recruited fringe).').

omega_variable(
    m_set_completeness,
    'Is the fringe-adoption model (M-set) sufficient to explain all historical commitment displacements, or does the exogenous-override mechanism require a separate cell in the model?',
    'Comparative historical analysis: identify commitment displacements across cultures and time periods; classify by presence/absence of pre-decree fringe adoption; assess whether exogenous override occurs without fringe-climb completion or whether all cases eventually produce fringe adoption and climb.',
    'If exogenous override is merely an invisible fringe stage (too subtle to observe), the M-set is complete. If exogenous override is a distinct mechanism that persists without fringe-climb completion, the M-set requires revision. This omega is the theoretical question at the kernel''s center.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(m_set_completeness, conceptual, 'Whether the fringe-adoption (M-set) model is complete or requires a separate exogenous-override cell.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 1868, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1868, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(impo_tr_t1872, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1872, 0.12).
narrative_ontology:measurement(impo_tr_t1876, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1876, 0.18).
narrative_ontology:measurement(impo_tr_t1884, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1884, 0.26).
narrative_ontology:measurement(impo_tr_t1892, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1892, 0.3).
narrative_ontology:measurement(impo_tr_t1900, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1900, 0.28).

% Extraction over time
narrative_ontology:measurement(impo_be_t1868, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1868, 0.12).
narrative_ontology:measurement(impo_be_t1872, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1872, 0.58).
narrative_ontology:measurement(impo_be_t1876, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1876, 0.64).
narrative_ontology:measurement(impo_be_t1884, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1884, 0.68).
narrative_ontology:measurement(impo_be_t1892, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1892, 0.67).
narrative_ontology:measurement(impo_be_t1900, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1900, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1868, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1868, 0.15).
narrative_ontology:measurement(impo_su_t1872, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1872, 0.79).
narrative_ontology:measurement(impo_su_t1876, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1876, 0.81).
narrative_ontology:measurement(impo_su_t1884, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1884, 0.82).
narrative_ontology:measurement(impo_su_t1892, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1892, 0.8).
narrative_ontology:measurement(impo_su_t1900, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1900, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__exogenous_override_reading, 0.18).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the imposition_pathway_kernel, which contests whether commitment displacement requires fringe adoption or whether state capacity enables exogenous override. The exogenous_override_reading asserts the Meiji calendar change had no pre-decree fringe adoption and required sustained enforcement to displace the lunar calendar. The endogenous_climb_reading asserts the fringe adoption was present but invisible. The hybrid_cascade_reading asserts the state created artificial fringe (military, state employees) which then climbed organically. All three readings are structurally distinct constraints with different ε values and stakeholder maps. They are linked via network.affects_constraints to enable contamination analysis: if evidence emerges showing pre-decree Gregorian adoption, the exogenous_override reading's ε declines and the endogenous_climb reading's validity rises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__exogenous_override_reading, institutional, 0.05).
constraint_indexing:directionality_override(imposition_pathway_kernel__exogenous_override_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
