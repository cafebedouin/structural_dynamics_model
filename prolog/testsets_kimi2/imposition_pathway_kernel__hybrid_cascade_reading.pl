% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: Hybrid Cascade State Imposition Pathway
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This constraint is one reading of the contested imposition_pathway_kernel
 *   in historical sociology. The hybrid_cascade_reading holds that top-down
 *   state imposition creates an artificial fringe (government employees,
 *   military personnel required to adopt a new commitment) which then becomes
 *   the vector for organic climb; the override initiates the process, but the
 *   climb completes it. This reading is contested against an
 *   endogenous_climb_reading (all displacement is organic) and an
 *   exogenous_override_reading (state capacity displaces without fringe
 *   adoption). The Meiji-era creation of a state-employee and military fringe
 *   for Western dress, calendar, or language adoption is the canonical case.
 *
 * KEY AGENTS:
 *   - centralizing_state_elites (agenda_setter/beneficiary, institutional/constrained) â initiates imposition and captures state-consolidation gains
 *   - state_employees (payer, moderate/constrained) â bear forced adoption costs in civilian bureaucracy
 *   - military_personnel (payer, organized/trapped) â bear forced adoption costs under coercive discipline
 *   - later_organic_adopters (beneficiary, powerless/mobile) â adopt once the commitment has climbed
 *   - traditional_elites (excluded, powerful/constrained) â lose standing under the old commitment and are excluded from agenda-setting
 *   - comparative_historian (observer, analytical/analytical) â tracks generalizability of the mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.48).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.46).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "Hybrid Cascade State Imposition Pathway").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, '59ed68e5-023f-42da-a665-cd4346fc6b5a').
narrative_ontology:cs_kernel_codification('59ed68e5-023f-42da-a665-cd4346fc6b5a', distributed).
narrative_ontology:cs_authority_grounding('59ed68e5-023f-42da-a665-cd4346fc6b5a', distributed).
narrative_ontology:cs_reading_relation('59ed68e5-023f-42da-a665-cd4346fc6b5a', imposition_pathway_kernel__endogenous_climb_reading, influences).
narrative_ontology:cs_reading_relation('59ed68e5-023f-42da-a665-cd4346fc6b5a', imposition_pathway_kernel__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('59ed68e5-023f-42da-a665-cd4346fc6b5a', foundational, state_capacity_manufactures_fringe).
narrative_ontology:cs_axiom_status(state_capacity_manufactures_fringe, holdable).
narrative_ontology:cs_axiom_grounding('59ed68e5-023f-42da-a665-cd4346fc6b5a', state_capacity_manufactures_fringe, empirically_contingent).
narrative_ontology:cs_axiom('59ed68e5-023f-42da-a665-cd4346fc6b5a', foundational, organic_climb_requires_artificial_seed).
narrative_ontology:cs_axiom_status(organic_climb_requires_artificial_seed, holdable).
narrative_ontology:cs_axiom_grounding('59ed68e5-023f-42da-a665-cd4346fc6b5a', organic_climb_requires_artificial_seed, empirically_contingent).
narrative_ontology:cs_reference_frame('59ed68e5-023f-42da-a665-cd4346fc6b5a', compressed_climb_with_state_seed).
narrative_ontology:cs_drift_state('59ed68e5-023f-42da-a665-cd4346fc6b5a', contemporary_comparative_historical_sociology, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59ed68e5-023f-42da-a665-cd4346fc6b5a', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, centralizing_state_elites).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, later_organic_adopters).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, state_employees).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, military_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue decrees requiring adoption of new commitment practices by government functionaries and military units. Capture consolidated state capacity and expanded legitimate authority as the commitment displaces local alternatives. Their exit from the nation-building project would mean abandoning the modernizing state itself.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, centralizing_state_elites, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, centralizing_state_elites, beneficiary).

% Are required by decree to abandon prior local practices and adopt the new national standard in official and often personal conduct. Bear the daily costs of code-switching, wardrobe replacement, and social dislocation from home communities. Resignation is possible but means loss of livelihood and status in the new bureaucratic order.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, state_employees, payer,
    moderate, biographical, constrained, national).

% Subject to strict uniform and conduct codes mandating the new commitment system; non-compliance risks punishment, demotion, or discharge. Serve as the visible vanguard of the new order in public spaces. Exit is severely constrained by enlistment terms and the coercive structure of military discipline.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, military_personnel, payer,
    organized, biographical, trapped, national).

% Observe the state fringe adopting the new commitment and gradually treat it as normal or prestige-bearing practice. Adopt voluntarily as the commitment climbs from fringe to center. Benefit from belonging to the unified national community but did not bear the forced-displacement costs of the initial phase.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, later_organic_adopters, beneficiary,
    powerless, generational, mobile, national).

% Held authority under the pre-displacement commitment system and lose status, clientele, and ritual role as the state-manufactured fringe climbs past them. Are excluded from the state-building agenda and would argue for the legitimacy of local endogenous practice if given voice.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, traditional_elites, excluded,
    powerful, biographical, constrained, regional).

% Sits outside the constraint's operation, tracking whether the artificial-fringe mechanism generalizes across cases or is specific to high state-capacity contexts. Compares state-formation episodes across civilizations without bearing the costs of any particular commitment displacement.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, comparative_historian, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, centralizing_state_elites).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Accelerates commitment displacement by using state capacity to manufacture an artificial fringe (government employees, military) that would not arise organically at the needed speed, creating a visible climb vector for subsequent organic adoption.
% TRANSFER_FUNCTION: Moves compliance costs and identity-displacement burden from the state-building center onto state employees and military personnel; transfers legitimacy and normalized practice from the artificial fringe to later organic adopters.
% ABSENT_VOICES: Traditional community elites, local religious authorities, and pre-displacement cultural practitioners who would argue for organic endogenous change or resist the specific content of the imposed commitment; they are sidelined by the state's monopoly on legitimate coercion and agenda-setting.
% DISAPPEARANCE_RATIONALE: If the hybrid cascade pathway vanished, rapid nation-state consolidation via manufactured fringe would not occur; state formation would revert to slower endogenous climb or fail to achieve vertical integration, rearranging the timeline and territorial coherence of the modern state.
% FOUNDING_PROBLEM: How to displace entrenched local commitments and unify diverse populations under a new national commitment system faster than organic fringe-and-climb dynamics permit.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historical sociologists attest the founding problem of rapid state consolidation existed in the nineteenth-century context. Critics from subaltern studies and post-colonial historiography attest the problem was manufactured or that the solution outlived the problem; these sources sit outside the direct beneficiary set of state elites.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.48, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.48) is moderate-high at the start and declines toward 0.48 as organic climb naturalizes compliance, but remains non-zero because the artificial fringe never fully escapes its coerced origin. Suppression (0.46 endpoint) tracks the enforcement capacity needed to create the fringe; it peaks early and declines but never reaches zero because the threat of state sanction persists in the background. Theater_ratio rises to 0.50, indicating that by the end of the interval a substantial share of state activity is performative maintenance of authority over what has become organic practice. Accessibility_collapse (0.55) is moderate: alternatives (local endogenous practice) are marginalized but not erased. Resistance (0.42) is moderate: pockets of local practice persist and scholarly critique continues.
 *
 * PERSPECTIVAL GAP:
 *   The centralizing state elite seat experiences the constraint as necessary nation-building coordination; the artificial fringe seats (state employees, military) experience it as coerced identity displacement; the later organic adopter seat experiences it as natural social evolution. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Centralizing_state_elites are positioned near the beneficiary pole (agenda_setter, institutional, constrained exit but capturing gains). State_employees and military_personnel are positioned near the target pole (payer roles, moderate/organized power, constrained/trapped exit). Later_organic_adopters sit low toward beneficiary (mobile exit, voluntary adoption, diffuse gains). Traditional_elites are excluded and would sit high toward target if they were inside the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid cascade reading prevents mislabeling the constraint as a pure rope (ignoring the coerced artificial fringe) or a pure snare (ignoring the genuine coordination function in rapid state formation and the organic climb phase). By documenting both the beneficiary and victim sets alongside active enforcement, the classification captures that the coordination and extraction are structurally coupled through the same state-imposition mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imposition_pathway_kernel_reading_identity,
    'Is this constraint a genuine hybrid mechanism or a conflation of two separable processes (exogenous override and endogenous climb)?',
    'Comparative historical analysis identifying cases where top-down imposition failed to produce organic climb, or where organic climb occurred without state-manufactured fringe.',
    'If separable, this reading dissolves into its sibling readings; if genuinely hybrid, it warrants distinct classification as a tangled rope with coupled coordination and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imposition_pathway_kernel_reading_identity, conceptual, 'Kernel reading identity and structural separability of hybrid cascade.').

omega_variable(
    fringe_organic_transition,
    'Does the artificial fringe ever become genuinely organic, or does it remain a permanently performative, state-dependent constituency?',
    'Longitudinal identity-studies of state-employee and military cohorts measuring whether compliance becomes internalized or persists as instrumental accommodation.',
    'If permanently performative, theater_ratio is higher than recorded and the constraint is closer to a snare; if genuinely organic, the coordination function is stronger and the rope element is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_organic_transition, empirical, 'Whether state-manufactured fringe internalizes or performs commitment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by the artificial fringe structural (state sanctions for non-compliance) or internalized (identity fusion with the nation-building project)?',
    'Post-reform trajectory analysis: if compliance collapses when state enforcement is suspended, suppression was structural; if compliance persists, it was internalized.',
    'Internalized suppression would raise effective extraction for the fringe even after formal imposition ends, shifting the constraint toward a more identity-locked profile for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in artificial fringe.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(impo_tr_t50, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(impo_be_t50, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(impo_su_t50, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 50, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
