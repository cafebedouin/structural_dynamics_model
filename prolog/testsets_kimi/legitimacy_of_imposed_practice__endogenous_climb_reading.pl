% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Endogenous Climb Requirement for Practice Displacement
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint instantiates the endogenous_climb_reading of the
 *   legitimacy_of_imposed_practice kernel in the domain of state formation
 *   and cultural imposition. It asserts that practice displacement requires
 *   bottom-up internalization; imposed commitments fail without endogenous
 *   adoption pathways. Historical evidence includes failed calendar
 *   displacement (persistent lunar observance) and partial dress adoption
 *   (urban public compliance with private retention). Communities preserving
 *   autonomy are the structural beneficiaries; state modernizers constrained
 *   by the need for persuasion rather than command are the structural
 *   victims. The constraint is claimed as mountain because it is framed as a
 *   structural feature of social order, but the presence of identifiable
 *   beneficiaries triggers FSM evaluation. Sibling readings include
 *   exogenous_override_reading (state decree suffices) and
 *   hybrid_scaffolding_reading (ideological reinforcement generates
 *   quasi-endogenous pull).
 *
 * KEY AGENTS:
 *   - autonomous_communities: Primary beneficiary (powerless/identity_locked/local) â preserve traditional practices against state imposition.
 *   - state_modernizers: Primary target (institutional/constrained/national) â bear the costs of failed displacement and forced investment in persuasion.
 *   - urban_intermediaries: Intermediate actor (moderate/constrained/regional) â partially adopt publicly while retaining privately, displaying incomplete internalization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.25).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.12).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, mountain).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Endogenous Climb Requirement for Practice Displacement").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:emerges_naturally(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, 'aae836d8-8abc-4c3c-bb63-670a89a9f459').
narrative_ontology:cs_kernel_codification('aae836d8-8abc-4c3c-bb63-670a89a9f459', distributed).
narrative_ontology:cs_authority_grounding('aae836d8-8abc-4c3c-bb63-670a89a9f459', distributed).
narrative_ontology:cs_reading_relation('aae836d8-8abc-4c3c-bb63-670a89a9f459', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('aae836d8-8abc-4c3c-bb63-670a89a9f459', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('aae836d8-8abc-4c3c-bb63-670a89a9f459', foundational, internalization_prerequisite_for_displacement).
narrative_ontology:cs_axiom_status(internalization_prerequisite_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('aae836d8-8abc-4c3c-bb63-670a89a9f459', internalization_prerequisite_for_displacement, empirically_contingent).
narrative_ontology:cs_axiom('aae836d8-8abc-4c3c-bb63-670a89a9f459', foundational, imposed_legitimacy_is_oxymoron).
narrative_ontology:cs_axiom_status(imposed_legitimacy_is_oxymoron, holdable).
narrative_ontology:cs_axiom_grounding('aae836d8-8abc-4c3c-bb63-670a89a9f459', imposed_legitimacy_is_oxymoron, deontological).
narrative_ontology:cs_reference_frame('aae836d8-8abc-4c3c-bb63-670a89a9f459', endogenous_legitimacy_requirement).
narrative_ontology:cs_drift_state('aae836d8-8abc-4c3c-bb63-670a89a9f459', contemporary_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aae836d8-8abc-4c3c-bb63-670a89a9f459', '2026-06-20T00:00:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, autonomous_communities).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain traditional practices such as lunar calendar observance and customary dress within their local social world. State decrees reach them formally but fail to replace embedded rituals because these practices are woven into kinship, religious, and agricultural cycles. Their continued observance is not organized political resistance but the persistence of ordinary life.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, autonomous_communities, beneficiary,
    powerless, generational, identity_locked, local).

% Central state officials and ideologues who issue decrees standardizing calendars, dress, or language across a territory. They observe that formal compliance in administrative centers masks private retention in rural areas and that each generation of decrees requires renewed investment without achieving full displacement. They cannot simply command adoption but must invest in schools, propaganda, and material incentives.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernizers, payer,
    institutional, generational, constrained, national).

% Town-dwelling populations who publicly display state-mandated practices to secure employment, licensing, or social standing while continuing traditional observance in domestic or religious spaces. They experience the arrangement as a dual requirement: visible conformity is necessary for urban participation, but private retention is necessary for family and community belonging.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_intermediaries, payer,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Governs the transfer of social legitimacy from existing practices to new ones, ensuring that practice displacement occurs only when populations have integrated new norms into their everyday cognitive and ritual frameworks.
% TRANSFER_FUNCTION: Moves the burden of cultural change from state decree to community adoption; forces modernizers to expend persuasion, education, and institutional resources rather than relying on legal command, while preserving traditional practice domains for communities that do not internalize the new order.
% ABSENT_VOICES: State actors who succeeded in rapid cultural revolution through overwhelming coercion are absent from the endogenous frame; communities that were entirely eradicated or forcibly resettled (and thus cannot testify to persistent practice) are absent from the autonomy-preservation narrative.
% DISAPPEARANCE_RATIONALE: If practice displacement no longer required internalization, state modernizers could replace calendars, dress, and ritual through decree alone; autonomous communities would lose their structural protection and traditional practices would be displaced at the speed of administrative enforcement rather than generational adoption.
% FOUNDING_PROBLEM: How to unify heterogeneous populations under common state practices without triggering persistent ritual non-compliance, identity-based resistance, or shallow compliance that reverses when enforcement lapses.
% FOUNDING_PROBLEM_CORROBORATION: Social historians and anthropologists documenting persistent lunar observance and private dress retention across multiple state-formation episodes attest from outside both the state beneficiary seat and the traditional community seat that the problem of shallow compliance is recurrent; state chronicles complaining of 'superstition' and 'backwardness' corroborate from the modernizer side.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.25, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, ExtMetricName, E),
    domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(legitimacy_of_imposed_practice__endogenous_climb_reading),
    narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Accessibility_collapse is high (0.88) because once the endogenous requirement is understood, the alternative (pure top-down imposition) is recognized as non-viable. Resistance is low (0.18) because the constraint is not a defended construct but an emergent social dynamic; state modernizers struggle against it but do not constitute resistance to a maintained arrangement. Extractiveness is moderate-low (0.25) because the constraint forces modernizers to redirect resources from command to institution-building. Theater is minimal (0.08) because the dynamic requires no performative maintenance. The measurement series shows extractiveness peaking mid-interval when modernizers confront shallow compliance, then declining as they adapt to the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The state_modernizer seat experiences the constraint as a frustrating structural limit that inflates the cost of cultural unification; the autonomous_communities seat experiences the same dynamic as protective insulation that preserves generational practice. The urban_intermediary seat experiences a bifurcated situation where public performance satisfies the state while private retention satisfies community. These divergences are computed by the engine from the directionalities derived from beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   autonomous_communities are declared beneficiaries with identity_locked exit and local scope, deriving a low directionality near the beneficiary pole. state_modernizers are declared victims/payers with institutional power but constrained exit and national scope, deriving a high directionality near the target pole. urban_intermediaries are payers with moderate power and constrained exit, placing them at intermediate directionality. The effective extraction is thus amplified for the state and damped for communities, consistent with the structural claim.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as mountain rather than snare or tangled rope is warranted by the absence of active enforcement: the constraint does not require maintenance by a beneficiary to persist. autonomous_communities do not enforce the constraint; they simply continue their practices. If the classification were snare, we would need an active enforcer capturing gains from the arrangement, which is absent. If it were tangled rope, we would need both coordination and asymmetric extraction actively enforced. The only active party is the state, which is the target, not the maintainer. Piton is ruled out because the constraint is functional, not theatrical. FSM evaluation is triggered by beneficiary presence and will test whether the mountain claim is a false summit constructed to benefit communities retroactively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the requirement for internalization a universal structural feature of social order, or a constructed pattern that benefits identifiable communities and could be overcome with sufficient state capacity or technology?',
    'Comparative historical analysis of cases where centralized states achieved rapid practice displacement (e.g., colonial regimes, cultural revolution) to determine if internalization was bypassed or merely accelerated.',
    'If universal, mountain classification holds; if contingent on state capacity or technology, reclassify as tangled_rope or scaffold (state imposition with variable success), triggering FSM override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Whether endogenous climb is a natural law of social dynamics or a contingent historical pattern.').

omega_variable(
    domain_specificity_of_resistance,
    'Does the internalization requirement apply uniformly across all practice domains (calendar, dress, language, religion), or does it vary by the depth of identity-embedding?',
    'Cross-domain comparison within the same state-formation episode measuring displacement success rates against identity-fusion metrics.',
    'Would refine the constraint''s scope; if domain-specific, the universal mountain claim must be narrowed to a family of domain-specific constraints with different epsilon profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_specificity_of_resistance, empirical, 'Whether resistance to displacement is uniform or domain-specific.').

omega_variable(
    reading_stability,
    'This constraint is the endogenous_climb_reading of a contested kernel. Does the sibling exogenous_override_reading describe a structurally different constraint or merely a different measurement of the same displacement phenomenon?',
    'Examine whether the two readings share a single causal ontology or posit incompatible mechanisms; if the latter, the kernel is irreducibly polysemic.',
    'Determines whether the constraint family is a true decomposition (per epsilon-invariance) or whether one reading should subsume the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_stability, conceptual, 'Whether sibling readings are distinct constraints or framing variants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legitimacy_endogenous_climb_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(legitimacy_endogenous_climb_tr_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(legitimacy_endogenous_climb_tr_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(legitimacy_endogenous_climb_tr_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(legitimacy_endogenous_climb_tr_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(legitimacy_endogenous_climb_tr_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(legitimacy_endogenous_climb_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legitimacy_endogenous_climb_be_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(legitimacy_endogenous_climb_be_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(legitimacy_endogenous_climb_be_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(legitimacy_endogenous_climb_be_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(legitimacy_endogenous_climb_be_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 50, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(legitimacy_of_imposed_practice__endogenous_climb_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legitimacy_of_imposed_practice kernel. The endogenous_climb_reading models the structural necessity of bottom-up adoption; siblings model alternative displacement logics. Epsilon values differ by reading because each reading identifies a different causal mechanism and beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
