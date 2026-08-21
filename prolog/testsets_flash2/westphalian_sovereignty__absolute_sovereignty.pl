% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Westphalian Sovereignty (Non-Interference Principle)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'absolute sovereignty' reading of the
 *   Westphalian principle, asserting that states have unconditional authority
 *   over their domestic affairs and external interference is categorically
 *   illegitimate. It is one reading of the broader 'westphalian_sovereignty'
 *   kernel. This reading prioritizes state autonomy and non-interference,
 *   often at the expense of human rights or global collective action. The
 *   metrics reflect a system where this principle is actively enforced,
 *   benefiting certain regimes while victimizing others.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.55).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.7).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Westphalian Sovereignty (Non-Interference Principle)").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'bb774205-6325-4ac5-b2aa-d85b67bff909').
narrative_ontology:cs_kernel_codification('bb774205-6325-4ac5-b2aa-d85b67bff909', formalized).
narrative_ontology:cs_authority_grounding('bb774205-6325-4ac5-b2aa-d85b67bff909', lineage).
narrative_ontology:cs_interpretation_layer_present('bb774205-6325-4ac5-b2aa-d85b67bff909').
narrative_ontology:cs_reading_relation('bb774205-6325-4ac5-b2aa-d85b67bff909', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('bb774205-6325-4ac5-b2aa-d85b67bff909', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('bb774205-6325-4ac5-b2aa-d85b67bff909', foundational, state_autonomy_is_absolute).
narrative_ontology:cs_axiom_status(state_autonomy_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('bb774205-6325-4ac5-b2aa-d85b67bff909', state_autonomy_is_absolute, deontological).
narrative_ontology:cs_axiom('bb774205-6325-4ac5-b2aa-d85b67bff909', foundational, non_interference_is_categorical).
narrative_ontology:cs_axiom_status(non_interference_is_categorical, holdable).
narrative_ontology:cs_axiom_grounding('bb774205-6325-4ac5-b2aa-d85b67bff909', non_interference_is_categorical, conventional).
narrative_ontology:cs_reference_frame('bb774205-6325-4ac5-b2aa-d85b67bff909', post_westphalian_state_system).
narrative_ontology:cs_drift_state('bb774205-6325-4ac5-b2aa-d85b67bff909', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bb774205-6325-4ac5-b2aa-d85b67bff909', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, states_seeking_autonomy).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, international_human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shielded from external scrutiny or intervention regarding domestic human rights abuses or governance failures, allowing them to maintain power without accountability to international norms. They actively invoke this principle.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from the principle as a defense against unwanted external influence, allowing them to pursue independent domestic policies without fear of intervention, even if those policies are controversial.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, states_seeking_autonomy, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of state repression without recourse to external protection or intervention, as the principle of absolute sovereignty prioritizes state non-interference over individual human rights. Their suffering is often invisible to the international community.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, immediate, trapped, national).

% Their efforts to protect human rights are often frustrated by the invocation of absolute sovereignty, limiting their ability to intervene or hold states accountable for abuses. They expend significant resources to challenge this principle.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_human_rights_advocates, payer,
    moderate, generational, constrained, global).

% The primary body for authorizing international intervention, but its actions are constrained by the absolute sovereignty principle and the veto power of permanent members who often invoke it to protect allies or their own interests.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Often find themselves in a dilemma, balancing their commitment to human rights with the principle of non-interference. They observe the tension and sometimes advocate for conditional or graduated sovereignty, but are bound by existing international law.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, liberal_democracies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for stable international relations by defining clear boundaries of state authority and preventing constant external meddling in domestic affairs, thereby reducing interstate conflict.
% TRANSFER_FUNCTION: Transfers the right to self-determination and non-interference to states, at the cost of potential accountability for domestic actions, particularly from populations under repressive regimes.
% ABSENT_VOICES: Populations suffering under repressive regimes, whose voices are suppressed domestically and whose pleas for international intervention are often silenced by the invocation of absolute sovereignty. Also, emerging global civil society organizations advocating for a 'responsibility to protect' that challenges this absolute view.
% DISAPPEARANCE_RATIONALE: If absolute sovereignty vanished overnight, the international system would undergo a profound rearrangement. States would lose their primary shield against external intervention, potentially leading to increased humanitarian interventions, but also a rise in proxy wars and destabilization as powerful states might interfere more readily in weaker ones. The very definition of statehood would be challenged.
% FOUNDING_PROBLEM: The chaos and constant warfare of post-Reformation Europe, where religious and dynastic conflicts frequently spilled across borders, necessitating a principle to delineate state authority and prevent endless intervention.
% FOUNDING_PROBLEM_CORROBORATION: Many states, particularly those with authoritarian tendencies or a history of colonial interference, attest the problem of external meddling is still live. Liberal democracies and human rights organizations argue the original problem is largely solved, and the principle now serves to shield human rights abuses, citing the rise of global challenges that transcend national borders and require collective action.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is moderate-high, reflecting the cost borne by populations under repressive regimes who are denied external protection. Suppression (0.7) is high because the principle actively suppresses alternative international norms (like R2P) and requires constant diplomatic and legal defense by states that benefit from it. Theater ratio (0.2) is low, as the principle is genuinely invoked and enforced, not merely performed. The increasing extractiveness over time reflects the growing tension between this absolute view and evolving international human rights norms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authoritarian regimes, this is a legitimate 'rope' for international order, preventing chaos. From the perspective of repressed populations, it is a 'snare' that traps them. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and states prioritizing autonomy are clear beneficiaries (low d), as the principle shields them from external accountability. Domestic populations under repression and human rights advocates are victims (high d), as their interests are subordinated to state sovereignty. The UN Security Council, while an agenda-setter, is constrained by the principle itself and the political will of its members, often leading to inaction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_non_interference,
    'Is the categorical illegitimacy of external interference a foundational principle of international order, or a historically contingent norm that can evolve?',
    'Analysis of evolving international customary law and state practice, particularly regarding humanitarian intervention and the ''Responsibility to Protect'' (R2P) doctrine.',
    'If historically contingent, the constraint''s ''mountain-like'' claim to naturalness is undermined, potentially reclassifying it as a ''tangled_rope'' or ''snare'' that can be reformed. If foundational, its persistence is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_non_interference, conceptual, 'Whether absolute non-interference is an immutable principle or an evolving norm.').

omega_variable(
    impact_on_human_rights,
    'To what extent does the principle of absolute sovereignty directly enable or exacerbate human rights abuses within states?',
    'Empirical studies correlating state invocation of absolute sovereignty with documented human rights violations and the absence of international accountability mechanisms.',
    'Strong correlation would increase the measured extractiveness and suppression, pushing the classification more firmly towards ''snare'' for victim populations. Weak correlation would suggest other factors are more dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_human_rights, empirical, 'Direct causal link between absolute sovereignty and human rights abuses.').

omega_variable(
    coordination_vs_extraction_balance,
    'At what point does the coordination benefit of preventing interstate conflict (by upholding non-interference) become outweighed by the extraction cost of shielding repressive regimes?',
    'A normative and empirical assessment of the trade-offs, potentially involving a global deliberative process or a shift in the balance of power among states advocating different sovereignty interpretations.',
    'A finding that extraction outweighs coordination would justify a reclassification from ''tangled_rope'' to ''snare'' for the international system as a whole, signaling a need for fundamental reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_balance, preference, 'Normative balance between coordination benefits and extraction costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0, 0.1).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 10, 0.12).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 20, 0.14).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 30, 0.16).
narrative_ontology:measurement(west_tr_t40, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 40, 0.18).
narrative_ontology:measurement(west_tr_t50, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 50, 0.19).
narrative_ontology:measurement(west_tr_t60, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 60, 0.2).
narrative_ontology:measurement(west_tr_t70, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 70, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(west_be_t40, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(west_be_t50, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(west_be_t60, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 60, 0.59).
narrative_ontology:measurement(west_be_t70, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 70, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(west_su_t40, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(west_su_t50, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(west_su_t60, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 60, 0.69).
narrative_ontology:measurement(west_su_t70, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 70, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is the 'absolute_sovereignty' reading of the 'westphalian_sovereignty' kernel. It is structurally distinct from 'conditional_sovereignty' and 'graduated_sovereignty', which represent alternative interpretations of state authority and intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
