% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: Outer Space Treaty Article II: Deferral to International Regime
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint represents the 'international regime' reading of Outer
 *   Space Treaty (OST) Article II, which interprets the non-appropriation
 *   principle as deferring the question of space resource ownership to a
 *   future multilateral framework. This reading acknowledges that neither
 *   outright prohibition nor explicit permission for private extraction is
 *   currently authoritative, leading to a legal grey zone. The constraint
 *   functions as a Scaffold, intended to be transitional, but its persistence
 *   has allowed for increasing de facto extraction and diplomatic theater.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.65).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.4).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.65).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "Outer Space Treaty Article II: Deferral to International Regime").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__international_regime).
narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, 'dbb4f5eb-ce36-41cf-8299-95981241661c').
narrative_ontology:cs_kernel_codification('dbb4f5eb-ce36-41cf-8299-95981241661c', fixed_text).
narrative_ontology:cs_authority_grounding('dbb4f5eb-ce36-41cf-8299-95981241661c', lineage).
narrative_ontology:cs_interpretation_layer_present('dbb4f5eb-ce36-41cf-8299-95981241661c').
narrative_ontology:cs_reading_relation('dbb4f5eb-ce36-41cf-8299-95981241661c', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('dbb4f5eb-ce36-41cf-8299-95981241661c', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_axiom('dbb4f5eb-ce36-41cf-8299-95981241661c', foundational, appropriation_requires_multilateral_consent).
narrative_ontology:cs_axiom_status(appropriation_requires_multilateral_consent, holdable).
narrative_ontology:cs_axiom_grounding('dbb4f5eb-ce36-41cf-8299-95981241661c', appropriation_requires_multilateral_consent, conventional).
narrative_ontology:cs_axiom('dbb4f5eb-ce36-41cf-8299-95981241661c', secondary, ost_is_living_document).
narrative_ontology:cs_axiom_status(ost_is_living_document, holdable).
narrative_ontology:cs_axiom_grounding('dbb4f5eb-ce36-41cf-8299-95981241661c', ost_is_living_document, conventional).
narrative_ontology:cs_reference_frame('dbb4f5eb-ce36-41cf-8299-95981241661c', deliberate_ambiguity_for_future_regime).
narrative_ontology:cs_drift_state('dbb4f5eb-ce36-41cf-8299-95981241661c', contemporary_space_race_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dbb4f5eb-ce36-41cf-8299-95981241661c', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, space_resource_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, states_seeking_resource_access).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, un_committee_on_peaceful_uses_of_outer_space).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, environmental_advocates).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, states_advocating_commons_conservation).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, firms_seeking_legal_certainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandated to negotiate a future international regime for space resource governance. Benefits from the ongoing mandate and diplomatic activity, but is constrained by the difficulty of achieving consensus among member states.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, un_committee_on_peaceful_uses_of_outer_space, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, un_committee_on_peaceful_uses_of_outer_space, beneficiary).

% Operate in the legal grey zone created by the deferral, allowing them to pursue resource extraction without explicit prohibition. They benefit from the lack of a restrictive regime, though they face some legal uncertainty.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_resource_firms, beneficiary,
    powerful, biographical, mobile, global).

% Benefit from the current ambiguity, which allows their national firms to explore and potentially extract resources without violating a clear international prohibition. They participate in negotiations but may stall progress to maintain the status quo.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, states_seeking_resource_access, beneficiary,
    institutional, generational, constrained, global).

% Bear the cost of potential environmental degradation and unregulated resource exploitation in space. They actively lobby for a conservation-oriented regime but are frustrated by the lack of progress and the legal ambiguity.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, environmental_advocates, payer,
    organized, generational, constrained, global).

% Bear the cost of the ongoing legal uncertainty and the risk of de facto appropriation. They actively push for a clear prohibition on resource extraction and the establishment of a robust conservation regime.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, states_advocating_commons_conservation, payer,
    institutional, generational, constrained, global).

% Desire clear legal frameworks to de-risk investments in space resource activities. They bear the cost of legal uncertainty, which can deter investment and make long-term planning difficult.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, firms_seeking_legal_certainty, payer,
    moderate, immediate, constrained, global).

% Will inherit the consequences of current space resource governance (or lack thereof). They have no voice in current negotiations but will bear the long-term benefits or costs of resource management decisions.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__international_regime, space_resource_firms).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__international_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent unilateral claims of sovereignty over celestial bodies and to defer the complex question of space resource appropriation to a future, multilaterally agreed international regime, thereby maintaining a temporary legal equilibrium.
% TRANSFER_FUNCTION: Transfers the authority to define appropriation rights from individual states to a future, as-yet-undefined international regime. In the interim, it allows for de facto resource exploration and potential extraction in a legal grey zone, transferring potential future resources to current actors.
% ABSENT_VOICES: Future generations, who will be most affected by the long-term consequences of space resource exploitation, are entirely absent from the current diplomatic and legal discourse. Their interests are represented by proxy, if at all.
% DISAPPEARANCE_RATIONALE: If the deferral mechanism of Article II vanished, either a clear prohibition on appropriation or an explicit permission for extraction would become the de facto norm. This would fundamentally reorganize the legal and economic landscape of space activities, leading to either rapid exploitation or stringent conservation efforts, with significant geopolitical consequences.
% FOUNDING_PROBLEM: The original drafters of the Outer Space Treaty recognized the potential for future resource exploitation but lacked the consensus or foresight to establish a definitive legal framework for it, opting instead for a deliberate ambiguity to secure broader agreement on the treaty's core principles.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, diplomatic historians, and ongoing debates within the UN Committee on the Peaceful Uses of Outer Space (COPUOS) consistently corroborate that the problem of space resource governance remains unresolved and is a direct legacy of the OST's original drafting compromises.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate because the deferral, while not directly extractive, enables a legal grey zone that benefits firms pursuing resource extraction. Suppression (0.40) is low, as the constraint primarily maintains ambiguity rather than actively suppressing either pro-extraction or pro-conservation efforts. Theater ratio (0.70) is high, reflecting the ongoing, often stalled, diplomatic negotiations that produce little concrete progress while the underlying issue remains unresolved. The accessibility collapse (0.30) is low, as both extraction and conservation remain live, albeit legally ambiguous, possibilities. Resistance (0.50) is moderate, coming from both sides of the debate (pro-extraction for certainty, pro-conservation for protection). The temporal measurements show a gradual increase in extractiveness and theater as the 'temporary' deferral persists and becomes a de facto operating environment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of space resource firms, this constraint provides a workable (if uncertain) environment for innovation. From the perspective of conservation advocates, it represents a failure to protect the commons. The 'international regime' reading attempts to bridge this by framing it as a necessary, albeit slow, path to consensus, but the divergence in experience is stark.
 *
 * DIRECTIONALITY LOGIC:
 *   Space resource firms and states seeking resource access are beneficiaries, as the ambiguity allows them to operate without clear prohibition. The UN Committee benefits from its ongoing mandate. Environmental advocates and states advocating for conservation are victims, bearing the cost of potential unregulated exploitation. Firms seeking legal certainty are also victims, as the ambiguity creates investment risk. Future generations are excluded, bearing the long-term consequences.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_permission_vs_deferral,
    'Is the deferral of a definitive appropriation regime a genuine attempt at future coordination, or does it function as a tacit permission for de facto extraction?',
    'Analysis of state practice and diplomatic statements: if states actively encourage and protect private extraction in the absence of a regime, it suggests tacit permission. If states consistently uphold the spirit of non-appropriation in their national laws, it suggests genuine deferral.',
    'If tacit permission, the constraint''s effective extractiveness is higher, and its ''scaffold'' nature is a cover for a ''snare'' benefiting first-movers. If genuine deferral, it remains a ''scaffold'' with high theater due to negotiation difficulties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_permission_vs_deferral, conceptual, 'Ambiguity between deferral as a coordination mechanism and as a cover for extraction.').

omega_variable(
    regime_negotiation_sincerity,
    'Are the ongoing negotiations for an international regime genuinely aimed at achieving consensus, or are they primarily performative to maintain the legal grey zone?',
    'Examination of negotiation progress, proposed texts, and state positions over time: consistent stalling by key actors or a lack of substantive proposals would indicate performative negotiation.',
    'If negotiations are performative, the ''scaffold'' classification is undermined, and the constraint leans towards a ''piton'' (inertial maintenance of ambiguity) or ''tangled_rope'' (extraction masked by coordination theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_negotiation_sincerity, empirical, 'Sincerity of diplomatic efforts to resolve the appropriation question.').

omega_variable(
    future_regime_feasibility,
    'Is a comprehensive international regime for space resource governance realistically achievable given current geopolitical dynamics and divergent national interests?',
    'Expert assessment of international relations, historical precedent for commons governance, and analysis of current state positions on key issues (e.g., benefit sharing, environmental protection).',
    'If unachievable, the ''scaffold'' classification is unsustainable, and the constraint is effectively a ''snare'' (for those exploiting the grey zone) or a ''piton'' (for the international bodies maintaining the illusion of progress).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_regime_feasibility, empirical, 'Feasibility of the intended outcome of the deferral.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(ost__tr_t1995, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1995, 0.5).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2010, 0.6).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2020, 0.68).
narrative_ontology:measurement(ost__tr_t2024, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(ost__be_t2024, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1980, 0.32).
narrative_ontology:measurement(ost__su_t1995, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2010, 0.37).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2020, 0.39).
narrative_ontology:measurement(ost__su_t2024, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Outer Space Treaty's Article II non-appropriation principle, each representing a distinct structural claim about its operation and consequences. They form a constraint family linked by their common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
