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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Non-Appropriation: Deferral to International Regime
 *   domain: international_space_law/commons_governance
 *
 * SUMMARY:
 *   This constraint represents the 'international regime' reading of Outer
 *   Space Treaty (OST) Article II's non-appropriation clause. Under this
 *   reading, Article II is seen as deferring the question of space resource
 *   appropriation to a future international agreement, akin to Article XI's
 *   call for an international regime for lunar resources. Neither a strict
 *   prohibition (commons_conservation reading) nor an open-access permission
 *   (extraction_permissive reading) is considered authoritative in the
 *   absence of such a multilateral framework. This creates a legal grey zone
 *   where first-mover firms operate, and states maintain flexibility, while
 *   negotiations for a definitive regime remain stalled by zero-sum
 *   distributional conflicts. The constraint is classified as a Scaffold
 *   because it is a temporary, transitional arrangement that has persisted
 *   longer than intended due to lack of consensus on a permanent solution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.45).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.2).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.45).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Non-Appropriation: Deferral to International Regime").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_space_law/commons_governance").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, '47ad7c67-8fa6-4c13-9a0e-a176c8475176').
narrative_ontology:cs_kernel_codification('47ad7c67-8fa6-4c13-9a0e-a176c8475176', fixed_text).
narrative_ontology:cs_authority_grounding('47ad7c67-8fa6-4c13-9a0e-a176c8475176', lineage).
narrative_ontology:cs_interpretation_layer_present('47ad7c67-8fa6-4c13-9a0e-a176c8475176').
narrative_ontology:cs_reading_relation('47ad7c67-8fa6-4c13-9a0e-a176c8475176', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('47ad7c67-8fa6-4c13-9a0e-a176c8475176', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_axiom('47ad7c67-8fa6-4c13-9a0e-a176c8475176', foundational, appropriation_requires_multilateral_agreement).
narrative_ontology:cs_axiom_status(appropriation_requires_multilateral_agreement, holdable).
narrative_ontology:cs_axiom_grounding('47ad7c67-8fa6-4c13-9a0e-a176c8475176', appropriation_requires_multilateral_agreement, conventional).
narrative_ontology:cs_axiom('47ad7c67-8fa6-4c13-9a0e-a176c8475176', foundational, current_treaty_is_ambiguous_on_private_appropriation).
narrative_ontology:cs_axiom_status(current_treaty_is_ambiguous_on_private_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('47ad7c67-8fa6-4c13-9a0e-a176c8475176', current_treaty_is_ambiguous_on_private_appropriation, conventional).
narrative_ontology:cs_reference_frame('47ad7c67-8fa6-4c13-9a0e-a176c8475176', ost_original_intent_deferral).
narrative_ontology:cs_drift_state('47ad7c67-8fa6-4c13-9a0e-a176c8475176', contemporary_space_mining_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('47ad7c67-8fa6-4c13-9a0e-a176c8475176', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_space_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, states_seeking_future_flexibility).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, principle_of_international_cooperation).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, future_regime_negotiation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the current legal ambiguity, allowing them to develop resource extraction technologies and establish de facto operations without a clear prohibition. They operate in a regulatory grey zone, anticipating a future regime that might legitimize their activities.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_space_firms, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the deferral, as it keeps open options for future national policy on space resource utilization without committing to either a strict prohibition or an open-access regime. They are involved in stalled negotiations for a multilateral framework.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, states_seeking_future_flexibility, beneficiary,
    institutional, generational, constrained, global).

% Is tasked with negotiating a future international regime for space resource appropriation, as implied by Article II and XI. However, negotiations are stalled due to conflicting national interests and zero-sum distributional conflicts, leaving the current legal vacuum.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, international_community, agenda_setter,
    institutional, generational, constrained, universal).

% Advocate for a strict interpretation of non-appropriation, viewing space resources as a common heritage of mankind. They are largely excluded from the current decision-making process, which is dominated by states and commercial interests, and their arguments are not authoritative in the absence of a binding regime.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, commons_advocates, excluded,
    moderate, civilizational, identity_locked, universal).

% Analyze the legal implications of Article II's non-appropriation clause, the lack of a subsequent regime, and the activities of private actors. They highlight the legal uncertainty and the need for a clear framework.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Temporarily coordinates the absence of a definitive legal framework for space resource appropriation, allowing for continued exploration and technological development while deferring the complex political question of ownership and distribution to a future, as-yet-unnegotiated international regime.
% TRANSFER_FUNCTION: Transfers the burden of establishing a clear legal framework from the initial treaty drafters to future international negotiations, effectively transferring the 'right to operate in ambiguity' to first-mover firms and states that benefit from this deferral.
% ABSENT_VOICES: Advocates for a strict 'common heritage' principle are present in discourse but lack authoritative standing in the absence of a binding international regime. Their calls for immediate prohibition of private appropriation are not legally enforceable under this reading.
% DISAPPEARANCE_RATIONALE: If the deferral to a future regime vanished, and Article II was immediately interpreted as either strictly prohibitive or fully permissive, the legal landscape for space activities would fundamentally shift. First-mover firms would either face immediate prohibition or a free-for-all, and states would be forced to take definitive stances, leading to a rapid reorganization of space policy and commercial ventures.
% FOUNDING_PROBLEM: The original drafters of the Outer Space Treaty recognized the future potential for resource appropriation but lacked consensus on how to regulate it, leading to a deliberate ambiguity and deferral to a future international agreement (Article XI analogue).
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic records from the OST negotiations corroborate the intent to defer. Contemporary international legal scholars and ongoing UN COPUOS (Committee on the Peaceful Uses of Outer Space) discussions confirm the problem remains live and unresolved, with no consensus on a future regime.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because the legal ambiguity allows some actors (first-mover firms) to gain an advantage by operating without clear rules, effectively 'extracting' opportunity from the deferral. Suppression (0.20) is low because there's no active enforcement of either prohibition or permission; rather, it's a lack of enforcement that defines the current state. Theater ratio (0.10) is low, as the deferral is a genuine, if stalled, diplomatic reality, not primarily performative. The 'has_sunset_clause: true' reflects the implicit expectation of a future regime, even if that sunset has been indefinitely postponed.
 *
 * PERSPECTIVAL GAP:
 *   First-mover firms perceive this as an opportunity, a 'freedom to operate' in a new domain, while commons advocates see it as a failure of international law to protect a shared heritage. States view it as a necessary, if imperfect, holding pattern. The engine's classification will reflect the structural reality of deferral and its consequences, rather than any single party's preferred interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   First-mover space firms and states seeking future flexibility are beneficiaries, as the legal vacuum allows them to pursue their interests without definitive legal constraints. The international community is the agenda-setter, tasked with resolving the ambiguity but currently stalled. Commons advocates are excluded, as their preferred interpretation lacks binding authority. Legal scholars act as observers, analyzing the situation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to defer to a future regime) is still technically 'live' as negotiations continue, but its function as a temporary bridge has atrophied into a persistent legal vacuum. The classification as a Scaffold highlights its transitional nature, which has been prolonged beyond its intended lifespan due to the failure to establish the promised permanent regime. This prevents mislabeling it as a Rope (which would imply successful coordination) or a Snare (which would imply active, intentional extraction by the deferral itself, rather than extraction arising from the deferral's persistence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_negotiation_viability,
    'Is a comprehensive international regime for space resource appropriation still a viable outcome, or has the window for effective multilateral governance closed due to unilateral actions and technological advancements?',
    'Analysis of future UN COPUOS negotiation outcomes, national space legislation trends, and the pace of private sector resource extraction capabilities.',
    'If non-viable, the ''scaffold'' nature of this constraint collapses, and the legal vacuum becomes a de facto ''extraction_permissive'' regime, reclassifying it towards a Snare for commons advocates. If viable, the Scaffold classification holds, and the deferral remains a temporary, if prolonged, state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_negotiation_viability, empirical, 'Whether the deferred international regime is still a realistic possibility.').

omega_variable(
    interpretation_authority_ambiguity,
    'Who holds the authoritative interpretive power over OST Article II in the absence of a new international regime?',
    'Analysis of state practice, national legislation, and international court rulings (if any) regarding space resource activities.',
    'If states assert unilateral interpretive authority, the ''international_regime'' reading weakens, potentially shifting towards an ''extraction_permissive'' or ''commons_conservation'' reading depending on the dominant national interpretations. If no single authority emerges, the legal uncertainty persists, reinforcing the Scaffold classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_authority_ambiguity, conceptual, 'Ambiguity regarding the ultimate arbiter of OST Article II''s meaning.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the ''ost_article_ii_non_appropriation'' kernel. How would the classification change if a sibling reading (e.g., ''commons_conservation'' or ''extraction_permissive'') became authoritative?',
    'Resolution of the ''regime_negotiation_viability'' omega, or a definitive ruling by an international body or a widely adopted multilateral treaty.',
    'If ''commons_conservation'' became authoritative, the constraint would shift towards a Mountain (prohibitive natural law) or a Rope (strong coordination). If ''extraction_permissive'' became authoritative, it would shift towards a Snare (active extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__international_regime, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ost__tr_t10, ost_article_ii_non_appropriation__international_regime, theater_ratio, 10, 0.07).
narrative_ontology:measurement(ost__tr_t20, ost_article_ii_non_appropriation__international_regime, theater_ratio, 20, 0.08).
narrative_ontology:measurement(ost__tr_t30, ost_article_ii_non_appropriation__international_regime, theater_ratio, 30, 0.09).
narrative_ontology:measurement(ost__tr_t40, ost_article_ii_non_appropriation__international_regime, theater_ratio, 40, 0.1).
narrative_ontology:measurement(ost__tr_t50, ost_article_ii_non_appropriation__international_regime, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ost__be_t10, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(ost__be_t20, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(ost__be_t30, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(ost__be_t40, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(ost__be_t50, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t0, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(ost__su_t10, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(ost__su_t20, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(ost__su_t30, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 30, 0.19).
narrative_ontology:measurement(ost__su_t40, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(ost__su_t50, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the OST Article II non-appropriation kernel. This 'international_regime' reading defers the appropriation question to a future multilateral framework, creating a legal grey zone. It contrasts with the 'commons_conservation' reading (strict prohibition) and the 'extraction_permissive' reading (allows private ownership).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
