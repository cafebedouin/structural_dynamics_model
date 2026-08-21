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
 *   human_readable: OST Article II Non-Appropriation: Deferral to Future International Regime
 *   domain: international_law/space_governance
 *
 * SUMMARY:
 *   This constraint represents the 'international regime' reading of Outer
 *   Space Treaty (OST) Article II, which states that outer space is 'not
 *   subject to national appropriation by claim of sovereignty, by means of
 *   use or occupation, or by any other means.' This reading interprets
 *   Article II as deferring the question of resource appropriation to a
 *   future international framework, akin to Article XI's call for a regime
 *   for the Moon and other celestial bodies. In this view, neither a strict
 *   prohibition on all resource extraction (commons_conservation) nor a
 *   permissive stance on private ownership (extraction_permissive) is fully
 *   authoritative in the absence of such a multilateral agreement. This
 *   creates a Scaffold-like situation where legal uncertainty persists,
 *   allowing first-mover firms to operate in a regulatory grey zone, while
 *   negotiations for a comprehensive regime are stalled by zero-sum
 *   distributional conflicts.
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
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Non-Appropriation: Deferral to Future International Regime").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_law/space_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__international_regime).
narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, '37004b00-cacc-4e68-ad9c-196d8ca19faf').
narrative_ontology:cs_kernel_codification('37004b00-cacc-4e68-ad9c-196d8ca19faf', fixed_text).
narrative_ontology:cs_authority_grounding('37004b00-cacc-4e68-ad9c-196d8ca19faf', lineage).
narrative_ontology:cs_interpretation_layer_present('37004b00-cacc-4e68-ad9c-196d8ca19faf').
narrative_ontology:cs_reading_relation('37004b00-cacc-4e68-ad9c-196d8ca19faf', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('37004b00-cacc-4e68-ad9c-196d8ca19faf', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_axiom('37004b00-cacc-4e68-ad9c-196d8ca19faf', foundational, appropriation_requires_explicit_regime).
narrative_ontology:cs_axiom_status(appropriation_requires_explicit_regime, holdable).
narrative_ontology:cs_axiom_grounding('37004b00-cacc-4e68-ad9c-196d8ca19faf', appropriation_requires_explicit_regime, conventional).
narrative_ontology:cs_reference_frame('37004b00-cacc-4e68-ad9c-196d8ca19faf', ost_original_intent_deferral).
narrative_ontology:cs_drift_state('37004b00-cacc-4e68-ad9c-196d8ca19faf', contemporary_space_resource_boom, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('37004b00-cacc-4e68-ad9c-196d8ca19faf', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, space_resource_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, states_with_space_capabilities).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, future_generations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, developing_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the current legal ambiguity, allowing them to operate in a regulatory grey zone without clear prohibitions on resource extraction or ownership. They advocate for a permissive interpretation or a regime that legitimizes their activities.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_resource_firms, beneficiary,
    organized, biographical, mobile, global).

% Are the primary actors in negotiating any future regime. They benefit from the current deferral as it allows them to develop national space resource policies and support their domestic industries without immediate international constraints. Some also benefit from the ambiguity to pursue their own resource interests.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, states_with_space_capabilities, agenda_setter,
    institutional, generational, constrained, global).

% Bear the potential long-term costs of unregulated resource extraction, including environmental damage, inequitable distribution of benefits, and the loss of a shared heritage. They have no direct voice in current negotiations.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Are concerned about equitable access to space resources and the potential for a new 'space race' that excludes them. They bear the cost of not having a voice in the initial resource grab and face potential future resource scarcity or dependency.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, developing_nations, payer,
    moderate, generational, constrained, global).

% Analyze the legal implications of Article II, the history of its negotiation, and the need for a new international regime. They highlight the legal uncertainty and the risks of de facto appropriation in the absence of clear rules.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the expectation that a future international regime will eventually clarify appropriation rules for space resources, preventing immediate unilateral claims of sovereignty while deferring the harder questions of resource ownership and benefit sharing.
% TRANSFER_FUNCTION: The constraint transfers the burden of establishing clear appropriation rules from the present (when the OST was signed) to a future, as-yet-unnegotiated international regime. It also implicitly transfers potential resource benefits to first-mover firms and states, at the potential cost of future generations and developing nations.
% ABSENT_VOICES: The voices of future generations are absent, as are those of non-state actors who might advocate for a stronger 'common heritage' principle. Indigenous communities, whose terrestrial resource rights are often violated, also lack a voice in shaping space resource governance.
% DISAPPEARANCE_RATIONALE: If this deferral interpretation vanished, states and private entities would immediately face a stark choice between a strong prohibition on all appropriation (as per the commons_conservation reading) or a free-for-all (as per the extraction_permissive reading). This would either halt space resource development or accelerate unilateral claims, fundamentally altering the legal and economic landscape of space.
% FOUNDING_PROBLEM: The original problem was to prevent a 'new scramble for Africa' in space, avoiding national appropriation of celestial bodies and ensuring space exploration benefited all humanity, while acknowledging the nascent stage of space resource technology.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and developing nations corroborate that the problem of preventing unilateral appropriation and ensuring equitable benefit sharing remains live, especially with advancing space resource technologies. Space resource firms and states with capabilities acknowledge the need for clarity but dispute the urgency or the scope of 'appropriation'.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate because while the constraint doesn't explicitly permit extraction, its deferral creates a vacuum that allows de facto appropriation by those with the means. Suppression (0.20) is low because there's no active enforcement against resource extraction itself, only against national sovereignty claims. Theater ratio (0.10) is low as the deferral is a genuine legal position, not primarily performative. Accessibility collapse (0.30) is low as alternatives (unilateral action, different interpretations) are actively pursued. Resistance (0.40) is moderate, coming from developing nations and scholars advocating for a stronger common heritage principle.
 *
 * PERSPECTIVAL GAP:
 *   States with space capabilities and space resource firms perceive this deferral as a pragmatic, temporary solution that allows for innovation, while developing nations and future generations view it as a dangerous loophole enabling a new form of colonial resource grab. The engine's classification will reflect this divergence based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Space resource firms and states with space capabilities are beneficiaries, as the deferral allows them to advance their interests in a legally ambiguous environment. Future generations and developing nations are victims, bearing the costs of potential inequitable resource distribution and environmental impacts without a clear protective framework. International legal scholars act as observers, analyzing the legal landscape.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_negotiation_viability,
    'Is a comprehensive international regime for space resource appropriation genuinely viable, or is the deferral a permanent state of regulatory capture by first-movers?',
    'Observation of progress in UN COPUOS or other multilateral fora; analysis of state positions and willingness to compromise on benefit-sharing principles.',
    'If negotiations are genuinely stalled by irreconcilable interests, the ''scaffold'' classification is misleading, and the constraint functions more like a ''snare'' for future generations, enabling de facto extraction. If a viable path to a regime exists, the scaffold remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_negotiation_viability, empirical, 'Assesses whether the ''scaffold'' is truly temporary or a de facto permanent state.').

omega_variable(
    de_facto_appropriation_threshold,
    'At what point does ''use or occupation'' for resource extraction constitute de facto appropriation, even without a formal sovereignty claim?',
    'Legal precedent from international courts or arbitral tribunals, or a consensus interpretation emerging from state practice and scholarly opinion.',
    'A low threshold for de facto appropriation would strengthen the ''commons_conservation'' reading and increase the perceived extractiveness of the current deferral. A high threshold would reinforce the ''extraction_permissive'' reading, reducing perceived extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(de_facto_appropriation_threshold, conceptual, 'Defines the boundary between permissible resource use and prohibited appropriation.').

omega_variable(
    common_heritage_principle_status,
    'To what extent does the ''common heritage of mankind'' principle (from the Moon Agreement, Article XI) apply to space resources under the OST, given the OST''s silence on resource ownership?',
    'State declarations, subsequent treaties, or advisory opinions from international bodies clarifying the relationship between the OST and the Moon Agreement''s principles.',
    'If the common heritage principle is deemed applicable, the current deferral is more extractive and unjust. If it''s considered non-binding or irrelevant to OST Article II, the deferral is less problematic from a common heritage perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_heritage_principle_status, conceptual, 'Clarifies the normative framework for space resource governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1980, 0.07).
narrative_ontology:measurement(ost__tr_t1995, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(ost__tr_t2024, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(ost__be_t2024, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1967, 0.1).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1980, 0.12).
narrative_ontology:measurement(ost__su_t1995, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(ost__su_t2024, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Outer Space Treaty's Article II non-appropriation principle. This 'international_regime' reading interprets Article II as deferring resource appropriation to a future international framework, creating a Scaffold-like situation of legal uncertainty. It contrasts with the 'commons_conservation' reading (prohibiting de facto appropriation) and the 'extraction_permissive' reading (allowing private ownership of extracted resources).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
