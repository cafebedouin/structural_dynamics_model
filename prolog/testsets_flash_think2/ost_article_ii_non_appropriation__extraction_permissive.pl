% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: OST Article II: Permissive Extraction Interpretation
 *   domain: international_space_law/commons_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'extraction_permissive' reading of
 *   Article II of the Outer Space Treaty (OST). This interpretation holds
 *   that while sovereign territorial claims in outer space are barred,
 *   private entities are permitted to extract and own resources under the
 *   jurisdiction of their national laws. This reading facilitates the
 *   commercialization of space resources by technologically advanced states
 *   and private companies, leading to a de facto enclosure of the space
 *   commons.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.78).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.7).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.78).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "OST Article II: Permissive Extraction Interpretation").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_space_law/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '743ca291-b1ea-43a7-b9b5-7ef7d9e99052').
narrative_ontology:cs_kernel_codification('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', fixed_text).
narrative_ontology:cs_authority_grounding('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', extraction).
narrative_ontology:cs_interpretation_layer_present('743ca291-b1ea-43a7-b9b5-7ef7d9e99052').
narrative_ontology:cs_reading_relation('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', foundational, sovereign_claims_only_barred).
narrative_ontology:cs_axiom_status(sovereign_claims_only_barred, holdable).
narrative_ontology:cs_axiom_grounding('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', sovereign_claims_only_barred, conventional).
narrative_ontology:cs_axiom('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', foundational, private_entities_not_sovereigns).
narrative_ontology:cs_axiom_status(private_entities_not_sovereigns, holdable).
narrative_ontology:cs_axiom_grounding('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', private_entities_not_sovereigns, conventional).
narrative_ontology:cs_reference_frame('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', technological_frontier_access).
narrative_ontology:cs_drift_state('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', contemporary_space_resource_utilization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('743ca291-b1ea-43a7-b9b5-7ef7d9e99052', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_space_mining_companies).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, developing_nations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states interpret Article II as permitting private resource extraction under their national jurisdiction, enabling their domestic companies to pursue space mining. They benefit strategically and economically from this access, and actively defend this interpretation in international forums.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These companies directly engage in prospecting and extraction of space resources, operating under the legal frameworks provided by technologically advanced states. They are the primary economic beneficiaries of the permissive interpretation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, private_space_mining_companies, beneficiary,
    powerful, biographical, mobile, global).

% These nations lack the technological and financial capacity to access space resources themselves. They bear the cost of the global commons being enclosed by a few actors, with no mechanism for equitable sharing or compensation, and their protests are largely unheeded.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, developing_nations, payer,
    powerless, generational, trapped, global).

% These generations will inherit a space environment where resources may be depleted or already appropriated, limiting their future options for space utilization and benefit. Their interests are not represented in current legal interpretations.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations, payer,
    powerless, civilizational, identity_locked, universal).

% Academics and legal experts who analyze the Outer Space Treaty and its interpretations, often highlighting the ambiguities and potential for inequitable outcomes. They contribute to the debate but have no direct enforcement power.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% The primary UN body for international cooperation in space, tasked with discussing legal issues. While it provides a forum, it lacks the mandate or enforcement power to prevent de facto appropriation under the permissive interpretation, effectively excluded from shaping the outcome.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, un_committee_on_peaceful_uses_of_outer_space, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_states).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for technologically capable states and private entities to pursue space resource utilization, preventing a chaotic free-for-all among these actors by establishing national oversight and property rights over extracted resources.
% TRANSFER_FUNCTION: Transfers potential economic and strategic value from the global commons of outer space to the private companies and national economies of technologically advanced states, at the expense of other nations and future generations.
% ABSENT_VOICES: Developing nations and future generations are largely absent from the decision-making processes that shape this interpretation. Advocates for a 'common heritage of mankind' principle for space resources are marginalized by the current legal and technological realities.
% DISAPPEARANCE_RATIONALE: If this permissive interpretation vanished, private space resource extraction would likely halt or be severely curtailed due to legal uncertainty and increased international contestation. This would fundamentally alter the nascent space economy and force a renegotiation of international space law, leading to a significant rearrangement of power and resource access.
% FOUNDING_PROBLEM: The Outer Space Treaty was established to prevent the militarization and national appropriation of outer space, ensuring its use for the benefit and in the interests of all countries, and promoting international cooperation.
% FOUNDING_PROBLEM_CORROBORATION: Technologically advanced states and private companies argue this interpretation prevents a regulatory vacuum and fosters innovation, aligning with the spirit of peaceful utilization. Developing nations and many international legal scholars contest this, arguing it undermines the 'common heritage' principle and enables a new form of resource colonialism, citing UN resolutions and scholarly critiques as corroboration.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is high (0.78) because this interpretation allows for the unilateral appropriation of resources from a global commons without a compensation or sharing mechanism for excluded parties. Suppression (0.70) is significant, as access is effectively denied to nations lacking advanced space capabilities, and this exclusion is maintained through legal interpretations and diplomatic pressure. The theater ratio is low (0.15) because the extraction and appropriation are real and functional, not merely performative. Accessibility collapse is moderate (0.60) as alternatives for resource access (e.g., through an international regime) are actively suppressed by the current interpretation, but not entirely foreclosed conceptually. Resistance (0.55) is present from developing nations and legal scholars, but insufficient to alter the dominant interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of technologically advanced states and private companies, this interpretation provides necessary legal clarity for investment and innovation, framing it as a 'rope' that coordinates orderly development. From the perspective of developing nations and advocates for a common heritage, it functions as a 'snare' that legitimizes unilateral resource grabs and perpetuates global inequalities. The engine's classification as 'tangled_rope' reflects this hybrid nature, acknowledging a coordination function for some while highlighting asymmetric extraction from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Technologically advanced states and private space mining companies are clear beneficiaries, gaining exclusive access to valuable resources. Developing nations and future generations are the primary victims, excluded from access and bearing the long-term costs of resource enclosure. International legal scholars and UN committees act as observers or excluded parties, highlighting the inequity but lacking direct power to change the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ost_article_ii_interpretation_ambiguity,
    'Is Article II''s non-appropriation principle truly limited to sovereign territorial claims, or does it implicitly extend to private resource extraction, given the spirit of the treaty?',
    'A definitive ruling by an international court with universal jurisdiction, or a new multilateral treaty explicitly clarifying the scope of non-appropriation for private actors.',
    'If found to extend to private extraction, this reading would be reclassified as a ''snare'' (pure extraction) or ''piton'' (if the original intent is entirely lost), and the beneficiaries would become targets. If confirmed as limited, the ''tangled_rope'' classification would be reinforced, but the ethical questions would remain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ost_article_ii_interpretation_ambiguity, conceptual, 'Ambiguity regarding the scope of Article II''s non-appropriation principle.').

omega_variable(
    future_generations_standing,
    'Do future generations have legal or moral standing to claim a share of space resources, or is current technological capability the sole determinant of access and ownership?',
    'The establishment of international legal precedents or a new treaty recognizing the rights of future generations to common resources, potentially leading to a ''common heritage'' regime for space.',
    'If future generations gain standing, the ''extraction_permissive'' reading''s legitimacy would be severely undermined, potentially reclassifying it as a ''snare'' due to the explicit victimhood of future generations. If not, the current ''tangled_rope'' classification remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_standing, preference, 'The legal and moral standing of future generations regarding space resources.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the exclusion of developing nations from space resource access primarily structural (lack of technology/capital) or actively enforced through legal interpretations and diplomatic pressure by advanced states?',
    'Analysis of diplomatic records, voting patterns in international bodies, and the impact of national space laws on international cooperation. If active resistance to equitable sharing is documented, it points to active enforcement.',
    'If active enforcement is the primary mechanism, the suppression metric for this constraint is higher than currently estimated, pushing the classification closer to a ''snare''. If purely structural, the ''tangled_rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. actively enforced suppression of developing nations'' access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(ost__tr_t1995, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(ost__tr_t2024, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(ost__be_t2024, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(ost__su_t1995, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(ost__su_t2024, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, space_debris_liability_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, lunar_settlement_governance_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Outer Space Treaty's Article II non-appropriation principle, alongside 'commons_conservation' and 'international_regime'. Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
