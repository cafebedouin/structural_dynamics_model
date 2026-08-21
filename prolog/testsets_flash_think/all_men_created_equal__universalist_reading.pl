% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Universalist Reading of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint represents the 'universalist reading' of the Declaration
 *   of Independence's phrase 'all men are created equal.' This reading
 *   interprets equality as a dynamic, universal principle that demands
 *   iterative expansion to include all individuals and groups, regardless of
 *   the original founders' intent or historical limitations. It serves as a
 *   moral and legal anchor for ongoing social justice movements, constantly
 *   challenging existing hierarchies and exclusions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.65).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.7).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Universalist Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, 'c767bf30-1e80-4326-8cd1-e62df48cf66b').
narrative_ontology:cs_kernel_codification('c767bf30-1e80-4326-8cd1-e62df48cf66b', fixed_text).
narrative_ontology:cs_authority_grounding('c767bf30-1e80-4326-8cd1-e62df48cf66b', lineage).
narrative_ontology:cs_interpretation_layer_present('c767bf30-1e80-4326-8cd1-e62df48cf66b').
narrative_ontology:cs_reading_relation('c767bf30-1e80-4326-8cd1-e62df48cf66b', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c767bf30-1e80-4326-8cd1-e62df48cf66b', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('c767bf30-1e80-4326-8cd1-e62df48cf66b', foundational, equality_as_inherent_dignity).
narrative_ontology:cs_axiom_status(equality_as_inherent_dignity, holdable).
narrative_ontology:cs_axiom_grounding('c767bf30-1e80-4326-8cd1-e62df48cf66b', equality_as_inherent_dignity, deontological).
narrative_ontology:cs_axiom('c767bf30-1e80-4326-8cd1-e62df48cf66b', foundational, rights_are_not_static).
narrative_ontology:cs_axiom_status(rights_are_not_static, holdable).
narrative_ontology:cs_axiom_grounding('c767bf30-1e80-4326-8cd1-e62df48cf66b', rights_are_not_static, conventional).
narrative_ontology:cs_reference_frame('c767bf30-1e80-4326-8cd1-e62df48cf66b', post_civil_war_amendments).
narrative_ontology:cs_drift_state('c767bf30-1e80-4326-8cd1-e62df48cf66b', contemporary_rights_movements, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c767bf30-1e80-4326-8cd1-e62df48cf66b', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, advocates_for_equality).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, those_denied_equal_status).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, status_quo_defenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the primary beneficiaries of the principle's expansion, gaining rights and recognition. However, they also bear significant costs in the struggle for that expansion, facing resistance and systemic barriers. Their identity is often deeply tied to the fight for equality.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion, payer).

% Organizations and individuals actively working to expand the application of the equality principle. They set the agenda for legal and social change, benefiting from the moral authority of the principle, but also expending considerable resources in the effort.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, advocates_for_equality, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, advocates_for_equality, beneficiary).

% Individuals and groups who benefit from existing hierarchies and resist the expansion of equality, viewing it as a threat to their status or traditional values. They 'pay' by losing unearned privilege or by having to adapt to new social norms, and actively work to limit the principle's scope.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, status_quo_defenders, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, status_quo_defenders, agenda_setter).

% Courts and judges interpret and apply the principle, often acting as a crucial mechanism for its expansion or limitation. Their decisions shape the practical scope of equality.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Scholars who argue that the principle's meaning is fixed by the original intent of the founders. From the universalist reading's perspective, their interpretive framework is excluded as a valid basis for determining the principle's contemporary scope.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_scholars, excluded,
    analytical, civilizational, analytical, universal).

% Analysts who highlight the inherent paradox between the universal language of the founding text and its historically restricted application. While not directly participating in the expansion, their critique influences the universalist project by underscoring the need for resolution.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, textualist_critics, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, diffuse).
narrative_ontology:fixing_cost_class(all_men_created_equal__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a foundational moral and legal principle that coordinates diverse social and political movements towards the iterative expansion of rights, recognition, and inclusion for all individuals and groups.
% TRANSFER_FUNCTION: Transfers moral and legal standing, rights, and access to resources from historically privileged groups to historically marginalized groups. It also transfers the burden of justification for any exclusion onto those who would deny equal status.
% ABSENT_VOICES: Future generations or groups whose claims to equality are not yet articulated or recognized within the current social and legal discourse. Also, non-human entities whose moral status might be considered under an even broader universalist framework.
% DISAPPEARANCE_RATIONALE: If the universalist reading of equality vanished, the entire framework for civil rights, social justice, and human rights movements in the United States would lose its foundational moral and legal anchor. The political and social landscape would profoundly reorganize, likely reverting to more static and hierarchical forms of social ordering.
% FOUNDING_PROBLEM: The inherent contradiction between the Declaration of Independence's universal language ('all men are created equal') and the historical reality of slavery, gender inequality, and other forms of exclusion at the nation's founding.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights movements, feminist movements, LGBTQ+ rights movements, disability rights movements, and international human rights bodies consistently invoke and expand upon this principle, providing corroboration from outside the original framers' intent. Scholarly consensus in critical legal studies and political philosophy also supports the ongoing nature of this foundational tension.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates societal efforts towards a more inclusive society (benefiting marginalized groups) but does so with significant, asymmetric extraction. The 'extraction' here refers to the costs borne by those who lose unearned privilege or who must adapt to new social norms, as well as the immense effort and struggle required from marginalized groups to achieve recognition. Suppression is high because the expansion of equality often requires actively overcoming entrenched resistance and discriminatory practices. Theater ratio is low, as the struggle for equality is a deeply functional and often confrontational process, not merely performative. Accessibility collapse is moderate; while the constraint aims to open access, it also makes alternatives to its own operation (i.e., maintaining static, limited equality) less legitimate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups, this constraint is a vital, albeit costly, engine for justice and liberation. From the perspective of status quo defenders, it is an imposition that unjustly extracts their traditional rights or social order. The engine's classification will reflect this divergence, showing a beneficial outcome for some seats and an extractive one for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups and advocates for equality are primary beneficiaries, as the constraint's operation directly improves their status and rights. However, they also act as payers, bearing the costs of activism, legal battles, and social resistance. Status quo defenders are victims, as the constraint extracts their unearned privileges and forces changes they resist. The judiciary acts as an agenda-setter, interpreting and applying the principle, thereby shaping its practical impact.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the ongoing struggle for equality as either pure coordination (ignoring the costs and resistance) or pure extraction (ignoring the genuine coordination function of expanding rights). The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that the constraint's mandate is still highly relevant and actively pursued, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_scope_ambiguity,
    'What is the ultimate, universal scope of ''all men'' in this principle? Does it extend beyond human beings to other sentient life, or to future forms of consciousness?',
    'Philosophical consensus on moral personhood, or future legal precedents extending rights to non-human entities.',
    'If the scope expands, the beneficiary and victim sets would broaden significantly, potentially increasing the perceived extractiveness and resistance from those who would maintain a human-centric view.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_scope_ambiguity, conceptual, 'Ambiguity regarding the ultimate boundaries of the ''universal'' principle.').

omega_variable(
    iterative_expansion_legitimacy,
    'Is the ''iterative expansion'' of equality a legitimate interpretive method, or does it fundamentally depart from the original meaning and intent of the founding document?',
    'Overwhelming judicial consensus, constitutional amendment, or a shift in public understanding of constitutional interpretation.',
    'If deemed illegitimate, the constraint''s moral and legal authority would significantly diminish, potentially reclassifying it as a Snare (if maintained by coercion) or Piton (if maintained by inertia). If affirmed, its legitimacy as a Tangled Rope would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iterative_expansion_legitimacy, conceptual, 'Contestation over the legitimacy of an evolving interpretation of equality.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression faced by status quo defenders primarily structural (e.g., legal mandates, institutional changes) or internalized (e.g., changing social norms, moral pressure)?',
    'Sociological studies tracking behavioral changes after legal shifts, or analysis of public discourse regarding social acceptance versus legal compliance.',
    'If largely internalized, the constraint''s effective suppression is higher than structural measures suggest, as resistance becomes harder to articulate or organize. If primarily structural, the constraint''s persistence depends more directly on active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for those resisting equality expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.1).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__universalist_reading, theater_ratio, 1865, 0.12).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.14).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__universalist_reading, theater_ratio, 1965, 0.16).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__universalist_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__universalist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.3).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__universalist_reading, base_extractiveness, 1865, 0.5).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__universalist_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__universalist_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__universalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.4).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__universalist_reading, suppression_requirement, 1865, 0.6).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(all__su_t1965, all_men_created_equal__universalist_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(all__su_t2000, all_men_created_equal__universalist_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__universalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, civil_rights_act_of_1964).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, voting_rights_act_of_1965).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, equal_pay_act_of_1963).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, americans_with_disabilities_act_of_1990).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, marriage_equality_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'all_men_created_equal' kernel. It focuses on the iterative expansion of equality as a universal principle, contrasting with originalist and textualist interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
