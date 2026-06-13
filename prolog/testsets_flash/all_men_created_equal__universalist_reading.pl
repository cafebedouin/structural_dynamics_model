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
 *   This constraint represents the 'universalist reading' of the phrase 'all
 *   men are created equal' from the US Declaration of Independence. This
 *   reading interprets the principle as a dynamic, evolving commitment to
 *   expanding equality to all persons, regardless of the specific social
 *   context or intent of the founders. It serves as a foundational moral and
 *   legal lever for civil rights movements and judicial expansion of rights.
 *   The constraint's extractiveness arises from the coordination costs of
 *   this ongoing expansion and the resistance it generates from those who
 *   lose privileged status. It is claimed as a Rope because its primary
 *   function is to coordinate social progress, despite the inherent friction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.45).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.3).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Universalist Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, '8ac73266-d2ab-4389-ae3d-c1015361684e').
narrative_ontology:cs_kernel_codification('8ac73266-d2ab-4389-ae3d-c1015361684e', fixed_text).
narrative_ontology:cs_authority_grounding('8ac73266-d2ab-4389-ae3d-c1015361684e', lineage).
narrative_ontology:cs_interpretation_layer_present('8ac73266-d2ab-4389-ae3d-c1015361684e').
narrative_ontology:cs_reading_relation('8ac73266-d2ab-4389-ae3d-c1015361684e', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ac73266-d2ab-4389-ae3d-c1015361684e', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('8ac73266-d2ab-4389-ae3d-c1015361684e', foundational, equality_is_dynamic_and_expansive).
narrative_ontology:cs_axiom_status(equality_is_dynamic_and_expansive, holdable).
narrative_ontology:cs_axiom_grounding('8ac73266-d2ab-4389-ae3d-c1015361684e', equality_is_dynamic_and_expansive, deontological).
narrative_ontology:cs_axiom('8ac73266-d2ab-4389-ae3d-c1015361684e', foundational, founder_intent_is_subordinate_to_universal_principle).
narrative_ontology:cs_axiom_status(founder_intent_is_subordinate_to_universal_principle, holdable).
narrative_ontology:cs_axiom_grounding('8ac73266-d2ab-4389-ae3d-c1015361684e', founder_intent_is_subordinate_to_universal_principle, deontological).
narrative_ontology:cs_reference_frame('8ac73266-d2ab-4389-ae3d-c1015361684e', evolving_moral_imperative).
narrative_ontology:cs_drift_state('8ac73266-d2ab-4389-ae3d-c1015361684e', contemporary_human_rights_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8ac73266-d2ab-4389-ae3d-c1015361684e', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_seeking_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, civil_rights_advocates).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, judicial_activists).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, groups_losing_privileged_status).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, traditionalists_resisting_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups (e.g., women, racial minorities, LGBTQ+ individuals) claim their inherent right to equal status under the principle, using it as a legal and moral lever for expanding rights and protections. Their identity is often tied to the struggle for this recognition.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_groups_seeking_inclusion, beneficiary,
    organized, generational, identity_locked, national).

% Legal and social organizations that actively interpret and apply the principle to new contexts, pushing for legislative and judicial expansion of equality. They invest significant resources in litigation and public education.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, civil_rights_advocates, agenda_setter,
    institutional, generational, constrained, national).

% Judges who interpret the 'all men are created equal' clause as a dynamic, evolving principle that requires reapplication to contemporary social realities, often leading to new rights or protections not explicitly envisioned by the founders. Their power is constrained by precedent and public opinion.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, judicial_activists, agenda_setter,
    institutional, biographical, constrained, national).

% Groups (e.g., historical beneficiaries of racial segregation, gender-based exclusions) whose traditional advantages or social hierarchies are challenged and dismantled by the expansion of equality. They bear the cost of losing established status and power.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, groups_losing_privileged_status, payer,
    organized, biographical, constrained, national).

% Individuals and movements who resist the expansion of equality, often on grounds of original intent, religious belief, or social conservatism. They bear the social and political costs of being on the losing side of legal and cultural shifts.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, traditionalists_resisting_change, payer,
    moderate, generational, constrained, national).

% Legal scholars and judges who argue that the meaning of the Constitution, including the equality principle, should be fixed to the original public meaning at the time of its adoption. They are excluded from the universalist interpretive process, though they actively contest its legitimacy.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_originalists, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a foundational moral and legal principle for coordinating the expansion of rights and social inclusion, allowing diverse groups to claim a common standard of justice and dignity within the American political system.
% TRANSFER_FUNCTION: Transfers social, legal, and political status from historically privileged groups to historically marginalized groups, and from a static, founder-centric interpretation to a dynamic, evolving one.
% ABSENT_VOICES: The founders themselves, if they could speak, would likely object to an interpretation that disregards their specific historical context and intent. Their 'voice' is mediated through historical texts and originalist scholarship.
% DISAPPEARANCE_RATIONALE: If the universalist reading of 'all men are created equal' vanished, the legal and moral basis for ongoing civil rights struggles and the expansion of equality would collapse. The entire framework for progressive social change in the US would need to be re-established on different grounds, leading to profound societal reorganization.
% FOUNDING_PROBLEM: The problem of establishing a legitimate basis for self-governance and individual rights, articulated in a way that could inspire a new nation, while simultaneously grappling with the inherent contradiction of slavery and other forms of inequality.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political philosophers widely corroborate the founding problem of establishing a nation on principles of liberty while practicing slavery. The 'live' status of the problem is attested by ongoing debates about systemic inequality and the unfinished project of American democracy, corroborated by civil rights organizations, academic scholarship, and international human rights bodies.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).
:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the ongoing social and political costs of expanding equality, which includes legal battles, legislative efforts, and shifts in social norms. This 'extraction' is the friction of progress, not rent-seeking. Suppression (0.30) is relatively low, as the universalist reading actively challenges existing suppressive structures rather than enforcing them; however, some suppression is inherent in overcoming resistance to change. Theater ratio (0.10) is low, indicating that the efforts to expand equality are largely genuine and functional, not performative. Resistance (0.70) is high, as this reading directly confronts entrenched interests and traditional interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups, this constraint is a powerful engine for justice and inclusion. From the perspective of traditionalists, it is an overreach that undermines foundational principles and established order. The engine's classification will reflect this divergence based on the declared roles and attributes.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups and civil rights advocates are clear beneficiaries (d near 0.0) as the constraint's operation directly expands their rights and status. Groups losing privileged status and traditionalists are victims/payers (d near 1.0) as they bear the costs of social and legal shifts. Judicial activists act as agenda-setters, actively shaping the interpretation and enforcement of the principle. Constitutional originalists are excluded, as their interpretive framework is fundamentally at odds with the universalist approach.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to expand equality) is very much 'live' and actively pursued, preventing mandatrophy. The ongoing resistance and the need for active enforcement demonstrate that its function has not atrophied. The 'extraction' is a necessary cost of coordinating a dynamic, inclusive social order, not a sign of a decaying or captured function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_universalism,
    'What are the ultimate boundaries of ''universal'' in this reading? Does it extend beyond human persons (e.g., to sentient AI, non-human animals, ecosystems)?',
    'Ongoing philosophical debate, future legal challenges, and societal consensus formation.',
    'If the scope expands further, the ''extractiveness'' and ''resistance'' metrics would likely increase as new groups claim inclusion, potentially shifting the classification towards a Tangled Rope due to increased coordination costs and conflict. If it is found to have inherent limits, the current metrics might stabilize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_universalism, conceptual, 'Ambiguity regarding the ultimate scope of the universalist principle.').

omega_variable(
    coordination_vs_extraction_of_progress,
    'Is the ''extractiveness'' measured truly a coordination cost of social progress, or does it contain elements of rent-seeking by specific advocacy groups or legal institutions?',
    'Detailed financial audits of advocacy organizations, analysis of legal fees and settlements, and examination of the distribution of benefits from expanded rights.',
    'If significant rent-seeking is identified, the constraint''s effective extraction would be higher than currently estimated, potentially pushing it towards a Tangled Rope or even Snare, as some beneficiaries would be extracting from the process of ''progress'' itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_of_progress, empirical, 'Distinguishing genuine coordination costs from potential rent-seeking within the process of expanding equality.').

omega_variable(
    founder_intent_relevance,
    'To what extent should founder intent, even if explicitly exclusionary, be considered in contemporary interpretations of the equality principle?',
    'Ongoing legal and philosophical debate, judicial rulings, and shifts in constitutional theory. This is a core conceptual conflict with the originalist reading.',
    'If founder intent gains more weight, the universalist reading''s legitimacy would be challenged, potentially reducing its ''power'' and ''scope'' and increasing ''resistance'' from those who adhere to it. This would shift the constraint''s operational profile significantly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founder_intent_relevance, conceptual, 'The irreducible tension between universalist interpretation and historical founder intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.05).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__universalist_reading, theater_ratio, 1865, 0.08).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__universalist_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__universalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.1).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__universalist_reading, base_extractiveness, 1865, 0.2).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.3).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__universalist_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__universalist_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.1).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__universalist_reading, suppression_requirement, 1865, 0.25).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.35).
narrative_ontology:measurement(all__su_t1965, all_men_created_equal__universalist_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__universalist_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, civil_rights_legislation).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, affirmative_action_policies).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, marriage_equality_doctrine).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'all_men_created_equal' kernel. This universalist reading emphasizes iterative expansion of equality, contrasting with the originalist reading (fixed intent) and the textualist paradox reading (focus on inherent contradiction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
