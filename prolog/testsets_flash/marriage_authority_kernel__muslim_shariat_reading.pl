% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Muslim Personal Law (Shariat) Marriage Authority
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority structure of Muslim personal law
 *   in India, where marriage and family matters are governed by Shariat as
 *   interpreted by community boards and qazis. It is one reading of the
 *   broader 'marriage_authority_kernel' in India, which also includes Hindu,
 *   Christian, Parsi, and secular civil readings. This reading is
 *   characterized by community adjudication and contested state intervention,
 *   often resulting in lower gender equity compared to secular alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.65).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.75).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Muslim Personal Law (Shariat) Marriage Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, '68f174d0-2dc9-42a9-a40d-d564b881594c').
narrative_ontology:cs_kernel_codification('68f174d0-2dc9-42a9-a40d-d564b881594c', formalized).
narrative_ontology:cs_authority_grounding('68f174d0-2dc9-42a9-a40d-d564b881594c', lineage).
narrative_ontology:cs_interpretation_layer_present('68f174d0-2dc9-42a9-a40d-d564b881594c').
narrative_ontology:cs_reading_relation('68f174d0-2dc9-42a9-a40d-d564b881594c', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('68f174d0-2dc9-42a9-a40d-d564b881594c', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('68f174d0-2dc9-42a9-a40d-d564b881594c', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('68f174d0-2dc9-42a9-a40d-d564b881594c', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('68f174d0-2dc9-42a9-a40d-d564b881594c', foundational, shariat_divine_and_immutable).
narrative_ontology:cs_axiom_status(shariat_divine_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('68f174d0-2dc9-42a9-a40d-d564b881594c', shariat_divine_and_immutable, theological).
narrative_ontology:cs_axiom('68f174d0-2dc9-42a9-a40d-d564b881594c', foundational, community_autonomy_in_personal_law).
narrative_ontology:cs_axiom_status(community_autonomy_in_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('68f174d0-2dc9-42a9-a40d-d564b881594c', community_autonomy_in_personal_law, conventional).
narrative_ontology:cs_reference_frame('68f174d0-2dc9-42a9-a40d-d564b881594c', traditional_shariat_application).
narrative_ontology:cs_drift_state('68f174d0-2dc9-42a9-a40d-d564b881594c', contemporary_constitutional_scrutiny, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('68f174d0-2dc9-42a9-a40d-d564b881594c', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_muslim_spouses).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazis).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, female_muslim_spouses).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, children_of_muslim_marriages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Shariat and administer Muslim personal law, including marriage, divorce, and inheritance. They assert autonomy from state intervention and resist attempts to codify or secularize these laws, deriving authority from religious tradition and community acceptance.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards, agenda_setter,
    institutional, generational, constrained, national).

% Religious judges who solemnize marriages, grant divorces (including unilateral talaq), and adjudicate family disputes according to Shariat. Their authority is recognized by the community and upheld by the personal law boards.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazis, agenda_setter,
    organized, biographical, constrained, local).

% Benefit from provisions like unilateral talaq (divorce without judicial intervention) and polygamy (up to four wives), as well as preferential inheritance rights. They generally support the existing personal law framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_muslim_spouses, beneficiary,
    moderate, biographical, mobile, local).

% Bear the costs of gender-unequal provisions, including vulnerability to unilateral divorce, limited rights in polygamous marriages, and disadvantageous inheritance laws. Their exit options are severely constrained by social norms, economic dependency, and religious identity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, female_muslim_spouses, payer,
    powerless, biographical, identity_locked, local).

% Are affected by the stability of their parents' marriage, custody arrangements, and inheritance laws, which are all governed by this framework. They have no agency in the system.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, children_of_muslim_marriages, payer,
    powerless, generational, trapped, local).

% Advocate for a uniform civil code and gender-equitable family laws, arguing that personal laws violate constitutional rights. They are excluded from the internal interpretive processes of the personal law boards but exert pressure through legislative and judicial channels.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, secular_legal_reformers, excluded,
    organized, generational, constrained, national).

% Intervenes in cases challenging the constitutionality of specific provisions of Muslim personal law, balancing religious freedom with fundamental rights. Its rulings can influence the interpretation and application of these laws.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, indian_supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for marriage, divorce, and family matters for the Muslim community, ensuring religious adherence and social order according to Shariat principles, and offering a recognized system for dispute resolution within the community.
% TRANSFER_FUNCTION: Transfers authority over family matters from individual autonomy or state civil courts to religious boards and qazis. It also transfers rights and privileges within marriage and inheritance from female spouses and children to male spouses.
% ABSENT_VOICES: Female Muslim scholars and activists who advocate for gender-just interpretations of Shariat or for the application of a uniform civil code are largely excluded from the decision-making processes of the personal law boards and qazis. Their perspectives are often marginalized within the community's religious discourse.
% DISAPPEARANCE_RATIONALE: If this authority structure vanished overnight, the Muslim community would face a legal vacuum for marriage and family matters. Individuals would either resort to secular civil law (Special Marriage Act) or create new, informal community-based systems, leading to significant social and legal reorganization.
% FOUNDING_PROBLEM: To provide a distinct legal framework for the Muslim community in India, allowing them to govern their personal lives according to their religious tenets, thereby preserving their cultural and religious identity in a pluralistic society.
% FOUNDING_PROBLEM_CORROBORATION: Muslim personal law boards and many male Muslim spouses assert that the problem of preserving religious identity and distinct legal practice is still live. However, female Muslim spouses, secular legal reformers, and some constitutional scholars argue that while identity preservation is important, the current framework's gender inequality has become the dominant problem, indicating the founding problem is either dead or has been superseded by new concerns. The Indian Supreme Court's interventions also suggest a contested status.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provides a genuine coordination function (religious identity, community dispute resolution) but also involves significant asymmetric extraction, primarily from female Muslim spouses and children. Extractiveness (0.65) is driven by gender-unequal provisions. Suppression (0.75) is high due to social norms, economic dependency, and the identity-locked nature of exit for many female spouses. Theater ratio (0.15) is low, indicating the system is actively functional, not merely performative, though its stated coordination goals are increasingly questioned by its extractive outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of male Muslim spouses and religious authorities, this system is a legitimate and necessary coordination mechanism for religious identity. From the perspective of female Muslim spouses and secular reformers, it is an extractive system that perpetuates gender inequality under the guise of religious freedom. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Male Muslim spouses, personal law boards, and qazis are beneficiaries, as they derive authority, privilege, or social order from the system. Female Muslim spouses and children are victims, bearing the costs of unequal provisions. Secular legal reformers are excluded, as their proposals challenge the very basis of this system. The Indian Supreme Court acts as an observer, intervening to balance religious freedom with constitutional rights.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shariat_interpretation_flexibility,
    'To what extent is the interpretation of Shariat by Muslim personal law boards and qazis flexible enough to incorporate modern gender-equitable principles without undermining its religious legitimacy?',
    'Analysis of judicial precedents and fatwas from progressive Islamic scholars and courts in other Muslim-majority countries that have reformed personal laws. Empirical study of community acceptance of such reforms.',
    'If flexible, the constraint could evolve towards lower extractiveness and suppression, potentially shifting towards a Rope. If rigid, its extractive nature is inherent to this reading, reinforcing its Tangled Rope classification or pushing it towards a Snare if coordination function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shariat_interpretation_flexibility, conceptual, 'Flexibility of Shariat interpretation regarding gender equity.').

omega_variable(
    state_intervention_legitimacy,
    'Is state intervention (e.g., through a Uniform Civil Code) perceived as a legitimate means to ensure gender equity in personal laws, or as an illegitimate infringement on religious freedom?',
    'Public opinion surveys across different communities, analysis of political discourse, and legal challenges to state interventions. The outcome of Supreme Court cases on personal law reform.',
    'If state intervention gains legitimacy, it could reduce suppression and extractiveness by providing alternative legal recourse, potentially shifting the constraint towards a more balanced Tangled Rope or even a Scaffold (if transitional). If illegitimate, resistance to reform will remain high, perpetuating the current extractive dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_intervention_legitimacy, preference, 'Legitimacy of state intervention in religious personal laws.').

omega_variable(
    identity_lock_vs_structural_barriers,
    'For female Muslim spouses, what proportion of their ''identity_locked'' exit option is due to internalized religious identity fusion versus structural barriers (economic dependency, social ostracization)?',
    'Qualitative sociological studies, post-exit trajectory analysis for those who do exit, and comparison of outcomes for women with varying levels of economic independence and social support.',
    'If primarily internalized, the effective suppression is higher and more resistant to external legal changes. If primarily structural, economic empowerment and legal reforms could more directly reduce suppression and improve exit options, potentially lowering extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_barriers, empirical, 'Structural vs. internalized suppression mechanism for female spouses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1937, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(marr_tr_t1960, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(marr_tr_t1980, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t1937, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1937, 0.55).
narrative_ontology:measurement(marr_be_t1960, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(marr_be_t1980, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1980, 0.62).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1937, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1937, 0.65).
narrative_ontology:measurement(marr_su_t1960, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1960, 0.68).
narrative_ontology:measurement(marr_su_t1980, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel' in India, focusing on Muslim personal law. Its structural features and outcomes differ significantly from other religious and secular readings of the same kernel, necessitating separate constraint stories linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
