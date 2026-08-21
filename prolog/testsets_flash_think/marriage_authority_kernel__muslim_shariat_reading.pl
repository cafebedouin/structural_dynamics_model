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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Muslim Personal Law (Shariat) Marriage Authority
 *   domain: comparative_law/religious_governance/constitutional_pluralism
 *
 * SUMMARY:
 *   This constraint describes the authority of Muslim personal law in India,
 *   where marriage and family matters for Muslims are governed by Shariat as
 *   interpreted by community boards and qazis, rather than a uniform civil
 *   code. This reading emphasizes the preservation of religious identity and
 *   community autonomy, but is characterized by significant gender inequality
 *   in areas like divorce, polygamy, and inheritance. State intervention in
 *   these matters is highly contested, with community leaders often resisting
 *   secular legal reforms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.8).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.85).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Muslim Personal Law (Shariat) Marriage Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "comparative_law/religious_governance/constitutional_pluralism").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, 'f9f2092c-d008-4d61-9b30-825c7c24a49c').
narrative_ontology:cs_kernel_codification('f9f2092c-d008-4d61-9b30-825c7c24a49c', fixed_text).
narrative_ontology:cs_authority_grounding('f9f2092c-d008-4d61-9b30-825c7c24a49c', lineage).
narrative_ontology:cs_interpretation_layer_present('f9f2092c-d008-4d61-9b30-825c7c24a49c').
narrative_ontology:cs_reading_relation('f9f2092c-d008-4d61-9b30-825c7c24a49c', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9f2092c-d008-4d61-9b30-825c7c24a49c', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9f2092c-d008-4d61-9b30-825c7c24a49c', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9f2092c-d008-4d61-9b30-825c7c24a49c', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('f9f2092c-d008-4d61-9b30-825c7c24a49c', foundational, divine_law_supremacy).
narrative_ontology:cs_axiom_status(divine_law_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('f9f2092c-d008-4d61-9b30-825c7c24a49c', divine_law_supremacy, theological).
narrative_ontology:cs_axiom('f9f2092c-d008-4d61-9b30-825c7c24a49c', secondary, patriarchal_gender_roles).
narrative_ontology:cs_axiom_status(patriarchal_gender_roles, holdable).
narrative_ontology:cs_axiom_grounding('f9f2092c-d008-4d61-9b30-825c7c24a49c', patriarchal_gender_roles, conventional).
narrative_ontology:cs_reference_frame('f9f2092c-d008-4d61-9b30-825c7c24a49c', traditional_shariat_application).
narrative_ontology:cs_drift_state('f9f2092c-d008-4d61-9b30-825c7c24a49c', contemporary_pluralist_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9f2092c-d008-4d61-9b30-825c7c24a49c', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_muslim_community_members).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazis_and_personal_law_boards).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, female_muslim_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce Shariat-based family law for Muslims. They derive significant authority, social capital, and legitimacy from this role, acting as community adjudicators for marriage, divorce, and inheritance. Their authority is challenged by secular courts but largely respected within the community.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazis_and_personal_law_boards, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from provisions such as unilateral divorce (talaq), the possibility of polygamy, and generally greater inheritance shares. They experience the system as upholding religious tradition and their social status within the community, with strong social pressure to adhere to its norms.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_muslim_community_members, beneficiary,
    powerful, biographical, constrained, national).

% Bear the costs of unequal provisions, including limited recourse in divorce, challenges in inheritance, and social pressure to conform to patriarchal norms. Their religious and social identity is deeply intertwined with the community, making exit from the system (e.g., by opting for secular marriage) extremely difficult and often socially punitive.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, female_muslim_community_members, payer,
    powerless, biographical, identity_locked, national).

% Advocate for gender equality and a uniform civil code, challenging the discriminatory aspects of personal laws in higher courts. While they can operate outside the personal law system, their arguments are often dismissed by personal law boards as external interference in religious matters.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, secular_legal_advocates, excluded,
    organized, biographical, mobile, national).

% Oversee the personal law system, intervening in cases of gross injustice or constitutional violations (e.g., banning instant triple talaq). However, they generally defer to personal law boards on matters of religious interpretation and community practice, operating within a framework of legal pluralism.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, state_civil_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__muslim_shariat_reading, qazis_and_personal_law_boards).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a religiously sanctioned and communally administered framework for marriage, divorce, and inheritance for Muslim adherents, maintaining social cohesion and religious identity within the community.
% TRANSFER_FUNCTION: Transfers authority over family matters from the secular state to religious bodies (qazis and personal law boards); transfers rights and resources (e.g., divorce initiation, inheritance shares) from female to male community members.
% ABSENT_VOICES: Female Muslim scholars advocating for reformist interpretations of Shariat that prioritize gender equity, and those who have left the community due to its strictures. They would argue for interpretations that align with modern human rights principles and individual autonomy.
% DISAPPEARANCE_RATIONALE: If this authority vanished overnight, the Muslim community would face a profound vacuum in family law, leading to widespread social and legal disruption. It would necessitate a forced adoption of secular civil code or other personal laws, fundamentally altering community identity and religious practice.
% FOUNDING_PROBLEM: To provide a religiously sanctioned and communally administered system for family law (marriage, divorce, inheritance) for Muslims, distinct from other religious communities and colonial secular law, thereby preserving religious identity and community autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Adherents and personal law boards attest that the problem of maintaining religious identity and community autonomy in family matters is still live and essential. Secular legal scholars and women's rights activists argue that while the original problem of identity is acknowledged, the system's primary function has shifted to maintaining patriarchal power structures, with the founding problem now serving as a cover story for gender inequality. Legislative hearings and independent legal analyses from outside the benefiting parties support this shifted-function reading.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.8) due to the structural disadvantages faced by female Muslim community members under current interpretations of Shariat, particularly regarding unilateral divorce and inheritance. Suppression is very high (0.85) because social pressure, community norms, and limited legal alternatives within the system effectively trap individuals, especially women, within these arrangements. The theater ratio is low (0.1) as the system is actively functional and enforced by community institutions, not merely performative. Accessibility collapse is 0.7, as secular alternatives exist but are often culturally and socially prohibitive. Resistance is 0.6, reflecting ongoing advocacy for reform and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of qazis and many male community members, this system is a legitimate and necessary expression of religious identity and community self-governance, a 'rope' that coordinates religious life. From the perspective of female community members and secular advocates, it operates as a 'snare' or 'tangled rope,' extracting rights and autonomy under the guise of religious tradition, maintained by social and institutional suppression. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Qazis and personal law boards are clear beneficiaries and agenda-setters, deriving authority and social capital from their role. Male Muslim community members are also beneficiaries, gaining from patriarchal provisions and the maintenance of traditional social structures. Female Muslim community members are the primary payers, bearing the costs of unequal rights and limited autonomy, often identity-locked by their religious and social ties. Secular legal advocates and state civil courts act as observers or external challengers, excluded from the internal interpretive process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shariat_interpretation_flexibility,
    'To what extent are current interpretations of Shariat by personal law boards immutable, versus being open to reformist interpretations that align with gender equity principles?',
    'Analysis of historical fatwas and legal precedents, and engagement with diverse Islamic jurisprudence scholars to identify scope for re-interpretation within Shariat principles.',
    'If interpretations are found to be flexible, the constraint''s extractiveness could be reduced through internal reform, shifting it closer to a genuine coordination ''rope.'' If immutable, external legal intervention would be the only path to reduce extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shariat_interpretation_flexibility, conceptual, 'Ambiguity regarding the reformability of Shariat interpretations.').

omega_variable(
    state_intervention_legitimacy,
    'Is state intervention in religious personal laws, such as the ban on instant triple talaq, a legitimate exercise of constitutional authority or an infringement on religious freedom and community autonomy?',
    'Ongoing judicial review, constitutional debates, and public discourse on the balance between individual rights, religious freedom, and state authority in a pluralistic society.',
    'If state intervention is broadly accepted as legitimate, it could lead to further reforms reducing extraction. If widely rejected, it could strengthen community resistance and entrench the existing system, potentially increasing suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_intervention_legitimacy, preference, 'Contestation over the legitimacy and scope of state intervention in religious personal law.').

omega_variable(
    identity_lock_vs_choice,
    'For female Muslim community members, is the ''identity_locked'' exit option a result of genuine religious conviction and community belonging, or primarily a consequence of social pressure and lack of viable alternatives?',
    'Sociological studies on post-exit outcomes for individuals who opt for secular marriage or leave the community, assessing social, economic, and psychological costs.',
    'If primarily social pressure, the effective suppression and extractiveness are higher than perceived by beneficiaries. If genuine conviction, the ''cost'' of exit is self-imposed, altering the perceived extractiveness from an internal perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_choice, empirical, 'Distinguishing genuine identity-based adherence from coerced conformity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1947, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(marr_tr_t1960, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1947, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1947, 0.75).
narrative_ontology:measurement(marr_be_t1960, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1960, 0.77).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1975, 0.78).
narrative_ontology:measurement(marr_be_t1990, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1990, 0.79).
narrative_ontology:measurement(marr_be_t2005, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1947, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1947, 0.8).
narrative_ontology:measurement(marr_su_t1960, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1960, 0.82).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1975, 0.83).
narrative_ontology:measurement(marr_su_t1990, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1990, 0.84).
narrative_ontology:measurement(marr_su_t2005, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel,' which encompasses various religious and secular legal frameworks governing marriage and family law in India. Each reading represents a distinct structural claim with different beneficiaries, victims, and operational dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
