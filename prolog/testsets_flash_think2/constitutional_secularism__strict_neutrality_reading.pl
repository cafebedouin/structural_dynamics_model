% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism: Strict Neutrality Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint represents the 'strict neutrality' reading of
 *   constitutional secularism, where the state maintains an equal distance
 *   from all religions, refraining from preferential treatment or
 *   interference. It aims to ensure religious freedom and prevent state
 *   capture by any religious group. This reading emphasizes uniform
 *   constraint application across communities, limits state capacity for
 *   religious reform, and preserves minority autonomy, though it may leave
 *   minorities vulnerable to majority social norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.35).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.55).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism: Strict Neutrality Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, 'd4cc28dd-b299-409c-bd81-2eb026e3895d').
narrative_ontology:cs_kernel_codification('d4cc28dd-b299-409c-bd81-2eb026e3895d', fixed_text).
narrative_ontology:cs_authority_grounding('d4cc28dd-b299-409c-bd81-2eb026e3895d', lineage).
narrative_ontology:cs_interpretation_layer_present('d4cc28dd-b299-409c-bd81-2eb026e3895d').
narrative_ontology:cs_reading_relation('d4cc28dd-b299-409c-bd81-2eb026e3895d', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4cc28dd-b299-409c-bd81-2eb026e3895d', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('d4cc28dd-b299-409c-bd81-2eb026e3895d', foundational, state_religious_impartiality).
narrative_ontology:cs_axiom_status(state_religious_impartiality, holdable).
narrative_ontology:cs_axiom_grounding('d4cc28dd-b299-409c-bd81-2eb026e3895d', state_religious_impartiality, deontological).
narrative_ontology:cs_axiom('d4cc28dd-b299-409c-bd81-2eb026e3895d', foundational, non_interference_principle).
narrative_ontology:cs_axiom_status(non_interference_principle, holdable).
narrative_ontology:cs_axiom_grounding('d4cc28dd-b299-409c-bd81-2eb026e3895d', non_interference_principle, deontological).
narrative_ontology:cs_reference_frame('d4cc28dd-b299-409c-bd81-2eb026e3895d', founding_constitutional_principles).
narrative_ontology:cs_drift_state('d4cc28dd-b299-409c-bd81-2eb026e3895d', contemporary_pluralistic_society, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d4cc28dd-b299-409c-bd81-2eb026e3895d', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, non_believers).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, state_institutions).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, religious_majorities).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, religious_organizations_seeking_state_support).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The government and judiciary responsible for upholding the constitutional principle of secularism by ensuring no religion receives preferential treatment or faces interference. They actively enforce laws and policies to maintain this distance.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from protection against the dominance of majority religions and state-sponsored discrimination. Their autonomy in religious practice is generally preserved, but they may feel vulnerable to majority social norms in the public sphere.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from the absence of state-imposed religious practices or beliefs, ensuring their freedom from religious coercion. They are protected from religious influence in public institutions.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, non_believers, beneficiary,
    moderate, biographical, constrained, national).

% Bear the cost of not having their religious traditions or values preferentially recognized or supported by the state. They may perceive the strict neutrality as a curtailment of their public religious expression or influence.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_majorities, payer,
    powerful, generational, constrained, national).

% Are denied direct financial or institutional support from the state, which they might otherwise seek to promote their faith or social services. They must operate independently without state endorsement.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_organizations_seeking_state_support, payer,
    organized, biographical, constrained, national).

% Believe the state should intervene in religious affairs to advance social reform or protect weaker sections within communities. Their perspective is excluded by the strict neutrality reading, which limits state capacity for such interventions.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, advocates_for_principled_intervention, excluded,
    moderate, biographical, constrained, national).

% Argue for an affirmative state duty to eliminate religious practices oppressing marginalized groups, even if it supersedes religious autonomy claims. This view is foreclosed by the strict neutrality reading's non-interference principle.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, reformist_activists, excluded,
    moderate, biographical, constrained, national).

% Analyze the application and implications of strict neutrality, debating its effectiveness, fairness, and consistency with other constitutional principles. They provide critical commentary but do not directly enforce or benefit from the constraint.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent religious conflict and ensure equal citizenship for all individuals, regardless of their religious or non-religious identity, by establishing the state as an impartial arbiter.
% TRANSFER_FUNCTION: Transfers state endorsement, resources, and preferential treatment away from all religious groups, preventing any single religion from capturing state power or resources. It also transfers the burden of self-sustenance to religious organizations.
% ABSENT_VOICES: Advocates for state-supported religion (e.g., religious nationalists) and those who believe the state has a duty to intervene in religious communities for social reform (e.g., principled interventionists, reformist activists) are structurally excluded from the framework of strict neutrality.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished, the state would likely be captured by dominant religious groups, leading to the establishment of a state religion, discrimination against minorities and non-believers, and increased religious conflict. Public institutions would become sites of religious contestation.
% FOUNDING_PROBLEM: Historical religious conflicts, discrimination against religious minorities, and the potential for state capture by powerful religious factions, leading to social instability and inequality.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of religious wars and persecution, contemporary reports of religious discrimination in countries lacking secular governance, and analyses by international human rights organizations corroborate the ongoing relevance of these problems. The state's own stability is also a corroborating factor.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).
:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because while the principle aims for non-extraction, its enforcement can be perceived as extracting public influence or support from religious groups, particularly majorities. Suppression is moderate (0.55) as the state must actively suppress attempts at religious favoritism or interference, which can feel coercive to those seeking state endorsement. Theater ratio is low (0.1) because the principle is generally genuinely applied, even if its interpretation is contested. Accessibility collapse is high (0.8) for the alternative of state-sponsored religion, as this reading fundamentally rejects it. Resistance is moderate (0.45) from groups seeking greater religious influence or feeling marginalized by the strict application.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious minorities and non-believers, strict neutrality is a protective 'rope' ensuring equality. From the perspective of religious majorities or organizations seeking state support, it can feel like a 'snare' that curtails their public influence and denies them legitimate support. The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State institutions are agenda-setters, enforcing the principle. Religious minorities and non-believers are beneficiaries, protected from discrimination and coercion. Religious majorities and organizations seeking state support are payers, as they are denied preferential treatment or state endorsement. Advocates for principled intervention and reformist activists are excluded, as their approaches conflict with strict neutrality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to prevent religious conflict and ensure equality remains live. The classification as a 'rope' reflects its genuine coordination function. The moderate extractiveness and suppression are acknowledged as costs of maintaining this coordination in a diverse society, preventing mislabeling as pure extraction unless these metrics significantly increase without corresponding coordination benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_secular_bias,
    'Does the strict neutrality reading, in practice, implicitly favor a secular worldview or the norms of the dominant non-religious majority, thereby creating a de facto disadvantage for religious expression?',
    'Empirical studies analyzing the impact of strict neutrality policies on diverse religious practices and expressions in public life, particularly for minority religions, compared to the impact on secular norms.',
    'If an implicit bias is confirmed, the effective extractiveness and suppression for religious groups would be higher than currently measured, potentially shifting the classification towards a ''tangled_rope'' or ''snare'' for those groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_secular_bias, empirical, 'Whether strict neutrality has an unacknowledged secular bias.').

omega_variable(
    state_intervention_necessity,
    'Is strict neutrality''s non-interference principle structurally adequate to protect vulnerable individuals or groups within religious communities from internal oppression, or does it prevent necessary state intervention?',
    'Comparative legal analysis of jurisdictions with different secularism models, examining outcomes for vulnerable groups (e.g., women, LGBTQ+ individuals) within religious communities, alongside human rights reports.',
    'If strict neutrality is found to systematically fail in protecting vulnerable groups, its coordination function would be undermined, and the ''principled_intervention_reading'' or ''reformist_reading'' would gain structural legitimacy, potentially reclassifying this reading as a ''piton'' or ''snare'' for those vulnerable groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_intervention_necessity, conceptual, 'Adequacy of non-interference for internal community protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__strict_neutrality_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__strict_neutrality_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__strict_neutrality_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__strict_neutrality_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cons_tr_t50, constitutional_secularism__strict_neutrality_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(cons_be_t50, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(cons_su_t50, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, public_education_religious_instruction).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, religious_symbols_in_public_spaces).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, religious_personal_laws_recognition).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of 'constitutional_secularism', each with different structural properties and classifications. This 'strict_neutrality_reading' emphasizes non-interference and equal distance, contrasting with 'principled_intervention_reading' and 'reformist_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
