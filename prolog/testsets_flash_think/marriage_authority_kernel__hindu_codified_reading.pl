% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Marriage Act 1955 Authority (Codified Reading)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint story analyzes the Hindu Marriage Act 1955 as a codified
 *   reading of the broader 'marriage_authority_kernel' in India. It focuses
 *   on how the Act, interpreted by civil courts, provides a uniform legal
 *   framework for Hindus while simultaneously entrenching certain gender
 *   inequalities. The 'claimed_type' of Tangled Rope reflects its dual
 *   function: coordinating family law within the Hindu community while
 *   extracting from Hindu women through its 'moderate gender equity' and
 *   requiring active state enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.65).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.55).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Act 1955 Authority (Codified Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'b552435f-ce89-4f3c-922f-9805b3955f3e').
narrative_ontology:cs_kernel_codification('b552435f-ce89-4f3c-922f-9805b3955f3e', formalized).
narrative_ontology:cs_authority_grounding('b552435f-ce89-4f3c-922f-9805b3955f3e', lineage).
narrative_ontology:cs_interpretation_layer_present('b552435f-ce89-4f3c-922f-9805b3955f3e').
narrative_ontology:cs_reading_relation('b552435f-ce89-4f3c-922f-9805b3955f3e', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('b552435f-ce89-4f3c-922f-9805b3955f3e', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('b552435f-ce89-4f3c-922f-9805b3955f3e', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('b552435f-ce89-4f3c-922f-9805b3955f3e', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('b552435f-ce89-4f3c-922f-9805b3955f3e', foundational, hindu_law_as_primary_authority_for_hindus).
narrative_ontology:cs_axiom_status(hindu_law_as_primary_authority_for_hindus, holdable).
narrative_ontology:cs_axiom_grounding('b552435f-ce89-4f3c-922f-9805b3955f3e', hindu_law_as_primary_authority_for_hindus, conventional).
narrative_ontology:cs_axiom('b552435f-ce89-4f3c-922f-9805b3955f3e', foundational, gender_equity_within_traditional_framework).
narrative_ontology:cs_axiom_status(gender_equity_within_traditional_framework, holdable).
narrative_ontology:cs_axiom_grounding('b552435f-ce89-4f3c-922f-9805b3955f3e', gender_equity_within_traditional_framework, conventional).
narrative_ontology:cs_reference_frame('b552435f-ce89-4f3c-922f-9805b3955f3e', post_independence_codification).
narrative_ontology:cs_drift_state('b552435f-ce89-4f3c-922f-9805b3955f3e', contemporary_gender_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b552435f-ce89-4f3c-922f-9805b3955f3e', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_leaders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_courts).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_men).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, interfaith_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and interpret the Hindu Marriage Act, upholding its principles and ensuring community adherence. They benefit from the stability and uniformity it provides within the Hindu community, reinforcing their traditional authority.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_community_leaders, agenda_setter,
    institutional, generational, constrained, national).

% Enforce the provisions of the Hindu Marriage Act, adjudicating disputes related to marriage, divorce, and inheritance within the Hindu community. They provide legal certainty and state backing to the codified religious law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_courts, agenda_setter,
    institutional, generational, constrained, national).

% Generally benefit from the existing interpretations of the Act, which, despite reforms, often retain elements that favor traditional patriarchal structures, particularly concerning property rights and marital roles. They experience the law as a stable framework for family life.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_men, beneficiary,
    powerful, biographical, constrained, national).

% Bear the costs of the Act's 'moderate gender equity.' While the Act brought significant reforms, its interpretation and application can still disadvantage women in areas like property rights, maintenance, and divorce, compared to a fully secular or more progressive framework. Social pressure often limits their ability to seek alternatives.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women, payer,
    powerless, biographical, constrained, national).

% Are explicitly excluded from the Hindu Marriage Act, forcing them to marry under the Special Marriage Act 1954. This exclusion can carry social stigma and legal complexities, making their choice of marriage framework a constrained one.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_couples, excluded,
    powerless, biographical, constrained, national).

% Actively campaign for further reforms to the Hindu Marriage Act and its interpretation to achieve full gender equality. They analyze the law's impact on women and challenge its discriminatory aspects through legal and social activism.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, women_rights_advocates, observer,
    organized, generational, analytical, national).

% Advocate for a Uniform Civil Code that would replace all religion-specific personal laws, including the Hindu Marriage Act, with a single secular framework. They view the current system as perpetuating inequality and legal pluralism.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, secular_law_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, hindu_men).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform and legally recognized framework for marriage, divorce, and family matters for individuals identifying as Hindu, replacing diverse customary laws and reducing legal uncertainty within the community.
% TRANSFER_FUNCTION: Transfers authority over marital and family disputes from diverse customary or religious bodies to state civil courts, and implicitly transfers certain rights and privileges (e.g., property, inheritance) in ways that can disproportionately benefit men within the Hindu community.
% ABSENT_VOICES: Individuals and groups advocating for a fully gender-equal or secular family law framework are often marginalized in discussions about the Hindu Marriage Act, as the debate is framed around religious identity and community autonomy. Interfaith couples, by definition, cannot participate in this law's framework.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act vanished overnight, the legal framework for millions of Hindu marriages would collapse, leading to immense legal chaos regarding marital status, divorce, inheritance, and child custody. The state would be forced to rapidly implement an alternative, likely a secular civil code, fundamentally reorganizing family law in India.
% FOUNDING_PROBLEM: The problem of diverse, often contradictory, and sometimes discriminatory customary laws governing marriage and family matters within the Hindu community, leading to legal uncertainty and social injustice.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and government reports from the mid-20th century corroborate the historical problem of legal fragmentation. Contemporary legal experts and women's rights advocates acknowledge the Act's success in achieving uniformity but contest its full resolution of gender equity issues, indicating the problem's status as 'live' but evolving.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because, despite its progressive intent, the Act's application and social context often result in outcomes less equitable for women than a fully secular or more advanced gender-neutral law. Suppression is moderate (0.55) as civil courts actively enforce the Act, and social pressures within the community can constrain alternatives. Theater ratio is low (0.15) because the Act is genuinely functional and actively applied, not merely performative. Accessibility collapse is moderate (0.45) as the Special Marriage Act offers a secular alternative, but social and identity factors make opting out difficult. Resistance is moderate (0.50) due to ongoing advocacy from women's rights and secular groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hindu community leaders and civil courts, the Act is a successful coordination mechanism that modernized and unified Hindu personal law. From the perspective of Hindu women and women's rights advocates, it is a system that, while improved, still contains elements of extraction and inequality, maintained by state enforcement and social norms. The engine's computation of per-seat types will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu community leaders and civil courts are beneficiaries and agenda-setters, gaining stability and authority. Hindu men are beneficiaries, often retaining traditional advantages. Hindu women are payers, bearing the costs of less-than-full gender equity. Interfaith couples are excluded, forced into alternative legal frameworks. Women's rights and secular law advocates act as observers, pushing for reform or replacement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_equity_interpretation_ambiguity,
    'Is the current interpretation of gender equity within the Hindu Marriage Act a faithful reading of its original intent, or has it been shaped by evolving social norms and judicial conservatism?',
    'Comprehensive legal-historical analysis of legislative debates and judicial precedents, coupled with comparative analysis of gender equity outcomes under alternative legal frameworks.',
    'If shaped by conservatism, the Act''s extractiveness from women is higher than its stated intent suggests, strengthening the case for judicial reinterpretation or legislative amendment. If faithful, the challenge shifts to the Act''s foundational premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_interpretation_ambiguity, conceptual, 'Ambiguity regarding the source of gender equity limitations in the HMA.').

omega_variable(
    secular_law_adoption_rate_and_reasons,
    'What proportion of Hindu individuals/couples choose to marry under the Special Marriage Act (secular civil code) instead of the Hindu Marriage Act, and what are their primary motivations (e.g., interfaith marriage, desire for full gender equality, avoidance of social pressure)?',
    'Empirical sociological studies and analysis of marriage registration data, disaggregated by community and stated reasons for choosing a particular Act.',
    'A high adoption rate of secular law by Hindu couples for reasons of gender equality would indicate higher effective suppression and extractiveness within the HMA, as individuals actively seek alternatives to avoid its costs. A low rate would suggest the HMA''s coordination function remains dominant for most.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_law_adoption_rate_and_reasons, empirical, 'Impact of secular alternatives on the HMA''s perceived extractiveness and suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.1).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.55).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(marr_be_t1995, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(marr_be_t2010, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.45).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(marr_su_t1995, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement(marr_su_t2010, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel', which encompasses various religious and secular legal frameworks for marriage in India. Each reading represents a distinct constraint with its own structural properties and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
