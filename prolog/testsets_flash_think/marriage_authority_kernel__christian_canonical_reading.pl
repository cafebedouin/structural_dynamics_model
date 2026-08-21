% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Indian Christian Marriage Act 1872 (Christian Canonical Reading)
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This constraint story analyzes the Indian Christian Marriage Act 1872 as
 *   a reading of the broader 'marriage_authority_kernel' in India. It focuses
 *   on how authority for marriage and family law for Christians derives from
 *   Christian canonical law, codified into a state Act. This reading entails
 *   restrictive divorce provisions (fault-based), reliance on church
 *   tribunals for annulment, and moderate gender inequity compared to
 *   contemporary secular standards. The constraint is claimed as a Tangled
 *   Rope, reflecting its dual function of coordinating religious identity and
 *   extracting compliance with traditional norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.65).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.75).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Indian Christian Marriage Act 1872 (Christian Canonical Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '7f9c161d-1195-4d59-adfd-11b4cf8eb0a7').
narrative_ontology:cs_kernel_codification('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', formalized).
narrative_ontology:cs_authority_grounding('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', lineage).
narrative_ontology:cs_interpretation_layer_present('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7').
narrative_ontology:cs_reading_relation('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', foundational, marriage_as_sacrament_indissoluble).
narrative_ontology:cs_axiom_status(marriage_as_sacrament_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', marriage_as_sacrament_indissoluble, theological).
narrative_ontology:cs_axiom('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', foundational, gender_roles_as_divinely_ordained).
narrative_ontology:cs_axiom_status(gender_roles_as_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', gender_roles_as_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', christian_canonical_tradition_1872).
narrative_ontology:cs_drift_state('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', contemporary_india, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f9c161d-1195-4d59-adfd-11b4cf8eb0a7', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_religious_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, traditional_christian_families).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_spouses_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_equality).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, christian_doctrine_of_marriage_indissolubility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce canonical law, benefit from maintaining traditional structures and their authority within the community. They advocate for the Act's continued application based on religious freedom.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_religious_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Subject to restrictive, fault-based divorce laws, often requiring church annulment processes, facing high legal and social barriers to marital dissolution. Their options are limited by the Act's provisions.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_spouses_seeking_divorce, payer,
    powerless, biographical, constrained, national).

% Face moderate gender inequity in matters like property rights and guardianship within the framework, with limited legal recourse for reform from within the Act, often relying on broader constitutional challenges.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_equality, payer,
    powerless, biographical, constrained, national).

% Benefit from the stability, clear moral framework, and traditional values upheld by the Act, which reinforces their social standing and community norms. They often resist calls for reform.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, traditional_christian_families, beneficiary,
    moderate, generational, identity_locked, local).

% Are legally bound to apply the Indian Christian Marriage Act 1872, but also operate under the Indian Constitution, leading to tension and occasional judicial review of the Act's provisions for constitutional compliance.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, indian_civil_courts, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for a uniform civil code and more liberal, gender-equitable family laws, but their proposals are not directly incorporated into this Act and they lack direct standing to alter its provisions, operating primarily through advocacy and constitutional litigation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, secular_legal_reformers, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__christian_canonical_reading, christian_religious_authorities).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines the legal and social framework for marriage and family for the Christian community in India, providing clarity, stability, and recognition of their religious customs within the broader legal system.
% TRANSFER_FUNCTION: Transfers authority over marital dissolution and family matters from individual autonomy to religious and legal institutions, imposing costs on those seeking to deviate from traditional norms, particularly regarding divorce and gender roles.
% ABSENT_VOICES: Secular legal reformers, human rights advocates, and Christian individuals seeking more progressive interpretations of family law are often excluded from the direct legislative or ecclesiastical processes that shape the Act's application, though they engage through constitutional challenges.
% DISAPPEARANCE_RATIONALE: If the Act vanished, the Christian community would lack a specific legal framework for marriage and divorce, leading to legal chaos and forcing reliance on either general civil law or ad-hoc religious rulings, fundamentally altering family structures and legal certainty.
% FOUNDING_PROBLEM: To provide a specific legal framework for marriage and divorce for the Christian community in British India, respecting their religious customs while integrating them into the colonial legal system, ensuring legal recognition and order.
% FOUNDING_PROBLEM_CORROBORATION: Christian religious authorities argue the problem of maintaining distinct religious identity and customs is still live. Secular legal scholars and human rights groups attest that the original problem is substantially solved, and the Act now primarily serves to maintain traditional power structures, citing constitutional challenges and calls for a uniform civil code.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Act's extractiveness (0.65) stems from the high barriers to divorce and the perpetuation of certain gender inequalities, imposing significant costs on individuals seeking more autonomy. Suppression (0.75) is high due to the combined force of state law, religious authority, and social pressure within the community. The theater ratio (0.25) is moderate; while the Act genuinely provides a legal framework, a growing portion of its maintenance involves defending traditional interpretations against modern challenges rather than purely functional coordination. The temporal measurements show a gradual increase in extractiveness and suppression as societal norms and constitutional interpretations have evolved, making the Act's provisions more burdensome over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Christian religious authorities and traditional families, the Act is a legitimate expression of religious freedom and cultural preservation, a necessary coordination mechanism. From the perspective of those seeking divorce or greater gender equality, and secular reformers, it functions as an extractive mechanism that perpetuates outdated norms under the guise of religious autonomy. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Christian religious authorities are clear beneficiaries and agenda-setters, maintaining their influence and traditional structures. Traditional Christian families also benefit from the stability and reinforcement of their values. Christian spouses seeking divorce and Christian women seeking equality are the primary targets, bearing the costs of restrictive laws and unequal provisions. Indian civil courts act as agenda-setters by applying the law, but are also constrained by constitutional principles. Secular legal reformers are excluded, their voices advocating for a uniform civil code not directly shaping this specific legal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_freedom_vs_equality,
    'Is the persistence of the Indian Christian Marriage Act 1872 primarily an exercise of religious freedom for the Christian community, or does it perpetuate gender and marital inequality under the guise of religious autonomy?',
    'Judicial review by the Supreme Court of India on constitutional grounds (e.g., equality, non-discrimination), or legislative reform towards a uniform civil code that balances religious rights with individual liberties.',
    'If primarily religious freedom, the extraction is a legitimate cost of identity coordination. If primarily inequality, it functions more as a snare, requiring intervention to protect individual rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_freedom_vs_equality, conceptual, 'Ambiguity between religious freedom and equality in the context of personal law.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the suppression experienced by Christian spouses seeking divorce or women seeking equality structural (legal barriers, church authority, social pressure) versus internalized (personal religious belief, social stigma, identity-lock)?',
    'Sociological studies on post-exit trajectories for those who use the Special Marriage Act, or surveys on community attitudes towards marital dissolution and gender roles among Indian Christians.',
    'If largely internalized, the effective suppression is higher and more resistant to legal reform alone, requiring community-level engagement and education. If primarily structural, legal reforms would have a more direct and immediate impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in religious personal law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 1872, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1872, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1872, 0.1).
narrative_ontology:measurement(marr_tr_t1920, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(marr_tr_t1960, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(marr_be_t1872, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1872, 0.45).
narrative_ontology:measurement(marr_be_t1920, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1920, 0.5).
narrative_ontology:measurement(marr_be_t1960, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(marr_be_t1990, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(marr_be_t2010, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1872, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1872, 0.65).
narrative_ontology:measurement(marr_su_t1920, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(marr_su_t1960, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(marr_su_t1990, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(marr_su_t2010, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel', which encompasses various personal laws governing marriage in India. Each reading represents a distinct legal and normative framework for different religious communities or secular citizens.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
