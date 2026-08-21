% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Gendered Category Membership (Biological Sex Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint defines gendered category membership (e.g., 'woman',
 *   'man') based strictly on immutable biological markers such as chromosomes
 *   and reproductive anatomy at birth. It is presented as a natural and
 *   necessary framework for social organization and the protection of
 *   sex-segregated spaces. However, its operation involves significant
 *   exclusion and suppression of transgender and non-binary identities,
 *   leading to high extraction from these groups. The claimed type is
 *   'tangled_rope' because it offers a coordination function for cisgender
 *   individuals who benefit from clear, binary categories, while
 *   simultaneously extracting from those who are excluded or misrecognized by
 *   this framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.85).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.9).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Gendered Category Membership (Biological Sex Reading)").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d').
narrative_ontology:cs_kernel_codification('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', formalized).
narrative_ontology:cs_authority_grounding('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', expertise).
narrative_ontology:cs_interpretation_layer_present('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d').
narrative_ontology:cs_reading_relation('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', gendered_category_membership__social_role_reading, influences).
narrative_ontology:cs_axiom('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', foundational, gender_categories_derive_from_sex).
narrative_ontology:cs_axiom_status(gender_categories_derive_from_sex, holdable).
narrative_ontology:cs_axiom_grounding('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', gender_categories_derive_from_sex, conventional).
narrative_ontology:cs_reference_frame('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', binary_biological_essentialism).
narrative_ontology:cs_drift_state('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', contemporary_identity_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4aa26849-5b4f-45bf-a9a4-a32a6ecafa0d', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cisgender_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, biological_essentialists).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, transgender_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, non_binary_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, gender_identity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proponents who define gendered categories strictly by immutable biological markers (chromosomes, reproductive anatomy at birth). They actively enforce these definitions in policy, law, and social discourse, claiming to protect the integrity and safety of sex-segregated spaces and categories.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, biological_essentialists, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the clarity and exclusivity of categories defined by biological sex, particularly in contexts like sports, changing rooms, and women's-only spaces, which they perceive as protecting their safety, privacy, and fairness. Some may also be payers if they bear social costs of enforcing these boundaries.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cisgender_women, beneficiary,
    powerful, biographical, constrained, global).

% Are excluded from categories they identify with (e.g., 'woman') and from sex-segregated spaces aligned with their gender identity. They bear the costs of misgendering, discrimination, and denial of their self-identified status, with limited options for recourse within this framework.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, transgender_women, payer,
    powerless, biographical, identity_locked, global).

% Are often rendered invisible or forced into binary categories that do not reflect their identity, as this reading primarily recognizes only two biological sexes. They bear the costs of misrecognition and lack of appropriate social or legal categories.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, non_binary_individuals, payer,
    powerless, biographical, identity_locked, global).

% Actively contest this reading, advocating for gender identity as the primary determinant of category membership. They bear the costs of ongoing social and political struggle, legal challenges, and public backlash against their positions.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_identity_advocates, payer,
    organized, generational, constrained, global).

% Are tasked with interpreting and applying biological markers, often navigating complex cases of intersex individuals or medical transitions. Their role can be to reinforce or challenge the strict binary, depending on their ethical and scientific frameworks.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, medical_professionals, observer,
    institutional, biographical, analytical, global).

% Codify and enforce definitions of sex and gender in law, impacting everything from identity documents to access to public facilities. They are a primary site of contestation and enforcement for this reading.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides clear, unambiguous, and ostensibly stable categories for human beings based on biological sex at birth, facilitating sex-segregated spaces, data collection, and social roles for those who adhere to this binary framework.
% TRANSFER_FUNCTION: Transfers social recognition, access to sex-segregated spaces, and definitional authority from transgender and non-binary individuals to cisgender individuals and proponents of biological essentialism, by excluding the former from categories they identify with.
% ABSENT_VOICES: Intersex individuals, whose biological realities challenge the strict binary definition of sex, are often marginalized or excluded from the foundational premises of this debate. Those advocating for more fluid or self-determined understandings of gender are actively suppressed.
% DISAPPEARANCE_RATIONALE: If category membership grounded in immutable biological markers vanished overnight, many sex-segregated spaces, legal definitions, and social norms would lose their foundational justification, leading to a significant reorganization of identity recognition, legal frameworks, and social structures.
% FOUNDING_PROBLEM: The need for clear, stable, and universally recognizable categories for human beings to organize society, particularly for reproduction, social roles, and the protection of specific groups (e.g., women's sports, women's shelters, data collection).
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., some feminist groups, conservative organizations) assert the problem is live, citing ongoing concerns about fairness in sports, safety in single-sex spaces, or the erosion of clear social categories. Opponents (e.g., trans rights organizations, some academic bodies) argue the founding problem is either solved, reframed, or exaggerated to justify exclusion; independent sociological and psychological research often challenges the necessity of strict biological essentialism for these functions.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the profound impact of exclusion from self-identified categories and spaces, leading to identity denial and discrimination. Suppression (0.90) is severe due to active social, legal, and political efforts to enforce biological definitions and delegitimize alternative gender identities. Resistance (0.80) is high, driven by organized advocacy from transgender and allied communities. Theater ratio is low (0.10) because the constraint's function is direct and actively maintained exclusion, not performative maintenance of an atrophied function. Accessibility collapse is moderate (0.60) because while alternative readings exist, this reading actively seeks to collapse their social and legal viability.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading perceive it as a natural, protective 'mountain' or a beneficial 'rope' that coordinates social life. Those targeted by its exclusions experience it as a 'snare' or 'tangled_rope' that extracts identity, dignity, and access. The engine's classification will reflect this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Biological essentialists and many cisgender women are beneficiaries, gaining clarity, perceived safety, and exclusive access to categories/spaces. Transgender women and non-binary individuals are clear targets, experiencing exclusion and misrecognition. Gender identity advocates bear the costs of contesting this framework. Legal systems and medical professionals act as agenda-setters, codifying and interpreting these definitions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_ambiguity,
    'Is this constraint a fundamental, natural definition of gendered categories, or one socially constructed reading of a contested kernel?',
    'Analysis of cross-cultural and historical variations in gender definitions, and the degree to which biological markers are interpreted and applied rather than simply observed.',
    'If a constructed reading, its claimed naturalness (mountain-like qualities) is a cover for active enforcement and extraction, reclassifying it as a tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_ambiguity, conceptual, 'Ambiguity between natural law and social construction in gendered category definitions.').

omega_variable(
    sibling_reading_impact_gender_identity,
    'What would be the structural impact if the ''gender_identity_reading'' of category membership were adopted?',
    'Observing jurisdictions where gender identity is legally recognized as primary for category membership, and analyzing changes in social norms, legal frameworks, and access to spaces.',
    'The ''gender_identity_reading'' would lead to the inclusion of transgender women in ''woman'' categories and spaces, fundamentally altering the beneficiary/victim structure and reducing extraction from transgender individuals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_gender_identity, empirical, 'Impact of adopting gender identity as the basis for category membership.').

omega_variable(
    sibling_reading_impact_social_role,
    'What would be the structural impact if the ''social_role_reading'' of category membership were adopted?',
    'Sociological analysis of communities that emphasize social performance and recognition over biological markers for gendered roles, and how this affects social cohesion and individual well-being.',
    'The ''social_role_reading'' would shift the basis of category membership from immutable biology to lived experience and social performance, potentially creating more fluid categories but also new forms of exclusion based on conformity to roles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_social_role, empirical, 'Impact of adopting social role as the basis for category membership.').

omega_variable(
    disagreement_locus,
    'Where is the fundamental disagreement located: in the definition of ''sex'', ''gender'', or their relationship to social categories?',
    'Conceptual analysis of philosophical arguments, legal precedents, and scientific consensus regarding the definitions and interrelations of sex and gender.',
    'Clarifying the locus of disagreement would inform targeted policy interventions and legal reforms, potentially resolving some aspects of the conflict or highlighting irreducible conceptual differences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_locus, conceptual, 'Locus of disagreement in the sex/gender debate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gend_tr_t6, gendered_category_membership__biological_sex_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__biological_sex_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(gend_tr_t18, gendered_category_membership__biological_sex_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(gend_tr_t24, gendered_category_membership__biological_sex_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(gend_tr_t30, gendered_category_membership__biological_sex_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(gend_be_t6, gendered_category_membership__biological_sex_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__biological_sex_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(gend_be_t18, gendered_category_membership__biological_sex_reading, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(gend_be_t24, gendered_category_membership__biological_sex_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(gend_be_t30, gendered_category_membership__biological_sex_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(gend_su_t6, gendered_category_membership__biological_sex_reading, suppression_requirement, 6, 0.83).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__biological_sex_reading, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(gend_su_t18, gendered_category_membership__biological_sex_reading, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(gend_su_t24, gendered_category_membership__biological_sex_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(gend_su_t30, gendered_category_membership__biological_sex_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gendered_category_membership' kernel, alongside 'gender_identity_reading' and 'social_role_reading'. Each reading instantiates a distinct constraint with different structural properties and impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
