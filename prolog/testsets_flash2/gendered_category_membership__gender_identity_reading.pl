% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gendered Category Membership (Gender Identity Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint story describes the 'gender identity reading' of gendered
 *   category membership, where an individual's self-declared gender identity
 *   is the primary determinant of their membership in gendered categories
 *   (e.g., 'woman', 'man'). This reading has gained significant traction in
 *   legal and social discourse, leading to the inclusion of transgender
 *   individuals in categories aligned with their identity. The constraint is
 *   a Tangled Rope because it genuinely coordinates social recognition for
 *   transgender individuals (beneficiaries) but does so by imposing costs and
 *   suppressing alternative definitions from those who define categories by
 *   biological sex (victims).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.45).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.6).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gendered Category Membership (Gender Identity Reading)").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '7a18c262-707e-452e-b366-06685fd5a95e').
narrative_ontology:cs_kernel_codification('7a18c262-707e-452e-b366-06685fd5a95e', formalized).
narrative_ontology:cs_authority_grounding('7a18c262-707e-452e-b366-06685fd5a95e', practice).
narrative_ontology:cs_interpretation_layer_present('7a18c262-707e-452e-b366-06685fd5a95e').
narrative_ontology:cs_reading_relation('7a18c262-707e-452e-b366-06685fd5a95e', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a18c262-707e-452e-b366-06685fd5a95e', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('7a18c262-707e-452e-b366-06685fd5a95e', foundational, gender_is_self_declared_identity).
narrative_ontology:cs_axiom_status(gender_is_self_declared_identity, holdable).
narrative_ontology:cs_axiom_grounding('7a18c262-707e-452e-b366-06685fd5a95e', gender_is_self_declared_identity, deontological).
narrative_ontology:cs_axiom('7a18c262-707e-452e-b366-06685fd5a95e', foundational, inclusion_of_transgender_individuals_is_moral_imperative).
narrative_ontology:cs_axiom_status(inclusion_of_transgender_individuals_is_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('7a18c262-707e-452e-b366-06685fd5a95e', inclusion_of_transgender_individuals_is_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('7a18c262-707e-452e-b366-06685fd5a95e', inclusive_identity_framework).
narrative_ontology:cs_drift_state('7a18c262-707e-452e-b366-06685fd5a95e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7a18c262-707e-452e-b366-06685fd5a95e', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cisgender_women_resisting_inclusion).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, biological_sex_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, social_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain recognition and inclusion in gendered categories aligned with their self-declared identity. This provides social validation and access to spaces and resources. Exit from this framework would mean denying their identity, which is not a viable option.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and enforce the understanding of gendered categories based on self-declared identity. They shape policy, discourse, and social norms, benefiting from the expansion of this framework's acceptance.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, national).

% Are pressured to accept trans women into categories and spaces they define by biological sex, often at the cost of perceived safety, privacy, or distinct group identity. Resistance can lead to social ostracization or professional repercussions.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cisgender_women_resisting_inclusion, payer,
    moderate, biographical, constrained, local).

% Advocate for gendered categories to be defined by biological sex. They bear the cost of being marginalized in public discourse and policy-making, facing accusations of transphobia for their stance.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, biological_sex_advocates, payer,
    organized, generational, constrained, national).

% Navigate competing demands from different advocacy groups, often adopting policies that align with the gender identity reading to avoid political backlash, even if it creates tension for other groups. Their decisions shape the legal and institutional landscape.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, policy_makers, agenda_setter,
    institutional, immediate, constrained, national).

% Are tasked with implementing policies based on gender identity, often requiring changes to facilities, data collection, and communication. They bear the administrative and social costs of adapting to new norms and managing internal and external conflicts.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, social_institutions, payer,
    institutional, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for social inclusion and recognition of individuals' self-declared gender identities, aiming to reduce discrimination and promote well-being for transgender people.
% TRANSFER_FUNCTION: Transfers social recognition, access to gendered spaces/categories, and validation from traditional biological-sex-based definitions to self-declared gender identity. It transfers the burden of adaptation and potential social costs to those who resist this redefinition.
% ABSENT_VOICES: Children and adolescents whose developing identities are shaped by these categories, and who lack full agency in the discourse, are largely absent. Their long-term well-being and understanding of self are profoundly impacted but not directly represented.
% DISAPPEARANCE_RATIONALE: If this reading of gendered category membership vanished, transgender individuals would lose a crucial framework for social recognition and inclusion, leading to significant social and psychological disruption. Gendered spaces and policies would revert to biological sex definitions, causing widespread re-evaluation and conflict.
% FOUNDING_PROBLEM: The historical exclusion and discrimination faced by transgender individuals due to rigid, biologically-essentialist definitions of gender, leading to lack of recognition and social marginalization.
% FOUNDING_PROBLEM_CORROBORATION: Transgender individuals and human rights organizations universally attest to the ongoing problem of discrimination and the need for identity-based recognition. While some groups contest the *solution*, the problem of historical marginalization is widely acknowledged by independent human rights bodies and sociological research.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).
:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the social and political capital expended by those who resist this definition, as well as the redefinition of spaces and resources. Suppression (0.6) is significant, as public discourse and institutional policies actively discourage or penalize resistance to the gender identity framework. Resistance (0.7) is high, indicating ongoing, active contestation from groups advocating for biological sex-based definitions. Accessibility collapse (0.3) is moderate, as alternative frameworks for understanding gendered categories still exist and are actively promoted, though often marginalized. The claimed type is Tangled Rope because it serves a genuine coordination function (inclusion of trans individuals) but involves clear asymmetric extraction and requires active enforcement against dissenting views.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender individuals and their advocates, this constraint is a necessary Rope, providing vital coordination for social recognition and reducing harm. From the perspective of cisgender women resisting inclusion and biological sex advocates, it operates as a Snare, extracting their ability to define their own categories and imposing costs on their resistance. The engine's classification as Tangled Rope reflects this inherent tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals and gender identity advocates are beneficiaries, as the constraint directly validates their identities and expands their social inclusion. Cisgender women who resist inclusion and biological sex advocates are victims, as they bear the costs of redefinition, loss of sex-segregated spaces, and social pressure. Policy makers and social institutions are agenda-setters and payers, respectively, as they implement and manage the changes, often under pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to provide recognition and inclusion for transgender individuals) is still live. However, the 'tangled' aspect arises from the method of achieving this mandate, which involves re-ordering existing social categories and imposing costs on those who do not consent to the redefinition. This prevents mislabeling it as pure extraction, as a genuine coordination function exists, but also prevents mislabeling it as a pure Rope, as the extraction is significant and actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_category_redefinition,
    'To what extent does the redefinition of gendered categories based on identity extend to all social contexts (e.g., sports, medical data, legal definitions)?',
    'Empirical analysis of policy implementation across different sectors and jurisdictions, tracking the consistency and scope of identity-based definitions.',
    'If the redefinition is universal, the extractiveness and suppression for biological sex advocates would be higher. If it remains context-dependent, the constraint''s overall impact is more localized, potentially reducing perceived extraction in some domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_category_redefinition, empirical, 'Ambiguity regarding the universal vs. context-specific application of gender identity definitions.').

omega_variable(
    impact_on_sex_based_rights,
    'Does the gender identity reading of category membership fundamentally undermine the concept and enforcement of sex-based rights (e.g., for women''s safety, reproductive health, or data collection)?',
    'Legal analysis of case law and policy outcomes in jurisdictions where gender identity is legally prioritized, assessing the practical impact on sex-based protections.',
    'If sex-based rights are demonstrably undermined, the extractiveness for cisgender women resisting inclusion would be re-evaluated as substantially higher, potentially shifting the constraint closer to a Snare for this group. If they are found to be compatible, the extraction would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_sex_based_rights, conceptual, 'Whether gender identity definitions conflict with or are compatible with sex-based rights.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., legal penalties, institutional policies) or internalized (e.g., social pressure, fear of ostracization) for those who resist the gender identity reading?',
    'Post-exit suppression trajectory: if resistance persists after formal penalties are removed, reclassify as partially internalized. Qualitative sociological studies on self-censorship and social conformity.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would amplify the perceived coercive force of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting views.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(gend_be_t1990, gendered_category_membership__gender_identity_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(gend_be_t2000, gendered_category_membership__gender_identity_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__gender_identity_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(gend_be_t2020, gendered_category_membership__gender_identity_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(gend_be_t2024, gendered_category_membership__gender_identity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t1990, gendered_category_membership__gender_identity_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(gend_su_t2000, gendered_category_membership__gender_identity_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__gender_identity_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(gend_su_t2020, gendered_category_membership__gender_identity_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(gend_su_t2024, gendered_category_membership__gender_identity_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gendered_category_membership' kernel. Its structural properties and classification differ significantly from the 'biological_sex_reading' and 'social_role_reading' siblings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
