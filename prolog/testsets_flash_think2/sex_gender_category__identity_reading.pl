% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__identity_reading, []).

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
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Category membership by subjective gender identity (identity reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint describes the social and legal framework where an
 *   individual's self-identified gender determines their membership in
 *   gendered categories (e.g., 'man' or 'woman'). This 'identity reading' of
 *   gender aims to ensure the inclusion and recognition of transgender
 *   individuals. However, it generates significant conflict, particularly
 *   regarding access to sex-segregated spaces and the definition of sex-based
 *   rights, leading to a perceived loss of exclusive claims for cisgender
 *   women.
 *
 * KEY AGENTS:
 *   - trans_women: Primary beneficiary (moderate power/identity_locked)
 *   - gender_identity_advocates: Agenda-setter (organized power/mobile exit)
 *   - cis_women_seeking_sex_based_protections: Primary payer (organized power/constrained exit)
 *   - gender_critical_feminists: Payer/Excluded (organized power/constrained exit)
 *   - legal_systems_and_institutions: Agenda-setter/Enforcer (institutional power/analytical exit)
 *   - general_public: Beneficiary/Payer (moderate power/mobile exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.75).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.8).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Category membership by subjective gender identity (identity reading)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '54f1fa1a-c0f1-4130-9fcd-db7a5d76208c').
narrative_ontology:cs_kernel_codification('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', distributed).
narrative_ontology:cs_authority_grounding('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', practice).
narrative_ontology:cs_interpretation_layer_present('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c').
narrative_ontology:cs_reading_relation('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', foundational, gender_identity_is_self_determined).
narrative_ontology:cs_axiom_status(gender_identity_is_self_determined, holdable).
narrative_ontology:cs_axiom_grounding('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', gender_identity_is_self_determined, deontological).
narrative_ontology:cs_axiom('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', foundational, social_categories_should_be_inclusive_of_gender_identity).
narrative_ontology:cs_axiom_status(social_categories_should_be_inclusive_of_gender_identity, holdable).
narrative_ontology:cs_axiom_grounding('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', social_categories_should_be_inclusive_of_gender_identity, conventional).
narrative_ontology:cs_reference_frame('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', individual_gender_autonomy).
narrative_ontology:cs_drift_state('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', contemporary_social_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('54f1fa1a-c0f1-4130-9fcd-db7a5d76208c', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women_seeking_sex_based_protections).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, gender_critical_feminists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, general_public).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, general_public).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, gender_identity_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain recognition and inclusion in categories aligned with their self-identified gender, including access to spaces and services previously exclusive to cis women. They also bear social friction and backlash from those who contest this definition.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and defend the principle of self-identification for gender category membership. They work to influence policy, legal frameworks, and social norms to ensure this reading is adopted and enforced.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, global).

% Experience a loss of exclusive claim to sex-based categories and protections (e.g., in sports, changing rooms, domestic violence shelters). They often feel their concerns are dismissed or suppressed, leading to organized resistance.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women_seeking_sex_based_protections, payer,
    organized, biographical, constrained, national).

% Advocate for the primacy of biological sex in defining categories, particularly for women's rights. They are often marginalized in mainstream discourse and face social and professional penalties for their views, making their participation in policy-making constrained.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_critical_feminists, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, gender_critical_feminists, excluded).

% Are tasked with interpreting and implementing policies based on gender identity. They face pressure from advocates to adopt self-identification and from critics to maintain sex-based distinctions, leading to varying and often contested applications of the constraint.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, legal_systems_and_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from a perceived increase in inclusivity and reduced discrimination for transgender individuals. However, they also bear the costs of social friction, confusion over changing norms, and the enforcement of new social rules, often without clear guidance.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, general_public, beneficiary,
    moderate, immediate, mobile, local).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, general_public, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, trans_women).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide social and legal recognition for individuals' self-identified gender, ensuring that transgender people are affirmed in their chosen categories and reducing misgendering.
% TRANSFER_FUNCTION: Transfers social and legal access to gendered categories and spaces from cisgender individuals (who previously held exclusive claim based on sex) to transgender individuals. It also transfers the burden of enforcing these new category boundaries to institutions and the general public.
% ABSENT_VOICES: Those who believe that sex is an immutable biological reality that should be the sole basis for certain social and legal categories, particularly for women's sex-based rights, are often excluded from policy-making and public discourse, facing accusations of bigotry.
% DISAPPEARANCE_RATIONALE: If category membership by self-identification vanished overnight, social and legal categories would largely revert to sex-based definitions. This would lead to a significant re-evaluation of rights, protections, and access for transgender individuals, and a re-assertion of sex-based boundaries in many areas, causing widespread social and legal reorganization.
% FOUNDING_PROBLEM: The historical and ongoing exclusion, misgendering, and discrimination faced by transgender individuals due to rigid, sex-based social and legal categories, leading to a lack of recognition and systemic disadvantage.
% FOUNDING_PROBLEM_CORROBORATION: Transgender rights organizations, human rights bodies, and a significant portion of legal and social scholars attest that the problem of transgender exclusion and discrimination remains live. This is corroborated by ongoing reports of violence, discrimination, and mental health disparities affecting transgender people, as well as legislative efforts to restrict their rights in various jurisdictions.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the reallocation of access and definitional control over gendered categories, which is experienced as a cost by those who previously held exclusive claim. Suppression (0.80) is high due to active social and institutional pressure to enforce the self-identification principle and marginalize dissenting views. The theater ratio (0.40) is moderate; while there is genuine intent for inclusion, some enforcement activities become performative displays of adherence to new norms, rather than purely functional coordination. Resistance (0.85) is very high, indicating significant ongoing social and political contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of trans women and gender identity advocates, this constraint functions as a necessary coordination mechanism for recognition and inclusion. From the perspective of cis women seeking sex-based protections and gender-critical feminists, it operates as an extractive mechanism that erodes their rights and spaces. The engine's computation of per-seat classification will reflect this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women are primary beneficiaries (low d) as the constraint directly affirms their identity and grants access. Gender identity advocates are also beneficiaries and agenda-setters, actively shaping and benefiting from the constraint's operation. Cis women seeking sex-based protections and gender-critical feminists are targets/payers (high d) as they bear the costs of redefined categories and loss of exclusive spaces. Legal systems and institutions act as agenda-setters and enforcers, mediating the conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (inclusion and recognition) is actively pursued and contested. The classification as a Tangled Rope prevents mislabeling it as pure extraction by acknowledging its genuine coordination function for one group, while simultaneously recognizing the asymmetric extraction from another. The ongoing high resistance and suppression indicate it is far from an atrophied Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_boundary_ambiguity,
    'Does the principle of self-identification for gender category membership create new exclusions or ambiguities for other groups (e.g., cis women, intersex individuals)?',
    'Empirical studies on the lived experiences of various groups regarding access to spaces, services, and legal protections under self-ID policies, compared to sex-based or hybrid models.',
    'If new exclusions or significant ambiguities are demonstrated, the constraint''s effective extractiveness and suppression for those groups would be higher, potentially shifting its classification towards a Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_boundary_ambiguity, empirical, 'Uncertainty regarding the full scope of inclusion/exclusion effects of self-identification.').

omega_variable(
    impact_on_sex_based_rights,
    'To what extent does the inclusion of trans women in ''woman'' categories undermine or dilute sex-based protections and rights historically established for cisgender women?',
    'Legal analysis of case law and policy outcomes in jurisdictions with self-ID, specifically examining the efficacy and enforceability of sex-based protections (e.g., in sports, domestic violence shelters, data collection) for cis women.',
    'If a significant undermining is demonstrated, the extractiveness from cis women would be unequivocally higher, reinforcing the ''tangled_rope'' or even ''snare'' classification for their seat, and potentially shifting the overall constraint''s character.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(impact_on_sex_based_rights, conceptual, 'The conceptual and practical tension between gender identity rights and sex-based rights.').

omega_variable(
    social_cohesion_vs_individual_autonomy,
    'Is the current level of social conflict and resistance a temporary adjustment phase, or an irreducible tension between collective social categories and individual self-determination?',
    'Longitudinal sociological studies tracking public acceptance, policy stability, and levels of social friction over decades in jurisdictions that have adopted self-ID policies.',
    'If irreducible tension, the constraint''s high suppression and resistance metrics are structural features, not transient. If temporary, these metrics might decline over time, suggesting a path towards a more stable ''rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_cohesion_vs_individual_autonomy, empirical, 'Whether social conflict is transient or inherent to the constraint''s operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__identity_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__identity_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__identity_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__identity_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__identity_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__identity_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__identity_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__identity_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__identity_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'sex_gender_category' kernel. Each reading has a different structural definition of category membership and distinct beneficiaries/victims. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
