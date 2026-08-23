% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Biological Sex Category Membership Rule
 *   domain: social ontology / political philosophy / bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the biological_sex_reading of the
 *   gendered_category_membership kernel. It treats category membership
 *   (woman/man) as grounded in immutable biological markers assigned at birth
 *   (chromosomes, reproductive anatomy). Under this reading, trans women are
 *   structurally excluded from the 'woman' category, intersex variation is
 *   administratively erased, and sex-segregated spaces are policed by
 *   biological criteria. The constraint is presented as natural law (biology)
 *   but functions as an actively enforced social boundary with high
 *   extractive and suppressive costs for identity-divergent populations.
 *
 * KEY AGENTS:
 *   - state_bureaucracy (institutional/constrained): Administers legal sex categories and enforces binary boundaries via legislation and documentation.
 *   - medical_establishments (institutional/constrained): Assign biological sex at birth and control gatekeeping through anatomical and chromosomal criteria.
 *   - cis_women (organized/constrained): Claimed beneficiaries of sex-segregated protections; their category integrity is the stated justification.
 *   - trans_women (powerless/trapped): Primary targets of extraction and identity suppression; denied category membership and excluded from protected spaces.
 *   - intersex_people (powerless/trapped): Secondary targets; binary marker rule forces their bodies into invisible administrative categories.
 *   - gender_identity_advocates (organized/constrained): Absent voices; excluded from definitional authority.
 *   - critical_bioethicists (analytical/analytical): Observers mapping the constraint's empirical failures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.82).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.88).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Biological Sex Category Membership Rule").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social ontology / political philosophy / bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '91fe1532-b92b-4504-a048-7ba48e7f26e5').
narrative_ontology:cs_kernel_codification('91fe1532-b92b-4504-a048-7ba48e7f26e5', fixed_text).
narrative_ontology:cs_authority_grounding('91fe1532-b92b-4504-a048-7ba48e7f26e5', extraction).
narrative_ontology:cs_interpretation_layer_present('91fe1532-b92b-4504-a048-7ba48e7f26e5').
narrative_ontology:cs_reading_relation('91fe1532-b92b-4504-a048-7ba48e7f26e5', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('91fe1532-b92b-4504-a048-7ba48e7f26e5', gendered_category_membership__social_role_reading, forecloses).
narrative_ontology:cs_axiom('91fe1532-b92b-4504-a048-7ba48e7f26e5', foundational, biological_markers_exhaust_category_membership).
narrative_ontology:cs_axiom_status(biological_markers_exhaust_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('91fe1532-b92b-4504-a048-7ba48e7f26e5', biological_markers_exhaust_category_membership, empirically_contingent).
narrative_ontology:cs_axiom('91fe1532-b92b-4504-a048-7ba48e7f26e5', foundational, binary_sex_boundary_is_immutable).
narrative_ontology:cs_axiom_status(binary_sex_boundary_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('91fe1532-b92b-4504-a048-7ba48e7f26e5', binary_sex_boundary_is_immutable, empirically_contingent).
narrative_ontology:cs_reference_frame('91fe1532-b92b-4504-a048-7ba48e7f26e5', binary_biological_essentialism).
narrative_ontology:cs_drift_state('91fe1532-b92b-4504-a048-7ba48e7f26e5', contemporary_gender_identity_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('91fe1532-b92b-4504-a048-7ba48e7f26e5', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines legal sex categories via birth certificates, identity documents, and legislation governing bathroom access, sports participation, and military service. Enforces binary boundary through administrative procedure and criminal or civil penalties for documentation that contradicts biological markers assigned at birth.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, state_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).

% Assigns sex at birth based on external reproductive anatomy and chromosomal assessment; controls access to gender-affirming care by requiring acknowledgment of biological markers; performs non-consensual normalizing surgeries on intersex infants to preserve binary categories.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, medical_establishments, agenda_setter,
    institutional, generational, constrained, national).

% Receive access to sex-segregated shelters, sports divisions, and legal protections justified as preserving their safety and categorical integrity. Their category membership is uncontested under the biological marker rule, though they remain constrained by its reductionist logic.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women, beneficiary,
    organized, biographical, constrained, national).

% Denied membership in the 'woman' category regardless of identity, social transition, or medical treatment. Excluded from sex-segregated protections and spaces; identity claims are structurally invalidated because chromosomes and birth anatomy are declared immutable grounds of membership.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, trapped, national).

% Existence is administratively erased by the binary biological marker rule; subjected to non-consensual surgical normalization or forced into male/female categories that misrepresent their bodies. Their variation is treated as a disorder to be corrected rather than a category challenge.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_people, payer,
    powerless, biographical, trapped, national).

% Advocate for self-determination and identity-based category access. Structurally excluded from legislative and medical policy tables where biological definitions are codified; their testimony is ruled irrelevant by the marker-grounded framework itself.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_identity_advocates, excluded,
    organized, biographical, constrained, national).

% Analyze the empirical mismatch between biological variation and binary legal categories. Neither collect benefits nor bear the constraint's costs; produce epistemic friction by documenting intersex prevalence and the social construction of binary medical taxonomy.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, critical_bioethicists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes sex-segregated spaces, medical categories, and legal identities around immutable biological markers assigned at birth, ostensibly to protect vulnerable populations and ensure single-sex provision integrity.
% TRANSFER_FUNCTION: Moves the authority to define category membership from self-identification and social performance to biological assessment at birth, transferring access to sex-segregated rights and spaces away from trans women and toward cis women.
% ABSENT_VOICES: Trans women, intersex people, and gender identity advocates are structurally excluded from the definitional table; their testimony about lived identity and bodily diversity is ruled inadmissible by the biological marker rule itself.
% DISAPPEARANCE_RATIONALE: If biological markers ceased to ground category membership, legal sex registration, medical gatekeeping, and access to sex-segregated spaces would shift to identity-based or performative criteria, rearranging who is admitted to shelters, sports, prisons, and medical protocols.
% FOUNDING_PROBLEM: The need to organize reproduction, medical care, and sex-segregated protections in a world where biological differences correlate with social vulnerability and functional requirements.
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and feminist legal scholars outside the immediate beneficiary class attest that biological categorization originally solved specific reproductive and medical coordination problems; they contest whether those problems justify the current breadth of category enforcement, citing historical contingency of the binary medical model.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint extracts identity recognition, legal protection, and social participation from trans women by fiat of birth status. Suppression is very high (0.88) because the boundary cannot be maintained without active legal, medical, and social enforcement against identity claims. Theater is moderate (0.45): biological markers genuinely coordinate some medical contexts, but a large share of enforcement activity (birth certificate policing, bathroom legislation, sports genital inquiries) is performative boundary maintenance that exceeds any coordination function. Accessibility collapse is high (0.75) because once the biological rule is institutionalized, identity-based alternatives are legally and medically foreclosed. Resistance is high (0.80) because trans communities, allied medical professionals, and human rights bodies actively contest the boundary.
 *
 * PERSPECTIVAL GAP:
 *   The state_bureaucracy and medical_establishment seats experience the constraint as necessary administrative coordination (protecting vulnerable populations, ensuring medical clarity). The trans_women and intersex_people seats experience the same structure as enforced erasure and identity suppression. The cis_women seat experiences it as protective boundary maintenance. The engine computes this divergence from the structural asymmetry in power, exit, and cost-bearing.
 *
 * DIRECTIONALITY LOGIC:
 *   State and medical agenda-setters sit near the beneficiary end (low d): they control the rule and accrue administrative authority from it. Cis_women beneficiaries sit at low-to-mid d: they receive protective coordination but are also constrained by the biological reductionism of the category. Trans_women and intersex_people payers sit at high d (near full target): they bear the extraction directly through exclusion and forced misclassification. Their exit is structurally blocked by the immutability premise of the markers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring both coordination and extraction to be present for tangled_rope classification. Here, a genuine coordination residue exists (reproductive medicine, certain sex-specific health screenings), preventing a pure snare classification. However, the constraint's primary modern operation is the suppression of identity-based category claims, which is extractive. The asymmetric cost distribution (cis women nominally protected, trans women fully excluded) satisfies the tangled_rope gate rather than rope or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint a genuine biological coordination mechanism, or a social-political boundary weaponized against trans identity?',
    'Comparative analysis of jurisdictions with identity-based category membership: if sex-segregated protections and medical coordination persist without biological-marker enforcement, the constraint is social-political rather than natural.',
    'If social-political, the biological-marker claim is a false summit mountain or a snare; if genuine biological coordination, it is a tangled rope with a strong natural component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the biological marker rule is natural law or constructed extraction.').

omega_variable(
    biological_immutability_ambiguity,
    'Do chromosomal and anatomical markers constitute an immutable natural kind that grounds social categories, or are they a medical-scientific frame elevated to ontological status?',
    'Philosophy of biology consensus on sex as a multilevel composite variable vs. binary essentialism; empirical tracking of intersex prevalence and chromosomal variation.',
    'If sex is a composite variable, the binary boundary is a constructed simplification (high extraction); if a natural kind, the constraint approaches a mountain for its referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_immutability_ambiguity, empirical, 'Whether biological sex is a natural kind or constructed category.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of trans identity by this constraint primarily structural (legal barriers, medical gatekeeping) or internalized (identity denial, self-exclusion)?',
    'Post-policy-change studies measuring mental health and social participation of trans populations after removal of biological-marker requirements.',
    'If internalized, effective extraction exceeds the structural measure because the constraint persists psychologically after legal removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of trans identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gend_tr_t5, gendered_category_membership__biological_sex_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__biological_sex_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(gend_tr_t15, gendered_category_membership__biological_sex_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__biological_sex_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(gend_tr_t25, gendered_category_membership__biological_sex_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gend_be_t5, gendered_category_membership__biological_sex_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__biological_sex_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(gend_be_t15, gendered_category_membership__biological_sex_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__biological_sex_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(gend_be_t25, gendered_category_membership__biological_sex_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gend_su_t5, gendered_category_membership__biological_sex_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__biological_sex_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(gend_su_t15, gendered_category_membership__biological_sex_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__biological_sex_reading, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(gend_su_t25, gendered_category_membership__biological_sex_reading, suppression_requirement, 25, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
