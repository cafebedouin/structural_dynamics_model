% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Biological Determinism Reading of Sex/Gender Category Membership
 *   domain: social/ontological/legal
 *
 * SUMMARY:
 *   This constraint story instantiates the biology_reading of the
 *   sex_gender_category kernel: the claim that sex category membership is
 *   determined exclusively by immutable reproductive biology (chromosomes,
 *   anatomy at birth). Under this reading, trans women are categorically
 *   excluded from the 'woman' category, intersex individuals are forced into
 *   a binary male/female schema, and cis women are positioned as the sole
 *   victim set requiring sex-based protection. The identity_reading
 *   (subjective gender identity as determinant) and hybrid_reading (medical
 *   gatekeeping/social transition model) are sibling constraints within the
 *   same kernel. The natural-language label 'sex/gender category' conflates
 *   these structurally distinct claims; this story isolates the
 *   biology-reading with its own epsilon, stakeholders, and classification.
 *   The constraint is claimed as tangled_rope because it coordinates genuine
 *   protection for a vulnerable population while asymmetrically extracting
 *   from trans women and intersex individuals through actively enforced,
 *   biologically immutable boundaries.
 *
 * KEY AGENTS:
 *   - state_legal_apparatus: Agenda setter (institutional/national) â sets and enforces biological classification criteria in law and administration.
 *   - cis_women: Primary beneficiary (organized/national) â receive sex-segregated protections and unchallenged category membership under the biological schema.
 *   - trans_women: Primary target (powerless/national) â excluded from the 'woman' category and its protections regardless of identity or social transition.
 *   - intersex_individuals: Secondary target (powerless/national) â forced into binary legal categories through administrative assignment or medical intervention.
 *   - medical_establishment: Analytical observer (institutional/national) â possesses evidence of biological non-binarity but operates within a legal framework that overrides it.
 *   - trans_rights_advocates: Excluded voice (organized/national) â would challenge the immutable-biology premise but are structurally absent from classification policy forums.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.72).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.8).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Biological Determinism Reading of Sex/Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social/ontological/legal").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, 'e7f0df8c-9a0d-4ef1-9447-434dad312046').
narrative_ontology:cs_kernel_codification('e7f0df8c-9a0d-4ef1-9447-434dad312046', formalized).
narrative_ontology:cs_authority_grounding('e7f0df8c-9a0d-4ef1-9447-434dad312046', lineage).
narrative_ontology:cs_interpretation_layer_present('e7f0df8c-9a0d-4ef1-9447-434dad312046').
narrative_ontology:cs_reading_relation('e7f0df8c-9a0d-4ef1-9447-434dad312046', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('e7f0df8c-9a0d-4ef1-9447-434dad312046', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('e7f0df8c-9a0d-4ef1-9447-434dad312046', foundational, sex_category_immutable_at_birth).
narrative_ontology:cs_axiom_status(sex_category_immutable_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('e7f0df8c-9a0d-4ef1-9447-434dad312046', sex_category_immutable_at_birth, empirically_contingent).
narrative_ontology:cs_axiom('e7f0df8c-9a0d-4ef1-9447-434dad312046', foundational, sex_based_protections_require_biological_boundary).
narrative_ontology:cs_axiom_status(sex_based_protections_require_biological_boundary, holdable).
narrative_ontology:cs_axiom_grounding('e7f0df8c-9a0d-4ef1-9447-434dad312046', sex_based_protections_require_biological_boundary, instrumental).
narrative_ontology:cs_reference_frame('e7f0df8c-9a0d-4ef1-9447-434dad312046', biological_binary_reference).
narrative_ontology:cs_drift_state('e7f0df8c-9a0d-4ef1-9447-434dad312046', contemporary_gender_politics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e7f0df8c-9a0d-4ef1-9447-434dad312046', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets legal criteria for sex category markers on identity documents, birth certificates, and access to sex-segregated spaces. Enforces biological criteria through administrative rules, document review, and medical inspection requirements. Bears high boundary enforcement costs.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Receive sex-segregated protections and unchallenged legal category membership under the biological schema. Their automatic inclusion in the 'woman' category is not contested by the constraint. They may experience secondary policing of gender presentation at enforcement boundaries.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women, beneficiary,
    organized, biographical, constrained, national).

% Excluded from the 'woman' legal and social category regardless of transition status, identity, or social presentation. Lose access to sex-segregated protections and legal recognition. Must navigate systems that classify them by anatomy at birth or chromosomes.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    powerless, biographical, trapped, national).

% Born with anatomical or chromosomal configurations that do not fit typical binary definitions. Forced into male or female legal categories through surgical assignment, administrative erasure, or binary administrative choice. The constraint offers no formal accommodation for non-binary biological variation.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, national).

% Possesses extensive clinical evidence that biological sex is not strictly binary, yet must operate within a legal framework that overrides medical consensus with administrative binary criteria. Their expertise is solicited only when it supports the binary.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_establishment, observer,
    institutional, generational, analytical, national).

% Advocate for gender-identity-based classification and oppose biological immutability. Structurally excluded from policy-making rooms where biological criteria are established; their testimony about exclusion costs is treated as external lobbying rather than stakeholder input.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_rights_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protecting cis women from sex-based harms (violence, exploitation, reproductive coercion) by establishing an unambiguous, biologically grounded boundary for legal recognition and sex-segregated spaces.
% TRANSFER_FUNCTION: Moves categorical membership, legal recognition, and space access away from trans women and intersex individuals toward a binary biological schema; moves enforcement and verification costs onto state administrative systems and onto individuals subject to sex classification checks.
% ABSENT_VOICES: Trans women, intersex individuals, and gender-identity clinicians are structurally absent from policy-making forums where biological criteria are set; their testimony about the costs of categorical exclusion and the empirical non-binarity of biological sex is excluded from deliberation.
% DISAPPEARANCE_RATIONALE: Sex-segregated shelters, prisons, sports categories, and legal identity systems would face immediate boundary crises and require reorganization under different ontological premises; the current distribution of recognition and exclusion would collapse.
% FOUNDING_PROBLEM: Cis women face distinct, severe sex-based harms from male-bodied individuals requiring reliable categorical boundaries for protection and legal remedy.
% FOUNDING_PROBLEM_CORROBORATION: Victim-services organizations and some feminist groups attest to ongoing sex-based harms from outside the benefiting party, but they are divided on whether biological immutability is the appropriate remedy; human rights organizations corroborate the harms while disputing the biology-reading's solution, attesting instead to distinct extraction from trans and intersex populations.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint categorically denies recognition and protection to trans women and forces intersex individuals into misaligned categories. Suppression (0.80) is higher because the arrangement depends on active enforcement: document checks, medical inspections, administrative binary assignments, and exclusion of gender-identity claims. Theater ratio (0.50) reflects that a significant share of enforcement is performative â bathroom bills and birth-certificate rules that are costly to enforce and signal political allegiance more than they prevent harms. Accessibility collapse (0.75) is high because once biological immutability is institutionalized, self-identification alternatives collapse for those subject to the jurisdiction. Resistance (0.78) is high and rising because trans communities, medical professionals, and human rights organizations actively contest the biological binary. The measurement series show extraction and enforcement intensifying over the interval as political conflict over sex classification has sharpened.
 *
 * PERSPECTIVAL GAP:
 *   From the cis_women seat, the constraint is protective coordination against documented sex-based harms. From the trans_women and intersex_individuals seats, the same structure operates as enforced ontological erasure and categorical exclusion. The state_legal_apparatus experiences it as an administrative mandate with high enforcement costs. The engine computes this divergence from structural data; the authored claim (tangled_rope) does not resolve the asymmetry but names it.
 *
 * DIRECTIONALITY LOGIC:
 *   cis_women are declared beneficiaries (low d, subsidized by the constraint's protective boundary). trans_women and intersex_individuals are declared victims (high d, targets of extraction). The state_legal_apparatus administers enforcement but is not a direct beneficiary of the extracted value; its directionality is structurally nearer the middle, though enforcement power gives it partial insulation. medical_establishment holds analytical exit options but is constrained by legal mandates, placing it near the middle. The engine will compute trans_women and intersex_individuals as high-extraction seats and cis_women as low-extraction or subsidized seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the constraint as pure coordination (it extracts from identifiable victims) while also preventing mislabeling it as pure snare (it addresses a genuine founding problem of sex-based harms against cis women). If the protective coordination function were dead and only extraction remained, the constraint would degrade toward snare or piton; if the extraction were illusory and only protection remained, it would compute as rope. The authored metrics and the temporal drift series show intensifying extraction without a corresponding increase in protective efficacy, suggesting the coordination function is being progressively overshadowed by enforcement theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biology_reading_kernel_position,
    'Does the biology reading foreclose the identity and hybrid readings logically, or do all three readings coexist as politically contested framings without logical resolution?',
    'Formal analysis of the logical structure of category-definition claims combined with cross-jurisdictional policy comparison.',
    'If the readings are merely coexisting political positions, the foreclosure relation in cs_structure overstates logical necessity and the constraint''s authority_grounding may shift from lineage to distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biology_reading_kernel_position, conceptual, 'Logical versus political relationship between kernel readings.').

omega_variable(
    intersex_variation_empirical_status,
    'Does empirical biological reality support a strict binary sex classification, or does intersex variation constitute a persistent, non-negligible exception that undermines the binary kernel?',
    'Systematic review of intersex prevalence and typology; analysis of whether legal systems accommodate or erase this variation.',
    'If intersex variation is substantial and legally erased, the constraint''s empirical premise is axiom-overridden and its extraction from intersex individuals intensifies toward snare-like operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersex_variation_empirical_status, empirical, 'Empirical status of strict biological binary.').

omega_variable(
    enforcement_cost_bearing,
    'Who bears the material and psychological costs of high boundary enforcement â the state, cis women subject to policing, trans women excluded, or intersex individuals medicalized?',
    'Administrative cost accounting of sex-verification programs; qualitative data on experiences of exclusion and medical intervention.',
    'If enforcement costs fall primarily on trans and intersex individuals while benefits diffuse to cis women, the effective extraction is higher than the base metric suggests and directionality toward trapped identities strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_bearing, empirical, 'Distribution of boundary enforcement costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__biology_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__biology_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__biology_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__biology_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__biology_reading, theater_ratio, 25, 0.5).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__biology_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__biology_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__biology_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__biology_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__biology_reading, base_extractiveness, 25, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__biology_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__biology_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__biology_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__biology_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__biology_reading, suppression_requirement, 25, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The sex_gender_category kernel decomposes into three structurally distinct constraints because the natural-language label 'sex/gender category' conflates biologically deterministic, identity-based, and hybrid gatekeeping models. Each reading has different epsilon, victim sets, and enforcement requirements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
