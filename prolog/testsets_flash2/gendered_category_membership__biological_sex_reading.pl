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
    domain_priors:emerges_naturally/1,
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
 *   'man') based exclusively on immutable biological markers such as
 *   chromosomes and reproductive anatomy at birth. It asserts that these
 *   categories are natural and fixed, and that social recognition must align
 *   with this biological reality. This reading inherently excludes
 *   transgender women from the 'woman' category and non-binary individuals
 *   from binary categories, leading to significant identity suppression.
 *   While claimed as a 'mountain' due to its assertion of naturalness, its
 *   active enforcement and high suppression of alternative identities suggest
 *   a more constructed, extractive dynamic. The metrics reflect the impact of
 *   this reading on those it excludes.
 *
 * KEY AGENTS:
 *   - cisgender_women: Primary beneficiary (organized/mobile) — category preservation
 *   - biological_sex_advocates: Agenda setter (powerful/analytical) — enforces the definition
 *   - transgender_women: Primary payer (powerless/identity_locked) — excluded from categories
 *   - non_binary_individuals: Payer (powerless/identity_locked) — rendered invisible/misgendered
 *   - gender_identity_advocates: Excluded (organized/constrained) — argue for alternative definitions
 *   - scientific_community: Observer (institutional/analytical) — researches sex/gender complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.7).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.85).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, mountain).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Gendered Category Membership (Biological Sex Reading)").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).
domain_priors:emerges_naturally(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '711185ab-ab44-40f6-bb00-dea05de36113').
narrative_ontology:cs_kernel_codification('711185ab-ab44-40f6-bb00-dea05de36113', implicit).
narrative_ontology:cs_authority_grounding('711185ab-ab44-40f6-bb00-dea05de36113', practice).
narrative_ontology:cs_interpretation_layer_present('711185ab-ab44-40f6-bb00-dea05de36113').
narrative_ontology:cs_reading_relation('711185ab-ab44-40f6-bb00-dea05de36113', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('711185ab-ab44-40f6-bb00-dea05de36113', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('711185ab-ab44-40f6-bb00-dea05de36113', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('711185ab-ab44-40f6-bb00-dea05de36113', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('711185ab-ab44-40f6-bb00-dea05de36113', foundational, gendered_categories_derive_from_sex).
narrative_ontology:cs_axiom_status(gendered_categories_derive_from_sex, holdable).
narrative_ontology:cs_axiom_grounding('711185ab-ab44-40f6-bb00-dea05de36113', gendered_categories_derive_from_sex, conventional).
narrative_ontology:cs_reference_frame('711185ab-ab44-40f6-bb00-dea05de36113', traditional_biological_binary).
narrative_ontology:cs_drift_state('711185ab-ab44-40f6-bb00-dea05de36113', contemporary_gender_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('711185ab-ab44-40f6-bb00-dea05de36113', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cisgender_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, biological_sex_advocates).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, transgender_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, non_binary_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the preservation of sex-segregated spaces and categories, which they perceive as protecting their rights, safety, and opportunities. They are positioned as the primary referent for the 'woman' category under this reading.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cisgender_women, beneficiary,
    organized, generational, mobile, global).

% Actively promote and enforce the definition of gendered categories based solely on biological sex markers. They frame this as upholding scientific truth and protecting the rights of cisgender women. They set the terms of the debate and influence policy.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, biological_sex_advocates, agenda_setter,
    powerful, generational, analytical, global).

% Are excluded from categories and spaces aligned with their gender identity, leading to social marginalization, discrimination, and denial of recognition. Their identity is structurally suppressed by this reading.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, transgender_women, payer,
    powerless, biographical, identity_locked, global).

% Are rendered invisible or misgendered by the strict binary enforcement of this reading, experiencing a denial of their self-identified gender and exclusion from relevant categories.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, non_binary_individuals, payer,
    powerless, biographical, identity_locked, global).

% Are actively campaigning for the recognition of gender identity as the basis for category membership. They are excluded from the definitional power of this reading and their arguments are often dismissed as ideological rather than scientific.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, global).

% Observes and researches the biological and social aspects of sex and gender. While some members support the biological sex reading, others emphasize the complexity and spectrum of biological markers, or the social construction of gender, leading to internal disagreements.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, scientific_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous, and historically consistent framework for sex-segregated categories and spaces, ensuring that membership is based on observable, immutable biological characteristics.
% TRANSFER_FUNCTION: Transfers definitional power and access to sex-segregated spaces to individuals assigned female at birth, while denying these to transgender women and non-binary individuals.
% ABSENT_VOICES: Transgender individuals and their advocates, whose lived experience and self-identification are dismissed as irrelevant to category membership. They would argue for inclusion based on gender identity.
% DISAPPEARANCE_RATIONALE: If this reading of category membership vanished, the concept of 'woman' as solely defined by biological sex would dissolve, leading to a re-evaluation of sex-segregated spaces and a shift towards gender identity or social role as primary definers. This would fundamentally alter social structures and legal frameworks.
% FOUNDING_PROBLEM: To establish clear, objective, and universally applicable criteria for distinguishing between sexes, particularly for reproductive roles and the organization of social life around a binary understanding of human biology.
% FOUNDING_PROBLEM_CORROBORATION: Biological sex advocates attest that the problem of maintaining clear sex distinctions is live and fundamental to human biology and social organization. Critics argue that while biological sex is real, its exclusive use for social category membership is a social construct, not a biological imperative, and that the 'problem' is now one of exclusion, not clarity.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, ExtMetricName, E),
    domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gendered_category_membership__biological_sex_reading),
    narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) and suppression (0.85) reflect the profound impact on transgender and non-binary individuals, who are denied fundamental recognition and access to categories aligned with their identity. The 'mountain' claim is based on the assertion of biological immutability, leading to high accessibility collapse (0.9) for alternative definitions within this framework. However, the high resistance (0.75) from excluded groups and the need for active enforcement (true) indicate it is not a universally accepted natural law. The low theater ratio (0.1) suggests that the constraint is genuinely functional in its stated goal of maintaining biological sex distinctions, even if that function is extractive for others. The increasing extractiveness and suppression over time reflect the hardening of positions in the ongoing 'culture war' around gender identity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of biological sex advocates, this constraint is a natural, immutable truth (a mountain) that protects the integrity of sex-based categories. From the perspective of transgender women, it is a deeply extractive and suppressive snare that denies their identity and rights. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Cisgender women and biological sex advocates are beneficiaries, as the constraint preserves categories they identify with or actively defend. Transgender women and non-binary individuals are clear targets/victims, experiencing direct exclusion and identity suppression. Their 'identity_locked' exit option means the cost of non-compliance (denial of self) is extremely high. Gender identity advocates are excluded, as their framing is actively suppressed by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (to define categories by biological sex) is considered live and fundamental by its proponents. The contest is over the validity of this mandate itself, not its obsolescence. The classification prevents mislabeling by highlighting the active enforcement and high suppression required to maintain a 'natural' category in the face of significant resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_category,
    'Is the exclusive grounding of gendered categories in immutable biological markers a natural law (mountain) or a social construct (snare/tangled_rope) that benefits identifiable groups?',
    'Cross-cultural anthropological studies of gender systems, historical analysis of sex/gender definitions, and philosophical analysis of ''natural kinds'' vs. ''social kinds''.',
    'If primarily a social construct, the constraint would reclassify from mountain to a more extractive type (snare or tangled_rope), highlighting the active enforcement and beneficiary structure. If genuinely a natural law, the mountain classification would be affirmed, but the high extractiveness would still be a feature of its operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_category, conceptual, 'Ambiguity between natural biological fact and social interpretation/enforcement of categories.').

omega_variable(
    identity_suppression_mechanism,
    'Is the suppression experienced by transgender and non-binary individuals primarily structural (legal/institutional barriers) or internalized (psychological impact of societal non-recognition)?',
    'Longitudinal studies of individuals in contexts with varying legal recognition and social acceptance of gender identity; post-recognition mental health outcomes.',
    'If largely internalized, the effective suppression is even higher than the structural measure suggests, as the harm persists even if external barriers are removed. If primarily structural, legal and policy changes would have a more direct and immediate ameliorative effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for gender identity.').

omega_variable(
    scientific_consensus_on_sex_complexity,
    'To what extent does the scientific community''s understanding of biological sex support a strict binary definition versus a more complex, spectrum-based view?',
    'Systematic review of current biological, genetic, and endocrinological research on sex determination and differentiation, including intersex conditions.',
    'If scientific consensus shifts towards a more complex view, the ''emerges_naturally'' claim of this reading would be weakened, potentially reclassifying it from mountain to a more constructed type. This would challenge the authority grounding of biological sex advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_consensus_on_sex_complexity, empirical, 'The degree to which biological science supports a strict binary definition of sex.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t1950, gendered_category_membership__biological_sex_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(gend_tr_t1970, gendered_category_membership__biological_sex_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(gend_tr_t1990, gendered_category_membership__biological_sex_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__biological_sex_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(gend_tr_t2024, gendered_category_membership__biological_sex_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gend_be_t1950, gendered_category_membership__biological_sex_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(gend_be_t1970, gendered_category_membership__biological_sex_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(gend_be_t1990, gendered_category_membership__biological_sex_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__biological_sex_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(gend_be_t2024, gendered_category_membership__biological_sex_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t1950, gendered_category_membership__biological_sex_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(gend_su_t1970, gendered_category_membership__biological_sex_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(gend_su_t1990, gendered_category_membership__biological_sex_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__biological_sex_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(gend_su_t2024, gendered_category_membership__biological_sex_reading, suppression_requirement, 2024, 0.85).


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
