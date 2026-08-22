% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Categorical Balancing: Protected/Unprotected Speech Categories via Judicial Balancing
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint story models the categorical balancing reading of the
 *   First Amendment — the dominant doctrinal framework since the mid-20th
 *   century. Under this reading, the First Amendment's protection is not
 *   absolute but structured through judicially created categories of
 *   'unprotected' or 'less protected' speech (obscenity, incitement, true
 *   threats, fighting words, defamation, commercial speech, child
 *   pornography). Each category emerges from case-by-case balancing of the
 *   speech's 'value' against its 'harm' or the government's interest in
 *   regulation. The institutional judiciary is the primary beneficiary,
 *   maintaining interpretive supremacy; government regulation interests
 *   benefit derivatively. Legal predictability pays through doctrinal
 *   instability and chilling effects; minority speakers in marginalized
 *   categories pay through disproportionate exclusion. The constraint is a
 *   tangled rope: it coordinates a genuine governance problem (how to
 *   regulate some speech without licensing all speech) but extracts
 *   asymmetrically — the coordination function is real but the categorical
 *   boundaries migrate toward government power over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.35).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.42).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing: Protected/Unprotected Speech Categories via Judicial Balancing").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '5d0ad2b9-6884-49c6-affb-825d349d73e6').
narrative_ontology:cs_kernel_codification('5d0ad2b9-6884-49c6-affb-825d349d73e6', fixed_text).
narrative_ontology:cs_authority_grounding('5d0ad2b9-6884-49c6-affb-825d349d73e6', lineage).
narrative_ontology:cs_interpretation_layer_present('5d0ad2b9-6884-49c6-affb-825d349d73e6').
narrative_ontology:cs_reading_relation('5d0ad2b9-6884-49c6-affb-825d349d73e6', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d0ad2b9-6884-49c6-affb-825d349d73e6', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('5d0ad2b9-6884-49c6-affb-825d349d73e6', foundational, speech_protection_is_category_based_not_absolute).
narrative_ontology:cs_axiom_status(speech_protection_is_category_based_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('5d0ad2b9-6884-49c6-affb-825d349d73e6', speech_protection_is_category_based_not_absolute, conventional).
narrative_ontology:cs_axiom('5d0ad2b9-6884-49c6-affb-825d349d73e6', foundational, judicial_balancing_of_value_vs_harm_determines_category_boundaries).
narrative_ontology:cs_axiom_status(judicial_balancing_of_value_vs_harm_determines_category_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('5d0ad2b9-6884-49c6-affb-825d349d73e6', judicial_balancing_of_value_vs_harm_determines_category_boundaries, conventional).
narrative_ontology:cs_reference_frame('5d0ad2b9-6884-49c6-affb-825d349d73e6', founding_era_absolute_text_with_pragmatic_necessity).
narrative_ontology:cs_drift_state('5d0ad2b9-6884-49c6-affb-825d349d73e6', contemporary_doctrinal_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5d0ad2b9-6884-49c6-affb-825d349d73e6', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, government_regulation_interests).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers_in_marginalized_categories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, mainstream_media_and_platforms).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, mainstream_media_and_platforms).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, judicial_supremacy_in_speech_interpretation).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, government_interest_balancing_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal and state courts define and redefine the categories of unprotected speech (obscenity, incitement, true threats, fighting words, commercial speech, etc.) through case-by-case balancing. They maintain interpretive control over the First Amendment's scope, collecting institutional authority and legitimacy from their role as the final arbiters of speech protection boundaries.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, beneficiary).

% Legislatures and regulatory agencies gain expanded authority to regulate speech that falls into judicially created unprotected categories. Each new categorical exclusion or narrowed protection expands the regulatory toolkit. They do not administer the doctrine but benefit from its permissive effect on government power.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, government_regulation_interests, beneficiary,
    organized, biographical, mobile, national).

% The case-by-case balancing approach produces inherently unpredictable boundaries — speakers, platforms, and lower courts cannot reliably predict whether novel speech falls inside or outside protection until appellate courts rule. This uncertainty chills speech broadly and creates compliance costs that scale with doctrinal instability. The 'category' itself is a moving target.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    moderate, generational, constrained, national).

% Groups whose speech is disproportionately categorized as 'unprotected' or 'low value' — including racial minorities, LGBTQ+ speakers, political dissidents, and anti-establishment voices — bear concentrated costs. The balancing test's 'value' prong historically tracks majority sensibilities; their speech is more likely to be deemed low-value and high-harm. Exit is identity-locked: their speech is constitutive of their political identity and community survival.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers_in_marginalized_categories, payer,
    powerless, biographical, identity_locked, national).

% Large institutional speakers benefit from doctrinal stability in core political speech categories (high-value, high-protection) while bearing compliance costs at the margins. Their legal resources let them navigate uncertainty better than smaller speakers, creating a stratified protection regime.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, mainstream_media_and_platforms, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, mainstream_media_and_platforms, payer).

% Produce the doctrinal commentary, amicus briefs, and litigation strategies that shape balancing outcomes. They do not directly collect rents or bear extraction but structurally influence the constraint's evolution through elite legal discourse.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, constitutional_scholars_and_civil_liberties_orgs, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable framework for courts to resolve speech-government conflicts without either absolute paralysis (no regulation ever) or absolute deference (all regulation valid). The categorical structure gives lower courts and regulators predictable(ish) buckets; the balancing test allows context-sensitive line-drawing.
% TRANSFER_FUNCTION: Moves interpretive authority and regulatory permission from speakers to courts and government. Each categorical exclusion or narrowed protection transfers the power to suppress that speech from the speaker (who loses protection) to the state (which gains regulatory authority). The judiciary collects institutional legitimacy as the broker.
% ABSENT_VOICES: Speakers whose expression has not yet been litigated into a category — future speakers, emerging art forms, new political movements, and marginalized communities without litigation capacity. They are excluded from the category-creation process, which only runs through adversarial litigation brought by parties with standing and resources.
% DISAPPEARANCE_RATIONALE: If categorical balancing vanished overnight, either absolutist protection would expand (absolutist_reading gains ground) or harm-based restriction would expand (harm_limited_reading gains ground). The entire edifice of unprotected categories (obscenity, incitement, true threats, commercial speech doctrine, etc.) would collapse or transform. Lower courts, legislatures, and platforms would lose their primary doctrinal roadmap.
% FOUNDING_PROBLEM: The absolutist text ('Congress shall make no law') collides with the practical necessity of regulating some speech (perjury, threats, obscenity, wartime sedition). Early 20th century Court needed a framework that preserved core protection while admitting necessary exceptions without textual amendment.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Rabin, Kalven, Schauer) document the pragmatic origin: Schenck, Abrams, and Gitlow era Courts invented 'clear and present danger' and categorical exclusions to avoid either striking down all speech regulation or abandoning the First Amendment entirely. The beneficiaries (judiciary, government) cite ongoing necessity; civil liberties scholars (e.g., Stone, Volokh, Chemerinsky) attest the founding problem is substantially solved for core political speech but the categorical apparatus persists as a regulatory engine.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).
:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate: the constraint transfers regulatory authority from speakers to state via categorical exclusions, but core political speech remains highly protected. The accumulation over time (0.15→0.35) reflects category proliferation (commercial speech, indecent speech, student speech, etc.) and narrowing of existing categories. Suppression (0.42) is moderate: the constraint suppresses alternative frameworks (absolutist, harm-limited) through doctrinal entrenchment and stare decisis, but does not eliminate them from elite discourse. Theater ratio (0.28) reflects that the balancing test's 'case-by-case' rhetoric increasingly masks categorical rulemaking — courts announce balancing but apply de facto categorical rules. Accessibility collapse (0.55) is moderate: speakers can often predict core protection but face genuine uncertainty at category boundaries. Resistance (0.38) is moderate: academic and litigation resistance persists but has not displaced the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, this is a rope: genuine coordination solving the text-practice gap with minimal extraction. From minority speakers' seat, it is a snare: the categories are cover for viewpoint-discriminatory suppression. From legal predictability's seat, it is a piton: the categorical structure persists through inertia despite failing its coordinating function (predictability). The engine computes these per-seat divergences from the structural data — the claimed_type (tangled_rope) represents the authoring seat's structural assessment that BOTH coordination and extraction are genuinely present and neither reduces to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional judiciary sits at the beneficiary end (d ~ 0.15): it controls the doctrine, collects legitimacy, and faces no exit pressure. Government regulation interests are secondary beneficiaries (d ~ 0.25): they gain regulatory space but do not control the categories. Legal predictability is a structural payer (d ~ 0.7): the doctrine's instability is a feature, not a bug, from the judiciary's perspective — it preserves interpretive discretion. Minority speakers are deep targets (d ~ 0.9): identity-locked exit, concentrated costs, historical pattern of disfavored categorization. Mainstream media/platforms sit near symmetric (d ~ 0.5): they benefit from core protection but pay compliance costs at margins. Constitutional scholars are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling absolute text with necessary regulation) was substantially solved for core political speech by the 1960s (Brandenburg, New York Times v. Sullivan). Yet the categorical apparatus expanded rather than contracted — new categories (commercial speech, student speech, indecent broadcast, etc.) proliferated. The mandate has atrophied into a regulatory engine: the coordination function (predictable categories for necessary regulation) has been overtaken by the extraction function (expanding government regulatory authority through judicial category creation). This is not pure mandatrophy (which would be piton) because active enforcement and doctrinal innovation continue — it is a tangled rope where the coordination rationale has become a cover story for ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_boundary_migration,
    'Do the categorical boundaries (obscenity, incitement, true threats, etc.) structurally migrate toward government regulatory power over time, or do they stabilize around a genuine coordination equilibrium?',
    'Longitudinal doctrinal analysis: track category definitions and protected speech sets across 50+ years. If boundaries consistently narrow protection and expand regulation regardless of political valence, migration is structural. If boundaries oscillate or stabilize, coordination equilibrium is plausible.',
    'If structural migration toward government power is confirmed, the constraint''s claimed coordination function is a cover story — it is a snare masquerading as tangled rope. If equilibrium, tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_boundary_migration, empirical, 'Whether categorical balancing''s boundaries are structurally stable or extractively migratory.').

omega_variable(
    minority_disfavored_categorization,
    'Is the disproportionate categorization of minority/dissident speech as ''low value'' or ''unprotected'' a structural feature of the balancing test, or a contingent historical pattern?',
    'Comparative analysis of categorization outcomes across speaker identity, controlling for speech content. If identity predicts categorization independently of content, the test structurally disfavors minorities.',
    'If structural, the constraint is a snare for minority speakers regardless of its coordination function for the majority. The tangled rope classification would require seat-specific qualification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_disfavored_categorization, empirical, 'Whether the balancing test''s ''value'' prong structurally tracks majority sensibilities.').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Is the categorical_balancing_reading a single coherent constraint, or does the label ''categorical balancing'' conflate multiple structurally distinct doctrinal regimes (e.g., strict scrutiny balancing vs. intermediate scrutiny vs. rational basis for speech)?',
    'Decompose the reading into its component scrutiny regimes and test whether each has distinct ε, beneficiaries, victims. If ε varies widely across regimes, the reading is a family, not a single constraint.',
    'If the reading decomposes into multiple constraints, each must be authored separately per ε-invariance. The current story would be an over-aggregation masking distinct extraction profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Whether ''categorical balancing'' is one constraint or a family of constraints with different extraction profiles.').

omega_variable(
    committer_structure_reading_relations,
    'How do the three readings of the first_amendment_speech_protection kernel structurally relate to each other — do they foreclose, coexist, or influence?',
    'Analyze whether adopting one reading''s core premise logically commits a party to rejecting another''s within a single framework (forecloses), or whether different parties can simultaneously hold different readings without internal contradiction (coexists_with), or whether one reading''s dominance changes the operating conditions for others (influences).',
    'Determines cs_structure.reading_relations values. Foreclosure implies zero-sum doctrinal competition; coexistence implies pluralist stability; influence implies path-dependent drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_reading_relations, conceptual, 'Structural relationship between this reading and its sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1919, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1919, 0.08).
narrative_ontology:measurement(firs_tr_t1940, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1940, 0.12).
narrative_ontology:measurement(firs_tr_t1960, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(firs_tr_t1973, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1973, 0.22).
narrative_ontology:measurement(firs_tr_t1990, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(firs_tr_t2010, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(firs_be_t1919, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1919, 0.15).
narrative_ontology:measurement(firs_be_t1940, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1940, 0.22).
narrative_ontology:measurement(firs_be_t1960, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(firs_be_t1973, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1973, 0.31).
narrative_ontology:measurement(firs_be_t1990, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement(firs_be_t2010, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2010, 0.34).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1919, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1919, 0.25).
narrative_ontology:measurement(firs_su_t1940, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1940, 0.35).
narrative_ontology:measurement(firs_su_t1960, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1960, 0.38).
narrative_ontology:measurement(firs_su_t1973, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1973, 0.4).
narrative_ontology:measurement(firs_su_t1990, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1990, 0.41).
narrative_ontology:measurement(firs_su_t2010, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__categorical_balancing_reading, 0.1).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, obscenity_doctrine).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, incitement_doctrine).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, true_threats_doctrine).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, commercial_speech_doctrine).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, student_speech_doctrine).

% DUAL FORMULATION NOTE:
% This constraint (categorical_balancing_reading) is one of three readings of the first_amendment_speech_protection kernel. The absolutist_reading claims near-zero extraction (mountain-like); the harm_limited_reading claims moderate extraction with different victim set. All three share the same constitutional text but instantiate different constraints with different ε, beneficiaries, and victims. This decomposition follows the ε-invariance principle: the label 'First Amendment protection' covers structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__categorical_balancing_reading, moderate, 0.7).
constraint_indexing:directionality_override(first_amendment_speech_protection__categorical_balancing_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
