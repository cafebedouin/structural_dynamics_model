% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: First Amendment Categorical Balancing Framework
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   The categorical balancing reading of the First Amendment holds that 'no
 *   law' does not mean no law — instead, the Amendment authorizes courts to
 *   balance speech's value against its potential harm, creating
 *   judicially-administered categories of protected and unprotected speech.
 *   This reading originated in Schenck (1919) and matured through Chaplinsky
 *   (1942), Roth (1957), Brandenburg (1969), and Miller (1973). The
 *   constraint presents itself as a coordination mechanism (solving the
 *   text-vs-regulation paradox) but operates as an extraction mechanism: the
 *   institutional judiciary captures interpretive monopoly while minority
 *   speakers and excluded-category speakers bear the costs of categorical
 *   exclusion and doctrinal unpredictability. The claim/metric gap is
 *   structural: the reading claims to be a rope (neutral coordination) but
 *   the metrics reveal tangled_rope (coordination + asymmetric extraction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.55).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.45).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing Framework").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, 'a819e73a-a99f-47b3-80fe-907635902e76').
narrative_ontology:cs_kernel_codification('a819e73a-a99f-47b3-80fe-907635902e76', formalized).
narrative_ontology:cs_authority_grounding('a819e73a-a99f-47b3-80fe-907635902e76', lineage).
narrative_ontology:cs_interpretation_layer_present('a819e73a-a99f-47b3-80fe-907635902e76').
narrative_ontology:cs_reading_relation('a819e73a-a99f-47b3-80fe-907635902e76', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a819e73a-a99f-47b3-80fe-907635902e76', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('a819e73a-a99f-47b3-80fe-907635902e76', foundational, judicial_balancing_authority).
narrative_ontology:cs_axiom_status(judicial_balancing_authority, holdable).
narrative_ontology:cs_axiom_grounding('a819e73a-a99f-47b3-80fe-907635902e76', judicial_balancing_authority, conventional).
narrative_ontology:cs_axiom('a819e73a-a99f-47b3-80fe-907635902e76', foundational, categorical_exclusion_legitimacy).
narrative_ontology:cs_axiom_status(categorical_exclusion_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a819e73a-a99f-47b3-80fe-907635902e76', categorical_exclusion_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('a819e73a-a99f-47b3-80fe-907635902e76', judicial_balancing_mandate).
narrative_ontology:cs_drift_state('a819e73a-a99f-47b3-80fe-907635902e76', contemporary_doctrinal_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a819e73a-a99f-47b3-80fe-907635902e76', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, excluded_category_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, government_regulators).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, general_speakers).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, judicial_balancing_authority).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, categorical_exclusion_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal and state courts define and apply the categorical balancing test, creating and maintaining the taxonomy of protected and unprotected speech (obscenity, incitement, true threats, fighting words, defamation, commercial speech). They benefit from the interpretive control this framework grants — the power to say what speech counts as valuable enough to protect. Their exit options are maximal: they author the doctrine and face no institutional constraint on its evolution.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, beneficiary).

% Communities historically targeted by speech restrictions (racial, religious, sexual, political minorities) bear disproportionate costs when the judiciary places their expression into unprotected categories. The balancing test's 'speech value' assessment systematically undervalues marginalized perspectives. Their exit is identity-locked: they cannot exit their social position or the categories that capture their speech, and the constraint's legitimacy depends on their continued subjection to it.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers, payer,
    powerless, biographical, identity_locked, national).

% Speakers whose expression falls within judicially-declared unprotected categories — producers of sexually explicit material, political radicals accused of incitement, speakers deemed to utter true threats — face categorical exclusion from First Amendment protection. They can attempt to litigate category boundaries or modify their speech, but the categories themselves are judicially controlled and their exit from the constraint's reach is structurally constrained.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, excluded_category_speakers, payer,
    moderate, biographical, constrained, national).

% The broad public of speakers lives under doctrinal uncertainty: the balancing test means protection depends on a court's ex post assessment of their speech's value versus its harm. They bear the cost of unpredictability but retain mobility — they can self-censor, seek legal counsel, or forum-shop. Their organizations (ACLU, FIRE, media coalitions) can litigate to shape category boundaries.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, general_speakers, payer,
    organized, biographical, mobile, national).

% Legislatures and executives gain regulatory latitude within the unprotected categories the judiciary creates. They benefit from the balancing framework's permission structure: once a category is declared unprotected, regulation faces minimal scrutiny. Their exit is mobile — they can choose whether to regulate, and regulatory regimes vary across jurisdictions.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, government_regulators, beneficiary,
    institutional, biographical, mobile, national).

% Legal scholars and historians analyze the balancing framework's doctrinal evolution, normative foundations, and empirical effects. They neither collect rents nor bear direct costs from the categories, but their professional standing depends on the framework's continued complexity. Their exit is analytical: they can critique from outside but their discourse constitutes the framework's intellectual environment.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_academy, observer,
    analytical, generational, analytical, global).

% Advocates and justices who read the First Amendment as categorically prohibiting speech regulation ('no law means no law') are structurally excluded from the balancing framework's operation. Their position is treated as a dissenting opinion, not a live interpretive option within the doctrine. They are trapped: the constraint's categories exist precisely to marginalize their reading, and they cannot exit the legal system that enforces it.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, absolutist_advocates, excluded,
    moderate, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable framework for constitutional speech protection that reconciles absolute constitutional text ('no law') with government's legitimate interest in regulating genuinely harmful expression, by delegating category-creation to judicial balancing rather than legislative whim.
% TRANSFER_FUNCTION: Moves interpretive authority from the constitutional text's literal command to judicial balancing calculus, concentrating the power to define speech's constitutional value in courts while distributing the costs of categorical exclusion and doctrinal uncertainty to speakers — especially minority speakers and those in judicially-disfavored categories.
% ABSENT_VOICES: Speakers whose expression has been historically categorized as unprotected — particularly sexual minorities targeted by obscenity doctrine, political dissidents targeted by incitement doctrine, and marginalized communities disproportionately charged with true threats — are absent from the balancing calculus that defines the categories. Their exclusion is structural: the categories are defined by the judiciary without their participation, and their speech is the raw material the categories sort.
% DISAPPEARANCE_RATIONALE: If the categorical balancing framework vanished overnight, speech protection would revert to one of two poles: absolutist textualism (no categories, near-total protection) or harm-based limitation (protection yields to demonstrated harm). Either shift would fundamentally restructure the First Amendment's architecture, redistribute interpretive authority from courts to either text or legislature, and reclassify vast swaths of currently regulated speech.
% FOUNDING_PROBLEM: The need to reconcile the First Amendment's absolute textual command ('Congress shall make no law... abridging the freedom of speech') with the universal judicial and legislative consensus that some speech regulation is necessary and legitimate (obscenity, incitement, fraud, threats).
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the judiciary (David Rabban, 'Free Speech in Its Forgotten Years'; Geoffrey Stone, 'Perilous Times') document that the balancing framework emerged from judicial construction in the early 20th century, not from the founding era. The judiciary itself acknowledges the framework's evolution in United States v. Stevens (2010), where the Court refused to recognize a new unprotected category, admitting the categories are judge-made. No non-judicial source corroborates the claim that the founding generation understood 'no law' to authorize judicial category-creation.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the judiciary's capture of categorization authority — a power the text does not explicitly grant. Suppression (0.45) is moderate because categories exclude specific speech types but leave most speech protected. Theater ratio (0.25) is low-moderate: the balancing test performs real doctrinal work, but the category boundaries (especially obscenity and true threats) show performative maintenance. Accessibility collapse (0.50) reflects that speakers cannot easily exit the framework — alternative regimes (absolutism, harm-limitation) exist but are legally foreclosed. Resistance (0.60) is high: the categories remain doctrinally contested (Stevens 2010, Alvarez 2012, Counterman 2023) and the balancing test itself faces persistent academic and judicial critique.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the balancing framework is genuine coordination — it solves the text/regulation paradox and prevents legislative overreach. From minority and excluded-category speakers' seats, the same structure operates as enforced extraction — categories reflect majority/judicial values, not neutral value/harm calculus. The engine computes this divergence from the structural data: the agenda-setter's arbitrage exit vs. the payers' identity-locked/constrained exit produces diametrically opposed effective extraction values for the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional judiciary sits at the beneficiary pole (d ≈ 0.1): it authors the categories, collects interpretive rents, and faces no exit constraint. Minority speakers and excluded-category speakers sit at the target pole (d ≈ 0.85–0.9): they bear categorical exclusion and identity-locked or constrained exit. General speakers sit near symmetric (d ≈ 0.5): they bear uncertainty costs but retain mobility. Government regulators are secondary beneficiaries (d ≈ 0.2): they gain regulatory space within judicial categories. Absolutist advocates are excluded (d ≈ 0.95): their reading is structurally foreclosed by the framework's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling 'no law' with necessary regulation) remains contested: originalists argue the founding generation understood narrow historical exceptions (libel, obscenity, incitement), not open-ended judicial balancing. The categorical balancing reading prevents mislabeling by exposing the dual structure: the coordination function (preventing legislative censorship) is real, but the extraction function (judicial category monopoly) is equally real. Calling this a pure rope would ignore the extraction; calling it a pure snare would ignore the coordination. Tangled rope captures the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the categorical balancing framework a faithful reading of the First Amendment kernel, or a judicial construction that displaces the kernel''s original meaning?',
    'Comparative analysis of founding-era speech understandings vs. early 20th century judicial innovation; originalist vs. living constitutionalist methodological dispute.',
    'If judicial construction, the constraint''s claimed coordination function (faithful constitutional interpretation) is undermined — it becomes extraction masked as fidelity. If faithful reading, the extraction is the price of constitutional workability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading instantiates the kernel or constructs a new constraint atop it.').

omega_variable(
    absolutist_foreclosure,
    'Does the categorical balancing reading logically foreclose the absolutist reading within a single constitutional framework, or do they coexist as competing live positions?',
    'Doctrinal analysis: can a single justice consistently apply balancing while accepting absolutist premises? Historical test: have justices switched between frameworks?',
    'If forecloses, the kernel has a structural split — the two readings cannot occupy the same authority structure. If coexists_with, the kernel tolerates irreducible pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_foreclosure, conceptual, 'Structural relationship between categorical balancing and absolutist readings of the same kernel.').

omega_variable(
    harm_limited_distinction,
    'Is the harm_limited_reading a distinct constraint or a parameter variation within the categorical balancing framework?',
    'Compare the structural data: do both readings share beneficiaries (judiciary), victims (speakers in disfavored categories), and enforcement (judicial review)? If yes, they may be one constraint with a calibration difference.',
    'If same constraint, the kernel has two readings but one structural constraint — the ε-invariance principle demands a single story. If distinct, they are a constraint family linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_limited_distinction, conceptual, 'Whether harm-based limitation is a sibling reading or a calibration of this one.').

omega_variable(
    category_boundary_stability,
    'Are the unprotected categories (obscenity, incitement, true threats) stable coordinative settlements or expanding extraction zones?',
    'Longitudinal doctrinal analysis: track category boundaries from Chaplinsky/Miller/Brandenburg to Counterman/Stevens. Measure whether categories contract, expand, or drift.',
    'If expanding extraction zones, extractiveness trajectory will continue rising and theater ratio may increase. If stable settlements, current metrics represent equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_boundary_stability, empirical, 'Whether the categorical architecture is settling or metastasizing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa_cbr_tr_t1919, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement(fa_cbr_tr_t1942, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1942, 0.15).
narrative_ontology:measurement(fa_cbr_tr_t1957, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1957, 0.18).
narrative_ontology:measurement(fa_cbr_tr_t1969, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1969, 0.2).
narrative_ontology:measurement(fa_cbr_tr_t1973, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1973, 0.22).
narrative_ontology:measurement(fa_cbr_tr_t1992, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1992, 0.23).
narrative_ontology:measurement(fa_cbr_tr_t2010, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(fa_cbr_tr_t2024, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(fa_cbr_be_t1919, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1919, 0.25).
narrative_ontology:measurement(fa_cbr_be_t1942, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1942, 0.35).
narrative_ontology:measurement(fa_cbr_be_t1957, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1957, 0.4).
narrative_ontology:measurement(fa_cbr_be_t1969, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1969, 0.45).
narrative_ontology:measurement(fa_cbr_be_t1973, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1973, 0.5).
narrative_ontology:measurement(fa_cbr_be_t1992, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1992, 0.52).
narrative_ontology:measurement(fa_cbr_be_t2010, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(fa_cbr_be_t2024, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(fa_cbr_su_t1919, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1919, 0.3).
narrative_ontology:measurement(fa_cbr_su_t1942, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1942, 0.35).
narrative_ontology:measurement(fa_cbr_su_t1957, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1957, 0.38).
narrative_ontology:measurement(fa_cbr_su_t1969, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1969, 0.4).
narrative_ontology:measurement(fa_cbr_su_t1973, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1973, 0.42).
narrative_ontology:measurement(fa_cbr_su_t1992, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1992, 0.43).
narrative_ontology:measurement(fa_cbr_su_t2010, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement(fa_cbr_su_t2024, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__categorical_balancing_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the first_amendment_speech_protection kernel. The absolutist_reading claims the kernel means categorical protection with only historical exceptions; the harm_limited_reading claims protection yields to demonstrated unconsented harm. All three readings share the same constitutional text but instantiate different constraints with different beneficiary/victim structures and different ε values. This reading's ε (0.55) is higher than the absolutist_reading's (near 0) because it authorizes judicial category-creation; lower than harm_limited_reading's if that reading permits broader legislative regulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__categorical_balancing_reading, institutional, 0.1).
constraint_indexing:directionality_override(first_amendment_speech_protection__categorical_balancing_reading, powerless, 0.9).
constraint_indexing:directionality_override(first_amendment_speech_protection__categorical_balancing_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
