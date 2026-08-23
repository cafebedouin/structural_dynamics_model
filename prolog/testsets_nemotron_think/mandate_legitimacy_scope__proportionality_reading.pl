% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality-Based Vaccine Mandate Legitimacy
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the proportionality_reading of the
 *   mandate_legitimacy_scope kernel: mandate legitimacy is conditional on
 *   disease severity, vaccine safety/efficacy, and availability of less
 *   restrictive alternatives. The constraint is not a single mandate but the
 *   proportionality framework itself — the legal/ethical structure that
 *   calibrates extraction to pathogen parameters. A measles mandate (high
 *   severity, high vaccine efficacy, no alternatives) operates as
 *   low-extraction coordination; a flu mandate for healthy adults (low
 *   severity, moderate efficacy, alternatives exist) operates as
 *   higher-extraction imposition. The victim set is conditional: individuals
 *   facing disproportionate mandates are victims; those facing proportionate
 *   mandates are net beneficiaries. The ε = 0.45 reflects this conditional
 *   extraction averaged across the mandate portfolio, with temporal
 *   measurements showing COVID-era spike (t=40) and partial reversion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.45).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.52).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality-Based Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '9dfb34d7-d46e-4643-8fcf-071e26b1a74d').
narrative_ontology:cs_kernel_codification('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', distributed).
narrative_ontology:cs_authority_grounding('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', lineage).
narrative_ontology:cs_interpretation_layer_present('9dfb34d7-d46e-4643-8fcf-071e26b1a74d').
narrative_ontology:cs_reading_relation('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', mandate_legitimacy_scope__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', foundational, proportionality_as_constitutional_limit).
narrative_ontology:cs_axiom_status(proportionality_as_constitutional_limit, holdable).
narrative_ontology:cs_axiom_grounding('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', proportionality_as_constitutional_limit, empirically_contingent).
narrative_ontology:cs_axiom('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', foundational, least_restrictive_means_requirement).
narrative_ontology:cs_axiom_status(least_restrictive_means_requirement, holdable).
narrative_ontology:cs_axiom_grounding('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', least_restrictive_means_requirement, conventional).
narrative_ontology:cs_reference_frame('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', jacobson_proportionality_framework).
narrative_ontology:cs_drift_state('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', post_covid_emergency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9dfb34d7-d46e-4643-8fcf-071e26b1a74d', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_infrastructure).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, pediatric_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, individuals_under_disproportionate_mandates).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, medically_contraindicated_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, individuals_facing_low_severity_mandates).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, conscientious_objectors_narrow_exemptions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, general_population_subject_to_mandates).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, general_population_subject_to_mandates).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, least_restrictive_means_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, siracusa_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue, calibrate, and enforce vaccine mandates based on epidemiological assessment. They define disease severity thresholds, vaccine safety/efficacy standards, and what counts as a less restrictive alternative. Their legitimacy rests on claiming the proportionality framework as their operating constraint.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Immunocompromised, elderly, and pediatric populations who cannot vaccinate or mount adequate immune response. They depend on herd immunity from mandates for survival. Their exit from the constraint's protection is structurally impossible — they cannot individually opt into herd immunity.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Bear the autonomy cost of mandates (bodily intrusion, employment/education exclusion for noncompliance) while receiving collective protection benefit. Exit is constrained: can seek exemptions (narrow), relocate (costly), or accept mandate. The proportionality framework means their burden varies by pathogen — measles mandate extracts little net cost; flu mandate extracts more relative to benefit.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, general_population_subject_to_mandates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, general_population_subject_to_mandates, beneficiary).

% Have valid medical reasons against vaccination (allergies, immunodeficiency, prior adverse events). When mandates narrow exemptions, they bear disproportionate extraction — either vaccinate against medical advice or face exclusion. Their identity as 'medically vulnerable' is fused with the constraint's operation; they cannot exit the category that makes them targets of mandate enforcement.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, medically_contraindicated_individuals, payer,
    powerless, biographical, identity_locked, national).

% Subject to mandates for diseases where severity/vaccine profile fails proportionality test (e.g., annual flu mandates for healthy adults, low-transmission pathogen mandates). They bear autonomy costs with minimal collective benefit. Exit is relatively mobile: can change employers, schools, or jurisdictions more easily than for high-severity mandates.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, individuals_facing_low_severity_mandates, payer,
    moderate, immediate, mobile, regional).

% Object to mandates on religious/philosophical grounds. Proportionality frameworks typically offer narrow or no conscientious exemptions, treating them as free-riders. They bear full mandate costs without the medical justification that contraindicated individuals have. Exit requires abandoning sincere beliefs or accepting exclusion — constrained by identity commitment.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, conscientious_objectors_narrow_exemptions, payer,
    moderate, biographical, constrained, national).

% Review mandate proportionality under constitutional standards (Jacobson, Siracusa, Oakes test, German proportionality). They do not bear mandate costs nor collect its benefits. Their analytical seat sees the full structure: how the framework calibrates extraction to disease parameters across jurisdictions and eras.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, constitutional_courts_adjudicators, observer,
    institutional, generational, analytical, national).

% Organize against mandates as bodily autonomy violations. They would argue proportionality is a sham — that any mandate violates integrity. Excluded from mandate design rooms; their testimony is heard in litigation but not in initial calibration. Their exclusion is structural: the framework treats objection as evidence of the problem mandates solve.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, anti_mandate_advocacy_networks, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Calibrates state compulsion to disease parameters so that mandates achieve population immunity for serious threats (measles, smallpox) while avoiding autonomy extraction for marginal threats (seasonal flu, low-transmission pathogens). Solves the collective action problem of free-riding on herd immunity without imposing universal mandates regardless of proportionality.
% TRANSFER_FUNCTION: Moves disease risk from vulnerable populations (who cannot protect themselves) to the general population (who accept mandate burdens), and moves autonomy/liberty interests from individuals to collective authority — but only when the transfer is proportionate to severity, vaccine profile, and lack of alternatives. The transfer magnitude is pathogen-dependent.
% ABSENT_VOICES: Children (subject to mandates without consent capacity), future generations (bear precedent-setting of mandate scope), medically exempt individuals whose exemptions are narrowed by emergency powers, global populations affected by vaccine nationalism that proportionality frameworks do not address. These voices are absent from the calibration calculus.
% DISAPPEARANCE_RATIONALE: Without the proportionality framework, mandate legitimacy would collapse into binary: either all mandates are legitimate (public health absolutism) or no mandates are legitimate (bodily autonomy absolutism). The framework's disappearance would force jurisdictions to adopt one pole or the other, restructuring the entire legal architecture of compulsory vaccination.
% FOUNDING_PROBLEM: How to reconcile state authority to protect public health with constitutional guarantees of bodily integrity and liberty in a way that is neither absolutist (all mandates valid / no mandates valid) nor arbitrary (case-by-case with no principled calibration).
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts globally corroborate: Jacobson v. Massachusetts (1905) established proportionality seed; German Federal Constitutional Court (Measles Vaccination Act 2020) applied structured proportionality; Canadian Charter s.1 Oakes test (repeatedly); South African Constitutional Court (2022) on COVID mandates; WHO Siracusa Principles (1984) as international law codification. Bioethics literature (Childress & Faden, Gostin, Buchanan) corroborates from outside state authority. The problem remains live per ongoing litigation over COVID, flu, and HPV mandates.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).
:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the framework's operation extracts autonomy from some for the benefit of others, but the extraction is calibrated — not uniform. Suppression (0.52) reflects enforcement machinery (school exclusion, employment termination, fines) that activates when mandates are contested. Theater ratio (0.28) captures the gap between proportionality rhetoric and actual calibration: emergency powers often suspend the framework's least-restrictive-means requirement. Accessibility collapse (0.58) is moderate — alternatives (masking, testing, remote work) exist but are treated as insufficient by authorities. Resistance (0.55) reflects sustained litigation and political mobilization against mandates deemed disproportionate. The claimed type is tangled_rope: genuine coordination function (disease control for serious threats) coexists with asymmetric extraction that varies by pathogen and population.
 *
 * PERSPECTIVAL GAP:
 *   From the authority seat, the framework is a rope: it solves a genuine collective action problem with calibrated coercion. From the medically contraindicated seat, it is a snare: their specific vulnerability is exploited by narrowing exemptions. From the low-severity mandate seat, it is extraction without coordination benefit. The engine computes this divergence from the declared structural positions — the proportionality framework's genius and its danger is that it produces different constraint types for different seats simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda_setters with arbitrage-grade exit (they design the framework). Vulnerable populations are trapped beneficiaries — they cannot exit the need for herd immunity. General population are payers with constrained exit (dual role: pay autonomy costs, receive protection). Medically contraindicated are identity_locked payers — their medical status fuses them to disproportionate burden. Low-severity mandate subjects are mobile payers — can exit jurisdictionally. Conscientious objectors are constrained payers — identity commitment limits exit. Courts are analytical observers. Anti-mandate advocates are excluded — their structural position is objection without access to calibration.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality framework was built to solve the binary trap (all mandates valid / none valid). That founding problem remains live — new pathogens (COVID, H5N1, mpox) continually test the calibration. However, mandatrophy risk appears in two forms: (1) emergency powers that suspend proportionality (the framework becomes a scaffold without sunset), and (2) mandate expansion to low-severity pathogens where the coordination function atrophies but the enforcement machinery persists (piton drift). The theater ratio rise at t=40 (COVID) captures emergency suspension; the partial reversion at t=50 captures incomplete restoration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_natural_law_vs_constructed,
    'Is the proportionality principle a discovered constraint of practical reason (natural law) or a constructed legal framework that serves identifiable institutional interests?',
    'Comparative constitutional analysis: if proportionality analysis converges across unrelated legal traditions (German, Canadian, South African, European Court of Human Rights, US strict scrutiny) on the same calibration structure, that supports natural law reading. If calibration diverges predictably with state capacity and political culture, that supports constructed reading.',
    'If natural law, the framework is a mountain (emerges_naturally=true, extraction near zero). If constructed, it is a tangled rope or snare depending on who calibrates. Current ε=0.45 assumes constructed with genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_natural_law_vs_constructed, conceptual, 'Whether proportionality is a natural constraint or institutional construction').

omega_variable(
    pathogen_specific_extraction_measurement,
    'How should base extractiveness be measured for a constraint whose ε varies by pathogen — as a portfolio average, as a distribution, or as separate constraints per pathogen?',
    'Decompose the mandate portfolio into pathogen-specific constraint stories (measles_mandate, flu_mandate, covid_mandate, hpv_mandate) each with its own ε, then test whether the proportionality framework adds extraction beyond the sum of its parts. The ε-invariance principle suggests decomposition is required if ε differs structurally.',
    'If decomposition is required, this story should be a constraint family with network links. The current single-story approach with averaged ε=0.45 may mask snare-class mandates (flu) inside a tangled_rope portfolio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pathogen_specific_extraction_measurement, conceptual, 'Whether conditional ε requires constraint decomposition per ε-invariance principle').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, employment exclusion) or partially internalized (moral suasion, social pressure, identity fusion with ''good citizen'' compliance)?',
    'Post-mandate suppression trajectory: if compliance persists after legal penalties are removed (e.g., after COVID emergency ends), internalized suppression is present. Survey experiments measuring compliance under anonymity vs. observation.',
    'If internalized, effective suppression exceeds structural measure — targets carry suppression with them after formal exit. This would increase χ for identity_locked payers (medically contraindicated, conscientious objectors) beyond what structural d predicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in mandate compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_tr_t10, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_tr_t20, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_tr_t30, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_tr_t40, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_tr_t50, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_be_t10, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_be_t20, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_be_t30, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_be_t40, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_be_t50, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_su_t10, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_su_t20, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_su_t30, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_su_t40, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(mandate_legitimacy_scope__proportionality_reading_su_t50, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one member of the mandate_legitimacy_scope constraint family. The proportionality_reading calibrates ε to pathogen parameters (moderate, variable). The public_health_primary reading treats mandate legitimacy as broader (lower threshold, higher ε for marginal mandates). The bodily_autonomy_primary reading treats mandate legitimacy as near-zero (ε≈0 for all mandates, extraction is violation). All three share the kernel but instantiate different constraints with different ε, different victim sets, and different claimed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__proportionality_reading, institutional, 0.15).
constraint_indexing:directionality_override(mandate_legitimacy_scope__proportionality_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
