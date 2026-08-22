% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality Test for Vaccine Mandate Legitimacy
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the proportionality_reading of the
 *   mandate_legitimacy_scope kernel. Under this reading, a vaccine mandate's
 *   legitimacy is not absolute but conditional on three parameters: disease
 *   severity, vaccine safety/efficacy, and availability of less restrictive
 *   alternatives. This creates a structurally partitioned victim set: mandate
 *   subjects facing a high-severity pathogen with a safe/effective vaccine
 *   and no alternatives (e.g., measles) are not victims — the constraint
 *   operates as genuine coordination for them. Subjects facing a low-severity
 *   pathogen, or a vaccine with questionable safety/efficacy, or where
 *   alternatives exist (e.g., seasonal influenza) are victims — the
 *   constraint extracts from them without sufficient coordination
 *   justification. The engine computes per-seat classification from this
 *   structural asymmetry. The other two readings of this kernel —
 *   public_health_primary (mandates legitimate when necessary to protect
 *   vulnerable populations) and bodily_autonomy_primary (mandates violate
 *   bodily integrity regardless of collective benefit) — are separate
 *   constraint stories with different ε, different victim sets, and different
 *   types.
 *
 * KEY AGENTS:
 *   - vulnerable_populations: Primary beneficiary (institutional/biographical) — protected by mandates when proportionality conditions met
 *   - public_health_institutions: Agenda setter/beneficiary (institutional/generational) — administers mandates, gains legitimacy and compliance infrastructure
 *   - mandate_subjects_high_risk_pathogen: Conditional beneficiary/payer (moderate/biographical/constrained) — measles context: gains herd immunity, bears minor intrusion
 *   - mandate_subjects_low_risk_pathogen: Victim/payer (moderate/biographical/constrained) — flu context: bears mandate burden without proportional justification
 *   - courts_judicial_review: Observer/analytical (institutional/generational/analytical) — adjudicates proportionality in specific cases
 *   - vaccine_skeptic_communities: Excluded (organized/biographical/trapped) — would challenge mandate legitimacy but structurally excluded from proportionality calculus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.42).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.38).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality Test for Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '945ceb55-06e5-4f43-8022-774236279bf5').
narrative_ontology:cs_kernel_codification('945ceb55-06e5-4f43-8022-774236279bf5', fixed_text).
narrative_ontology:cs_authority_grounding('945ceb55-06e5-4f43-8022-774236279bf5', lineage).
narrative_ontology:cs_interpretation_layer_present('945ceb55-06e5-4f43-8022-774236279bf5').
narrative_ontology:cs_reading_relation('945ceb55-06e5-4f43-8022-774236279bf5', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('945ceb55-06e5-4f43-8022-774236279bf5', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('945ceb55-06e5-4f43-8022-774236279bf5', foundational, mandate_legitimacy_conditional_on_proportionality).
narrative_ontology:cs_axiom_status(mandate_legitimacy_conditional_on_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('945ceb55-06e5-4f43-8022-774236279bf5', mandate_legitimacy_conditional_on_proportionality, conventional).
narrative_ontology:cs_axiom('945ceb55-06e5-4f43-8022-774236279bf5', foundational, least_restrictive_means_required).
narrative_ontology:cs_axiom_status(least_restrictive_means_required, holdable).
narrative_ontology:cs_axiom_grounding('945ceb55-06e5-4f43-8022-774236279bf5', least_restrictive_means_required, conventional).
narrative_ontology:cs_reference_frame('945ceb55-06e5-4f43-8022-774236279bf5', jacobson_police_power_framework).
narrative_ontology:cs_drift_state('945ceb55-06e5-4f43-8022-774236279bf5', post_covid_mandate_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('945ceb55-06e5-4f43-8022-774236279bf5', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_institutions).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, mandate_subjects_high_risk_pathogen).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, mandate_subjects_low_risk_pathogen).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, mandate_subjects_high_risk_pathogen).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, least_restrictive_means_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Immunocompromised, elderly, and medically fragile people who gain indirect protection from herd immunity when mandates achieve high coverage. They do not bear the mandate's direct costs but depend on its coordination function. Their exit from vulnerability is medically constrained.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    moderate, biographical, constrained, national).

% Health departments, CDC, WHO — they design, justify, and administer mandate policies. They gain institutional legitimacy, compliance infrastructure, and budgetary authority from successful mandates. They can shift between pathogens and policy tools (arbitrage-grade exit from any single mandate).
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, public_health_institutions, beneficiary).

% Parents, workers, students subject to mandates for severe diseases (measles, polio). They bear the intrusion of compelled vaccination but gain direct protection and contribute to herd immunity that protects them. Their exit is constrained — school/work access depends on compliance, but the trade-off is favorable.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, mandate_subjects_high_risk_pathogen, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, mandate_subjects_high_risk_pathogen, beneficiary).

% Same demographic as above but facing mandates for low-severity diseases (seasonal flu, COVID-19 post-variant) where vaccine efficacy wanes, alternatives exist, or severity is low. They bear the mandate's full intrusion without proportional benefit. Exit remains constrained (employment, education access) but the cost-benefit is inverted.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, mandate_subjects_low_risk_pathogen, payer,
    moderate, biographical, constrained, national).

% Courts applying intermediate scrutiny or strict scrutiny to mandate challenges. They do not bear costs or collect benefits directly but structurally determine where the proportionality line falls. Their analytical seat sees the full victim set partition across pathogens.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, courts_judicial_review, observer,
    institutional, generational, analytical, national).

% Communities that reject vaccine mandates on philosophical, religious, or safety grounds. They are structurally excluded from the proportionality calculus — their objections are heard in court but the test's parameters (severity, efficacy, alternatives) are set by public health institutions. They bear suppression (mandates apply regardless) without voice in the calibration.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_skeptic_communities, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__proportionality_reading, public_health_institutions).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of achieving herd immunity for severe diseases: individual vaccination decisions under-protect vulnerable populations; mandates align individual incentives with collective protection when disease severity, vaccine efficacy, and lack of alternatives make the trade-off favorable.
% TRANSFER_FUNCTION: Moves bodily autonomy and medical decision-making authority from mandate subjects to public health institutions, conditional on disease parameters. For severe pathogens with effective vaccines and no alternatives, the transfer is reciprocated with protection. For marginal cases, the transfer is one-way extraction.
% ABSENT_VOICES: Vaccine-skeptic communities and philosophical objectors are structurally excluded — their voices enter only through litigation, not through the proportionality test's parameter-setting. Future generations (who inherit precedent) are also absent. The proportionality test's parameters are set by the institutions that benefit from mandates.
% DISAPPEARANCE_RATIONALE: If the proportionality test vanished, mandates would either become absolute (public_health_primary reading dominates — all vaccines mandatory regardless of severity) or forbidden (bodily_autonomy_primary reading dominates — no mandates ever). The conditional structure is what partitions the victim set by pathogen. Without it, the legal landscape reorganizes completely.
% FOUNDING_PROBLEM: Early 20th century smallpox mandates (Jacobson v. Massachusetts) established state police power to compel vaccination during epidemics. The proportionality reading emerged later as courts required the state to show the mandate was necessary and not excessive relative to the threat.
% FOUNDING_PROBLEM_CORROBORATION: Jacobson v. Massachusetts (1905) and subsequent jurisprudence (Zucht v. King, Prince v. Massachusetts) corroborate the state's police power origin. The proportionality refinement is corroborated by modern constitutional scholars (e.g., Gostin, Jacobson) and international human rights frameworks (Siracusa Principles) — sources outside the public health institutions that benefit from mandate authority.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   The proportionality reading is a tangled_rope because it has BOTH a genuine coordination function (protecting vulnerable populations from severe disease via high-efficacy vaccines when no alternatives exist) AND asymmetric extraction (subjects bear mandate burdens for low-severity diseases, marginal vaccine efficacy, or where alternatives exist). Extraction is moderate (0.42) and varies by pathogen — the ε-invariance principle demands separate stories for measles vs flu mandates, but this reading's structure is the conditional test itself. Suppression (0.38) reflects legal enforcement (school exclusion, employment mandates) but is not total — exemptions exist. Theater (0.25) captures performative 'science-based' framing that sometimes masks policy preferences. Accessibility collapse (0.45) and resistance (0.55) reflect that alternatives (masking, testing, remote work) partially mitigate but don't eliminate the mandate's force.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations are full beneficiaries (d ≈ 0.0) — they gain protection without bearing mandate costs. Public health institutions are agenda setters with beneficiary subsidy (d ≈ 0.15) — they administer and gain institutional capacity. Mandate subjects split: high-risk-pathogen subjects are near-symmetric (d ≈ 0.45) — they gain herd immunity but bear intrusion; low-risk-pathogen subjects are targets (d ≈ 0.85) — they bear costs without proportional benefit. Courts are analytical observers (d ≈ 0.5). Vaccine-skeptic communities are excluded/trapped (d ≈ 1.0) — they bear suppression without voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality test prevents mislabeling coordination as pure extraction by making extraction conditional on disease parameters. A mandate that persists after the founding problem (severe disease threat) is resolved becomes a piton or snare. The conditional victim set is the structural signal: when the same legal form applies to both measles and flu, the extraction on flu subjects is not coordination — it is the constraint's extractive component riding on the measles coordination story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_proportionality,
    'Is the proportionality_reading of mandate_legitimacy_scope a distinct constraint from its sibling readings, or a parameterization of a single constraint?',
    'Apply the ε-invariance test: if the proportionality reading''s ε varies by pathogen while siblings'' ε does not, they are distinct constraints. Confirm by checking whether the victim set is structurally conditional on disease parameters.',
    'If distinct, the proportionality reading instantiates a tangled_rope whose extraction is conditional and whose victim set partitions by disease severity; siblings are separate constraint stories linked by affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_proportionality, conceptual, 'Whether the proportionality reading is a separate constraint from public_health_primary and bodily_autonomy_primary').

omega_variable(
    severity_threshold_ambiguity,
    'Where is the disease severity threshold that flips a mandate from legitimate to illegitimate under proportionality?',
    'Analyze case law and public health guidance for the operational threshold where ''less restrictive alternatives'' become sufficient. Measles and flu are clear poles; the boundary is contested.',
    'Threshold location determines the victim set partition. A high threshold means fewer pathogens trigger legitimate mandates (smaller victim set, lower ε); a low threshold means more pathogens trigger mandates (larger victim set, higher ε).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_threshold_ambiguity, empirical, 'The disease severity threshold that partitions the victim set under proportionality').

omega_variable(
    vaccine_safety_efficacy_measurement,
    'How are vaccine safety and efficacy measured for the proportionality test, and does measurement method change the extraction profile?',
    'Compare regulatory approval standards (FDA/EMA) against real-world effectiveness data across populations. If EUA vs full approval creates different victim sets, the constraint''s ε becomes measurement-dependent.',
    'If safety/efficacy measurement changes the victim set, the proportionality reading may decompose further. High efficacy/safety = lower extraction on subjects; low efficacy/safety = higher extraction on subjects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaccine_safety_efficacy_measurement, empirical, 'Whether vaccine safety/efficacy measurement method affects the proportionality constraint''s extraction profile').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mandate_proportionality_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mandate_proportionality_tr_t25, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(mandate_proportionality_tr_t50, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(mandate_proportionality_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mandate_proportionality_be_t25, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(mandate_proportionality_be_t50, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mandate_proportionality_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(mandate_proportionality_su_t25, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 25, 0.32).
narrative_ontology:measurement(mandate_proportionality_su_t50, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, school_vaccine_requirements).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, healthcare_worker_mandates).

% DUAL FORMULATION NOTE:
% This is the proportionality_reading of the mandate_legitimacy_scope kernel family. The three readings share the kernel 'state authority to compel medical intervention' but instantiate different constraints with different ε, victim sets, and types. Proportionality = tangled_rope (conditional extraction). Public_health_primary = rope or tangled_rope depending on enforcement scope. Bodily_autonomy_primary = mountain from the autonomy seat, snare from the state seat.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__proportionality_reading, moderate, 0.85).
constraint_indexing:directionality_override(mandate_legitimacy_scope__proportionality_reading, organized, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
