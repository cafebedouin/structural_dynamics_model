% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public-Health-Primary Reading: Vaccination/Intervention Mandate Enforcement Regime
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the public-health-primary reading of the
 *   contested legitimate_health_intervention kernel: legitimacy is grounded
 *   exclusively in measurable population-level morbidity/mortality reduction,
 *   and individual refusal is reframed as an externality-imposing act rather
 *   than a private medical or conscience decision. Under this reading,
 *   unvaccinated and refusing individuals enter the victim set as disease
 *   vectors whose non-compliance is treated as harm-causing, while
 *   immunocompromised populations who cannot self-protect are the structural
 *   beneficiaries. Enforcement runs primarily through employers and
 *   access-gating institutions, which produces high measured extraction
 *   (termination, exclusion) even though the underlying coordination problem
 *   — population immunity — is genuine. This is one of three sibling
 *   constraints sharing the same kernel; bodily_autonomy_primary and
 *   proportionality_reading are NOT represented here and must not be folded
 *   into this ε. Each reading has its own beneficiary/victim structure and
 *   its own ε; this file speaks only for the public-health-primary premise.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter, sets thresholds and authorizes enforcement
 *   - immunocompromised_populations: powerless beneficiary, depends on aggregate compliance
 *   - employers_seeking_liability_shield: organized beneficiary/agenda_setter, administers termination-based enforcement
 *   - vaccine_refusing_workers: powerless payer, bears termination/exclusion as externality-imposer
 *   - religious_exemption_seekers: powerless payer, sincerity not weighted under this reading
 *   - civil_liberties_advocates: excluded, hold the rival premise but are not weighted internally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.71).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public-Health-Primary Reading: Vaccination/Intervention Mandate Enforcement Regime").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, 'a330d00b-22d0-4c8f-a5d5-ca88d605bf3a').
narrative_ontology:cs_kernel_codification('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', distributed).
narrative_ontology:cs_authority_grounding('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', expertise).
narrative_ontology:cs_interpretation_layer_present('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a').
narrative_ontology:cs_reading_relation('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', foundational, population_metric_dispositiveness).
narrative_ontology:cs_axiom_status(population_metric_dispositiveness, holdable).
narrative_ontology:cs_axiom_grounding('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', population_metric_dispositiveness, empirically_contingent).
narrative_ontology:cs_axiom('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', foundational, refusal_as_externality).
narrative_ontology:cs_axiom_status(refusal_as_externality, holdable).
narrative_ontology:cs_axiom_grounding('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', refusal_as_externality, instrumental).
narrative_ontology:cs_reference_frame('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', consequentialist_public_health_authority).
narrative_ontology:cs_drift_state('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', post_pandemic_enforcement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a330d00b-22d0-4c8f-a5d5-ca88d605bf3a', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, employers_seeking_liability_shield).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, vaccine_refusing_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, religious_exemption_seekers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, medically_uncertain_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, vaccinated_general_population).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, population_morbidity_reduction_as_legitimacy_ground).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, externality_theory_of_refusal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandate thresholds, defines what counts as adequate population-level protection, and authorizes enforcement mechanisms (employment conditions, access restrictions) delegated to employers and institutions. Justifies its authority entirely by reference to morbidity/mortality curves, not by reference to any individual's consent.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Cannot generate adequate immune response themselves and depend structurally on population-level vaccination coverage (herd protection) to reduce their exposure risk. They have no exit from this dependency — their survival odds are a direct function of aggregate compliance by others. They do not administer or enforce the constraint; they receive its protective effect passively.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).

% Adopt and enforce mandates set by public health guidance to reduce workplace transmission liability and insurance exposure, terminating or reassigning non-compliant employees. They benefit from regulatory cover and reduced outbreak liability while administering the actual coercive mechanism (termination) against workers.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, employers_seeking_liability_shield, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, employers_seeking_liability_shield, agenda_setter).

% Face termination, loss of income, or exclusion from workplaces and public venues for declining the intervention. Under this reading, their refusal is treated as an externality-imposing act — akin to pollution — because it is read as raising transmission risk to others, not merely as a private medical choice. Their exit options are constrained by economic dependency on employment or access to public life; leaving the labor market or region is the only genuine exit and is rarely viable.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccine_refusing_workers, payer,
    powerless, biographical, trapped, local).

% Seek exemption on sincerely held religious grounds; under the public-health-primary reading, sincerity of belief does not override the externality calculus, so exemptions are narrowed or denied when population coverage targets are not met. They experience the same enforcement consequences as outright refusers despite a different stated basis.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, religious_exemption_seekers, payer,
    powerless, biographical, trapped, local).

% Have contested or borderline medical histories (prior adverse reaction, rare contraindication under dispute) that do not meet the formal exemption bar under population-first policy design, which sets the exemption threshold narrowly to avoid coverage erosion. They bear enforcement consequences despite a genuine, if disputed, individual medical basis for refusal.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, medically_uncertain_refusers, payer,
    powerless, immediate, trapped, local).

% Comply with the mandate and receive its protective and social benefits (access, employment continuity, reduced infection risk) without bearing enforcement costs. Their compliance is what supplies the population-level effect the legitimacy claim rests on.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccinated_general_population, beneficiary,
    moderate, biographical, mobile, national).

% Argue that treating individual refusal as pure externality collapses the distinction between contagious disease transmission and bodily autonomy claims, and that the enforcement apparatus (termination, access denial) is disproportionate to actual individualized risk. Their framework is a rival reading (bodily_autonomy_primary), not adjudicated within this constraint; they are heard in litigation and public comment but the public-health-primary reading's internal logic does not weight their premise.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% Adjudicate challenges to mandate enforcement, weighing agency justifications against claimed harms. Their rulings can narrow or widen the exemption bar and the enforcement mechanisms available, without themselves being parties who benefit or pay under the constraint.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level disease suppression by making individual vaccination/intervention uptake a condition of employment and public access, so that aggregate coverage crosses the threshold needed for herd protection and reduced systemic morbidity/mortality.
% TRANSFER_FUNCTION: Moves health risk from immunocompromised and otherwise vulnerable populations onto refusing individuals — via termination, income loss, and access denial — under the premise that refusal itself constitutes an externality equivalent to imposing risk on others.
% ABSENT_VOICES: Individuals with genuine but formally unrecognized medical contraindications, and religious objectors, are present in litigation and public comment but their premises (individualized risk assessment, sincerity of belief) are not weighted inside this reading's own legitimacy calculus, which treats population-level metrics as dispositive.
% DISAPPEARANCE_RATIONALE: If this reading's enforcement apparatus vanished overnight, employers would lose the regulatory cover for mandates, terminated workers would be reinstated or exemption practices would loosen substantially, and population coverage would likely fall toward voluntary-uptake levels — a materially different equilibrium for both morbidity outcomes and individual liberty exposure.
% FOUNDING_PROBLEM: Communicable disease outbreaks (e.g., measles resurgence, COVID-19) demonstrated that voluntary uptake alone left coverage gaps large enough to sustain transmission chains and endanger populations who could not be vaccinated themselves.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and immunocompromised-advocacy groups attest the founding problem remains live, citing ongoing outbreak risk in under-vaccinated pockets. Independent legal scholars and civil liberties organizations, outside the beneficiary set, attest that the enforcement mechanisms have outrun the marginal epidemiological benefit in several documented cases (e.g., mandates persisting after coverage plateaus), supporting a partially-dead-problem, persistent-mechanism reading.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.35 to 0.68) tracking the shift from voluntary guidance to enforced mandates with employment and access consequences — the same population-protection goal increasingly pursued through coercive machinery rather than persuasion. Suppression rises even faster and plateaus higher (0.71) because once enforcement infrastructure (employer compliance systems, access-gating credential checks) is built, it becomes a standing capacity independent of the acute outbreak conditions that justified it. Theater ratio remains comparatively low and stable (~0.28) because the coordination function (actual coverage increase, actual morbidity reduction in some populations) is real and measurable, not merely performed — this is not a piton, it is an actively functioning tangled rope. Accessibility collapse (0.58) and resistance (0.74) reflect that this is a contested, actively defended arrangement, not a settled natural fact: alternatives (informed consent frameworks, proportionality-scaled interventions) remain visible and litigated, and refusers mount real resistance rather than acquiescing.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter's seat (public health agencies), the arrangement reads as necessary, proportionate coordination justified purely by aggregate outcome metrics. From the payer seats, the same structure reads as coercive extraction that reclassifies personal medical/conscience decisions as harms without individualized assessment. The engine computes this divergence from the structural data (power, exit options, beneficiary/victim declarations) rather than from either side's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised populations and the vaccinated general population sit near the beneficiary end of directionality: they receive protective effect without bearing enforcement costs. Vaccine-refusing workers, religious exemption seekers, and medically uncertain refusers sit near the full-target end: under this reading's own logic, their refusal is reclassified from private choice to externality imposition, which is precisely the move that licenses coercive enforcement against them. Employers occupy a dual position — administering enforcement (agenda_setter) while also benefiting from liability reduction (beneficiary) — captured via secondary_role rather than forced into one seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (outbreak risk from coverage gaps) is contested as live vs. dead depending on which population and which point in the coverage curve is examined — for immunocompromised populations in under-vaccinated regions it remains live; for populations that have long since crossed herd-immunity thresholds, the enforcement mechanism persists past its marginal epidemiological utility, which is the classic mandatrophy signature (status=contested, verdict=world_rearranges). This is exactly the kind of mismatch the R5 genealogy interview is built to surface: self-reported live status from agencies and employers versus independent corroboration that the mechanism has outrun the problem in specific documented instances.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_framing_validity,
    'Is treating individual medical refusal as an externality-imposing act structurally sound, or does it presuppose the very population-primacy premise the kernel contest is about (begging the question against bodily_autonomy_primary)?',
    'Compare enforcement outcomes and legal reasoning across jurisdictions that adopt public_health_primary versus proportionality_reading frameworks for the same disease threat level; assess whether externality framing tracks actual transmission risk or is applied uniformly regardless of individualized risk profile.',
    'If the externality framing is found to be applied independent of actual individualized transmission risk, this reading''s legitimacy claim weakens toward tangled_rope or snare; if tightly coupled to demonstrated risk, the coordination function is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_framing_validity, conceptual, 'Whether the externality recasting of refusal is analytically sound or question-begging against the sibling readings.').

omega_variable(
    coverage_threshold_mandatrophy,
    'At what population coverage level does the marginal epidemiological benefit of continued mandate enforcement fall below the marginal liberty/economic cost imposed on refusers — and has that threshold already been crossed in some jurisdictions where enforcement persists?',
    'Epidemiological modeling comparing current coverage levels against herd-immunity thresholds per disease, cross-referenced against continued enforcement intensity in each jurisdiction.',
    'If enforcement persists well past the coverage threshold in specific jurisdictions, those instances should be flagged as mandatrophy (founding_problem_status=dead, disappearance_verdict=world_rearranges) even while the reading remains live elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_threshold_mandatrophy, empirical, 'Whether enforcement in specific jurisdictions has outlived its epidemiological justification.').

omega_variable(
    exemption_bar_calibration,
    'Is the narrow exemption bar (denying sincere religious and medically-uncertain claims) calibrated to actual coverage-preservation necessity, or set narrower than necessary as an administrative convenience?',
    'Compare exemption rates and resulting coverage outcomes across jurisdictions with looser versus stricter exemption criteria to determine whether looser criteria meaningfully erode population coverage.',
    'If looser exemption criteria do not meaningfully erode coverage, the narrow bar is administrative overreach rather than epidemiologically necessary, shifting the classification toward higher extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exemption_bar_calibration, empirical, 'Whether the exemption bar is necessity-calibrated or administratively narrowed beyond epidemiological need.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__public_health_primary, theater_ratio, 6, 0.14).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.19).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__public_health_primary, theater_ratio, 18, 0.24).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.27).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__public_health_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement(legi_tr_t36, legitimate_health_intervention__public_health_primary, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__public_health_primary, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__public_health_primary, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__public_health_primary, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(legi_be_t36, legitimate_health_intervention__public_health_primary, base_extractiveness, 36, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__public_health_primary, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__public_health_primary, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__public_health_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(legi_su_t36, legitimate_health_intervention__public_health_primary, suppression_requirement, 36, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimate_health_intervention kernel, decomposed per the ε-invariance principle: public_health_primary (this file, high ε from enforcement machinery, victim set includes refusers reframed as vectors), bodily_autonomy_primary (consent-primacy, different victim/beneficiary structure), and proportionality_reading (severity-weighted, intermediate ε). Each reading has independently authored ε and structural data; none averages or hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
