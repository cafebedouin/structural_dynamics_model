% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Primacy: Population Outcomes as Sufficient Legitimacy for Coercive Intervention
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the public_health_primary reading of the
 *   legitimate_health_intervention kernel: legitimacy derives solely from
 *   measurable population-level morbidity/mortality reduction, and individual
 *   refusal is classified as externality imposition justifying coercive
 *   enforcement. The constraint operates through vaccine mandates, employment
 *   termination for non-compliance, venue access restrictions, and travel
 *   bans. The unvaccinated are structurally positioned as disease vectors
 *   bearing the constraint's extraction; the immunocompromised are its
 *   primary beneficiaries. The coordination function (disease suppression) is
 *   genuine but the extraction is asymmetric and enforced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.78).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.82).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Primacy: Population Outcomes as Sufficient Legitimacy for Coercive Intervention").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '33efdb1f-78a9-4c31-81d9-a9167eed4461').
narrative_ontology:cs_kernel_codification('33efdb1f-78a9-4c31-81d9-a9167eed4461', formalized).
narrative_ontology:cs_authority_grounding('33efdb1f-78a9-4c31-81d9-a9167eed4461', extraction).
narrative_ontology:cs_interpretation_layer_present('33efdb1f-78a9-4c31-81d9-a9167eed4461').
narrative_ontology:cs_reading_relation('33efdb1f-78a9-4c31-81d9-a9167eed4461', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('33efdb1f-78a9-4c31-81d9-a9167eed4461', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('33efdb1f-78a9-4c31-81d9-a9167eed4461', foundational, population_health_outcome_is_sufficient_legitimacy).
narrative_ontology:cs_axiom_status(population_health_outcome_is_sufficient_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('33efdb1f-78a9-4c31-81d9-a9167eed4461', population_health_outcome_is_sufficient_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('33efdb1f-78a9-4c31-81d9-a9167eed4461', foundational, individual_refusal_is_externality_imposition).
narrative_ontology:cs_axiom_status(individual_refusal_is_externality_imposition, holdable).
narrative_ontology:cs_axiom_grounding('33efdb1f-78a9-4c31-81d9-a9167eed4461', individual_refusal_is_externality_imposition, instrumental).
narrative_ontology:cs_reference_frame('33efdb1f-78a9-4c31-81d9-a9167eed4461', police_power_parens_patriae_framework).
narrative_ontology:cs_drift_state('33efdb1f-78a9-4c31-81d9-a9167eed4461', post_emergency_phase_covid, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('33efdb1f-78a9-4c31-81d9-a9167eed4461', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, general_population).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, public_health_infrastructure).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, employers_subject_to_mandates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, general_population).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, population_health_outcome_is_sufficient_legitimacy).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, individual_refusal_is_externality_imposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce vaccine mandates, quarantine orders, and access restrictions citing police power and parens patriae authority. Collect compliance data and allocate enforcement resources. Their legitimacy rests on demonstrable population-level morbidity/mortality reduction.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Cannot safely vaccinate or mount adequate immune response; depend on high population immunity for survival. Bear no enforcement costs but receive the primary protective benefit of mandates. Have no exit from vulnerability.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, national).

% Gains reduced disease transmission, healthcare system stability, and economic continuity from mandates. Bears diffuse costs: compliance friction, occasional adverse events, tax burden for enforcement infrastructure. Exit is constrained by residency and citizenship.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, general_population, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, general_population, payer).

% Face employment termination, education exclusion, venue access bans, and travel restrictions for refusing vaccination. Classified as disease vectors whose refusal imposes externalities on the immunocompromised and healthcare system. Exit options: comply, relocate to low-enforcement jurisdictions, or bear escalating exclusion costs.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_individuals, payer,
    moderate, immediate, constrained, national).

% Required to enforce vaccination verification, terminate non-compliant employees, and bear litigation risk from both sides. Compliance costs include tracking systems, legal counsel, and workforce disruption. Exit is constrained by licensing, contracts, and regulatory capture — cannot opt out of mandate enforcement without losing operating authority.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, employers_subject_to_mandates, payer,
    organized, biographical, constrained, national).

% Adjudicate challenges to mandates under Jacobson v. Massachusetts, strict scrutiny, and statutory authority frameworks. Their rulings calibrate the enforceable boundary of public health power. They neither collect nor pay the constraint's extraction but determine its legal contours.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, courts_and_oversight_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves population-level disease suppression through coordinated immunity, preventing healthcare collapse and protecting those who cannot vaccinate. Solves the collective action problem where individual vaccination decisions create positive externalities that no single actor internalizes.
% TRANSFER_FUNCTION: Moves liberty interests and bodily autonomy from unvaccinated individuals (who bear mandates, exclusions, employment loss) to immunocompromised and general population (who receive infection risk reduction). Moves enforcement costs to employers and state capacity. Moves legitimacy claims from individual consent to measurable epidemiological outcomes.
% ABSENT_VOICES: Future cohorts who will inherit the precedent of state bodily intrusion for population health; vaccine-injured individuals whose compensation claims are structurally excluded from the mandate calculus; global populations in low-access regions whose viral evolution risk is externalized by high-income mandate regimes.
% DISAPPEARANCE_RATIONALE: If mandates vanished overnight, vaccination rates would drop measurably within weeks; immunocompromised mortality would rise; healthcare surge capacity would be tested; employers would face OSHA/liability chaos; the legal architecture of emergency public health authority would face immediate legislative challenge. The arrangement carries the world.
% FOUNDING_PROBLEM: Recurring epidemic cycles where voluntary compliance failed to achieve herd immunity thresholds, causing preventable mortality among vulnerable populations and healthcare system collapse during surges.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest the problem remains live citing novel variants and waning immunity. Civil liberties organizations and epidemiologists outside the benefiting parties attest the founding problem was substantially solved by vaccine development itself, and mandates now persist as institutional inertia. Legislative hearings and independent modeling studies support both readings.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   High extractiveness (0.78) reflects that enforcement mechanisms (job loss, exclusion from public life) impose severe costs on the unvaccinated, decoupled from individual risk calculus. Suppression (0.82) is very high because the constraint's persistence depends on active exclusion machinery — verification systems, termination protocols, legal defense — not voluntary participation. Theater ratio (0.38) captures that while epidemiological justification is real, a growing share of enforcement activity serves institutional face-saving and precedent protection rather than marginal health gain. Accessibility collapse (0.68) reflects that once the 'externality imposition' framing is accepted, alternative frameworks (informed consent, proportionality) become structurally invisible in policy discourse. Resistance (0.71) is substantial: litigation, legislative bans, non-compliance, and political realignment all contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_authorities seat, the constraint is a rope: genuine coordination solving a collective action problem with minimal coercive overhead (Jacobson precedent). From the unvaccinated_individuals seat, it is a snare: the coordination story is cover for bodily intrusion, persistence depends on suppressing exits (religious/philosophical exemptions eliminated), identifiable victims exist. From the immunocompromised seat, it is a mountain: natural law of infection dynamics, no alternative exists. The engine computes this divergence; the authored claim (tangled_rope) asserts the structural truth is hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda_setters with institutional power and arbitrage-grade exit (they design the rules, control enforcement budgets, and can move between agencies). Immunocompromised are beneficiaries with powerless/trapped exit — they cannot exit vulnerability. General population are beneficiaries with secondary payer role (organized power, constrained exit). Unvaccinated individuals are primary payers (moderate power, constrained exit — can comply or relocate but not easily). Employers are payers (organized power, constrained exit — regulatory capture prevents opt-out). Courts are observers (analytical exit). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epidemic cycles with voluntary failure) is contested as live vs. solved. If dead, the constraint persists as mandate inertia — classic mandatrophy. The analysis prevents mislabeling by naming the coordination function (real disease suppression) AND the asymmetric extraction (unvaccinated bear disproportionate costs via employment/exclusion). A pure coordination reading would miss the extraction; a pure extraction reading would miss the immunocompromised mortality data. The tangled_rope classification captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the legitimate_health_intervention kernel. What structural elements differ across readings, and where is the disagreement located?',
    'Map each reading''s beneficiary/victim sets, ε referents, and axiom structures. The disagreement is located in: (1) whether individual refusal constitutes externality imposition (this reading) vs. bodily integrity exercise (bodily_autonomy_primary), (2) whether population outcomes are sufficient for legitimacy (this reading) vs. necessary but not sufficient (proportionality_reading).',
    'If the kernel frame is rejected (no shared kernel), these are three independent constraints with no structural relation. If accepted, the reading relations (forecloses/influences) and axiom conflicts drive cross-reading classification dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system framing: kernel identity and reading decomposition').

omega_variable(
    externality_imposition_boundary,
    'Where does the ''externality imposition'' framing stop? Does it extend to booster mandates for low-severity variants? To pediatric mandates for diseases with near-zero pediatric mortality? To non-pharmaceutical interventions (masking, ventilation)?',
    'Track mandate scope over time: if ''externality'' justification expands while measurable population benefit contracts, the framing has become a cover story. Measure the ratio of (marginal population benefit) / (marginal individual cost) across mandate extensions.',
    'If the boundary is unbounded, the constraint drifts from tangled_rope toward snare — coordination function atrophies while extraction persists. If bounded by measurable benefit thresholds, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_imposition_boundary, empirical, 'Whether the externality framing has a structural stopping condition').

omega_variable(
    enforcement_mechanism_proportionality,
    'Are employment termination and venue exclusion proportionate to the marginal transmission reduction achieved, or do they impose costs vastly exceeding the epidemiological benefit?',
    'Compare marginal R_t reduction from mandate enforcement vs. marginal individual cost (lost livelihood, education, mobility). Use natural experiments from jurisdictions that dropped mandates while maintaining high voluntary uptake.',
    'If costs vastly exceed benefits, the constraint''s extraction is not functionally coupled to its coordination — supporting snare reclassification. If proportionate, supports tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_proportionality, empirical, 'Proportionality of enforcement mechanisms to epidemiological effect').

omega_variable(
    suppression_internalization,
    'Is the high suppression (0.82) primarily structural (legal barriers, employment consequences) or partially internalized (unvaccinated individuals self-exclude from social life due to stigma, anticipated rejection, or identity fusion with ''non-complier'' status)?',
    'Post-mandate-lift observation: if exclusion behaviors persist after legal mandates end, internalized component is significant. Survey-based measurement of anticipated stigma vs. actual legal barriers.',
    'If substantially internalized, effective suppression is higher than structural measure suggests — the constraint has colonized the target''s choice architecture. Affects piton vs. tangled_rope trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism in mandate enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__public_health_primary, theater_ratio, 6, 0.18).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.25).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__public_health_primary, theater_ratio, 18, 0.32).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.35).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__public_health_primary, theater_ratio, 30, 0.37).
narrative_ontology:measurement(legi_tr_t36, legitimate_health_intervention__public_health_primary, theater_ratio, 36, 0.38).
narrative_ontology:measurement(legi_tr_t42, legitimate_health_intervention__public_health_primary, theater_ratio, 42, 0.38).
narrative_ontology:measurement(legi_tr_t48, legitimate_health_intervention__public_health_primary, theater_ratio, 48, 0.38).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__public_health_primary, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__public_health_primary, base_extractiveness, 18, 0.74).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__public_health_primary, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(legi_be_t36, legitimate_health_intervention__public_health_primary, base_extractiveness, 36, 0.78).
narrative_ontology:measurement(legi_be_t42, legitimate_health_intervention__public_health_primary, base_extractiveness, 42, 0.78).
narrative_ontology:measurement(legi_be_t48, legitimate_health_intervention__public_health_primary, base_extractiveness, 48, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__public_health_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__public_health_primary, suppression_requirement, 18, 0.81).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__public_health_primary, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(legi_su_t36, legitimate_health_intervention__public_health_primary, suppression_requirement, 36, 0.82).
narrative_ontology:measurement(legi_su_t42, legitimate_health_intervention__public_health_primary, suppression_requirement, 42, 0.82).
narrative_ontology:measurement(legi_su_t48, legitimate_health_intervention__public_health_primary, suppression_requirement, 48, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__public_health_primary, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, emergency_public_health_powers).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, vaccine_injury_compensation_systems).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, school_immunization_requirements).

% DUAL FORMULATION NOTE:
% This constraint, legitimate_health_intervention__bodily_autonomy_primary, and legitimate_health_intervention__proportionality_reading form a constraint family decomposing the 'legitimate health intervention' label. This reading (public_health_primary) has ε=0.78 (high extraction from enforcement). The bodily_autonomy_primary reading would have ε≈0.15 (low extraction, high coordination via voluntary uptake). The proportionality_reading would have intermediate ε≈0.45 depending on disease severity weighting. They share the kernel but instantiate different constraints with different beneficiary/victim structures and different ε values — the ε-invariance principle requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, moderate, 0.85).
constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
