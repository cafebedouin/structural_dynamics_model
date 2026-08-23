% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Proportionality Reading: Coercion Legitimacy Scales with Disease Severity and Transmission
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The proportionality reading of the coercion legitimacy boundary holds
 *   that state compulsion for medical intervention is legitimate only when
 *   the disease's severity (mortality, morbidity) and transmission dynamics
 *   (R0, mode of spread) cross a threshold that makes voluntary measures
 *   insufficient. Measles (R0 12-18, high complications) justifies
 *   school-entry mandates; seasonal influenza (R0 1.3, low mortality in
 *   general population) does not. This reading instantiates a case-by-case
 *   adjudication structure where each pathogen is evaluated against
 *   proportionality criteria (necessity, least restrictive means, balancing).
 *   The extraction is moderate (0.42) because the constraint operates
 *   selectively — it extracts from mandate subjects for high-threat diseases
 *   but denies extraction for low-threat diseases. Suppression is moderate
 *   (0.38) because enforcement is active (school exclusion, employment
 *   conditions) but bounded by judicial review. Theater is present (0.28)
 *   because the proportionality analysis can become performative — elaborate
 *   frameworks that consistently validate authority preferences.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.42).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.38).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionality Reading: Coercion Legitimacy Scales with Disease Severity and Transmission").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '140f2be7-a4a3-443c-9eb7-fecd900da3d3').
narrative_ontology:cs_kernel_codification('140f2be7-a4a3-443c-9eb7-fecd900da3d3', fixed_text).
narrative_ontology:cs_authority_grounding('140f2be7-a4a3-443c-9eb7-fecd900da3d3', lineage).
narrative_ontology:cs_interpretation_layer_present('140f2be7-a4a3-443c-9eb7-fecd900da3d3').
narrative_ontology:cs_reading_relation('140f2be7-a4a3-443c-9eb7-fecd900da3d3', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('140f2be7-a4a3-443c-9eb7-fecd900da3d3', coercion_legitimacy_boundary__public_health_primary, influences).
narrative_ontology:cs_axiom('140f2be7-a4a3-443c-9eb7-fecd900da3d3', foundational, coercion_requires_proportional_threat).
narrative_ontology:cs_axiom_status(coercion_requires_proportional_threat, holdable).
narrative_ontology:cs_axiom_grounding('140f2be7-a4a3-443c-9eb7-fecd900da3d3', coercion_requires_proportional_threat, deontological).
narrative_ontology:cs_axiom('140f2be7-a4a3-443c-9eb7-fecd900da3d3', foundational, least_restrictive_means_applies_to_mandates).
narrative_ontology:cs_axiom_status(least_restrictive_means_applies_to_mandates, holdable).
narrative_ontology:cs_axiom_grounding('140f2be7-a4a3-443c-9eb7-fecd900da3d3', least_restrictive_means_applies_to_mandates, conventional).
narrative_ontology:cs_reference_frame('140f2be7-a4a3-443c-9eb7-fecd900da3d3', jacobson_proportionality_framework).
narrative_ontology:cs_drift_state('140f2be7-a4a3-443c-9eb7-fecd900da3d3', post_covid_emergency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('140f2be7-a4a3-443c-9eb7-fecd900da3d3', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, mandate_subjects_high_r0).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, mandate_subjects_low_r0).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, least_restrictive_means_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, implement, and enforce disease-specific mandates (school-entry requirements, healthcare worker mandates, emergency orders). Their legitimacy rests on demonstrating that each mandate is proportionate to the specific threat. They control the case-by-case adjudication process and define the metrics (R0, IFR, transmission dynamics) that trigger coercion.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Immunocompromised individuals, infants too young for vaccination, and others who rely on herd immunity. They benefit from mandates that achieve high coverage for high-R0 diseases (measles) but gain little from mandates for low-severity diseases (flu) where mandates are not justified. Their protection varies by pathogen.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations, beneficiary,
    organized, biographical, constrained, national).

% Parents of school-age children, healthcare workers, and others subject to mandates for high-R0/high-mortality diseases (measles, polio). They bear the autonomy cost of compelled vaccination but face high exclusion costs (school exclusion, job loss) for noncompliance. The constraint extracts from them, but the proportionality reading treats this extraction as justified by the severity of the collective threat.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, mandate_subjects_high_r0, payer,
    moderate, biographical, constrained, national).

% Individuals who would be subject to mandates for low-severity diseases (seasonal influenza, COVID-19 post-emergency) if such mandates existed. Under the proportionality reading, they are NOT currently extracted from — the reading denies legitimacy to such mandates. Their inclusion as payers reflects the structural position they would occupy if the reading were displaced by public_health_primary.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, mandate_subjects_low_r0, payer,
    moderate, biographical, mobile, national).

% Organizations and individuals who hold that medical intervention without consent is categorically impermissible. They are excluded from the proportionality adjudication because their premise (categorical prohibition) is treated as foreclosed by the framework. They would object to any mandate, including measles.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, generational, trapped, national).

% Adjudicate challenges to specific mandates under the proportionality framework (strict scrutiny, least restrictive means, Jacobson v. Massachusetts lineage). They do not set policy but define the legal boundaries within which the proportionality reading operates. Their rulings determine which diseases meet the threshold.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, courts_constitutional_law, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of achieving herd immunity for diseases where voluntary uptake is insufficient to prevent outbreaks, while avoiding unnecessary coercion for diseases where the collective threat does not justify the autonomy intrusion.
% TRANSFER_FUNCTION: Moves autonomy/body-integrity interests from mandate_subjects (who bear compelled vaccination) to vulnerable_populations and public_health_authorities (who gain outbreak prevention and institutional legitimacy). The transfer is calibrated per pathogen: large for measles, near-zero for flu.
% ABSENT_VOICES: Bodily autonomy absolutists who reject any calibration — they are structurally excluded because the proportionality framework treats categorical refusal as outside the legitimate range of debate. Also excluded: populations in jurisdictions without proportionality review where mandates are imposed by decree without case-by-case justification.
% DISAPPEARANCE_RATIONALE: If the proportionality reading vanished, the boundary between justified and unjustified mandates would collapse. Either public_health_primary would expand mandates to flu/COVID without individualized justification (increasing extraction on mandate_subjects_low_r0), or bodily_autonomy_primary would eliminate measles mandates (increasing risk to vulnerable_populations). The case-by-case adjudication infrastructure would dissolve.
% FOUNDING_PROBLEM: The Jacobson v. Massachusetts (1905) framework left an open question: how to distinguish diseases that justify compulsion from those that do not. The proportionality reading was built to answer that question without collapsing into either categorical authorization or categorical prohibition.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars outside the public health establishment (e.g., Gostin, Jacobson, contemporary law review literature) attest that the proportionality boundary remains contested and live. Public health authorities attest it is live but argue the threshold is met for expanding disease sets. No single tradition has settled it.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).
:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate because the constraint's victim set is pathogen-contingent: high for measles mandates, near-zero for flu. The case-by-case structure means the constraint does not universally extract; it calibrates. Suppression is lower than a categorical mandate because exit options exist (home schooling, religious exemptions in some jurisdictions, geographic mobility) and judicial review provides a check. Theater ratio reflects the risk that proportionality analysis becomes a ritual that always concludes the mandate is justified — the 2020 spike reflects pandemic emergency frameworks that dispensed with individualized proportionality review.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_authorities seat, the constraint is a rope — genuine coordination solving a collective action problem with minimal necessary coercion. From the mandate_subjects_high_R0 seat, it is a tangled_rope — they are coordinated (protected from outbreaks) but pay the autonomy cost. From the bodily_autonomy_advocates seat (excluded), it would read as a snare — any coercion is extraction. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda_setters with arbitrage-grade exit (they design the framework). Vulnerable populations are beneficiaries with constrained exit (they cannot individually secure herd immunity). Mandate_subjects_high_R0 are payers with constrained exit (school/job exclusion costs are high). Mandate_subjects_low_R0 are payers with mobile exit (they are not currently coerced; the reading protects them). Bodily autonomy advocates are excluded (their categorical premise is foreclosed by the framework). Courts are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing justified from unjustified mandates) remains live. The constraint has not atrophied into a piton because the boundary is actively litigated (COVID mandates, measles outbreaks, flu mandate proposals). However, the rising theater_ratio and the 2020 spike in suppression_requirement suggest drift toward performative proportionality — frameworks that claim case-by-case review but functionally rubber-stamp. This is a mandatrophy risk, not a resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_location,
    'Where exactly does the proportionality threshold fall between measles (clearly justified) and flu (clearly not)? What about COVID-19, pertussis, mumps, rubella?',
    'Systematic coding of judicial decisions, legislative mandates, and public health recommendations across jurisdictions and time, mapping each disease''s epidemiological parameters to mandate status.',
    'If the threshold is de facto elastic (expanding to include COVID, then perhaps RSV, then perhaps flu), the reading converges toward public_health_primary. If the threshold holds at measles-level severity, the reading maintains its distinguishing structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_location, empirical, 'Whether the proportionality boundary is stable or drifting toward categorical authorization').

omega_variable(
    adjudication_performativeness,
    'Does the case-by-case proportionality analysis genuinely constrain authority, or is it a ritual that produces the authority''s preferred outcome?',
    'Compare proportionality analyses for mandates that were upheld vs. struck down; measure independence of the reviewing body; track whether any proposed mandate has been rejected on proportionality grounds in the last 20 years.',
    'If performative, the constraint''s theater_ratio is understated and its effective type drifts toward snare (extraction disguised as coordination). If genuinely constraining, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adjudication_performativeness, conceptual, 'Whether proportionality review is structural or theatrical').

omega_variable(
    kernel_reading_relations,
    'How does the proportionality reading structurally relate to the public_health_primary and bodily_autonomy_primary readings of the same kernel?',
    'Analyze whether any legal framework can simultaneously hold the proportionality reading and a sibling reading, or whether adoption of one logically commits to rejection of the others.',
    'If proportionality forecloses bodily_autonomy_primary (as the framework treats categorical refusal as outside legitimate debate), that is a structural foreclosure. If it merely coexists with public_health_primary as a higher threshold, the relation is influences. This determines the reading_relations in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relations between sibling readings of the coercion_legitimacy_boundary kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 1905, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t1905, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1905, 0.1).
narrative_ontology:measurement(coer_tr_t1950, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(coer_tr_t1977, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(coer_tr_t1990, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(coer_tr_t2005, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(coer_tr_t2015, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(coer_tr_t2020, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2020, 0.32).
narrative_ontology:measurement(coer_tr_t2025, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(coer_be_t1905, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1905, 0.15).
narrative_ontology:measurement(coer_be_t1950, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(coer_be_t1977, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1977, 0.28).
narrative_ontology:measurement(coer_be_t1990, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(coer_be_t2005, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(coer_be_t2015, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(coer_be_t2020, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(coer_be_t2025, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t1905, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1905, 0.25).
narrative_ontology:measurement(coer_su_t1950, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(coer_su_t1977, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1977, 0.32).
narrative_ontology:measurement(coer_su_t1990, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(coer_su_t2005, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(coer_su_t2015, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(coer_su_t2020, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement(coer_su_t2025, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__proportionality_reading, 0.1).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, school_entry_vaccine_mandates).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, healthcare_worker_vaccine_mandates).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, emergency_use_authorization_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the coercion_legitimacy_boundary kernel. The public_health_primary reading has higher extractiveness (broader victim set) and lower suppression (less need for case-by-case justification). The bodily_autonomy_primary reading has near-zero extractiveness for mandates but creates a different victim set (the unprotected). The three readings form a constraint family linked by mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__proportionality_reading, institutional, 0.15).
constraint_indexing:directionality_override(coercion_legitimacy_boundary__proportionality_reading, moderate, 0.75).
constraint_indexing:directionality_override(coercion_legitimacy_boundary__proportionality_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
