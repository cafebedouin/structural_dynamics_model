% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy: Livelihood Security Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint represents the 'livelihood security' reading of
 *   performance legitimacy, where the state's right to rule is primarily
 *   justified by its ability to deliver tangible improvements in citizens'
 *   daily lives, such as employment, healthcare, education, and elderly care.
 *   This reading prioritizes social welfare and consumption support over
 *   other development metrics, leading to a redirection of state resources.
 *   It is one of several competing interpretations of the broader
 *   'performance legitimacy' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.3).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.4).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy: Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, '3aae140c-9d62-4d4c-8fbd-88e9d1bfa669').
narrative_ontology:cs_kernel_codification('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', implicit).
narrative_ontology:cs_authority_grounding('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', practice).
narrative_ontology:cs_interpretation_layer_present('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669').
narrative_ontology:cs_reading_relation('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', foundational, social_welfare_as_primary_legitimacy_driver).
narrative_ontology:cs_axiom_status(social_welfare_as_primary_legitimacy_driver, holdable).
narrative_ontology:cs_axiom_grounding('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', social_welfare_as_primary_legitimacy_driver, instrumental).
narrative_ontology:cs_axiom('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', foundational, state_as_guarantor_of_daily_life).
narrative_ontology:cs_axiom_status(state_as_guarantor_of_daily_life, holdable).
narrative_ontology:cs_axiom_grounding('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', state_as_guarantor_of_daily_life, conventional).
narrative_ontology:cs_reference_frame('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', social_contract_for_welfare).
narrative_ontology:cs_drift_state('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3aae140c-9d62-4d4c-8fbd-88e9d1bfa669', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, citizens_receiving_services).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, social_service_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, household_consumption).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_expansion).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_spending).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary actor responsible for setting policy priorities and allocating resources. Its legitimacy is directly tied to its ability to deliver tangible improvements in citizens' daily lives, particularly in areas of employment, healthcare, education, and elderly care. It actively enforces policies that prioritize social welfare over other development metrics.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, ruling_party_state, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from increased state investment in social services and a strengthened social safety net. Their satisfaction and perceived well-being are crucial for the ruling party's legitimacy. While they benefit, their ability to exit the system or demand different priorities is constrained by the state's dominant role.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, citizens_receiving_services, beneficiary,
    organized, biographical, constrained, local).

% Experience growth and increased funding as the state prioritizes livelihood security. This includes healthcare providers, educators, and social workers. They align with the state's agenda as it directly supports their expansion and influence.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, social_service_sectors, beneficiary,
    powerful, biographical, mobile, national).

% Faces reduced state support and investment as resources are redirected towards social welfare. These sectors, often focused on heavy industry or large-scale infrastructure, bear the cost of the shift in development priorities. Their ability to influence policy is diminished under this reading.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_expansion, payer,
    powerful, generational, constrained, national).

% Experiences budget cuts and reduced priority for large-scale infrastructure projects, as central government funds are reallocated to social programs. This impacts local development plans and the autonomy of regional authorities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_spending, payer,
    institutional, generational, constrained, regional).

% Monitor the state's development strategies and their impact on human development indicators. They provide analysis and sometimes funding, influencing the discourse around what constitutes legitimate development.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, international_development_organizations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state resources and policy efforts towards improving citizens' daily lives, ensuring a baseline of social welfare and stability across diverse populations, thereby maintaining social cohesion and political legitimacy.
% TRANSFER_FUNCTION: Transfers state resources and policy focus from capital-intensive industrial development and large-scale infrastructure projects towards social service provision (healthcare, education, elderly care) and direct consumption support for households.
% ABSENT_VOICES: Advocates for rapid industrialization, large-scale infrastructure developers, and proponents of 'trickle-down' economic growth models would object, arguing that long-term prosperity requires different investment priorities. Their voices are often marginalized in policy debates dominated by livelihood security concerns.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the ruling party would lose its primary source of legitimacy, potentially leading to social unrest and political instability. Resource allocation would likely shift back towards industrial growth or other priorities, fundamentally altering the state's relationship with its citizens and the structure of its economy.
% FOUNDING_PROBLEM: The state faced challenges of social inequality, inadequate public services, and a perceived disconnect between economic growth metrics and citizens' lived experiences, leading to potential erosion of public trust and political stability.
% FOUNDING_PROBLEM_CORROBORATION: Public opinion surveys, social stability reports, and independent analyses from academic institutions and international organizations consistently corroborate the ongoing importance of livelihood security for maintaining social order and state legitimacy. These sources are outside the direct beneficiaries of the policy.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).
:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Rope because it genuinely coordinates state efforts to improve citizen welfare, with identifiable beneficiaries (citizens, social service sectors). However, it requires active enforcement to redirect resources away from other powerful interests (capital-intensive industries, local infrastructure spending), leading to some extraction from these 'victim' sectors. The extractiveness is moderate (0.3) as the primary goal is coordination for welfare, not pure rent-seeking. Suppression (0.4) is present to ensure compliance with the new priorities. Theater ratio is low (0.2) as the commitment to livelihood security is largely genuine, though some performative elements may exist.
 *
 * PERSPECTIVAL GAP:
 *   The ruling_party_state perceives this as a necessary and beneficial coordination mechanism for social stability. The victim sectors, however, experience it as a constraint that extracts resources and limits their growth, viewing it as a political choice rather than an inevitable coordination. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ruling_party_state acts as the agenda_setter, benefiting from enhanced legitimacy (d near 0.0). Citizens and social service sectors are direct beneficiaries (d near 0.0-0.2). Capital-intensive industrial expansion and local government infrastructure spending are victims, bearing the costs of redirected resources (d near 0.8-1.0). International development organizations are observers, with an analytical perspective (d near 0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    livelihood_security_vs_other_readings,
    'Is the prioritization of livelihood security a genuine, long-term commitment, or a temporary strategy to address immediate social pressures, potentially to be superseded by other performance legitimacy readings (e.g., quantitative growth) once stability is achieved?',
    'Longitudinal analysis of state budget allocations, policy rhetoric, and actual development outcomes over several decades, particularly during periods of economic fluctuation or leadership transition.',
    'If temporary, the constraint''s classification might shift towards a Scaffold (transitional support) or even a Snare (if the ''livelihood'' narrative is primarily cover for consolidating power), with higher extractiveness from other sectors in the long run. If genuine, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(livelihood_security_vs_other_readings, empirical, 'Ambiguity regarding the long-term commitment to livelihood security as the primary basis for legitimacy.').

omega_variable(
    measurement_of_tangible_improvements,
    'How are ''tangible improvements in daily life'' objectively measured and verified, and to what extent do these metrics capture the full lived experience of citizens versus being amenable to political manipulation?',
    'Independent, randomized household surveys and qualitative studies conducted by non-state actors, cross-referenced with official statistics and public feedback mechanisms. Transparency in data collection and reporting.',
    'If metrics are easily manipulated or fail to capture genuine improvements, the theater_ratio would be higher, potentially pushing the constraint towards a Piton (performative maintenance) or a Snare (if the ''performance'' is purely for extraction). If robustly measured, it strengthens the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_tangible_improvements, empirical, 'Uncertainty regarding the objectivity and comprehensiveness of ''tangible improvements'' measurement.').

omega_variable(
    livelihood_security_vs_constructed_necessity,
    'Is the state''s focus on livelihood security a response to an inherent social need, or a constructed necessity designed to centralize power and control over economic development, thereby limiting alternative development paths?',
    'Comparative analysis with states pursuing alternative development models that achieve similar social outcomes with different governance structures. Examination of historical policy choices and the suppression of dissenting economic theories.',
    'If primarily a constructed necessity for power centralization, the constraint''s suppression and extractiveness would be higher, pushing it towards a Tangled Rope or Snare, as the coordination story would be a cover for control. If a genuine response to social need, it reinforces the Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(livelihood_security_vs_constructed_necessity, conceptual, 'Ambiguity between genuine social need and constructed necessity for state control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t2000, performance_legitimacy__livelihood_security_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(perf_tr_t2005, performance_legitimacy__livelihood_security_reading, theater_ratio, 2005, 0.17).
narrative_ontology:measurement(perf_tr_t2010, performance_legitimacy__livelihood_security_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(perf_tr_t2015, performance_legitimacy__livelihood_security_reading, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(perf_tr_t2020, performance_legitimacy__livelihood_security_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(perf_tr_t2025, performance_legitimacy__livelihood_security_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(perf_be_t2000, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(perf_be_t2005, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(perf_be_t2010, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(perf_be_t2015, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2015, 0.27).
narrative_ontology:measurement(perf_be_t2020, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2020, 0.29).
narrative_ontology:measurement(perf_be_t2025, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2025, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t2000, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(perf_su_t2005, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2005, 0.32).
narrative_ontology:measurement(perf_su_t2010, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(perf_su_t2015, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(perf_su_t2020, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2020, 0.39).
narrative_ontology:measurement(perf_su_t2025, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel, focusing on livelihood security. It structurally influences and is influenced by other readings of the same kernel, as resource allocation and policy priorities are finite and competing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
