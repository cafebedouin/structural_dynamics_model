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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint story instantiates the 'livelihood security' reading of
 *   the broader 'performance legitimacy' kernel. It describes a state's
 *   commitment to grounding its legitimacy in delivering tangible
 *   improvements in citizens' daily lives, specifically focusing on
 *   employment, healthcare, education, and elderly care. This involves active
 *   state planning and resource allocation, often at the expense of other
 *   development priorities like raw industrial growth or technological
 *   self-sufficiency. The constraint functions as a Tangled Rope,
 *   coordinating social welfare and stability for citizens while extracting
 *   resources from other sectors to fund these priorities, requiring active
 *   enforcement to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.5).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.6).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy: Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, '130c857f-0e66-4eeb-8bc3-800b26251fa5').
narrative_ontology:cs_kernel_codification('130c857f-0e66-4eeb-8bc3-800b26251fa5', formalized).
narrative_ontology:cs_authority_grounding('130c857f-0e66-4eeb-8bc3-800b26251fa5', practice).
narrative_ontology:cs_interpretation_layer_present('130c857f-0e66-4eeb-8bc3-800b26251fa5').
narrative_ontology:cs_reading_relation('130c857f-0e66-4eeb-8bc3-800b26251fa5', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('130c857f-0e66-4eeb-8bc3-800b26251fa5', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('130c857f-0e66-4eeb-8bc3-800b26251fa5', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('130c857f-0e66-4eeb-8bc3-800b26251fa5', foundational, social_stability_through_welfare).
narrative_ontology:cs_axiom_status(social_stability_through_welfare, holdable).
narrative_ontology:cs_axiom_grounding('130c857f-0e66-4eeb-8bc3-800b26251fa5', social_stability_through_welfare, conventional).
narrative_ontology:cs_axiom('130c857f-0e66-4eeb-8bc3-800b26251fa5', foundational, state_responsibility_for_basic_needs).
narrative_ontology:cs_axiom_status(state_responsibility_for_basic_needs, holdable).
narrative_ontology:cs_axiom_grounding('130c857f-0e66-4eeb-8bc3-800b26251fa5', state_responsibility_for_basic_needs, deontological).
narrative_ontology:cs_reference_frame('130c857f-0e66-4eeb-8bc3-800b26251fa5', social_welfare_state_building).
narrative_ontology:cs_drift_state('130c857f-0e66-4eeb-8bc3-800b26251fa5', contemporary_global_economy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('130c857f-0e66-4eeb-8bc3-800b26251fa5', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, citizens).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, elderly_care_recipients).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, healthcare_recipients).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, education_recipients).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industries).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_spending).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for formulating and enforcing policies that prioritize social welfare, employment, healthcare, education, and elderly care. They direct state resources and set development priorities to ensure tangible improvements in citizens' daily lives, thereby grounding state legitimacy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, state_planning_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Directly experience the benefits of improved social services, employment opportunities, and social safety nets. Their satisfaction and perceived well-being are central to the state's legitimacy under this reading. They bear indirect costs through taxation or foregone alternative development paths.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, citizens, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from increased state investment and demand for social services (healthcare, education, elderly care). They are key implementers of the livelihood security agenda and experience growth and stability through state prioritization.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sectors, beneficiary,
    organized, biographical, mobile, national).

% Directly receive state-provided or subsidized elderly care services, which are critical for their well-being and quality of life. Their dependence on these services makes their exit options limited.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, elderly_care_recipients, beneficiary,
    powerless, immediate, trapped, local).

% Benefit from accessible and affordable healthcare services, a core component of livelihood security. Their health outcomes are a direct measure of the constraint's effectiveness.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, healthcare_recipients, beneficiary,
    powerless, immediate, trapped, local).

% Receive state-supported education, which improves their future prospects and contributes to social mobility. Their access to quality education is a key indicator of livelihood security.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, education_recipients, beneficiary,
    powerless, biographical, trapped, local).

% Bear the costs of resource redirection, potentially facing lower investment priority, higher taxation, or stricter environmental/labor regulations as the state prioritizes social welfare over raw industrial expansion. Their growth might be constrained by this policy focus.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industries, payer,
    powerful, biographical, constrained, national).

% May experience reduced central government funding for large-scale infrastructure projects or other non-welfare-related initiatives, as resources are reallocated to social spending. They are tasked with implementing welfare policies, often with limited autonomy over resource allocation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_governments, payer,
    organized, biographical, constrained, national).

% Analyze the state's development model, assessing its effectiveness in delivering social welfare and its implications for economic growth, human rights, and political stability. They provide external corroboration or critique of the state's claims.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure basic welfare, social stability, and public trust by directing state resources and policy efforts towards essential services (employment, healthcare, education, elderly care) and consumption support, thereby preventing social unrest and fostering a sense of shared prosperity.
% TRANSFER_FUNCTION: Transfers state budget, policy attention, and human capital from areas like capital-intensive industrial expansion and large-scale infrastructure projects towards social welfare programs, service sectors, and direct household consumption, effectively prioritizing social outcomes over raw economic output.
% ABSENT_VOICES: Advocates for pure market liberalization, unfettered capital accumulation, or rapid industrialization without robust social safety nets are structurally marginalized. They would argue for different development priorities but are excluded by the state's overarching commitment to social stability and welfare as the primary basis for legitimacy.
% DISAPPEARANCE_RATIONALE: If this commitment to livelihood security vanished overnight, the state would likely re-prioritize other forms of development (e.g., raw GDP growth, industrial output, or technological self-sufficiency). This would lead to reduced social spending, increased inequality, potential social unrest, and a fundamental shift in the social contract and economic structure, as the state's legitimacy would need to be re-grounded.
% FOUNDING_PROBLEM: To address social inequality, widespread poverty, and potential instability arising from rapid, uneven economic development, ensuring that the benefits of growth are broadly shared and basic needs are met for the populace.
% FOUNDING_PROBLEM_CORROBORATION: Independent social scientists, international development organizations, and citizen surveys consistently highlight the ongoing importance of livelihood security for social stability and public satisfaction. Their findings, from outside the immediate benefiting parties, corroborate the state's stated founding problem and its continued relevance.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.5, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.50) reflects the significant resource redirection and prioritization required to fund comprehensive social welfare programs, which implicitly extracts from other potential investment areas. Suppression (0.60) is moderate because the state actively manages and limits alternative development paths (e.g., unfettered capital-intensive growth) to ensure resources flow to livelihood security. The theater ratio (0.15) is low, as the constraint's effectiveness is directly tied to observable, tangible improvements in citizens' lives, leaving little room for purely performative maintenance. The measurement series shows a gradual increase in extractiveness and suppression over the interval, reflecting a deepening commitment to this development model and the increasing costs of maintaining it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state planning authorities and citizens, this constraint is a vital coordination mechanism for social stability and welfare. However, from the perspective of capital-intensive industries and local governments, it represents a form of state-directed extraction and suppression of alternative development priorities. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State planning authorities are the agenda-setters, directing policy and resources. Citizens and service sectors are the primary beneficiaries, directly receiving or implementing welfare provisions. Capital-intensive industries and local governments are the payers, bearing the costs of resource reallocation and potentially constrained growth. International observers provide an analytical perspective. The directionality for beneficiaries is low (subsidized), while for payers it is high (extracted from), consistent with a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of performance legitimacy is designed to prevent mandatrophy by continuously adapting to and addressing the evolving needs of citizens for livelihood security. However, it risks becoming a snare if the state uses welfare provision as a cover for suppressing political dissent, maintaining inefficient resource allocation, or if the costs to other sectors become unsustainable without genuine public benefit. The ongoing 'live' status of the founding problem and the 'world_rearranges' disappearance verdict suggest it is still functionally active, but the moderate extractiveness and suppression warrant careful monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_of_redistribution,
    'Is the state''s resource redirection and redistribution mechanism efficient in delivering livelihood security, or does it create new inefficiencies and unintended extraction?',
    'Independent audits of state welfare programs, comparative studies of resource allocation models, and longitudinal analysis of social welfare outcomes versus input costs.',
    'If inefficient, the effective extraction from payer seats is higher than measured, and the coordination function for beneficiaries is less effective, potentially shifting the classification towards a Snare or a more extractive Tangled Rope. If highly efficient, the coordination function is stronger, justifying the redistribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_of_redistribution, empirical, 'Assesses the actual efficiency of state-led welfare resource allocation.').

omega_variable(
    legitimacy_vs_control,
    'Is the state''s commitment to livelihood security primarily driven by a genuine desire for citizen well-being, or is it a strategic means to maintain political control and suppress dissent?',
    'Analysis of political freedoms, civil liberties, and alternative channels for citizen participation alongside welfare provision. If welfare is coupled with severe political repression, the control aspect is dominant.',
    'If primarily a control mechanism, the constraint''s suppression is effectively higher and its coordination function is more theatrical, pushing it closer to a Snare. If genuine, it reinforces the Tangled Rope classification with a stronger coordination component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_vs_control, conceptual, 'Distinguishes genuine welfare commitment from instrumental use for political control.').

omega_variable(
    global_economic_pressure_interaction,
    'How does this livelihood security reading interact with and adapt to global economic pressures for market liberalization, export-led growth, and technological competition?',
    'Comparative analysis of policy shifts in response to international economic crises or trade agreements. Examination of internal debates within state planning authorities regarding balancing domestic welfare with global competitiveness.',
    'If global pressures force significant compromises on livelihood security, the constraint''s stability is challenged, potentially leading to a decline in its coordination function and increased internal resistance. If it successfully buffers against these pressures, its resilience is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_economic_pressure_interaction, empirical, 'Examines the resilience of livelihood security commitment against external economic forces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t1990, performance_legitimacy__livelihood_security_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(perf_tr_t1996, performance_legitimacy__livelihood_security_reading, theater_ratio, 1996, 0.11).
narrative_ontology:measurement(perf_tr_t2002, performance_legitimacy__livelihood_security_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement(perf_tr_t2008, performance_legitimacy__livelihood_security_reading, theater_ratio, 2008, 0.13).
narrative_ontology:measurement(perf_tr_t2014, performance_legitimacy__livelihood_security_reading, theater_ratio, 2014, 0.14).
narrative_ontology:measurement(perf_tr_t2020, performance_legitimacy__livelihood_security_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(perf_be_t1990, performance_legitimacy__livelihood_security_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(perf_be_t1996, performance_legitimacy__livelihood_security_reading, base_extractiveness, 1996, 0.38).
narrative_ontology:measurement(perf_be_t2002, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2002, 0.41).
narrative_ontology:measurement(perf_be_t2008, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2008, 0.44).
narrative_ontology:measurement(perf_be_t2014, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2014, 0.47).
narrative_ontology:measurement(perf_be_t2020, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2020, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t1990, performance_legitimacy__livelihood_security_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(perf_su_t1996, performance_legitimacy__livelihood_security_reading, suppression_requirement, 1996, 0.48).
narrative_ontology:measurement(perf_su_t2002, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2002, 0.51).
narrative_ontology:measurement(perf_su_t2008, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2008, 0.54).
narrative_ontology:measurement(perf_su_t2014, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2014, 0.57).
narrative_ontology:measurement(perf_su_t2020, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'performance_legitimacy' kernel. Each reading represents a different structural commitment and set of priorities for grounding state legitimacy, leading to different ε values and classifications. They are linked to show their interdependencies within the broader concept of state performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
