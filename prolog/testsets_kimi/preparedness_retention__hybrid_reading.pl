% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness: Core Competence with Ceremonial Periphery
 *   domain: governance/institutional_memory/disaster_preparedness
 *
 * SUMMARY:
 *   In Dutch disaster preparedness governance, technical competence has
 *   concentrated in specialized institutions â Rijkswaterstaat and regional
 *   water authorities â while broader societal memory and municipal
 *   capacity have become largely ceremonial. The constraint presents itself
 *   as necessary professional coordination in a complex delta environment.
 *   The hybrid reading of the preparedness_retention kernel claims this is
 *   not universal competence nor universal husk, but a stratified dual-track:
 *   genuine expertise in the core, performative retention in the periphery.
 *   This reading is one of three in a contested kernel; siblings include
 *   competence_reading (universal live knowledge) and husk_reading (universal
 *   memorial performance).
 *
 * KEY AGENTS:
 *   - Rijkswaterstaat: Primary agenda setter (institutional/constrained) â maintains live national technical competence and decides on critical infrastructure.
 *   - Regional water authorities: Agenda setter and beneficiary (institutional/constrained) â operate regional systems with real capacity and concentrated mandate.
 *   - Municipal emergency services: Primary payer (moderate/constrained) â perform drills and plans as compliance theater with upward-migrated authority.
 *   - General public: Payer (powerless/trapped) â depend on centralized protection, participate in ritual preparedness, bear catastrophic risk.
 *   - Local communities: Payer (moderate/constrained) â historically held water knowledge, now displaced and bearing loss of autonomous resilience.
 *   - Distributed resilience advocates: Excluded (moderate/constrained) â argue for community-based preparedness but absent from core planning.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.62).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.55).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Disaster Preparedness: Core Competence with Ceremonial Periphery").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/institutional_memory/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '60ad7eb4-8daf-4100-9190-0cf2a3ec125c').
narrative_ontology:cs_kernel_codification('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', formalized).
narrative_ontology:cs_authority_grounding('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', practice).
narrative_ontology:cs_interpretation_layer_present('60ad7eb4-8daf-4100-9190-0cf2a3ec125c').
narrative_ontology:cs_reading_relation('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_axiom('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', foundational, competence_is_stratified_not_general).
narrative_ontology:cs_axiom_status(competence_is_stratified_not_general, holdable).
narrative_ontology:cs_axiom_grounding('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', competence_is_stratified_not_general, empirically_contingent).
narrative_ontology:cs_axiom('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', foundational, institutional_continuity_over_distributed_resilience).
narrative_ontology:cs_axiom_status(institutional_continuity_over_distributed_resilience, holdable).
narrative_ontology:cs_axiom_grounding('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', institutional_continuity_over_distributed_resilience, instrumental).
narrative_ontology:cs_reference_frame('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', stratified_preparedness_state).
narrative_ontology:cs_drift_state('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', contemporary_ceremonial_periphery, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60ad7eb4-8daf-4100-9190-0cf2a3ec125c', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, rijkswaterstaat).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, regional_water_authorities).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, central_government).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, municipal_emergency_services).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, general_public).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains live technical competence in national flood defense and water infrastructure. Designs, operates, and decides under real accountability for catastrophic outcomes. The expertise is exercised and consequential, but its concentration creates structural dependency throughout the system.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, rijkswaterstaat, agenda_setter,
    institutional, generational, constrained, national).

% Operate regional water systems with genuine exercised technical capacity. Hold constitutional mandates and professional expertise. Benefit from concentrated authority and guaranteed funding while performing a real coordination function for their catchments.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, regional_water_authorities, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, regional_water_authorities, beneficiary).

% Funds and legitimizes specialized preparedness institutions. Benefits from institutional continuity and the political manageability of centralized, predictable crisis response. Could theoretically reform the distribution of competence but does not.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, central_government, beneficiary,
    institutional, generational, mobile, national).

% Conduct drills, file plans, and participate in coordination networks. Their activities increasingly approximate compliance theater because actual decision-capacity for major events has migrated upward to specialized agencies. Bear the cost of maintaining the appearance of preparedness without the authority to act decisively.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, municipal_emergency_services, payer,
    moderate, biographical, constrained, local).

% Participate in drills and trust state water management as civic ritual rather than lived practice. Have lost the distributed competence and local water knowledge that once characterized Dutch communities. Bear the catastrophic risk if centralized systems fail.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, general_public, payer,
    powerless, biographical, trapped, national).

% Once maintained lived relationships with water landscapes and local early-warning practices. Have ceded this capacity to specialized institutions over generations. Bear the loss of autonomous resilience capacity and the risk of institutional single-point failure.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, local_communities, payer,
    moderate, generational, constrained, local).

% Argue for community-based preparedness, local knowledge retention, and distributed decision-making. Structurally absent from core preparedness planning and resource allocation because funding and authority flow exclusively to specialized institutions.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, distributed_resilience_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scarce technical expertise in dedicated institutions capable of managing complex water infrastructure and coordinating large-scale flood response across a densely populated delta.
% TRANSFER_FUNCTION: Moves authority, funding, and decision-capacity from local communities and municipal actors to specialized national and regional water institutions; moves risk of catastrophic failure from distributed networks to centralized system integrity.
% ABSENT_VOICES: Distributed resilience advocates, local ecological knowledge holders, and community-based disaster planners are structurally absent from core preparedness design; their exclusion is what allows the ceremonial periphery to be mistaken for functional capacity.
% DISAPPEARANCE_RATIONALE: If the stratified constraint vanished, municipalities and communities would need to rebuild distributed competence rapidly or face unmitigated exposure. Central institutions would lose their monopoly on legitimate expertise. The division of labor between ceremonial periphery and competent core would collapse and the flood-risk governance landscape would reorganize.
% FOUNDING_PROBLEM: Managing complex water infrastructure and flood risk in a densely populated delta requires specialized engineering knowledge that cannot be assumed to exist universally across all communities and municipalities.
% FOUNDING_PROBLEM_CORROBORATION: Disaster sociologists and historical ecologists outside the benefiting institutions attest that Dutch water communities historically maintained distributed competence; the founding problem of universal technical incapacity is contested by evidence of prior distributed capacity systematically displaced by professionalization.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial transfer of authority and risk-bearing from distributed actors to centralized institutions, bounded below 1.0 by the genuine coordination function in the core. Suppression (0.55) reflects the collapse of alternative distributed preparedness infrastructures and the channeling of all legitimate expertise into the specialized track. Theater_ratio (0.70) is high because the periphery â municipal drills, public engagement, broader societal memory â is substantially performative relative to actual decision capacity. Accessibility_collapse (0.75) is high because once the stratification is understood, alternatives (distributed competence) appear historically closed off rather than currently available. Resistance (0.40) is moderate: excluded advocates and some municipalities register dissent, but the institutionalized core and public dependency absorb it. The measurement series tracks the historical drift from distributed competence to stratified ceremony on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seats (specialized institutions) experience the constraint as necessary professional coordination with unfortunate but manageable ceremonial overhead in peripheral partners. The payer seats (municipalities, public) experience it as structural infantilization where their role is reduced to validating institutional monopoly. The excluded seats (resilience advocates) experience it as a false summit â a system claiming societal preparedness while actually concentrating catastrophic risk. The engine computes this divergence from the structural data rather than adjudicating whose experience is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Rijkswaterstaat and regional_water_authorities are structural beneficiaries: the constraint subsidizes their authority, budget, and epistemic monopoly (low d). Central_government is a secondary beneficiary of political predictability (low d). Municipal_emergency_services and local_communities are structural targets: the constraint extracts their former autonomy and loads them with ceremonial obligations (high d). The general_public sits near full target because their exit options are structurally trapped â they have no alternative flood protection and their participation is reduced to ritual consent (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling this as either pure coordination (rope) or pure extraction (snare). The genuine coordination function in the specialized core is structurally real: Rijkswaterstaat and water boards maintain exercised technical capacity that prevents flooding. However, the asymmetric extraction is equally real: the same concentration that enables coordination disables distributed resilience, converting broader societal memory to ceremony. Mandatrophy would occur if the founding problem (universal lack of technical capacity) were misapplied to justify perpetual stratification even after the periphery has lost all meaningful function. The R5 genealogy shows the founding problem is contested: outside corroboration suggests distributed competence once existed and was displaced, indicating the arrangement may persist beyond its functional justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Is the stratification described by the hybrid reading an empirical feature of Dutch preparedness, or an institutional narrative that legitimates concentration by admitting peripheral dysfunction?',
    'Comparative ethnography of core and periphery preparedness practices, coupled with drill outcome measurement across municipal versus specialized institutional actors.',
    'If the stratification is narrative rather than empirical, the constraint''s epsilon is higher than authored and the core is also performative; if empirical, the authored epsilon and dual-track structure are validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether the hybrid reading describes real stratification or institutional apologia.').

omega_variable(
    core_periphery_boundary_leakage,
    'Does ceremonial practice in the periphery leak into and degrade core technical competence over generational turnover?',
    'Longitudinal analysis of decision quality in specialized institutions across cohorts, controlling for infrastructure complexity.',
    'If leakage occurs, the constraint drifts toward husk_reading over time (higher theater_ratio, lower genuine coordination); if sealed, the dual-track is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_periphery_boundary_leakage, empirical, 'Whether periphery ceremony infects core competence over time.').

omega_variable(
    distributed_resilience_feasibility,
    'Could distributed community-based preparedness achieve acceptable outcomes for Dutch flood risk, or is geographic and technical complexity a genuine mountain requiring centralized expertise?',
    'Historical counterfactual analysis of pre-professionalization water governance, coupled with comparative case studies of distributed flood management in analogous delta regions.',
    'If distributed resilience is viable, the constraint''s extraction is discretionary rather than necessary; if complexity is a genuine mountain, part of the extraction is the unavoidable price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_resilience_feasibility, empirical, 'Whether distributed resilience is technically viable or a fantasy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__hybrid_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__hybrid_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__hybrid_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__hybrid_reading, theater_ratio, 40, 0.66).
narrative_ontology:measurement(prep_tr_t50, preparedness_retention__hybrid_reading, theater_ratio, 50, 0.7).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__hybrid_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__hybrid_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__hybrid_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__hybrid_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(prep_be_t50, preparedness_retention__hybrid_reading, base_extractiveness, 50, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the preparedness_retention kernel, which decomposes into three structurally distinct claims: competence_reading (universal live knowledge), husk_reading (universal memorial performance), and hybrid_reading (stratified core/periphery). Each has distinct epsilon, beneficiary/victim structure, and empirical status. They form a constraint family linked via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
