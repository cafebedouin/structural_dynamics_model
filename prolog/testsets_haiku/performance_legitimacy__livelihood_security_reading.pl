% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy via Livelihood Security (Service Delivery Reading)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel: the
 *   legitimacy basis of a developmental state. The
 *   livelihood_security_reading grounds legitimacy in tangible improvements
 *   citizens directly experience in daily life — employment in service
 *   sectors, access to healthcare, education enrollment, elderly care
 *   support. This reading contrasts with three siblings: the
 *   quantitative_growth_reading (legitimacy via GDP expansion rates), the
 *   qualitative_development_reading (legitimacy via innovation and efficiency
 *   gains), and the techno_nationalist_reading (legitimacy via strategic
 *   industrial capacity and technological autonomy). Each reading
 *   instantiates different ε values, different beneficiary/victim structures,
 *   and different constraint operations. This file describes ONLY the
 *   livelihood-security reading.
 *
 * KEY AGENTS:
 *   - service_sector_workers: Primary beneficiary and organized political base; employment expansion anchors the constraint
 *   - household_consumption_base: Direct service beneficiaries; visible daily-life improvements sustain legitimacy claims
 *   - social_redistribution_coalitions: Institutional agenda-setters; state ministries and planning bodies that administer budget prioritization
 *   - capital_intensive_industrial_sectors: Primary victims; capital diverted from their expansion constrains their growth trajectory
 *   - local_government_infrastructure_budgets: Secondary victims; deferred maintenance and constrained capital for local projects
 *   - quantitative_growth_advocates and techno_nationalist_strategists: Structurally excluded; their priorities are deprioritized by this reading's logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.62).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.38).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy via Livelihood Security (Service Delivery Reading)").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, 'f803d9c4-12fb-4887-ae11-cd2caee6a78f').
narrative_ontology:cs_kernel_codification('f803d9c4-12fb-4887-ae11-cd2caee6a78f', distributed).
narrative_ontology:cs_authority_grounding('f803d9c4-12fb-4887-ae11-cd2caee6a78f', extraction).
narrative_ontology:cs_interpretation_layer_present('f803d9c4-12fb-4887-ae11-cd2caee6a78f').
narrative_ontology:cs_reading_relation('f803d9c4-12fb-4887-ae11-cd2caee6a78f', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('f803d9c4-12fb-4887-ae11-cd2caee6a78f', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('f803d9c4-12fb-4887-ae11-cd2caee6a78f', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('f803d9c4-12fb-4887-ae11-cd2caee6a78f', foundational, legitimacy_grounded_in_livelihood_tangibility).
narrative_ontology:cs_axiom_status(legitimacy_grounded_in_livelihood_tangibility, holdable).
narrative_ontology:cs_axiom_grounding('f803d9c4-12fb-4887-ae11-cd2caee6a78f', legitimacy_grounded_in_livelihood_tangibility, deontological).
narrative_ontology:cs_axiom('f803d9c4-12fb-4887-ae11-cd2caee6a78f', foundational, service_delivery_priority_over_capital_accumulation).
narrative_ontology:cs_axiom_status(service_delivery_priority_over_capital_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('f803d9c4-12fb-4887-ae11-cd2caee6a78f', service_delivery_priority_over_capital_accumulation, instrumental).
narrative_ontology:cs_reference_frame('f803d9c4-12fb-4887-ae11-cd2caee6a78f', service_delivery_legitimacy_framework).
narrative_ontology:cs_drift_state('f803d9c4-12fb-4887-ae11-cd2caee6a78f', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f803d9c4-12fb-4887-ae11-cd2caee6a78f', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sector_workers).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, household_consumption_base).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, social_redistribution_coalitions).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_budgets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Employment in healthcare, education, elderly care, and social services expands under this constraint. State directs capital toward service workforce expansion and wage structures. They benefit from stable employment and protected budgets; their careers depend on the state's commitment to service legitimacy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sector_workers, beneficiary,
    organized, biographical, constrained, national).

% Direct recipients of expanded healthcare, education, elderly care, and social transfers. These households experience visible improvements in daily life — access to clinics, school capacity, nursing support. The constraint delivers to them tangibly and repeatedly, anchoring legitimacy in their lived experience.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, household_consumption_base, beneficiary,
    moderate, biographical, constrained, national).

% State planners, health ministries, education bureaucracies, and social policy architects who design and administer the constraint. They hold institutional power to prioritize service spending over capital investment; their authority and budget control depend on the constraint's continued operation and visible delivery.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, social_redistribution_coalitions, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, social_redistribution_coalitions, beneficiary).

% Manufacturing, resource extraction, and heavy industrial enterprises that would prefer capital investment and capacity expansion to redistribution. They bear the cost through redirected investment budgets, wage pressure from service-sector employment alternatives, and lower state support for industrial infrastructure. They can relocate production or exit if capital returns elsewhere fall persistently.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_sectors, payer,
    powerful, generational, mobile, global).

% Local governments and municipal authorities lose discretionary capital for roads, utilities, industrial parks, and commercial infrastructure as central budgets prioritize service delivery. They are trapped: they cannot exit the national system, cannot easily redirect service spending once committed, and face rising fiscal pressure as deferred maintenance accumulates.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_budgets, payer,
    moderate, biographical, trapped, local).

% Economic planners and industrial strategists who advocate prioritizing aggregate GDP growth over redistribution and service delivery. They are structurally excluded from agenda-setting when the livelihood-security reading dominates; their position would argue for investment in capital-intensive sectors and growth-maximizing policies that this constraint explicitly deprioritizes.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, quantitative_growth_advocates, excluded,
    institutional, generational, constrained, national).

% State security and technology planners focused on strategic industrial capacity and technological autonomy. They would argue for capital concentration in defense, semiconductor, and high-tech manufacturing sectors. The livelihood-security reading's redistribution logic starves these sectors of the investment capital they require.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, techno_nationalist_strategists, excluded,
    institutional, generational, constrained, global).

% Economists, policy researchers, and independent analysts who assess whether the constraint's legitimacy claim is sustained by actual delivery, whether extraction from industrial sectors is proportionate to service benefits, and whether the constraint's composition (service delivery vs. redistribution vs. safety net) matches its declared function.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__livelihood_security_reading, social_redistribution_coalitions).
narrative_ontology:fixing_cost_class(performance_legitimacy__livelihood_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of delivering employment, healthcare, education, and elderly care consistently across a population. Service sectors require stable, long-term funding; households require predictable access to these services; the constraint ensures state budgets are committed to service delivery rather than fragmented across competing priority claims.
% TRANSFER_FUNCTION: Moves capital, labor, and state budget allocation FROM capital-intensive industrial sectors and from local infrastructure budgets TO service-sector employment, household healthcare and education access, and elderly care provision. The transfer is enforced through budget prioritization and wage-setting authority.
% ABSENT_VOICES: Quantitative growth advocates and techno-nationalist strategists would argue that the constraint starves productive investment and strategic industrial capacity. They are excluded from the agenda-setting table when livelihood-security legitimacy dominates; legislative debates and policy forums in competing institutional contexts carry their objections, but they lack veto power within the constraint's enforcement structure.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, state budget allocation would revert to competing claims: industrial investment, defense spending, infrastructure bonds, and administrative overhead would immediately claim shares that service budgets now hold. Service sectors would lose job security; household access to healthcare, education, and elderly care would fragment as budgets dried up. The labor and social organization around service delivery would atrophy within years.
% FOUNDING_PROBLEM: Early-stage rapid industrialization created dual crises: (1) massive rural-to-urban migration left employment and basic services (healthcare, education, elderly support) catastrophically undersupplied in rapidly growing cities; (2) growth-only legitimacy bred resentment in populations with rising incomes but deteriorating living conditions. Visible service collapse (overcrowded clinics, school closures, urban poverty) delegitimized the regime despite GDP gains.
% FOUNDING_PROBLEM_CORROBORATION: Service-sector unions, health and education ministries, and urban advocacy groups attest the founding problem remains live — service infrastructure lags behind population growth and migration continues. Industrial strategists and some development economists attest the problem has been substantially solved and the constraint persists as redistributive overreach that starves productive investment. Independent analyst reports from outside the benefiting parties document persistent service gaps in specific regions while also noting that industrial capacity has been constrained below strategic levels.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).

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
 *   Extractiveness is moderate-to-high (0.62) because the constraint diverts substantial capital FROM industrial sectors and local infrastructure TO service delivery, and this diversion is enforced through state budget authority rather than market coordination. The constraint IS genuinely coordinating: service delivery is genuinely difficult to solve without stable, centralized funding. But it IS also extractive: the extraction from industrial sectors to fund service delivery is asymmetric and enforced. Suppression is moderate (0.38) because the constraint does not require high coercion of the beneficiary side (service workers and households gain employment and access); the enforcement effort is directed at maintaining the capital reallocation against industrial-sector resistance. Theater is low-to-moderate (0.28) and rising slightly: early periods show genuine service-delivery expansion; later periods show rising administrative and eligibility-verification overhead as budgets tighten and service gaps persist despite stable spending. Accessibility collapse is high (0.71) because once the constraint is understood, alternatives (private healthcare, fee-based education, informal elderly care networks) are incomplete substitutes for the state-delivered services the constraint prioritizes; exit is real but costly. Resistance is moderate (0.45) because capital-intensive industrial sectors have genuine exit options (relocation, reallocation of investment) and do resist, but the constraint's legitimacy basis makes sustained resistance costly for those sectors. The measurement series show extraction rising initially as service expansion is prioritized, then plateauing as budgets stabilize; theater rises as administrative overhead compounds; suppression requirement stays moderate and stable.
 *
 * PERSPECTIVAL GAP:
 *   The institutional agenda-setter (social_redistribution_coalitions) experiences this constraint as genuine coordination solving a real collective-action problem: service delivery requires state commitment, long-term funding, and integrated planning. The payer seats (capital_intensive_industrial_sectors, local_government_infrastructure_budgets) experience it as enforced extraction: their capital is diverted without their consent, and the constraint's persistence depends on state enforcement, not on their voluntary participation. The beneficiary side (service_sector_workers, household_consumption_base) experiences it as legitimacy-sustaining: it delivers visible daily-life improvements. The engine will compute different types from different seats because the structural data declares asymmetric beneficiary and victim groups, asymmetric exit options, and active enforcement. The authored claim is tangled_rope; the metrics should support that divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Service-sector workers and households are beneficiaries with constrained exit (d near 0.2-0.3: they benefit from the constraint, but are not fully free to exit). Social redistribution coalitions are institutional beneficiaries and agenda-setters with mobile exit (d near 0.0-0.1: they control the constraint and benefit from it). Capital-intensive industrial sectors are victims with mobile exit (d near 0.7-0.8: they bear costs, but can exit through relocation or reallocation). Local government infrastructure budgets are trapped victims (d near 0.9: they bear costs and cannot easily exit). The directionality derivation should produce this profile from the authored beneficiary/victim declarations plus the exit and power atoms. The asymmetry is the point: different seats compute different types because they occupy different structural positions relative to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested: social beneficiaries and service ministries attest it is live (service gaps persist despite expansion); growth advocates attest it is substantially solved. If the problem is dead, the constraint persists as pure redistribution divorced from its stated function — a candidates for mandatrophy classification. If the problem is live, the constraint is genuine tangled_rope: real coordination (service delivery) layered with real extraction (capital diversion). The mismatch between founding_problem_status='contested' and disappearance_verdict='world_rearranges' is the engine's trigger for mandatrophy investigation: the constraint clearly shapes arrangements (world would rearrange without it), but its originating mandate is disputed. This is exactly the pattern mandatrophy detection measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    service_delivery_vs_redistribution_boundary,
    'Is the constraint''s core function delivering service coordination (genuine tangled_rope with real coordination component), or is it primarily redistributive wealth transfer using service delivery as the delivery mechanism?',
    'Comparative analysis of service outcomes: if service quality/access improves proportionately to budget increase, the coordination function is real; if outcomes plateau while administrative overhead rises, the function has shifted toward pure redistribution.',
    'If primarily redistributive, reclassify toward snare (extraction via service framing); if primarily coordination, confirm tangled_rope. The boundary is empirically resolvable by tracing service-outcome metrics against spending trajectories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_delivery_vs_redistribution_boundary, empirical, 'Whether the constraint coordinates genuine service delivery or redistributes via service framing.').

omega_variable(
    industrial_sector_exit_materiality,
    'Do capital-intensive industrial sectors actually exit or relocate when the constraint diverts their capital, or do they remain trapped despite exit options?',
    'Observation of actual relocations, capacity reductions, or divestment by major industrial firms; comparison of stated exit threats against actual exit behavior; analysis of whether exit-cost barriers (labor force dependence, local embeddedness, regulatory costs of exit) exceed the extraction magnitude.',
    'If exit is rare and costly, victims are more trapped than mobility suggests; directionality should shift toward higher d (more-target-like). If exit is frequent, the constraint''s persistence depends more on beneficiary preference than on suppression; classification shifts toward legitimate coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industrial_sector_exit_materiality, empirical, 'Whether industrial-sector exit options are real or institutional theater.').

omega_variable(
    sibling_reading_dominance_contest,
    'Which performance-legitimacy reading dominates state policy and institutional priority at different moments? Is there stable dominance or cyclical cycling among the four readings?',
    'Longitudinal analysis of state budget allocation, ministry focus, strategic planning documents, and leadership rhetoric across the interval; measurement of which legitimacy basis is invoked in public justifications for major policy choices.',
    'If the livelihood-security reading remains dominant, the constraint''s operation is stable; if cyclical or shifting, the constraint should be modeled as contested and temporally bound. If another reading rises to dominance, this constraint''s extractiveness may decrease (replaced by a different extraction pattern) or the constraint may degrade to piton status (performative invocation without enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_dominance_contest, empirical, 'Institutional dominance and temporal stability of this specific reading among competing performance-legitimacy framings.').

omega_variable(
    identity_locked_service_workers,
    'Are service-sector workers genuinely beneficiaries, or are they identity-locked to the state through professional identity fusion (their career identity, credentials, and social status are constituted through state-sector employment)?',
    'Analysis of exit behavior and exit costs: if service workers can exit to private healthcare, education, elderly care sectors without credential or status loss, they are not identity-locked; if exit requires credential translation, status degradation, or identity reconfiguration, they are identity-locked beneficiaries whose exit is theoretical.',
    'If identity-locked, the apparent beneficiary status masks partial targeting: service workers are simultaneously beneficiaries (employment) and targets (locked into the state-controlled employment relationship). Directionality should reflect the lock, raising d for identity-locked workers toward constrained/target-like positioning despite nominal beneficiary role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_service_workers, empirical, 'Whether service-sector beneficiaries are genuinely benefiting from choice or identity-locked to state employment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(perf_tr_t0, observed).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__livelihood_security_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(perf_tr_t5, observed).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(perf_tr_t10, observed).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__livelihood_security_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(perf_tr_t15, observed).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(perf_tr_t20, observed).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__livelihood_security_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(perf_tr_t25, observed).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__livelihood_security_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(perf_tr_t30, observed).
narrative_ontology:measurement(perf_tr_t35, performance_legitimacy__livelihood_security_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(perf_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(perf_be_t0, observed).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__livelihood_security_reading, base_extractiveness, 5, 0.49).
narrative_ontology:measurement_basis(perf_be_t5, observed).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(perf_be_t10, observed).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__livelihood_security_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(perf_be_t15, observed).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(perf_be_t20, observed).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__livelihood_security_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(perf_be_t25, observed).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__livelihood_security_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(perf_be_t30, observed).
narrative_ontology:measurement(perf_be_t35, performance_legitimacy__livelihood_security_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(perf_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(perf_su_t0, observed).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__livelihood_security_reading, suppression_requirement, 5, 0.34).
narrative_ontology:measurement_basis(perf_su_t5, observed).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(perf_su_t10, observed).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__livelihood_security_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(perf_su_t15, observed).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(perf_su_t20, observed).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__livelihood_security_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(perf_su_t25, observed).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__livelihood_security_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(perf_su_t30, observed).
narrative_ontology:measurement(perf_su_t35, performance_legitimacy__livelihood_security_reading, suppression_requirement, 35, 0.38).
narrative_ontology:measurement_basis(perf_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__livelihood_security_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% The performance_legitimacy kernel decomposes into four structurally distinct readings, each instantiating a different operationalization of 'performance'. The livelihood_security_reading prioritizes service delivery and household consumption; it coexists_with the quantitative_growth_reading (both remain live institutional positions), influences the qualitative_development_reading (service delivery is necessary but not sufficient for 'high-quality development'), and influences the techno_nationalist_reading (strategic industries are resource-constrained by service-delivery prioritization). Each reading has its own constraint_id, its own ε, its own beneficiary/victim structure, and its own type classification. The family is linked via network.affects_constraints and documented via omega variables naming the contested framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__livelihood_security_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
