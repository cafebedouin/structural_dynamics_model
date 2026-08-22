% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Livelihood-Security Reading of Performance Legitimacy
 *   domain: political economy/development planning/state capitalism
 *
 * SUMMARY:
 *   A developmental state's authority rests on performance rather than
 *   procedure or ideology. This story instantiates ONE reading of that
 *   contested kernel: legitimacy grounded in tangible improvements in daily
 *   life — employment, healthcare, education, elderly care — that citizens
 *   directly experience. Under this reading, the standing arrangement under
 *   contest is a service-first fiscal constitution: social insurance
 *   expansion, consumption support, and service-sector buildout become the
 *   primary claims on state capacity, while capital-intensive industrial
 *   expansion and local government infrastructure spending lose fiscal
 *   priority. The epsilon referent is that standing service-first arrangement
 *   as this reading holds it — never the growth-first arrangement a sibling
 *   reading would endorse. This is one member of a four-story constraint
 *   family: quantitative_growth_reading (headline GDP as the performance
 *   standard), qualitative_development_reading (structural transformation
 *   toward high-quality development), and techno_nationalist_reading
 *   (strategic self-sufficiency as the performance standard) are separate
 *   files with their own epsilon values, because each reading instantiates a
 *   different constraint with a different victim set and fiscal geometry —
 *   the growth reading's epsilon attaches to an arrangement extractive toward
 *   households and environmental sinks, this reading's epsilon attaches to an
 *   arrangement extractive toward investment channels. The stories are linked
 *   through network.affects_constraints, not folded into one.
 *
 * KEY AGENTS:
 *   - - central_authority: Agenda setter (institutional/arbitrage) — defines the delivery criteria, redirects fiscal flows, adjudicates which reading of the kernel governs
 *   - - urban_households: Primary beneficiary (organized/constrained) — directly experiences delivered services; carries indirect costs where prices adjust
 *   - - rural_households: Secondary beneficiary (moderate/constrained) — reached later and thinner by the expanding safety net
 *   - - healthcare_education_workforce: Beneficiary (organized/identity_locked) — employment and professional identity bound to the service-expansion mandate
 *   - - heavy_industrial_expansion_coalition: Primary payer (powerful/constrained) — loses fiscal priority, credit access, and planning attention
 *   - - local_governments: Payer and local administrator (institutional/constrained) — infrastructure spending squeezed while tasked with delivery
 *   - - elderly_dependents: Beneficiary (moderate/trapped) — care coverage is their direct stake; they cannot exit aging
 *   - - migrant_workers: Excluded (powerless/trapped) — outside registration-bound service access
 *   - - development_economists: Analytical observer (analytical/analytical) — tracks allocation shifts and delivery integrity from outside the allocation process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.58).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.55).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Livelihood-Security Reading of Performance Legitimacy").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political economy/development planning/state capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, '95e8f9ae-31a1-4ce6-99c4-6dd05396afff').
narrative_ontology:cs_kernel_codification('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', implicit).
narrative_ontology:cs_authority_grounding('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', practice).
narrative_ontology:cs_interpretation_layer_present('95e8f9ae-31a1-4ce6-99c4-6dd05396afff').
narrative_ontology:cs_reading_relation('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', foundational, legitimacy_attaches_only_to_directly_experienced_improvement).
narrative_ontology:cs_axiom_status(legitimacy_attaches_only_to_directly_experienced_improvement, holdable).
narrative_ontology:cs_axiom_grounding('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', legitimacy_attaches_only_to_directly_experienced_improvement, deontological).
narrative_ontology:cs_axiom('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', secondary, household_welfare_precedence_over_capital_accumulation).
narrative_ontology:cs_axiom_status(household_welfare_precedence_over_capital_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', household_welfare_precedence_over_capital_accumulation, instrumental).
narrative_ontology:cs_reference_frame('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', tangible_livelihood_delivery_compact).
narrative_ontology:cs_drift_state('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', contemporary_slowdown, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('95e8f9ae-31a1-4ce6-99c4-6dd05396afff', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, urban_households).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, rural_households).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, healthcare_education_workforce).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, heavy_industrial_expansion_coalition).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, elderly_dependents).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, delivery_based_statecraft_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, consumption_led_stabilization_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the criteria by which its own performance is judged, redirects fiscal flows between consumption support and investment channels, and reweights cadre evaluations toward livelihood outcomes. It can redefine the delivery standard when convenient, which is its structural escape route: unlike every other seat, it can change the rules of the arrangement rather than exit it. It converts delivered services into political deference and bears the stability risk if delivery falters.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, central_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Receive expanded healthcare, education, eldercare, and consumption support they can verify in daily life. They carry part of the cost indirectly where firms and local governments pass fiscal pressure into prices and fees. Their exit options are poor: registration systems, property, and family ties bind them to the jurisdiction, so their leverage is voice — petitions, complaints, compliance withdrawal — aimed at the delivery standard they are promised.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, urban_households, beneficiary,
    organized, biographical, constrained, national).

% Are reached by the expanding safety net later and thinner than urban residents: pension floors, basic insurance, and clinic buildout arrive with coverage gaps and quality shortfalls. They gain real protection against catastrophic medical and old-age costs but at lower service intensity. Their bargaining position inside the allocation process is weak, and migration to cities is their main adjustment channel.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, rural_households, beneficiary,
    moderate, biographical, constrained, national).

% Doctors, teachers, and care workers whose employment, staffing ratios, and career pipelines expand with the service mandate. Their professional identity is fused with the mission — they are the delivery the legitimacy claim runs through — which makes exit psychologically costly even where administrative mobility exists. They absorb workload pressure when targets rise faster than staffing, and their compliance determines whether reported delivery matches experienced delivery.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, healthcare_education_workforce, beneficiary,
    organized, biographical, identity_locked, national).

% Capital-intensive industrial groups, their planning allies, and the banking channels that financed them. They lose subsidized credit, land allocation priority, and planning attention as fiscal flows rotate toward consumption and services. Their assets are location- and relationship-specific, their political embeddedness cuts both ways, and relocating or converting capacity is slow and costly. They contest every allocation cycle and lobby for the competing readings that would restore their priority.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, heavy_industrial_expansion_coalition, payer,
    powerful, generational, constrained, continental).

% Administer the delivery targets locally — running the clinics, schools, and care programs — while losing the land-sale and infrastructure-spending model that previously financed their budgets and officials' advancement. They receive conditional central transfers earmarked for services but cannot borrow freely for the projects that once defined local success. They are simultaneously the enforcement arm of the arrangement and one of its principal financial casualties, and they respond with statistical padding, unfunded mandates, and lobbying for fiscal recentralization relief.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_governments, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, local_governments, agenda_setter).

% Retirees and their families for whom pension adequacy, chronic-care coverage, and eldercare capacity are the entire substance of the arrangement. They cannot exit aging and cannot defer their claims: their time horizon is short, which makes them the most immediate constituency for delivery and the least patient with statistical substitutes. Their growing share of the population is the demographic engine that keeps the reading ascendant.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, elderly_dependents, beneficiary,
    moderate, immediate, trapped, national).

% Work in cities while holding registration elsewhere, which places them outside much of the locally delivered healthcare, schooling, and eldercare the arrangement funds. They contribute labor and payroll deductions to systems whose benefits they access incompletely or only after returning home. They have no seat in the allocation process and would object that tangible improvement is rationed by administrative status; their exclusion is invisible in aggregate delivery statistics.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, migrant_workers, excluded,
    powerless, immediate, trapped, national).

% Academic and multilateral analysts who track whether fiscal flows actually rotated, whether reported delivery matches household experience, and whether the consumption-led rebalancing is macroeconomically coherent. They publish incidence analyses and audit-style comparisons, advise both defenders and critics of the arrangement, and bear no direct cost or benefit from its operation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, development_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the developmental state's commitment problem: citizens cannot verify aggregate output claims, but they can verify whether clinics are staffed, schools teach, and pensions pay. Tying legitimacy to directly experienced welfare gives the state a legible, hard-to-fake performance standard, gives citizens a shared criterion for evaluating government, and stabilizes the social contract through a period when the growth-based standard is becoming unavailable.
% TRANSFER_FUNCTION: Moves fiscal resources from capital-intensive investment channels — industrial subsidies, local infrastructure borrowing, land-financed projects — toward household consumption support, social insurance, and service provision; and moves political deference from citizens to the state, continuously, contingent on delivered improvement.
% ABSENT_VOICES: Migrant and informal workers outside registration-bound service access would object that tangible improvement is rationed by administrative status. Future cohorts inherit the deferred maintenance of underbuilt infrastructure and the debt behind the transfers. Residents of declining industrial regions absorb adjustment without a compensation seat. None of these hold positions in the allocation process; their objections surface only as unrest statistics or survey outliers.
% DISAPPEARANCE_RATIONALE: If the livelihood-security criterion vanished overnight, fiscal flows would revert to investment channels within a budget cycle, the expanded service workforce would contract, and the state would need an immediate substitute legitimacy basis — headline growth, unavailable at the current slowdown, or nationalist mobilization, which does not address daily insecurity — leaving a legitimacy vacuum in which every seat's expectations are already structured by the delivery compact.
% FOUNDING_PROBLEM: Rapid marketization dismantled inherited welfare institutions while growth-first legitimacy offered citizens nothing when shocks arrived — unemployment, medical bankruptcy, unsupported old age. The state needed a legitimacy basis that did not require indefinitely sustained growth rates and that answered the insecurity marketization itself created.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: recurring citizen-priority surveys consistently rank healthcare, eldercare, and employment security among top public concerns; demographic and actuarial projections independently attest deepening aging and care-cost pressure; international health-system and pension-sufficiency assessments corroborate the service-gap baseline the arrangement responds to. No source outside the arrangement's defenders attests that the founding problem is resolved.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.58 because the costs imposed on the investment channels are real, ongoing, and decoupled from any compensating service those seats receive: industrial coalitions lose subsidized credit and planning priority, and local governments lose the land-and-infrastructure revenue model that financed their prior expansion. Suppression is 0.55 because persistence requires active machinery — cadre evaluation reweighted toward livelihood metrics, central transfers made conditional on delivery targets, borrowing limits closing the local infrastructure channel — which disadvantages the growth-first alternative without eliminating it. Theater_ratio is 0.32: delivery statistics are partially gameable (beds counted, classes logged), but the standard is anchored in citizens' direct experience, which caps how far performance can detach from delivery. Accessibility_collapse is 0.48 because the growth-first alternative remains a live competing reading rather than a collapsed option. Resistance is 0.62: the industrial coalition and fiscally squeezed local governments actively contest allocations in every planning cycle. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; all three trajectories rise monotonically as slowing growth intensifies the reading, enforcement hardens against mounting resistance, and reporting targets multiply faster than audit capacity. Final-series values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the central authority's position the arrangement is a legitimacy machine it built and can recalibrate; from the heavy-industrial coalition's position it is a confiscation of accumulated planning privilege; from urban households' position it is a compact finally paying out. Local governments occupy the sharpest split: they administer the delivery targets (agenda-setter function) while absorbing the fiscal squeeze (payer position), so their computed seat should sit between the center's and the industrial coalition's. The engine derives these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the household, workforce, and elderly seats toward the beneficiary end of directionality: services and transfers flow to them and their exits are poor, so effective extraction damps toward zero or inverts into subsidy. The payer declarations drive the industrial coalition and local governments toward the target end: they finance the redirection with constrained exits, so effective extraction amplifies. Urban households sit nearest symmetric among the beneficiaries because price adjustments pass part of the fiscal burden back to them. No directionality overrides are authored: the derivation from declared roles, power, and exits captures the structure, and the override mechanism keys on power atoms — an override at the institutional atom would conflate the two institutionally-powered seats (central_authority near the beneficiary end, local_governments near the target end) whose relationships genuinely differ.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — insecurity created by marketization colliding with a legitimacy basis that could not survive a growth slowdown — is live: aging demographics, care costs, and employment insecurity are worsening, not solved, so no mandatrophy declaration is authored. The hybrid classification prevents two symmetrical mislabels. Reading the arrangement as pure extraction misses the genuine coordination it performs: it solves the developmental state's commitment problem by giving citizens a verifiable, hard-to-fake performance standard, and it stabilizes the social contract through a period when the growth reading is becoming unavailable. Reading it as pure coordination misses the identifiable payers whose losses are structural rather than incidental — the industrial coalition and local governments are coordinated INTO paying, by the same evaluation and transfer machinery that delivers the services. The theater series is the early-warning channel for the third failure mode: if reported delivery detaches from experienced delivery, the arrangement drifts toward inertial performance maintained by statistics rather than services.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operative_reading_selection,
    'As headline growth slows, does the livelihood-security reading become the operative legitimacy criterion, or do the quantitative-growth or techno-nationalist sibling readings displace it?',
    'Track fiscal allocation shares (consumption support versus industrial subsidy), cadre evaluation weightings, and official doctrinal statements across successive plan periods.',
    'If techno_nationalist_reading wins, the victim set flips: households absorb consumption compression while strategic industries regain fiscal priority, inverting this story''s beneficiary/payer structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_reading_selection, empirical, 'Which sibling reading of the performance_legitimacy kernel becomes operative as growth slows.').

omega_variable(
    indirect_industrial_benefit,
    'Do capital-intensive industrial sectors bear net costs under this reading, or do they recover those costs indirectly through a healthier workforce, a larger consumption market, and social stability?',
    'Sector-level fiscal incidence analysis comparing taxes and forgone subsidies paid by heavy industry against demand externalities, stabilized labor supply, and residual support received under the service-first allocation.',
    'If industrial sectors are net indirect beneficiaries, the measured extraction asymmetry falls and the arrangement computes nearer pure coordination than hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indirect_industrial_benefit, empirical, 'Whether the payer seats are net losers or indirect beneficiaries.').

omega_variable(
    delivery_statistics_integrity,
    'How much of administratively reported service delivery — hospital beds, training slots, care coverage — reflects experienced improvement versus statistical construction?',
    'Independent audit studies and household-experience surveys benchmarked against administrative statistics.',
    'High divergence pushes theater_ratio toward piton-drift territory and would date a degradation transition; low divergence supports the coordination-function reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delivery_statistics_integrity, empirical, 'Real-versus-reported gap in service delivery, the main theater channel.').

omega_variable(
    authority_grounding_framing,
    'Is the authority enforcing this reading grounded in delivered practice (a functioning social compact) or in extraction (the center''s need to prevent any revision of the legitimacy criterion that would expose non-delivery)?',
    'Observe behavior when delivery targets are missed: quiet revision of the criterion signals extraction-grounded authority; visible admission of failure followed by compensation signals practice-grounded authority.',
    'An extraction-grounded framing shifts cs_structure.authority_grounding from practice to extraction and raises computed suppression; the practice-grounded framing preserves the current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Framing under-determination of the authority structure beneath the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__livelihood_security_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__livelihood_security_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__livelihood_security_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__livelihood_security_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__livelihood_security_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__livelihood_security_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__livelihood_security_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__livelihood_security_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__livelihood_security_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__livelihood_security_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__livelihood_security_reading, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__livelihood_security_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% Performance legitimacy decomposes into four structurally distinct constraints, one per reading of the shared kernel. This story (livelihood_security_reading) exerts structural pressure on quantitative_growth_reading: prioritizing consumption over investment drains the resource base the growth reading requires and shifts the legitimacy conditions under which headline growth is evaluated. It coexists with qualitative_development_reading (overlapping constituencies, compatible framings frequently held by the same faction) and with techno_nationalist_reading (competing fiscal claims, but the security framing gives the latter independent standing that neither displaces). Each file carries its own epsilon for its own standing arrangement; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
