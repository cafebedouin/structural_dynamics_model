% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__developmental_state_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: flexible_employment_legitimacy__developmental_state_reading
 *   human_readable: Flexible Employment as Managed Transition (Developmental State Reading)
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   In the developmental-state reading, flexible and platform employment is a
 *   transitional form: legitimate because the state manages it and because it
 *   is headed somewhere — toward formalization on a dated timetable. The
 *   standing arrangement this story assesses: roughly two hundred million
 *   people work outside standard labor relations, dispatched by platforms;
 *   the state stewards the category through multi-ministry guidance (the 2021
 *   twelve-point opinions on protecting workers in new employment forms),
 *   occupational-injury insurance pilots expanding province by province
 *   toward a 2027 nationwide standardization target, algorithmic time-floor
 *   rules, and wage guidance for delivery work. This file instantiates ONE
 *   reading of the contested kernel flexible_employment_legitimacy; the
 *   market-efficiency and precarity-extraction readings are separate
 *   constraints (linked in network.affects_constraints), not positions inside
 *   this one. Epsilon is authored for the standing managed semi-formal
 *   arrangement as this reading assesses it — conceding real deferral costs,
 *   crediting incremental delivery — never for the fully formalized end state
 *   this reading endorses. Claim and metrics are independent authored facts:
 *   claimed_type records the structural judgment (a genuine coordination
 *   function plus asymmetric extraction under active enforcement); the
 *   metrics record descriptive operation; divergence between them and any
 *   computed per-seat type is the measurement the corpus takes. KEY AGENTS
 *   (by structural relationship): - developmental_state_agencies:
 *   agenda-setter (institutional/arbitrage) — stewards the transition,
 *   collects governance performance - platform_firms: primary beneficiary
 *   (powerful/arbitrage) — captures flexibility rents, bears rising
 *   compliance costs - flexible_platform_workers: primary target
 *   (powerless/trapped) — bears volatility, risk, absent benefits -
 *   migrant_gig_workers: most-exposed target subgroup (powerless/trapped) —
 *   registration-linked exclusion deepens immobility -
 *   urban_service_consumers: incidental beneficiary (moderate/mobile) — cheap
 *   rapid services - formal_sector_employers: secondary payer
 *   (organized/constrained) — carries obligations platforms avoid -
 *   independent_union_organizers: excluded voice (powerless/trapped) — no
 *   bargaining channel - labor_research_community: analytical observer
 *   (moderate/analytical) — documents conditions, proposes formalization
 *
 * KEY AGENTS:
 *   - developmental_state_agencies: agenda-setter (institutional/arbitrage) — issues guidance documents, runs pilots, sets the 2027 standardization timetable; collects employment-stability and public-order performance
 *   - platform_firms: primary beneficiary (powerful/arbitrage) — operate dispatch systems, capture flexibility rents, bear growing compliance costs
 *   - flexible_platform_workers: primary target (powerless/trapped) — couriers, drivers, domestic workers, sellers; bear income volatility, accident risk, absent pension accrual
 *   - migrant_gig_workers: most-exposed target subgroup (powerless/trapped) — rural-registration workers in cities whose services they cannot access
 *   - urban_service_consumers: incidental beneficiary (moderate/mobile) — buy speed and low prices sustained by the dispatchable workforce
 *   - formal_sector_employers: secondary payer (organized/constrained) — carry full contribution and severance obligations competing platforms largely avoid
 *   - independent_union_organizers: excluded voice (powerless/trapped) — would bargain over dispatch rates and quotas; no lawful channel
 *   - labor_research_community: analytical observer (moderate/analytical) — surveys conditions, models pay structures, publishes formalization proposals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.55).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.6).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Flexible Employment as Managed Transition (Developmental State Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "economic/political/social").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, 'cc62ee34-5665-49cf-b60d-3c0c1af426bf').
narrative_ontology:cs_kernel_codification('cc62ee34-5665-49cf-b60d-3c0c1af426bf', formalized).
narrative_ontology:cs_authority_grounding('cc62ee34-5665-49cf-b60d-3c0c1af426bf', lineage).
narrative_ontology:cs_interpretation_layer_present('cc62ee34-5665-49cf-b60d-3c0c1af426bf').
narrative_ontology:cs_reading_relation('cc62ee34-5665-49cf-b60d-3c0c1af426bf', flexible_employment_legitimacy__market_efficiency_reading, influences).
narrative_ontology:cs_reading_relation('cc62ee34-5665-49cf-b60d-3c0c1af426bf', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('cc62ee34-5665-49cf-b60d-3c0c1af426bf', foundational, flexible_employment_is_transitional_not_terminal).
narrative_ontology:cs_axiom_status(flexible_employment_is_transitional_not_terminal, holdable).
narrative_ontology:cs_axiom_grounding('cc62ee34-5665-49cf-b60d-3c0c1af426bf', flexible_employment_is_transitional_not_terminal, instrumental).
narrative_ontology:cs_axiom('cc62ee34-5665-49cf-b60d-3c0c1af426bf', foundational, staged_protection_precedes_wholesale_conversion).
narrative_ontology:cs_axiom_status(staged_protection_precedes_wholesale_conversion, holdable).
narrative_ontology:cs_axiom_grounding('cc62ee34-5665-49cf-b60d-3c0c1af426bf', staged_protection_precedes_wholesale_conversion, instrumental).
narrative_ontology:cs_reference_frame('cc62ee34-5665-49cf-b60d-3c0c1af426bf', state_guided_transitional_formalization).
narrative_ontology:cs_drift_state('cc62ee34-5665-49cf-b60d-3c0c1af426bf', contemporary_post_pilot_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cc62ee34-5665-49cf-b60d-3c0c1af426bf', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, platform_firms).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, developmental_state_agencies).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, urban_service_consumers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, flexible_platform_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, migrant_gig_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, platform_firms).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, formal_sector_employers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, developmental_gradualism_doctrine).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, state_stewardship_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue the guidance documents that define how platform work is governed — the 2021 multi-ministry opinions on protecting workers in new employment forms, follow-up implementation notices, and the standardization timetable running to 2027. Run the occupational-injury insurance pilots, convene platform accountability meetings, and publish wage guidance for delivery work. Collect employment-stability figures and public-order outcomes that depend on the sector staying inside this managed category. Can shift instruments — pilot, mandate, target — and can accelerate or slow the timetable.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, developmental_state_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate the dispatch systems that organize courier, ride-hail, and domestic-service work. Gain a labor pool without standard employer obligations — no severance, minimal social-insurance contributions outside the pilots, dispatch control over pace and pay. Bear growing compliance costs as injury-insurance contributions, algorithmic time-floor rules, and wage guidance arrive. Can restructure entities, adjust algorithms, relocate registration, and lobby through industry associations; exiting entirely would mean abandoning the user base already built.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_firms, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, platform_firms, payer).

% Courier riders, ride-hail drivers, domestic workers, and live-stream sellers who earn through platform dispatch. Income varies with weather, demand, and algorithmic quota; most accrue no pension rights and, outside pilot provinces, carry accident risk personally. Classified outside standard labor relations, so overtime and severance rules do not reach them. Leaving means thin alternatives: formal jobs that screen by age and household registration, or return to lower-income home regions.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, flexible_platform_workers, payer,
    powerless, immediate, trapped, national).

% Rural-registration workers doing platform work in cities they cannot fully access. Public schooling, healthcare subsidies, and housing support stay tied to registration elsewhere, so urban earnings carry urban costs without urban services. The return-home option exists but trades away most of the income that brought them to the city. The most exposed seat when demand drops or injury strikes.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, migrant_gig_workers, payer,
    powerless, immediate, trapped, regional).

% Order meals, rides, and services at prices and speeds that depend on a large, instantly dispatchable workforce. Switch between apps freely and bear little of the arrangement's costs directly, beyond what platforms pass through in fees.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, urban_service_consumers, beneficiary,
    moderate, immediate, mobile, national).

% Small manufacturers, restaurants, and retail chains that carry full social-insurance contributions, overtime rules, and severance obligations for comparable staff. Compete for workers against platforms offering take-home pay unburdened by contributions they must legally pay. Voice through industry associations exists but sits subordinate to the transition timetable in policy deliberations.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, formal_sector_employers, payer,
    organized, biographical, constrained, national).

% Worker mutual-aid networks, rider rights groups, and independent labor advocates who would bargain collectively over dispatch rates, quotas, and injury compensation. No lawful channel: bargaining outside the official federation is not permitted, and rider groups that mobilize publicly are dissolved or absorbed. Kept outside the consultation process that produces the guidance documents.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, independent_union_organizers, excluded,
    powerless, biographical, trapped, national).

% Academic labor economists, sociologists, and legal scholars who survey worker conditions, model platform pay structures, and publish formalization proposals. Testify in policy consultations and shape the terms of debate; hold no administrative power over the timetable.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, labor_research_community, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__developmental_state_reading, platform_firms).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__developmental_state_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sequences the incorporation of a very large, mobile labor supply into platform-mediated service work: keeps the sector operating while protections are layered in stages (injury insurance first, wage floors next, portable benefits later), so that governing new work forms requires neither forcing standard employment templates onto them immediately nor leaving them wholly outside protection.
% TRANSFER_FUNCTION: Moves flexibility rents from workers to platform firms (avoided employer obligations, dispatch control, uncompensated volatility and risk-bearing); moves governance performance — employment-stability figures and maintained public order — to state agencies; moves low-priced rapid services to urban consumers; and, under the pilots, moves partial insurance coverage back toward workers.
% ABSENT_VOICES: Independent union organizers and rider mutual-aid networks would object — demanding collective bargaining over dispatch rates, quotas, and injury compensation — but hold no seat: bargaining outside the official federation is not permitted, and consultation on the guidance documents runs through ministries, platforms, and the official union federation. Rank-and-file workers participate mainly as survey subjects, not as negotiating parties.
% DISAPPEARANCE_RATIONALE: Roughly two hundred million people's work arrangements hang on this classification. Overnight removal forces an immediate choice the arrangement exists to defer: platforms either inherit standard employer obligations (a cost shock, contraction, accelerated automation) or the sector falls into a protection vacuum. Urban service prices, platform labor models, and local-government employment accounting all reorganize.
% FOUNDING_PROBLEM: Mid-2010s platform growth produced work that fit neither the standard-employment template nor the informal-economy category: regulators had to govern new work forms during mass employment restructuring without destroying the sector's absorption capacity or abandoning its workers entirely.
% FOUNDING_PROBLEM_CORROBORATION: State planners attest the problem is live — new forms keep emerging and protection gaps persist. Outside the benefiting parties, academic labor surveys and international labor-body assessments corroborate both the original governance gap and the dispute over whether 'transition' still describes the arrangement: court findings that platform-algorithm control constitutes de facto employment in individual cases, and scholarship documenting that pension portability remains unsolved a decade in, attest that parts of the founding problem are solved in name only. Corroboration exists and cuts both ways.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55: from this reading's own lights the standing arrangement is partly justified — the absorption function is real, injury-insurance pilots pay real claims, and staged sequencing has a defensible rationale — but costs compound on the worker side while formalization stays marginal: most riders accrue no pension rights, volatility is uncompensated, and the workforce has grown far faster than conversion. Mid-range epsilon reflects a reading that concedes real deferral costs while crediting incremental delivery. Suppression 0.60 is mostly structural — registration-linked service exclusion, thin formal alternatives for older and rural workers, no lawful bargaining channel outside the official federation — with a smaller coercive component (public rider mobilizations are dissolved or absorbed quickly). Theater 0.35: document issuance and accountability meetings partly perform, but pilots disburse real money and algorithmic time-floor rules bind. Accessibility_collapse 0.45: the sibling readings remain live policy alternatives — platforms lobby for lighter touch, some cities diverge locally — so alternatives are narrowed, not collapsed. Resistance 0.55: the 2020-2021 rider actions forced the algorithm rules; scholarship and occasional judicial findings of de facto employment keep pressure on; the response has been partial accommodation inside the managed frame rather than attack on the frame itself. Worker-side coalition potential is real but episodic: flash coordination during strikes has not consolidated into durable organization, which is why the payer seats classify individually powerless rather than organized. The temporal series run on one shared grid (2015-2027, odd years); 2025 and 2027 points are authored projections keyed to the announced expansion timetable and marked basis=projected. Drift declaration behind cs_structure: practice (compounding workforce, marginal conversion) has substantially departed from the frame's promised convergence; official documents acknowledge protection deficits explicitly while treating convergence as pending — hence acknowledged=true, with the endpoint question routed to the transition_endpoint_indefiniteness omega rather than resolved here.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same documents. From the agenda-setter seat the twelve-point plan and 2027 target are stewardship working as designed — protection deepening on schedule. From the platform seat they are a rising compliance curve to be arbitrated through entity structure and lobbying. From the worker seats the same instruments arrive as too-little-too-late — an injury payout that does not touch pension accrual, a wage floor that does not touch quota intensity. The engine computes these divergences from power, exit, and directional position; the authored claim does not adjudicate them. Inter-institutionally, formal-sector employers experience the arrangement as an obligation asymmetry — they contribute, platforms largely do not — which gives organized payers a stake in the precarity sibling's critique even while remaining outside this reading's coalition.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: platform_firms (arbitrage exit, capturing the flexibility rent) sit nearest the beneficiary end; urban_service_consumers (mobile, paying little directly) sit near it; developmental_state_agencies derive low d from their beneficiary listing, moderated by the fiscal and systemic costs they carry as administrator. Victim declarations drive high d: flexible_platform_workers (trapped — registration barriers, age-screened formal hiring, thin alternatives) sit near the full-target end; migrant_gig_workers sit at the extreme, their exit option the worst of any seat. formal_sector_employers are payers by obligation asymmetry — organized and constrained, high d but with associational voice the worker seats lack. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms reproduce the intended ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves visible: a pure-extraction reading would erase the real absorption and insurance function that gives the arrangement its coordination content, and a pure-coordination reading would erase the compounding deferral that concentrates its costs on trapped workers. Mandatrophy turns on the founding problem: governing new work forms during mass restructuring was real and remains partly unsolved (pension portability above all), but the 'transition' framing now does double duty — describing a genuine sequencing problem and licensing indefinite deferral. The 2027 standardization target stabilizes the kernel rather than sunsetting the arrangement: has_sunset_clause is authored false because no milestone terminates the category itself. If post-2027 milestones convert workers into standard labor relations, transitional dynamics with a real terminus emerge; if each milestone becomes the next baseline, the coordination half decays toward theatrical maintenance. The transition_endpoint_indefiniteness omega carries that fork; the genealogy mismatch consumer watches the contested-status x world_rearranges combination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel flexible_employment_legitimacy; which reading''s policy program will stabilize, and what would each sibling change structurally?',
    'Track which instrument regime consolidates after the 2027 standardization target: mandated labor-relation conversion (the precarity program), deregulatory rollback (the market-efficiency program), or continued staged management (this reading''s program).',
    'If the market-efficiency program consolidates, epsilon falls toward rope-like profiles; if the precarity program consolidates, epsilon rises toward snare-like profiles; this reading''s classification holds only under continued staged management.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the flexible-employment legitimacy kernel stabilizes.').

omega_variable(
    transition_endpoint_indefiniteness,
    'Does the managed transition have a genuine endpoint in standard labor relations, or is ''transition'' a self-extending classification that regenerates its own justification at each milestone?',
    'Compare pre- and post-2027 cohorts: does hitting the standardization target convert workers into standard labor relations with pension accrual, or does the target become the next baseline while the workforce keeps compounding?',
    'If self-extending, the coordination half of the arrangement decays and the structure drifts toward extraction maintained theatrically; if genuine, the arrangement carries scaffold-like dynamics with a real terminus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_endpoint_indefiniteness, empirical, 'Whether the transition framing terminates or self-extends.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is worker-side immobility structural (registration-linked service exclusion, thin formal alternatives, no lawful bargaining channel) or partly internalized (a genuine preference for flexibility over factory discipline that would persist if alternatives improved)?',
    'Post-exit trajectory studies of workers who move from platform work into formal employment: if stated flexibility preferences persist and workers attempt return despite matched formal wages, the preference component is real; if return rates collapse when formal options improve, immobility was structural.',
    'If substantially internalized, measured suppression understates effective lock-in and the exit-option atoms overstate worker mobility; if structural, removing registration and bargaining barriers would collapse the arrangement''s labor-supply advantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized sources of worker immobility.').

omega_variable(
    wage_growth_attribution,
    'Is observed wage growth in platform work attributable to state management (floors, insurance mandates, wage guidance) or to market clearing for scarce delivery labor?',
    'Cross-city comparison where management intensity varies (pilot versus non-pilot provinces, cities with versus without wage guidance), using difference-in-differences on rider earnings.',
    'Management-side attribution vindicates this reading''s core empirical warrant and strengthens its authority; market-side attribution hands the warrant to the market-efficiency sibling and erodes the stewardship claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wage_growth_attribution, empirical, 'Attribution of sector wage growth: management versus market.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 2015, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t2015, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement_basis(flex_tr_t2015, observed).
narrative_ontology:measurement(flex_tr_t2017, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement_basis(flex_tr_t2017, observed).
narrative_ontology:measurement(flex_tr_t2019, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement_basis(flex_tr_t2019, observed).
narrative_ontology:measurement(flex_tr_t2021, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 2021, 0.42).
narrative_ontology:measurement_basis(flex_tr_t2021, observed).
narrative_ontology:measurement(flex_tr_t2023, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 2023, 0.4).
narrative_ontology:measurement_basis(flex_tr_t2023, observed).
narrative_ontology:measurement(flex_tr_t2025, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 2025, 0.37).
narrative_ontology:measurement_basis(flex_tr_t2025, projected).
narrative_ontology:measurement(flex_tr_t2027, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 2027, 0.35).
narrative_ontology:measurement_basis(flex_tr_t2027, projected).

% Extraction over time
narrative_ontology:measurement(flex_be_t2015, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement_basis(flex_be_t2015, observed).
narrative_ontology:measurement(flex_be_t2017, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement_basis(flex_be_t2017, observed).
narrative_ontology:measurement(flex_be_t2019, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 2019, 0.48).
narrative_ontology:measurement_basis(flex_be_t2019, observed).
narrative_ontology:measurement(flex_be_t2021, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement_basis(flex_be_t2021, observed).
narrative_ontology:measurement(flex_be_t2023, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 2023, 0.57).
narrative_ontology:measurement_basis(flex_be_t2023, observed).
narrative_ontology:measurement(flex_be_t2025, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 2025, 0.56).
narrative_ontology:measurement_basis(flex_be_t2025, projected).
narrative_ontology:measurement(flex_be_t2027, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 2027, 0.55).
narrative_ontology:measurement_basis(flex_be_t2027, projected).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t2015, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(flex_su_t2015, observed).
narrative_ontology:measurement(flex_su_t2017, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 2017, 0.36).
narrative_ontology:measurement_basis(flex_su_t2017, observed).
narrative_ontology:measurement(flex_su_t2019, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 2019, 0.45).
narrative_ontology:measurement_basis(flex_su_t2019, observed).
narrative_ontology:measurement(flex_su_t2021, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement_basis(flex_su_t2021, observed).
narrative_ontology:measurement(flex_su_t2023, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 2023, 0.6).
narrative_ontology:measurement_basis(flex_su_t2023, observed).
narrative_ontology:measurement(flex_su_t2025, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 2025, 0.61).
narrative_ontology:measurement_basis(flex_su_t2025, projected).
narrative_ontology:measurement(flex_su_t2027, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 2027, 0.6).
narrative_ontology:measurement_basis(flex_su_t2027, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__precarity_extraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'flexible employment legitimacy' covers three structurally distinct claims and decomposes into a three-story family: this developmental-state reading (mid epsilon — transition justified, deferral costly), market_efficiency_reading (low epsilon — friction-minimizing clearing), precarity_extraction_reading (high epsilon — surplus capture). Each story carries its own epsilon, beneficiaries, and victims; they are linked because the developmental reading's instruments (targets, pilots, mandates) reshape the operating environment of both siblings — upstream institutional embodiment pressing on downstream rivals — while the siblings' evidential bases (market outcomes, worker conditions) feed back into this reading's legitimacy. Epsilon differs across the family because each reading assesses the same standing arrangement under different lights; the referent (the managed semi-formal arrangement) is shared.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
