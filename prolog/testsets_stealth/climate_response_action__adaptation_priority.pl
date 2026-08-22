% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response Settlement
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of the climate-response kernel,
 *   instantiated as governing practice: public capital is directed first to
 *   resilience infrastructure and adaptive capacity on the planning
 *   assumption that a further rise in temperature is locked in, and
 *   protection of vulnerable populations is ranked ahead of abatement
 *   spending. The arrangement delivers genuine protection — sea defenses,
 *   early-warning systems, resilient agriculture — while embedding an
 *   asymmetric cost structure: assessed needs (~$540B/yr) exceed pledged
 *   flows by ~$350B, the residual lands on developing-nation budgets and
 *   uncovered populations, and the accepted-warming baseline externalizes
 *   compounding costs onto the future. This file is ONE READING of the kernel
 *   'climate_response_action'; sibling files instantiate mitigation_priority
 *   and degrowth_transformation with different victim sets and different
 *   epsilon values. Per the epsilon-referent rule, extractiveness here is
 *   authored for the standing adaptation-priority arrangement as this
 *   reading's own lights assess it: the reading endorses the priority
 *   ordering itself and locates extraction in the implementation gap
 *   (unfunded needs, protection disparities, externalized warming), not in
 *   the ordering.
 *
 * KEY AGENTS:
 *   - - developed_nation_treasuries: Agenda-setting beneficiary (institutional/arbitrage) — sets finance terms, defers abatement expenditure, retains the avoided-mitigation saving
 *   - - multilateral_adaptation_funds: Administrator with secondary collection (institutional/constrained) — writes disbursement rules, funded from administered flows
 *   - - adaptation_construction_sector: Secondary beneficiary (organized/mobile) — collects project revenue across jurisdictions
 *   - - protected_asset_holders: Secondary beneficiary (powerful/mobile) — coastal and exposed assets defended by public works
 *   - - developing_nation_fiscs: Primary target (moderate/trapped) — absorbs the financing gap through debt and deferred services
 *   - - unfinanced_exposed_populations: Primary target (powerless/trapped) — outside protection perimeters, no seat in allocation bodies
 *   - - small_island_states: Existential-exposure target (organized/trapped) — coalition voice without exit
 *   - - future_generations: Intergenerational target (powerless/trapped) — inherits the accepted-warming baseline
 *   - - ipcc_assessment_bodies: Analytical observer (institutional/analytical) — common evidentiary reference across rival readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.62).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.5).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Adaptation-Priority Climate Response Settlement").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, '61eb1613-61d6-4a36-9e44-79b53b48bfdd').
narrative_ontology:cs_kernel_codification('61eb1613-61d6-4a36-9e44-79b53b48bfdd', formalized).
narrative_ontology:cs_authority_grounding('61eb1613-61d6-4a36-9e44-79b53b48bfdd', distributed).
narrative_ontology:cs_reading_relation('61eb1613-61d6-4a36-9e44-79b53b48bfdd', climate_response_action__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('61eb1613-61d6-4a36-9e44-79b53b48bfdd', climate_response_action__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('61eb1613-61d6-4a36-9e44-79b53b48bfdd', foundational, committed_warming_makes_protection_the_paramount_duty).
narrative_ontology:cs_axiom_status(committed_warming_makes_protection_the_paramount_duty, holdable).
narrative_ontology:cs_axiom_grounding('61eb1613-61d6-4a36-9e44-79b53b48bfdd', committed_warming_makes_protection_the_paramount_duty, empirically_contingent).
narrative_ontology:cs_axiom('61eb1613-61d6-4a36-9e44-79b53b48bfdd', foundational, vulnerable_protection_outranks_abatement_spending).
narrative_ontology:cs_axiom_status(vulnerable_protection_outranks_abatement_spending, holdable).
narrative_ontology:cs_axiom_grounding('61eb1613-61d6-4a36-9e44-79b53b48bfdd', vulnerable_protection_outranks_abatement_spending, deontological).
narrative_ontology:cs_reference_frame('61eb1613-61d6-4a36-9e44-79b53b48bfdd', protection_first_resilience_settlement).
narrative_ontology:cs_drift_state('61eb1613-61d6-4a36-9e44-79b53b48bfdd', post_paris_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('61eb1613-61d6-4a36-9e44-79b53b48bfdd', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, developed_nation_treasuries).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, adaptation_construction_sector).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, protected_asset_holders).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nation_fiscs).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, unfinanced_exposed_populations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, small_island_states).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, multilateral_adaptation_funds).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, locked_in_warming_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms under which adaptation finance flows: they pledge contributions to multilateral funds, shape allocation criteria, and decide domestically how much capital goes to resilience versus abatement. By treating a further rise in temperature as the planning baseline, they defer the larger expenditures that aggressive domestic decarbonization would require, while their own coastal assets receive first call on protection budgets. Exit for them means reframing the portfolio toward a different response strategy — an option their fiscal position keeps open.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developed_nation_treasuries, agenda_setter,
    institutional, generational, arbitrage, global).

% Administer pooled adaptation finance: they write disbursement rules, attach conditions to transfers, and report on delivery. Secretariat operations are funded from the flows they manage, so the volume of administered capital sustains the institutions themselves. Their governing boards are weighted toward contributor states.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, multilateral_adaptation_funds, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, multilateral_adaptation_funds, beneficiary).

% Engineering firms, insurers, and contractors build the sea walls, drainage, cooling networks, and resilient infrastructure the arrangement finances. Revenue scales with the size of the adaptation pipeline; firms operate across jurisdictions and follow capital wherever projects are commissioned.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, adaptation_construction_sector, beneficiary,
    organized, biographical, mobile, global).

% Owners of coastal real estate, ports, and exposed industrial assets whose property is defended by publicly financed protection works. Holdings appreciate when protection arrives, and owners can relocate or divest ahead of areas that lose out in allocation.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, protected_asset_holders, beneficiary,
    powerful, biographical, mobile, regional).

% Government budgets in exposed low- and middle-income countries must fund adaptation from limited tax bases, often borrowing to do so. Pledged international transfers arrive late and partial against assessed needs, leaving a recurring gap these budgets absorb through debt or deferred services. Leaving the arrangement is not available: the exposure is territorial.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developing_nation_fiscs, payer,
    moderate, biographical, trapped, national).

% Households in informal settlements, drylands, and floodplains that fall outside financed protection perimeters. They absorb heat, flood, and crop losses directly, pay for their own coping, and migrate when coping fails. They hold no seat in the bodies that rank protection priorities.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, unfinanced_exposed_populations, payer,
    powerless, immediate, trapped, local).

% Nations whose territory faces existential inundation exposure. They organize collectively to press for finance and priority treatment, but their physical exposure cannot be exited and their fiscal capacity cannot self-fund the required works.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, small_island_states, payer,
    organized, generational, trapped, regional).

% People not yet born who inherit both the infrastructure built today and the higher warming this arrangement accepts as its baseline. They bear the compounded costs embedded in the planning assumption, with no mechanism to consent or decline.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Assessment panels compile the physical-science and impacts literature that all parties cite. They quantify committed warming, adaptation needs, and finance gaps, and their reports serve as the common evidentiary reference across the rival response strategies.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, ipcc_assessment_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__adaptation_priority, developed_nation_treasuries).
narrative_ontology:fixing_cost_class(climate_response_action__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools and allocates capital for resilience infrastructure — sea defenses, early-warning systems, resilient agriculture, heat-response networks — that no individual actor or nation can provide alone, and sequences protection across exposed regions under a shared planning baseline.
% TRANSFER_FUNCTION: Moves roughly $540B annually in required adaptation capital from national fiscs and development lenders toward protection projects; because pledged flows fall short by ~$350B, the residual burden shifts onto developing-nation budgets and uncovered populations, while high-emitting economies retain the avoided cost of deeper abatement.
% ABSENT_VOICES: Future generations and populations outside financed protection perimeters have no seat anywhere in the arrangement. Mitigation-first and degrowth advocates are present in public discourse but marginalized inside this arrangement's planning and finance bodies, which treat the priority ordering as settled rather than as one reading among rivals.
% DISAPPEARANCE_RATIONALE: Planned sea defenses, early-warning systems, and resilient-agriculture programs would halt mid-build; exposed nations would face unbudgeted disaster losses on the current trajectory; and the political settlement that treats protection as the primary climate obligation would need replacement by one of the rival readings, redistributing who pays and who is protected.
% FOUNDING_PROBLEM: Past emissions had already committed a portion of warming, so some climate damage was unavoidable on any emissions path; exposed populations needed funded protection immediately rather than waiting for abatement programs to act.
% FOUNDING_PROBLEM_CORROBORATION: IPCC physical-science assessments corroborate committed warming and quantified adaptation needs independently of any benefiting party. However, no source outside this reading's coalition attests that protection should OUTRANK abatement: mitigation and degrowth proponents dispute the priority ordering itself. Corroboration therefore covers the founding problem's existence, not this reading's ranking of responses — the distinction is recorded rather than smoothed over.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62: the reading's own accounting concedes a ~$350B annual financing gap against ~$540B assessed need, protection allocated by contributor-weighted governance rather than by exposure, and a planning baseline that compounds costs onto those unborn. Suppression is 0.50 and is authored as a RAW STRUCTURAL PROPERTY — covenant conditionality attached to disbursements, debt leverage over constrained fiscs, and perimeter exclusion — deliberately unscaled by scope or power; the engine owns all scaling arithmetic on the extractive side alone. Theater ratio 0.32 reflects the pledge-versus-delivery gap: real infrastructure is built, but a growing share of activity is pledging, reporting, and summitry around undelivered finance. Accessibility collapse is 0.40 — the rival readings remain live and partially accessible in law and discourse; nothing coercively removes them, which is exactly why this is not a snare. Resistance is 0.58 — G77/AOSIS finance contests, climate-litigation dockets, and mitigation-advocate pushback against the acceptance premise are persistent and occasionally effective. The three temporal series run on ONE SHARED GRID (2015/2019/2023/2027/2031/2035) with every metric authored at every point; post-2023 points carry projected basis. The suppression_requirement series is included because enforcement machinery visibly matured over the interval — voluntary pledges hardened into conditional-lending covenants and debt-linked adaptation instruments — a rising enforcement trajectory, not a static picture. Receipt surface: the largest captured gain, the abatement expenditure deferred by accepting the warming baseline, accrues to developed_nation_treasuries, so gain_flow names that seat; construction margins and fund overheads are smaller accruals spread across many firms and institutions. Fixing cost is prohibitive: closing the gap requires fiscal transfers contributor electorates have declined for a decade, and the benefits of fixing are discounted and future-skewed from the fixer's seat.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the agenda-setter seats (treasuries, funds) the arrangement appears as stewardship of scarce capital among competing needs, with conditionality as prudence. From the developing-nation fisc seat the same structure is a bill presented at terms they did not set, payable in sovereign debt. From the unfinanced-population seat it is a perimeter drawn around other people's assets. From the future-generations seat it is a baseline chosen without a counterparty. Coalition dynamics matter here: small_island_states show that organizing converts powerless exposure into voice (AOSIS bargaining), yet voice without exit leaves them on the target side of the ledger — coalition power changes the terms of extraction, not its incidence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: developed_nation_treasuries sit nearest the beneficiary pole (they collect the avoided-abatement saving AND set the terms — the derivation's beneficiary-plus-arbitrage profile), adaptation_construction_sector collects pure project revenue, protected_asset_holders receive subsidized defense of appreciating assets. Victim declarations drive high directionality: unfinanced_exposed_populations and future_generations sit nearest the full-target pole (zero exit, no seat, full cost incidence), developing_nation_fiscs slightly below them (trapped, but partial recipients of some flows), small_island_states high-target with organized voice but physically immobile. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already separate the seats correctly, and adding overrides would duplicate structural data the derivation handles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — committed warming requiring funded protection now — is live and independently corroborated, so no mandatrophy is declared and none should be inferred from the theater ratio. The tangled-rope classification performs double duty: it blocks the misread of this arrangement as pure extraction (which would erase the real protection delivered to covered populations and the genuine pooling problem solved) and blocks the misread of the financing gap as ordinary coordination overhead (which would erase the asymmetric burden landing on trapped payers through the same pipes that deliver protection). The omega variables carry the branch points: if the gap closed and coverage converged, the structure migrates toward rope; if disparities harden into durable exclusion, toward snare. Because the founding problem is live, the persistence question is about the gap, not the mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operative_reading_contest,
    'This constraint is the adaptation_priority reading of the climate_response_action kernel; do the sibling readings (mitigation_priority, degrowth_transformation) remain merely rival files, or does one displace this reading as the operative constraint governing actual capital flows?',
    'Track budget composition and treaty text across successive COP cycles: if abatement spending overtakes adaptation flows, or a sufficiency framework replaces growth-framed finance, the operative reading has shifted.',
    'If mitigation_priority becomes operative, the primary victim set shifts toward future generations via abatement shortfalls and this file''s epsilon no longer describes the standing arrangement; if degrowth_transformation prevails, the protection-perimeter structure dissolves into throughput caps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_reading_contest, conceptual, 'Whether this reading remains the operative instantiation of the kernel or a sibling displaces it.').

omega_variable(
    accepted_warming_constructedness,
    'How much of the temperature rise this arrangement accepts as its planning baseline is physically committed, and how much is a politically constructed acceptance reflecting forgone deeper abatement?',
    'Compare committed warming under current-policy trajectories against technically feasible abatement pathways assessed by independent physical-science bodies.',
    'If a large share of accepted warming is constructed rather than committed, the acceptance premise functions as a choice benefiting present high emitters, and the arrangement''s true extraction exceeds this reading''s own accounting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accepted_warming_constructedness, empirical, 'Physical-commitment versus political-choice content of the accepted-warming baseline.').

omega_variable(
    protection_disparity_trajectory,
    'Do protection outcomes converge toward universal coverage as finance scales, or do disparities between financed and unfinanced perimeters widen into durable exclusion?',
    'Longitudinal comparison of protection coverage and disaster-loss rates between financed and unfinanced exposed regions as aggregate adaptation flows grow.',
    'Convergence would move the arrangement toward pure coordination cost; widening disparity would deepen the target-side burden and push the structure toward enforced exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_disparity_trajectory, empirical, 'Whether the protection perimeter narrows or hardens over time.').

omega_variable(
    north_south_gap_structurality,
    'Is the ~$350B North-South adaptation finance gap a transient mobilization failure that scaled instruments can close, or a structural feature of contributor fiscal politics that persists indefinitely?',
    'Test whether innovative-finance instruments (levies, SDR rechanneling, loss-and-damage capitalization) close the gap over successive replenishment cycles or merely relabel it.',
    'If structural, the transfer asymmetry is permanent and the arrangement cannot reach a rope-like steady state; if transient, the extraction component may decay toward coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_south_gap_structurality, empirical, 'Permanence of the North-South financing gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cra_adapt_tr_t2015, climate_response_action__adaptation_priority, theater_ratio, 2015, 0.2).
narrative_ontology:measurement_basis(cra_adapt_tr_t2015, observed).
narrative_ontology:measurement(cra_adapt_tr_t2019, climate_response_action__adaptation_priority, theater_ratio, 2019, 0.24).
narrative_ontology:measurement_basis(cra_adapt_tr_t2019, observed).
narrative_ontology:measurement(cra_adapt_tr_t2023, climate_response_action__adaptation_priority, theater_ratio, 2023, 0.28).
narrative_ontology:measurement_basis(cra_adapt_tr_t2023, observed).
narrative_ontology:measurement(cra_adapt_tr_t2027, climate_response_action__adaptation_priority, theater_ratio, 2027, 0.3).
narrative_ontology:measurement_basis(cra_adapt_tr_t2027, projected).
narrative_ontology:measurement(cra_adapt_tr_t2031, climate_response_action__adaptation_priority, theater_ratio, 2031, 0.31).
narrative_ontology:measurement_basis(cra_adapt_tr_t2031, projected).
narrative_ontology:measurement(cra_adapt_tr_t2035, climate_response_action__adaptation_priority, theater_ratio, 2035, 0.32).
narrative_ontology:measurement_basis(cra_adapt_tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(cra_adapt_be_t2015, climate_response_action__adaptation_priority, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement_basis(cra_adapt_be_t2015, observed).
narrative_ontology:measurement(cra_adapt_be_t2019, climate_response_action__adaptation_priority, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement_basis(cra_adapt_be_t2019, observed).
narrative_ontology:measurement(cra_adapt_be_t2023, climate_response_action__adaptation_priority, base_extractiveness, 2023, 0.56).
narrative_ontology:measurement_basis(cra_adapt_be_t2023, observed).
narrative_ontology:measurement(cra_adapt_be_t2027, climate_response_action__adaptation_priority, base_extractiveness, 2027, 0.59).
narrative_ontology:measurement_basis(cra_adapt_be_t2027, projected).
narrative_ontology:measurement(cra_adapt_be_t2031, climate_response_action__adaptation_priority, base_extractiveness, 2031, 0.61).
narrative_ontology:measurement_basis(cra_adapt_be_t2031, projected).
narrative_ontology:measurement(cra_adapt_be_t2035, climate_response_action__adaptation_priority, base_extractiveness, 2035, 0.62).
narrative_ontology:measurement_basis(cra_adapt_be_t2035, projected).

% Suppression requirement over time
narrative_ontology:measurement(cra_adapt_su_t2015, climate_response_action__adaptation_priority, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement_basis(cra_adapt_su_t2015, observed).
narrative_ontology:measurement(cra_adapt_su_t2019, climate_response_action__adaptation_priority, suppression_requirement, 2019, 0.42).
narrative_ontology:measurement_basis(cra_adapt_su_t2019, observed).
narrative_ontology:measurement(cra_adapt_su_t2023, climate_response_action__adaptation_priority, suppression_requirement, 2023, 0.46).
narrative_ontology:measurement_basis(cra_adapt_su_t2023, observed).
narrative_ontology:measurement(cra_adapt_su_t2027, climate_response_action__adaptation_priority, suppression_requirement, 2027, 0.48).
narrative_ontology:measurement_basis(cra_adapt_su_t2027, projected).
narrative_ontology:measurement(cra_adapt_su_t2031, climate_response_action__adaptation_priority, suppression_requirement, 2031, 0.49).
narrative_ontology:measurement_basis(cra_adapt_su_t2031, projected).
narrative_ontology:measurement(cra_adapt_su_t2035, climate_response_action__adaptation_priority, suppression_requirement, 2035, 0.5).
narrative_ontology:measurement_basis(cra_adapt_su_t2035, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, degrowth_transformation).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate response' decomposes into three structurally distinct constraints — one per reading of the kernel — each with its own epsilon, beneficiary/victim sets, and classification; they form a constraint family linked here. mitigation_priority is the historically upstream reading (its temperature targets anchor the treaty text this reading's finance architecture cites); adaptation_priority exerts downstream pressure on it through infrastructure lock-in and through the legitimacy cost the accepted-warming baseline imposes on abatement-first politics. degrowth_transformation shares this reading's critique of growth-framed response but locates extraction in resource throughput itself rather than in protection finance, so the two coexist without direct structural pressure in either direction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
