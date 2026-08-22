% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Adaptation-Priority Reading of the Climate Response Imperative
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The standing arrangement this story is about: the operative content of
 *   global climate response as practiced — nationally determined pledges
 *   without binding abatement schedules, adaptation finance channels,
 *   national adaptation plans, and defensive infrastructure spending in
 *   exposed regions — while deep mitigation is carried as aspiration:
 *   net-zero targets dated past incumbent officeholders' tenures, technology
 *   optimism, voluntary offset markets. The arrangement delivers real
 *   protection where it reaches, and that delivery is genuine coordination;
 *   the same structure dissolves abatement obligation and routes the costs of
 *   others' emissions back onto the exposed, which is the asymmetry.
 *   Claim/metric independence is preserved: claimed_type tangled_rope is
 *   asserted from structure (a real coordination function fused with an
 *   asymmetric burden), while the metrics describe observed operation; the
 *   engine computes per-seat classifications and any divergence between claim
 *   and computed type is the datum, not an error to reconcile. FAMILY NOTE:
 *   this file is one reading of the climate_response_imperative kernel and
 *   authors only that reading — the sibling readings
 *   (mitigation_priority_reading, degrowth_reading) are separate constraints
 *   in separate files with different epsilons and victim sets, linked via
 *   network.affects_constraints; per the epsilon-invariance principle this
 *   story hedges nothing across them. INTERVAL MAPPING: t=0 approximates 2009
 *   (Copenhagen Accord — the binding-mitigation track collapses and
 *   adaptation rises as the operative program); t=36 approximates 2025. KEY
 *   AGENTS (by structural relationship): -
 *   high_emitting_industrial_economies: Primary beneficiary and
 *   co-agenda-setter (institutional/arbitrage) — collects avoided-abatement
 *   rents, controls finance terms - fossil_fuel_extractors: Secondary
 *   beneficiary (powerful/arbitrage) — demand sustained by deferred
 *   mitigation - adaptation_engineering_contractors: Incidental beneficiary
 *   (powerful/mobile) — collects defensive-spending contracts -
 *   climate_exposed_developing_nations: Primary payer (organized/trapped) —
 *   bears capital requirements and damages - low_income_exposed_households:
 *   Deepest payer (powerless/constrained) — absorbs impacts directly -
 *   multilateral_climate_funds: Administrator (institutional/identity_locked)
 *   — allocates the envelope it inherits - climate_justice_movements:
 *   Excluded claimant (organized/constrained) — litigates and campaigns
 *   outside voting seats - future_generations_of_exposed_regions: Unseated
 *   bearer (powerless/trapped; non-agent entry kept for completeness) —
 *   inherits committed warming - ipcc_assessment_process: Analytical observer
 *   (institutional/analytical) — documents gaps, binds nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.74).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.62).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Adaptation-Priority Reading of the Climate Response Imperative").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, 'a9533cb9-7420-4031-be48-ed43feaea47e').
narrative_ontology:cs_kernel_codification('a9533cb9-7420-4031-be48-ed43feaea47e', distributed).
narrative_ontology:cs_authority_grounding('a9533cb9-7420-4031-be48-ed43feaea47e', distributed).
narrative_ontology:cs_reading_relation('a9533cb9-7420-4031-be48-ed43feaea47e', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9533cb9-7420-4031-be48-ed43feaea47e', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('a9533cb9-7420-4031-be48-ed43feaea47e', foundational, present_exposed_protection_primacy).
narrative_ontology:cs_axiom_status(present_exposed_protection_primacy, holdable).
narrative_ontology:cs_axiom_grounding('a9533cb9-7420-4031-be48-ed43feaea47e', present_exposed_protection_primacy, deontological).
narrative_ontology:cs_axiom('a9533cb9-7420-4031-be48-ed43feaea47e', foundational, deferred_mitigation_actionability).
narrative_ontology:cs_axiom_status(deferred_mitigation_actionability, holdable).
narrative_ontology:cs_axiom_grounding('a9533cb9-7420-4031-be48-ed43feaea47e', deferred_mitigation_actionability, empirically_contingent).
narrative_ontology:cs_reference_frame('a9533cb9-7420-4031-be48-ed43feaea47e', resilience_first_protection_framework).
narrative_ontology:cs_drift_state('a9533cb9-7420-4031-be48-ed43feaea47e', post_ar6_adaptation_limits_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a9533cb9-7420-4031-be48-ed43feaea47e', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, high_emitting_industrial_economies).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, fossil_fuel_extractors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, adaptation_engineering_contractors).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, climate_exposed_developing_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, low_income_exposed_households).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations_of_exposed_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the shape of climate response through diplomatic weight in treaty negotiations, control of multilateral fund replenishment, and domestic legislation. Direct public finance toward visible resilience projects at home and abroad while keeping abatement obligations voluntary and long-dated. Carbon-intensive production continues; fiscal exposure to abatement stays low. When pressure builds they can reframe pledges, adjust finance envelopes, or reroute trade — few outcomes force their hand.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, high_emitting_industrial_economies, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, high_emitting_industrial_economies, agenda_setter).

% Produce and sell fuels whose demand is sustained by the absence of binding abatement schedules. Expansion plans and asset valuations assume decades of continued throughput. They maintain a heavy presence in negotiation spaces and gain from policy stability that defers demand destruction; portfolio diversification is possible but slow against asset lifetimes.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, fossil_fuel_extractors, beneficiary,
    powerful, biographical, arbitrage, global).

% Design and build sea walls, drainage, resilient housing, water systems, and early-warning infrastructure financed by adaptation budgets. Revenue scales with the project pipeline wherever exposure is recognized, and client-concentration risk is low because exposure is global. Their commercial interest attaches to the volume of defensive spending, independent of what happens to upstream causes.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, adaptation_engineering_contractors, beneficiary,
    powerful, biographical, mobile, continental).

% Face immediate bills for coastal defense, agricultural transition, relocation, and disaster response that exceed domestic revenue many times over. Delivered international finance arrives as loans more often than grants, so preparing for damages incurred from others' emissions adds to sovereign debt. They negotiate as blocs but cannot leave their geography, their climate exposure, or the negotiating table; walking out costs them the finance channel itself.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_exposed_developing_nations, payer,
    organized, generational, trapped, regional).

% Live in flood plains, drought margins, and informal coastal settlements. Crop failure, inundation, and heat arrive directly, with insurance thin or absent. Migration is the main adaptation available to them and it is priced beyond reach or blocked at borders. Defensive projects reach them late, partially, or not at all.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, low_income_exposed_households, payer,
    powerless, immediate, constrained, local).

% Approve and disburse what donor states pledge, through project pipelines, accreditation rules, and country programs. Founding mandates balance mitigation and adaptation equally, but staffing, procedures, and pipelines have grown around adaptation projects because those are what get proposed and funded. They allocate; they do not set the size of the envelope, and their procedures now presuppose the adaptation-project form.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, multilateral_climate_funds, agenda_setter,
    institutional, generational, identity_locked, global).

% Organize litigation, street pressure, and loss-and-damage campaigns demanding liability, grant-based finance, and binding abatement. They hold observer badges at negotiations with restricted speaking slots and no vote on fund boards; their proposals circulate as side events, amicus briefs, and demonstrations outside the plenary halls.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_justice_movements, excluded,
    organized, generational, constrained, global).

% Will inherit whatever warming is committed and whatever defenses were or were not built. They are invoked in preamble language and youth constituencies but hold no delegation, no vote, and no standing to bring claims against deferred abatement or unpaid damages.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations_of_exposed_regions, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_response_imperative__adaptation_priority_reading, future_generations_of_exposed_regions).

% Synthesizes the physical and social science of impacts, adaptation limits, and emission trajectories into assessment reports. Its findings document the distance between pledges and pathways and between adaptation needs and delivered finance; the reports inform negotiations but compel nothing.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, ipcc_assessment_process, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__adaptation_priority_reading, high_emitting_industrial_economies).
narrative_ontology:fixing_cost_class(climate_response_imperative__adaptation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the real problem that physical climate impacts require anticipatory, place-based investment: pooled finance, shared engineering standards, early-warning systems, and national adaptation plans give exposed regions capabilities no single actor could build alone.
% TRANSFER_FUNCTION: Moves adaptation finance and technical assistance toward exposed-region projects, and moves the costs of ongoing emissions — damages, displacement, compounding losses — onto the populations and states least able to carry them; simultaneously converts what would be a shared mitigation burden into a deferred, unpriced externality absorbed by the exposed.
% ABSENT_VOICES: Future generations hold no seat; sub-national exposed communities are spoken for by state delegations balancing their claims against donor relations; loss-and-damage claimants have no adjudicative forum. Consensus in negotiated texts arises partly because the seats that would object loudest are outside the room or muted by finance dependence.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, adaptation pipelines would halt mid-construction, exposed states would lose the only functioning international finance channel for defense against impacts already arriving, and donor economies would lose the frame that substitutes for abatement — the mitigation contest would reopen immediately as the primary terrain, with no protective infrastructure in place during the transition.
% FOUNDING_PROBLEM: Early climate governance faced a sequencing dilemma: impacts were already arriving in exposed regions while mitigation required coordinated global action with diffuse, delayed benefits. The adaptation-priority arrangement was built to deliver tangible near-term protection to exposed populations and to give climate politics a concrete, place-based program while mitigation technology and political consensus matured.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and the UNEP Adaptation Gap Report — both outside the benefiting parties — corroborate that adaptation needs are real, growing, and grossly underfunded, supporting the founding problem's liveness. The same bodies, plus independent climate-finance trackers and recent international court opinions on states' mitigation obligations, corroborate that current arrangements deliver a fraction of stated needs while the mitigation gap widens, supporting the shifted-function reading. Vulnerable-nation negotiating blocs attest both sides of the contest from inside.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.74 is authored over the standing arrangement — adaptation-operational, mitigation-aspirational climate governance — as this reading assesses it: the arrangement converts a shared atmospheric burden into localized damage costs borne by those least responsible, and once finance composition (loans over grants) and the widening mitigation gap are counted, that conversion is the arrangement's principal output. Suppression 0.62 is a raw structural property and is deliberately UNSCALED — it registers procedural and discursive force (agenda control over what counts as 'response', loan conditionality, donor forum-shopping), not physical coercion; only extractiveness is scaled by the engine. Theater_ratio 0.45: a large share of the arrangement's visible activity is pledge performance (net-zero declarations dated beyond incumbency, adaptation plans without budget lines, communiqué language), while defensive construction and disbursement are real; the ratio tracks the pledge layer's share of total activity. Accessibility_collapse 0.38: alternatives are not collapsed — the mitigation-priority and degrowth readings remain live, litigation proceeds, blocs threaten walkouts — but every alternative hits the same finance-dependence wall. Resistance 0.65: international and domestic litigation, loss-and-damage campaigns, veto coalitions in fund boards. COALITION NOTE: the payer blocs are organized, not powerless — G77/AOSIS/V20-style coordination is real coalition power — but it is blunted by finance dependence: the credible threat disciplining bloc cohesion is exclusion from the channel. MEASUREMENTS share one grid (t = 0,6,12,18,24,30,36): extractiveness and suppression rise together as the pledge layer thickens and finance hardens into debt; theater climbs with the pledge layer. The series is monotonic, not cyclical; the COP-cycle rhythm (pledge spikes, implementation troughs) is real but averages out at this grain.
 *
 * PERSPECTIVAL GAP:
 *   Donor-economy and exposed-nation seats compute differently from identical treaty text. From the donor seat the arrangement is stewardship: finite budgets, sequenced priorities, adaptation as the achievable good. From the exposed-nation seat the same structure is payment without representation: damages from others' emissions, then loans to defend against the next tranche. The contractor seat sees only a growing market; the fund seat sees mandate fulfillment measured in approvals; the movement seat sees a closed room. None of these perceptions is authored as a type — the engine derives each seat's classification from role, power, and exit; the divergence between the donor-computed type and the payer-computed type is the perspectival fact this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low d: donor economies sit nearest the subsidy end (they collect avoided-abatement rents and set terms, with arbitrage-grade exit), extractors next (arbitrage but asset-bound), contractors lowest-stakes (mobile, incidental). Payers derive high d: exposed developing nations approach the full-target end — trapped exit, since geography is not exitable — with organized-but-blunted power; households are pinned at the target end by constrained exit. The funds' identity_locked exit marks institutional fusion with the adaptation-project form; they neither capture nor bear the arrangement's flows, so they are not the receipt seat. No directionality_overrides are authored: the derivation from declared roles and exits reproduces the structural relationships without correction, and an override keyed only to a power atom would blur the donor/fund distinction the secondary-role declaration already carries.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards both misreadings. Reading the arrangement as pure rope — 'adaptation is just help where help is needed' — misses that the same structure dissolves abatement obligation and reverses the direction of owed transfers. Reading it as pure snare — 'adaptation is the cover story' — misses the delivered seawalls, warning systems, and resistant crops that vanish if the arrangement does; hence disappearance_verdict world_rearranges, not world_unchanged. Mandatrophy is NOT resolved: the founding problem (protect the exposed while mitigation matures) retains a live core, because protection needs grow with every year of committed warming. What has rotted is the 'while mitigation matures' clause — maturity is perpetually redefined, and that clause is where the burden-shifting lives. Watch-item: the pledge subsystem is drifting toward piton dynamics (rising theater, maintenance by repetition); if disbursement ever decouples entirely from needs assessment, the whole arrangement reclassifies downward. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no dead-mandate zombie flag fires, correctly — the mandate is half-alive, and contested status is the honest signature of a coordination function fused with an extraction function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the climate_response_imperative kernel: does the adaptation-priority reading, the mitigation-priority reading, or the degrowth reading correctly specify the imperative''s structure, and at exactly which structural element do the readings part ways?',
    'Cross-reading comparison at engine level plus political-settlement evidence: which victim sets each reading generates, which reading''s predictions survive contact with observed finance flows, debt trajectories, and impact data.',
    'Each sibling reading instantiates a different constraint with a different victim set and a different epsilon over the same treaty landscape; resolving the contest rewires the family''s network edges and can move this reading''s payer seats into beneficiary seats under a rival reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Which reading of the climate-response kernel this constraint instantiates and what sibling readings would structurally change.').

omega_variable(
    victim_boundary_capital_trap,
    'Are present-day developing nations victims of this arrangement through adaptation capital requirements they cannot meet, or beneficiaries of adaptation flows they would otherwise lack entirely?',
    'Counterfactual finance accounting: delivered adaptation finance versus independently assessed needs, grant-versus-loan composition, debt-service ratios of exposed states, and whether net transfers after debt service are positive or negative.',
    'If net transfers are negative once debt service is counted, the victim designation holds and the vicious-circle delta stands; if strongly positive, the seat flips toward beneficiary and the arrangement reads as under-delivered coordination rather than reversed transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_boundary_capital_trap, empirical, 'Location of the victim/beneficiary boundary for exposed developing nations under this reading.').

omega_variable(
    adaptation_hard_limits_test,
    'Can resilience-building absorb committed warming in exposed regions, or do hard adaptation limits bind before mid-century?',
    'Observed loss events exceeding engineered adaptive capacity, managed-retreat scale versus finance delivered, and the next IPCC assessment cycle''s treatment of residual damages.',
    'If hard limits bind broadly, this reading''s actionability premise fails, the coordination cover thins, and the arrangement trends toward pure extraction maintained by agenda control alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_hard_limits_test, empirical, 'Empirical test of the premise that adaptation can carry the response while mitigation stays aspirational.').

omega_variable(
    suppression_internalization_in_payer_blocs,
    'Is the arrangement''s hold on exposed-nation seats purely structural (finance dependence, debt leverage) or partly internalized (delegations adopting the adaptation-first frame as their own realism)?',
    'Post-delinking trajectory: if exposed-nation coalitions sustain binding-mitigation demands when finance is decoupled from frame acceptance, the suppression was structural; if the frame persists after the lever is removed, part of it was internalized.',
    'An internalized component raises the arrangement''s effective suppression above the structural measure, because the payer seats carry the frame with them even where formal barriers fall.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_in_payer_blocs, empirical, 'Structural versus internalized composition of the suppression holding payer blocs in the adaptation-first frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t6, climate_response_imperative__adaptation_priority_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(clim_tr_t12, climate_response_imperative__adaptation_priority_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(clim_tr_t18, climate_response_imperative__adaptation_priority_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(clim_tr_t24, climate_response_imperative__adaptation_priority_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__adaptation_priority_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(clim_tr_t36, climate_response_imperative__adaptation_priority_reading, theater_ratio, 36, 0.45).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(clim_be_t6, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement(clim_be_t12, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(clim_be_t18, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(clim_be_t24, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(clim_be_t36, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 36, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(clim_su_t6, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(clim_su_t12, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(clim_su_t18, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 18, 0.56).
narrative_ontology:measurement(clim_su_t24, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(clim_su_t36, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 36, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'climate response'. The kernel climate_response_imperative decomposes into three structurally distinct readings, each a separate file with its own epsilon, victim set, and classification: this adaptation-priority reading (epsilon 0.74; victims include present-day exposed developing nations via capital requirements they cannot meet), the mitigation-priority reading (different primacy assignment, different victim set centered on future generations and atmospheric-budget overrun), and the degrowth reading (structural-transformation premise, victim set centered on Global North consumption externalities). The readings differ in epsilon because they instantiate different constraints over the same treaty landscape — not one constraint viewed from angles. This file links both siblings via affects_constraints; per the epsilon-invariance principle no story in the family hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
