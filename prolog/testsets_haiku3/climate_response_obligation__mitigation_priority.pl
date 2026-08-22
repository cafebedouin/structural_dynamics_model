% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Rapid Decarbonization Obligation: Mitigation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of the climate response obligation
 *   asserts that intergenerational justice requires rapid decarbonization to
 *   prevent future harm, even at substantial cost to current workers, fossil
 *   capital, and Global North economies. The standing arrangement under
 *   assessment is the global response to climate change — the set of
 *   policies, investments, and regulatory frameworks that determine emissions
 *   reductions and climate adaptation. This constraint evaluates that
 *   arrangement through the lens of mitigation-priority normative
 *   commitments: that future generations are primary beneficiaries (their
 *   welfare depends on warming prevention), that current generations are
 *   payers (they bear transition costs), and that the Global North bears
 *   disproportionate burdens due to historical responsibility. The constraint
 *   is CLAIMED as tangled_rope (real coordination problem solved, but
 *   asymmetric extraction from payers) while authored metrics reflect
 *   substantial extractiveness (0.68), high suppression (0.71), and moderate
 *   theater (0.42) — indicating active enforcement machinery defending the
 *   obligation against alternatives.
 *
 * KEY AGENTS:
 *   - Future generations: primary beneficiary, voiceless, trapped in consequence of current emissions policy
 *   - Climate-vulnerable populations (Global South): immediate beneficiary, constrained, bears costs of both mitigation and adaptation failure
 *   - Current-generation workers (fossil fuel dependent): primary payer, constrained by labor-market immobility, bears transition cost asymmetrically
 *   - Fossil fuel capital: powerful payer, faces stranded assets, constrained exit due to policy lock-in
 *   - Global North governments: institutional agenda-setter, bears pressure from all seats, faces cost-shifting tension
 *   - Climate advocacy movements: organized agenda-setter, articulates intergenerational justice frame, mobilizes political pressure
 *   - Adaptation-priority and degrowth advocates: excluded from dominant policy forums, hold contested alternative readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.71).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Rapid Decarbonization Obligation: Mitigation Priority Reading").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, 'f6f2c743-cc9d-4091-872c-4ea0ffb289f3').
narrative_ontology:cs_kernel_codification('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', fixed_text).
narrative_ontology:cs_authority_grounding('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', lineage).
narrative_ontology:cs_interpretation_layer_present('f6f2c743-cc9d-4091-872c-4ea0ffb289f3').
narrative_ontology:cs_reading_relation('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', foundational, harm_prevention_through_mitigation_primary).
narrative_ontology:cs_axiom_status(harm_prevention_through_mitigation_primary, holdable).
narrative_ontology:cs_axiom_grounding('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', harm_prevention_through_mitigation_primary, deontological).
narrative_ontology:cs_axiom('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', foundational, intergenerational_justice_requires_minimizing_warming).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_minimizing_warming, holdable).
narrative_ontology:cs_axiom_grounding('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', intergenerational_justice_requires_minimizing_warming, deontological).
narrative_ontology:cs_reference_frame('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', precaution_and_obligation_to_future).
narrative_ontology:cs_drift_state('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', contemporary_climate_emergency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f6f2c743-cc9d-4091-872c-4ea0ffb289f3', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, current_generation_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_capital).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_transition_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit a climate system shaped by current mitigation policy choices. Cannot participate in today's decisions; their welfare depends entirely on actions taken now to minimize warming. Benefit from rapid decarbonization through reduced climate catastrophe risk, but face implementation costs they did not incur and cannot influence.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Face immediate climate risks (sea-level rise, drought, extreme weather) that are already occurring. Benefit from mitigation that slows further warming. Disproportionately located in Global South and low-income regions; have minimal historical responsibility for cumulative emissions but suffer primary harm.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, global).

% Bear immediate transition costs: job displacement in fossil fuel industries, retraining expenses, wage pressures during sectoral shift, higher energy costs during infrastructure transition. Their exit options are constrained by labor market immobility, geographic dependence on fossil fuel economies, and credential lock-in. They bear costs on behalf of future generations they will not directly benefit from.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, current_generation_workers, payer,
    moderate, biographical, constrained, national).

% Faces stranded assets: reserves that cannot be extracted under rapid decarbonization; infrastructure (pipelines, refineries, power plants) rendered economically obsolete; market valuation collapse. The mitigation constraint forces a write-down of asset value without compensation and limits the exit window to liquidate holdings at full price. Capital can lobby and litigate but cannot fully escape the constraint once policy is enacted.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_capital, payer,
    powerful, biographical, constrained, global).

% Bear disproportionate mitigation costs because: (1) they have higher per-capita emissions to reduce, (2) they have capital to finance transition but lack motivation to self-impose costs, (3) historical responsibility for cumulative atmospheric CO2 creates a climate-justice claim for them to lead mitigation. They face coordinated pressure from vulnerable populations and future-focused movements but have the institutional power to delay or water down commitments.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_economies, payer,
    organized, biographical, constrained, continental).

% Articulates the mitigation-priority reading: that intergenerational justice requires preventing harm through rapid decarbonization rather than accepting warming and adapting. Sets the normative frame, mobilizes constituencies, organizes pressure campaigns, litigates in domestic and international venues. Does NOT directly set policy but shapes the constraint's legitimacy narrative and political feasibility.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_advocacy_movements, agenda_setter,
    organized, generational, mobile, global).

% Enact and enforce mitigation policy: carbon taxes, emissions trading systems, fossil fuel phase-outs, renewable subsidies, grid decarbonization mandates. Face cross-pressures: accountability to future generations (and vulnerable present populations), pressure from fossil capital, and domestic worker/industry resistance. Their exit is constrained by international treaties, market dynamics, and the scientific consensus that deep decarbonization is necessary.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% IPCC, UNFCCC, regional bodies coordinate scientific consensus and frame policy targets (1.5°C, net-zero 2050). They do not enforce but legitimize the mitigation-priority reading through authority-grounding in climate science. Their role is to transform climate physics into intergenerational justice narratives that become binding political commitments.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, international_climate_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Argues that rapid mitigation is economically infeasible and that resources should prioritize adaptation to warming that is already locked in. Would contest the mitigation-priority framing if seated in climate policy design; currently marginalized in high-stakes forums where the mitigation reading dominates but holds significant influence in some developing-nation governments and business sectors.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, adaptation_advocacy, excluded,
    organized, biographical, mobile, global).

% Argues that mitigation within growth frameworks is impossible; that only material reduction (degrowth) can meet planetary boundaries. Their reading is excluded from mainstream climate policy because it challenges the growth assumption central to current economic frameworks. Present in academic and activist spaces but not in governmental climate commissions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, degrowth_critics, excluded,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, climate_advocacy_movements).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: no single nation has incentive to bear unilateral decarbonization costs if others free-ride on reduced atmospheric CO2; rapid global mitigation requires coordination to (1) distribute costs fairly across emitters, (2) provide capital to developing nations for clean energy transition, (3) enforce commitments through market and regulatory mechanisms, (4) shift investment away from fossil infrastructure toward renewable systems. Without coordination, each actor defaults to delayed action.
% TRANSFER_FUNCTION: Transfers costs from future generations and climate-vulnerable populations (primary beneficiaries, currently voiceless) to current-generation workers, fossil fuel capital, and Global North governments (primary payers). The transfer operates through: carbon pricing (money to green funds), stranded-asset losses (capital write-downs), labor-market disruption (worker retraining costs), energy-cost increases (consumer burden), and opportunity costs (foregone fossil fuel revenues).
% ABSENT_VOICES: Future generations cannot participate in climate policy design; their interests are represented through proxy advocates (climate movements, youth strikes, IPCC scientific consensus) but lack direct voice. Fossil fuel workers in developing nations face double extraction (cost of transition plus loss of revenue that funded development). Adaptation-priority advocates are sidelined in mitigation-dominated forums despite holding legitimate empirical claims about feasibility and cost-effectiveness in high-warming scenarios. Degrowth advocates are entirely absent from mainstream policy design, relegated to academic and activist spaces.
% DISAPPEARANCE_RATIONALE: If the mitigation obligation vanished, current policy would revert to incremental carbon reduction or adaptation-focused investment; fossil fuel extraction would continue at higher rates; stranded assets would be recovered; worker displacement would be delayed by decades; future generations would inherit a warmer climate with compounding lock-in costs (e.g., thawing permafrost, tipping points). The constraint's disappearance would reorganize the global energy system, financial markets, and intergenerational risk allocation entirely.
% FOUNDING_PROBLEM: The founding problem of the mitigation-priority reading is: atmospheric CO2 accumulation from 150+ years of fossil-fuel-powered industrialization creates committed warming (lock-in even if emissions stop today) and non-linear climate tipping points; preventing the worst outcomes requires rapidly reducing emissions to zero while the climate system still responds to emissions cuts, rather than waiting and later adapting to unavoidable warming.
% FOUNDING_PROBLEM_CORROBORATION: IPCC scientific consensus (independent from fossil interests, funded by governments) attests the founding problem is live and worsening: each year of delay reduces the feasibility window and increases future costs. Climate-vulnerable nations attest through negotiating positions at UNFCCC that rapid mitigation is their survival interest. Fossil fuel industry disputes the framing (argues adaptation is cheaper, mitigation is premature) but does not dispute the underlying physics — they dispute the normative weight given to future harm. Independent economic analyses from outside the benefiting parties (Stern Review, NGFS climate scenarios) corroborate that unmitigated warming imposes far larger costs than rapid decarbonization, validating the founding problem's urgency.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 (early interval, mitigation optional and marginal) to 0.68 (mature interval, mitigation mandatory and locked-in), plateauing as policy crystallizes and becomes path-dependent. This rise tracks the constraint's hardening from aspirational commitment (Paris Agreement early years) to enforcement regime (carbon pricing, phase-outs, grid mandates, stranded-asset litigation). Suppression is initially lower (0.54) because fossil interests can still lobby and delay; it rises to 0.71 as the mitigation frame becomes institutionalized in courts, regulatory bodies, and investment mandates — alternative readings (adaptation, degrowth) are increasingly sidelined rather than openly debated. Theater is moderate throughout (0.25–0.42) because genuine coordination benefits exist (global emissions reduction is a real collective-action problem), but a growing share of enforcement energy goes toward suppressing fossil alternatives rather than solving the underlying climate coordination itself — the ratio rises from 0.25 (mostly functional) to 0.42 (substantial performative maintenance of the mitigation-only frame against alternatives). The time-series plateaus around t=25 as the constraint matures into institutional form: extractiveness stabilizes because payers are locked in, suppression stabilizes because alternatives are normatively marginalized, theater stabilizes because the mitigation frame is now the unquestioned default.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (advocacy movements, national governments, international bodies) experience the constraint as legitimate collective action solving a genuine coordination problem — future generations depend on mitigation, vulnerable populations need prevention, the global North has historical responsibility to lead. The payer seats (current workers, fossil capital) experience it as extraction imposed without their negotiation or consent: workers face job loss for a benefit they will not directly consume; fossil capital faces value destruction without compensation; Global North governments face unilateral burden while emerging economies argue for development rights. The beneficiary seats (future generations, vulnerable populations) experience voicelessness — they cannot negotiate terms, only receive what current majorities decide to give them. This perspectival divergence is structural, not eliminable by better communication: the mitigation-priority frame genuinely excludes the adaptation and degrowth readings from high-stakes forums, and workers/capital genuinely bear costs that future beneficiaries cannot reciprocate.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and vulnerable populations are full beneficiaries (d ≈ 0.0 in the mitigation frame) — they receive substantial welfare improvement (reduced catastrophic warming), bear no decision-making costs, and have trapped exit (cannot decline the benefit). Current workers are targets (d ≈ 0.8–0.9) — they pay through job loss and retraining burdens, have constrained exit (cannot easily retrain or relocate), and the constraint's benefits accrue to voiceless future agents they cannot negotiate with. Fossil capital is a high-d target (d ≈ 0.85) — it bears stranded-asset costs, has constrained exit (cannot accelerate extraction enough to recover assets before phase-out), and cannot fully exit the jurisdiction where mitigation policy applies. Global North governments sit near middle (d ≈ 0.55–0.65) — they benefit from reduced climate risk and moral-leadership legitimacy, but pay through political cost (worker discontent), investment burden, and comparative disadvantage if other nations delay. Advocacy movements have near-zero directionality (d ≈ 0.05) — they benefit from policy alignment without bearing transition costs. The engine derives these d values from beneficiary/victim declarations and exit options; the divergence produces per-seat classifications that should show tangled-rope for beneficiary seats (real coordination with side benefits) and snare-approaching for victim seats (extraction with minimal coordination benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   The mitigation-priority reading runs a mandatrophy risk: if the founding problem (need for rapid decarbonization) becomes obsolete while the constraint persists, the arrangement would shift from justified extraction (current payers bear costs for real future benefit) to zombie extraction (the constraint persists from institutional inertia, not because the founding coordination problem is live). Scenarios triggering this risk: (1) carbon-removal technology breakthrough making rapid emission cuts unnecessary — then the mitigation obligation becomes an artifact; (2) observed climate tipping points causing adaptation-focused response (spending shifts from prevention to resilience) — the mandate for preventing harm is retroactively invalidated; (3) political shift toward degrowth, making mitigation-within-growth obsolete as a frame — the constraint's legitimacy dissolves in favor of the degrowth reading. Current status: founding problem is CONTESTED (not unanimously live, not universally dead), which is why the mitigation reading itself is contested. The constraint avoids classical mandatrophy because the founding problem's urgency is still disputed; a consensus shift (either toward adaptation being sufficient or toward degrowth being necessary) would flip the status to 'dead' and activate mandatrophy signals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_vs_adaptation_boundary,
    'Is rapid decarbonization empirically necessary to prevent catastrophic tipping points, or is high-cost mitigation offsetable by future adaptation technologies?',
    'Empirical observation: (1) detection of triggering mechanisms for Amazon dieback, ice-sheet collapse, or permafrost runaway — events that would confirm lock-in and justify prevention over adaptation; (2) technological breakthrough in carbon removal at scale (<$100/ton) — would suggest adaptation-heavy strategies are viable; (3) 10-year delay in peak warming — would validate adaptation-priority hypothesis.',
    'If tipping points are empirically imminent and irreversible, mitigation is categorically non-negotiable and the constraint is structural necessity, not extraction. If adaptation technologies mature faster than expected, the mitigation reading becomes a cost-inefficient choice and the constraint shifts toward snare (unjustified extraction masquerading as necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_vs_adaptation_boundary, empirical, 'Whether rapid mitigation is empirically necessary or strategically chosen given available alternatives.').

omega_variable(
    intergenerational_justice_grounding,
    'Does intergenerational justice specifically require preventing harm through mitigation, or is it equally satisfied by adaptation that keeps living standards stable across generations?',
    'Philosophical and legal analysis: (1) do intergenerational justice frameworks ground obligations in outcome-minimization (harm prevention) or in fair-process distribution (intergenerational cost-sharing); (2) what do constitutional courts and international bodies declare when asked to adjudicate intergenerational rights — do they mandate mitigation or permit adaptation-only strategies?',
    'If justice requires harm prevention specifically, the mitigation-priority reading is grounded in an irreducible normative commitment. If justice permits adaptation-focused strategies that distribute costs fairly, the mitigation reading becomes one option among several, not a categorical imperative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_justice_grounding, conceptual, 'What philosophical axioms ground the intergenerational justice frame.').

omega_variable(
    beneficiary_discounting_and_voicelessness,
    'Does the powerlessness of future generations create a bias in mitigation policy that extracts from current payers beyond what a hypothetical intergenerational negotiation would yield?',
    'Counterfactual analysis: (1) what terms would future generations (if represented) negotiate for current burden-sharing — would they accept higher present costs to avoid their own lock-in, or choose lower present costs and higher adaptation burdens; (2) discount-rate sensitivity: as discount rates rise (prioritizing present welfare), does mitigation remain justified by future-benefit calculus, or does it become extraction disguised as altruism?',
    'If future generations would rationally choose mitigation even in a fair negotiation, the constraint is genuine coordination. If they would choose lower present mitigation and higher future adaptation (to preserve current living standards), then current mitigation policy may extract from current payers on behalf of a voiceless beneficiary who never consented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_discounting_and_voicelessness, preference, 'Whether voicelessness of beneficiaries creates extraction bias in mitigation obligations.').

omega_variable(
    reading_contest_on_referent,
    'This constraint is one reading of a contested kernel (climate_response_obligation). The three readings (mitigation_priority, adaptation_priority, degrowth_reading) each assess the SAME referent — the standing climate response arrangement — but evaluate its extractiveness through different normative lenses. Mitigation-priority reading: ε=0.68 because rapid decarbonization extracts from current workers/capital while benefiting future generations (an asymmetry the reading endorses as justice). Adaptation-priority reading would assess: same arrangement, lower ε (adaptation is cheaper near-term, less burdensome). Degrowth reading would assess: same arrangement, higher ε (materialist logic shows mitigation within growth is an extractive illusion; true decarbonization requires degrowth). Which reading''s assessment of this standing arrangement is structurally sound?',
    'Each reading''s ε derives from its own normative framework (justice, efficiency, ecological limits). The readings are not competing empirical claims — they are different commitments to which normative dimension matters most. No single omega resolves this; the question marks the irreducible contest.',
    'This omega documents that THIS constraint (mitigation_priority) is one reading of a kernel, not a universal fact. The ε=0.68 is valid WITHIN the mitigation-priority normative frame (intergenerational justice is primary; extraction is justified). Sibling readings would author different ε values for the same standing arrangement. The constraint family emerges from this irreducible reading contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_on_referent, conceptual, 'The mitigation-priority reading is one normative commitment among contested alternatives; its ε is reading-indexed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_response_obligation__mitigation_priority, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__mitigation_priority, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_obligation__mitigation_priority, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_response_obligation__mitigation_priority, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__mitigation_priority, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_response_obligation__mitigation_priority, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__mitigation_priority, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_obligation__mitigation_priority, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_response_obligation__mitigation_priority, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__mitigation_priority, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_response_obligation__mitigation_priority, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__mitigation_priority, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_obligation__mitigation_priority, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_response_obligation__mitigation_priority, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__mitigation_priority, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% The climate_response_obligation kernel decomposes into three structurally distinct constraints corresponding to three contested readings: mitigation_priority (prevent future harm through rapid decarbonization, ε=0.68), adaptation_priority (accept warming, invest in resilience, ε estimated lower ~0.35–0.45), degrowth_reading (reduce material throughput, ε estimated higher ~0.75–0.82). Each reading has different beneficiary/victim structures, different founding problems, and different normative grounds. The readings are linked via network.affects_constraints to show family membership; they are separate stories because their ε values differ structurally (not just observationally) based on which normative frame is primary. No single constraint can model the contest; the constraint family preserves the irreducible pluralism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__mitigation_priority, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
