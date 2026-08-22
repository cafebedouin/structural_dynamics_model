% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__adaptation_priority, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Climate Adaptation Priority: Accept 2-3°C Warming, Invest in Resilience Over Prevention
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested climate response
 *   kernel. The adaptation-priority reading frames climate policy around
 *   accepting 2-3°C warming as inevitable and investing heavily in resilience
 *   infrastructure rather than pursuing rapid decarbonization. The reading is
 *   endorsed by wealthy-nation policymakers, incumbent fossil-fuel capital,
 *   and adaptation-finance intermediaries. It is contested by climate
 *   scientists, the Global South, future-generations advocates, and
 *   ecological perspectives. The structural claim: this reading is a tangled
 *   rope that coordinates adaptation investment (genuine coordination
 *   problem: how to make communities resilient to warming impacts) while
 *   extracting by deferring prevention costs from the current generation and
 *   wealthy nations to future generations, the Global South, and
 *   climate-vulnerable populations. The constraint's authority structure
 *   grounds itself in economic pragmatism (prevention costs are too high) and
 *   technical capability (adaptation technology exists), which permits the
 *   beneficiary set to suppress alternative readings by framing them as
 *   economically impossible.
 *
 * KEY AGENTS:
 *   - wealthy_nations_current_generation: primary beneficiary, agenda-setter; sets policy frame through multilateral mechanisms and domestic climate policy
 *   - fossil_fuel_capital: beneficiary; operates under extended runway for existing assets and deferred write-downs
 *   - future_generations: primary victim; inherit warming and adaptation liability
 *   - global_south_nations: victim set; bear disproportionate climate impacts with minimal historical responsibility and constrained adaptation investment
 *   - climate_vulnerable_populations: victim set; face existential loss of livelihood and cultural identity under 2-3°C warming
 *   - adaptation_finance_brokers: secondary beneficiary; collect fees and organizational rents from decades of adaptation funding flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.82).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.71).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.82).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Climate Adaptation Priority: Accept 2-3°C Warming, Invest in Resilience Over Prevention").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, '42e6c878-9bd8-460f-b872-5e9ba9e85fff').
narrative_ontology:cs_kernel_codification('42e6c878-9bd8-460f-b872-5e9ba9e85fff', distributed).
narrative_ontology:cs_authority_grounding('42e6c878-9bd8-460f-b872-5e9ba9e85fff', extraction).
narrative_ontology:cs_interpretation_layer_present('42e6c878-9bd8-460f-b872-5e9ba9e85fff').
narrative_ontology:cs_reading_relation('42e6c878-9bd8-460f-b872-5e9ba9e85fff', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('42e6c878-9bd8-460f-b872-5e9ba9e85fff', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('42e6c878-9bd8-460f-b872-5e9ba9e85fff', foundational, warming_2_3c_inevitable).
narrative_ontology:cs_axiom_status(warming_2_3c_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('42e6c878-9bd8-460f-b872-5e9ba9e85fff', warming_2_3c_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('42e6c878-9bd8-460f-b872-5e9ba9e85fff', foundational, adaptation_sufficiency_for_resilience).
narrative_ontology:cs_axiom_status(adaptation_sufficiency_for_resilience, holdable).
narrative_ontology:cs_axiom_grounding('42e6c878-9bd8-460f-b872-5e9ba9e85fff', adaptation_sufficiency_for_resilience, empirically_contingent).
narrative_ontology:cs_axiom('42e6c878-9bd8-460f-b872-5e9ba9e85fff', secondary, prevention_economically_unfeasible).
narrative_ontology:cs_axiom_status(prevention_economically_unfeasible, holdable).
narrative_ontology:cs_axiom_grounding('42e6c878-9bd8-460f-b872-5e9ba9e85fff', prevention_economically_unfeasible, empirically_contingent).
narrative_ontology:cs_reference_frame('42e6c878-9bd8-460f-b872-5e9ba9e85fff', high_emission_compatible_development).
narrative_ontology:cs_drift_state('42e6c878-9bd8-460f-b872-5e9ba9e85fff', contemporary_2020s_empirical_challenge_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('42e6c878-9bd8-460f-b872-5e9ba9e85fff', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, wealthy_nations_current_generation).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_fuel_capital).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, industrial_agriculture).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, climate_vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_poor_wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, adaptation_finance_brokers).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, current_generation_poor_wealthy_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Framing adaptation-over-prevention allows the current generation in high-income nations to avoid the transition costs (energy retooling, workforce retraining, stranded fossil infrastructure write-downs) that rapid decarbonization would impose. They set the policy frame through multilateral climate negotiations, domestic energy policy, and funding allocation. They possess capital, technology access, and geographic advantage (adaptable agriculture, temperate climates, capital reserves for infrastructure). Their stated position: adaptation is pragmatic, prevention is economically ruinous.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_nations_current_generation, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, wealthy_nations_current_generation, agenda_setter).

% Adaptation framing defers the write-down of stranded assets (coal reserves, oil fields, refinery infrastructure). It permits extended operation of existing capital stock and delays the market shift toward renewables. Fossil firms profit from continued fuel sales during the adaptation window and from adaptation-related infrastructure investment (disaster-resilient roads, water treatment, backup power systems that may run on gas). Their preference: keep burning, invest in resilience to the burning's effects.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_fuel_capital, beneficiary,
    institutional, biographical, arbitrage, global).

% Bear the consequences of cumulative warming accepted today. At 2-3°C warming, they inherit: accelerated sea-level rise (island nations submerged, coastal cities uninhabitable), agricultural disruption in staple crop regions, intensified extremes (hurricanes, droughts), ecosystem collapse, and compounding resource scarcity. They have no choice, no exit, and no negotiating power in the current decision frame. Their interests are unrepresented in adaptation-priority decisions because they do not yet exist as economic actors.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Carry disproportionate climate impacts despite minimal historical contribution to emissions. They depend on agriculture, fisheries, and water systems most vulnerable to 2-3°C warming. Their capital stock is smaller and more geographically exposed than wealthy nations; adaptation investment requires external funding that the adaptation-priority frame routes to wealthy regions first. They argue for prevention but lack negotiating leverage; adaptation-priority policies convert them from negotiating parties to aid recipients, establishing dependency.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_nations, payer,
    organized, generational, trapped, global).

% Communities embedded in climate-sensitive livelihoods (subsistence farming, island fishing, pastoralism, tropical agriculture) face existential loss under 2-3°C warming. Their identity, social structure, and food security are inseparable from their environment. Exit is identity-dissolution. Adaptation framing presumes they can be relocated or retrained, which erases the cultural and relational loss. They experience the constraint as externally imposed acceptance of their dispossession.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_vulnerable_populations, payer,
    powerless, biographical, identity_locked, local).

% Would benefit from accelerated decarbonization mandates and technology transfer regimes. They are excluded from the adaptation-priority frame because that frame deprioritizes rapid energy transition. They lobby for mitigation-priority readings but face institutional resistance from incumbent fossil capital and governments committed to adaptation framing.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, renewable_energy_manufacturers, excluded,
    organized, biographical, constrained, global).

% Experience adaptation investment unevenly: wealthy regions build seawalls and flood-resistant infrastructure; poor regions are told to accept greater climate risk or self-fund protection. They avoid immediate transition costs but absorb climate impacts first and receive adaptation resources last. Structurally, they benefit from deferral of transition pain but are payers for the adaptation liability.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_generation_poor_wealthy_nations, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, current_generation_poor_wealthy_nations, beneficiary).

% Present empirical evidence that warming beyond 1.5-2°C crosses irreversible tipping points and that adaptation alone cannot offset cascading impacts. They are systematically excluded from policy framing by funders with vested interests in adaptation-priority readings. Their warnings are characterized as alarmism or cost-exaggeration by the adaptation frame's institutional defenders.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_scientists_mitigation_advocates, excluded,
    moderate, biographical, constrained, global).

% Financial institutions, consulting firms, and development banks that mediate adaptation funding flows (World Bank, regional development banks, climate finance vehicles). They charge fees, structure deals, and allocate capital. The adaptation-priority frame generates a massive, decades-long revenue stream for these intermediaries. They have structural incentive to perpetuate adaptation focus and oppose prevention mandates that would compress their revenue horizon.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, adaptation_finance_brokers, beneficiary,
    institutional, biographical, arbitrage, global).

% The UNFCCC, regional climate agreements, and national climate ministries administer and enforce the adaptation-priority frame through: funding allocation mechanisms, technology transfer terms, nationally determined contributions (NDCs) permitting high emissions with adaptation budgets, and narrative justification. They are staffed by officials from wealthy nations and actors with vested interests in incumbent systems. The apparatus sustains itself through continuous allocation of adaptation funds and organizational mandate expansion.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, international_climate_governance_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% The philosophical and ethical seat that asks: does the current generation have the right to foreclose prevention in exchange for accepting warming its descendants will bear? From this analytical position, the adaptation-priority reading violates intergenerational justice principles by converting a preventable harm into an inherited obligation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, observer_intergenerational_ethics, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, wealthy_nations_current_generation).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocation of climate investment capital toward adaptation infrastructure (flood management, drought-resistant agriculture, disaster response, resilience-building) rather than rapid decarbonization. Solves the coordination problem of who funds what and in what time frame; establishes a shared expectation that warming above 2°C is the baseline against which adaptation is planned.
% TRANSFER_FUNCTION: Transfers prevention costs (stranded asset write-downs, energy transition investment, industrial restructuring) from the current generation to future generations and the Global South; transfers adaptation costs to future generations and climate-vulnerable populations; transfers adaptation-finance flows to wealthy nations and financial intermediaries; transfers fossil-fuel rents from decarbonization mandates to incumbent energy capital.
% ABSENT_VOICES: Future generations (not yet actors in current economic/political systems), subsistence populations whose livelihoods would be entirely displaced by accepted warming, and the non-human ecological systems that cannot negotiate. The Global South participates nominally but with vastly asymmetric power; their objections are heard but overruled by institutional voting weights favoring wealthy nations.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority frame vanished and were replaced by mitigation-priority or degrowth readings, climate investment would shift: renewable energy deployment would accelerate, fossil infrastructure would face accelerated write-downs, energy transition timelines would compress, and intergenerational wealth transfers would reverse (current generation absorbs transition costs rather than deferring climate liability). The current beneficiary set would lose the extended operational window and the ability to diffuse costs temporally.
% FOUNDING_PROBLEM: Rapid decarbonization is economically costly in the short term (stranded assets, workforce displacement, energy infrastructure investment). Persuading wealthy nations and incumbent energy capital to bear these costs voluntarily is politically difficult. The adaptation-priority frame solves this by reframing the problem: instead of 'How do we transition quickly?' it becomes 'How do we live with higher warming?' This reframing makes deferral the rational choice and converts prevention from a moral obligation to an optional cost.
% FOUNDING_PROBLEM_CORROBORATION: Wealthy-nation policymakers and fossil-energy actors attest that rapid decarbonization is economically ruinous and adaptation is pragmatic. Climate scientists, the Global South, and future-generations advocates attest that adaptation costs will exceed prevention costs and that accepting 2-3°C warming violates intergenerational justice. The empirical record (insurance industry data on rising adaptation costs, ecological tipping-point research, economic analyses by institutions outside the fossil-capital sphere) supports the contested reading: the founding problem of 'decarbonization cost' is real but is being weaponized to obscure a deeper problem of intergenerational cost-shifting.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.82 reflects the magnitude of the intergenerational transfer: the current generation avoids ~$2-4 trillion in decarbonization costs (conservative estimate) by accepting $1-2 trillion in annual adaptation costs that future generations will bear at rising cumulative damage. The transfer is asymmetric: prevention costs are front-loaded and concentrated; adaptation costs are back-loaded and diffuse, making the true cost less visible to current decision-makers. Suppression is high (0.71) because the adaptation frame must actively suppress mitigation-priority readings and exclude future-generation interests from the negotiating table. The mechanism: institutional voting weights in climate governance favor wealthy nations; scientific advisory committees are staffed from institutions dependent on adaptation funding; media coverage frames decarbonization as economically ruinous (a narrative manufactured by fossil interests). Theater ratio is moderate-high (0.48) because adaptation investment includes genuine resilience activities (flood management, water systems) but growing fractions are theatrical performance of climate action without addressing root drivers. Measurements show extraction rising from 0.68 to 0.82 over the interval because adaptation costs accumulate while prevention is deferred, revealing the structural inequality; theater ratio rises from 0.32 to 0.48 because adaptation projects increasingly emphasize visibility and leadership branding over impact. The coercion grid shows asymmetric pressure: at the structural level (wealthy nations and fossil capital), suppression stabilizes and alternatives remain accessible; at the class and individual level (Global South, vulnerable populations), suppression rises and accessibility collapses, trapping those populations into accepting adaptation liability.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (wealthy-nation policymakers, fossil capital), the adaptation-priority reading computes as rope or even mountain: genuine coordination (adaptation is necessary), unavoidable (warming is inevitable), and pragmatic (prevention is impossible). From the victim seat (future generations, Global South, vulnerable populations), it computes as snare: the inevitability is enforced by suppressing mitigation alternatives; the pragmatism is rationalization for cost-shifting; the coordination is illusory (the victims did not consent to accept warming). The engine's per-seat computation will diverge: beneficiary seats will see coordinated response; victim seats will see extractive constraint. This divergence is the measurement that detects reading-dependent framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations and fossil capital are near full beneficiary end (d ≈ 0.1–0.2): they collect the benefit of deferred transition costs. Future generations are at full target end (d ≈ 0.95): they bear the extraction through inherited climate liability with no input into the decision. The Global South is at high-target end (d ≈ 0.85): they face disproportionate warming and constrained adaptation funding. Climate-vulnerable populations face identity-lock (d ≈ 0.90): their exit from the constraint is cultural dissolution. Adaptation-finance brokers are moderate beneficiaries (d ≈ 0.25): they collect rents but remain dependent on the reading's persistence. The directionality structure is not symmetric: the beneficiary set is small, concentrated, and institutionally powerful; the victim set is large, dispersed, and institutionally marginal.
 *
 * MANDATROPHY ANALYSIS:
 *   The adaptation-priority reading exhibits mandatrophy signals: the founding problem (decarbonization is economically ruinous) is increasingly contested by economic analyses showing decarbonization costs < climate-damage costs; the coordination function (allocating adaptation investment) is being sustained despite the victim set growing (as warming accumulates, more communities require adaptation); the suppression requirement is rising (alternative readings must be excluded more actively) despite the mandate's purported pragmatism. The constraint persists not because the founding problem remains live but because institutional actors (wealthy governments, fossil capital, adaptation intermediaries) are invested in its persistence. The theater ratio rising toward 0.48 indicates adaptation projects are increasingly performance rather than impact. A piton-stage reading might emerge within 10–15 years if: (a) adaptation costs exceed adaptation investment (making adaptation failure obvious), (b) prevention costs fall below adaptation costs (making the original economics claim false), and (c) the institutional beneficiary set remains captured by sunk-cost investment in adaptation infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prevention_cost_empirical_uncertainty,
    'What is the true economic cost of rapid decarbonization relative to the cost of climate damages under 2-3°C warming?',
    'Independent meta-analysis of decarbonization economics vs. climate-damage economics, controlling for funding source bias and accounting for non-market damages (ecological collapse, human suffering). Key data: energy-transition cost curves (solar/wind/battery), stranded-asset write-down estimates, avoided-damage monetization (health, agriculture, infrastructure).',
    'If prevention costs < climate-damage costs (a consensus finding emerging from recent literature), the founding-problem claim that prevention is economically ruinous is false, and the adaptation-priority reading is exposed as rationalization for cost-shifting rather than pragmatism. The constraint would be reclassified as pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prevention_cost_empirical_uncertainty, empirical, 'The empirical foundation of the adaptation-priority reading''s legitimacy claim.').

omega_variable(
    adaptation_sufficiency_assumption,
    'Can adaptive capacity and technological adaptation actually offset the cascade effects of 2-3°C warming, or are there irreversible tipping points that adaptation cannot protect against?',
    'Empirical monitoring of tipping-point thresholds (Amazon dieback, Atlantic Meridional Overturning Circulation collapse, permafrost-carbon release, ice-sheet destabilization). If tipping points are crossed within the adaptation window, adaptation sufficiency is falsified.',
    'If adaptation cannot offset cascading ecological collapse, the constraint''s core technical premise (adaptation is sufficient) fails, and the choice to accept warming becomes indefensible even on pragmatic grounds. The constraint would degrade to performance (theater_ratio → 1.0) because its coordinating function (enable high-emission policy through adaptation) persists despite functional failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_sufficiency_assumption, empirical, 'Whether adaptation can actually provide the resilience the reading claims.').

omega_variable(
    intergenerational_authorization_ambiguity,
    'Do current-generation actors have moral and institutional authority to accept on behalf of future generations climate impacts that future generations cannot consent to?',
    'Moral-philosophy literature on intergenerational justice, legal precedents on fiduciary duty, institutional practice in other domains (resource management, waste disposal). Empirical record of institutional responses when future-generation interests conflict with current-generation benefits (e.g., radioactive waste storage, ocean acidification treaties).',
    'If intergenerational fiduciary duty is recognized as binding, the adaptation-priority reading violates structural legitimacy and the constraint is exposed as resting on moral authority that is not actually held. Victim-seat perception shifts from ''externally imposed constraint'' to ''violation of fiduciary trust''; suppression mechanism shifts from institutional gatekeeping to manifest injustice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_authorization_ambiguity, conceptual, 'Normative question about authority to impose climate liability on future generations.').

omega_variable(
    reading_dependence_of_beneficiary_structure,
    'Are the beneficiary and victim sets determined by the reading itself (adaptation-priority reading constructs its beneficiary set by accepting warming; mitigation-priority reading constructs a different set by preventing warming), or are they objective facts independent of reading?',
    'Comparative analysis of all three climate-response readings. If the same actors are beneficiaries under adaptation-priority but victims under mitigation-priority (e.g., fossil capital benefits from adaptation framing, would be harmed by mitigation framing), then beneficiary/victim structure is reading-dependent — a sign that the readings are incommensurable frameworks, not competing empirical claims.',
    'If beneficiary structure is reading-dependent, no neutral external adjudication between readings is possible; the choice between readings is fundamentally a choice about who benefits and who pays. The constraint''s classification would depend on adopting a particular moral frame (who has standing, which futures matter). This is a kernel-level structural fact about the climate-response obligation domain, not a defect in the adaptation-priority reading alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_dependence_of_beneficiary_structure, conceptual, 'Whether the beneficiary/victim structure is an objective fact or constructed by the reading choice.').

omega_variable(
    fossil_capital_lock_in_incentive,
    'Is adaptation-priority framing being promoted because it is pragmatically superior, or because fossil-fuel capital has structural incentive to suppress mitigation-priority readings that would trigger asset write-downs?',
    'Funding-source analysis: map the flow of capital into adaptation-research institutions, climate-policy think tanks, and narrative-production mechanisms (media, academic hiring, grant allocation). Identify whether fossil-capital funding correlates with institutional endorsement of adaptation-priority frames. Comparative analysis: what policy frames are promoted by institutions with no fossil-capital funding?',
    'If fossil-capital funding is a primary driver of adaptation-priority narrative, the reading is exposed as capture-driven rather than evidence-driven. Suppression mechanism shifts from institutional gatekeeping to coordinated narrative manufacturing. The constraint''s legitimacy claim (adaptation is pragmatic) is revealed as manufactured consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_capital_lock_in_incentive, empirical, 'Whether adaptation-priority framing is driven by evidence or by capital incentive to suppress alternative readings.').

omega_variable(
    kernel_contest_foreclosure_structure,
    'Do any pair of the three climate-response readings logically foreclose each other, or are all three capable of coexisting within different institutional frames simultaneously?',
    'Logical analysis of the core premises: adaptation-priority (warming inevitable, adaptation sufficient), mitigation-priority (prevention feasible, morally obligatory), degrowth (production system is the problem). Test whether accepting any one premise logically requires rejecting the others, or whether they represent different answers to different sub-questions within climate response.',
    'If readings coexist (most likely outcome), the kernel is a true contest with no internal logical resolution — the choice between readings is institutional/political, not empirical. If one reading forecloses others (unlikely but possible if prevention is either demonstrably infeasible or demonstrably already underway beyond adaptation''s operation window), the contest structure collapses and one reading becomes canonical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_structure, conceptual, 'The logical structure of the contest between the three climate-response readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(croa_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.32).
narrative_ontology:measurement(croa_tr_t5, climate_response_obligation__adaptation_priority, theater_ratio, 5, 0.37).
narrative_ontology:measurement(croa_tr_t10, climate_response_obligation__adaptation_priority, theater_ratio, 10, 0.41).
narrative_ontology:measurement(croa_tr_t15, climate_response_obligation__adaptation_priority, theater_ratio, 15, 0.44).
narrative_ontology:measurement(croa_tr_t20, climate_response_obligation__adaptation_priority, theater_ratio, 20, 0.46).
narrative_ontology:measurement(croa_tr_t25, climate_response_obligation__adaptation_priority, theater_ratio, 25, 0.47).
narrative_ontology:measurement(croa_tr_t30, climate_response_obligation__adaptation_priority, theater_ratio, 30, 0.48).
narrative_ontology:measurement(croa_tr_t35, climate_response_obligation__adaptation_priority, theater_ratio, 35, 0.48).

% Extraction over time
narrative_ontology:measurement(croa_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(croa_be_t5, climate_response_obligation__adaptation_priority, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(croa_be_t10, climate_response_obligation__adaptation_priority, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(croa_be_t15, climate_response_obligation__adaptation_priority, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(croa_be_t20, climate_response_obligation__adaptation_priority, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(croa_be_t25, climate_response_obligation__adaptation_priority, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(croa_be_t30, climate_response_obligation__adaptation_priority, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(croa_be_t35, climate_response_obligation__adaptation_priority, base_extractiveness, 35, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(croa_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(croa_su_t5, climate_response_obligation__adaptation_priority, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(croa_su_t10, climate_response_obligation__adaptation_priority, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(croa_su_t15, climate_response_obligation__adaptation_priority, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(croa_su_t20, climate_response_obligation__adaptation_priority, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(croa_su_t25, climate_response_obligation__adaptation_priority, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(croa_su_t30, climate_response_obligation__adaptation_priority, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(croa_su_t35, climate_response_obligation__adaptation_priority, suppression_requirement, 35, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(croa_grid_01, climate_response_obligation__adaptation_priority, accessibility_collapse(class), 0, 0.71).
narrative_ontology:measurement(croa_grid_02, climate_response_obligation__adaptation_priority, accessibility_collapse(class), 35, 0.75).
narrative_ontology:measurement(croa_grid_03, climate_response_obligation__adaptation_priority, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(croa_grid_04, climate_response_obligation__adaptation_priority, accessibility_collapse(individual), 35, 0.72).
narrative_ontology:measurement(croa_grid_05, climate_response_obligation__adaptation_priority, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(croa_grid_06, climate_response_obligation__adaptation_priority, accessibility_collapse(organizational), 35, 0.48).
narrative_ontology:measurement(croa_grid_07, climate_response_obligation__adaptation_priority, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(croa_grid_08, climate_response_obligation__adaptation_priority, accessibility_collapse(structural), 35, 0.62).
narrative_ontology:measurement(croa_grid_09, climate_response_obligation__adaptation_priority, resistance(class), 0, 0.58).
narrative_ontology:measurement(croa_grid_10, climate_response_obligation__adaptation_priority, resistance(class), 35, 0.62).
narrative_ontology:measurement(croa_grid_11, climate_response_obligation__adaptation_priority, resistance(individual), 0, 0.52).
narrative_ontology:measurement(croa_grid_12, climate_response_obligation__adaptation_priority, resistance(individual), 35, 0.48).
narrative_ontology:measurement(croa_grid_13, climate_response_obligation__adaptation_priority, resistance(organizational), 0, 0.71).
narrative_ontology:measurement(croa_grid_14, climate_response_obligation__adaptation_priority, resistance(organizational), 35, 0.67).
narrative_ontology:measurement(croa_grid_15, climate_response_obligation__adaptation_priority, resistance(structural), 0, 0.62).
narrative_ontology:measurement(croa_grid_16, climate_response_obligation__adaptation_priority, resistance(structural), 35, 0.58).
narrative_ontology:measurement(croa_grid_17, climate_response_obligation__adaptation_priority, stakes_inflation(class), 0, 0.78).
narrative_ontology:measurement(croa_grid_18, climate_response_obligation__adaptation_priority, stakes_inflation(class), 35, 0.82).
narrative_ontology:measurement(croa_grid_19, climate_response_obligation__adaptation_priority, stakes_inflation(individual), 0, 0.72).
narrative_ontology:measurement(croa_grid_20, climate_response_obligation__adaptation_priority, stakes_inflation(individual), 35, 0.78).
narrative_ontology:measurement(croa_grid_21, climate_response_obligation__adaptation_priority, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(croa_grid_22, climate_response_obligation__adaptation_priority, stakes_inflation(organizational), 35, 0.42).
narrative_ontology:measurement(croa_grid_23, climate_response_obligation__adaptation_priority, stakes_inflation(structural), 0, 0.62).
narrative_ontology:measurement(croa_grid_24, climate_response_obligation__adaptation_priority, stakes_inflation(structural), 35, 0.68).
narrative_ontology:measurement(croa_grid_25, climate_response_obligation__adaptation_priority, suppression(class), 0, 0.71).
narrative_ontology:measurement(croa_grid_26, climate_response_obligation__adaptation_priority, suppression(class), 35, 0.74).
narrative_ontology:measurement(croa_grid_27, climate_response_obligation__adaptation_priority, suppression(individual), 0, 0.61).
narrative_ontology:measurement(croa_grid_28, climate_response_obligation__adaptation_priority, suppression(individual), 35, 0.65).
narrative_ontology:measurement(croa_grid_29, climate_response_obligation__adaptation_priority, suppression(organizational), 0, 0.55).
narrative_ontology:measurement(croa_grid_30, climate_response_obligation__adaptation_priority, suppression(organizational), 35, 0.51).
narrative_ontology:measurement(croa_grid_31, climate_response_obligation__adaptation_priority, suppression(structural), 0, 0.68).
narrative_ontology:measurement(croa_grid_32, climate_response_obligation__adaptation_priority, suppression(structural), 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_stranded_assets).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, intergenerational_wealth_transfer_mechanisms).

% DUAL FORMULATION NOTE:
% This story is one reading of the climate_response_obligation kernel. The sibling readings (mitigation_priority, degrowth_reading) are separate constraint stories with different ε values, beneficiary/victim structures, and types. The three readings are linked by network.affects_constraints and should be analyzed comparatively. The adaptation-priority reading dominates current institutional policy (UNFCCC, multilateral development banks, wealthy-nation climate ministries) but is increasingly contested by the other two readings. Decomposition follows ε-invariance (OQ-18): each reading has a stable, reading-indexed ε computed from its own framing of what counts as extraction; beneficiary/victim structure differs by reading; the constraint families enable detection of reading-driven classification divergence as a measurement of institutional capture and contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
