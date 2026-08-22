% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Mitigation-Priority Climate Response Architecture
 *   domain: environmental/political/economic
 *
 * SUMMARY:
 *   The mitigation-priority reading structures climate response as emissions
 *   reduction pursued through technological innovation and market mechanisms
 *   — carbon pricing, subsidy-led deployment, offset and removal markets,
 *   disclosure regimes — with adaptation assigned a residual status: funded
 *   late, partially, and often as loans. The ε referent is the standing
 *   arrangement itself: the actual mitigation-first architecture of finance
 *   and rulemaking as it operates, assessed by this reading's own lights —
 *   not the adaptation-first or degrowth counterfactuals, which are separate
 *   constraints. Under that referent the structure coordinates genuinely
 *   (decarbonization is a real collective-action problem and deployment is
 *   real) while the benefit-cost incidence is asymmetric: the costs of
 *   deferred adaptation and speculative removal fall on parties with no seat
 *   — future generations and climate-vulnerable regions — while subsidy and
 *   market rents accrue to Global North innovation and finance seats. Claimed
 *   type and metrics are authored independently: the type states the
 *   structural belief (hybrid coordination and extraction, actively
 *   enforced); the metrics describe observed operation.
 *
 * KEY AGENTS:
 *   - national_governments_of_major_emitters: Agenda setter (institutional/constrained) — administers targets, subsidies, carbon markets, and finance rules; bears domestic compliance costs but gains political feasibility from the market framing
 *   - global_north_clean_tech_sectors: Primary beneficiary (powerful/mobile) — receives deployment mandates and subsidy flows; the arrangement's demand creation accrues here
 *   - green_finance_institutions and carbon_market_intermediaries: Secondary beneficiaries (powerful/organized, arbitrage exit) — earn fees on the market machinery the framing creates
 *   - fossil_fuel_incumbents: Dual-positioned beneficiary/payer (powerful/constrained) — pays compliance costs but gains gradual timelines, offset demand, and avoidance of consumption mandates
 *   - future_generations: Full target (powerless/trapped/universal) — inherits residual warming, unbuilt adaptation, and removal liabilities; no seat
 *   - climate_vulnerable_global_south and frontline_coastal_communities: Targets (moderate/powerless, trapped) — bear present damages and residual adaptation costs; coalition capacity (AOSIS, V20) is moral-diplomatic, not material
 *   - climate_justice_movements: Excluded voice (organized/constrained) — present in streets and side events, marginal in negotiation rooms
 *   - ipcc_assessment_community: Analytical observer — produces the scenario layer that both documents and stabilizes the framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.66).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.68).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Mitigation-Priority Climate Response Architecture").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "environmental/political/economic").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '9ff0c1ad-f111-4647-894d-2ff82c3cee3a').
narrative_ontology:cs_kernel_codification('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', distributed).
narrative_ontology:cs_authority_grounding('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', expertise).
narrative_ontology:cs_interpretation_layer_present('9ff0c1ad-f111-4647-894d-2ff82c3cee3a').
narrative_ontology:cs_reading_relation('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_reading_relation('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', foundational, innovation_market_sufficiency).
narrative_ontology:cs_axiom_status(innovation_market_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', innovation_market_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', foundational, adaptation_residual_subordination).
narrative_ontology:cs_axiom_status(adaptation_residual_subordination, holdable).
narrative_ontology:cs_axiom_grounding('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', adaptation_residual_subordination, instrumental).
narrative_ontology:cs_reference_frame('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', cost_benefit_optimized_mitigation_pathway).
narrative_ontology:cs_drift_state('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', contemporary_net_zero_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ff0c1ad-f111-4647-894d-2ff82c3cee3a', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_clean_tech_sectors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, green_finance_institutions).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, climate_vulnerable_global_south).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, frontline_coastal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, fossil_fuel_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set national mitigation targets, legislate subsidy and carbon-pricing regimes, negotiate international finance rules, and administer compliance markets. They carry domestic compliance costs and constituent pressure, but the market-and-innovation framing lets them legislate climate action without confronting consumption patterns or distribution directly, which is the source of its political durability. Reorienting toward adaptation-co-primary would require reopening international finance obligations and domestic coalitions they are invested in.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, national_governments_of_major_emitters, agenda_setter,
    institutional, generational, constrained, continental).

% Manufacture and deploy the technologies the response is built around — renewables, storage, electric vehicles, hydrogen — under demand mandates, tax credits, and procurement guarantees. Subsidy flows and guaranteed markets accrue to this seat; capital and production can relocate across jurisdictions chasing the best incentive packages.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_clean_tech_sectors, beneficiary,
    powerful, biographical, mobile, global).

% Originate climate funds, structure green bonds and ESG products, and hold positions across the market machinery. Fee and asset-gathering revenue scales with the volume of finance the framing channels; instruments and books can be rebalanced across jurisdictions as rules shift.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, green_finance_institutions, beneficiary,
    powerful, biographical, arbitrage, global).

% Develop offset and removal projects, verify credits, and broker compliance and voluntary market volume. Revenue exists only while credit markets operate; skills transfer to adjacent verification, disclosure, and sustainability-consulting services if the market contracts.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Pay carbon compliance costs and face long-run demand erosion, but under this arrangement they gain gradual phase-out timelines, offset demand for their capture projects, and freedom from consumption-reducing mandates that a structural-transformation response would impose. Reserves on their books become stranded under faster alternatives, so they work to keep the market framing in place rather than exit it.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, fossil_fuel_incumbents, payer).

% Hold no seat in any negotiation. They inherit whatever warming is locked in by the pace of present reductions, an adaptation infrastructure that was deferred as residual, and the liabilities of removal promises made on their behalf. There is no exit from the climate they are left.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Already bear intensifying drought, heat, and storm damages while adaptation finance arrives late, partially, and often as loans rather than grants. Offset and removal projects sited on their territory can displace agriculture and land rights. Their leverage runs through moral pressure and voting blocs (AOSIS, V20) rather than material power, and they cannot exit the damages.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_vulnerable_global_south, payer,
    moderate, generational, trapped, regional).

% Face sea-level rise, saltwater intrusion, and storm intensification with protective infrastructure treated as a residual funding category. Managed retreat is largely unfunded, so exit means abandoning homes and livelihoods without compensation, or staying and absorbing the damages.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, frontline_coastal_communities, payer,
    powerless, biographical, trapped, local).

% Organize loss-and-damage claims, adaptation finance demands, and critiques of market mechanisms. They are present in the streets and the side events but structurally marginal to the rooms where agenda, finance rules, and target levels are set; disengaging would cede the framing entirely, so they operate under permanent pressure at the boundary of the conversation.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_justice_movements, excluded,
    organized, generational, constrained, global).

% Assesses climate science and produces the emissions-pathway scenario library that governments and markets plan against. Its scenarios embed this arrangement's assumptions — overshoot handled by future removal, adaptation costs discounted — which both documents the gap between promise and delivery and stabilizes the framing by making it the default planning object. It holds no enforcement power.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, ipcc_assessment_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__mitigation_priority_reading, global_north_clean_tech_sectors).
narrative_ontology:fixing_cost_class(climate_response_imperative__mitigation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the real collective-action problem of reducing emissions across sovereign jurisdictions: establishes a common metric (tonnes CO2e), price signals and markets that find low-cost abatement first, subsidy regimes that scale new technology down its cost curve, and accounting and MRV machinery that makes national efforts comparable and verifiable.
% TRANSFER_FUNCTION: Moves public subsidy, carbon-market revenue, and political attention toward Global North innovation and finance seats; moves continued operating license to fossil incumbents via offsets and gradual timelines; moves the costs of present pace — unbuilt adaptation, residual warming, removal liabilities — onto future generations and climate-vulnerable regions who hold no seat in the allocation.
% ABSENT_VOICES: Future generations have no seat anywhere in the architecture; frontline adaptation-dependent communities are represented only indirectly, through state delegations whose finance priorities are set by mitigation instruments; climate justice and loss-and-damage advocates are physically present at negotiations but structurally marginal to agenda and finance rule-setting; proponents of structural consumption transformation are excluded from mainstream scenario and policy venues as outside the feasible set.
% DISAPPEARANCE_RATIONALE: Carbon markets, subsidy regimes, disclosure mandates, and the negotiation architecture built around mitigation-first would dissolve or re-form around a different primary allocation; adaptation finance would be re-bargained from residual to co-primary status; the innovation sectors' demand mandates would lapse; incumbent operating licenses would be renegotiated without offset cover.
% FOUNDING_PROBLEM: After Rio and Kyoto, climate governance needed an architecture sovereign states would actually join: binding sacrifice mandates failed politically, so the founding problem was how to coordinate emission reductions while keeping domestic political settlements and incumbent economic structures intact — answered with technological optimism and market mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessments and UNEP emissions-gap reporting — seats outside the beneficiary set — corroborate that the emissions problem remains live and that the architecture's coordination function is real. The same sources, plus AOSIS/V20 vulnerable-state statements and adaptation-gap assessments, attest from outside the benefiting parties that adaptation is not being adequately handled as residual: the finance gap widens as mitigation finance grows. No source outside the beneficiary set attests that the residual-adaptation allocation is working as designed.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66: the coordination is real (deployment, pricing, MRV all function) but incidence is asymmetric — adaptation finance runs far below assessed need, offset demand preserves incumbent operations, and removal promises shift present costs onto future balance sheets. Suppression (0.68, a raw structural property, unscaled by scope or power) reflects the enforcement machinery the framing requires: compliance markets, border adjustments, disclosure mandates, and finance conditionality that starve alternative framings rather than outlawing them. Theater_ratio 0.58 crosses the Goodhart drift threshold: pledge architecture, voluntary offset integrity failures, and net-zero accounting now constitute the majority of the arrangement's visible activity relative to verified atmospheric effect. Accessibility_collapse is low (0.35) because the sibling readings remain coherent and live — the arrangement suppresses them materially, not logically; resistance is correspondingly substantial (0.60) from justice movements, vulnerable-state blocs, and degrowth advocates. All three tracked series share one time grid (1992, 1997, 2005, 2015, 2020, 2026) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the architecture reads as the politically feasible pathway — the framing made climate action legislatable where sacrifice mandates were not, and the same governments bear real domestic compliance costs. From the trapped payer seats — coastal communities, vulnerable states, the unborn — the identical structure reads as deferred abandonment: adaptation funded as residual, damages discounted, removal promised on credit. The beneficiary seats see a functioning market they built; the excluded seat sees a room it cannot enter. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (clean-tech, finance, intermediaries, and dual-positioned incumbents) sit near the subsidy end: low directionality, effective extraction damped or inverted for them. The agenda-setter sits mid-structure: it administers the arrangement and absorbs part of its costs domestically. Trapped targets sit near the full-target end: future_generations combine powerless power, a civilizational horizon, trapped exit, and universal scope — maximal directionality with scope amplification; vulnerable regions and coastal communities are trapped by geography and finance dependence. The derivation chain handles these from the declared roles and exit options, so no directionality overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating sovereign emission reductions without politically impossible sacrifice mandates — remains live, so no mandatrophy is declared and the dead-status-plus-rearranges mismatch flag does not fire. The tangled_rope classification does double preventive work: the genuine coordination function blocks a pure-extraction mislabel (emissions reduction is not cover — it happens), while the asymmetric deferral structure and active enforcement block a pure-coordination mislabel (the same structure that coordinates also systematically shifts costs onto seatless parties). Theater is treated as symptom, not test: pledge inflation and offset failure indicate Goodhart drift, but the classification rests on the cost-incidence asymmetry, which is present independently of how performative the pledges become.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cdr_deliverability,
    'Is carbon dioxide removal at the gigatonne scale and cost assumed by mitigation-priority pathways empirically deliverable within the century, or does its inclusion function as structural deferral of both emissions cuts and adaptation?',
    'Track delivered and verified removal volumes and costs against pathway assumptions year by year; compare with independent engineering assessments of DACCS/BECCS scaling constraints, land, energy, and storage limits.',
    'If CDR under-delivers, the residual-adaptation structure converts from risky bet to breached promise — deferred adaptation costs become unrecoverable and the victim set''s burden is retroactively confirmed as extraction rather than reallocation; if deliverable, part of the measured burden is a defensible intertemporal trade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_deliverability, empirical, 'Whether reliance on unproven CDR is deliverable engineering or a deferral device.').

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the mitigation_priority_reading of the climate_response_imperative kernel; what structural deltas would the adaptation_priority_reading or degrowth_reading instantiate in the victim and beneficiary sets?',
    'Author the sibling readings as separate epsilon-invariant constraint stories and compare incidence structure: adaptation-priority moves exposed-region communities into the protected class and innovation sectors out of primary benefit; degrowth adds Global North consumption itself to the target set and dissolves the offset-reliance structure entirely.',
    'The classification of this reading is unchanged (one reading, one constraint, one epsilon), but cross-reading comparison determines whether the kernel''s contest is over resource allocation among coexisting readings or over the identity of the protected class — structurally opposed victim sets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: kernel membership and sibling-reading structural deltas.').

omega_variable(
    reading_disagreement_location,
    'Where exactly do the readings of the climate_response_imperative kernel disagree, and which observable would adjudicate between them?',
    'The disagreement is located at three points: (a) the allocation of primary effort (reduction vs. resilience), (b) the status of adaptation (residual vs. co-primary), and (c) the treatment of Global North consumption (instrument of innovation policy vs. object of transformation). Adjudicating observables: the adaptation finance gap trajectory, realized overshoot magnitude, and absolute decoupling rates in Global North economies.',
    'If the disagreement is primarily at (b), the readings are rival allocations within one framework and coexist; if primarily at (c), the degrowth reading contests this reading''s sufficiency axiom empirically rather than logically, and this reading''s foundational claim stands or falls on decoupling evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_disagreement_location, conceptual, 'Location of inter-reading disagreement within the kernel and the observables that would resolve it.').

omega_variable(
    adaptation_residual_status,
    'Is adaptation residual in this architecture by sequencing choice (to be funded once mitigation is secured) or by structural subordination (finance rules that permanently deprioritize it)?',
    'Compare adaptation finance flows against assessed needs across the interval and against the conditional language of finance commitments; if the gap widens even as mitigation finance grows, the subordination is structural rather than sequential.',
    'If structural, the deferred costs are an operating feature of the arrangement rather than an unfortunate sequencing artifact — supporting the hybrid coordination/extraction reading over a benign coordination reading; if merely sequential, the measured burden on vulnerable seats is lower than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_residual_status, empirical, 'Whether residual adaptation is a sequencing choice or structural subordination.').

omega_variable(
    innovation_sufficiency_rebound,
    'Do innovation and market mechanisms deliver absolute emission reductions at the required pace, or do rebound effects and deployment shortfalls make the sufficiency claim fail in practice?',
    'Compare realized decarbonization rates against pathway requirements by sector; measure consumption rebound in Global North economies alongside efficiency gains, and test whether efficiency-led reductions survive in absolute terms.',
    'If sufficiency fails empirically, this reading''s foundational axiom is contested on its own empirical grounds — strengthening degrowth-reading pressure and confirming the burden shifted onto future generations who absorb the shortfall; if it holds, the coordination function dominates and the authored extractiveness is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_sufficiency_rebound, empirical, 'Whether the innovation-market sufficiency premise holds empirically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 1992, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_response_imperative__mitigation_priority_reading, theater_ratio, 1992, 0.15).
narrative_ontology:measurement_basis(clim_tr_t1992, observed).
narrative_ontology:measurement(clim_tr_t1997, climate_response_imperative__mitigation_priority_reading, theater_ratio, 1997, 0.25).
narrative_ontology:measurement_basis(clim_tr_t1997, observed).
narrative_ontology:measurement(clim_tr_t2005, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement_basis(clim_tr_t2005, observed).
narrative_ontology:measurement(clim_tr_t2015, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement_basis(clim_tr_t2015, observed).
narrative_ontology:measurement(clim_tr_t2020, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2020, 0.55).
narrative_ontology:measurement_basis(clim_tr_t2020, observed).
narrative_ontology:measurement(clim_tr_t2026, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2026, 0.58).
narrative_ontology:measurement_basis(clim_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 1992, 0.3).
narrative_ontology:measurement_basis(clim_be_t1992, observed).
narrative_ontology:measurement(clim_be_t1997, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 1997, 0.42).
narrative_ontology:measurement_basis(clim_be_t1997, observed).
narrative_ontology:measurement(clim_be_t2005, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement_basis(clim_be_t2005, observed).
narrative_ontology:measurement(clim_be_t2015, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement_basis(clim_be_t2015, observed).
narrative_ontology:measurement(clim_be_t2020, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement_basis(clim_be_t2020, observed).
narrative_ontology:measurement(clim_be_t2026, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2026, 0.66).
narrative_ontology:measurement_basis(clim_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement_basis(clim_su_t1992, observed).
narrative_ontology:measurement(clim_su_t1997, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 1997, 0.35).
narrative_ontology:measurement_basis(clim_su_t1997, observed).
narrative_ontology:measurement(clim_su_t2005, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement_basis(clim_su_t2005, observed).
narrative_ontology:measurement(clim_su_t2015, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement_basis(clim_su_t2015, observed).
narrative_ontology:measurement(clim_su_t2020, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement_basis(clim_su_t2020, observed).
narrative_ontology:measurement(clim_su_t2026, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2026, 0.68).
narrative_ontology:measurement_basis(clim_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, degrowth_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'climate response' decomposes into at least three structurally distinct constraints — one per reading of the climate_response_imperative kernel — because the readings assign different victim and beneficiary sets and different primary-effort allocations. This file is the mitigation_priority_reading; the siblings are linked here as family members. Per the epsilon-invariance principle, no single story could hold one stable epsilon across these readings: the referent (which arrangement is under contest) and the incidence structure both differ, so each reading is authored separately and linked rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
