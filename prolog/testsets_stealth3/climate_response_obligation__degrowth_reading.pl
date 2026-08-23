% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Throughput-Contraction Obligation (Degrowth Reading of the Climate Response Kernel)
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   This file instantiates the degrowth reading of the
 *   climate_response_obligation kernel: the obligation to respond to climate
 *   disruption is read as a requirement to contract aggregate material and
 *   energy throughput to within planetary boundaries, with sufficiency
 *   prioritized over efficiency. The standing arrangement under contest — and
 *   therefore epsilon's referent — is this throughput-contraction obligation
 *   itself, as it gains institutional form (EU circular-economy and
 *   sufficiency instruments, national material-footprint accounting, IPCC
 *   demand-side mitigation chapters), assessed by the reading's own lights:
 *   the reading holds contraction necessary and just, and simultaneously
 *   acknowledges that its costs fall asymmetrically on identifiable
 *   present-day seats. The claim/metric split is deliberate: claimed_type
 *   tangled_rope is asserted from structure (a genuine collective scale
 *   problem, asymmetrically borne, requiring enforcement), while the metrics
 *   describe observed operation — rising burden, rising required coercion, a
 *   thickening rhetorical layer. The sibling readings are separate
 *   constraints in separate files; nothing about them is averaged into this
 *   one. KEY AGENTS (by structural relationship): -
 *   global_north_high_consumption_households: primary target
 *   (powerful/identity_locked) — bears lifestyle-reduction costs -
 *   fossil_carbon_capital: primary target (institutional/arbitrage) — bears
 *   stranded-asset and market-shrinkage costs - growth_dependent_labor:
 *   target with a compensating stake (organized/constrained) -
 *   future_generations: primary beneficiary (powerless/trapped/excluded) —
 *   receives preserved headroom - global_south_development_populations:
 *   conditional beneficiary and contingent payer (organized/constrained) -
 *   low_throughput_communities: incidental beneficiary (powerless/trapped) -
 *   sufficiency_policy_institutions: agenda setter (institutional/mobile) -
 *   ecological_economists: analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.65).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.58).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Throughput-Contraction Obligation (Degrowth Reading of the Climate Response Kernel)").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '59b042e1-3151-40c1-a4e9-586966c0c168').
narrative_ontology:cs_kernel_codification('59b042e1-3151-40c1-a4e9-586966c0c168', distributed).
narrative_ontology:cs_authority_grounding('59b042e1-3151-40c1-a4e9-586966c0c168', expertise).
narrative_ontology:cs_interpretation_layer_present('59b042e1-3151-40c1-a4e9-586966c0c168').
narrative_ontology:cs_reading_relation('59b042e1-3151-40c1-a4e9-586966c0c168', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('59b042e1-3151-40c1-a4e9-586966c0c168', climate_response_obligation__adaptation_priority, forecloses).
narrative_ontology:cs_axiom('59b042e1-3151-40c1-a4e9-586966c0c168', foundational, sufficiency_obligates_throughput_contraction).
narrative_ontology:cs_axiom_status(sufficiency_obligates_throughput_contraction, holdable).
narrative_ontology:cs_axiom_grounding('59b042e1-3151-40c1-a4e9-586966c0c168', sufficiency_obligates_throughput_contraction, empirically_contingent).
narrative_ontology:cs_axiom('59b042e1-3151-40c1-a4e9-586966c0c168', foundational, north_first_contraction_sequencing).
narrative_ontology:cs_axiom_status(north_first_contraction_sequencing, holdable).
narrative_ontology:cs_axiom_grounding('59b042e1-3151-40c1-a4e9-586966c0c168', north_first_contraction_sequencing, deontological).
narrative_ontology:cs_reference_frame('59b042e1-3151-40c1-a4e9-586966c0c168', economy_subordinate_to_biosphere).
narrative_ontology:cs_drift_state('59b042e1-3151-40c1-a4e9-586966c0c168', post_ar6_demand_side_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('59b042e1-3151-40c1-a4e9-586966c0c168', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_development_populations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, low_throughput_communities).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_high_consumption_households).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, fossil_carbon_capital).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, growth_dependent_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, growth_dependent_labor).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_development_populations).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, steady_state_economics).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, contraction_and_convergence_principle).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Biophysical cycles — carbon sinks, freshwater flows, nutrient cycles, biodiversity — that absorb industrial waste and supply raw materials. Cannot act, negotiate, or refuse; responds only through measurable state changes such as warming, acidification, and species loss. Enters human decision-making solely through scientific assessment. Listed for narrative completeness; not an actor.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_systems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_systems).

% People who will live under whatever throughput trajectory today's institutions lock in. Hold no seat in any current decision forum; their interests arrive only through advocacy proxies and ethical argument. They receive preserved carbon and material headroom, lower warming, and functioning ecosystems if present high-throughput actors contract; they inherit depleted systems and committed warming otherwise. Exit is impossible — they cannot leave the outcome they are assigned.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, future_generations, excluded).

% Roughly five-sixths of humanity still building energy access, housing, nutrition, and mobility toward affluent-country baselines. Under North-first contraction they gain development space as wealthy-country consumption frees budget headroom; if wealthy countries do not contract first, restraint lands on the only growth trajectory available to them. Collectively organized through climate diplomacy blocs; individually poor, with little capacity to absorb imposed costs.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_development_populations, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_development_populations, payer).

% Subsistence, pastoral, and Indigenous communities whose material footprints already sit within regional regenerative capacities. They bear land enclosure and sink-appropriation pressures when high-throughput economies expand, and gain protection when those economies contract. They hold little formal power in climate governance and frequently cannot leave affected territories without losing livelihood and community.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, low_throughput_communities, beneficiary,
    powerless, generational, trapped, regional).

% Affluent households — roughly the top income quintile globally, concentrated in North America, Europe, and East Asia — responsible for a disproportionate share of material and carbon footprints through home size, flights, meat, vehicles, and goods turnover. Sufficiency policy asks downsizing of exactly these consumption lines. Social position, daily habit, and self-conception are bound up with current consumption levels, so the demanded change registers as identity loss rather than adjustment. They vote, donate, and set cultural defaults in the very polities that would enforce the obligation, giving them strong defensive leverage; the only exit is accepting lower-status consumption, which the identity frame renders close to unthinkable.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_high_consumption_households, payer,
    powerful, biographical, identity_locked, global).

% Firms and asset holders whose valuations assume decades of expanding fuel throughput: producers, pipeline operators, combustion-adjacent manufacturers, and the funds holding them. Throughput contraction strands reserves and shrinks addressable markets. Capital is mobile across sectors and jurisdictions — divestment, diversification, and relocation are real exits — though core holdings impair on the way out. Decision horizons run to quarterly returns and fund cycles, shorter than the obligation's timeline.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, fossil_carbon_capital, payer,
    institutional, immediate, arbitrage, global).

% Workers in construction, transport, logistics, consumer manufacturing, and retail whose employment tracks expanding production volumes. Contraction threatens their jobs unless paired with work-time reduction, job guarantees, or transition income — provisions the sufficiency policy tradition proposes but has not delivered at scale. Unionized in part, able to slow policy through strike threats and electoral weight, unable to leave wage dependence. They stand to gain from the shorter-working-life and public-provision elements of the same program, which is why their position is genuinely double-edged.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, growth_dependent_labor, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, growth_dependent_labor, beneficiary).

% Environment ministries, EU directorates, national climate councils, and advisory bodies translating boundary science into caps, material-footprint targets, repairability standards, advertising restrictions, and sufficiency directives. They administer whatever enforcement exists and calibrate its stringency. They face constant reframing pressure toward less disruptive doctrines and can pivot institutional effort between competing response framings without personal cost — their exit from this obligation is a reassignment of priorities, not a hardship.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, sufficiency_policy_institutions, agenda_setter,
    institutional, generational, mobile, continental).

% Researchers in the steady-state and degrowth tradition who measure overshoot, design contraction instruments, and audit whether delivered policy matches declared sufficiency goals. They neither collect nor bear the arrangement's flows; they produce the accounting that the other seats argue over.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, ecological_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:fixing_cost_class(climate_response_obligation__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the aggregate scale of human material and energy throughput relative to biophysical regeneration and waste-absorption capacities — a common-pool scale problem that prices, efficiency gains, and voluntary restraint have not solved, because each actor's rational expansion degrades the shared envelope.
% TRANSFER_FUNCTION: Moves consumable headroom — carbon budget, material budget, land and sink capacity — from present high-throughput actors (affluent Northern households and throughput-dependent capital) toward future generations and Global South development space; moves social status from accumulation toward sufficiency.
% ABSENT_VOICES: Future generations are absent from every forum and enter only through proxies; nonhuman systems are represented solely by scientific assessment; Southern negotiators are present in diplomacy but outmatched on finance and sequencing decisions; low-throughput communities are rarely seated at all. Unanimity in favor of growth-compatible framings arises partly because these seats were never in the room.
% DISAPPEARANCE_RATIONALE: Energy systems, urban form, agricultural supply chains, and fiscal models currently assume throughput expansion; remove the obligation and investment reverts to growth pathways, boundary transgression accelerates, and the rearrangement lands hardest on the seats this constraint shields — future cohorts, Southern development space, and remaining low-throughput territories.
% FOUNDING_PROBLEM: Industrial metabolism outgrew the biosphere: material and energy throughput now exceeds regenerative and absorptive capacities, eroding the Holocene stability within which agriculture and settlement developed. The arrangement was built to organize a deliberate descent to sustainable scale before forced collapse imposes one.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the degrowth beneficiary set: Earth-system scientists quantify multiple planetary boundaries as transgressed (Richardson et al. 2023 and successors); IPBES and IPCC assessments document overshoot; NGFS central-bank scenario work and insurance-sector actuarial loss data independently attest escalating physical risk. No corroborating source outside the beneficiary set attests that the founding problem is resolved.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.65 at interval end) is substantial because the obligation concentrates costs on identifiable seats — affluent Northern consumption and throughput-dependent capital — while its benefits accrue diffusely and temporally distant; the reading's embedded fairness corrections (North-first sequencing, protection of low-income consumption) cap it below predatory ranges. Suppression (0.58) reflects the enforcement machinery the obligation requires — caps, rationing instruments, advertising restrictions, mobility quotas — because voluntary sufficiency fails against growth dynamics, and because the reading itself actively delegitimizes the efficiency-only alternative ('sufficiency over efficiency' is a rejection of the escape route, not a neutral preference). Suppression is authored as a raw structural property; the engine scales only extractiveness by directionality and scope. Accessibility collapse (0.45) is partial: once overshoot is understood, indefinite-material-growth collapses as a physical possibility, but decoupling and green-growth alternatives remain live and contested, keeping the alternative space partly open. Resistance (0.72) is high: industry lobbying, consumer backlash (fuel-price protests as recurring precedent), labor fear, and Southern insistence on development rights all push against contraction. Theater (0.38) captures the thick layer of sufficiency rhetoric, awareness campaigns, and pledge instruments relative to delivered throughput reduction. All three temporal series run on one shared grid (t=0,2,4,6,8,10 over 2015-2025) so no metric is sampled against another's end-state; the trajectories are monotonic rather than cyclical — enforcement build-out and burden intensification track the observable sequence from Paris-era pledge politics through Green Deal instrumentation to the post-AR6 demand-side turn. Gain_flow names future_generations because the constraint's gains demonstrably accrue there (preserved headroom is received by whoever lives later); fixing_cost is prohibitive because repairing the asymmetry — delivering North-first sequencing, compensating labor, building legitimate enforcement — requires overcoming the growth dependence of the very polities that would legislate it, a cost that has exceeded what any fixing coalition has so far been able to bear.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from identical structural facts. From the Northern household seat, the obligation arrives as a demand to surrender the consumption that constitutes social position — identity-fused resistance, the exit closest to unthinkable in this story; their computed extraction is amplified by identity lock. From the fossil-capital seat, the same obligation is a portfolio problem with real arbitrage exits, damping experienced extraction despite the seat being a declared target. Growth-dependent labor experiences a double edge: job threat now, promised work-time dividend later. From the future-generations seat the arrangement is nearly pure subsidy — maximal benefit, zero power, zero exit — while Southern seats sit conditionally: beneficiaries under honored sequencing, payers under broken sequencing. The two institutional seats share a power atom but opposite directionalities (capital targeted, policy institutions administering), which the role declarations differentiate. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: future_generations (full benefit, trapped, powerless) anchors the beneficiary end; global_south_development_populations derives low-but-not-floor directionality because its secondary payer position under failed sequencing pulls it up from pure subsidy; low_throughput_communities derive near-beneficiary values with minimal enforcement relevance. Victim declarations drive high directionality: global_north_high_consumption_households approach the full-target end (victim + identity_locked exit removes the damping mobile targets enjoy); fossil_carbon_capital is a declared target whose arbitrage exit moderates but does not invert its position — divestment is an exit from impaired holdings, not from bearing the constraint's costs; growth_dependent_labor sits high with partial compensation from its secondary beneficiary role. The agenda_setter seat derives intermediate directionality: it administers the obligation rather than collecting its gains, and its mobile exit lets it reframe away at low cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and externally corroborated, so no mandatrophy is declared and the mismatch consumer finds status=live crossed with verdict=world_rearranges — the consistent cell, no zombie flag. The classification work this story performs is preventing two symmetrical mislabelings: opponents frame the obligation as pure sacrifice imposed on the innocent (snare-flavored), ignoring the genuine collective scale problem it coordinates; advocates frame it as pure harmony (rope-flavored), ignoring that its costs concentrate on named seats while its benefits diffuse. The tangled_rope claim holds both halves: real coordination function, real asymmetric burden, real enforcement requirement. The temporal series additionally guard against future degradation: if enforcement never materializes and only theatrical sufficiency persists, the theater_ratio series is positioned to catch the drift toward piton; if sequencing breaks, the sequencing omega routes the Southern-seat inversion through review rather than letting the beneficiary declaration silently rot.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the climate_response_obligation kernel (reading: degrowth_reading). Which structural element do the sibling readings actually contest, and what would adopting a sibling change?',
    'Comparative classification across the sibling files (mitigation_priority, adaptation_priority): the disagreement is located in the temporal distribution of burden (present consumption vs future welfare) and in the binding variable (aggregate throughput scale vs emissions intensity vs managed harm). Reading the sibling stories'' beneficiary/victim sets against this one localizes the dispute.',
    'Under mitigation_priority, future generations remain beneficiaries but the victim set shifts to present carbon-intensive activity broadly, with efficiency permitted as the main lever; under adaptation_priority, the victim set shifts to exposed populations and the beneficiary set to resilience-building industries. This file''s epsilon and type hold only for the degrowth instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega recording that this story instantiates one reading of a contested kernel and naming where the readings diverge.').

omega_variable(
    absolute_decoupling_feasibility,
    'Can GDP be absolutely decoupled from material and energy throughput at the rates and scales required to respect planetary boundaries, or is the degrowth reading''s necessity premise empirically forced?',
    'Long-run material-footprint and energy-accounting datasets against GDP, plus sectoral studies of efficiency rebound; the decisive test is whether absolute decoupling persists at required contraction rates in already-efficient economies.',
    'If robust absolute decoupling is demonstrated, the degrowth reading''s foundational premise weakens and this constraint relaxes toward an efficiency-led coordination form converging with the mitigation_priority sibling; if refuted, the constraint hardens and its enforcement requirements grow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_feasibility, empirical, 'Whether sufficiency-over-efficiency is empirically forced or one option among several.').

omega_variable(
    north_first_sequencing_durability,
    'Will the North-first contraction sequencing that protects Global South development space hold politically, or will the obligation degrade into uniform austerity that restrains Southern throughput without prior Northern contraction?',
    'Track delivered Northern material-footprint reductions against Southern per-capita convergence trajectories; finance-transfer delivery ratios; sequencing clauses in enacted instruments versus proposal drafts.',
    'If sequencing collapses, Southern seats flip from beneficiary toward payer, the constraint''s asymmetry inverts into imposition on the least-consuming populations, and the classification for those seats moves sharply toward the extractive end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_first_sequencing_durability, preference, 'Durability of the distributive-justice sequencing on which this reading''s fairness claim rests.').

omega_variable(
    northern_identity_lock_composition,
    'Is Northern household resistance to throughput contraction primarily structural (employment, pension, and fiscal systems dependent on expanding consumption) or internalized (self-concept and social position fused with consumption levels)?',
    'Post-intervention consumption trajectories: where price signals and infrastructure change without identity intervention, does consumption revert? Compare jurisdictions that decoupled mobility and housing norms from status display.',
    'If internalized share is large, effective suppression persists after formal barriers fall — households carry the growth orientation with them — and enforcement requirements stay elevated even under favorable policy; if structural, removing economic dependence releases the constraint''s grip.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(northern_identity_lock_composition, empirical, 'Structural versus internalized composition of the dominant payer seat''s resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t2, climate_response_obligation__degrowth_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement_basis(clim_tr_t2, observed).
narrative_ontology:measurement(clim_tr_t4, climate_response_obligation__degrowth_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(clim_tr_t4, observed).
narrative_ontology:measurement(clim_tr_t6, climate_response_obligation__degrowth_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(clim_tr_t6, observed).
narrative_ontology:measurement(clim_tr_t8, climate_response_obligation__degrowth_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(clim_tr_t8, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__degrowth_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(clim_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t2, climate_response_obligation__degrowth_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement_basis(clim_be_t2, observed).
narrative_ontology:measurement(clim_be_t4, climate_response_obligation__degrowth_reading, base_extractiveness, 4, 0.56).
narrative_ontology:measurement_basis(clim_be_t4, observed).
narrative_ontology:measurement(clim_be_t6, climate_response_obligation__degrowth_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement_basis(clim_be_t6, observed).
narrative_ontology:measurement(clim_be_t8, climate_response_obligation__degrowth_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement_basis(clim_be_t8, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__degrowth_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(clim_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t2, climate_response_obligation__degrowth_reading, suppression_requirement, 2, 0.46).
narrative_ontology:measurement_basis(clim_su_t2, observed).
narrative_ontology:measurement(clim_su_t4, climate_response_obligation__degrowth_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement_basis(clim_su_t4, observed).
narrative_ontology:measurement(clim_su_t6, climate_response_obligation__degrowth_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement_basis(clim_su_t6, observed).
narrative_ontology:measurement(clim_su_t8, climate_response_obligation__degrowth_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement_basis(clim_su_t8, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__degrowth_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(clim_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the climate response obligation' conflates three structurally distinct constraints — mitigation_priority (prevent harm via rapid decarbonization; efficiency-compatible), adaptation_priority (accept 2-3C; invest in resilience), and this degrowth_reading (contract aggregate throughput; sufficiency over efficiency). Each has its own epsilon, beneficiary/victim sets, and classification; they are linked here per the epsilon-invariance principle. Mitigation_priority is the upstream member (highest empirical confidence, most cited); this reading exerts downstream legitimacy pressure on it (its green-growth assumptions are what the degrowth critique targets) and stands in direct premise-level contradiction with adaptation_priority. This file instantiates ONLY the degrowth reading; nothing from the siblings is averaged into its metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
