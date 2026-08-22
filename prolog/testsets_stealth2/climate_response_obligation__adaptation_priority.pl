% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response Regime (Accepted 2-3C Warming)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   A policy regime — dominant in wealthy democracies since the 1992 Rio
 *   framework and consolidated after Copenhagen — treats warming of 2-3C as
 *   the operative planning assumption and directs climate capital toward
 *   resilience investment rather than rapid decarbonization, on the ground
 *   that prevention is too costly to justify. The arrangement has a genuine
 *   coordination core: protective infrastructure must be built under every
 *   reading of the climate obligation. It also has a genuine political
 *   function: it releases current voters and asset holders from present
 *   sacrifice. The structural cost falls on parties with no seat — future
 *   generations and present climate-vulnerable populations bear the damages
 *   of the accepted trajectory, and fossil asset holders are protected from
 *   the stranding a prevention-first regime would impose. This story
 *   instantiates the adaptation_priority reading of the
 *   climate_response_obligation kernel (see kernel_context); the claim and
 *   the metrics are authored independently — the claimed type is what I
 *   believe structurally true of the regime, the metrics what I believe
 *   descriptively true of its operation.
 *
 * KEY AGENTS:
 *   - wealthy_nation_governments: agenda-setter (institutional/constrained) — sets the adaptation-versus-mitigation budget split and the discount rates that price future harm
 *   - fossil_capital_owners: primary beneficiary (powerful/arbitrage) — asset values protected from stranding; capital internationally mobile
 *   - wealthy_nation_current_electorates: beneficiary (organized/constrained) — avoids transition costs; damages buffered by wealth; votes in the elections that set the split
 *   - adaptation_infrastructure_industry: secondary beneficiary (organized/mobile) — receives the resilience spending pipeline
 *   - future_generations: primary target (powerless/trapped) — bears unmitigated warming with no seat, no consent, no exit
 *   - global_south_populations: primary target (moderate/constrained) — highest exposure, least adaptation capital; structurally excluded from the fiscal forums that decide the split
 *   - small_island_nations: primary target (moderate/trapped) — existential territorial exposure; organized but leverage-limited
 *   - youth_climate_movements: resisting target (organized/constrained) — litigation and mobilization against the inevitability premise
 *   - climate_science_community: analytical observer (institutional/analytical) — documents trajectories, adaptation limits, and the finance gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.78).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.68).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Adaptation-Priority Climate Response Regime (Accepted 2-3C Warming)").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, '85c08595-8c38-44fa-b35b-4daf24e4864b').
narrative_ontology:cs_kernel_codification('85c08595-8c38-44fa-b35b-4daf24e4864b', fixed_text).
narrative_ontology:cs_authority_grounding('85c08595-8c38-44fa-b35b-4daf24e4864b', distributed).
narrative_ontology:cs_reading_relation('85c08595-8c38-44fa-b35b-4daf24e4864b', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('85c08595-8c38-44fa-b35b-4daf24e4864b', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('85c08595-8c38-44fa-b35b-4daf24e4864b', foundational, warming_trajectory_effectively_locked).
narrative_ontology:cs_axiom_status(warming_trajectory_effectively_locked, holdable).
narrative_ontology:cs_axiom_grounding('85c08595-8c38-44fa-b35b-4daf24e4864b', warming_trajectory_effectively_locked, empirically_contingent).
narrative_ontology:cs_axiom('85c08595-8c38-44fa-b35b-4daf24e4864b', foundational, resilience_satisfies_response_obligation).
narrative_ontology:cs_axiom_status(resilience_satisfies_response_obligation, holdable).
narrative_ontology:cs_axiom_grounding('85c08595-8c38-44fa-b35b-4daf24e4864b', resilience_satisfies_response_obligation, instrumental).
narrative_ontology:cs_axiom('85c08595-8c38-44fa-b35b-4daf24e4864b', secondary, discounted_future_harms_acceptable).
narrative_ontology:cs_axiom_status(discounted_future_harms_acceptable, holdable).
narrative_ontology:cs_axiom_grounding('85c08595-8c38-44fa-b35b-4daf24e4864b', discounted_future_harms_acceptable, conventional).
narrative_ontology:cs_reference_frame('85c08595-8c38-44fa-b35b-4daf24e4864b', resilience_sufficiency_baseline).
narrative_ontology:cs_drift_state('85c08595-8c38-44fa-b35b-4daf24e4864b', post_paris_overshoot_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('85c08595-8c38-44fa-b35b-4daf24e4864b', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, wealthy_nation_current_electorates).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_capital_owners).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, adaptation_infrastructure_industry).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_populations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, small_island_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, youth_climate_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set national budget allocations between resilience programs and decarbonization programs, choose the discount rates that price future climate damages in official cost-benefit analysis, and negotiate the international finance commitments that determine how much adaptation support flows to vulnerable regions. Their planning horizon is anchored to electoral cycles: redirecting spending toward rapid mitigation would impose visible present costs on their voters and confront entrenched asset holders, while the costs of the accepted warming fall mostly on parties outside their electorate.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_nation_governments, agenda_setter,
    institutional, biographical, constrained, global).

% Hold reserves, infrastructure, and equities whose valuations depend on continued fossil throughput. A policy regime that treats deep warming as given and defers decarbonization keeps those assets productive and financeable; a rapid-mitigation regime would strand a large share of them. Capital is internationally mobile, so holdings can shift across jurisdictions regardless of where any single government tightens policy.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_capital_owners, beneficiary,
    powerful, biographical, arbitrage, global).

% Consume energy at high per-capita rates and would bear the visible costs of rapid transition — fuel prices, retrofit bills, industrial restructuring — under a prevention-first regime. Under the accepted-warming regime those costs stay off their budgets while their exposure to climate damage is buffered by wealth: air conditioning, insurance, hardened infrastructure, and the ability to relocate within their own countries. They vote in the elections that set the budget split.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_nation_current_electorates, beneficiary,
    organized, biographical, constrained, national).

% Engineering firms, construction contractors, insurers, and water-management companies that design and build sea defenses, resilient grids, cooling systems, and flood infrastructure. Adaptation spending is their revenue pipeline; a shift of public capital toward mitigation would redirect that pipeline. Their operations concentrate in the wealthy regions where the spending occurs.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, adaptation_infrastructure_industry, beneficiary,
    organized, biographical, mobile, continental).

% Will inhabit the climate that present budget decisions lock in. They bear the damages of unmitigated warming — heat mortality, crop stress, sea-level rise, extreme weather — without having consented to the trade and without any seat, vote, or veto in the allocation process. Exit does not exist for them; their only representation is advocacy conducted by present actors on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Live in regions with the highest climate exposure and the least adaptation capital. They bear intensifying impacts — drought, flood, heat, crop failure — under a regime that directs resilience investment toward wealthy regions and leaves adaptation finance for their regions underfunded. Migration toward safer regions is restricted by the border regimes of the wealthy states. They hold formal voice in international negotiation, but the binding allocation decisions are made in national treasuries and G20 fiscal forums where they have no vote.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_populations, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, global_south_populations, excluded).

% Face existential exposure: sea-level rise under an accepted 2-3C trajectory threatens territorial loss. They organize collectively through coalition blocs and have won formal recognition of loss-and-damage in international frameworks, but they command little leverage over the fiscal decisions of large emitters. Their exit option is their territory itself.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, small_island_nations, payer,
    moderate, generational, trapped, national).

% Young people in wealthy and vulnerable countries alike who will live deepest into the warming the regime accepts. They litigate constitutional and human-rights claims, mobilize electorally, and contest the inevitability premise in public discourse, but their formal power over current budget allocation is minimal and their standing in court is contested.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, youth_climate_movements, payer,
    organized, biographical, constrained, global).

% Produces the trajectory assessments, attribution studies, and adaptation-limit findings that define what is physically locked in and what remains achievable, and documents the gap between adaptation needs and adaptation finance delivered. Holds analytical standing but no allocation power; its findings are cited selectively by every seat.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_science_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, fossil_capital_owners).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates protective investment against a shared warming scenario: sea defenses, resilient infrastructure, heat-health systems, drought-tolerant agriculture, and early-warning systems are planned once, centrally, against a common 2-3C planning assumption rather than per-project guesswork. It also coordinates political expectations, releasing current voters and firms from the demand for present sacrifice.
% TRANSFER_FUNCTION: Moves the costs of climate destabilization from present, wealthy, enfranchised populations onto future generations and present climate-vulnerable populations (Global South, small island states); simultaneously preserves the valuation of fossil assets that a prevention-first regime would strand, with adaptation spending flowing disproportionately to wealthy-region infrastructure firms.
% ABSENT_VOICES: Future generations have no seat anywhere in the allocation process. Global South communities hold formal presence in COP negotiation, but the binding adaptation/mitigation split is set in national treasuries, G20 finance ministries, and domestic budget processes where they have no vote. Small island states carry moral and formal standing but minimal leverage over the fiscal decisions that determine their exposure.
% DISAPPEARANCE_RATIONALE: If the accepted-warming, adaptation-first ordering vanished overnight, budget shares would shift toward mitigation, fossil asset valuations would reprice downward against stranding risk, adaptation industry pipelines would restructure, and the inevitability narrative would lose its institutional carrier — the international climate finance architecture and domestic capital allocation would reorganize around a prevention-first default.
% FOUNDING_PROBLEM: The political-economy problem of imposing concentrated present costs for diffuse, distant benefits: how to fund a climate response without demanding immediate sacrifice from current voters and asset holders. Secondarily, the genuine engineering problem of protecting populations from warming already committed by past emissions.
% FOUNDING_PROBLEM_CORROBORATION: The adaptation-necessity half is corroborated from outside the beneficiary set: IPCC working groups and the UNEP Adaptation Gap series attest that protective needs are real and growing under every trajectory. The inevitability half — that 2-3C is unavoidable and mitigation is costly prevention not worth its price — is attested mainly by fossil-funded policy institutes and industry associations inside the beneficiary set, and is contested from outside by mitigation economists, the climate science community, and small island state submissions. No source outside the beneficiary set attests the inevitability half; that absence is itself signal.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.78 at interval end) because the regime externalizes its largest cost category — unmitigated warming damages — onto parties who never consented and cannot bill, while simultaneously preserving fossil asset values that a prevention-first regime would strand; the transfer is decoupled from any service rendered to the payers. Suppression (0.68) is real but mostly non-coercive: the regime holds through an inevitability narrative, discounting conventions that render future harm small in official analysis, and sustained lobbying, rather than through policing — the payers largely cannot resist because they are absent (future people) or structurally outgunned (Global South, island states), not because they are suppressed in the interpersonal sense. Theater (0.44) is rising: adaptation strategies, resilience frameworks, and planning documents proliferate faster than funded implementation, so a growing share of the regime's visible activity is performative cover for the priority ordering. Accessibility collapse is moderate (0.45): the mitigation-first alternative remains fully visible in discourse and is periodically enacted (Paris, national net-zero laws), so alternatives have not collapsed — the regime holds through budget allocation, not through closing conceptual exits. Resistance (0.72) is high and organized: youth constitutional litigation, island-state coalitions, climate movements, and parts of the scientific community actively contest the priority ordering. The measurement series run on one shared time grid so every tracked metric is authored at every examined point; all three series rise over the interval as the pledge-delivery gap widens, lock-in accumulates, and more narrative and enforcement work is needed to hold the ordering against growing resistance.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structural data. From the agenda-setter seat the regime is prudent fiscal management under uncertainty — a responsible hedge, not a choice of victims. From the fossil-capital seat it is ordinary asset protection. From the payer seats the same arrangement is life-determining externalization: future generations experience pure exposure without consent, small island states experience it as territorial risk, Global South populations as underfinanced adaptation against intensifying impacts. The wealthy-electorate seat sits nearest symmetric — real avoided transition costs, real buffered damages. The engine derives these per-seat divergences from the structural declarations; the divergence itself is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map to directionality as follows. Beneficiaries: wealthy_nation_current_electorates avoid transition costs but bear residual buffered impacts, so they sit near but not at the beneficiary end; fossil_capital_owners, with arbitrage-grade capital mobility and pure asset protection, sit nearest the beneficiary end; adaptation_infrastructure_industry collects the spending pipeline and is mobile. Victims: future_generations — powerless, trapped, civilizational horizon — sit nearest the full-target end, the purest target seat in the story; small_island_nations are trapped by territory itself; global_south_populations have constrained exit via wealthy-state border regimes. The agenda-setter seat is genuinely dual: it administers the arrangement and absorbs some of its fiscal and reputational costs, sitting nearer symmetric than a pure beneficiary. Scope is global, which amplifies effective extraction for the trapped targets because verification of harm and enforcement of protection are weakest at planetary scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves of the structure visible, and that is exactly what the mandatrophy question turns on here. Reading the regime as a pure snare would erase the protective function that climate-vulnerable populations themselves need — adaptation is not a cover story in the way a pure extraction's coordination story is; sea defenses and early-warning systems save lives under every reading of the kernel, and the victims of this arrangement are simultaneously its would-be beneficiaries on the adaptation side. Reading it as a pure rope would erase the asymmetric cost-shift: the coordination is real, but the priority ordering that subordinates prevention to resilience transfers the largest cost category to non-consenting parties and protects a specific asset class. On obsolescence: the founding problem has a live half (adaptation need grows under every trajectory — corroborated outside the beneficiary set) and a contested half (the inevitability of 2-3C and the not-worth-the-cost premise). If the inevitability premise collapses empirically, the regime does not vanish; it reverts from a response-priority into a component of a broader response. The 'instead of prevention' mandate is what would be resolved, and the unimplemented-resilience-strategy share of the theater ratio is the part that would decay first.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_premise_status,
    'Is 2-3C warming actually locked in by physical and economic inertia, or is it a politically constructed inevitability that present cost-avoidance produces and then cites as its own justification?',
    'Integrated assessment modeling of 1.5-2C pathway feasibility under observed policy, updated against realized emissions and infrastructure lock-in data; the UNEP Emissions Gap series tracks exactly this gap year over year.',
    'If deep mitigation remains feasible at costs comparable to projected adaptation-plus-damage costs, the reading''s foundational inevitability axiom is empirically undermined and the arrangement loses its core justification; if lock-in is real, part of the measured extraction is misattributed and the prudence defense strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_premise_status, empirical, 'Whether the accepted warming trajectory is physical fact or self-fulfilling political construction.').

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the adaptation_priority reading of the climate_response_obligation kernel; what structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'Structural comparison across the three reading-stories. The disagreement is located in two places: (a) the inevitability premise — adaptation_priority holds 2-3C as given, mitigation_priority holds minimization as required, degrowth_reading holds throughput reduction as required; and (b) the definition of the cost-bearing party — present enfranchised populations versus future generations and the climate-vulnerable present.',
    'Under the mitigation_priority reading, fossil capital exits the beneficiary set, future generations move toward the beneficiary side, and the epsilon referent changes to a different arrangement (a different constraint story); under degrowth_reading the beneficiary set shifts again. Classification is per-reading; no reading''s epsilon transfers to another.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    discount_rate_contestation,
    'Are the discounting conventions that shrink future climate damages in official cost-benefit analysis ethically neutral technical choices, or a structural mechanism by which the present renders its victims invisible?',
    'Normative analysis of intergenerational discounting combined with revealed-preference comparison: what discount rates the same institutions apply to their own long-horizon assets (pensions, infrastructure, sovereign debt) versus to future climate harms.',
    'If the discount is self-serving rather than principled, the regime''s effective suppression is higher than the structural measure suggests — the mechanism is narrative-internalized in official analysis — and measured extraction understates the true transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discount_rate_contestation, conceptual, 'Whether intergenerational discounting is neutral technique or the arrangement''s central suppression mechanism.').

omega_variable(
    adaptation_genuine_share,
    'What share of declared adaptation activity is implemented, funded protection versus planning documents, strategies, and announcements?',
    'Budget-versus-implementation audit; the UNEP Adaptation Gap Report series tracks finance delivered against needs and the implementation status of national adaptation plans.',
    'Sets the theater ratio''s ground truth: a high unimplemented share would confirm performative maintenance of the resilience narrative and push the adaptation component toward the degraded, theatrical end of the lifecycle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_genuine_share, empirical, 'Implemented versus performative share of adaptation activity.').

omega_variable(
    adaptation_distribution_omega,
    'Does adaptation investment actually concentrate in wealthy regions while climate-vulnerable regions face a widening adaptation finance gap?',
    'Cross-national adaptation finance flow data (UNEP Adaptation Gap, OECD development tracking) measured against exposure-weighted need.',
    'If concentration is confirmed, the victim set''s realized exposure is deeper than headline adaptation spending suggests, raising effective extraction on the Global South seat beyond what aggregate figures imply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_distribution_omega, empirical, 'Geographic distribution of adaptation investment versus exposure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_response_obligation__adaptation_priority, theater_ratio, 1992, 0.2).
narrative_ontology:measurement(clim_tr_t2001, climate_response_obligation__adaptation_priority, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(clim_tr_t2010, climate_response_obligation__adaptation_priority, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(clim_tr_t2015, climate_response_obligation__adaptation_priority, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__adaptation_priority, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2025, climate_response_obligation__adaptation_priority, theater_ratio, 2025, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_response_obligation__adaptation_priority, base_extractiveness, 1992, 0.45).
narrative_ontology:measurement(clim_be_t2001, climate_response_obligation__adaptation_priority, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(clim_be_t2010, climate_response_obligation__adaptation_priority, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(clim_be_t2015, climate_response_obligation__adaptation_priority, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__adaptation_priority, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement(clim_be_t2025, climate_response_obligation__adaptation_priority, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_response_obligation__adaptation_priority, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement(clim_su_t2001, climate_response_obligation__adaptation_priority, suppression_requirement, 2001, 0.46).
narrative_ontology:measurement(clim_su_t2010, climate_response_obligation__adaptation_priority, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(clim_su_t2015, climate_response_obligation__adaptation_priority, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__adaptation_priority, suppression_requirement, 2020, 0.63).
narrative_ontology:measurement(clim_su_t2025, climate_response_obligation__adaptation_priority, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, degrowth_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate response obligation' decomposes into three structurally distinct readings of one kernel: mitigation_priority (obligation = rapid decarbonization), adaptation_priority (this story; obligation = resilience against an accepted trajectory), and degrowth_reading (obligation = throughput reduction). Each reading instantiates a different constraint with a different epsilon, different beneficiary/victim sets, and different enforcement structure; this story authors epsilon only for the adaptation-priority arrangement. Coupling: adaptation-priority governance creates carbon and infrastructure lock-in that raises the cost of the mitigation path and strengthens the inevitability narrative — structural pressure on the mitigation reading's operating environment without logical foreclosure. The upstream empirical claims (climate sensitivity, impact projections) feed all three readings; the readings diverge on the normative response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
