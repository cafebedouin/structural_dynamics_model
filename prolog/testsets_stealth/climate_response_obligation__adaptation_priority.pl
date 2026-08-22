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
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response (2-3C Acceptance Regime)
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the adaptation_priority reading of the
 *   climate_response_obligation kernel: the standing arrangement under which
 *   public authorities treat 2-3C of warming as a settled planning baseline,
 *   allocate marginal climate budgets to resilience rather than prevention,
 *   and frame mitigation as prohibitively costly or already foreclosed. The
 *   referent of epsilon is this standing arrangement itself - the
 *   adaptation-priority budget-and-framing regime as it operates - never the
 *   mitigation-priority arrangement this reading declines to build, and never
 *   the sibling readings (mitigation_priority, degrowth_reading), which are
 *   separate constraints in the same kernel family. The arrangement has a
 *   genuine coordination core (adaptation to already-committed warming is
 *   needed under every reading of the obligation) wrapped around an
 *   asymmetric transfer (the costs of not-preventing land on parties with no
 *   seat in any budget process). Claim and metrics are authored
 *   independently: the authoring seat reads the structure as a hybrid - real
 *   coordination function, real asymmetric extraction, actively maintained
 *   inevitability framing - and the metrics describe that structure
 *   descriptively without being tuned to any predicted engine output.
 *
 * KEY AGENTS:
 *   - fossil_capital_owners: primary beneficiary (powerful/arbitrage) - holds assets whose value depends on the arrangement, funds the inevitability framing
 *   - wealthy_national_governments: agenda_setter (institutional/constrained) - sets the budget priority, answers to present electorates
 *   - current_generation_consumers: beneficiary (moderate/constrained) - avoids transition costs, receives visible resilience goods
 *   - adaptation_engineering_contractors: secondary beneficiary (organized/mobile) - collects the adaptation budget as contract revenue
 *   - wealthy_region_municipalities: secondary beneficiary (organized/constrained) - receive the concentrated adaptation investment
 *   - future_generations: primary target (powerless/trapped) - inherit unmitigated impacts with no seat and no exit
 *   - global_south_populations: primary target (powerless/trapped) - highest exposure, lowest adaptation finance per exposed person
 *   - low_lying_island_communities: existential target (powerless/trapped) - territory and political community at stake
 *   - youth_climate_litigants: excluded challenger (organized/constrained) - argues for the seatless from outside budget processes
 *   - global_south_finance_negotiators: excluded claimant (organized/constrained) - present in treaty forums, excluded from enforceable outcomes
 *   - ipcc_scientific_assessors: analytical observer (institutional/analytical) - attests mitigation feasibility and adaptation limits without budget authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.7).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.58).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.7).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Adaptation-Priority Climate Response (2-3C Acceptance Regime)").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, '0e271a77-0634-49b4-859a-6178ae576fbb').
narrative_ontology:cs_kernel_codification('0e271a77-0634-49b4-859a-6178ae576fbb', distributed).
narrative_ontology:cs_authority_grounding('0e271a77-0634-49b4-859a-6178ae576fbb', distributed).
narrative_ontology:cs_reading_relation('0e271a77-0634-49b4-859a-6178ae576fbb', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('0e271a77-0634-49b4-859a-6178ae576fbb', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('0e271a77-0634-49b4-859a-6178ae576fbb', foundational, resilience_suffices_for_committed_warming).
narrative_ontology:cs_axiom_status(resilience_suffices_for_committed_warming, holdable).
narrative_ontology:cs_axiom_grounding('0e271a77-0634-49b4-859a-6178ae576fbb', resilience_suffices_for_committed_warming, instrumental).
narrative_ontology:cs_axiom('0e271a77-0634-49b4-859a-6178ae576fbb', foundational, market_discounting_justifies_prevention_deprioritization).
narrative_ontology:cs_axiom_status(market_discounting_justifies_prevention_deprioritization, holdable).
narrative_ontology:cs_axiom_grounding('0e271a77-0634-49b4-859a-6178ae576fbb', market_discounting_justifies_prevention_deprioritization, conventional).
narrative_ontology:cs_reference_frame('0e271a77-0634-49b4-859a-6178ae576fbb', resilience_within_committed_warming).
narrative_ontology:cs_drift_state('0e271a77-0634-49b4-859a-6178ae576fbb', post_renewables_cost_decline, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0e271a77-0634-49b4-859a-6178ae576fbb', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_capital_owners).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_consumers).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, adaptation_engineering_contractors).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, wealthy_region_municipalities).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_populations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, low_lying_island_communities).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, warming_inevitability_thesis).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, market_discount_rate_adequacy).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, adaptation_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold reserves, pipelines, and processing assets whose booked value depends on continued fossil throughput. Fund think tanks, advertising, and lobbying that promote the framing that warming of 2-3C is already locked in and that resilience spending is the responsible use of public money. Capital is mobile: portfolios can be rebalanced, firms rebranded, holdings shifted toward adaptation-adjacent assets, so exit from the framing's protection is cheap even though the assets themselves lose value if prevention resumes at scale.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_capital_owners, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, fossil_capital_owners, agenda_setter).

% Avoid the energy-price increases, carbon taxes, and transition levies that a prevention-first budget would require, and receive visible resilience goods - flood defenses, heat-action plans, hardened grids - paid from the same budgets. Their costs under the arrangement are modest and their immediate benefits tangible; they cannot cheaply exit their national energy systems, and their attention is on present prices rather than the distribution of future climate harm.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_generation_consumers, beneficiary,
    moderate, immediate, constrained, national).

% Win public contracts for sea walls, resilient infrastructure, climate-risk consulting, and managed-retreat engineering. Revenue scales with adaptation budget lines, so the arrangement's spending priorities are their order book; they can move crews and bids to other infrastructure markets if priorities shift.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, adaptation_engineering_contractors, beneficiary,
    organized, biographical, mobile, global).

% Set the budget priority between prevention and resilience, publish national adaptation plans, and administer the finance commitments made in treaty processes. They answer to electorates whose prevention costs would be immediate and concentrated while prevention benefits are diffuse and delayed, and they are locked to their own territories' climate exposure - they cannot exit the arrangement they administer without electoral cost.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_national_governments, agenda_setter,
    institutional, biographical, constrained, national).

% Will inherit whatever warming accumulates plus whatever resilience was built, bearing unmitigated impacts - heat mortality, crop stress, sea-level rise, extreme-event damages - with no seat in any current budget process and no exit from the climate they inherit. Their interests are represented only vicariously, by litigants and advocates who hold no vote.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, generational, trapped, global).

% Live in regions with the highest climate exposure and the lowest adaptation finance per exposed person. Adaptation investment concentrates elsewhere, migration options are narrowed by border regimes, and the impacts they bear grow with every year of deprioritized prevention. They hold little leverage over the budget decisions made in wealthy capitals.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_populations, payer,
    powerless, biographical, trapped, continental).

% Face existential exposure to sea-level rise and storm intensification. Adaptation finance reaches them late and small; their territory, freshwater lenses, and political community cannot be relocated intact. Their delegations attend treaty processes but cannot enforce finance or prevention commitments against large emitters.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, low_lying_island_communities, payer,
    powerless, biographical, trapped, local).

% Receive the concentrated share of adaptation investment - defenses, cooling infrastructure, water security - that protects their property values and tax bases. Their demand shapes where adaptation budgets land, and their geography makes them the last constituencies to be abandoned, but they cannot exit their exposure either.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_region_municipalities, beneficiary,
    organized, biographical, constrained, regional).

% Bring suits and mobilize for binding prevention obligations, arguing on behalf of the parties with no seat. They stand outside budget-setting processes; their standing to represent future interests is contested in most jurisdictions, and their wins to date have been declaratory or narrowly procedural.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, youth_climate_litigants, excluded,
    organized, biographical, constrained, global).

% Press in treaty negotiations for binding adaptation-finance and loss-and-damage commitments. Their demands are acknowledged in preambles and excluded from enforceable budget outcomes; exiting the treaty process would cost them the only forum where their claims are heard at all.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_finance_negotiators, excluded,
    organized, biographical, constrained, continental).

% Assess mitigation feasibility, adaptation limits, and remaining carbon budgets. Their findings bear directly on both premises of the arrangement - that warming of 2-3C is locked in and that resilience suffices - but they hold no budget authority, and their assessments enter the policy process only as advice.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, ipcc_scientific_assessors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, fossil_capital_owners).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Adapting settlements, water systems, agriculture, and health systems to warming that is already committed to the climate system regardless of near-term policy choice - sea defenses, heat-action plans, drought-tolerant crops, and early-warning systems solve a real protection problem that exists under every reading of the obligation.
% TRANSFER_FUNCTION: Moves the cost of climate response away from present emitters and asset holders (who would bear prevention costs) onto future generations and low-adaptation-capacity regions (who bear unmitigated impacts); moves public adaptation budgets toward infrastructure in wealthy regions; protects fossil asset values from stranding.
% ABSENT_VOICES: Future generations have no seat in any budget process. Youth litigants argue from outside the room; Global South finance negotiators are present in treaty forums but structurally unable to convert demands into enforceable outcomes; island-state delegations are formally seated without leverage over the finance or prevention decisions that determine their exposure.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority allocation and its inevitability framing vanished overnight, marginal public climate spending would reflow toward prevention (the funds are fungible and mitigation capacity exists), fossil assets would reprice toward stranding, and the distribution of climate harm would shift toward present emitters - the arrangement, not the climate system, holds the current allocation in place.
% FOUNDING_PROBLEM: Some warming was already committed by historical emissions, so populations needed protection from impacts no near-term policy could prevent; simultaneously, prevention's costs are concentrated on present voters and industries while its benefits are diffuse and delayed - a collective-action problem stretched across generations.
% FOUNDING_PROBLEM_CORROBORATION: IPCC impact and adaptation chapters corroborate the genuine need for adaptation to committed warming, from outside the arrangement's benefiting parties. No independent source corroborates the arrangement's stronger premise - that warming of 2-3C is inevitable and prevention is not worthwhile; IPCC mitigation pathways, IEA cost analyses, and national engineering assessments attest that rapid decarbonization remains technically and economically feasible, actively contradicting the inevitability premise. The adaptation need is corroborated; the deprioritization of prevention is attested only by the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.70 for the standing arrangement: the regime delivers real resilience goods (the coordination core) while the inevitability framing transfers the costs of not-preventing onto parties with no seat - high, but short of pure extraction because the adaptation function is genuine and partially delivered. Suppression is authored at 0.58 as a raw structural property, unscaled by power or scope: the foreclosure of the prevention alternative is budgetary and discursive (framing, lobbying, infrastructure path dependency, electoral horizons) rather than physically coercive, but it is real and actively maintained. Theater ratio 0.32: announced resilience systematically exceeds delivered resilience (the documented adaptation-finance gap), so roughly a third of the arrangement's visible activity is performative. Accessibility collapse 0.45: the prevention alternative has not collapsed - it remains technically available and increasingly cheap - the collapse is political, not technical. Resistance 0.62: litigation, mass mobilization, island-state diplomacy, and Global South finance demands meet the arrangement continuously. All three series run on one shared grid (T0 approximately 2015, points every 6 years to T30 approximately 2045); points 0 and 6 are observed, later points projected. The trajectories are monotonic ratchets, not cycles: committed warming and the adaptation gap accumulate, and enforcement effort must rise as mitigation gets cheaper because the suppressed alternative keeps becoming more attractive - there is no reconciliation phase to oscillate around.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From wealthy_national_governments the arrangement appears as prudent risk management: adaptation addresses certain near-term harms while prevention's benefits are diffuse, delayed, and discounted. From fossil_capital_owners it appears as realism. From future_generations, global_south_populations, and low_lying_island_communities the identical structure operates as a transfer of harm across time and borders to seats with no vote. The coalition problem is structural: the three payer groups are disjoint across time, borders, and generation, so their combined stake - the largest in the arrangement - never converts into agenda-setting power. The engine computes per-seat classifications from the declared power, exit, and directionality data; the divergence between seats is the measurement, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   fossil_capital_owners and current_generation_consumers are declared beneficiaries with arbitrage and constrained exit respectively - the derivation places them near the beneficiary end, damping their effective extraction. adaptation_engineering_contractors and wealthy_region_municipalities are secondary beneficiaries: they collect contract revenue and concentrated investment without running the arrangement. future_generations, global_south_populations, and low_lying_island_communities are declared victims with trapped exit - the derivation places them near the full-target end, amplifying their effective extraction, and the arrangement's global scope scales that amplification further. wealthy_national_governments hold the agenda_setter seat with no declared beneficiary or victim position; their derived directionality reflects administration of the arrangement rather than a declared stake, and no directionality override is needed because the derivation from the declared structure is adequate.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure extraction would erase the genuine adaptation function - populations do need protection from warming already committed by historical emissions, and defunding resilience would harm the very parties the extraction reading claims to defend. Reading it as pure coordination would license the inevitability cover and erase the transfer to the seatless. The hybrid classification holds both: the coordination function (adaptation to committed warming) and the asymmetric extraction (the inevitability framing transfers prevention costs across time and borders) coexist in one structure and require active enforcement to stay combined. The mandatrophy question - whether the founding problem (protecting against committed warming) has been superseded by asset protection - is carried by the R5 mismatch: a contested founding-problem status paired with a world_rearranges disappearance verdict flags an arrangement held in place by allocation and framing rather than by necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the climate_response_obligation kernel: does the epsilon, beneficiary/victim structure, and classification authored here hold only for the adaptation_priority reading, and what would the mitigation_priority or degrowth_reading instantiations change structurally?',
    'Author the sibling readings as separate constraints (mitigation_priority, degrowth_reading) and compare per-seat classifications. The disagreement is located in what the response obligation IS - protection within committed warming versus prevention versus sufficiency - not in the climate science all readings share.',
    'Under mitigation_priority, current_generation_consumers and fossil_capital_owners move into the payer set and future_generations into the beneficiary set, inverting the directionality profile; under degrowth_reading, wealthy consumers join the payer set and the throughput question enters the coordination function. This story''s high epsilon does not transfer to the siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    warming_lockin_contingency,
    'Is warming of 2-3C actually locked in given current technology, costs, and remaining carbon budgets, or is the inevitability premise a policy artifact that the arrangement itself produces through self-fulfilling delay?',
    'Integrated assessment of remaining carbon budgets against declining mitigation costs (IEA/IRENA cost series, IPCC feasibility assessments); natural experiments from jurisdictions that decoupled emissions from economic growth.',
    'If prevention remains feasible at declining cost, the inevitability premise fails and the arrangement''s cost transfer is chosen rather than forced - effective extraction rises toward the pure-extraction end and the vindicated inevitability thesis is exposed as cover. If lock-in is genuine, part of the measured extraction is misattributed necessity and the coordination share is larger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warming_lockin_contingency, empirical, 'Whether the inevitability premise is physical fact or produced by the arrangement that invokes it.').

omega_variable(
    adaptation_sufficiency_limits,
    'Can resilience investment actually protect welfare at 2-3C of warming, or does protection capacity hit hard limits (tipping elements, compound extremes, non-linear damages) that the resilience-suffices premise ignores?',
    'Adaptation-gap assessments, tipping-point literature, and stress tests of adaptation portfolios against compound-event scenarios at 2.5C and 3C.',
    'If adaptation has hard limits below 3C, the reading''s foundational axiom fails on its own instrumental terms - resilience cannot be the sufficient response it claims to be, and the arrangement loses even its prudence justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_sufficiency_limits, empirical, 'Whether the resilience-suffices axiom survives contact with high-warming damage functions.').

omega_variable(
    discount_rate_normativity,
    'Does market-rate discounting adequately represent intergenerational obligation, or is the choice of discount rate itself the normative decision that manufactures the arrangement''s apparent efficiency?',
    'Not resolvable by data alone: requires an explicit normative commitment on the moral weight of future persons. Empirical damage functions bound the stakes but do not settle the rate.',
    'A near-zero discount rate inverts the cost-benefit case: prevention becomes the efficient priority and the deprioritization axiom loses its conventional warrant; the arrangement''s efficiency claim is revealed as a value choice rather than a finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discount_rate_normativity, preference, 'Whether the discounting convention that justifies deprioritizing prevention is a technical input or a smuggled ethical premise.').

omega_variable(
    mitigation_foreclosure_mechanism,
    'Is the suppression of the prevention alternative primarily structural (budget lock-in, infrastructure path dependency, electoral time horizons) or internalized (fatalism - the belief that it is too late to act - among publics and policymakers)?',
    'Post-framing attitude trajectories: if support for prevention recovers when the inevitability framing is challenged with cost data, the suppression is substantially internalized; if budget structures persist regardless of public support, it is structural.',
    'If internalized, effective suppression exceeds the structural measure - the fatalism travels with the policymakers and outlives any single budget cycle, shifting the enforcement machinery''s characterization toward identity-lock dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_foreclosure_mechanism, empirical, 'Structural versus internalized mechanism maintaining the foreclosure of the prevention alternative.').

omega_variable(
    adaptation_distribution_skew,
    'Does adaptation investment in fact concentrate in wealthy regions while residual risk accumulates in the Global South, or do finance mechanisms (adaptation funds, loss-and-damage commitments) materially offset the skew?',
    'Track adaptation-finance flows by recipient region against exposure-weighted need, using UNEP Adaptation Gap reports and COP finance-commitment delivery records.',
    'If the skew holds, the global_south_populations seat sits near the full-target end and the arrangement''s effective extraction is amplified by its global scope; if finance delivery closes the gap, the victim structure narrows and the arrangement moves toward the coordination end of the hybrid spectrum.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_distribution_skew, empirical, 'Whether the declared geographic concentration of adaptation benefit is real and persistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t6, climate_response_obligation__adaptation_priority, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(clim_tr_t6, observed).
narrative_ontology:measurement(clim_tr_t12, climate_response_obligation__adaptation_priority, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(clim_tr_t12, projected).
narrative_ontology:measurement(clim_tr_t18, climate_response_obligation__adaptation_priority, theater_ratio, 18, 0.29).
narrative_ontology:measurement_basis(clim_tr_t18, projected).
narrative_ontology:measurement(clim_tr_t24, climate_response_obligation__adaptation_priority, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(clim_tr_t24, projected).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__adaptation_priority, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(clim_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t6, climate_response_obligation__adaptation_priority, base_extractiveness, 6, 0.6).
narrative_ontology:measurement_basis(clim_be_t6, observed).
narrative_ontology:measurement(clim_be_t12, climate_response_obligation__adaptation_priority, base_extractiveness, 12, 0.64).
narrative_ontology:measurement_basis(clim_be_t12, projected).
narrative_ontology:measurement(clim_be_t18, climate_response_obligation__adaptation_priority, base_extractiveness, 18, 0.67).
narrative_ontology:measurement_basis(clim_be_t18, projected).
narrative_ontology:measurement(clim_be_t24, climate_response_obligation__adaptation_priority, base_extractiveness, 24, 0.69).
narrative_ontology:measurement_basis(clim_be_t24, projected).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__adaptation_priority, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(clim_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t6, climate_response_obligation__adaptation_priority, suppression_requirement, 6, 0.47).
narrative_ontology:measurement_basis(clim_su_t6, observed).
narrative_ontology:measurement(clim_su_t12, climate_response_obligation__adaptation_priority, suppression_requirement, 12, 0.51).
narrative_ontology:measurement_basis(clim_su_t12, projected).
narrative_ontology:measurement(clim_su_t18, climate_response_obligation__adaptation_priority, suppression_requirement, 18, 0.54).
narrative_ontology:measurement_basis(clim_su_t18, projected).
narrative_ontology:measurement(clim_su_t24, climate_response_obligation__adaptation_priority, suppression_requirement, 24, 0.57).
narrative_ontology:measurement_basis(clim_su_t24, projected).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__adaptation_priority, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(clim_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, degrowth_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate response' decomposes into three structurally distinct readings of the climate_response_obligation kernel. This story authors the adaptation_priority arrangement only: epsilon 0.70 refers to the standing adaptation-priority budget-and-framing regime, with current-generation consumers and fossil capital in the beneficiary set and future generations, Global South populations, and island communities in the victim set. The mitigation_priority sibling authors the prevention arrangement with an inverted beneficiary/victim structure; the degrowth_reading authors the throughput-sufficiency arrangement with wealthy consumers added to the payer set. Relation choices: adaptation_priority to mitigation_priority is 'influences' because the adaptation-priority allocation consumes the carbon budget and legitimacy space that the mitigation reading's feasibility depends on - structural downstream pressure without logical foreclosure; adaptation_priority to degrowth_reading is 'coexists_with' because both remain live positions held by different parties and neither reading's core premise logically eliminates the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
