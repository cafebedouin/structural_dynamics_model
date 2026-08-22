% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the baseload-necessity reading of the
 *   contested climate-mitigation-legitimacy kernel: the claim that reliable
 *   deep decarbonization structurally requires dispatchable baseload power
 *   (principally nuclear, alongside firm gas-with-CCS or similar) that
 *   intermittent renewables cannot supply at the scale and reliability grids
 *   demand. This reading treats renewable-only or renewable-dominant pathways
 *   as inadequate on physical-engineering grounds, not merely as a slower or
 *   costlier option. The reading has real coordination value — it lets
 *   long-horizon capital commit to specific technology roadmaps and lets grid
 *   operators plan resource adequacy against a stable standard — but it also
 *   concentrates rents in incumbent nuclear operators, their financiers, and
 *   the reliability-engineering bodies whose institutional authority depends
 *   on the dispatchable-generation paradigm remaining the accepted standard,
 *   at the expense of distributed renewable developers and captive ratepayers
 *   who bear nuclear cost-overrun risk. Three sibling readings of the same
 *   kernel (renewable-primacy, portfolio-pragmatism, degrowth-sufficiency)
 *   are NOT represented in this story; they are separate constraints with
 *   their own ε, beneficiaries, and classifications, linked here only by
 *   network reference.
 *
 * KEY AGENTS:
 *   - incumbent_nuclear_utilities: institutional beneficiary/agenda-setter — captures long-term capacity revenue and rate-basing treatment from the necessity framing
 *   - large_capital_infrastructure_financiers: institutional beneficiary — underwrites long-duration assets de-risked by the necessity claim
 *   - grid_reliability_engineering_establishment: organized beneficiary/agenda-setter — institutional authority tied to the dispatchable-generation paradigm
 *   - distributed_renewable_developers: moderate-power payer — discounted or excluded from adequacy credit under necessity-weighted rules
 *   - ratepayers_in_nuclear_cost_overrun_jurisdictions: powerless, trapped payer — bears cost-overrun risk with no exit
 *   - future_generations_bearing_climate_and_asset_lockin_risk: excluded, powerless — no procedural voice, inherits either climate or stranded-asset outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.52).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '5f720b97-3b7e-430d-893b-0b398e337c5e').
narrative_ontology:cs_kernel_codification('5f720b97-3b7e-430d-893b-0b398e337c5e', distributed).
narrative_ontology:cs_authority_grounding('5f720b97-3b7e-430d-893b-0b398e337c5e', expertise).
narrative_ontology:cs_interpretation_layer_present('5f720b97-3b7e-430d-893b-0b398e337c5e').
narrative_ontology:cs_reading_relation('5f720b97-3b7e-430d-893b-0b398e337c5e', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('5f720b97-3b7e-430d-893b-0b398e337c5e', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('5f720b97-3b7e-430d-893b-0b398e337c5e', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('5f720b97-3b7e-430d-893b-0b398e337c5e', foundational, dispatchable_synchronous_generation_is_physically_necessary).
narrative_ontology:cs_axiom_status(dispatchable_synchronous_generation_is_physically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('5f720b97-3b7e-430d-893b-0b398e337c5e', dispatchable_synchronous_generation_is_physically_necessary, empirically_contingent).
narrative_ontology:cs_axiom('5f720b97-3b7e-430d-893b-0b398e337c5e', secondary, renewable_only_pathways_are_reliability_inadequate_at_scale).
narrative_ontology:cs_axiom_status(renewable_only_pathways_are_reliability_inadequate_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('5f720b97-3b7e-430d-893b-0b398e337c5e', renewable_only_pathways_are_reliability_inadequate_at_scale, empirically_contingent).
narrative_ontology:cs_reference_frame('5f720b97-3b7e-430d-893b-0b398e337c5e', twentieth_century_synchronous_grid_engineering_paradigm).
narrative_ontology:cs_drift_state('5f720b97-3b7e-430d-893b-0b398e337c5e', post_storage_cost_collapse_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5f720b97-3b7e-430d-893b-0b398e337c5e', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_utilities).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, large_capital_infrastructure_financiers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_reliability_engineering_establishment).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers_in_nuclear_cost_overrun_jurisdictions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, community_energy_cooperatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate or plan large baseload nuclear fleets and lobby regulators and grid operators to classify dispatchable baseload as a mandatory reliability requirement rather than one option among several. Capture long-term capacity contracts, loan guarantees, and rate-basing treatment justified by the necessity claim. Their capital is sunk in long-lived assets that only pay off if the baseload-necessity framing holds for decades.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_utilities, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_utilities, agenda_setter).

% Underwrite multi-decade nuclear and large-scale dispatchable projects at high volumes because the necessity framing de-risks the asset class by making it politically load-bearing. Benefit from stable long-duration returns that a renewables-and-storage-dominant grid would not generate in the same form.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, large_capital_infrastructure_financiers, beneficiary,
    institutional, generational, arbitrage, global).

% Sets reliability standards, capacity market rules, and resource adequacy models. Career and institutional legitimacy are built on engineering paradigms centered on synchronous, dispatchable generation; models that would validate high-renewable-plus-storage reliability threaten the standard-setting body's authority over what counts as 'firm' capacity.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_reliability_engineering_establishment, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, grid_reliability_engineering_establishment, agenda_setter).

% Build wind, solar, and storage projects but face capacity-market rules, interconnection queues, and reliability certifications weighted toward dispatchable baseload, discounting or excluding their output from adequacy credit. Cannot exit the regulatory framework that determines market access; can only lobby for rule changes from a structurally weaker position than incumbents.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_renewable_developers, payer,
    moderate, biographical, constrained, national).

% Bear rate increases and cost-recovery surcharges when nuclear projects justified as necessary infrastructure run over budget and behind schedule — a recurring pattern in large baseload builds. Cannot choose their utility or exit the rate base; the necessity framing insulates the project from cancellation even as costs balloon.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers_in_nuclear_cost_overrun_jurisdictions, payer,
    powerless, biographical, trapped, regional).

% Operate small-scale distributed and community-owned renewable generation. Structurally disadvantaged by resource-adequacy frameworks built around centralized dispatchable capacity, which treat their contribution as unreliable regardless of aggregate performance data, limiting access to capacity payments and long-term contracts.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, community_energy_cooperatives, payer,
    powerless, biographical, constrained, local).

% Produce integrated assessment models and grid simulations bearing on whether high-renewable-plus-storage pathways can achieve reliability parity with baseload-centric pathways. Findings are contested and cited selectively by all sides of the kernel dispute; some modelers hold the necessity view, others dispute it.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_scientists_and_energy_modelers, observer,
    analytical, civilizational, analytical, global).

% Will inherit either the climate outcomes of the decarbonization pathway chosen now, or the stranded-asset and decommissioning liabilities of long-lived nuclear infrastructure if the necessity claim proves overstated. Have no seat in current capacity-market or licensing proceedings.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, future_generations_bearing_climate_and_asset_lockin_risk, excluded,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_nuclear_utilities).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__baseload_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-term investment and grid-planning decisions around a shared claim that dispatchable, synchronous generation is structurally required for reliability at deep decarbonization levels — allowing regulators, financiers, and grid operators to commit capital and licensing timelines to a stable technology roadmap rather than face a moving target of contested reliability claims.
% TRANSFER_FUNCTION: Moves capacity-market revenue, loan guarantees, rate-basing authority, and regulatory priority toward incumbent nuclear operators and their financiers, and away from distributed renewable and storage developers whose output is discounted or excluded under adequacy frameworks built on the necessity premise; moves cost-overrun risk onto captive ratepayers.
% ABSENT_VOICES: Distributed and community-scale renewable developers are present but structurally outvoted in standard-setting bodies dominated by incumbent utility and engineering-establishment representatives. Future generations bearing either climate risk or stranded-asset liability have no procedural voice in current capacity-market or licensing decisions at all.
% DISAPPEARANCE_RATIONALE: If the baseload-necessity framing were abandoned overnight, capacity markets and reliability standards would need to be rebuilt around a different adequacy paradigm — nuclear projects reliant on necessity-justified guarantees and rate treatment could lose their financing basis, while renewable and storage developers would gain market access. Whether this counts as 'the world rearranging' or 'correcting a temporary distortion' is exactly the site of the kernel dispute: incumbents say the underlying physical reliability requirement is real and would reassert itself through blackouts; challengers say the requirement is an artifact of engineering-paradigm lock-in, not physics.
% FOUNDING_PROBLEM: Early decarbonization planning needed a way to guarantee grid reliability while retiring fossil dispatchable capacity, and dispatchable baseload (originally coal, then extended to nuclear) was the only proven technology class capable of firm, synchronous, weather-independent output at the scale utilities were used to planning around.
% FOUNDING_PROBLEM_CORROBORATION: Grid reliability engineers and incumbent utilities attest the problem remains live, citing frequency-stability and dispatchability studies. Independent energy modelers outside the beneficiary set (some academic and NGO-affiliated groups) report grid simulations in which high-renewable-plus-storage-plus-demand-response portfolios achieve comparable reliability metrics without new baseload builds, directly contesting the founding problem's continued necessity — this is not an unopposed origin story.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a real but partial coordination function riding alongside genuine rent concentration: the necessity claim does real planning work (it is not pure fiction — dispatchability is a real engineering property) but its treatment as an absolute requirement rather than a contestable modeling assumption channels capacity payments, loan guarantees, and regulatory priority toward incumbents whose long-lived assets depend on the claim holding. Suppression (0.52) is moderate — the constraint operates mainly through standard-setting and capacity-market rule design rather than overt coercion, but rule design that discounts non-dispatchable resources functions as a soft exclusion mechanism. Theater ratio is comparatively low (0.28): most of the activity is genuine engineering and financial commitment, not performance, though public reliability messaging increasingly outpaces the contested state of the underlying modeling literature. Resistance is substantial (0.62) because distributed renewable developers, some academic modelers, and increasingly some regulators actively contest the necessity framing with competing simulation evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent nuclear utilities and their financiers sit near the full-beneficiary end: they collect capacity revenue, loan guarantees, and rate-basing treatment that depend on the necessity claim's continued regulatory acceptance, and they have arbitrage-grade exit (they can redeploy capital elsewhere if the framing shifts, unlike their sunk domestic assets). The grid reliability engineering establishment benefits institutionally (authority, standard-setting relevance) even though individual engineers are not extracting rents personally — this is captured via the beneficiary declaration at the institutional level. Distributed renewable developers and ratepayers sit near the target end: they pay through discounted market access or direct rate surcharges and have constrained-to-trapped exit, since utility service territories and capacity-market participation rules are not something individual ratepayers or even project developers can opt out of.
 *
 * MANDATROPHY ANALYSIS:
 *   The necessity claim's founding problem — reliably retiring fossil dispatchable capacity without reliability collapse — was genuinely live when the framing consolidated, and grid engineers outside the beneficiary set continue to attest that frequency-stability requirements are physically real, not manufactured. This forecloses a naive extraction-only reading: the constraint has a genuine coordination substrate, which is why it is classified tangled_rope rather than snare. But the founding_problem_status is authored as 'contested' rather than 'live' because independent modeling work increasingly shows high-renewable-plus-storage portfolios achieving comparable reliability without new baseload builds — meaning the necessity claim's absolute (not merely probabilistic or context-dependent) form may have outlived the evidence that first justified it, while the rent flows built on that absolute form persist. This is precisely the mandatrophy signature: a genealogically real problem whose contemporary necessity is now disputed by parties outside the beneficiary set, while the institutional and financial structures built on the strong claim continue extracting as if the dispute were settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_necessity_vs_paradigm_lockin,
    'Is dispatchable baseload a genuine physical requirement for grid reliability at deep decarbonization levels, or is the necessity claim an artifact of engineering paradigms and capacity-market rules built around synchronous generation that could be redesigned around alternative reliability mechanisms (storage, demand response, transmission expansion, sector coupling)?',
    'Comparative grid-simulation studies from independent (non-utility, non-vendor-funded) modeling groups testing whether high-renewable-plus-storage-plus-demand-response portfolios can match dispatchable-baseload reliability metrics at comparable or lower cost across multiple climate zones and load profiles; resolution also requires observing real-world grids that have attempted high-renewable pathways without new baseload builds.',
    'If the necessity claim is a genuine physical requirement, the coordination function is stronger than the extraction reading suggests and the classification should weight toward the coordination pole. If it is substantially a paradigm-lock-in artifact, the constraint is closer to a snare wearing engineering-necessity clothing, and the FSM-adjacent dynamic (a claimed-natural technical requirement that happens to benefit identifiable incumbents) should be weighted more heavily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_necessity_vs_paradigm_lockin, empirical, 'Whether baseload necessity is physical fact or paradigm-dependent modeling artifact.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (baseload_necessity_reading) of the climate_mitigation_legitimacy kernel. The sibling readings (renewable_primacy, portfolio_pragmatism, degrowth_sufficiency) share the same underlying question — how should decarbonization investment be prioritized — but diverge on where the physical and economic bottleneck actually sits. Where exactly is the disagreement located: in the underlying physics/engineering (is dispatchability actually scarce?), in the economics (are storage costs falling fast enough to substitute?), or in values (is growth-compatible decarbonization even the right goal)?',
    'Structural decomposition of the disagreement: track whether contesting parties agree on physical facts but disagree on cost trajectories (economic locus), agree on both but disagree on acceptable risk/timeline tradeoffs (values locus), or dispute the physical facts themselves (empirical locus). Each locus implies a different resolution path and a different classification trajectory for this reading.',
    'If the disagreement is purely economic (cost trajectories), this reading''s necessity claim weakens over time as storage costs fall, and mandatrophy status should shift toward ''dead'' as evidence accumulates. If the disagreement is genuinely about physics that remains scarce/necessary, the reading''s coordination substrate holds. If it is a values dispute (risk tolerance, growth model), no amount of empirical resolution settles it and the kernel remains permanently contested across all four readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Where the kernel-level disagreement between readings is actually located: physics, economics, or values.').

omega_variable(
    nuclear_cost_overrun_pattern_generality,
    'Are the cost overruns and schedule delays observed in nuclear projects justified under the necessity framing a generalizable feature of large dispatchable-baseload construction, or specific to particular regulatory and supply-chain conditions that could be corrected without abandoning the necessity claim?',
    'Comparative cost-and-schedule analysis across jurisdictions with different regulatory regimes, supply chains, and construction experience (e.g., serial-build programs vs. first-of-a-kind projects) to isolate whether overruns are structural to the technology class or contingent on correctable institutional factors.',
    'If overruns are structural, the ratepayer-borne cost risk is an intrinsic feature of this reading''s beneficiary structure, strengthening the tangled_rope classification. If overruns are contingent and correctable, the victim-side cost burden could be substantially mitigated without changing the necessity claim itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_cost_overrun_pattern_generality, empirical, 'Whether nuclear cost overruns are structural to the technology or contingent on correctable institutional factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(clim_tr_t32, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(clim_be_t32, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(clim_su_t32, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__baseload_necessity_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the natural-language label 'the climate mitigation legitimacy debate' per the ε-invariance principle: the four readings measure different observables (physical dispatchability requirements vs. cost-trajectory comparisons vs. technology-neutral portfolio optimization vs. demand-side sufficiency) and would produce materially different ε values if collapsed into one story. Each reading is authored as its own constraint with its own beneficiary/victim structure; they are linked here as a constraint family via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
