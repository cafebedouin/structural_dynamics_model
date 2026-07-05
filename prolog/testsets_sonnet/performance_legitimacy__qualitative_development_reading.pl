% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Legitimacy via 'High-Quality Development' (Innovation/Sustainability/Efficiency Reading)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This story instantiates the 'qualitative development' reading of a
 *   contested performance-legitimacy kernel: the claim that a governing
 *   authority's right to rule is grounded not in raw GDP growth or headline
 *   employment figures, but in demonstrated structural transformation —
 *   innovation intensity, industrial upgrading, sustainability, and
 *   efficiency gains. Under this reading, official statistics, promotion
 *   criteria for local officials, credit allocation, and fiscal transfers are
 *   progressively reweighted away from property-driven and
 *   manufacturing-driven growth toward metrics legible as 'high quality':
 *   patent counts, R&D share, green-tech output, total factor productivity.
 *   The reading has a genuine coordination function — escaping a maturing,
 *   debt-fragile growth model requires synchronized signals no single actor
 *   can produce alone — but it also concentrates capital and
 *   legitimacy-conferring attention on a specific, state-cultivated
 *   innovation ecosystem while withdrawing support from traditional
 *   manufacturing regions and the local governments whose fiscal base
 *   depended on the old model. This is a genuinely distinct constraint from
 *   its sibling readings (quantitative_growth_reading,
 *   techno_nationalist_reading, livelihood_security_reading): each has a
 *   different ε, a different beneficiary/victim structure, and a different
 *   failure mode, and each should be authored as its own story per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - central_planning_technocrats: agenda_setter (institutional/analytical) — administers the reclassification of legitimate growth
 *   - state_backed_innovation_ecosystem: primary beneficiary (institutional/arbitrage) — captures reallocated capital and legitimacy
 *   - high_tech_sector_firms: beneficiary (powerful/mobile) — gains preferential treatment, can exit if conditions sour
 *   - traditional_manufacturing_workers: primary target (powerless/trapped) — bears withdrawal of support
 *   - property_dependent_local_governments: secondary target (moderate/constrained) — loses fiscal base, gains unfunded mandates
 *   - external_economic_analysts: analytical observer — assesses whether transformation is real or relabeling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.61).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.55).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Legitimacy via 'High-Quality Development' (Innovation/Sustainability/Efficiency Reading)").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '2eaeca05-8f37-4771-b4c3-20dd42eb4f1b').
narrative_ontology:cs_kernel_codification('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', distributed).
narrative_ontology:cs_authority_grounding('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', extraction).
narrative_ontology:cs_interpretation_layer_present('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b').
narrative_ontology:cs_reading_relation('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', foundational, structural_upgrading_supersedes_growth_rate).
narrative_ontology:cs_axiom_status(structural_upgrading_supersedes_growth_rate, holdable).
narrative_ontology:cs_axiom_grounding('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', structural_upgrading_supersedes_growth_rate, instrumental).
narrative_ontology:cs_axiom('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', foundational, efficiency_and_sustainability_gains_are_the_legitimate_measure_of_progress).
narrative_ontology:cs_axiom_status(efficiency_and_sustainability_gains_are_the_legitimate_measure_of_progress, holdable).
narrative_ontology:cs_axiom_grounding('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', efficiency_and_sustainability_gains_are_the_legitimate_measure_of_progress, conventional).
narrative_ontology:cs_reference_frame('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', reform_era_growth_primacy_framework).
narrative_ontology:cs_drift_state('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', post_property_slowdown_recalibration, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2eaeca05-8f37-4771-b4c3-20dd42eb4f1b', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sector_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, venture_capital_and_ma_intermediaries).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, central_planning_technocrats).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_workers).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, declining_industrial_regions).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, informal_and_low_skill_labor_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets national and provincial performance targets around innovation output, patent filings, R&D intensity, and industrial upgrading indices. Reallocates fiscal transfers, credit quotas, and cadre promotion criteria away from raw GDP and property-driven growth toward metrics legible as 'high-quality development.' Administers the reclassification and bears none of the transitional cost directly.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_planning_technocrats, agenda_setter,
    institutional, generational, analytical, national).

% State guidance funds, national champions in semiconductors, EVs, and advanced manufacturing, and affiliated research institutes receive preferential credit, land, and procurement under the new legitimacy framing. Their success is used as proof-of-concept that the transition is real, which further entrenches the resource flow toward them.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    institutional, generational, arbitrage, national).

% Private and mixed-ownership tech firms gain access to subsidized capital, tax holidays, and streamlined listing pathways because their activity fits the innovation-and-efficiency template. They can relocate operations or list offshore if domestic conditions sour, giving them leverage the older manufacturing base never had.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sector_firms, beneficiary,
    powerful, biographical, mobile, global).

% State-guided funds-of-funds, exchanges, and M&A advisory firms are built out specifically to intermediate the shift toward innovation financing. They capture fees and allocation power from a financial infrastructure that exists because this reading of legitimacy was adopted.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, venture_capital_and_ma_intermediaries, beneficiary,
    organized, biographical, arbitrage, national).

% Workers in legacy heavy industry, textiles, and low-margin export manufacturing see credit, subsidy, and political attention withdrawn as these sectors are reclassified as low-quality growth to be phased out. Retraining and relocation are promised but underfunded relative to the speed of capital reallocation; they cannot easily follow capital into innovation sectors requiring different skills.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_workers, payer,
    powerless, biographical, trapped, regional).

% Municipal and provincial governments whose fiscal base depended on land sales and construction-linked GDP lose revenue as the center deprioritizes property-driven growth. They are simultaneously pressured to fund innovation parks and R&D subsidies they cannot afford, given the shrinking of their traditional revenue instrument.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    moderate, biographical, constrained, regional).

% Entire regions built around now-deprioritized industries face capital flight, population outmigration to coastal tech hubs, and declining local services as national investment concentrates in innovation clusters. Geographic exit means abandoning homes, social networks, and local institutions.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, declining_industrial_regions, payer,
    powerless, generational, trapped, regional).

% Workers without credentials or capital to enter innovation-sector employment absorb the labor-market consequences of the transition — informal work, underemployment, or migration to shrinking manufacturing niches — without a seat in the redefinition of what counts as legitimate growth.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, informal_and_low_skill_labor_force, payer,
    powerless, immediate, trapped, local).

% Officials, firms, and workers whose legitimacy claim rests on raw GDP and employment numbers are structurally sidelined by this reading's redefinition of success; their objection — that innovation metrics undercount employment and near-term livelihood effects — is heard mainly as a rival kernel reading, not incorporated into this reading's own targets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, quantitative_growth_constituency, excluded,
    organized, biographical, constrained, national).

% International economists, rating agencies, and comparative political economists assess whether the qualitative-development framing reflects genuine productivity transformation or is a relabeling exercise that manages the optics of a growth slowdown while concentrating capital in politically favored sectors.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, external_economic_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuine industrial-policy problem: reallocating capital, R&D effort, and human capital away from a maturing, increasingly low-return growth model (property and heavy manufacturing) toward higher-value-add sectors, which requires synchronized fiscal, credit, and regulatory signals that no single firm or locality could produce alone.
% TRANSFER_FUNCTION: Moves fiscal transfers, preferential credit, land allocation, and political attention away from property-dependent local governments and traditional manufacturing regions toward state-backed innovation firms, venture and M&A intermediaries, and the technocratic apparatus that administers the reclassification.
% ABSENT_VOICES: Manufacturing workers, informal laborers, and property-dependent local governments have no institutionalized channel to contest the redefinition of legitimate growth; their objections surface as complaints about unemployment or fiscal shortfall rather than as a competing claim on what 'development' should mean. The quantitative-growth constituency's rival framing is structurally excluded from co-authoring this reading's targets.
% DISAPPEARANCE_RATIONALE: The innovation ecosystem, VC/M&A infrastructure, and technocratic KPI apparatus insist the transition is irreversible and structurally necessary — its disappearance would mean reverting to an exhausted, debt-fueled growth model. Manufacturing regions and property-dependent local governments would argue the world already rearranged around them (job loss, fiscal strain) and that reversion would restore, not disrupt, their prior stability. Whether removal 'rearranges the world' therefore depends on which seat is asked.
% FOUNDING_PROBLEM: Diminishing returns and rising financial fragility from a growth model dependent on real estate expansion, debt-financed infrastructure, and low-value-add export manufacturing, combined with a need to escape a technology and productivity ceiling relative to advanced economies.
% FOUNDING_PROBLEM_CORROBORATION: Central planning technocrats and state media attest the problem is live and the transition necessary. Independent economists and international financial institutions outside the beneficiary set corroborate that the underlying productivity and debt-sustainability problem is real, but diverge sharply from the technocrats on whether the specific reclassification of 'quality' growth resolves it or primarily redistributes capital toward politically favored sectors while underfunding the social costs of transition.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, contested).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects a real but partial transfer: capital, credit, and political attention move systematically from property/manufacturing-dependent actors to a state-cultivated innovation ecosystem, and the reallocation compounds over time as the KPI apparatus hardens (extraction rises from 0.34 to 0.61 across the interval as the reclassification becomes entrenched in cadre evaluation and fiscal design). Suppression (0.55) is moderate — dissent from manufacturing regions and quantitative-growth-oriented officials is not violently suppressed but is structurally excluded from the metric-setting process, and administrative promotion incentives increasingly punish officials who prioritize the old growth model. Theater ratio (0.42) is meaningfully above zero: some share of 'high-quality development' activity is genuine industrial upgrading, but a growing share is performative — provincial governments rebranding existing projects with innovation-sector labels to hit KPI targets without corresponding productivity gains. Accessibility collapse (0.50) and resistance (0.58) reflect that this is a live, contested policy framework, not a settled natural fact — local governments and displaced workers actively resist reclassification through informal noncompliance, underreporting, and political pressure, even though they cannot formally veto it.
 *
 * PERSPECTIVAL GAP:
 *   From the central-technocrat and innovation-ecosystem seats, this reading is a rope: a necessary, self-evidently superior coordination mechanism replacing an exhausted and fragile growth model. From the manufacturing-worker and property-dependent-local-government seats, the same structure computes as extractive: their prior claim on legitimacy-conferring resources is unilaterally revoked and reassigned without their participation in redefining the terms. The engine's per-seat computation is expected to surface exactly this divergence — that is the seat-divergence data point this story is built to produce, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The state-backed innovation ecosystem and its financial intermediaries sit near the full-beneficiary end of directionality: they were structurally created by, and collect the primary gains of, this legitimacy reading. High-tech firms benefit but retain mobility (arbitrage/mobile exit), which damps their effective extraction relative to trapped actors. Traditional manufacturing workers, informal labor, and declining industrial regions sit near the full-target end: they are powerless, trapped, and bear the withdrawal of support without a comparable channel to contest the redefinition. Property-dependent local governments occupy an intermediate position — moderate power, constrained exit — since they retain some administrative capacity to resist or slow-walk reclassification even as their fiscal base erodes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an exhausted, debt-fragile growth model with a productivity ceiling — is genuinely live, which prevents a blanket 'this is pure extraction' reading. But the specific instrument (KPI reclassification concentrating capital on a state-cultivated innovation ecosystem while defunding transition support for displaced workers and local governments) has visibly outrun the coordination need it was built to serve, evidenced by the theater_ratio climbing toward 0.42 as compliance activity increasingly substitutes for genuine upgrading. Classifying this as tangled_rope rather than snare or rope captures both facts simultaneously: real coordination function, real asymmetric extraction, both riding the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_transformation_vs_relabeling,
    'Is the shift toward ''high-quality development'' metrics tracking real productivity and technological transformation, or is it substantially a relabeling exercise that manages the optics of a growth slowdown while redirecting capital to politically favored sectors?',
    'Independent productivity and total-factor-productivity studies disaggregating genuine efficiency gains from KPI-driven reclassification of existing activity; comparison of firm-level R&D output against reported innovation-sector investment growth.',
    'If substantially genuine, this reading is closer to a tangled_rope with a strong coordination core; if substantially relabeling, effective extraction is understated and the structure is closer to a snare wearing coordination language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_transformation_vs_relabeling, empirical, 'Whether reported structural transformation reflects real productivity gains or reclassification theater.').

omega_variable(
    transition_support_adequacy,
    'Is the retraining, relocation, and fiscal-transfer support offered to displaced manufacturing workers and property-dependent local governments structurally adequate to the pace of capital reallocation, or is inadequacy a designed feature that lowers the cost of the transition to its beneficiaries?',
    'Comparison of transition-support budget allocations against displacement rates and fiscal-shortfall data in affected regions over the measurement interval.',
    'Adequate support would move this reading toward genuine coordination with manageable transitional cost; systematic underfunding would sharpen the tangled_rope classification toward snare-adjacent territory for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_support_adequacy, empirical, 'Whether underfunded transition support is incidental or structurally load-bearing for the extraction.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''high-quality development'' a coherent, independently specifiable reading of performance legitimacy, or is it better understood as a discretionary umbrella term whose content is set post hoc by whichever coalition currently controls the KPI-setting apparatus?',
    'Track whether the specific metrics counted as ''high quality'' remain stable across political cycles and leadership transitions, or shift opportunistically to match whatever sectors the current coalition favors.',
    'A stable, independently specifiable reading supports treating this as a genuine distinct kernel reading with its own ε; a discretionary umbrella would suggest the reading itself is downstream of raw power allocation rather than a freestanding legitimacy claim — this would not change the classification of this specific story, but would weaken confidence that the reading is a stable analytical unit across time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the qualitative-development reading is a stable kernel reading or a shifting discretionary label.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__qualitative_development_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__qualitative_development_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__qualitative_development_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__qualitative_development_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__qualitative_development_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__qualitative_development_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__qualitative_development_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__qualitative_development_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__qualitative_development_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__qualitative_development_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__qualitative_development_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__qualitative_development_reading, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__qualitative_development_reading, suppression_requirement, 4, 0.43).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__qualitative_development_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__qualitative_development_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__qualitative_development_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__qualitative_development_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__qualitative_development_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__qualitative_development_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the performance_legitimacy kernel. Each reading (qualitative_development, quantitative_growth, techno_nationalist, livelihood_security) is authored as a separate constraint with its own ε, beneficiary/victim structure, and claimed type, per the ε-invariance principle — they are not the same constraint viewed from different angles but structurally distinct legitimacy claims that compete for the same governing authority's justificatory resources. This reading structurally influences (and is influenced by) the others: resources committed to innovation-sector legitimacy are resources unavailable to quantitative-growth stimulus, techno-nationalist strategic-sector subsidy, or livelihood-security transfer programs, so a rise in this reading's dominance exerts downstream pressure on the others' feasibility and legitimacy-conferring power without logically foreclosing any of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
