% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__techno_nationalist_reading, []).

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
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Performance Legitimacy — Techno-Nationalist (Strategic Self-Sufficiency) Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This story instantiates the techno-nationalist reading of the
 *   performance-legitimacy kernel: the state's claim to legitimacy is
 *   grounded in achieving technological self-sufficiency and dominance in
 *   strategic industries, framed as a matter of national security and
 *   great-power status rather than economic efficiency or living-standard
 *   improvement. Under this reading, the primary constraint is
 *   strategic-sector dominance, not GDP growth (quantitative_growth_reading),
 *   not innovation-and-sustainability-weighted transformation
 *   (qualitative_development_reading), and not tangible daily-life
 *   improvements (livelihood_security_reading) — those are structurally
 *   distinct claims authored as sibling constraints. The techno-nationalist
 *   reading directs massive capital toward designated sectors regardless of
 *   market signals or commercial return, uses export controls and
 *   supply-chain-resilience posture as evidence of the program's necessity,
 *   and treats foreign retaliation as vindication rather than as a cost to be
 *   weighed. The coordination function (solving a genuine chokepoint-exposure
 *   problem) is real but is now substantially intertwined with rent capture
 *   by entrenched national champions and self-referential agenda-setting by
 *   the very state-owned firms that benefit.
 *
 * KEY AGENTS:
 *   - party_industrial_planning_apparatus: agenda-setter (institutional/analytical) — designates strategic sectors, directs credit
 *   - national_champion_firms: primary beneficiary (organized/arbitrage) — receives subsidized capital, protection from competition
 *   - defense_adjacent_tech_sector: primary beneficiary (powerful/arbitrage) — center of the legitimacy narrative, shielded from efficiency scrutiny
 *   - consumer_goods_sector, private_sme_exporters, urban_service_sector_workers, household_savers_funding_directed_credit: payers — lose capital access, absorb financial repression and export-control blowback
 *   - independent_economists_and_multilateral_bodies: analytical observer — documents misallocation but lacks enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.62).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Performance Legitimacy — Techno-Nationalist (Strategic Self-Sufficiency) Reading").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, 'e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43').
narrative_ontology:cs_kernel_codification('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', distributed).
narrative_ontology:cs_authority_grounding('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', extraction).
narrative_ontology:cs_interpretation_layer_present('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43').
narrative_ontology:cs_reading_relation('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', foundational, strategic_self_sufficiency_supersedes_market_efficiency).
narrative_ontology:cs_axiom_status(strategic_self_sufficiency_supersedes_market_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', strategic_self_sufficiency_supersedes_market_efficiency, instrumental).
narrative_ontology:cs_axiom('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', foundational, great_power_status_is_a_legitimate_state_end_in_itself).
narrative_ontology:cs_axiom_status(great_power_status_is_a_legitimate_state_end_in_itself, holdable).
narrative_ontology:cs_axiom_grounding('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', great_power_status_is_a_legitimate_state_end_in_itself, conventional).
narrative_ontology:cs_reference_frame('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', reform_era_growth_primacy_consensus).
narrative_ontology:cs_drift_state('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', post_export_control_escalation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e1b6beaf-7173-46a8-aaf4-66a8fdbc0f43', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champion_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sector).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, party_industrial_planning_apparatus).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_owned_semiconductor_and_materials_firms).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_goods_sector).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, private_sme_exporters).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, urban_service_sector_workers).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, household_savers_funding_directed_credit).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, great_power_status_requires_technological_sovereignty).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, market_allocation_is_insufficient_for_strategic_security).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets sectoral priority lists (semiconductors, AI, aerospace, advanced materials), directs state bank credit, procurement quotas, and industrial policy funds toward designated champions, and evaluates cadre performance partly on strategic-sector milestones. Frames the entire program as existential — national security and sovereignty depend on it — which makes deviation from the sector list difficult to argue against internally.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, party_industrial_planning_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Receive concessional credit, guaranteed procurement, subsidized land and energy, and protection from foreign competition in exchange for pursuing designated technology targets. Can raise capital on favorable terms regardless of commercial viability because failure is framed as a security risk rather than a market outcome; can lobby for continued designation even after missing targets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champion_firms, beneficiary,
    organized, generational, arbitrage, national).

% Positioned at the center of the legitimacy narrative; receives outsized R&D funding, talent-recruitment subsidies, and shelter from antitrust or efficiency scrutiny because its output is coded as strategic rather than commercial. Its continued designation as 'strategic' is largely self-reinforcing once embedded in five-year plans.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sector, beneficiary,
    powerful, civilizational, arbitrage, national).

% Competes for capital, land, and skilled labor against subsidized strategic sectors and consistently loses the allocation contest; faces tighter credit conditions and lower policy attention despite generating more employment per unit of capital. Cannot easily relocate out of the domestic market and has no comparable lobbying access to the planning apparatus.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_goods_sector, payer,
    moderate, biographical, constrained, national).

% Depend on export markets that are destabilized by the same export-control and supply-chain-resilience posture that legitimizes the strategic-sector program; absorb retaliatory tariffs and market access restrictions triggered by great-power competition dynamics they did not create and cannot influence. Exit means relocating production abroad, which the state actively discourages.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, private_sme_exporters, payer,
    powerless, biographical, trapped, regional).

% Work in sectors (retail, hospitality, gig platforms) that receive minimal policy attention and no directed investment because they carry no strategic-sector prestige; bear the employment volatility when consumption is suppressed to fund investment-led strategic programs. Have little organized voice in industrial policy debates.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, urban_service_sector_workers, payer,
    powerless, biographical, constrained, local).

% Provide the deposit base that state banks channel into subsidized strategic-sector lending at below-market returns; face financial repression (capped deposit rates, limited alternative investment channels) that functions as an implicit tax funding the technology programs. Capital controls make cross-border exit largely unavailable.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, household_savers_funding_directed_credit, payer,
    powerless, generational, trapped, national).

% Simultaneously implement and help define strategic-sector targets through embedded advisory roles in planning bodies; receive both funding and a say in which technologies count as strategic, creating a self-referential loop between beneficiary status and agenda-setting power.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_owned_semiconductor_and_materials_firms, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, state_owned_semiconductor_and_materials_firms, agenda_setter).

% React to the self-sufficiency drive with reciprocal export controls, sanctions, and reshoring policies but have no voice inside the domestic legitimacy calculus; their countermeasures are treated by the planning apparatus as further proof that the strategic program is necessary, reinforcing rather than testing the underlying premise.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, foreign_governments_and_rival_blocs, excluded,
    institutional, generational, analytical, global).

% Track capital misallocation, overcapacity in designated sectors, and rate-of-return divergence between strategic and non-strategic industries; publish findings on efficiency costs but have no enforcement power over domestic industrial policy and are frequently dismissed as reflecting a rival development philosophy rather than neutral measurement.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, independent_economists_and_multilateral_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, national_champion_firms).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scarce capital, talent, and policy attention on a narrow set of technologies deemed critical to national security, potentially solving genuine coordination failures in sectors with high fixed costs, long payback horizons, and geopolitical externalities that private markets underprovide (e.g., leading-edge semiconductor fabrication, aerospace).
% TRANSFER_FUNCTION: Moves capital from savers and non-strategic sectors (via financial repression, tax incentives, and preferential credit allocation) to designated national-champion firms and defense-adjacent technology enterprises, and moves policy attention and land/energy access away from consumer-facing industries toward strategic ones.
% ABSENT_VOICES: Consumer-sector firms, SME exporters, and ordinary savers have no seat in the strategic-sector designation process; foreign governments whose countermeasures are treated as vindication rather than feedback are structurally excluded from any deliberation that could recalibrate the program.
% DISAPPEARANCE_RATIONALE: If the techno-nationalist legitimacy frame were withdrawn, directed credit to designated sectors would likely be redirected toward market-return-weighted allocation, financial repression on household deposits would face pressure to ease, consumer and export-oriented SME sectors would gain relative access to capital and land, and the current national-champion firms most dependent on non-commercial support would face restructuring or contraction.
% FOUNDING_PROBLEM: Perceived exposure to foreign chokepoints in critical technologies (advanced chips, aerospace components, industrial software) that could be weaponized during geopolitical conflict, combined with a belief that market-driven allocation alone would not produce adequate strategic depth or speed of catch-up in frontier sectors.
% FOUNDING_PROBLEM_CORROBORATION: The planning apparatus and defense-adjacent firms attest the chokepoint-exposure problem remains acute and worsening, citing tightening foreign export controls as evidence. Independent economists and multilateral bodies attest that after a decade of directed investment, overcapacity and capital misallocation in several designated sectors now exceed the residual security gap, and that some 'strategic' designations serve entrenched incumbents rather than addressing new exposure — this corroboration comes from analysts outside the beneficiary set, though it is contested by the planning apparatus itself.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the substantial and growing gap between capital allocated to designated strategic sectors and the risk-adjusted commercial return those sectors generate, financed by financial repression on savers and reduced capital access for consumer/export sectors. Suppression (0.62) is high because the security framing forecloses internal debate — questioning strategic-sector allocation can be coded as insufficiently patriotic or as underestimating security threats, and capital controls trap savers in the financing arrangement. Theater ratio (0.31) is moderate: much of the investment produces real technological capacity, but a rising share of activity (target announcements, symbolic 'breakthrough' declarations, overlapping duplicate fabs across provinces) increasingly serves signaling rather than genuine capability-building, consistent with the rising trajectory across the measurement grid.
 *
 * DIRECTIONALITY LOGIC:
 *   National champion firms and the defense-adjacent tech sector sit near the full-beneficiary end: they receive the transfer, face minimal accountability for missed targets (failure is recoded as evidence of the threat's severity, not of policy error), and can exit into arbitrage (accessing both preferential domestic financing and, increasingly, dominant global market positions). The planning apparatus itself is agenda-setter with analytical/civilizational time horizon — it does not directly extract rents but administers and legitimizes the transfer. Consumer-sector firms, SME exporters, service workers, and savers sit near the full-target end: they bear the financing cost (financial repression, capital misallocation, reduced policy attention) without commensurate voice or exit. State-owned semiconductor/materials firms occupy an unusual dual position — beneficiary AND partial agenda-setter — because their embedded advisory role in planning bodies lets them help define which technologies count as 'strategic,' a self-reinforcing loop the override below documents is likely understated by pure structural derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (chokepoint exposure in critical technologies) was genuinely live at the program's outset and provides real coordination value the classification must not erase — hence tangled_rope rather than snare. But the founding_problem_status is authored 'contested' rather than 'live' or 'dead' because credible corroboration from outside the beneficiary set (independent economists, multilateral bodies) now documents overcapacity and misallocation in several designated sectors exceeding the residual security gap, while the planning apparatus and its direct beneficiaries continue to assert undiminished urgency. This is the diagnostic case tangled_rope exists for: a genuine coordination function persists alongside a growing extraction component riding on the same structure, and the two cannot be cleanly separated without independent verification the current institutional design does not readily permit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chokepoint_exposure_residual_magnitude,
    'How much genuine strategic chokepoint exposure remains after a decade of directed investment, versus how much of the continued ''strategic sector'' designation now serves incumbent rent protection rather than addressing residual security gaps?',
    'Independent technical audit of actual supply-chain dependency reduction achieved per designated sector, compared against capital deployed and against counterfactual dependency reduction achievable via diversified sourcing or allied co-production agreements.',
    'If residual exposure is still large, the tangled_rope classification is well-calibrated (real coordination function persists alongside extraction); if residual exposure has been substantially closed, the constraint has drifted toward snare, with ''national security'' functioning primarily as a shield for incumbent capital capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chokepoint_exposure_residual_magnitude, empirical, 'Whether the founding security problem remains substantially live or has been overtaken by rent-seeking.').

omega_variable(
    self_referential_designation_loop,
    'Does embedding state-owned beneficiary firms in the strategic-sector designation process (advisory roles in planning bodies) constitute a structural capture mechanism, or a legitimate expertise-sourcing arrangement given that these firms hold the deepest technical knowledge of the sector?',
    'Compare designation outcomes and target-revision patterns in sectors with embedded-firm advisory input against sectors evaluated by arms-length technical panels; look for systematic bias toward continued/expanded designation in the former.',
    'If designation outcomes diverge systematically toward the interests of embedded advisory firms, this strengthens the case for extraction dominance in the tangled_rope reading and would support a directionality override raising these firms'' effective d further toward pure beneficiary status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_referential_designation_loop, conceptual, 'Whether the beneficiary/agenda-setter dual role in state-owned strategic firms constitutes capture or legitimate expertise input.').

omega_variable(
    kernel_reading_incommensurability,
    'Is the techno-nationalist reading genuinely a distinct legitimacy claim from the quantitative-growth reading, or is it better understood as an instrumental sub-claim nested inside growth legitimacy (i.e., strategic dominance is pursued because it is believed to secure long-run growth)?',
    'Examine cases where strategic-sector investment and near-term GDP-maximizing investment diverge (as in current overcapacity episodes) and observe which claim the planning apparatus defers to when forced to choose — persistent prioritization of strategic sectors despite growth costs would confirm the readings are structurally distinct kernels, not nested.',
    'If techno-nationalist legitimacy is shown to persist even against growth costs, the two-kernel-reading decomposition is vindicated and each story''s independent ε remains justified; if the planning apparatus reliably folds strategic goals back into growth-maximization once costs mount, the readings may need to be modeled as one constraint with a dominant/subordinate structure rather than fully independent siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the techno-nationalist and quantitative-growth readings are genuinely independent kernel readings or one nests inside the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__techno_nationalist_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__techno_nationalist_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__techno_nationalist_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__techno_nationalist_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__techno_nationalist_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__techno_nationalist_reading, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__techno_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language 'performance legitimacy' kernel per the ε-invariance principle. Each reading (techno-nationalist, quantitative-growth, qualitative-development, livelihood-security) authors a distinct ε, beneficiary/victim structure, and classification because the underlying legitimacy claim being evaluated differs structurally in each case — they are not the same constraint viewed from four angles. The techno-nationalist reading is authored with the highest suppression score among the four siblings (national-security framing forecloses internal debate) and links to all three siblings because directed strategic investment structurally competes with them for the same finite capital and policy-attention budget: it draws capital away from growth-maximizing consumption/SME investment (quantitative_growth_reading), from efficiency/sustainability-oriented restructuring (qualitative_development_reading), and from direct welfare/livelihood spending (livelihood_security_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
