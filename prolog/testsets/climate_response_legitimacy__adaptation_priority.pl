% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Climate Response: Adaptation-Priority Legitimacy Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   Under the adaptation-priority reading of climate-response legitimacy,
 *   wealthy developed economies and climate-finance institutions have framed
 *   'pragmatic acceptance of inevitable warming' as the appropriate policy
 *   posture, prioritizing resilience infrastructure and technology transfer
 *   to vulnerable regions over aggressive emissions reduction. The reading
 *   presents the choice as realistic, cost-effective, and compassionate —
 *   adaptation prevents catastrophic impacts now while waiting for technology
 *   to enable painless mitigation. However, this reading carries high
 *   extractiveness: it defers intergenerational mitigation costs (compounded
 *   nonlinearly by higher warming), locks vulnerable regions into
 *   technology-dependent adaptation rather than structural transformation,
 *   and preserves the development model of wealthy economies that generated
 *   the crisis. The constraint is simultaneously a coordination mechanism
 *   (mobilizing adaptation finance) and an extraction mechanism (capturing
 *   the policy frame to avoid mitigation costs and structural economic
 *   change).
 *
 * KEY AGENTS:
 *   - Wealthy developed economies (agenda_setter, institutional power) — set the adaptation-priority frame via UNFCCC, COP agreements, IPCC adaptation committees; preserve growth models; control climate finance conditions
 *   - Low-income vulnerable regions (payer, powerless) — bear immediate concentrated impacts; trapped in adaptation-dependency; $350B annual finance gap; cannot exit climate system or afford unilateral mitigation
 *   - Adaptation finance intermediaries (agenda_setter/beneficiary, institutional) — manage World Bank, IMF, bilateral flows; enforce the reading via policy conditions; benefit from expanding adaptation-finance asset bases
 *   - Technology exporters (beneficiary, powerful) — provide adaptation technologies (seeds, defenses, water, energy); capture licensing and IP rents; reading legitimizes endless technology purchasing
 *   - Future generations post-2080 (payer, powerless, civilizational time horizon) — inherit compounded warming and deferred mitigation costs; trapped; cannot participate in present negotiations
 *   - Climate scientists (observer, analytical) — document that adaptation-without-mitigation compounds future costs nonlinearly; model cost trajectories showing mitigation-now is cheaper; attest to tipping-point risks
 *   - Mitigation-priority and degrowth advocates (excluded, moderate power) — institutionally gatekept out of IPCC/COP processes; would argue for rapid emissions cessation and structural transformation
 *   - Indigenous and traditional knowledge systems (excluded, moderate power, identity-locked) — possess adaptive practice, excluded from adaptation planning; trapped to place even when adaptation fails
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.71).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Climate Response: Adaptation-Priority Legitimacy Reading").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '2d3a4938-6bb0-411d-a4bb-5bd203433ad2').
narrative_ontology:cs_kernel_codification('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', distributed).
narrative_ontology:cs_authority_grounding('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', extraction).
narrative_ontology:cs_interpretation_layer_present('2d3a4938-6bb0-411d-a4bb-5bd203433ad2').
narrative_ontology:cs_reading_relation('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', foundational, adaptation_sufficiency_hypothesis).
narrative_ontology:cs_axiom_status(adaptation_sufficiency_hypothesis, holdable).
narrative_ontology:cs_axiom_grounding('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', adaptation_sufficiency_hypothesis, empirically_contingent).
narrative_ontology:cs_axiom('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', foundational, wealthy_economy_development_preservation).
narrative_ontology:cs_axiom_status(wealthy_economy_development_preservation, holdable).
narrative_ontology:cs_axiom_grounding('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', wealthy_economy_development_preservation, conventional).
narrative_ontology:cs_reference_frame('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', post_2015_paris_agreement_adaptation_mainstreaming).
narrative_ontology:cs_drift_state('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', contemporary_2024_high_impact_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2d3a4938-6bb0-411d-a4bb-5bd203433ad2', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_developed_economies).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, adaptation_finance_intermediaries).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_vulnerable_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations_2100_onward).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the reading transfers costs to vulnerable regions ($350B adaptation gap) and future generations (cumulative warming damage rises nonlinearly with every decade of deferred mitigation). The trajectory shows extractiveness rising from 0.45 to 0.68 as the reading becomes institutionally entrenched — the initial period (0–10) shows the reading gaining policy authority despite pushback; the middle period (10–25) shows extractiveness stabilizing as alternative readings are suppressed; the end (25–40) shows extractiveness plateauing at high level because the intergen­erational costs compound outside the policy window. Suppression is high (0.71 at interval end) because the reading requires continuous institutional gatekeeping: mitigation-priority and degrowth readings must be kept from policy voice, indigenous knowledge must be overwritten by technology narratives, and vulnerable-region skepticism must be managed through finance conditions and technology-lock-in. Theater rises early (0.28–0.41 from 0–20) as adaptation finance becomes performative (pledges unmet, projects delayed, but framing maintained), then plateaus (0.41 from 20–40) at moderate-high level — enough theater that the underlying extraction is visible to experts but obscured for general publics by the humanitarian adaptation narrative. Accessibility collapse is moderate (0.52) because alternatives (mitigation-priority, degrowth transformation) remain live in academic and activist discourse, even if institutionally suppressed — the reading does not have monopoly epistemic authority. Resistance is high (0.73) from scientists, climate-justice movements, vulnerable-region governments, and degrowth advocates challenging the reading's cost assumptions and inequality structure. The measurement series share one time grid (t=0,5,10,15,20,25,30,40) so every metric is authored at every shared point.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the reading's declared legitimacy (pragmatic, protective of vulnerable populations, cost-effective) and its structural operation (extraction from vulnerable regions and future generations, preservation of wealthy-economy growth models, suppression of systemic-change alternatives) is the core analytical content. Agenda-setter and intermediate-beneficiary seats should compute as rope or scaffold from their position (they experience coordination and transition); target seats should compute as snare or tangled rope (they experience extraction masked as protection). The engine's per-seat classification reveals this divergence; the authored metrics and beneficiary/victim declarations establish the structural asymmetry the computation reads.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: (1) Wealthy developed economies are beneficiaries (collects deferred costs, control frame, preserve development model) + institutional power + arbitrage exit (can shift to mitigation if political pressure rises) → low d, near beneficiary end (~0.15–0.25). (2) Low-income vulnerable regions are victims (pay immediate costs, trapped adaptation-dependency, powerless) + powerless power + trapped exit (cannot leave climate system, cannot fund unilateral mitigation) → high d, near target end (~0.85–0.95). (3) Adaptation finance intermediaries are beneficiaries (expanding asset bases, policy authority) + institutional power + constrained exit (locked into the reading's institutional success) → low-moderate d (~0.30–0.40). (4) Technology exporters are beneficiaries + powerful power + mobile exit (can sell to any warming-adaptation scenario) → low d (~0.20–0.30). (5) Future generations are victims (inherit compounded costs) + powerless + trapped exit → very high d (~0.95). (6) Scientists and advocates are observers (analytical power, analytical exit) → d=0.5 by symmetry (neither collecting nor paying; measuring). No directionality overrides are needed — the structural derivation from beneficiary/victim + power + exit produces coherent d assignments across all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The adaptation-priority reading's founding problem — protecting vulnerable populations from inevitable climate impacts — is live and urgent (evidenced by $350B annual adaptation finance gap, observable impacts 2020–2024, vulnerable-region climate vulnerability indices). However, the reading's mandate has been stretched to encompass a secondary function: providing cover for wealthy-economy avoidance of rapid mitigation costs. The primary mandate (protecting vulnerable populations) could be achieved through any climate-response reading; the reading's persistence is partly due to the secondary extraction function (deferring mitigation costs). A mandatrophy signal would trigger if the reading begins to fail on its primary function despite institutional entrenchment — e.g., adaptation finance continues underfunded while the reading remains policy orthodoxy, or adaptation infrastructure proves insufficient at higher warming levels, or vulnerable regions reject the reading explicitly. The reading is not yet mandatrophic (the primary function is still live, though underfunded), but it is at risk of mandatrophy if the nonlinear cost escalation (omega 1) and adaptation saturation (omega 2) materialize as projected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_deferral_cost_nonlinearity,
    'What is the actual cumulative cost difference between pursuing mitigation now vs. adaptation-only to higher warming levels? Does the nonlinear tipping-point acceleration documented by climate models materialize in economic terms?',
    'Integrated assessment models (IAMs) comparing mitigation-now pathways against adaptation-only pathways through 2100, with explicit tipping-point probability weighting and post-tipping impact costs. Cross-validation against observable climate response patterns (Amazon moisture feedback, ice-sheet acceleration, permafrost release) as the interval progresses.',
    'If adaptation-only cumulative cost substantially exceeds mitigation-now cost, the reading''s economic justification (''adaptation is more cost-effective'') collapses; if costs are genuinely lower under adaptation-only, the reading''s framing is vindicated. The high-sensitivity case reshuffles the victim set to include wealthy economies via intergenerational extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_deferral_cost_nonlinearity, empirical, 'Cost trajectory comparison: mitigation-now vs. adaptation-to-higher-warming through tipping points').

omega_variable(
    adaptation_technology_sufficiency_boundary,
    'At what warming level does adaptation technology saturate and can no longer absorb impacts? Is there a threshold (e.g., 2.5°C, 3.0°C) above which adaptation becomes physically or economically impossible?',
    'Physical climate modeling combined with engineering feasibility studies: can coastal defenses hold at 2m sea-level rise? Can irrigation replace rainfall at continental scale? Can permafrost thaw be arrested? Observable evidence from the 2020–2040 decade as warming accelerates.',
    'If a hard saturation threshold exists below 4°C, the reading''s assumption of ''unlimited adaptive capacity'' is false, and the constraint becomes a snare (promising protection that is structurally impossible). If no threshold exists, the reading''s technical optimism is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_technology_sufficiency_boundary, empirical, 'Physical and economic saturation of adaptation capacity at high warming levels').

omega_variable(
    kernel_reading_alternative_framing_contest,
    'Is the climate response kernel fundamentally about choosing between mitigation-priority, adaptation-priority, and degrowth-transformation readings, or does the reading choice depend on unstated empirical claims about cost, feasibility, or equity that could be resolved by data?',
    'The kernel contest itself is a preference/normative disagreement, but it rides on three empirical sub-claims: (1) mitigation technology cost trajectory (decoupling possible?), (2) adaptation capacity nonlinearity (cost multiplier as warming rises?), and (3) degrowth feasibility (can democracies sustain reduced consumption?). Resolve the empirical sub-questions; the preference-level disagreement persists independently.',
    'This omega routes the committer-frame structure through the existing apparatus: the three readings coexist at the preference level, but they are not equivalent in empirical coherence. The adaptation-priority reading rests on high empirical confidence in technology sufficiency (omega 2, low-confidence) and low-warming-cost assumptions (omega 1, medium-confidence). If either fails, the reading''s epistemic foundation weakens relative to siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing_contest, conceptual, 'Kernel readings as preference-plus-empirical-claim bundles; sibling reading coherence depends on resolution of empirical sub-questions').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of mitigation-priority and degrowth-transformation readings structural (gatekeeping by wealthy-economy institutions) or internalized (vulnerable regions have come to accept adaptation-only as realistic/inevitable)? Or both?',
    'Post-suppression trajectory: if mitigation and degrowth advocates gain institutional voice (policy seats, funding access, UNFCCC standing), do vulnerable regions shift advocacy positions, or do they continue accepting adaptation-framing even when alternatives are present? Survey preference data before and after institutional opening.',
    'If suppression is purely structural, removing it should shift policy consensus rapidly. If suppression is internalized (deprivation narratives, learned helplessness, identity-fusion to the adapted role), the constraint persists even after structural barriers fall. If both, fixing requires both institutional change and re-capacity-building.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression of alternative climate response readings').

omega_variable(
    beneficiary_identity_wealthy_economies_vs_adaptation_finance_capture,
    'Do wealthy developed economies genuinely benefit from adaptation-priority (by deferring their own emissions reduction costs), or does the real beneficiary seat shift to adaptation finance intermediaries and technology exporters, with wealthy economies caught in a legitimacy-maintenance cost?',
    'Trace the actual monetary flows of climate adaptation finance: who receives contracts, licensing fees, asset fees? Compare to GHG reduction commitments wealthy economies undertake under the reading vs. under mitigation-priority. If wealthy economies spend heavily on adaptation finance while evading emissions cuts, they benefit; if they spend heavily AND reduce emissions, they are partly targets bearing the cost of the reading''s legitimacy.',
    'If beneficiary is actually the intermediate class (finance institutions, technology exporters) and wealthy economies are partly targets bearing legitimacy costs, the reading''s beneficiary set should be adjusted. The extraction pattern would be more complex — some wealthy-economy institutional actors benefit while others pay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_wealthy_economies_vs_adaptation_finance_capture, empirical, 'Actual beneficiary identification via cash flow and policy-cost analysis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t5, climate_response_legitimacy__adaptation_priority, theater_ratio, 5, 0.32).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__adaptation_priority, theater_ratio, 10, 0.36).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__adaptation_priority, theater_ratio, 15, 0.39).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__adaptation_priority, theater_ratio, 20, 0.41).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__adaptation_priority, theater_ratio, 25, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__adaptation_priority, theater_ratio, 30, 0.41).
narrative_ontology:measurement(clim_tr_t40, climate_response_legitimacy__adaptation_priority, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t5, climate_response_legitimacy__adaptation_priority, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__adaptation_priority, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__adaptation_priority, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__adaptation_priority, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__adaptation_priority, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__adaptation_priority, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(clim_be_t40, climate_response_legitimacy__adaptation_priority, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clim_su_t5, climate_response_legitimacy__adaptation_priority, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__adaptation_priority, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__adaptation_priority, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__adaptation_priority, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__adaptation_priority, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__adaptation_priority, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(clim_su_t40, climate_response_legitimacy__adaptation_priority, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% The climate-response-legitimacy kernel decomposes into three constraint stories, each instantiating a different reading: adaptation_priority (this file) emphasizes inevitable warming + technology adaptation; mitigation_priority emphasizes decoupling + economic growth preservation; degrowth_transformation emphasizes structural economic change + consumption reduction. The three readings coexist as live policy positions held by different institutional and activist coalitions. Each story carries its own ε, beneficiary/victim structure, and type — they are not measurements of the same constraint under different frames, but three distinct structural instantiations of the contested kernel. Links between the stories are routed through cs_structure.reading_relations in each file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
