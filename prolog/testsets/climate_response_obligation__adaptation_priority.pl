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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Climate Response Obligation: Adaptation-Priority Reading
 *   domain: environmental/political/economic
 *
 * SUMMARY:
 *   The adaptation-priority reading of the climate response obligation kernel
 *   frames 2-3°C warming as inevitable given coordination failures in
 *   decarbonization, and redirects policy toward resilience investment and
 *   harm reduction. This reading benefits incumbent fossil capital and the
 *   current generation's wealthy constituencies by deferring transition costs
 *   and stranded-asset risks; it imposes extraction on future generations and
 *   the Global South, who inherit a warmer baseline and inadequate adaptive
 *   capacity. The reading is sustained by suppression of alternative framings
 *   (the inevitability narrative crowds out prevention-focused policy) and by
 *   theater (adaptation spending is presented as the responsible alternative
 *   to failed prevention, obscuring that prevention was politically
 *   suppressed, not technically infeasible). The measurement series traces
 *   the extraction trajectory: extraction climbs from 0.35 (1990, when
 *   prevention was still narratively possible) to 0.81 (2050 projected, when
 *   the cumulative effect of deferred prevention is locked in). Theater ratio
 *   rises as adaptation becomes the dominant policy surface while the actual
 *   function shifts from prevention to risk-protection-for-the-wealthy.
 *
 * KEY AGENTS:
 *   - fossil_fuel_capital: Primary beneficiary (institutional power) — avoids transition costs and asset writedowns
 *   - current_generation_wealthy: Agenda-setter and beneficiary (institutional power) — controls policy framing, defers costs
 *   - adaptation_infrastructure_vendors: Beneficiary (institutional power) — captures perpetual investment flows
 *   - future_generations: Payer (powerless) — trapped bearers of impact and cumulative warming
 *   - global_south_populations: Payer (powerless, constrained exit) — highest climate burden, lowest prevention investment
 *   - climate_vulnerable_communities: Payer (organized, constrained) — already experiencing impacts, displaced by adaptation
 *   - mitigation_advocates: Excluded (organized, constrained) — evidence-based alternative framings suppressed
 *   - climate_science_consensus: Observer (analytical) — provides evidence that prevention is more cost-effective at every warming level
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.78).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.71).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Climate Response Obligation: Adaptation-Priority Reading").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "environmental/political/economic").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, 'c8e244d5-f5db-4013-9a12-c831ef4f70be').
narrative_ontology:cs_kernel_codification('c8e244d5-f5db-4013-9a12-c831ef4f70be', distributed).
narrative_ontology:cs_authority_grounding('c8e244d5-f5db-4013-9a12-c831ef4f70be', extraction).
narrative_ontology:cs_reading_relation('c8e244d5-f5db-4013-9a12-c831ef4f70be', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('c8e244d5-f5db-4013-9a12-c831ef4f70be', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('c8e244d5-f5db-4013-9a12-c831ef4f70be', foundational, economic_decarbonization_infeasible).
narrative_ontology:cs_axiom_status(economic_decarbonization_infeasible, holdable).
narrative_ontology:cs_axiom_grounding('c8e244d5-f5db-4013-9a12-c831ef4f70be', economic_decarbonization_infeasible, empirically_contingent).
narrative_ontology:cs_axiom('c8e244d5-f5db-4013-9a12-c831ef4f70be', foundational, adaptation_sufficient_for_2_3_degrees).
narrative_ontology:cs_axiom_status(adaptation_sufficient_for_2_3_degrees, holdable).
narrative_ontology:cs_axiom_grounding('c8e244d5-f5db-4013-9a12-c831ef4f70be', adaptation_sufficient_for_2_3_degrees, empirically_contingent).
narrative_ontology:cs_axiom('c8e244d5-f5db-4013-9a12-c831ef4f70be', secondary, intergenerational_risk_transfer_acceptable).
narrative_ontology:cs_axiom_status(intergenerational_risk_transfer_acceptable, holdable).
narrative_ontology:cs_axiom_grounding('c8e244d5-f5db-4013-9a12-c831ef4f70be', intergenerational_risk_transfer_acceptable, deontological).
narrative_ontology:cs_reference_frame('c8e244d5-f5db-4013-9a12-c831ef4f70be', inevitable_warming_pragmatic_adaptation).
narrative_ontology:cs_drift_state('c8e244d5-f5db-4013-9a12-c831ef4f70be', contemporary_2024_empirical_record, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8e244d5-f5db-4013-9a12-c831ef4f70be', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_fuel_capital).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_wealthy).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, adaptation_infrastructure_vendors).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_populations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, climate_vulnerable_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).

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
 *   Extractiveness climbs over 60 years because the adaptation-priority framing delays decarbonization, accumulating a larger climate debt that adaptation cannot fully offset. The constraint's asymmetry is captured in the dual beneficiary/victim structure: those who benefit from fossil-dependent wealth today (fossil capital, current wealthy) are shielded by the framing; those who pay are those without power to exit (future generations are trapped by time; Global South is trapped by resource constraints and debt obligations). Suppression is high (0.71) because the reading requires active maintenance: the 'inevitability' of warming must be continuously reasserted against evidence that prevention is technically and economically feasible; mitigation advocates must be excluded or discredited; alternative policy framings must be blocked. Theater rises sharply (0.15 to 0.63) because adaptation spending becomes visible public policy while the extraction mechanism (deferred decarbonization protecting fossil capital) remains largely implicit. The coercion grid shows differentiated pressure: at the structural level, accessibility to alternative energy pathways collapses (infrastructure is locked into fossil-dependent systems); at the class level, stakes inflate dramatically for the Global South and vulnerable communities; at the individual level, both accessible exits and stakes are more compressed (individuals cannot unilaterally decarbonize or prevent displacement). Resistance declines over time as the framing normalizes and organizational capacity to contest it weakens.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of fossil capital and current-generation policymakers, adaptation-priority is pragmatic recognition of coordination failure—prevention is framed as politically impossible (a description of current constraints treated as permanent facts). From the seat of future generations and the Global South, the same framing is revealed as enforced extraction: the coordination failure is itself produced by incumbent capital preventing alternatives, and the 'inevitability' narrative functions as suppression. The engine computes the divergence per seat from directionality (beneficiary vs. victim) and exit options (arbitrage-mobile vs. trapped); the authored metrics describe the asymmetry's operational structure (high suppression, rising theater) that sustains the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel capital and current-generation wealthy have low directionality (d ≈ 0.15–0.25): they benefit from the constraint without bearing its costs; they have exit options (arbitrage—they can invest in adaptation infrastructure or diversify without penalty). Future generations have maximum directionality (d ≈ 0.95): they are trapped in time, inherit the constraint without choice, and cannot exit. The Global South has high directionality (d ≈ 0.85): constrained exit by debt and resource limits, maximum climate impact exposure, no policy influence. Adaptation infrastructure vendors have low-to-moderate directionality (d ≈ 0.30): they benefit (capture investment flows) and can exit if markets shift (arbitrage-capable). Mitigation advocates have neutral-to-high directionality (d ≈ 0.55): excluded from power (cannot shape the constraint directly), bearing the cost of suppressed evidence and delegitimized expertise. The constraint's effective extraction (χ) is high and spatially scaled: concentrated on powerless, globally scoped actors (future generations, Global South) with no exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—coordination failure in decarbonization—remains live and contested, but the mandatrophy signal is strong: the disappearance verdict is 'world_rearranges' (the constraint is actively maintained by policy choice, not natural necessity) yet the current generation's principal agenda-setters have chosen adaptation as the primary commitment. This combination flags extraction: the constraint persists because it benefits identifiable current agents (fossil capital, wealthy-nation policymakers) at the cost of future agents (future generations, Global South) who cannot contest it. The theater ratio rising from 0.15 to 0.63 indicates the constraint's primary function has drifted from addressing the founding problem (coordination failure) toward defending incumbent interests (preventing stranded-asset writedowns, maintaining energy prices, protecting current-generation wealth). Mandatrophy is resolved: the constraint's original coordination purpose (solve climate response through rapid decarbonization) has atrophied; what remains is extraction of climate risk onto those without power to exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prevention_cost_empirics,
    'Is rapid global decarbonization economically infeasible, or is the infeasibility narrative a political construction sustained by incumbent capital preventing regulatory alternatives?',
    'Regulatory forcing experiment: jurisdictions that mandate rapid decarbonization (zero-carbon procurement, carbon price floors, industrial transformation support) and measure actual cost vs. predicted cost; decoupling from incumbent capital''s preference reveals whether ''infeasibility'' was objective or constructed.',
    'If empirically feasible, the adaptation-priority reading loses its core justification and reclassifies from tangled-rope (coordination + extraction) to snare (pure extraction disguised as pragmatism). If infeasible, the extraction narrative dissolves and the constraint approaches rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prevention_cost_empirics, empirical, 'Whether decarbonization is technically/economically infeasible or politically suppressed.').

omega_variable(
    adaptation_sufficiency_boundary,
    'Can adaptation alone prevent catastrophic impacts and maintain human flourishing at 2-3°C warming, or does adaptation become impossible beyond some threshold (ecosystem collapse, uninhabitability, mass displacement)?',
    'Climate impact modeling at 2, 2.5, and 3°C warming across adaptation scenarios; empirical tracking of adaptation failure rates in pilot regions (climate displacement, crop failure despite irrigation, infrastructure overwhelm).',
    'If adaptation fails at 2°C, the reading''s core proposition is false and prevention becomes mandatory, reclassifying the constraint as snare (extraction defended by false inevitability). If adaptation succeeds, the reading''s coordination framing gains credibility and extraction becomes negotiable (tangled-rope dynamics persist but the beneficiary/victim boundary becomes contestable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_sufficiency_boundary, empirical, 'Whether adaptation can substitute for prevention across the 2-3°C range or breaks down.').

omega_variable(
    intergenerational_suppression_mechanism,
    'Is the suppression of mitigation-priority voices (inevitability framing, ''feasibility'' gatekeeping, expert discounting) structural (institutional barriers to alternative policy adoption) or internalized (future-generation advocates have absorbed the adaptation-priority framing as inevitable)?',
    'Post-suppression trajectory: in jurisdictions where adaptation-priority framing is challenged or overridden (e.g., Green New Deal adoption, rapid public decarbonization mandate), does resistance to mitigation persist (structural) or does support emerge rapidly (internalized)? Qualitative evidence from oral history and policy reversals.',
    'If suppression is purely structural, lifting it (policy reversal, new electoral coalition) restores mitigation as live. If partially internalized, even policy change faces belief-resistance from those socialized into adaptation-priority inevitability. The effective extraction sustained by internalized suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_suppression_mechanism, empirical, 'Whether suppression of mitigation-priority is structural (external barriers) or internalized (belief adoption).').

omega_variable(
    reading_coexistence_test,
    'Is the adaptation-priority reading logically foreclosed by the mitigation-priority reading''s core premises, or can both remain live positions held by different parties?',
    'Formalize the core premises: mitigation asserts ''prevention is more cost-effective and just than adaptation''; adaptation asserts ''prevention is politically infeasible; adaptation is sufficient.'' If mitigation''s premise is empirically true (prevention is cheaper), does adaptation''s infeasibility claim remain coherent? A true premise and a false claim cannot coexist in the same framework; if one premise is false, the readings coexist across different epistemic commitments.',
    'If adaptation-priority is foreclosed (prevention is demonstrably cheaper and feasible), the constraint reclassifies from contested reading to false-summit snare. If both readings remain live (they differ on empirical feasibility claims neither has decisively proven), the coexistence relationship is correctly characterized and the constraint remains in contested-reading space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_test, conceptual, 'Whether adaptation-priority reading is logically foreclosed by mitigation''s truth or coexists as a live alternative.').

omega_variable(
    diffuse_adaptation_capture,
    'Where does the adaptation investment actually accrue? Is adaptation spending captured by wealthy-region vendors and private infrastructure, or does it flow to vulnerable-community resilience?',
    'Financial tracking of climate adaptation investment 2020-2050: proportion flowing to Global South vs. OECD, to public vs. private infrastructure, to community-controlled vs. technocratic projects. Post-hoc analysis of displacement rates, livelihood preservation, and wealth transfer.',
    'If adaptation investment concentrates in wealthy regions and private vendors (current pattern), the constraint''s gain_flow is ''adaptation_infrastructure_vendors'' and the extraction mechanism is confirmed: prevention is deferred while adaptation is privatized. If adaptation flows to vulnerable communities at scale, the constraint''s structure approaches coordination and the victim set shrinks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diffuse_adaptation_capture, empirical, 'Whether adaptation investment flows to vulnerable communities or concentrates in wealthy-region vendors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_response_obligation__adaptation_priority, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(clim_tr_t2005, climate_response_obligation__adaptation_priority, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(clim_tr_t2015, climate_response_obligation__adaptation_priority, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(clim_tr_t2025, climate_response_obligation__adaptation_priority, theater_ratio, 2025, 0.51).
narrative_ontology:measurement(clim_tr_t2035, climate_response_obligation__adaptation_priority, theater_ratio, 2035, 0.57).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__adaptation_priority, theater_ratio, 2050, 0.63).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_response_obligation__adaptation_priority, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(clim_be_t2005, climate_response_obligation__adaptation_priority, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(clim_be_t2015, climate_response_obligation__adaptation_priority, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(clim_be_t2025, climate_response_obligation__adaptation_priority, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement(clim_be_t2035, climate_response_obligation__adaptation_priority, base_extractiveness, 2035, 0.78).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__adaptation_priority, base_extractiveness, 2050, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_response_obligation__adaptation_priority, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(clim_su_t2005, climate_response_obligation__adaptation_priority, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(clim_su_t2015, climate_response_obligation__adaptation_priority, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(clim_su_t2025, climate_response_obligation__adaptation_priority, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement(clim_su_t2035, climate_response_obligation__adaptation_priority, suppression_requirement, 2035, 0.72).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__adaptation_priority, suppression_requirement, 2050, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_asset_protection).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, global_south_climate_finance_architecture).

% DUAL FORMULATION NOTE:
% The climate_response_obligation kernel has three structurally distinct readings: adaptation_priority (this story: accepting warming as inevitable, prioritizing resilience; ε=0.78), mitigation_priority (preventing warming through decarbonization; ε differs), and degrowth_reading (material sufficiency as the constraint; ε differs). All three readings address the same founding problem (what does humanity owe itself across time regarding climate) but instantiate different constraints because they differ in beneficiary/victim structure and policy mechanisms. Each reading has a distinct ε and a distinct set of beneficiaries and victims. The three stories are siblings in a constraint family linked by network.affects_constraints; they are not three measurements of one constraint. The adaptation-priority reading influences the other two by establishing the empirical and political frame (once adaptation is accepted as primary, mitigation becomes peripheral and degrowth seems radical). The three readings coexist in live policy discourse; none has yet foreclosed the others logically, though the adaptation-priority reading has captured institutional power in most wealthy nations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
