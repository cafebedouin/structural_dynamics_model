% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Adaptation Priority Climate Constraint
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint is the adaptation_priority reading of the contested
 *   climate_response_obligation kernel. It treats 2-3Â°C warming as
 *   politically and technically inevitable and prioritizes resilience
 *   investment over costly prevention. The reading coordinates
 *   current-generation wealthy-nation governments and fossil capital around
 *   adaptation finance and infrastructure while externalizing climate damages
 *   to future generations and Global South populations. It protects fossil
 *   capital from transition risk and concentrates adaptation investment in
 *   wealthy regions. The sibling readingsâmitigation_priority and
 *   degrowth_readingârepresent competing interpretations of the same
 *   kernel.
 *
 * KEY AGENTS:
 *   - fossil_capital: Primary beneficiary (powerful/global/arbitrage) â avoids stranded assets and decarbonization pressure
 *   - wealthy_nations_current_gen: Primary beneficiary (organized/global/mobile) â avoids transition costs and captures adaptation investment
 *   - future_generations: Primary target (powerless/universal/trapped) â bears locked-in climate impacts without consent or recourse
 *   - global_south_vulnerable: Primary target (powerless/global/trapped) â faces disproportionate impacts with limited adaptive capacity and marginal agenda influence
 *   - wealthy_nation_governments: Agenda setter (institutional/global/constrained) â administers the adaptation-priority framework and manages international climate finance
 *   - mitigation_advocates: Excluded voice (moderate/global/constrained) â argues for prevention but structurally marginalized in policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.72).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.68).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Adaptation Priority Climate Constraint").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, 'd11026e6-c97e-492e-afdd-069b1d5a185b').
narrative_ontology:cs_kernel_codification('d11026e6-c97e-492e-afdd-069b1d5a185b', distributed).
narrative_ontology:cs_authority_grounding('d11026e6-c97e-492e-afdd-069b1d5a185b', extraction).
narrative_ontology:cs_interpretation_layer_present('d11026e6-c97e-492e-afdd-069b1d5a185b').
narrative_ontology:cs_reading_relation('d11026e6-c97e-492e-afdd-069b1d5a185b', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('d11026e6-c97e-492e-afdd-069b1d5a185b', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('d11026e6-c97e-492e-afdd-069b1d5a185b', foundational, warming_inevitability_pragmatism).
narrative_ontology:cs_axiom_status(warming_inevitability_pragmatism, holdable).
narrative_ontology:cs_axiom_grounding('d11026e6-c97e-492e-afdd-069b1d5a185b', warming_inevitability_pragmatism, empirically_contingent).
narrative_ontology:cs_axiom('d11026e6-c97e-492e-afdd-069b1d5a185b', foundational, current_resilience_over_future_prevention).
narrative_ontology:cs_axiom_status(current_resilience_over_future_prevention, holdable).
narrative_ontology:cs_axiom_grounding('d11026e6-c97e-492e-afdd-069b1d5a185b', current_resilience_over_future_prevention, instrumental).
narrative_ontology:cs_reference_frame('d11026e6-c97e-492e-afdd-069b1d5a185b', growth_preserving_adaptation).
narrative_ontology:cs_drift_state('d11026e6-c97e-492e-afdd-069b1d5a185b', post_2023_record_warming_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d11026e6-c97e-492e-afdd-069b1d5a185b', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_capital).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, wealthy_nations_current_gen).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_vulnerable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains asset value and revenue streams under a policy framework that avoids rapid decarbonization; the inevitability framing of 2-3Â°C warming removes political pressure for stranded assets and transition risk.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_capital, beneficiary,
    powerful, biographical, arbitrage, global).

% Avoids immediate economic restructuring and consumption constraints; benefits from adaptation infrastructure investment concentrated in wealthy regions while deferring mitigation costs to future budgets and foreign populations.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_nations_current_gen, beneficiary,
    organized, biographical, mobile, global).

% Excluded from present policy decisions but locked into inherited climate impacts; bears the full differential harm between a mitigated trajectory and the 2-3Â°C adaptation-priority pathway with no recourse or voice.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Faces disproportionate climate impacts with limited adaptive capacity; nominally included in UNFCCC forums but structurally sidelined in adaptation-priority agenda-setting that directs finance to wealthy-region contractors and debt-bearing infrastructure.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_vulnerable, payer,
    powerless, generational, trapped, global).

% Sets the international climate policy agenda through NDC frameworks, UNFCCC processes, and IFI lending priorities; frames 2-3Â°C as inevitable to manage domestic political economy and avoid rapid decarbonization conflicts with fossil capital and electorates.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_nation_governments, agenda_setter,
    institutional, generational, constrained, global).

% Scientific and civil society actors arguing for rapid decarbonization and prevention; structurally marginalized in national policy processes where adaptation priority dominates funding, regulatory attention, and diplomatic bandwidth.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, mitigation_advocates, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, fossil_capital).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared policy expectation that 2-3Â°C warming is locked in, coordinating current-generation wealthy-nation actors around resilience investment, adaptation finance, and infrastructure planning rather than emission reduction.
% TRANSFER_FUNCTION: Moves the costs of climate impacts from current-generation emitters and fossil capital to future generations and vulnerable Global South populations, while concentrating adaptation investment and political capital in wealthy regions.
% ABSENT_VOICES: Future generations are physically absent from policy forums; Global South vulnerable populations are nominally present in UNFCCC processes but structurally excluded from adaptation-priority agenda-setting; mitigation and degrowth advocates are marginalized in national policy frameworks where adaptation dominates.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, policy would reorganize around mitigation and prevention; fossil capital would face immediate transition risk and stranded assets; intergenerational and North-South cost shifting would become politically contestable; adaptation finance flows would be rebalanced toward emission reduction and loss-and-damage compensation.
% FOUNDING_PROBLEM: Climate change poses severe risks that require coordinated societal response; some degree of warming is already unavoidable due to historical emissions, requiring adaptation.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Groups and independent climate scientists attest that adaptation is necessary for unavoidable impacts, corroborating part of the founding problem. However, these same sources contest the framing of 2-3Â°C as inevitable or preferable to rapid mitigation, corroborating the contested status from outside the beneficiary set. Global South governments and intergenerational ethicists also contest the priority assignment.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because the constraint systematically transfers climate damages from present emitters to future generations and Global South populations while protecting fossil capital. Suppression is substantial (0.68) because the constraint depends on actively marginalizing mitigation alternatives, excluding future voices, and capturing international policy bandwidth. Theater_ratio (0.45) reflects significant performative adaptation planning that obscures continued emission growth and fossil investment. Accessibility_collapse is high (0.75) because once the inevitability framing is institutionalized, rapid mitigation appears politically impossible and future generations have no exit. Resistance is moderate (0.55) because climate movements and vulnerable-nation alliances contest the frame but remain institutionally contained. The measurement series run on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter and beneficiary seats, the constraint presents as pragmatic coordination around unavoidable physical reality; from the payer seats, it operates as an intergenerational and colonial extraction mechanism that forces those with no voice to bear the costs of continued emissions. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil_capital and wealthy_nations_current_gen are declared beneficiaries with mobile or arbitrage exit options; the engine derives low directionality for these seats, damping effective extraction into subsidy. Future_generations and global_south_vulnerable are declared victims with trapped exit and universal/global scope; the engine derives high directionality, amplifying effective extraction. Wealthy_nation_governments sit between as agenda_setters with constrained exitâstructurally closer to beneficiaries but not direct rent capturers.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents mislabeling it as a rope (pure coordination) because identifiable victims bear asymmetric costs, and it prevents mislabeling it as a snare because adaptation genuinely does reduce vulnerability for some populationsâthe coordination function is real, not cover. The mandatrophy risk would be declaring the founding problem dead and the constraint a piton, but the active enforcement and rising extraction series show it is not inertial theater; it is actively maintained because beneficiaries profit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_framing_ambiguity,
    'Is 2-3Â°C warming genuinely locked in by physical and infrastructural dynamics, or is the inevitability framing a political construct to avoid mitigation costs and protect fossil capital?',
    'Compare remaining carbon budgets and technological feasibility studies against the policy lock-in created by infrastructure investments and political declarations; test whether mitigation pathways were economically viable when the adaptation-priority frame was institutionalized.',
    'If physical models show remaining budget compatible with lower warming, the constraint is a snare using false inevitability to protect extraction; if genuinely locked in, the extraction is the unfortunate externality of necessary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_framing_ambiguity, empirical, 'Whether warming inevitability is physically determined or politically constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is suppression of mitigation alternatives structural (institutional agenda control, financial capture, diplomatic conditioning) or internalized (policy elites genuinely believing adaptation is the only pragmatic path)?',
    'Track policy discourse and funding shifts after extreme weather events or cost breakthroughs in clean technology; if suppression decays when windows open, it was structural; if it persists regardless, it is internalized cognitive capture.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure and persistence is harder to disrupt; if structural, targeted institutional reform could reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression of mitigation alternatives').

omega_variable(
    adaptation_finance_capture,
    'Does adaptation investment actually reach vulnerable Global South populations, or is it captured by wealthy-nation contractors, financial institutions, and domestic infrastructure in the global North?',
    'Audit adaptation finance flows, project ownership, and debt structures; compare disbursement volumes to local-control metrics.',
    'If captured, the coordination function is weaker and extraction stronger than measured; if reaching vulnerable populations, the tangled rope has a stronger coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_finance_capture, empirical, 'Whether adaptation finance is captured by wealthy-region interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__adaptation_priority, theater_ratio, 10, 0.3).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__adaptation_priority, theater_ratio, 20, 0.38).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__adaptation_priority, theater_ratio, 30, 0.42).
narrative_ontology:measurement(clim_tr_t34, climate_response_obligation__adaptation_priority, theater_ratio, 34, 0.45).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__adaptation_priority, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__adaptation_priority, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__adaptation_priority, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(clim_be_t34, climate_response_obligation__adaptation_priority, base_extractiveness, 34, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__adaptation_priority, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__adaptation_priority, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__adaptation_priority, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(clim_su_t34, climate_response_obligation__adaptation_priority, suppression_requirement, 34, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is the adaptation_priority reading of the climate_response_obligation kernel; sibling readings instantiate mitigation_priority and degrowth_reading constraints from the same contested kernel, linked by shared regulatory domain and causal interdependence of policy frames.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
