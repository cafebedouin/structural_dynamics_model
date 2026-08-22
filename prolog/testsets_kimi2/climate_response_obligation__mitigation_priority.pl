% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Intergenerational Climate Mitigation Obligation
 *   domain: climate_policy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the mitigation_priority reading of the
 *   climate_response_obligation kernel. It treats rapid decarbonization to
 *   minimize warming as an intergenerational justice imperative: future
 *   generations are the primary beneficiaries, while present Global North
 *   publics and fossil capital bear the transition costs and stranded assets.
 *   The constraint is actively enforced through multilateral carbon budgets,
 *   nationally determined contributions, and domestic regulatory mandates. It
 *   is claimed as coordination (preventing catastrophic climate change) but
 *   operates with pronounced asymmetric extraction.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary (powerless/universal) â receives stable climate but lacks present voice
 *   - global_north_transition_bearers: Primary payer (organized/constrained) â bears disproportionate mitigation costs via taxes and transition infrastructure
 *   - fossil_capital: Secondary payer (powerful/constrained) â absorbs stranded assets and regulatory obsolescence
 *   - multilateral_climate_regime: Agenda setter (institutional/global) â administers carbon budgets and compliance
 *   - intergenerational_justice_analysts: Analytical observer (analytical/civilizational) â evaluates fairness and efficacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.72).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.68).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Intergenerational Climate Mitigation Obligation").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, 'b7c122a0-4c63-450a-9834-a558c09639da').
narrative_ontology:cs_kernel_codification('b7c122a0-4c63-450a-9834-a558c09639da', formalized).
narrative_ontology:cs_authority_grounding('b7c122a0-4c63-450a-9834-a558c09639da', lineage).
narrative_ontology:cs_interpretation_layer_present('b7c122a0-4c63-450a-9834-a558c09639da').
narrative_ontology:cs_reading_relation('b7c122a0-4c63-450a-9834-a558c09639da', climate_response_obligation__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('b7c122a0-4c63-450a-9834-a558c09639da', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('b7c122a0-4c63-450a-9834-a558c09639da', foundational, intergenerational_mitigation_imperative).
narrative_ontology:cs_axiom_status(intergenerational_mitigation_imperative, holdable).
narrative_ontology:cs_axiom_grounding('b7c122a0-4c63-450a-9834-a558c09639da', intergenerational_mitigation_imperative, deontological).
narrative_ontology:cs_axiom('b7c122a0-4c63-450a-9834-a558c09639da', secondary, historical_emissions_differential_burden).
narrative_ontology:cs_axiom_status(historical_emissions_differential_burden, holdable).
narrative_ontology:cs_axiom_grounding('b7c122a0-4c63-450a-9834-a558c09639da', historical_emissions_differential_burden, conventional).
narrative_ontology:cs_reference_frame('b7c122a0-4c63-450a-9834-a558c09639da', unfccc_intergenerational_equity_framework).
narrative_ontology:cs_drift_state('b7c122a0-4c63-450a-9834-a558c09639da', contemporary_post_paris_accord, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b7c122a0-4c63-450a-9834-a558c09639da', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_transition_bearers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_capital).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Will inhabit the climate system resulting from present emissions choices; primary intended beneficiary of mitigation but possesses no present vote, negotiating seat, or market power to enforce the obligation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Bear disproportionate mitigation costs through carbon taxes, higher energy prices, infrastructure retrofit mandates, and consumption constraints justified by historical emissions responsibility.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_transition_bearers, payer,
    organized, biographical, constrained, global).

% Holds fossil-fuel reserves and combustion-dependent infrastructure that lose value under rapid decarbonization mandates; faces stranded assets, divestment pressure, and regulatory obsolescence.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_capital, payer,
    powerful, biographical, constrained, global).

% Administers carbon budgets, nationally determined contributions, and reporting compliance; sets rules for burden-sharing and enforces the intergenerational mitigation norm through treaty mechanisms.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, multilateral_climate_regime, agenda_setter,
    institutional, generational, constrained, global).

% Evaluate whether mitigation trajectories satisfy ethical duties to future persons and assess the fairness of transition-cost distribution across generations and regions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, intergenerational_justice_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global greenhouse gas emission reductions to prevent catastrophic climate change and satisfy intergenerational ethical duties by stabilizing the climate system for future people.
% TRANSFER_FUNCTION: Moves transition costs, stranded asset losses, and consumption constraints from present fossil capital and Global North publics to future generations in the form of a stabilized climate and reduced catastrophic risk.
% ABSENT_VOICES: Future generations cannot participate in climate negotiations; fossil fuel workers facing transition precarity are under-represented relative to capital holders; current non-human life has no bargaining seat.
% DISAPPEARANCE_RATIONALE: If the mitigation obligation vanished, present incentives would revert to fossil-intensive pathways, the intergenerational transfer of climate stability would collapse, and global temperature would rise toward unabated trajectories.
% FOUNDING_PROBLEM: Unregulated greenhouse gas emissions constitute a collective-action commons crisis in which present actors externalize costs onto future generations and vulnerable ecosystems, risking catastrophic and irreversible warming.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists (IPCC) attest the physical emissions gap from outside the beneficiary set; intergenerational ethicists attest the moral asymmetry from an analytical seat. Future generations cannot corroborate their own interest.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because rapid decarbonization imposes substantial transition costs, stranded assets, and consumption constraints on present actors. Suppression (0.68) is high because the obligation requires active enforcement through carbon pricing, regulatory mandates, and international compliance to overcome free-rider incentives. Theater ratio (0.40) reflects moderate performative action (NDC ambition gaps, greenwashing) alongside real decarbonization. Accessibility collapse (0.50) is moderate: once the intergenerational framing is accepted, unabated emissions become illegitimate, but adaptation remains an alternative framing. Resistance (0.75) is high from fossil capital and segments of the Global North public.
 *
 * PERSPECTIVAL GAP:
 *   From the future generations' seat, the constraint is life-preserving coordination; from the fossil capital seat, it is expropriation via stranded assets; from the Global North seat, it is a disproportionate extraction of transition costs justified by historical responsibility. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are declared beneficiaries with no exit (trapped), producing a strongly beneficiary-directional seat. Global North transition bearers and fossil capital are declared victims with constrained exit, producing target-directional seats. The multilateral climate regime sits near symmetric as agenda-setter administering the transfer but not personally collecting it. The analyst seat is neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â unpriced atmospheric carbon creating intergenerational externalities â remains live. The constraint has not outlived its function, so mandatrophy is not declared. Classifying as tangled_rope captures the genuine coordination (climate stabilization) alongside the asymmetric extraction (concentrated present costs, diffuse future benefits), preventing mislabeling as either pure coordination (rope) or pure extraction (snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_priority_kernel_position,
    'How does the mitigation_priority reading''s core axiom (minimize warming as intergenerational duty) structurally relate to the adaptation_priority reading''s axiom (accept warming, invest in resilience)?',
    'Comparative policy analysis of whether jurisdictions can simultaneously hold mitigation ceilings and adaptation floors without contradiction.',
    'If the readings are mutually exclusive in practice, this constraint approaches foreclosing adaptation_priority; if co-implementable, it merely influences it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_priority_kernel_position, conceptual, 'Structural relationship between mitigation and adaptation priority readings').

omega_variable(
    global_north_cost_naturality,
    'Is the disproportionate Global North mitigation burden a constructed distributive rule or a natural consequence of historical emissions?',
    'Historical emissions accounting and counterfactual analysis of alternative burden-sharing schemes.',
    'If purely constructed, the constraint is more extractive toward Global North publics; if naturally determined by stock-flow physics, the extraction is a material derivative rather than a policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_north_cost_naturality, empirical, 'Whether burden sharing is constructed or natural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__mitigation_priority, theater_ratio, 10, 0.28).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.35).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.38).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__mitigation_priority, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__mitigation_priority, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__mitigation_priority, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__mitigation_priority, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__mitigation_priority, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
