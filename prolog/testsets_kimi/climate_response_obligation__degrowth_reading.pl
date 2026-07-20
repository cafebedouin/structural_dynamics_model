% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Climate Response Obligation â Degrowth/Sufficiency Reading
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the degrowth reading of the
 *   climate_response_obligation kernel. Under this reading, the obligation to
 *   respond to climate change takes the form of binding reductions in
 *   aggregate material throughput, prioritizing sufficiency over efficiency,
 *   to remain within planetary boundaries. The constraint coordinates global
 *   human activity against biophysical limits but asymmetrically extracts
 *   from Global North consumption patterns and capital accumulation, while
 *   conditionally constraining Global South development unless Northern
 *   reductions create ecological headroom. It is claimed as tangled_rope
 *   because it combines a genuine coordination function (planetary survival,
 *   intergenerational justice) with asymmetric extraction (forced lifestyle
 *   reduction, accumulation limits). The claim and metrics are authored
 *   independently: the tangled_rope claim reflects the structural combination
 *   of coordination and extraction, while the metrics describe the severity
 *   of the imposed limits without tuning to match the claim.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary (powerless/trapped/global) â receives a stable Earth system but has no present policy voice
 *   - global_south_nations: Conditional beneficiary (moderate/constrained/global) â gains development headroom only if Global North reduces throughput first
 *   - global_north_consumers: Primary payer (powerful/constrained/national) â bears lifestyle reduction and sufficiency limits
 *   - fossil_capital_complex: Secondary payer (powerful/constrained/global) â faces stranded assets and accumulation constraints
 *   - degrowth_policy_coalition: Agenda setter (organized/mobile/global) â designs and advocates binding sufficiency frameworks
 *   - planetary_science_community: Observer (institutional/analytical/global) â establishes the boundary thresholds that define the constraint's content
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.72).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Climate Response Obligation â Degrowth/Sufficiency Reading").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, 'c426870a-36e9-4283-97c9-60d0f554ba38').
narrative_ontology:cs_kernel_codification('c426870a-36e9-4283-97c9-60d0f554ba38', distributed).
narrative_ontology:cs_authority_grounding('c426870a-36e9-4283-97c9-60d0f554ba38', expertise).
narrative_ontology:cs_interpretation_layer_present('c426870a-36e9-4283-97c9-60d0f554ba38').
narrative_ontology:cs_reading_relation('c426870a-36e9-4283-97c9-60d0f554ba38', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('c426870a-36e9-4283-97c9-60d0f554ba38', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('c426870a-36e9-4283-97c9-60d0f554ba38', foundational, planetary_boundaries_hard_limit).
narrative_ontology:cs_axiom_status(planetary_boundaries_hard_limit, holdable).
narrative_ontology:cs_axiom_grounding('c426870a-36e9-4283-97c9-60d0f554ba38', planetary_boundaries_hard_limit, empirically_contingent).
narrative_ontology:cs_axiom('c426870a-36e9-4283-97c9-60d0f554ba38', foundational, sufficiency_over_efficiency_imperative).
narrative_ontology:cs_axiom_status(sufficiency_over_efficiency_imperative, holdable).
narrative_ontology:cs_axiom_grounding('c426870a-36e9-4283-97c9-60d0f554ba38', sufficiency_over_efficiency_imperative, deontological).
narrative_ontology:cs_reference_frame('c426870a-36e9-4283-97c9-60d0f554ba38', planetary_sufficiency_equilibrium).
narrative_ontology:cs_drift_state('c426870a-36e9-4283-97c9-60d0f554ba38', carbon_lock_in_peak, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c426870a-36e9-4283-97c9-60d0f554ba38', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, fossil_capital_complex).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit the Earth system that current throughput decisions determine. They have no present voice or exit from the constraint; their interests are mediated entirely by contemporary advocacy and institutional proxy.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Stand to gain ecological and atmospheric headroom for development if Global North economies reduce throughput first under an equity-weighted sufficiency framework. Their beneficiary status is conditional on Northern compliance and effective transfer of material budget.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_nations, beneficiary,
    moderate, generational, constrained, global).

% Bear the direct costs of lifestyle reduction, decommodification, and consumption limits implied by binding throughput caps. Their high-throughput patterns are the explicit target of the constraint, generating sustained political resistance.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_consumers, payer,
    powerful, biographical, constrained, national).

% Faces structural devaluation, stranded assets, and accumulation limits as the constraint treats growth-dependent extraction as the primary mechanism of boundary transgression. Enforcement directly threatens their operating model.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, fossil_capital_complex, payer,
    powerful, biographical, constrained, global).

% Designs and advocates for binding throughput reductions, sufficiency standards, and contraction-and-convergence frameworks. Derives legitimacy from planetary boundary science and intergenerational ethics rather than from electoral majorities or market share.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_policy_coalition, agenda_setter,
    organized, generational, mobile, global).

% Monitors Earth system indicators and quantifies the planetary boundaries that give the constraint its biophysical content. Provides the epistemic architecture without directly collecting benefits or paying the constraint's costs.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_science_community, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global human material activity to remain within biophysical planetary boundaries, preventing Earth system destabilization that would otherwise collapse intergenerational carrying capacity and ecological stability.
% TRANSFER_FUNCTION: Moves material consumption headroom and ecological space from present high-throughput Global North economies to future generations and Global South development needs, while constraining aggregate capital accumulation and fossil-dependent growth.
% ABSENT_VOICES: Non-human planetary systems and unborn future generations bear the consequences of boundary transgression but hold no policy seats; mainstream growth-dependent macroeconomic institutions are present in discourse but the biophysical entities they impact are structurally excluded.
% DISAPPEARANCE_RATIONALE: If the obligation to reduce throughput and prioritize sufficiency vanished, growth-dependent extraction would reassert dominance, planetary boundaries would be further transgressed, and the burden of destabilization would fall on future generations and vulnerable regions â the global institutional order would reorganize around unbounded material expansion.
% FOUNDING_PROBLEM: Industrial economies have transgressed multiple planetary boundaries (climate, nitrogen, biodiversity loss) and efficiency-oriented market responses have failed to decouple material throughput from economic growth, generating an unsustainable intergenerational transfer of ecological debt.
% FOUNDING_PROBLEM_CORROBORATION: Earth system scientists (planetary boundaries framework) and ecological economists attest the founding problem from outside the immediate beneficiary set; adversarial corroboration comes from growth-dependent governments and neoclassical economists who contest the severity or framing, confirming the problem is contested rather than fabricated.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint imposes severe material and lifestyle reductions on high-throughput economies and fundamentally constrains capital accumulation. Suppression (0.72) is higher still because enforcing global throughput caps against growth-dependent institutions requires active, persistent coercion and institutional override. Theater_ratio (0.40) reflects moderate performative drift: green-growth rhetoric and efficiency promises frequently masquerade as sufficiency, but the degrowth reading explicitly distinguishes itself from such theater. Accessibility_collapse (0.82) is high because, once planetary boundaries are accepted as biophysical limits, infinite-growth alternatives conceptually collapse regardless of political resistance. Resistance (0.68) is substantial and well-documented from growth-dependent actors. The temporal series track the constraint's evolution from marginal ecological-economics discourse to a binding policy proposal with rising enforcement requirements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (degrowth policy coalition) experiences the constraint as necessary coordination against existential risk; the payer seats (Global North consumers, fossil capital) experience it as imposed extraction that threatens their operating models. The beneficiary seats (future generations, Global South nations) experience it as deferred or conditional subsidy. The engine computes these divergences from the structural data: agenda setters and beneficiaries derive low directionality, payers derive high directionality, producing computationally distinct per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (future_generations, global_south_nations) are structurally subsidized by the constraint â it creates ecological space and intergenerational stability for them â placing their derived directionality near the beneficiary pole. Victims/payers (global_north_consumers, fossil_capital_complex) bear the direct costs of throughput reduction and accumulation limits, placing their derived directionality near the target pole. The observer (planetary_science_community) sits at analytical scope with neutral directionality. No overrides are required because the structural derivation from beneficiary/victim declarations, combined with constrained or trapped exit options for the governed seats, already captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this constraint could be misread as either a pure rope (if only the planetary coordination function is visible) or a pure snare (if only the Global North lifestyle reduction is visible). The tangled_rope gate requires naming both coordinated beneficiaries and paying victims alongside active enforcement. This prevents false naturalization (treating biophysical limits as automatically licensing redistribution without coordination costs) and false criminalization (treating survival-oriented coordination as pure extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_kernel_reading_contest,
    'This constraint instantiates the degrowth reading of the climate_response_obligation kernel; how would its classification shift if the mitigation_priority or adaptation_priority reading were adopted instead?',
    'Comparative analysis of the sibling constraint stories: mitigation_priority would likely reduce victimization of global_north_consumers and reclassify toward rope or scaffold by foregrounding technological coordination; adaptation_priority would abandon planetary boundary compliance and likely reclassify as snare by shifting victimization to climate-vulnerable populations.',
    'Would reclassify the constraint toward lower extraction (mitigation) or pure extraction (adaptation), altering the directionality derivation and the legitimacy of the coordination story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_kernel_reading_contest, conceptual, 'Uncertainty about how sibling readings would change the constraint''s structural classification.').

omega_variable(
    global_south_beneficiary_ambiguity,
    'Are global_south_nations genuine beneficiaries of this constraint, or conditional victims of a globally binding throughput cap that the Global North has not yet implemented?',
    'Empirical tracking of North-South material flows, development finance, and per-capita ecological footprint under partial implementation scenarios.',
    'If the South remains constrained without meaningful Northern reduction, the constraint''s victim set expands, its equity coordination claim weakens, and the effective extraction from the South rises â potentially shifting the computed seat classification for Global South actors toward target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_beneficiary_ambiguity, empirical, 'Whether Global South beneficiary status is realized or merely projected.').

omega_variable(
    boundary_naturalness_vs_construct,
    'Is the planetary boundary a natural limit that anchors the constraint''s coordination function in physical reality, or a politically constructed threshold that enables redistribution?',
    'Independent scientific consensus audit on planetary boundaries independent of the policy prescriptions derived from them; examination of boundary parameter choices for political contingency.',
    'If boundaries are constructed with high political contingency, the coordination story becomes cover for a redistribution agenda and the constraint edges toward snare; if boundaries are rigid physical limits, the coordination function is robust and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_naturalness_vs_construct, empirical, 'Uncertainty about whether planetary boundaries are natural anchors or constructed instruments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__degrowth_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__degrowth_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__degrowth_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__degrowth_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(clim_tr_t50, climate_response_obligation__degrowth_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__degrowth_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__degrowth_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__degrowth_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__degrowth_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(clim_be_t50, climate_response_obligation__degrowth_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__degrowth_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__degrowth_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__degrowth_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__degrowth_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(clim_su_t50, climate_response_obligation__degrowth_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_obligation kernel. The kernel decomposes into three structurally distinct claims: mitigation_priority (green growth/efficiency), degrowth_reading (sufficiency/throughput limits), and adaptation_priority (resilience/acceptance). Each reading has a different beneficiary/victim structure and extractiveness profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
