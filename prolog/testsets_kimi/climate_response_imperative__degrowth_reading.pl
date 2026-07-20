% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Climate Response Imperative â Degrowth Reading
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint instantiates the degrowth reading of the contested
 *   climate_response_imperative kernel. It holds that genuine climate
 *   response requires structural economic transformation in the Global
 *   Northâreduced consumption, redistribution, and post-growth
 *   institutionsâto enable both mitigation and adaptation. Present-day
 *   Global North populations are structurally situated as the payer seat,
 *   while future generations and Global South populations are beneficiaries.
 *   The constraint is actively enforced through policy mandates and
 *   institutional redesign, and it must suppress growth-centric alternatives
 *   to maintain its operative logic.
 *
 * KEY AGENTS:
 *   - present_day_global_north_populations: Primary target (organized/constrained) â bears extraction via reduced consumption and working time
 *   - future_generations: Primary beneficiary (powerless/trapped) â receives climate stability and intergenerational justice
 *   - global_south_populations: Secondary beneficiary (organized/constrained) â receives redistribution and reduced climate impacts
 *   - global_north_governments: Agenda setter (institutional/constrained) â administers structural transformation facing domestic resistance
 *   - green_growth_advocates: Excluded voice (powerful/mobile) â holds competing mitigation-priority reading, marginalized within this frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.72).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.65).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Climate Response Imperative â Degrowth Reading").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '2efa7c66-154f-4523-90d4-b32c9314074d').
narrative_ontology:cs_kernel_codification('2efa7c66-154f-4523-90d4-b32c9314074d', distributed).
narrative_ontology:cs_authority_grounding('2efa7c66-154f-4523-90d4-b32c9314074d', distributed).
narrative_ontology:cs_reading_relation('2efa7c66-154f-4523-90d4-b32c9314074d', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('2efa7c66-154f-4523-90d4-b32c9314074d', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('2efa7c66-154f-4523-90d4-b32c9314074d', foundational, global_north_contraction_imperative).
narrative_ontology:cs_axiom_status(global_north_contraction_imperative, holdable).
narrative_ontology:cs_axiom_grounding('2efa7c66-154f-4523-90d4-b32c9314074d', global_north_contraction_imperative, deontological).
narrative_ontology:cs_axiom('2efa7c66-154f-4523-90d4-b32c9314074d', foundational, unproven_cdr_exclusion).
narrative_ontology:cs_axiom_status(unproven_cdr_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('2efa7c66-154f-4523-90d4-b32c9314074d', unproven_cdr_exclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('2efa7c66-154f-4523-90d4-b32c9314074d', climate_justice_via_structural_contraction).
narrative_ontology:cs_drift_state('2efa7c66-154f-4523-90d4-b32c9314074d', green_growth_hegemony, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2efa7c66-154f-4523-90d4-b32c9314074d', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, present_day_global_north_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the costs of structural economic transformation through reduced consumption, altered working-time arrangements, and transitional welfare losses mandated by post-growth institutions. Their current standard of living and expected economic trajectories are directly contracted by the imperative.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, present_day_global_north_populations, payer,
    organized, biographical, constrained, global).

% Receive the benefits of a stabilized climate and reduced atmospheric disruption enabled by Global North contraction, but are not yet born and therefore cannot participate in the policy decisions that determine their inheritance.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Benefit from reduced climate impacts and from redistribution mechanisms funded by Global North economic contraction, though they remain structurally exposed to locked-in warming and dependent on North-dominated institutions for delivery.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    organized, generational, constrained, global).

% Administer the structural transformation through legislation, working-time regulation, redistribution frameworks, and post-growth institution-building, caught between climate commitments and domestic political resistance to contraction.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_governments, agenda_setter,
    institutional, generational, constrained, continental).

% Promote technological innovation, market mechanisms, and green growth as sufficient for climate response; structurally marginalized within the degrowth framing which treats their core premise as empirically inadequate and ethically evasive.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, green_growth_advocates, excluded,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables collective climate action by aligning Global North economic activity with planetary boundaries through coordinated contraction, redistribution, and post-growth institution-building, preventing free-riding on the atmospheric commons and intergenerational resource transfer.
% TRANSFER_FUNCTION: Transfers economic capacity, consumption opportunities, and working-time autonomy from present-day Global North populations to future generations and Global South populations via enforced structural economic transformation.
% ABSENT_VOICES: Green growth and technology-first advocates who believe market mechanisms and innovation suffice; present-day Global North populations who would reject reduced consumption if directly consulted; fossil-fuel dependent industries and finance actors whose interests are written out of the degrowth frame.
% DISAPPEARANCE_RATIONALE: If the imperative vanished, Global North economies would revert to growth-centric pathways, redistribution mechanisms would dissolve, and the beneficiaries (future generations, Global South) would lose the planned mitigation and adaptation capacity that contraction is meant to secure.
% FOUNDING_PROBLEM: Climate change driven by Global North overconsumption and capital accumulation threatens planetary habitability and disproportionately harms the Global South and future generations, while market-based and technological responses remain insufficient to close the emissions gap.
% FOUNDING_PROBLEM_CORROBORATION: Climate science (IPCC) attests to the emissions gap and disproportionate impacts; ecological economists corroborate the insufficiency of absolute decoupling. Mainstream economists, technology policy actors, and several Global North governments dispute the necessity of contraction, arguing that innovation-driven decoupling can solve the problem without welfare losses. No universal corroboration outside the beneficiary set exists.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the imperative imposes concrete welfare losses on a defined present-day population for the benefit of temporally and geographically distant agents. Suppression (0.65) reflects the necessity of actively displacing growth-centric institutions and market alternatives that would otherwise reproduce the emissions trajectory. Theater ratio (0.40) acknowledges that while degrowth scholarship is analytically serious, institutional adoption risks performative 'green austerity' that maintains elite consumption while imposing costs on workers. Resistance (0.75) captures the strong political opposition from populations and interests bearing the contraction costs. Accessibility collapse (0.60) registers that within the degrowth frame, growth alternatives become cognitively and morally inaccessible, even if they remain structurally available in competing frames.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (present-day Global North populations) experiences the constraint as enforced extraction of living standards and autonomy. The beneficiary seats (future generations, Global South) experience it as necessary coordination to secure habitability and justice. The agenda-setter seat (Global North governments) experiences a bind between coordination benefits and domestic political resistance. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and Global South populations are structural beneficiaries (low d) because the constraint subsidizes their climate security and redistributive claims. Present-day Global North populations are structural targets (high d) because the constraint extracts consumption and working-time capacity from them. Global North governments sit near the middle: they administer the extraction but also stand to benefit from long-term systemic stability. Green growth advocates are excluded rather than coordinated; their exclusion is a precondition for the constraint's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by its explicit beneficiary/victim asymmetry and active enforcement requirement. If the costs of contraction were shared symmetrically across all global populations, or if a sunset clause tied the arrangement to a transitional decarbonization phase, it would read as rope or scaffold. If the climate coordination story were pure cover for austerity with no genuine mitigation/adaptation benefit, it would read as snare. The tangled_rope classification is structurally warranted because a genuine coordination function (climate stabilization) and an asymmetric extraction function (Global North present-population welfare loss) are locked together in the same institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_imperative_kernel_contest,
    'Is the climate response imperative properly read as requiring Global North structural contraction (degrowth), technological mitigation priority, or adaptation-first resilience?',
    'Comparative policy analysis tracking actual emissions reductions, welfare distributions, and adaptation outcomes under each reading''s implementation, combined with decoupling viability studies.',
    'Resolution would reallocate beneficiary and victim seats across the three readings; the degrowth reading collapses if absolute decoupling proves viable and equitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_imperative_kernel_contest, conceptual, 'Kernel-level contest between three readings of the climate imperative').

omega_variable(
    decoupling_viability,
    'Can absolute decoupling of economic growth from resource use and emissions achieve climate targets without Global North structural contraction?',
    'Longitudinal macro-ecological data on material footprint and emissions intensity versus GDP in Global North economies, assessed against climate budgets.',
    'If decoupling is viable, the degrowth reading''s victim set may be unnecessary and the constraint shifts toward snare; if not, the coordination function of contraction is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_viability, empirical, 'Empirical ambiguity about green growth decoupling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t8, climate_response_imperative__degrowth_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(clim_tr_t16, climate_response_imperative__degrowth_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(clim_tr_t24, climate_response_imperative__degrowth_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(clim_tr_t32, climate_response_imperative__degrowth_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(clim_tr_t40, climate_response_imperative__degrowth_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clim_be_t8, climate_response_imperative__degrowth_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(clim_be_t16, climate_response_imperative__degrowth_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(clim_be_t24, climate_response_imperative__degrowth_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(clim_be_t32, climate_response_imperative__degrowth_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(clim_be_t40, climate_response_imperative__degrowth_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(clim_su_t8, climate_response_imperative__degrowth_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(clim_su_t16, climate_response_imperative__degrowth_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(clim_su_t24, climate_response_imperative__degrowth_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(clim_su_t32, climate_response_imperative__degrowth_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(clim_su_t40, climate_response_imperative__degrowth_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is the degrowth reading of the climate_response_imperative kernel. The kernel decomposes into three structurally distinct constraints because the natural-language label 'climate response' conflates competing beneficiary/victim structures and policy mechanisms. Each reading assigns different agents to payer and beneficiary seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
