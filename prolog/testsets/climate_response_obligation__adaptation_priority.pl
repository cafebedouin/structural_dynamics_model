% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: Climate Response Obligation: Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of the climate response obligation kernel
 *   is the policy position that accepts 2-3°C warming as inevitable and
 *   proposes shifting resources from costly prevention (rapid
 *   decarbonization) toward building resilience in vulnerable regions and
 *   critical infrastructure. This reading exhibits Tangled Rope structure: it
 *   solves a genuine coordination problem (transition shock from rapid
 *   decarbonization can destabilize current economies, creating backlash and
 *   political failure) while simultaneously enabling asymmetric extraction
 *   (current wealthy generations avoid transition costs while future
 *   generations and the Global South inherit a warmer, less stable world and
 *   bear adaptation burdens disproportionately). The constraint is not pure
 *   extraction because adaptation investment does provide genuine value to
 *   vulnerable populations — the coordination function is real. But it is not
 *   pure coordination because the asymmetry in who benefits from continued
 *   business-as-usual (fossil capital, wealthy nations avoiding transition
 *   disruption) versus who bears the adaptation burden (future generations,
 *   Global South) is structural and foreseeable. The reading's core claim —
 *   that preventing 2-3°C warming is too costly and that adaptation is more
 *   feasible — rests on undisclosed axioms about intergenerational discount
 *   rates and the adequacy of adaptation at high warming levels. These axioms
 *   distinguish the adaptation-priority reading from the mitigation-priority
 *   reading (which asserts intergenerational duty to prevent future harm) and
 *   from the degrowth reading (which asserts that material throughput
 *   reduction is necessary and feasible). The structure of the constraint
 *   reveals how a policy reading can simultaneously provide genuine
 *   coordination benefits while creating conditions for sustained asymmetric
 *   extraction across temporal and geographic axes.
 *
 * KEY AGENTS:
 *   - Current generation wealthy nations (institutional/arbitrage): Primary beneficiary — avoids costly rapid decarbonization transition; extends fossil fuel economy; maintains existing capital structures.
 *   - Fossil fuel capital (powerful/arbitrage): Primary beneficiary — adaptation priority reading explicitly permits continued hydrocarbon extraction through mid-century; protects capital rents.
 *   - Future generations (powerless/trapped): Primary victim — structurally unable to exit; inherit accumulated warming; expected to adapt to world current generation was unwilling to prevent.
 *   - Global South climate-vulnerable populations (moderate/constrained): Secondary victim — lowest adaptive capacity; highest climate impact; receive minimal adaptation investment relative to need; cannot exit through migration.
 *   - Adaptation service providers (powerful/constrained): Mixed — benefit from massive adaptation investment pipeline but constrained by reading's commitment to 2-3°C baseline.
 *   - International climate institutions (institutional/arbitrage): Maintain performative climate commitment while permitting adaptation-priority extraction; degraded function masked by bureaucratic theater.
 *   - Analytical observer (analytical/analytical): Witnesses the reading's simultaneous real coordination value and systematic extraction structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.58).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.52).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Climate Response Obligation: Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, '2f173027-dd80-4a2d-9956-57ee22aa5fcf').
narrative_ontology:cs_kernel_codification('2f173027-dd80-4a2d-9956-57ee22aa5fcf', distributed).
narrative_ontology:cs_authority_grounding('2f173027-dd80-4a2d-9956-57ee22aa5fcf', extraction).
narrative_ontology:cs_reading_relation('2f173027-dd80-4a2d-9956-57ee22aa5fcf', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('2f173027-dd80-4a2d-9956-57ee22aa5fcf', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('2f173027-dd80-4a2d-9956-57ee22aa5fcf', foundational, rapid_decarbonization_infeasible).
narrative_ontology:cs_axiom_status(rapid_decarbonization_infeasible, holdable).
narrative_ontology:cs_axiom_grounding('2f173027-dd80-4a2d-9956-57ee22aa5fcf', rapid_decarbonization_infeasible, empirically_contingent).
narrative_ontology:cs_axiom('2f173027-dd80-4a2d-9956-57ee22aa5fcf', foundational, adaptation_adequacy_at_two_degrees).
narrative_ontology:cs_axiom_status(adaptation_adequacy_at_two_degrees, holdable).
narrative_ontology:cs_axiom_grounding('2f173027-dd80-4a2d-9956-57ee22aa5fcf', adaptation_adequacy_at_two_degrees, empirically_contingent).
narrative_ontology:cs_reference_frame('2f173027-dd80-4a2d-9956-57ee22aa5fcf', techno_economic_feasibility_primacy).
narrative_ontology:cs_drift_state('2f173027-dd80-4a2d-9956-57ee22aa5fcf', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2f173027-dd80-4a2d-9956-57ee22aa5fcf', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_wealthy).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_fuel_capital).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, adaptation_service_providers).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, climate_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEALTHY NATIONS (ROPE) — Experiences adaptation priority as legitimate coordination. The reading solves a real problem: rapid decarbonization is costly and disruptive to current economies. Adaptation investment enables economic continuity during the transition window. Net beneficiary; sees constraint as enabling efficient resource allocation.
constraint_indexing:constraint_classification(climate_response_obligation__adaptation_priority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Structurally trapped. Cannot exit, cannot negotiate, cannot opt out of bearing accumulated climate impacts. The adaptation priority reading forecloses their agency: they inherit a 2-3°C warmer world and are expected to adapt to it rather than having the warming prevented. Maximum experienced extraction with zero alternatives.
constraint_indexing:constraint_classification(climate_response_obligation__adaptation_priority, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GLOBAL SOUTH VULNERABLE POPULATIONS (SNARE) — Face constrained exit from climate impacts (some migration possible but costly and often blocked; relocation from degraded regions limited by sovereignty and resources). Adaptation investment concentrates in wealthy regions; vulnerable populations receive minimal adaptation support. High extraction with limited alternatives.
constraint_indexing:constraint_classification(climate_response_obligation__adaptation_priority, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FOSSIL FUEL CAPITAL (ROPE) — Primary beneficiary under adaptation priority reading. The reading permits continued hydrocarbon extraction and combustion through the 2050s; decarbonization is deferred. Fossil fuel producers capture rents during the extended extraction window. Experiences constraint as enabling their continued operation.
constraint_indexing:constraint_classification(climate_response_obligation__adaptation_priority, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ADAPTATION SERVICE PROVIDERS (TANGLED ROPE) — Mixed relationship. Benefits from the massive adaptation investment pipeline (insurance, infrastructure, water systems, agricultural technology) but also constrained by the reading's implicit commitment to 2-3°C warming as the baseline. Could benefit more from mitigation-priority reading if decarbonization opens new markets, but also benefits from the certainty of continued high-impact adaptation demand.
constraint_indexing:constraint_classification(climate_response_obligation__adaptation_priority, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL CLIMATE INSTITUTIONS (PITON) — Maintain performative commitment to climate action while permitting high-extraction adaptation priority. IPCC summaries emphasize 'both adaptation and mitigation' but policy implementation concentrates on adaptation. Institutional theater persists because the adaptation frame allows climate action language without threatening capital accumulation. Degraded function masked by bureaucratic performance.
constraint_indexing:constraint_classification(climate_response_obligation__adaptation_priority, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational timescale, the adaptation priority reading exhibits genuine coordination value (avoiding societal collapse via transition shock) AND asymmetric extraction (future generations and vulnerable present populations bear the costs; wealthy present bears minimal). The constraint is neither pure coordination nor pure extraction but both simultaneously — the definition of tangled rope.
constraint_indexing:constraint_classification(climate_response_obligation__adaptation_priority, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__adaptation_priority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_obligation__adaptation_priority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_obligation__adaptation_priority, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, TR),
    TR >= 0.70.

:- end_tests(climate_response_obligation__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading permits systematic extraction of future and Global South welfare to maintain current wealthy-generation comfort. However, extractiveness is not maximal because adaptation investment provides genuine value — the constraint is not pure predation. The value reflects that the coordination function (avoiding transition shock) is real but substantially smaller than the extraction function (protecting capital from decarbonization costs, deferring burdens to future). Trajectory rises from 0.32 to 0.68 as the cumulative warming and adaptation investment gap widens over the interval. Suppression (0.52): Moderate-high. The reading requires suppression of (a) intergenerational justice frameworks that would prioritize harm prevention over present comfort; (b) climate science findings that high-impact adaptation becomes inadequate above 2°C in many regions; (c) alternative policy framings (mitigation-priority, degrowth) that would disrupt the current extraction arrangement. Suppression is not total — these alternatives continue to exist and be articulated — but the adaptation-priority framing dominates policy implementation. Theater ratio (0.48): Low-moderate. The reading's coordination function is substantive (transition disruption risk is real), so the performative content is lower than for pure snares. However, a significant portion of the 'adaptation is adequate' claim relies on optimistic future capital availability and technological breakthroughs that may not materialize — the performative element increases over time as promised adaptation investment fails to materialize for vulnerable populations.
 *
 * PERSPECTIVAL GAP:
 *   The adaptation-priority reading produces maximal perspectival gaps. Current wealthy generations and fossil capital experience the constraint as enabling (Rope) — it solves the real problem of transition disruption while permitting capital continuity. Future generations experience it as pure extraction (Snare) — they cannot exit and cannot renegotiate the terms. The Global South experiences it as extraction with limited adaptation value (Snare) — they receive insufficient adaptation investment despite highest exposure. Adaptation service providers experience mixed benefits (Tangled Rope). The international climate institutions perform climate commitment while permitting extraction (Piton). The analytical observer recognizes that the reading provides genuine coordination while distributing extraction asymmetrically across time and geography. The perspectival gap is not a misunderstanding or observational disagreement — it reflects real structural differences in who benefits and who bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values for each perspective are derived from beneficiary/victim status, power level, and exit options. Current wealthy nations (institutional/arbitrage) have d ≈ 0.10 (beneficiary with costless exit) → low effective extraction. Future generations (powerless/trapped) have d ≈ 0.98 (victim with zero exit) → maximum effective extraction. Global South (moderate/constrained) have d ≈ 0.82 (victim with high-cost constrained exit) → high effective extraction. Fossil capital (powerful/arbitrage) have d ≈ 0.05 (beneficiary with arbitrage exit) → negative effective extraction. The reading's benefit flows toward low-d agents (wealthy present, fossil capital); the extraction flows toward high-d agents (future, vulnerable present). This is the defining feature of a Tangled Rope: genuine coordination function (avoiding transition shock) whose operation requires distributing extraction along power/exit axes.
 *
 * MANDATROPHY ANALYSIS:
 *   The adaptation-priority reading resolves mandatrophy by making explicit what is often implicit: the choice between temporal solidarity (intergenerational justice) and present stability (avoiding transition disruption). The coordination function (preventing transition shock) is real and significant. The extraction function (deferring warming costs to future, concentrating adaptation investment in wealthy regions, protecting fossil capital) is equally real and significant. The constraint is NOT misclassified as Tangled Rope — it genuinely exhibits both functions. The mandatrophy arises only if one denies either the coordination or extraction function. The adaptation-priority reading depends on the claim that transition costs are so high and adaptation is so feasible that present comfort justifies future burden. If this claim is false (transition is manageable and adaptation is inadequate), the constraint becomes pure Snare. The mitigation-priority reading asserts intergenerational duty and argues that transition costs are surmountable. The degrowth reading asserts that material reduction is necessary and that sufficiency reframes transition from cost to benefit. These are genuine alternatives, not observational disagreements about the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_sufficiency_threshold,
    'At 2-3°C warming, what level of adaptation investment is actually sufficient to prevent major livelihood collapse in vulnerable regions?',
    'Climate impact modeling by region; comparison of projected adaptation costs vs. available capital; longitudinal tracking of actual adaptation success rates in pilot regions',
    'If adaptation proves insufficient: the reading collapses into pure snare for vulnerable populations (no viable exit even with adaptation investment). If adaptation proves adequate: the tangled rope classification holds — genuine coordination function exists alongside extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_sufficiency_threshold, empirical, 'Whether adaptation investment actually provides sufficient resilience at 2-3°C warming').

omega_variable(
    intergenerational_discount_rate_ambiguity,
    'On what ethical and epistemic basis is a future generation''s welfare discounted relative to the present generation''s comfort?',
    'Philosophical analysis of revealed discount rates in climate policy; examination of whether discount rates are empirically justified (future adaptation capacity) or preference-based (present utility prioritized)',
    'If discount justified by empirical future prosperity: the adaptation reading''s asymmetry is legitimate. If discount is pure preference: the reading is preference-based extraction masquerading as rational allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discount_rate_ambiguity, conceptual, 'Justification for intergenerational welfare discounting in adaptation-priority reading').

omega_variable(
    fossil_capital_protection_mechanism,
    'Is adaptation priority reading de facto protection for fossil fuel capital, or is decoupling of adaptation from decarbonization genuinely neutral toward energy systems?',
    'Policy analysis: tracking of fossil fuel subsidies and protection under adaptation-priority policy regimes; comparison of decarbonization timelines in adaptation-priority vs. mitigation-priority jurisdictions; capital flight patterns in energy sector',
    'If fossil capital demonstrably protected: the reading is a snare for climate-constrained future and a rope for extractive capital. If fossil energy naturally phases under adaptation framework: the reading is neutral. If fossil energy accelerates under adaptation framework: the reading actively extracts from future for present benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fossil_capital_protection_mechanism, empirical, 'Whether adaptation-priority reading functions as de facto fossil fuel protection').

omega_variable(
    reading_committer_structure,
    'This constraint is ONE READING of the contested kernel ''climate_response_obligation''. How does this adaptation-priority reading relate structurally to the mitigation-priority and degrowth readings?',
    'Structural analysis of each reading''s foundational axioms and reference frames; identification of which readings'' core premises logically foreclose others vs. permit coexistence vs. create downstream pressure.',
    'If adaptation and mitigation readings foreclose each other: policy choice is binary, zero-sum. If they coexist: hybrid approaches are theoretically possible. If adaptation creates downstream pressure on mitigation: degrowth reading becomes necessary to prevent cascading extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_structure, conceptual, 'Structural relationship between adaptation-priority, mitigation-priority, and degrowth readings of the climate response kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_adapt_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.42).
narrative_ontology:measurement(clim_adapt_tr_t15, climate_response_obligation__adaptation_priority, theater_ratio, 15, 0.46).
narrative_ontology:measurement(clim_adapt_tr_t30, climate_response_obligation__adaptation_priority, theater_ratio, 30, 0.51).

% Extraction over time
narrative_ontology:measurement(clim_adapt_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_adapt_be_t15, climate_response_obligation__adaptation_priority, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(clim_adapt_be_t30, climate_response_obligation__adaptation_priority, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_adapt_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_adapt_su_t15, climate_response_obligation__adaptation_priority, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(clim_adapt_su_t30, climate_response_obligation__adaptation_priority, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, intergenerational_justice_framework).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, global_south_climate_reparations).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_subsidy_lock_in).

% DUAL FORMULATION NOTE:
% The climate response obligation kernel decomposes into three structurally distinct readings: mitigation-priority (prevent future warming; intergenerational justice frame), adaptation-priority (accept warming; optimize resilience; current-generation stability frame), and degrowth (material reduction is necessary; sufficiency reframes transition). Each reading has different ε values, different beneficiary/victim structures, and different temporal implications. They cannot be averaged or collapsed into a single constraint. The adaptation-priority reading (this file) sits upstream of intergenerational justice claims and global reparations demands — accepting warming makes reparations more difficult. It sits downstream of fossil fuel lock-in — adaptation priority permits continued extraction. Link all three readings to show the contested kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, institutional, 0.08).
constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, powerful, 0.06).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
