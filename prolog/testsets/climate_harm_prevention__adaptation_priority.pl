% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Climate Adaptation-First Priority (Higher Warming Trajectory Accepted)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of climate harm prevention accepts that
 *   mitigation (rapid fossil fuel phase-out, emissions reduction) is
 *   politically or economically infeasible, and legitimizes a response
 *   centered on building resilience infrastructure to protect present,
 *   high-capacity populations from near-term climate impacts. This reading
 *   distributes resources to present vulnerable populations and incumbent
 *   interests (energy infrastructure, adaptation industry), while deferring
 *   the compounding costs of higher warming to future generations and
 *   low-capacity regions. The reading is structurally CONTESTED: mitigation
 *   advocates argue the framing itself manufactures infeasibility by
 *   suppressing transition pathways; degrowth advocates argue adaptation
 *   becomes impossible without questioning growth. This is one reading of the
 *   kernel climate_harm_prevention, not a synthesis or consensus. The
 *   constraint story captures the adaptation-priority reading's internal
 *   structure as policy: who benefits, who bears costs, what enforcement is
 *   required to maintain it.
 *
 * KEY AGENTS:
 *   - present_vulnerable_populations_in_high_adaptation_capacity_regions: primary present beneficiaries of front-loaded resilience investment
 *   - future_generations: primary victims, bearing the cumulative harm of locked-in higher warming
 *   - low_latitude_regions and low_adaptation_capacity_countries: dual victims (high warming, insufficient adaptation finance) and excluded from negotiation
 *   - climate_adaptation_industry and incumbent_energy_infrastructure: beneficiaries of the frame that treats mitigation as infeasible and adaptation as profitable
 *   - wealthy_high_capacity_nations: agenda-setters, framing feasibility and legitimacy
 *   - degrowth_advocates and global_climate_science: excluded or marginalized voices whose readings would reshape the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.71).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Climate Adaptation-First Priority (Higher Warming Trajectory Accepted)").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '965af751-32a0-49d2-a500-ed7aa121efcf').
narrative_ontology:cs_kernel_codification('965af751-32a0-49d2-a500-ed7aa121efcf', distributed).
narrative_ontology:cs_authority_grounding('965af751-32a0-49d2-a500-ed7aa121efcf', extraction).
narrative_ontology:cs_reading_relation('965af751-32a0-49d2-a500-ed7aa121efcf', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('965af751-32a0-49d2-a500-ed7aa121efcf', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('965af751-32a0-49d2-a500-ed7aa121efcf', foundational, growth_is_necessary_constraint).
narrative_ontology:cs_axiom_status(growth_is_necessary_constraint, holdable).
narrative_ontology:cs_axiom_grounding('965af751-32a0-49d2-a500-ed7aa121efcf', growth_is_necessary_constraint, empirically_contingent).
narrative_ontology:cs_axiom('965af751-32a0-49d2-a500-ed7aa121efcf', foundational, intergenerational_harm_deferral_acceptable).
narrative_ontology:cs_axiom_status(intergenerational_harm_deferral_acceptable, holdable).
narrative_ontology:cs_axiom_grounding('965af751-32a0-49d2-a500-ed7aa121efcf', intergenerational_harm_deferral_acceptable, instrumental).
narrative_ontology:cs_reference_frame('965af751-32a0-49d2-a500-ed7aa121efcf', growth_compatible_climate_response).
narrative_ontology:cs_drift_state('965af751-32a0-49d2-a500-ed7aa121efcf', contemporary_implementation_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('965af751-32a0-49d2-a500-ed7aa121efcf', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations_in_high_adaptation_capacity_regions).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, climate_adaptation_industry).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, incumbent_energy_infrastructure).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_latitude_regions).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_countries).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, ecosystems_dependent_on_temperature_stability).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint distributes present resources disproportionately toward high-capacity populations while imposing future harms on low-capacity populations and future generations. The distribution is not driven by coordination necessity alone — it is driven by power asymmetry and frame control. Suppression is slightly higher (0.71) because maintaining the adaptation-priority reading requires suppressing mitigation and degrowth framings: characterizing them as economically impossible, politically radical, or unachievable. Theater is moderate-to-high (0.42 at plateau) because adaptation spending has genuine harm-reduction effects (seawalls do protect), but the framing obscures that adaptation is insufficient at the warming trajectory it accepts — the theater is the claimed sufficiency of adaptation as a response. Accessibility_collapse varies sharply by level: high for individuals (faced with immediate climate risk, adaptation appears urgent and necessary), lower for the structural level (the choice between adaptation/mitigation/degrowth remains open, visible to analytical seats). Stakes_inflation is high at individual and organizational levels (present risk rises continuously, justifying more spending) but lower at structural levels (the system-level choice between readings is contested). Suppression is highest at organizational level (incumbent interests and wealthy-nation governments actively suppress alternative framings) and lower at structural level (system-level contestation persists despite suppression). Resistance declines over time as the frame becomes normalized and participants internalize the narrative that mitigation is infeasible — resistance starts high from climate science and degrowth movements, erodes as the frame becomes policy consensus.
 *
 * PERSPECTIVAL GAP:
 *   Wealthy-nation agenda-setters perceive the constraint as solving a genuine coordination problem: how to deliver climate safety when aggregate mitigation is politically blocked. They frame adaptation as pragmatic and feasible. Present vulnerable populations perceive protection and near-term relief. Future generations and low-capacity countries perceive a structure that extracts present resources and imposes residual harms they cannot escape. The engine should compute different types across these seats: agenda-setters may see rope (genuine coordination); victims see snare (extraction hiding behind feasibility claims); excluded advocates see the frame itself as extractive (suppressing legitimate alternatives). The structural divergence is not in what each seat experiences, but in their power to set the frame that defines what is legitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy, high-capacity nations are the agenda-setters (d near 0.2, beneficiaries or symmetric). The adaptation industry and incumbent energy are powerful beneficiaries (d near 0.15–0.25). Present vulnerable populations in high-capacity regions are beneficiaries, but trapped and dependent on the resource allocation the constraint controls (d around 0.35–0.45). Future generations and low-capacity countries are trapped targets — their exit options are zero (cannot refuse to inherit the climate; cannot exit their regions), and they bear both the higher warming (from deferred mitigation) and the adaptation insufficiency (d near 0.85–0.95). Ecosystem victims have zero agency (d = 1.0 as targets, though non-agent). The directionality derivation should show sharp asymmetry: powerful beneficiaries have mobile exit and arbitrage options; trapped victims have none.
 *
 * MANDATROPHY ANALYSIS:
 *   The adaptation-priority reading was founded to solve a real problem: mitigation faced political blockage, and present populations faced immediate climate risk. The founding problem remains live (mitigation is politically difficult, present vulnerability is real). But the constraint has evolved to suppress mitigation alternatives rather than merely accepting their temporary infeasibility. The frame now actively enforces the narrative that mitigation is impossible, not merely politically difficult in the short term. Mandatrophy is emerging: the constraint's original coordination function (how do we protect present people given mitigation barriers?) has been replaced by a pure extraction function (how do we extract resources from future generations and defer their costs?). The constraint should be evaluated for mandatrophy resolution: either (a) lift the suppression on mitigation and degrowth framings, allowing them to compete, and reclassify as rope or scaffold (temporary adaptation while mitigation transitions), or (b) accept the extraction explicitly and reclassify as snare. Continuing to present it as a necessary coordination response while it actively suppresses alternatives is the mandatrophic position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feasibility_vs_legitimacy_drift,
    'Is mitigation genuinely economically or politically infeasible, or has the adaptation-priority frame itself created conditions that make mitigation appear infeasible by suppressing alternative framings?',
    'Comparative policy analysis: jurisdictions where mitigation is authorized and funded (Nordic countries, Costa Rica, parts of Germany) show faster emissions reduction. Cross-jurisdiction comparison reveals that infeasibility is variable by regime, not structural. Alternatively, a historical counterfactual: what would carbon pricing, fossil fuel phase-out, or energy transition look like if authorized equally to adaptation spending?',
    'If mitigation''s apparent infeasibility is constructed by frame suppression, the constraint is enforcing a particular distribution by narrative. If genuinely infeasible, the constraint represents a legitimate optimization within resource constraints. Classification could shift from tangled_rope (hybrid coordination/extraction) toward rope (genuine coordination problem with real tradeoffs) or toward snare (pure extraction hiding behind feasibility claims).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(feasibility_vs_legitimacy_drift, conceptual, 'Whether infeasibility is structural or constructed by the adaptation-priority frame itself.').

omega_variable(
    intergenerational_discount_rate,
    'What discount rate is implicit in the choice to accept higher warming and defer mitigation costs to future generations? Is that rate justified by economic theory, or is it a value choice being laundered as technical optimization?',
    'Ethical analysis of discount rates: revealed preference in adaptation spending suggests implicit intergenerational discount rates of 3–5% (treating future harm as less salient than present spending). Discounted-utility models find rates above 2% require justifying why future people''s welfare matters less than present people''s. Alternatively: ask whether the same discount logic applied to present populations would be deemed acceptable (accepting harm to one group now to benefit another) — if not, the asymmetry suggests value choices, not technical necessity.',
    'High discount rates justify extraction from future generations; low rates favor mitigation. If the rate is revealed to be a value choice disguised as technical fact, the extraction becomes visible and the classification cannot rest on feasibility alone. The constraint''s legitimacy would depend on explicit intergenerational justice reasoning, not on economic inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, empirical, 'Implicit intergenerational discount rate embedded in adaptation-priority allocation.').

omega_variable(
    adaptation_resilience_limits,
    'At what warming trajectory do adaptation expenditures become insufficient to maintain livability and economic function? Is there a hard limit, or is it a gradual degradation?',
    'Climate modeling and economics of adaptation: IPCC synthesis reports document tipping points and adaptation limits (small island nations, Sahel agriculture, coral reef ecosystems). At 2.5–3.5°C, major tipping points are crossed and adaptation cannot restore prior function. Quantify the residual harm that adaptation cannot offset and the populations bearing it.',
    'If adaptation''s scope is fundamentally bounded (cannot protect below sea-level cities from 2m sea-level rise, cannot maintain monoculture agriculture above temperature thresholds), then accepting higher warming trades present adaptation for future losses that adaptation itself cannot remediate. The constraint would be classifiable as snare-adjacent: presenting a solution (adaptation) while knowing its limits, extracting present resources and deferring irreversible harms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_resilience_limits, empirical, 'Physical and economic limits of climate adaptation under different warming scenarios.').

omega_variable(
    kernel_reading_structural_divergence,
    'Does the choice between adaptation_priority, mitigation_priority, and degrowth readings hinge on empirical claims about feasibility, or on foundational disagreements about growth, justice, and intergenerational obligation?',
    'Logical decomposition: if feasibility were the only variable, all parties would converge on the most-feasible path once empirical data settled. But they do not — degrowth advocates claim mitigation is feasible if growth is questioned; mitigation advocates claim both are feasible if technology transitions; adaptation advocates claim transition is infeasible and adaptation is optimal. The persistence of disagreement despite data suggests axiomatically distinct framings (growth is necessary vs. contingent; intergenerational obligation is symmetrical vs. asymmetrical; harm-deferral is legitimate vs. illegitimate). These are kernel readings, not empirical disagreements.',
    'If the divergence is axiomatically deep, then no single empirical finding will resolve it. The adaptation-priority frame''s authority rests on axioms (growth is necessary, present people''s welfare takes priority, adaptation is a legitimate response to constrained mitigation) that the other readings reject. Classification cannot adjudicate which axiom is correct — it can only note the reading is contested. The extraction visible in the adaptation-priority frame (harm deferral, resource allocation to present populations, suppression of alternative framings) becomes more salient when recognized as axiomatically driven, not empirically determined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_divergence, conceptual, 'Whether kernel divergence is empirical (feasibility) or axiomatically foundational (value choices about growth, justice, and obligation).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t5, climate_harm_prevention__adaptation_priority, theater_ratio, 5, 0.31).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__adaptation_priority, theater_ratio, 10, 0.35).
narrative_ontology:measurement(clim_tr_t15, climate_harm_prevention__adaptation_priority, theater_ratio, 15, 0.38).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__adaptation_priority, theater_ratio, 20, 0.41).
narrative_ontology:measurement(clim_tr_t25, climate_harm_prevention__adaptation_priority, theater_ratio, 25, 0.42).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.42).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__adaptation_priority, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__adaptation_priority, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__adaptation_priority, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__adaptation_priority, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__adaptation_priority, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(clim_be_t25, climate_harm_prevention__adaptation_priority, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__adaptation_priority, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__adaptation_priority, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__adaptation_priority, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__adaptation_priority, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__adaptation_priority, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(clim_su_t25, climate_harm_prevention__adaptation_priority, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__adaptation_priority, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, fossil_fuel_incumbent_power).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, intergenerational_harm_deferral).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, adaptation_finance_allocation).

% DUAL FORMULATION NOTE:
% The climate_harm_prevention kernel has three structural readings: adaptation_priority (this story), mitigation_priority, and degrowth_reading. They are not versions of a single constraint — they are structurally distinct instantiations from the same kernel, with different beneficiary/victim structures, different ε values, and different types. adaptation_priority has high extractiveness (0.68) because it concentrates present benefits and defers future harms. mitigation_priority has lower extractiveness (acceptance of transition costs is distributed across time). degrowth_reading has highest political suppression (0.85+) because it challenges growth itself. All three readings affect the same downstream constraints (fossil_fuel_incumbent_power strengthens if adaptation-priority dominates; intergenerational_harm_deferral is instantiated by adaptation-priority). Network links show dependency, not endorsement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, powerful, 0.22).
constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
