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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Rapid Decarbonization Mandate (Mitigation Priority Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading frames rapid decarbonization as an
 *   ethical obligation grounded in intergenerational justice: current-era
 *   emissions create climate harms that accumulate for centuries, imposing
 *   risks on future generations who had no voice in the decision. Under this
 *   reading, current generations are obligated to reduce emissions sharply
 *   (to 1.5–2°C warming) even if the transition costs are substantial and
 *   concentrated. The constraint operates as tangled rope: it coordinates the
 *   solution to a genuine collective-action problem (atmospheric carbon
 *   externality) while enforcing highly asymmetric cost distribution —
 *   between current and future generations, between Global North industrial
 *   economies and Global South vulnerable populations, between fossil fuel
 *   workers and beneficiaries of low-carbon energy systems. The claim/metric
 *   gap is intentional: the constraint is CLAIMED as foundational ethical
 *   obligation (grounded in physics and justice), while the authored metrics
 *   describe substantially extractive operation with rising suppression — the
 *   engine measures whether the proclaimed ethical obligation actually
 *   operates as pure coordination or masks extraction.
 *
 * KEY AGENTS:
 *   - Future generations (primary beneficiaries, powerless, civilizational horizon): inherit a planet whose climate state is determined by current emissions; benefit from rapid decarbonization via reduced warming damage.
 *   - Climate-vulnerable populations (beneficiaries + constrained payers, organized, generational horizon): face current climate impacts; advocate for aggressive mitigation but lack political power to enforce their preferred timelines.
 *   - Current-generation fossil fuel workers (payers, identity-locked, biographical horizon): face job displacement, wage pressure, and community disruption during energy transition; their exit options are constrained by professional identity fusion with fossil sectors.
 *   - Fossil fuel industry (victims/payers, institutional, biographical horizon): faces stranded-asset writedowns, regulatory prohibition, and liability exposure; named as extraction target by the constraint.
 *   - Global North industrial base (payers, institutional, generational horizon): absorbs capital reallocation and infrastructure replacement costs; faces domestic political resistance to rapid transition.
 *   - Global South populations (beneficiaries + constrained payers, moderate power, generational horizon): benefit from reduced warming but face constrained development pathways and technology dependency.
 *   - Climate scientists & advocacy coalition (agenda-setters, analytical power): articulate and defend the mitigation-priority reading; command epistemic authority but limited direct enforcement power.
 *   - Nation-states & multilateral bodies (agenda-setters, institutional): enforce the constraint through climate agreements, regulations, and carbon pricing.
 *   - Adaptation-priority coalition (excluded): argues for accepting higher warming and investing in resilience; structurally excluded from mitigation-priority decision forums.
 *   - Degrowth coalition (excluded, trapped): argues that decarbonization within growth economics is impossible; politically infeasible within market economies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.67).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.58).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.67).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Rapid Decarbonization Mandate (Mitigation Priority Reading)").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '9a189273-f5ab-4eee-8a08-32263e14e931').
narrative_ontology:cs_kernel_codification('9a189273-f5ab-4eee-8a08-32263e14e931', distributed).
narrative_ontology:cs_authority_grounding('9a189273-f5ab-4eee-8a08-32263e14e931', expertise).
narrative_ontology:cs_interpretation_layer_present('9a189273-f5ab-4eee-8a08-32263e14e931').
narrative_ontology:cs_reading_relation('9a189273-f5ab-4eee-8a08-32263e14e931', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('9a189273-f5ab-4eee-8a08-32263e14e931', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('9a189273-f5ab-4eee-8a08-32263e14e931', foundational, rapid_decarbonization_feasible_within_markets).
narrative_ontology:cs_axiom_status(rapid_decarbonization_feasible_within_markets, holdable).
narrative_ontology:cs_axiom_grounding('9a189273-f5ab-4eee-8a08-32263e14e931', rapid_decarbonization_feasible_within_markets, empirically_contingent).
narrative_ontology:cs_axiom('9a189273-f5ab-4eee-8a08-32263e14e931', foundational, intergenerational_justice_prioritizes_warming_minimization).
narrative_ontology:cs_axiom_status(intergenerational_justice_prioritizes_warming_minimization, holdable).
narrative_ontology:cs_axiom_grounding('9a189273-f5ab-4eee-8a08-32263e14e931', intergenerational_justice_prioritizes_warming_minimization, deontological).
narrative_ontology:cs_reference_frame('9a189273-f5ab-4eee-8a08-32263e14e931', pre_industrial_climate_stability).
narrative_ontology:cs_drift_state('9a189273-f5ab-4eee-8a08-32263e14e931', contemporary_post_paris_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9a189273-f5ab-4eee-8a08-32263e14e931', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, current_generation_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_industrial_base).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).

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
 *   Base extractiveness of 0.67 reflects the substantial current-era costs (transition burdens, stranded assets, foregone growth) borne by identified payers to generate benefits (reduced warming damage) that accrue to the future. The constraint is not pure coordination — the future generations who benefit most cannot participate in negotiating terms. Suppression of 0.58 reflects active regulatory enforcement (carbon pricing, emissions limits, technology mandates) required to suppress alternative energy pathways and restrict the fossil fuel industry's continued operation. Suppression is lower than extractiveness because the constraint's legitimacy rests on genuine environmental physics (carbon accumulation, warming trajectory) and genuine beneficiaries (the future, vulnerable populations today), not on pure coercion. Theater ratio of 0.42 reflects significant performative activity: countries announce climate targets they do not meet, corporates adopt net-zero pledges while expanding fossil fuel investments, and climate finance commitments are repeatedly delayed. The theater increases over time (0.25 → 0.45 over the first 30 years) as compliance becomes politically difficult and states substitute announcement for action. Accessibility collapse of 0.72 reflects that the constraint makes alternative energy pathways increasingly unavailable: regulatory prohibition closes fossil fuel investment routes, carbon pricing makes traditional energy expensive, and renewable deployment becomes mandatory. But alternatives are not completely collapsed — technological innovation, nuclear power, and adaptation remain possible, preventing the near-total collapse (0.85+) that would characterize a genuine natural law. Resistance of 0.71 reflects substantial opposition from fossil fuel interests, industrial workers, and nations fearing competitive disadvantage if they decarbonize faster than rivals. The temporal series show base_extractiveness rising through 2030 (peak transition phase) then stabilizing as the renewable infrastructure matures and transition costs decline; suppression_requirement follows a similar curve; theater_ratio stays elevated because the political difficulty of maintaining the constraint persists even as its functional justification strengthens. Measurements across the coercion grid differentiate that suppression intensity is highest at the structural level (global agreements, regulatory frameworks) and lower at the individual level (market-driven choices, voluntary adoption), while resistance is highest at the individual and organizational level (workers, incumbent industries) and lower at the structural level (no alternative global-commons regime is available).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (climate scientists, nation-states) and the beneficiary seats (future generations, vulnerable populations) experience this constraint as ethically binding coordination: the only adequate response to known, accumulating physical risk. The payer seats (fossil fuel workers, industrial base, current generation broadly) experience the same constraint as asymmetric extraction: costs are immediate, concentrated, and certain, while benefits are distant, diffuse, and uncertain. From the powerless future-generation seat, the constraint is binding and insufficient — current mitigation efforts track well below the pace required by 1.5°C pathways. From the fossil-fuel-worker seat, the constraint is coercive and unjust — they are being sacrificed for risks they did not create and for benefits they will not receive. The engine computes these divergences from the structural data (power differentials, exit options, time horizons, directionality) without needing adjudication. The claim/metric independence is critical here: the constraint is claimed as ethically foundational, but it operates with measurably extractive, suppressive characteristics. That divergence is exactly where the Deferential Realism framework performs its diagnostic function.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations have d ≈ 0.0 (full beneficiaries): the constraint exists to reduce their climate exposure; they bear no costs of the transition; their exit options are zero (trapped — they cannot avoid the future they inherit). Climate-vulnerable populations have d ≈ 0.2–0.3 (net beneficiaries but with participation costs): they benefit from reduced warming but face constrained political voice and development-path restrictions. Current-generation workers in fossil sectors have d ≈ 0.85 (near full targets): they bear concentrated, immediate costs (job loss, wage pressure, identity disruption); the constraint enforces their transition without compensating them fully; their exit options are identity-locked (professional identity fused with fossil work). The fossil fuel industry has d ≈ 0.9 (full target): the constraint's enforcement machinery names them as the primary extraction object; stranded assets represent direct wealth loss; no compensatory mechanism exists. Global North industrial base has d ≈ 0.7 (strong target): capital reallocation and infrastructure replacement are mandatory; they have some exit flexibility (automation, outsourcing) but constrained choice. Green technology and finance sector has d ≈ 0.1 (beneficiary): they capture rents from renewable deployment; the constraint drives resources toward their solutions. The directionality derivation from beneficiary/victim declarations + exit options is unambiguous here — beneficiaries with trapped or constrained exit get low d; victims with constrained or identity-locked exit get high d; the temporal misalignment (future vs. present) is captured by the time-horizon axis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is the atmospheric carbon externality: emissions accumulate on centennial timescales, deferring harms to the future. The founding problem is structurally live — atmospheric carbon is accumulating, warming is accelerating, and the harms are manifesting. The constraint prevents mandatrophy by keeping the founding problem front-and-center: any relaxation of mitigation efforts would directly increase future warming risk. However, mandatrophy could occur if: (1) adaptation technology advances so rapidly that future-generation welfare is nearly unaffected by warming (founding problem becomes moot), or (2) the constraint is maintained theatrically — countries announce targets but do not enforce them, and emissions continue on baseline trajectories, eventually making the constraint's stated purpose impossible to achieve. The measured theater_ratio rising to 0.45 signals some drift toward this second mode: enforcement is becoming performative relative to functional action. The vanishing point is t=50+ where theater_ratio stabilizes and suppression_requirement begins to decline — this projects a scenario where decarbonization is substantially achieved, the constraint's primary function (preventing dangerous warming) is accomplished, and the constraint begins to dematerialize into regulatory legacy and historical norm. This is not mandatrophy (the constraint successfully solved its problem) but rather obsolescence by success.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_discount_rate_ambiguity,
    'What discount rate appropriately reflects the moral weight of future generations'' welfare relative to current costs? Does standard economic discounting (3–5% per annum) ethically undervalue future climate harm, or do empirical uncertainties about future technological capability justify moderate discounting?',
    'Philosophical consensus on intergenerational justice principles; empirical evidence on technological decarbonization trajectories and adaptation feasibility; economic models that explicitly weight future welfare without standard discounting.',
    'A zero or near-zero discount rate would justify even extremely costly current mitigation; high discount rates would support the adaptation-priority reading. The choice of discount rate determines whether rapid decarbonization is a binding moral obligation or one option among several.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discount_rate_ambiguity, preference, 'The normative weighting of future generations'' interests relative to current transition costs.').

omega_variable(
    stranded_asset_liability_framing,
    'Are fossil fuel industry losses (stranded assets, regulatory prohibition) a legitimate extraction target (payment for climate damage and historical emissions) or an unjust taking (penalty for lawful past activity)?',
    'Legal precedent on retroactive liability; political resolution of climate reparations frameworks; empirical analysis of who bore the profits from historical emissions.',
    'If framed as legitimate extraction, the constraint''s classification as tangled_rope (coordination + asymmetric extraction) holds and the fossil fuel industry properly occupies the victim set. If framed as unjust taking, the constraint would reclassify as snare (pure extraction justified by false coordination claims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_liability_framing, preference, 'Whether stranded-asset losses are compensation or punishment.').

omega_variable(
    carbon_budget_vs_adaptation_trade,
    'At what warming level do marginal mitigation costs exceed marginal adaptation benefits? Is 1.5°C the appropriate carbon budget, or do higher warming levels (2°C, 2.5°C) offer a better cost-benefit trade-off?',
    'Integrated assessment models with explicit cost-benefit analysis; real-world adaptation outcomes from current climate impacts; technological breakthroughs in adaptation infrastructure.',
    'If 1.5°C is the correct threshold, rapid decarbonization is strictly necessary. If adaptation becomes cost-effective at 2–2.5°C, the adaptation-priority reading gains empirical ground and the mitigation mandate''s urgency diminishes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_budget_vs_adaptation_trade, empirical, 'The optimal carbon budget under cost-benefit analysis.').

omega_variable(
    identity_lock_vs_structural_exit_ambiguity,
    'For current-generation fossil fuel workers, is the measured identity_lock a psychological/cultural phenomenon that persists after job displacement, or a reflection of genuine structural inability to exit (regional economic dependence, credential specificity, geographic isolation)?',
    'Post-transition outcome data from coal-mining regions and refinery communities; studies of occupational identity persistence after career disruption; analysis of actual geographic mobility and retraining success rates.',
    'If identity_lock is primarily psychological, transition support and retraining are sufficient policy responses. If structural, the constraint''s suppression is higher than authored and the extraction weight on workers is more severe than the current directionality suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_exit_ambiguity, empirical, 'Whether fossil fuel worker exit constraints are structural or internalized.').

omega_variable(
    reading_committer_frame,
    'This constraint instantiates the MITIGATION_PRIORITY reading of the climate_response_obligation kernel, which contains three structurally distinct readings: mitigation_priority (this one), adaptation_priority, and degrowth_reading. Does the mitigation-priority framing of climate ethics (intergenerational justice via emissions reduction) foreclose the adaptation reading, or do they represent genuinely coexistent policy positions?',
    'Analysis of whether the two readings'' core premises can coexist in a single commitment framework; examination of whether accepting ''some adaptation is necessary'' logically requires rejecting ''rapid mitigation is necessary.''',
    'If mitigation forecloses adaptation, the mitigation reading is the unique ethically defensible position. If they coexist, the mitigation reading is one among live alternatives, and the constraint''s enforcement is contestable. If adaptation influences mitigation by constraining resource availability, both readings persist but the tradeoff becomes explicit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_frame, conceptual, 'Kernel committer structure: relationship between this reading and sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t6, climate_response_obligation__mitigation_priority, theater_ratio, 6, 0.31).
narrative_ontology:measurement(clim_tr_t13, climate_response_obligation__mitigation_priority, theater_ratio, 13, 0.38).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.43).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.45).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__mitigation_priority, theater_ratio, 40, 0.44).
narrative_ontology:measurement(clim_tr_t50, climate_response_obligation__mitigation_priority, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t6, climate_response_obligation__mitigation_priority, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(clim_be_t13, climate_response_obligation__mitigation_priority, base_extractiveness, 13, 0.61).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__mitigation_priority, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(clim_be_t50, climate_response_obligation__mitigation_priority, base_extractiveness, 50, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t6, climate_response_obligation__mitigation_priority, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(clim_su_t13, climate_response_obligation__mitigation_priority, suppression_requirement, 13, 0.51).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__mitigation_priority, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(clim_su_t50, climate_response_obligation__mitigation_priority, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% The climate_response_obligation kernel decomposes into three structurally distinct constraint stories: mitigation_priority (this story, ε≈0.67, tangled_rope), adaptation_priority (acceptance of 2–3°C warming, ε≈0.45, rope-like coordination), and degrowth_reading (critique of growth economics, ε≈0.71, snare-like extraction). The ε values differ because the readings make different structural claims about what solves the founding problem and who bears costs. All three readings reference the same kernel (the climate-response-obligation commitment), but each instantiates a different constraint. Sibling stories must be linked via this network array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__mitigation_priority, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
