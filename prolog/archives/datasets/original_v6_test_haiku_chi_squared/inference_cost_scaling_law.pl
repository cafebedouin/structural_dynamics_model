% ============================================================================
% CONSTRAINT STORY: inference_cost_scaling_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inference_cost_scaling_law, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: inference_cost_scaling_law
 *   human_readable: The Computational Tollgate: Inference Cost Scaling Law
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The inference cost scaling law represents a structural extraction
 *   mechanism in AI economics where the cost of running advanced models
 *   (LLMs, vision transformers, multimodal systems) scales at rates that
 *   prevent small-scale actors from accessing frontier capability in
 *   real-time. As model sizes and training costs have increased, inference
 *   pricing has followed, creating a tiered market where (1) hyperscale
 *   compute providers extract economic rent from everyone downstream, (2)
 *   frontier model labs (OpenAI, Anthropic, Google) maintain artificial
 *   scarcity through API rate-limiting and tiered pricing, (3) small-scale
 *   researchers and startups are locked into constrained tiers or forced to
 *   build on deprecated open-source models, and (4) developing-economy
 *   innovators are excluded entirely. The constraint is not that inference
 *   requires computational resources—it is that capital concentration has
 *   made frontier access a tollgate, and the cost structure enforces this
 *   tollgate through market leverage rather than algorithmic necessity. This
 *   story identifies the constraint as a Snare from most structural
 *   positions, but reveals competing perspectives: hyperscale providers and
 *   frontier labs see coordination (Rope), open-source coalitions see a
 *   hybrid with a visible exit path (Tangled Rope with Scaffold features),
 *   and powerful regulated actors might impose a sunset via governance
 *   intervention (Scaffold).
 *
 * KEY AGENTS:
 *   - Hyperscale Compute Providers (Google Cloud TPUs, AWS SageMaker, Azure OpenAI): Primary beneficiary (institutional/arbitrage) — capture margin on every inference call; control commodity price floor
 *   - Frontier Model Labs (OpenAI, Anthropic, Google DeepMind): Primary beneficiary (institutional/immediate) — maintain pricing power through API rate-limiting and capability gatekeeping
 *   - Small-Scale Researchers (academic labs, independent researchers): Primary victim (powerless/trapped) — locked out of frontier access; confined to lower-capability open-source models
 *   - Emerging AI Startups: Secondary victim (moderate/constrained) — face runaway inference costs; forced to build on legacy models or negotiate unfavorable enterprise deals
 *   - Developing Economy Innovators: Tertiary victim (powerless/trapped) — inability to pay in hard currency excludes entire regions from frontier AI access
 *   - Open Source Model Coalition (Hugging Face, Meta, community fine-tuning): Organized actors (organized/constrained) — building alternative pathways; constrained by capital intensity but achieving cost reduction through quantization and distillation
 *   - Regulatory Actors (potential EU/US governance): Powerful actors (powerful/mobile) — potential imposers of sunset; could mandate open access or subsidize frontier research access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inference_cost_scaling_law, 0.58).
domain_priors:suppression_score(inference_cost_scaling_law, 0.68).
domain_priors:theater_ratio(inference_cost_scaling_law, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inference_cost_scaling_law, extractiveness, 0.58).
narrative_ontology:constraint_metric(inference_cost_scaling_law, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(inference_cost_scaling_law, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inference_cost_scaling_law, snare).
narrative_ontology:human_readable(inference_cost_scaling_law, "The Computational Tollgate: Inference Cost Scaling Law").
narrative_ontology:topic_domain(inference_cost_scaling_law, "technological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inference_cost_scaling_law, hyperscale_compute_providers).
narrative_ontology:constraint_beneficiary(inference_cost_scaling_law, frontier_model_labs).
narrative_ontology:constraint_victim(inference_cost_scaling_law, small_scale_researchers).
narrative_ontology:constraint_victim(inference_cost_scaling_law, emerging_ai_startups).
narrative_ontology:constraint_victim(inference_cost_scaling_law, developing_economy_innovators).
narrative_ontology:constraint_victim(inference_cost_scaling_law, open_source_model_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL-SCALE RESEARCHER (SNARE) — Cannot afford frontier inference costs; trapped in lower-capability tiers. No alternative to proprietary APIs for access to state-of-art models. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.95. Maximal extraction from structural position.
constraint_indexing:constraint_classification(inference_cost_scaling_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING STARTUP (SNARE) — Can access inference APIs but faces massive scaling costs. Constrained by venture burn rate and inability to negotiate volume pricing. Exit paths (building own inference infrastructure) require capital beyond startup reach. d≈0.85, f(d)≈1.20, σ=1.0 → χ≈0.70. High extraction.
constraint_indexing:constraint_classification(inference_cost_scaling_law, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HYPERSCALE COMPUTE PROVIDER (ROPE) — Experiences the scaling law as a coordination mechanism: higher inference costs translate directly to revenue capture from the entire ecosystem. Capital leverage and unit economics produce net beneficiary status. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Negative effective extraction = pure beneficiary.
constraint_indexing:constraint_classification(inference_cost_scaling_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE MODEL COALITION (TANGLED ROPE) — Coordinating actors (Hugging Face, Meta's open releases, community fine-tuning networks) are building an alternative inference pathway that reduces computational costs through model distillation, quantization, and edge deployment. Benefits from coordination (shared knowledge, reduced inference costs through optimization). Constrained by capital intensity of model research. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45. Mixed extraction.
constraint_indexing:constraint_classification(inference_cost_scaling_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FRONTIER MODEL LAB COST JUSTIFICATION (PITON) — Internal narratives about 'training costs justify inference pricing' or 'compute scarcity forces allocation mechanisms' have atrophied: actual inference APIs are highly profitable despite marginal cost being orders of magnitude below published pricing. The cost story is maintained through institutional inertia (legacy pricing models) and theatrical scarcity narratives. theater_ratio=0.38 is borderline; the piton gate requires ≥0.70, so this perspective does not formally classify as piton, but the institutional degradation is observable.
constraint_indexing:constraint_classification(inference_cost_scaling_law, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATED FRONTIER (SCAFFOLD) — If regulatory intervention (EU AI Act, potential US compute governance) mandates open access to frontier models or subsidizes inference costs for public research, the constraint becomes temporary. Powerful regulatory actors see this as a coordination problem with a sunset: once open-access models mature and cost curves flatten through competition, the tollgate loses extraction capacity. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.25. Lower effective extraction because exit path is visible to powerful actors.
constraint_indexing:constraint_classification(inference_cost_scaling_law, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational scope, the inference cost scaling law is not a natural law but a contingent market structure enforced by capital concentration. The constraint extracts across all scales: small-scale researchers are locked out entirely (d≈0.92), moderate actors are squeezed (d≈0.85), powerful actors who own compute negotiate freely (d≈0.08). The constraint's structural property is asymmetric exit options across power levels, not algorithmic necessity. d≈0.82, f(d)≈1.18, σ=1.2 → χ≈0.82.
constraint_indexing:constraint_classification(inference_cost_scaling_law, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inference_cost_scaling_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inference_cost_scaling_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inference_cost_scaling_law, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inference_cost_scaling_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inference_cost_scaling_law, TR),
    TR >= 0.70.

:- end_tests(inference_cost_scaling_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximum. The constraint extracts significantly from small-scale actors (they are locked out) and moderate actors (they face unsustainable scaling costs). However, extractiveness is not at snare ceiling (≥0.66) because: (1) open-source alternatives exist and are improving, providing a partial exit path, and (2) some frontier labs have released models via APIs at reduced margins. The trajectory shows growth from 0.32 (early 2023, when open-source models were closer in capability) to 0.58 (current, reflecting widening gap). Suppression (0.68): High. Multiple barriers prevent exit: (a) algorithmic barrier—frontier models are genuinely more capable, and replicating capability requires similar capital investment; (b) economic barrier—building inference infrastructure costs $100M+; (c) institutional barrier—frontier labs' data and training know-how are proprietary; (d) temporal barrier—open-source models lag by 6-12 months. Theater ratio (0.38): Moderate. The cost story has theatrical elements (scarcity narratives, 'training cost allocation' justifications) but is not purely performative—actual inference costs do scale with model size and request volume. The theater is rising (0.22→0.38) as the gap between published marginal costs and API pricing has widened; the functional cost is real, but the extraction ratio is increasingly unmoored from underlying compute economics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a stark perspectival gulf. The hyperscale provider sees pure coordination (Rope, d≈0.08): inference pricing is a mechanism for allocating scarce compute resources. The small-scale researcher sees pure extraction (Snare, d≈0.92): they are locked out entirely. The emerging startup sees extraction with constrained options (Snare, d≈0.85): they can access frontier models but face unsustainable costs. The open-source coalition sees a hybrid (Tangled Rope, d≈0.50): they benefit from coordination effects (shared knowledge, distributed optimization) but are constrained by capital requirements. The regulatory actor sees a temporary problem (Scaffold, d≈0.45): they have the power to impose a sunset via mandates. The analytical observer sees pure extraction (Snare, d≈0.82): the constraint's structure is asymmetric exit options enforced by capital concentration, not by algorithmic necessity. The gap widens as actors move from beneficiary to victim: beneficiaries experience the constraint as infrastructure; victims experience it as a tollgate.
 *
 * DIRECTIONALITY LOGIC:
 *   Hyperscale provider: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. They control the commodity (compute) and set the price. Frontier lab: Beneficiary + immediate → d≈0.10, f(d)≈-0.08. Net beneficiary despite higher time pressure. Small-scale researcher: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. No alternatives; complete dependence on providers' terms. Emerging startup: Victim + constrained → d≈0.85, f(d)≈1.20. High extraction. Can access frontier models but scaling costs force suboptimal product decisions (lower-capability alternatives, reduced model switching). Open-source coalition: Organized + constrained → d≈0.50, f(d)≈0.65. Mixed extraction. Coalition has collective agency (can optimize inference) but constrained by capital barriers to closing the capability gap. Regulatory actor: Powerful + mobile → d≈0.45, f(d)≈0.45. Low effective extraction from their perspective; they have exit path (regulation) and organizational capacity to impose it. Analytical observer: Analytical → d≈0.82, f(d)≈1.18. The observer sees the constraint as enforced asymmetry: exit options diverge based on capital availability, not on any property of inference itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is NOT a law of computation or scarcity. The apparent conflict between 'inference requires compute (true)' and 'inference is extractive (true)' is false: both propositions are true but address different domains. Inference cost SCALING (more tokens → more cost) is algorithmic necessity. Inference cost LEVELS (the absolute price per token) is market structure. The snare classification applies to the level and its distribution across actors, not to the scaling relationship itself. Beneficiaries see the constraint as coordinating access to scarce resources (Rope truthfully applied to the coordination function). Victims see it as a tollgate enforcing extraction (Snare truthfully applied to the asymmetric exit structure). The mandatrophy resolution: the constraint is simultaneously a real coordination mechanism (limiting access to prevent tragedy of the commons in compute) AND an extraction mechanism (concentrating that access limitation to benefit capital holders). It is a Tangled Rope viewed neutrally, becomes a Snare from the victim's position, becomes a Rope from the beneficiary's position. The constraint's type depends on the observer's structural relationship to the exit options, not on the truth value of either framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_necessity_vs_market_structure,
    'Is inference cost scaling an immutable algorithmic property (more parameters = more compute required) or a contingent market structure (artificial scarcity + pricing power)?',
    'Compare inference cost per FLOP across competing providers; track cost curves after market entry or regulatory intervention; analyze relationship between model parameter count and minimum viable inference cost',
    'If algorithmic: constraint is closer to Mountain (ε≈0.15). If market structure: constraint is Snare (ε≈0.58, current). Resolution determines whether open-source alternatives can actually undercut or only serve lower-capability tiers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_necessity_vs_market_structure, empirical, 'Whether inference cost scaling is algorithmic or market-driven').

omega_variable(
    competitive_entry_barrier_height,
    'What minimum capital threshold is required to offer competitive inference services (price within 30% of frontier providers)?',
    'Historical analysis of new inference provider launches; capital requirements vs time-to-price-parity; identification of unit-economic breakeven points',
    'If < $100M: competitive entry is possible, snare classification weakens. If > $500M: barriers are severe, snare classification strengthens, scaffold sunset becomes increasingly speculative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_entry_barrier_height, empirical, 'Capital barrier to competitive inference service entry').

omega_variable(
    open_source_model_substitution_rate,
    'At what rate do open-source models close the capability gap with frontier proprietary models?',
    'Benchmark tracking (MMLU, GSM8K, real-world task performance) for open-source vs proprietary models over time; user adoption curves for open-source alternatives; cost savings achieved by practitioners switching',
    'If gap closes within 6-12 months: snare classification degrades to tangled_rope (scaffold exit visible). If gap persists > 2 years: snare classification hardens, victims remain trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_model_substitution_rate, empirical, 'Rate of open-source model capability convergence').

omega_variable(
    inference_margin_compression_feasibility,
    'Can inference pricing margins compress through competition without regulatory intervention, given the capital requirements for compute infrastructure?',
    'Economic modeling of inference provider competition; analysis of price movements in GPU/TPU markets; comparison to other infrastructure-intensive markets (cloud compute, telecommunications)',
    'If margins compress naturally: snare weakens toward rope (competitive extraction reduces to coordination). If margins persist: snare hardens, regulatory scaffold becomes necessary for constraint resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inference_margin_compression_feasibility, conceptual, 'Natural margin compression vs structural persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inference_cost_scaling_law, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infcost_tr_t0, inference_cost_scaling_law, theater_ratio, 0, 0.22).
narrative_ontology:measurement(infcost_tr_t3, inference_cost_scaling_law, theater_ratio, 3, 0.3).
narrative_ontology:measurement(infcost_tr_t6, inference_cost_scaling_law, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(infcost_be_t0, inference_cost_scaling_law, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(infcost_be_t3, inference_cost_scaling_law, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(infcost_be_t6, inference_cost_scaling_law, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inference_cost_scaling_law, resource_allocation).
narrative_ontology:affects_constraint(inference_cost_scaling_law, model_capability_frontier_gap).
narrative_ontology:affects_constraint(inference_cost_scaling_law, training_cost_accumulation).
narrative_ontology:affects_constraint(inference_cost_scaling_law, compute_access_equity).

% DUAL FORMULATION NOTE:
% The inference cost scaling law is upstream of three related constraints: (1) model_capability_frontier_gap (ε≈0.25, Mountain-adjacent) — the algorithmic fact that frontier models are more capable; (2) training_cost_accumulation (ε≈0.52, Snare) — the total cost of training and serving models at scale; (3) compute_access_equity (ε≈0.61, Snare) — the structural inequality in access to computational resources. The inference cost scaling law synthesizes these into a single extraction mechanism. The network edges show dependency: without frontier capability gaps, pricing power would erode; without training cost accumulation, scarcity narratives would fail; without compute access inequality, the tollgate would not function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
