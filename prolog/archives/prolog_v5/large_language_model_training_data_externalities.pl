% ============================================================================
% CONSTRAINT STORY: large_language_model_training_data_externalities
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_large_language_model_training_data_externalities, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: large_language_model_training_data_externalities
 *   human_readable: LLM Training Data Externalities: Uncompensated Value Extraction and Coordination Failure
 *   domain: artificial_intelligence/digital_labor/intellectual_property
 *
 * SUMMARY:
 *   Large language models trained on internet-scale datasets represent a
 *   structural extraction mechanism operating at the intersection of
 *   intellectual property law, computational capability, and digital labor.
 *   The constraint exhibits classic tangled rope dynamics: frontier AI
 *   companies solve a genuine coordination problem (aggregating distributed
 *   human knowledge into accessible models) while simultaneously extracting
 *   value from content creators who retain no compensation, attribution, or
 *   control over how their work is used. The extraction flow is uncompensated
 *   training data (from creators, cultural commons, publishing industries)
 *   flowing to AI companies that capture monopoly rents through model
 *   deployment, API access, and commercial applications. Suppression operates
 *   through multiple channels: technical (data already scraped; distribution
 *   irreversible), legal (fair use doctrine applied at scales it was not
 *   designed for), economic (creators individually lack bargaining power),
 *   and informational (asymmetry between what companies know about training
 *   data sources and what creators know about their data's use). The
 *   constraint shows measurable degradation (theater_ratio and extractiveness
 *   increasing over the measurement interval) as initial justifications (fair
 *   use, transformative use, public benefit) have become increasingly
 *   strained under the scale of industrial-scale training.
 *
 * KEY AGENTS:
 *   - Frontier AI Companies: Primary beneficiary (institutional/arbitrage) — capture value through model deployment, API monetization, and competitive advantage. High arbitrage capacity: can access alternative data sources (synthetic data, licensed datasets, proprietary corpora) and exit the open-web-scraping regime if compensation costs rise.
 *   - Content Creators: Primary victim (powerless/trapped) — writers, artists, photographers whose work was scraped without consent or compensation. Trapped by irreversibility of extraction (data already distributed globally) and lack of exit options (cannot prevent model deployment of already-trained models).
 *   - Publishing Industry: Secondary beneficiary and victim (organized/constrained) — nominally owns author intellectual property but cannot fully control its use; also benefits from LLM tools that reduce costs. Constrained exit: copyright litigation is slow and jurisdictionally complex.
 *   - Cultural Commons: Structural victim (powerless/trapped) — collective epistemic resource degraded by concentration of written culture into proprietary models; abstract actor with no voice.
 *   - Future AI-Trained Workers: Tertiary victim (moderate/constrained) — labor market effects not yet fully realized but structural: increased displacement risk and wage pressure from LLM-augmented work.
 *   - Data Governance Coalition: Organized actors (organized/constrained) — regulatory bodies, creator unions, privacy advocates proposing compensation frameworks and data provenance standards. Constrained but organized; perceive clear policy reform pathways.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing extraction as inherent to machine learning scaling, when it is actually a policy choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(large_language_model_training_data_externalities, 0.62).
domain_priors:suppression_score(large_language_model_training_data_externalities, 0.68).
domain_priors:theater_ratio(large_language_model_training_data_externalities, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(large_language_model_training_data_externalities, extractiveness, 0.62).
narrative_ontology:constraint_metric(large_language_model_training_data_externalities, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(large_language_model_training_data_externalities, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(large_language_model_training_data_externalities, tangled_rope).
narrative_ontology:human_readable(large_language_model_training_data_externalities, "LLM Training Data Externalities: Uncompensated Value Extraction and Coordination Failure").
narrative_ontology:topic_domain(large_language_model_training_data_externalities, "artificial_intelligence/digital_labor/intellectual_property").

domain_priors:requires_active_enforcement(large_language_model_training_data_externalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(large_language_model_training_data_externalities, frontier_ai_companies).
narrative_ontology:constraint_beneficiary(large_language_model_training_data_externalities, model_users_accessing_capabilities).
narrative_ontology:constraint_victim(large_language_model_training_data_externalities, content_creators).
narrative_ontology:constraint_victim(large_language_model_training_data_externalities, text_data_originators).
narrative_ontology:constraint_victim(large_language_model_training_data_externalities, future_ai_trained_workers).
narrative_ontology:constraint_victim(large_language_model_training_data_externalities, cultural_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNCOMPENSATED CONTENT CREATOR (SNARE) — Writers, artists, photographers whose work was scraped and incorporated into training datasets without consent, compensation, or attribution have no exit. Their intellectual property is already extracted. Even if they stop creating, their past work continues generating value for LLM companies. Trapped by the irreversibility of data extraction and global reach of models trained on their work. Maximum experienced extraction.
constraint_indexing:constraint_classification(large_language_model_training_data_externalities, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKING WRITER (TANGLED ROPE) — Faces constrained exit: can attempt to opt-out from future scraping or sue for copyright infringement, but these routes are costly (legal, technical) and uncertain (jurisdiction varies; data already copied). Also benefits from access to LLM tools that increase writing productivity. Mixed extraction and coordination — some benefit from the ecosystem, high costs of exit.
constraint_indexing:constraint_classification(large_language_model_training_data_externalities, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FRONTIER AI COMPANY (ROPE) — Experiences training data as a coordination mechanism: aggregating distributed human knowledge into a common model that enables communication and capability. The extraction is asymmetric (they capture value), but they genuinely solve a coordination problem (making human knowledge accessible at scale). High arbitrage capacity — can access alternative data sources (synthetic data, licensed datasets, proprietary corpora) and exit the training-on-web-text regime if incentives shift.
constraint_indexing:constraint_classification(large_language_model_training_data_externalities, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLISHING INDUSTRY (TANGLED ROPE) — Institutional actor with some organizational power (legal leverage, contractual control over author rights) but constrained exit: copyright litigation is slow and jurisdictionally complex; models trained on published work are already deployed globally. Benefits from LLM tools that reduce some publishing costs (editing, formatting) while experiencing extraction of the intellectual property they nominally control. Organized but constrained.
constraint_indexing:constraint_classification(large_language_model_training_data_externalities, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DATA GOVERNANCE COALITION (SCAFFOLD) — Organized actors (regulatory bodies, creator unions, privacy advocates) proposing data payment mechanisms, opt-in training datasets, and compensatory frameworks see the extraction as a temporary governance failure with a sunset clause. EU AI Act, data provenance standards, and creator compensation proposals represent alternative pathways. Low effective extraction because these agents have institutional leverage and perceive a clear exit path through policy reform.
constraint_indexing:constraint_classification(large_language_model_training_data_externalities, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FAIR USE DOCTRINE (PITON) — Legal framework claiming that LLM training falls under fair use (transformative use, noncompetitive benefit to society) persists despite degraded functional justification. Fair use doctrine was designed for limited human copying; applying it to the complete vectorization of humanity's written culture on a commercial scale is theater: the doctrine's language hasn't changed, but its functional content has evaporated. Maintained through institutional inertia and precedent, not because the logic holds.
constraint_indexing:constraint_classification(large_language_model_training_data_externalities, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MARKET INEVITABILITY (MOUNTAIN) — From a civilizational scale, some extraction may appear as an immutable feature of machine learning at scale: the informational requirements for training capable models exceed what any single actor can produce; distributed human creativity must be aggregated; some asymmetry is inherent to the geometry of data collection. This perspective risks naturalizing what is actually a contingent policy and contractual regime. The engine's false summit detector will identify this as a naturalization of contingent choices.
constraint_indexing:constraint_classification(large_language_model_training_data_externalities, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(large_language_model_training_data_externalities_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(large_language_model_training_data_externalities, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(large_language_model_training_data_externalities, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(large_language_model_training_data_externalities, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(large_language_model_training_data_externalities, TR),
    TR >= 0.70.

:- end_tests(large_language_model_training_data_externalities_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderately high. Creators bear full costs (no compensation, lost control) and capture none of the model's value, while AI companies capture monopoly rents through model deployment. However, extraction is not absolute (0.72+) because (1) society does gain genuine capability from LLM development; (2) creators retain nominal ownership of their original works; (3) some creators benefit from access to LLM tools. The measurement shows increasing extractiveness over the interval (0.35→0.62) reflecting progressive hardening of the extraction mechanism: early justifications (fair use, transformative use) have become strained as model capabilities and commercial deployment have scaled, and legal/policy alternatives (copyright enforcement, data compensation) remain unresolved. Suppression (0.68): Moderately high. Suppression operates through technical inevitability (data already scraped; distribution irreversible), legal ambiguity (fair use doctrine applied at scales it doesn't clearly cover), economic asymmetry (creators individually lack bargaining power), and informational asymmetry (companies know what data was used; creators often don't). However, suppression is not total (0.85+) because (1) copyright law provides some formal grounds for claims; (2) creator organizing is occurring (unions, advocacy); (3) regulatory reform pathways are visible (EU AI Act, data provenance standards). Theater ratio (0.55): Moderate. Fair use justifications and claims about 'transformative use' and 'public benefit' represent performative framing that obscures the core extraction mechanism. However, theater is not dominant (0.72+) because the underlying coordination function is genuine — LLM training does aggregate distributed knowledge into collective capability that wouldn't exist otherwise.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is whether the training data flow represents genuine coordination or pure extraction. Frontier AI companies see coordination: they are aggregating distributed human knowledge into a capability that enables broad benefit (model users, society). Content creators see extraction: their work is taken without consent or compensation, and they receive zero benefit while companies capture monopoly rents. Publishing industry occupies the middle: they nominally control author IP but cannot effectively defend it; they benefit from LLM tools but lose control over derivative uses. Data governance coalition sees a temporary governance failure: the legal and policy infrastructure for compensated data use is missing, but buildable. Fair use doctrine is performative (piton): it justifies extraction with language designed for small-scale human copying, not industrial-scale machine learning. The mountain perspective naturalizes extraction as inherent to ML scaling: some asymmetry is inevitable when aggregating distributed knowledge at scale. But this naturalizes what is actually a policy choice: the companies could pay for data; they choose not to, and the legal regime (fair use interpretation) enables that choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by the flow of uncompensated value from creators to AI companies. Content creators occupy high-d positions (d ≈ 0.92): structurally trapped, bearing extraction costs with no corresponding benefits. Frontier AI companies occupy low-d positions (d ≈ 0.08): net beneficiaries with multiple exit options (synthetic data, licensed datasets, proprietary corpora). Publishers occupy mid-d positions (d ≈ 0.55): asymmetric extraction (loss of control over author IP) partly offset by benefits from LLM tools and continued nominally-controlled IP. Data governance coalition occupies high-d position (d ≈ 0.65) but with organized power and constrained rather than trapped exit: they bear the costs of governance failure (creators uncompensated) but have institutional leverage to reform. The derived effective extractiveness chi varies by perspective: highest for powerless creators (high d, no scope dampening due to global distribution), moderate for organized coalition (high d offset by organized power and constrained exit), low for beneficiary company (low d, arbitrage exit). The analytical observer at civilizational/universal scope (d ≈ 0.73) risks deriving maximum chi by treating the constraint as a natural law rather than a policy choice.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVING THE MANDATROPHY: This constraint resolves the mandatrophy by showing that tangled_rope is the correct classification because (1) genuine coordination function exists (LLM training does aggregate distributed knowledge into collective capability); (2) asymmetric extraction also exists (creators get zero compensation while companies capture monopoly rents); (3) active enforcement is required (intellectual property law, regulatory frameworks, compensation mechanisms must be actively deployed to sustain or reform the extraction); and (4) multiple institutional actors have conflicting interests (companies want unrestricted data access; creators want compensation; regulators want governance). The false summit detector will flag the mountain perspective (naturalizing extraction as inherent to ML scaling) as a naturalization of policy choices. The snare perspective (pure extraction from powerless creators) is a valid experience from the trapped position but misses the coordination dimension. The scaffold perspective (governance reform with sunset) is realistic but requires institutional coordination that has not yet occurred. The piton perspective (fair use doctrine as degraded ritual) is diagnostically important: the fair use framing is theater maintained through inertia and precedent, not because the logic applies at industrial scale. The constraint's classification as tangled_rope is robust because both the coordination and extraction dimensions are structurally real, not perspectival artifacts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compensation_mechanism_feasibility,
    'Is per-training-instance compensation technically and economically feasible, or is the scale of data (billions of tokens) fundamentally incompatible with granular compensation?',
    'Technical analysis of blockchain-based provenance systems, smart contract payment mechanisms, and transaction cost analysis. Empirical comparison with music streaming micropayment systems (successful) vs. previous click-per-ad models (failed at scale).',
    'If feasible: compensation becomes a governance choice, shifting classification toward rope/scaffold. If infeasible: extraction becomes structurally embedded in the LLM training regime, deepening snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_mechanism_feasibility, empirical, 'Technical feasibility of per-instance data compensation').

omega_variable(
    value_attribution_identity,
    'Can the specific contribution of any individual training instance to final model capability be isolated and valued, or is LLM value emergent from aggregate statistical patterns such that individual attribution is incoherent?',
    'Data valuation research (Shapley values, influence functions); empirical analysis of whether removing individual training instances from a trained model produces measurable capability loss. Conceptual analysis of whether ''contribution'' is even meaningful for statistical systems.',
    'If attributable: per-creator compensation is coherent and feasible. If emergent/incoherent: individual compensation frameworks are theater, and only collective/aggregate mechanisms (universal revenue share, creator funds) are meaningful.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(value_attribution_identity, conceptual, 'Whether individual training data contribution can be isolated and valued').

omega_variable(
    synthetic_data_substitutability,
    'Can synthetic data (generated by models trained on human data) fully replace human-generated training data, or do LLMs require some irreducible core of original human expression for continued improvement?',
    'Empirical: train models on increasingly synthetic data pipelines; measure capability degradation. Conceptual: whether ''understanding'' can emerge from pure statistical patterns without grounding in human intent.',
    'If substitutable: AI companies have exit path from extraction (they can train on synthetic data only). Snare classification weakens. If irreducible: human creativity is a necessary input with no substitute, deepening extraction logic and snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_data_substitutability, empirical, 'Whether synthetic data can fully replace human training data').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of creator bargaining power (via technical barriers, legal ambiguity, information asymmetry) structural (external barriers) or internalized (creators accept low/zero compensation as inevitable)?',
    'Empirical: union organizing rates, creator advocacy group formation, litigation patterns. Survey-based: creator beliefs about compensability. Post-exit analysis: if copyright enforcement succeeds, do creators'' expectations shift?',
    'If structural: suppression can be reduced by removing barriers (clearer contracts, licensing platforms, data payment infrastructure). If internalized: suppression persists even after barrier removal — creator identity has fused with the unpaid-labor role. Identity_locked exit option becomes relevant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in creator expectations').

omega_variable(
    coordination_function_reality,
    'Does LLM training genuinely coordinate distributed human knowledge into a collective capability, or does it primarily extract and concentrate knowledge that was already accessible, adding value only through scale and interface?',
    'Comparison of pre- and post-LLM information availability: was human knowledge genuinely inaccessible before (coordination function real) or merely expensive to access (concentration function, no new coordination)? Analysis of what creators could have achieved through voluntary pooling.',
    'If coordination: tangled_rope classification is justified (genuine hybrid). If concentration: snare classification is more accurate (pure extraction with no coordination benefit to creators).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_reality, conceptual, 'Whether LLM training coordinates knowledge or merely concentrates existing information').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(large_language_model_training_data_externalities, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(llm_ext_tr_t0, large_language_model_training_data_externalities, theater_ratio, 0, 0.4).
narrative_ontology:measurement(llm_ext_tr_t3, large_language_model_training_data_externalities, theater_ratio, 3, 0.48).
narrative_ontology:measurement(llm_ext_tr_t6, large_language_model_training_data_externalities, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(llm_ext_be_t0, large_language_model_training_data_externalities, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(llm_ext_be_t3, large_language_model_training_data_externalities, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(llm_ext_be_t6, large_language_model_training_data_externalities, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(large_language_model_training_data_externalities, resource_allocation).
narrative_ontology:boltzmann_floor_override(large_language_model_training_data_externalities, 0.18).
narrative_ontology:affects_constraint(large_language_model_training_data_externalities, ai_labor_market_displacement).
narrative_ontology:affects_constraint(large_language_model_training_data_externalities, intellectual_property_regime_digital_content).
narrative_ontology:affects_constraint(large_language_model_training_data_externalities, platform_content_moderation_externalities).

% DUAL FORMULATION NOTE:
% This constraint can be decomposed into at least three structurally distinct stories with different ε values: (1) creator_consent_violation (ε≈0.78, pure snare) — ethical/legal wrongfulness of training without consent; (2) llm_value_capture (ε≈0.55, tangled rope) — asymmetric benefit distribution in knowledge coordination; (3) fair_use_doctrine_degradation (ε≈0.42, piton) — legal theater maintaining outdated doctrine at scale. These three stories share a domain (LLM training data) but have different ε values, different primary beneficiaries/victims, and different policy reform pathways. They are linked via network.affects_constraints but represent distinct structural constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(large_language_model_training_data_externalities, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
