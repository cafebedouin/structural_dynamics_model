% ============================================================================
% CONSTRAINT STORY: model_hallucination_scaling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_model_hallucination_scaling, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: model_hallucination_scaling
 *   human_readable: Model Hallucination Scaling: Coordination of Training-Deployment Asymmetry with Asymmetric Extraction
 *   domain: machine_learning/language_models
 *
 * SUMMARY:
 *   Model hallucination scaling represents the structural tension between the
 *   mathematical inevitability of next-token prediction producing confident
 *   false outputs and the institutional choice to deploy such systems at
 *   scale without compensating downstream consumers or establishing
 *   transparent accountability mechanisms. The constraint exhibits
 *   tangled_rope structure: genuine coordination problems (developing better
 *   evaluation metrics, training protocols, and shared understanding of
 *   failure modes) coexist with asymmetric extraction (deployers benefit from
 *   capability improvements while bearing minimal reputational cost; model
 *   builders bear reputation risk; downstream consumers bear epistemic harm
 *   with no exit). The extractiveness has risen from 0.35 to 0.58 over the
 *   measurement interval as model scale and deployment breadth have increased
 *   without proportional improvements in hallucination detection or
 *   mitigation. The theater_ratio has simultaneously risen from 0.45 to 0.68,
 *   indicating that public discourse increasingly focuses on managing
 *   perception of hallucinations (anthropomorphic terminology, uncertainty
 *   quantification theater, constitutional AI frameworks) rather than
 *   addressing the fundamental tension between the training objective and
 *   deployment safety. This gap is diagnostic: the constraint is becoming
 *   more extractive (higher χ) while its presentation becomes more
 *   performative (higher theater), which is the classic progression from
 *   tangled_rope toward snare.
 *
 * KEY AGENTS:
 *   - Downstream Information Consumer: Primary victim (powerless/trapped) — no practical exit from hallucinated outputs at global scale; bears epistemic harm and wasted time with no recourse
 *   - Model Deployer: Primary beneficiary (institutional/arbitrage) — captures value from improved model capabilities while externalizing hallucination costs to users; abundant exit options enable arbitrage across architectures
 *   - Model Builder Organization: Organized victim (organized/constrained) — bears reputational cost of hallucination failures while deployers distance themselves; genuine coordination benefits from publishing methods but constrained by competitive incentives
 *   - AI Safety Researcher: Moderate coordinator (moderate/constrained) — develops evaluation metrics and mitigation strategies (coordination function) but gains less value from improvements than deployers (asymmetric extraction)
 *   - Regulatory Coalition: Organized future actor (organized/constrained) — building alternative verification pathways (auditing, certification, mandatory transparency) with sunset trajectory
 *   - User Interface Theater: Institutional theater mechanism (institutional/arbitrage) — the performative 'hallucination' discourse and anthropomorphic framing that naturalizes the extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(model_hallucination_scaling, 0.58).
domain_priors:suppression_score(model_hallucination_scaling, 0.62).
domain_priors:theater_ratio(model_hallucination_scaling, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(model_hallucination_scaling, extractiveness, 0.58).
narrative_ontology:constraint_metric(model_hallucination_scaling, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(model_hallucination_scaling, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(model_hallucination_scaling, tangled_rope).
narrative_ontology:human_readable(model_hallucination_scaling, "Model Hallucination Scaling: Coordination of Training-Deployment Asymmetry with Asymmetric Extraction").
narrative_ontology:topic_domain(model_hallucination_scaling, "machine_learning/language_models").

domain_priors:requires_active_enforcement(model_hallucination_scaling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(model_hallucination_scaling, model_deployers).
narrative_ontology:constraint_beneficiary(model_hallucination_scaling, end_users_at_scale).
narrative_ontology:constraint_victim(model_hallucination_scaling, downstream_information_consumers).
narrative_ontology:constraint_victim(model_hallucination_scaling, model_builders_bearing_reputation_cost).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM INFORMATION CONSUMER (SNARE) — No practical exit from hallucinated outputs at scale. Cannot distinguish confident false claims from true ones without expert verification unavailable to most users. Trapped by accessibility of the interface and absence of reliable signaling. Bears full extraction cost: epistemic harm, wasted time, downstream decision errors. Maximum suppression: the mechanism producing hallucinations is opaque, the rate is variable and context-dependent, and no alternative channel for the same information exists at comparable scale.
constraint_indexing:constraint_classification(model_hallucination_scaling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AI SAFETY RESEARCHER (TANGLED ROPE) — Faces genuine coordination problem: hallucination is a systems-level phenomenon requiring shared measurement frameworks, training data transparency, and evaluation protocols. Also faces extraction: gains career visibility from publishing hallucination research but bears asymmetric cost of implementing mitigation while deployers capture value from improved models. Constrained exit: can switch domains but carries reputational/skill capital in this problem space. Genuinely serves coordination function (developing better evaluation metrics) while being extracted (deployers benefit more than researchers from improvements).
constraint_indexing:constraint_classification(model_hallucination_scaling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MODEL DEPLOYER (ROPE) — Experiences hallucination scaling as pure coordination problem at immediate time horizon. Scaling requires solving hallucination to serve users reliably. Net beneficiary through arbitrage: can deploy larger models despite hallucination costs if those models serve new markets or reduce deployment overhead. Extraction runs toward this agent. Exit options abundant — can switch to smaller models, closed-domain systems, or alternative architectures. Sees the constraint as a technical problem requiring shared investment in evaluation and mitigation.
constraint_indexing:constraint_classification(model_hallucination_scaling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Organized agents (AI safety institutes, auditing bodies, proposed regulatory frameworks) see hallucination as a temporary coordination failure with a clear sunset: better training protocols, constitutional AI methods, and external fact-checking infrastructure are building alternative verification pathways. High suppression initially (industry resists reporting metrics, metrics are contested, deployment moves faster than standards) but declining as sunset mechanisms mature. Effective extraction is low because organized agents perceive exit pathways and see timeline for their obsolescence.
constraint_indexing:constraint_classification(model_hallucination_scaling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: USER INTERFACE THEATER (PITON) — The discourse around 'hallucination as a distinct phenomenon' is substantially performative. Models hallucinate because they were trained to predict next tokens, not because of a separable bug. The term 'hallucination' is itself performative — it anthropomorphizes a mathematical artifact, enabling deployers to market confident false outputs as a charming limitation rather than a systematic flaw. The theater_ratio reflects this: much public discussion is about mitigating or detecting hallucinations rather than addressing their root in the training objective. Theater persists through institutional inertia — the 'hallucination' frame has become embedded in safety discourse, product marketing, and academic framing. Functional verification (distinguishing true from false outputs) remains unresolved; institutional processes around the term persist despite this.
constraint_indexing:constraint_classification(model_hallucination_scaling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MODEL BUILDER ORGANIZATION (TANGLED ROPE) — Faces asymmetric extraction through reputational cost while the constraint serves genuine coordination function. Deployers benefit from model capability improvements that reduce hallucination; model builders bear reputational cost of failure cases. Extraction asymmetry: deployers can disown hallucinations ('the model made it up') while model builders remain associated with the model's outputs. Coordination function: publishing training methods, evaluation protocols, and failure analysis drives field-wide improvement. Organized power allows coalition formation but constrained by competitive dynamics — open publication of flaws risks market disadvantage. This is the core tangled_rope: genuine coordination function (field improvement) wrapped in asymmetric extraction (reputational cost flows to builder, capability benefits flow to deployer).
constraint_indexing:constraint_classification(model_hallucination_scaling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT) — From a universal/civilizational perspective, the temptation is to classify this as a mountain: next-token prediction inevitably produces confident false outputs, therefore hallucination is inherent to the architecture, therefore it is immutable. However, the structural data contradicts this false summit. The constraint is not hallucination itself (a mathematical artifact) but the institutional arrangement that deploys hallucinating systems at scale without compensating downstream consumers, without transparent reporting mechanisms, and without activated exit pathways. The immutability framing naturalizes the contingent institutional arrangement. This classification is a diagnostic marker that the mountainization attempt is itself an extraction mechanism — claiming the problem is inherent justifies continuing the extraction.
constraint_indexing:constraint_classification(model_hallucination_scaling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(model_hallucination_scaling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(model_hallucination_scaling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(model_hallucination_scaling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(model_hallucination_scaling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(model_hallucination_scaling, TR),
    TR >= 0.70.

:- end_tests(model_hallucination_scaling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. The constraint extracts from downstream consumers (epistemic harm, wasted time) and model builders (reputational cost) while deployers benefit through capability gains and cost externalization. The rising trajectory reflects that as model scale and deployment breadth increase, the accumulated hallucination cost to the user base grows while mitigation mechanisms remain incomplete. Extractiveness is not maximal (snare-level ≥0.66) because safety researchers are making genuine progress on detection and mitigation, and because organized players (regulatory coalition) are building alternative pathways. Suppression (0.62): High and stable. Mechanisms include: (1) measurement opacity — hallucination rates are not standardly reported; (2) mechanism opacity — users cannot reliably distinguish confident true outputs from confident false outputs; (3) absence of practical alternatives — no comparable-scale systems have solved the hallucination problem; (4) asymmetric incentives — deployers benefit from maintaining the uncertainty about hallucination severity. Theater ratio (0.68): High and rising. The anthropomorphic 'hallucination' language itself is performative — it attributes agency to a mathematical artifact. Constitutional AI and uncertainty quantification are presented as solutions while leaving the fundamental training objective unchanged. The rising ratio indicates that as critics have questioned the constraint, public discourse has become increasingly focused on perception management rather than structural change.
 *
 * PERSPECTIVAL GAP:
 *   The classification ranges from snare (powerless/trapped: maximum experienced extraction, no exit) through tangled_rope (organized/constrained players: mixed benefit and cost, some coordination) to rope (institutional/arbitrage deployer: net benefit, pure coordination framing) to scaffold (organized regulator: sees sunset and exit pathways) to piton (institutional theater: sees the constraint as degraded ritual) to false-summit mountain (analytical: risks naturalizing the constraint). This full range from a single set of base properties (ε=0.58, suppression=0.62, theater=0.68) demonstrates that the constraint is not inherently any single type — its classification depends entirely on the observer's structural position and exit options. The gap between snare and rope is maximal: the powerless user and the institutional deployer are experiencing fundamentally different constraints despite the same underlying phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position relative to extraction flow. Downstream consumers have d ≈ 0.95 (pure target, trapped, powerless): extraction flows fully toward them with no escape. Model builders have d ≈ 0.60 (mixed target-beneficiary, organized, constrained): they benefit from field advances but bear reputational cost asymmetrically. Deployers have d ≈ 0.10 (net beneficiary, institutional, arbitrage): extraction flows away from them through externalization. Safety researchers have d ≈ 0.65 (slight net target due to asymmetric benefit capture despite contribution): they contribute to solutions but see less value capture than deployers. The sigmoid f(d) transforms these into experienced extraction factors: trapped powerless consumer experiences χ ≈ 1.42 × ε; organized model builder experiences χ ≈ 0.75 × ε; institutional deployer experiences χ ≈ -0.12 × ε (negative because they are net beneficiaries). The global scope modifier σ(global) = 1.2 amplifies these — the constraint's reach across language models deployed worldwide scales up the effective extraction for powerless agents and the effective benefit for deployers.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE GATES SATISFIED: (1) Genuine coordination function: developing evaluation metrics, training protocols, and field-wide understanding of hallucination modes is genuine coordination. (2) Asymmetric extraction: deployers capture capability benefits and externalize costs; model builders bear reputational risk; downstream consumers bear epistemic harm. (3) Requires active enforcement: maintaining the constraint requires continuing to deploy hallucinating systems without compensating consumers or establishing accountability. The mandatrophy is resolved by recognizing that the coordination function (better understanding of hallucination) and the extraction mechanism (cost externalization) are not separable — they are two faces of the same institutional arrangement. Improvements in hallucination detection do not dissolve the extraction because deployers can adopt them selectively and still externalize undetected hallucinations. The rising theater ratio indicates the constraint is approaching snare classification: as mitigation becomes performative rather than functional, the coordination function weakens and extraction becomes more apparent. Sunset scenarios (mandatory transparency, regulatory auditing, alternative architectures reaching parity) could transition this to scaffold if organized agents can enforce the sunset mechanisms; without enforcement, the constraint drifts toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hallucination_measurement_definition,
    'What distinguishes a hallucination from an artifact of the next-token prediction objective, and how should it be quantified?',
    'Comparative analysis of hallucination rates under different evaluation protocols; investigation of whether hallucination frequency tracks training data distribution or task-specific failure modes; examination of whether the term ''hallucination'' refers to a replicable phenomenon or an anthropomorphic label for mathematical behavior',
    'If hallucination is a distinct, measurable phenomenon: the constraint is real and extractiveness estimates are grounded. If hallucination is an inevitable artifact of the architecture: extractiveness reflects an institutional choice to deploy systems known to produce confident false outputs, strengthening snare classification. If hallucination is definitional ambiguity: the theater_ratio rises and piton classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hallucination_measurement_definition, empirical, 'Whether hallucination is a distinct phenomenon or an artifact of the training objective').

omega_variable(
    suppression_mechanism_transparency,
    'Are hallucination rates and failure modes systematically under-reported by deployers, and does this suppression stem from competitive incentives or technical measurement difficulty?',
    'Comparison of reported vs independently-measured hallucination rates; analysis of disclosure patterns correlating with competitive position; examination of whether transparency barriers are technical (metrics genuinely hard to compute) or institutional (benefits of non-disclosure exceed costs)',
    'If systematic under-reporting: suppression is strategic and extractive, raising chi and snare classification for powerless agents. If measurement difficulty: suppression is coordination problem and tea may be lower. If competitive incentives dominate: supports tangled_rope classification for organized players.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_transparency, empirical, 'Degree to which hallucination rates are systematically under-reported').

omega_variable(
    alternative_architecture_viability,
    'Do alternative architectures (retrieval-augmented generation, symbolic systems, hybrid approaches) provide viable exits from the hallucination scaling problem, or do they trade hallucination for other failure modes?',
    'Comparative failure analysis of RAG vs autoregressive vs hybrid systems on the same domains; measurement of whether users can practically switch to alternatives given deployment constraints; assessment of whether alternative failures (context irrelevance, latency, cost) are preferable to hallucination from downstream consumer perspective',
    'If viable exits exist: classification shifts from snare (trapped) toward tangled_rope (constrained) for users. If tradeoffs are severe: snare classification is strengthened. If deployment path dependency makes switching costly: suppression increases and extraction effect amplifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_architecture_viability, empirical, 'Viability of architectural alternatives to avoid hallucination scaling').

omega_variable(
    constitutional_ai_effectiveness,
    'Do training-time interventions (constitutional AI, chain-of-thought, uncertainty quantification) actually reduce hallucination rates or do they primarily reduce user perception of hallucination without changing the underlying distribution?',
    'Independent evaluation of whether constitutional AI reduces false output confidence or merely redistributes it; measurement of whether uncertainty quantification enables users to successfully self-correct; comparison of hallucination rates in production vs evaluation settings',
    'If interventions are effective: scaffold perspective is validated and sunset timeline is real. If interventions are theater: piton classification strengthens and the constraint persists despite technical improvements. If mixed effectiveness: tangled_rope classification is confirmed (genuine but incomplete progress enabling continued extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_ai_effectiveness, empirical, 'Whether training-time interventions reduce hallucination or redistribute it').

omega_variable(
    reputational_cost_asymmetry,
    'Do model builder reputations bear proportional cost to deployer reputations for hallucination failures, or is the cost asymmetrically distributed?',
    'Analysis of reputational impact across model builder organizations vs deployer organizations following major hallucination-based failures; investigation of whether deployers successfully distance themselves from model failures; measurement of whether users attribute hallucinations to the deployer, the model builder, or the architecture itself',
    'If cost is symmetric: constraint shifts toward rope classification. If asymmetric: tangled_rope classification and directionality logic are confirmed. If deployer can fully externalize cost: extraction effect amplifies and snare classification for model builders is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reputational_cost_asymmetry, empirical, 'Asymmetry of reputational cost between model builders and deployers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(model_hallucination_scaling, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(halluc_tr_t0, model_hallucination_scaling, theater_ratio, 0, 0.45).
narrative_ontology:measurement(halluc_tr_t2, model_hallucination_scaling, theater_ratio, 2, 0.55).
narrative_ontology:measurement(halluc_tr_t4, model_hallucination_scaling, theater_ratio, 4, 0.64).
narrative_ontology:measurement(halluc_tr_t6, model_hallucination_scaling, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(halluc_be_t0, model_hallucination_scaling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(halluc_be_t2, model_hallucination_scaling, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(halluc_be_t4, model_hallucination_scaling, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(halluc_be_t6, model_hallucination_scaling, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(model_hallucination_scaling, information_standard).
narrative_ontology:affects_constraint(model_hallucination_scaling, language_model_capability_deployment).
narrative_ontology:affects_constraint(model_hallucination_scaling, ai_safety_measurement_bottleneck).
narrative_ontology:affects_constraint(model_hallucination_scaling, training_data_curation_asymmetry).

% DUAL FORMULATION NOTE:
% Model hallucination scaling decomposes into distinct constraints depending on the observable used. The 'hallucination is inevitable given next-token prediction' claim (ε ≈ 0.10, mountain candidate) is structurally distinct from 'deploying hallucinating systems at scale without compensation is an institutional choice' (ε ≈ 0.58, tangled rope). The first focuses on mathematical inevitability; the second on institutional extraction. These are linked via network.affects_constraints: mathematical inevitability of next-token hallucination (if true) would affect the feasibility of perfect mitigation but does not determine the institutional choice to deploy at scale without accountability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(model_hallucination_scaling, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
