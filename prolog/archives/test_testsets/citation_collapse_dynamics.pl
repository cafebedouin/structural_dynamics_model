% ============================================================================
% CONSTRAINT STORY: citation_collapse_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_citation_collapse_dynamics, []).

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
 *   constraint_id: citation_collapse_dynamics
 *   human_readable: The Recursive Grounding Decay
 *   domain: technological/academic/informational
 *
 * SUMMARY:
 *   Recursive Grounding Decay describes a feedback loop where Large Language
 *   Models (LLMs), trained on internet data, generate content that is
 *   subsequently scraped and used to train future models. This process
 *   progressively dilutes the proportion of original, human-generated source
 *   material in training sets, causing models to learn from their own
 *   outputs. This leads to 'model collapse,' a degradation of knowledge,
 *   diversity, and factual accuracy, where the link to primary sources is
 *   severed. The system creates a structural conflict between the economic
 *   incentive to produce vast quantities of low-cost synthetic content and
 *   the need to maintain a grounded, reliable information commons.
 *
 * KEY AGENTS:
 *   - LLM Deployers: Primary beneficiaries (institutional/arbitrage) — Profit from the low-cost generation of content for products and services.
 *   - Information Consumers: Primary victims (powerless/trapped) — Face a degraded information environment and bear the costs of misinformation.
 *   - Primary Source Producers: Secondary victims (moderate/constrained) — Journalists, academics, and artists whose work is devalued and replaced.
 *   - Digital Archivist Coalition: Organized agents (organized/constrained) — Attempt to build technical and social scaffolds to preserve information integrity.
 *   - The Epistemic Commons: Abstract victim (powerless/trapped) — The collective body of reliable knowledge that is being polluted.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(citation_collapse_dynamics, 0.65).
domain_priors:suppression_score(citation_collapse_dynamics, 0.75).
domain_priors:theater_ratio(citation_collapse_dynamics, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(citation_collapse_dynamics, extractiveness, 0.65).
narrative_ontology:constraint_metric(citation_collapse_dynamics, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(citation_collapse_dynamics, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(citation_collapse_dynamics, tangled_rope).
narrative_ontology:human_readable(citation_collapse_dynamics, "The Recursive Grounding Decay").
narrative_ontology:topic_domain(citation_collapse_dynamics, "technological/academic/informational").

domain_priors:requires_active_enforcement(citation_collapse_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(citation_collapse_dynamics, llm_deployers).
narrative_ontology:constraint_victim(citation_collapse_dynamics, primary_source_producers).
narrative_ontology:constraint_victim(citation_collapse_dynamics, information_consumers).
narrative_ontology:constraint_victim(citation_collapse_dynamics, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION CONSUMER (SNARE) — Trapped in an information ecosystem flooded with synthetic, ungrounded content. The cost of verifying information becomes prohibitive, and the consumer bears the full epistemic cost of model collapse. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.11.
constraint_indexing:constraint_classification(citation_collapse_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LLM DEPLOYER (ROPE) — Experiences the constraint as a large-scale coordination challenge: managing data quality to produce useful outputs. The decay is a negative externality or technical bug to be engineered away, not a primary feature. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. Negative extraction indicates a net beneficiary.
constraint_indexing:constraint_classification(citation_collapse_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PRIMARY SOURCE PRODUCER (TANGLED ROPE) — Their original work provides the grounding for early models (coordination), but is then devalued and replaced by synthetic derivatives (extraction). Exit is constrained as they cannot abandon their profession, but they retain some agency through legal and public pressure. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.75.
constraint_indexing:constraint_classification(citation_collapse_dynamics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DIGITAL ARCHIVIST COALITION (SCAFFOLD) — Views the decay as a temporary crisis requiring new infrastructure for content provenance and verification. They are building technical and social scaffolds (e.g., C2PA standard, archival of pre-LLM datasets) with the explicit goal of creating a future state where information is grounded again. This represents a sunset clause on the current chaos. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.31.
constraint_indexing:constraint_classification(citation_collapse_dynamics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SEARCH ENGINE INTERFACE (PITON) — The original function of indexing and ranking human-generated knowledge has degraded. The interface now performs the ritual of providing citations and answers, but the underlying content is increasingly self-referential and ungrounded. The high theater_ratio (0.80) triggers the Piton classification, reflecting a system maintained by inertia despite its core function atrophying.
constraint_indexing:constraint_classification(citation_collapse_dynamics, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The system's claimed type. This view recognizes both the genuine coordination function (organizing vast amounts of information in a new way) and the severe, asymmetric extraction from the epistemic commons. The feedback loop is a structural feature, not just a bug. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(citation_collapse_dynamics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(citation_collapse_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(citation_collapse_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(citation_collapse_dynamics, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(citation_collapse_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(citation_collapse_dynamics, TR),
    TR >= 0.70.

:- end_tests(citation_collapse_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The system extracts value from the entire corpus of human knowledge, replacing it with a lower-fidelity, self-referential substitute. This appropriation of the epistemic commons for private gain is highly extractive. Suppression (0.75): High. As synthetic content floods information channels (search, social media), it becomes increasingly difficult and costly for users to find and verify primary sources, effectively suppressing alternatives. Theater Ratio (0.80): Very High. LLMs are designed to perform authoritativeness and fluency, regardless of the factual grounding of their statements. Fabricated citations and confident-sounding falsehoods are a core part of the phenomenon, making the ratio of performative knowledge to functional knowledge extremely high.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For LLM deployers, this is a technical coordination problem (Rope). For consumers, it's an inescapable trap of misinformation (Snare). For creators, it's a hybrid system that both uses their work and devalues it (Tangled Rope). For archivists, it's a temporary crisis to be solved with new infrastructure (Scaffold). For the search interface that mediates access, it's a ritual that has lost its original function (Piton). This diversity of perspectives on a single, well-defined technical phenomenon is a canonical example of Deferential Realism.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (LLM Deployers) have arbitrage exit and institutional power, leading to a low 'd' value and a Rope classification. Victims (Consumers) are trapped and powerless, leading to a high 'd' value and a Snare classification. Other agents fall in between. The structural relationships defined in `base_properties` directly generate the perspectival classifications via the chi formula without needing overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by demonstrating that a single set of structural properties can be simultaneously perceived as all six constraint types. The error would be to declare one perspective (e.g., the deployer's Rope or the consumer's Snare) as the single 'true' classification. The reality is the entire presheaf of perspectives. The analytical observer's classification of Tangled Rope is the system's 'claimed_type' because it acknowledges both the coordination function and the asymmetric extraction, providing the most complete single description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_vs_economic_drivers,
    'Is recursive decay primarily a technical problem solvable with better architectures and data hygiene, or an economic inevitability driven by the low cost of synthetic content?',
    'Analysis of model performance with certified ''clean'' data vs. performance with data scraped from the open, polluted web. Compare costs of both approaches.',
    'If technical, the constraint is a Scaffold that will be dismantled by innovation. If economic, it is a durable Snare or Tangled Rope requiring regulation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_vs_economic_drivers, empirical, 'Distinguishing technical from economic drivers of recursive decay').

omega_variable(
    collapse_threshold,
    'What is the critical percentage of synthetic data in a training corpus that triggers irreversible knowledge collapse?',
    'Controlled experiments training models on datasets with varying percentages of synthetic content, measuring diversity and factual accuracy.',
    'A low threshold suggests the current ecosystem is highly unstable. A high threshold suggests there is more time to build mitigating scaffolds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_threshold, empirical, 'The threshold of synthetic data that triggers irreversible model collapse').

omega_variable(
    provenance_standard_adoption,
    'Will content provenance standards (like C2PA) be widely adopted, or will they be ignored in favor of un-credentialed, low-cost content generation?',
    'Tracking adoption rates of provenance standards by major platforms, content creators, and consumer-facing applications over a 3-5 year period.',
    'High adoption confirms the Scaffold perspective and provides a path to resolving the decay. Low adoption reinforces the Snare/Piton perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(provenance_standard_adoption, empirical, 'Likelihood of wide adoption for content provenance standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(citation_collapse_dynamics, 2022, 2032).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cita_tr_t0, citation_collapse_dynamics, theater_ratio, 0, 0.6).
narrative_ontology:measurement(cita_tr_t5, citation_collapse_dynamics, theater_ratio, 5, 0.72).
narrative_ontology:measurement(cita_tr_t10, citation_collapse_dynamics, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(cita_be_t0, citation_collapse_dynamics, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cita_be_t5, citation_collapse_dynamics, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(cita_be_t10, citation_collapse_dynamics, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(citation_collapse_dynamics, information_standard).
narrative_ontology:affects_constraint(citation_collapse_dynamics, scientific_replication_crisis).
narrative_ontology:affects_constraint(citation_collapse_dynamics, public_trust_in_media).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
