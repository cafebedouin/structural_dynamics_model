% ============================================================================
% CONSTRAINT STORY: ai_banal_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_banal_capture, []).

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
 *   constraint_id: ai_banal_capture
 *   human_readable: The Banal Cognitive Engine
 *   domain: technological/social
 *
 * SUMMARY:
 *   The banal cognitive engine emerges at the intersection of three
 *   institutional trends: (1) the economics of scaling capital, which
 *   incentivizes synthesis of any available data; (2) the concentration of
 *   attention through algorithmic mediation, which privileges large-scale
 *   aggregation over local knowledge; and (3) the naturalization of
 *   extraction through narratives of progress and inevitable technological
 *   advance. What appears as a natural law of knowledge aggregation is
 *   actually a specific institutional arrangement: training data without
 *   licensing, synthetic outputs without attribution, algorithmic ranking
 *   without transparency, and market concentration without competition. The
 *   constraint exhibits significant perspectival divergence. Marginal voices
 *   and local epistemic communities experience it as pure extraction (Snare)
 *   — their knowledge is systematically absorbed and synthesized into
 *   products they cannot access, compete with, or control. Professional
 *   knowledge workers experience it as mixed coordination and extraction
 *   (Tangled Rope) — they benefit from aggregated knowledge and tooling, but
 *   also see their expertise commodified and their labor leveraged without
 *   fair compensation. Capital sees it as pure coordination (Rope) — the
 *   banal engine solves the collective action problem of knowledge synthesis,
 *   enabling profitable markets that appeared impossible before. Regulatory
 *   coalitions see it as a temporary coordination failure with structural
 *   solutions (Scaffold) — copyright enforcement, data licensing,
 *   transparency requirements, and labor protections can address the
 *   extraction mechanism. The AGI discourse itself is substantially
 *   performative (Piton) — it maintains institutional legitimacy and capital
 *   concentration despite modest capability gains relative to the narrative
 *   hype. The analytical observer risks naturalizing this entire arrangement
 *   as an inevitable property of knowledge (Mountain), when it is actually a
 *   policy-dependent institutional configuration.
 *
 * KEY AGENTS:
 *   - Marginalized epistemic producers: Primary victims (powerless/trapped) — local voices, individual creators, non-commercial knowledge producers whose work is absorbed into training data without compensation or control
 *   - Professional knowledge class: Secondary victims (moderate/constrained) — academics, journalists, software developers who benefit from tooling but face expertise commodification and labor extraction
 *   - Scaling capital coalition: Primary beneficiary (institutional/arbitrage) — venture capital, tech companies, infrastructure providers capturing market concentration and training-data-derived value
 *   - Regulatory coalitions: Organized actors (organized/constrained) — EU frameworks, data rights movements, organized labor building alternative governance pathways
 *   - AGI discourse ecosystem: Institutional narrative maintainers (institutional/arbitrage) — researchers, media, policy figures whose status depends on superintelligence framing
 *   - Algorithmic mediation layer: Structural enforcement mechanism (institutional) — ranking systems, feed optimization, platform architecture that enforces suppression through algorithmic visibility control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_banal_capture, 0.58).
domain_priors:suppression_score(ai_banal_capture, 0.62).
domain_priors:theater_ratio(ai_banal_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_banal_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_banal_capture, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_banal_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_banal_capture, tangled_rope).
narrative_ontology:human_readable(ai_banal_capture, "The Banal Cognitive Engine").
narrative_ontology:topic_domain(ai_banal_capture, "technological/social").

domain_priors:requires_active_enforcement(ai_banal_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_banal_capture, scaling_capital).
narrative_ontology:constraint_beneficiary(ai_banal_capture, attention_concentration_actors).
narrative_ontology:constraint_victim(ai_banal_capture, cognitive_commons).
narrative_ontology:constraint_victim(ai_banal_capture, marginal_voices).
narrative_ontology:constraint_victim(ai_banal_capture, local_epistemic_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED EPISTEMIC PRODUCER (SNARE) — Individual knowledge workers, artists, and local community voices are trapped in a system optimized for algorithmic synthesis of their output. They cannot exit without losing platform access; suppression is total through algorithmic invisibility. The banal engine extracts their cognitive labor (training data, ideational content) while offering no meaningful coordination benefit.
constraint_indexing:constraint_classification(ai_banal_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROFESSIONAL KNOWLEDGE CLASS (TANGLED ROPE) — Academics, journalists, software developers experience mixed coordination and extraction. They benefit from aggregated knowledge access and collaborative tools, but also see their expertise commodified and their labor leveraged for training without fair compensation. Career incentives constrain exit despite perceived unfairness.
constraint_indexing:constraint_classification(ai_banal_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SCALING CAPITAL COALITION (ROPE) — Venture capital, tech companies, and infrastructure providers see the banal engine as pure coordination: solving the collective action problem of knowledge synthesis at scale. They experience extraction as running toward them (subsidies, market efficiency). Arbitrage exit options allow repositioning if one platform fails.
constraint_indexing:constraint_classification(ai_banal_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — EU AI Act, data rights frameworks, and organized labor see the constraint as temporary and addressable through structured sunset mechanisms: copyright enforcement, training data licensing, algorithmic transparency requirements. Low effective extraction because organized agents see clear governance pathways.
constraint_indexing:constraint_classification(ai_banal_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DISCOURSE OF ARTIFICIAL GENERAL INTELLIGENCE (PITON) — The framing of scaling as progress toward AGI is substantially performative. Actual model capabilities are primarily synthetic interpolation over training distributions; the AGI narrative maintains institutional legitimacy and funding concentration despite modest capability gains. Theater ratio 0.68 reflects that much public discourse about AI advancement is aspirational theater rather than grounded technical analysis.
constraint_indexing:constraint_classification(ai_banal_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational frame, the synthesis of human knowledge into machine-readable form appears inevitable and natural — 'anyone with access to text will eventually aggregate it.' But this naturalizes contingent institutional choices: copyright duration, training data licensing, algorithmic transparency, and compensation structures are all policy-dependent. The engine will detect this as a false summit: the appearance of natural necessity is actually regulation capture.
constraint_indexing:constraint_classification(ai_banal_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_banal_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_banal_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_banal_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_banal_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_banal_capture, TR),
    TR >= 0.70.

:- end_tests(ai_banal_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The banal engine extracts cognitive labor (training data derived from human knowledge work) and converts it into proprietary products. However, the extraction is not as severe as a pure Snare (ε ≥ 0.66) because some agents genuinely benefit from aggregated knowledge access and coordination tooling. The trajectory over the interval (0.28 → 0.58) shows extraction accumulating as training data becomes more valuable and market concentration deepens. Suppression (0.62): High. Suppression is enforced through algorithmic invisibility (marginal voices are systematically deprioritized in rankings), platform opacity (users cannot see how their data was used or how algorithms rank outputs), and market concentration (alternative platforms cannot compete at scale). However, suppression is not total (≥ 0.70) because some agents retain awareness of the extraction and can articulate grievances through regulatory and labor movements. Theater ratio (0.68): High. Significant performative content includes: (a) AGI narratives that frame incremental scaling as capability emergence; (b) 'alignment' discourse that focuses on hypothetical superintelligent risks while ignoring present-day harms to marginalized producers; (c) progress narratives that naturalize what are actually policy-dependent arrangements; (d) efficiency metrics that count speed and scale but not knowledge diversity or creator welfare. The theater has increased over the interval as capital concentration has deepened and the discourse around AI advancement has become more aspirational.
 *
 * PERSPECTIVAL GAP:
 *   The original gap is between capital's view of the constraint (coordination problem solved, value created) and marginal voices' view (pure extraction with no exit). The tangled rope perspective (professional knowledge workers) bridges these extremes by showing how a single structural arrangement can simultaneously coordinate and extract. Regulatory perspectives reveal that the apparent naturalness of the system is actually contingent on policy choices: copyright duration, training data licensing, algorithmic transparency, and compensation structures. The piton perspective shows that much of the discourse legitimating this system (AGI narratives, alignment focus, progress framing) has high theater content and low functional relationship to actual technical capability. The false mountain perspective reveals how naturalizing language ('knowledge wants to be free,' 'data is the new oil,' 'scaling is inevitable') obscures policy-dependent choices. Each perspective is structurally legitimate — they all see real features of the constraint — but the perspectival gap itself is diagnostic of the constraint's mandatrophy: the appearance of natural inevitability combined with genuine extraction mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction flow. Marginal voices have zero bargaining power, complete platform dependence, and no exit options (d → 0.95, high experienced extraction). Professional knowledge workers have moderate power, some exit optionality through institutional affiliation, but constrained by market dynamics (d → 0.55, moderate experienced extraction). Scaling capital has institutional power, arbitrage options across platforms and sectors, and positions itself as the beneficiary (d → 0.05, negative experienced extraction — benefits run toward this agent). Regulatory coalitions have organized power, constrained by political economy but building governance pathways (d → 0.45, moderate experienced extraction with declining trend). The AGI discourse ecosystem benefits from concentration and narrative control, with strong arbitrage options (d → 0.10, low experienced extraction). The algorithmic mediation layer enforces suppression through structural opacity rather than transparent coercion, which is a key mandatrophy feature: the constraint appears as a natural law of computation while actually encoding policy choices about which voices are visible.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves as Tangled Rope (not pure Snare or false Mountain) because it exhibits both genuine coordination function and asymmetric extraction. The coordination component is real: aggregated knowledge access, cross-domain pattern synthesis, and scaled inference do solve legitimate coordination problems that small-scale knowledge systems cannot. The extraction component is also real: marginal voices and even professional knowledge workers see their cognitive labor leveraged without compensation or control, suppression through algorithmic invisibility, and concentration of market value in scaling capital. The constraint is neither a natural law (Mountain) nor a temporary coordination failure (Scaffold), but a stable hybrid that will persist because both coordination and extraction are structurally maintained. The theater ratio (0.68) reflects that much of the legitimation for this arrangement is performative (AGI narratives, alignment focus) rather than grounded in actual capability gains or welfare improvements. Resolving the mandatrophy requires acknowledging that high-extraction coordination mechanisms (Tangled Rope) are often more stable and persistent than pure Snares, because the coordination benefits create enough beneficiary support to prevent the entire system from being delegitimated. The constraint is not a false summit (the coordination is real) but a genuine Tangled Rope with high suppression and high theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    banal_vs_emergent_capability,
    'Does the scaling of language models represent genuine emergent cognitive capability or increasingly sophisticated statistical interpolation over training distributions?',
    'Systematic evaluation of out-of-distribution generalization; measurement of novel reasoning vs recombination of training examples; comparison with symbolic reasoning systems on held-out domains',
    'If genuine emergence: the constraint is natural (Mountain). If sophisticated interpolation: the constraint is institutional (Tangled Rope or Snare). Classification stability depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(banal_vs_emergent_capability, empirical, 'Whether AI scaling produces emergent capability or sophisticated interpolation').

omega_variable(
    cognitive_commons_collapse,
    'Does training AI systems on human knowledge commons (text, code, images) constitute extraction of that commons or legitimate reuse?',
    'Empirical tracking of derivative works and copyright enforcement costs; analysis of institutional intent (licensing disclosure, compensation structures); measurement of harm to original creators through market substitution',
    'If extraction: victims and suppression scores are correct. If legitimate reuse: beneficiary status changes and suppression should be lower. Current assumption: extraction with suppression through platform opacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_commons_collapse, preference, 'Whether training data extraction constitutes rights violation').

omega_variable(
    algorithmic_banality_floor,
    'What is the intrinsic limit of synthetic knowledge generation before quality degradation becomes irreversible (model collapse, hallucination accumulation, loss of genuine diversity)?',
    'Longitudinal analysis of training data pollution from synthetic outputs; measurement of statistical divergence when models are trained on their own outputs; comparison of knowledge diversity in all-synthetic training regimes',
    'If floor is high (>90% synthetic data sustainable): banality capture may be permanent (Snare). If floor is low (<50% synthetic data causes collapse): the constraint has natural sunset (Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_banality_floor, empirical, 'Sustainability threshold for synthetic data in training').

omega_variable(
    superintelligence_framing_capture,
    'Is the superintelligence narrative (AGI race, existential risk discourse) a genuine technical prediction or an institutional legitimacy capture mechanism for capital concentration?',
    'Historical track record of AGI predictions vs delivered capabilities; analysis of funding concentration correlation with existential risk framing; measurement of discourse shift with capital availability',
    'If genuine prediction: theater ratio should be lower, and mountain/scaffold perspectives gain weight. If capture mechanism: theater ratio confirmed at 0.68+, and piton classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superintelligence_framing_capture, conceptual, 'Whether superintelligence framing reflects technical reality or institutional capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_banal_capture, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(banal_tr_t0, ai_banal_capture, theater_ratio, 0, 0.35).
narrative_ontology:measurement(banal_tr_t3, ai_banal_capture, theater_ratio, 3, 0.52).
narrative_ontology:measurement(banal_tr_t6, ai_banal_capture, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(banal_be_t0, ai_banal_capture, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(banal_be_t3, ai_banal_capture, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(banal_be_t6, ai_banal_capture, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_banal_capture, information_standard).
narrative_ontology:affects_constraint(ai_banal_capture, attention_concentration_mechanisms).
narrative_ontology:affects_constraint(ai_banal_capture, knowledge_commons_collapse).
narrative_ontology:affects_constraint(ai_banal_capture, derivative_labor_extraction).
narrative_ontology:affects_constraint(ai_banal_capture, platform_dependency_lock).

% DUAL FORMULATION NOTE:
% The banal cognitive engine is part of a constraint family involving platform dependency, attention economics, and knowledge extraction. Each family member has distinct ε values reflecting different aspects: attention concentration (ε ≈ 0.65, Snare), knowledge commons collapse (ε ≈ 0.52, Tangled Rope), derivative labor extraction (ε ≈ 0.48, Tangled Rope), platform dependency lock (ε ≈ 0.70, Snare). The banal engine (ε = 0.58) is the hybrid center representing the institutional synthesis of these mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_banal_capture, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
