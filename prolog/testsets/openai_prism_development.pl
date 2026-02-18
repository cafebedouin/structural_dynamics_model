% ============================================================================
% CONSTRAINT STORY: openai_prism_development
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-12-14
% ============================================================================

:- module(constraint_openai_prism_development, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: openai_prism_development
 *   human_readable: Information Asymmetry in Frontier AI Model Development (OpenAI's "Prism"/GPT-5)
 *   domain: technological
 *
 * SUMMARY:
 *   The development of OpenAI's next-generation "Prism" model (GPT-5) represents a
 *   structural constraint characterized by extreme information asymmetry. A small,
 *   private consortium (OpenAI and Microsoft) controls access, training data,
 *   safety testing, and capability disclosure, while the broader ecosystem of
 *   competitors, researchers, regulators, and the public bears the disruptive
 *   risks and impacts without commensurate insight or control. This structure
 *   simultaneously solves a coordination problem (creating a new technological
 *   platform) while extracting immense value and concentrating power.
 *
 * KEY AGENTS (by structural relationship):
 *   - OpenAI & Microsoft: Primary beneficiary (institutional/arbitrage) — Control the technology, narrative, and timeline, capturing market value.
 *   - Independent AI Developers & Researchers: Primary target (powerless/trapped) — Face existential business/research risk from a technology they cannot access or audit.
 *   - Government Regulators: Inter-institutional victim (institutional/constrained) — Tasked with governing a technology whose capabilities and risks are opaque to them.
 *   - Analytical Observer: Sees the full dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_prism_development, 0.55).
domain_priors:suppression_score(openai_prism_development, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(openai_prism_development, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_prism_development, extractiveness, 0.55).
narrative_ontology:constraint_metric(openai_prism_development, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(openai_prism_development, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(openai_prism_development, tangled_rope).
narrative_ontology:topic_domain(openai_prism_development, "technological").
narrative_ontology:human_readable(openai_prism_development, "Information Asymmetry in Frontier AI Model Development (OpenAI's \"Prism\"/GPT-5)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(openai_prism_development). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(openai_prism_development, openai_microsoft_nexus).
narrative_ontology:constraint_beneficiary(openai_prism_development, early_access_enterprise_customers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(openai_prism_development, independent_ai_developers).
narrative_ontology:constraint_victim(openai_prism_development, public_knowledge_commons).
narrative_ontology:constraint_victim(openai_prism_development, government_regulators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (INDEPENDENT AI DEVELOPERS)
% Experiences the constraint as a pure Snare. Their business models and research
% paths are threatened by a superior, inaccessible technology whose development
% extracts value from the public data they also rely on.
% Engine derives: victim + trapped -> d ≈ 0.95 -> f(d) ≈ 1.42 -> high χ
% χ = 0.55 * 1.42 * 1.2 (global scope) ≈ 0.94
constraint_indexing:constraint_classification(openai_prism_development, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (OPENAI & MICROSOFT)
% Experiences the constraint as a pure coordination mechanism (Rope) for building
% the next technological platform. The extractive element is invisible or justified
% as a necessary cost of innovation.
% Engine derives: beneficiary + arbitrage -> d ≈ 0.05 -> f(d) ≈ -0.12 -> negative χ
% χ = 0.55 * -0.12 * 1.2 ≈ -0.08
constraint_indexing:constraint_classification(openai_prism_development, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Recognizes both the genuine coordination function (creating a powerful new tool)
% and the severe asymmetric extraction (market concentration, risk externalization).
% This dual nature is the hallmark of a Tangled Rope.
% Engine derives: analytical -> d ≈ 0.73 -> f(d) ≈ 1.15
% χ = 0.55 * 1.15 * 1.2 ≈ 0.76
constraint_indexing:constraint_classification(openai_prism_development, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: GOVERNMENT REGULATORS
% An institutional actor, but one that is a victim of the information asymmetry.
% They are forced to regulate reactively with incomplete data. Their exit is
% 'constrained' as they cannot opt out of their duty but have limited tools.
% The engine derives a 'd' value between the beneficiary and the trapped target.
% Engine derives: victim + institutional power + constrained exit -> d ≈ 0.65 -> f(d) ≈ 1.0
% χ = 0.55 * 1.0 * 1.1 (continental scope, e.g., EU AI Act) ≈ 0.61
constraint_indexing:constraint_classification(openai_prism_development, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_prism_development_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap: Snare for the target, Rope for the beneficiary.
    constraint_indexing:constraint_classification(openai_prism_development, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(openai_prism_development, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % The analytical observer must correctly identify the mixed nature of the constraint.
    constraint_indexing:constraint_classification(openai_prism_development, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that the structural data required for Tangled Rope classification exists.
    narrative_ontology:constraint_beneficiary(openai_prism_development, _),
    narrative_ontology:constraint_victim(openai_prism_development, _),
    domain_priors:requires_active_enforcement(openai_prism_development).

:- end_tests(openai_prism_development_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High, reflecting the value captured from public data commons and the market concentration enabled by controlling a frontier model.
 *   - Suppression (s=0.75): High, due to the immense capital/data requirements, trade secrecy, and network effects that create formidable barriers to entry for competitors.
 *   - Active Enforcement: The constraint is maintained by NDAs, API access controls, and intellectual property law, meeting a key requirement for Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For OpenAI/Microsoft (beneficiary), this is a Rope: a colossal engineering effort to coordinate resources and create a new utility. For an independent developer (target), it is a Snare: a black box that can render their work obsolete overnight, built on the public commons they cannot access at the same scale. The beneficiary sees innovation; the target sees enclosure and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `openai_microsoft_nexus` directly profits and controls the asset. `early_access_enterprise_customers` gain a competitive edge.
 *   - Victims: `independent_ai_developers` face existential risk. `public_knowledge_commons` is the resource being extracted without compensation. `government_regulators` are structurally disadvantaged by the information asymmetry, bearing the cost of managing societal risk without full insight. These declarations drive the directionality `d` and thus the effective extraction `χ` for each perspective.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The regulator perspective highlights a key modern dynamic. Both OpenAI and the regulator are 'institutional' actors, but their relationship to the constraint is fundamentally different. OpenAI has `arbitrage` exit (it controls the terms), while the regulator has `constrained` exit (it must engage but on terms set by the developer). This difference in exit options, combined with their respective beneficiary/victim status, allows the engine to derive different `d` values and correctly classify the constraint as a Rope for one and a Tangled Rope for the other, quantifying the structural power imbalance.
 *
 * MANDATROPHY ANALYSIS:
 *   This model prevents misclassification. A simplistic analysis might label frontier AI development as a pure Snare (focusing only on data extraction) or a pure Rope (focusing only on technological progress). The Tangled Rope classification, derived from the analytical perspective, correctly identifies that it is BOTH: a system that performs a genuine, powerful coordination function that is inextricably linked to an asymmetric extractive process. This is the definition of mandatrophy's core subject.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_prism_capabilities,
    'Are the emergent capabilities of "Prism", particularly autonomous agency, a predictable scaling effect or an uncontrollable, qualitative phase transition?',
    'Independent, adversarial, and public auditing of the model prior to widespread deployment.',
    'If predictable -> Tangled Rope (manageable risks). If phase transition -> a potential civilizational-scale Snare (unmanageable externalized risk).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(openai_prism_development, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over time, demonstrating 'extraction_accumulation'
% as the technology moved from academic research to a highly commercialized asset.
% Base extraction is > 0.46, so temporal data is required.

% Theater ratio over time (stable and low, function dominates):
narrative_ontology:measurement(prism_tr_t0, openai_prism_development, theater_ratio, 0, 0.10).
narrative_ontology:measurement(prism_tr_t5, openai_prism_development, theater_ratio, 5, 0.15).
narrative_ontology:measurement(prism_tr_t10, openai_prism_development, theater_ratio, 10, 0.20).

% Extraction over time (shows clear accumulation):
% T=0: GPT-3 era, more academic/research focus
% T=5: GPT-4 era, heavy commercialization and API monetization
% T=10: Prism/GPT-5 era, aiming for market dominance and platform lock-in
narrative_ontology:measurement(prism_ex_t0, openai_prism_development, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prism_ex_t5, openai_prism_development, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(prism_ex_t10, openai_prism_development, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint functions as a new layer of digital infrastructure.
narrative_ontology:coordination_type(openai_prism_development, global_infrastructure).

% Network relationships (structural influence edges)
% The development of such a model directly increases society's vulnerability
% to sophisticated, automated disinformation campaigns.
narrative_ontology:affects_constraint(openai_prism_development, global_disinformation_vulnerability).
narrative_ontology:affects_constraint(openai_prism_development, knowledge_worker_labor_market).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation chain,
% using beneficiary/victim declarations and exit options, accurately captures
% the directionality for all key agents, including the nuanced difference
% between the two institutional actors (developer vs. regulator).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */