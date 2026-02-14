% ============================================================================
% CONSTRAINT STORY: coalition_disinfo_framework_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_coalition_disinfo_framework_2026, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: coalition_disinfo_framework_2026
 *   human_readable: "Coalition of the Willing" Framework for AI Disinformation
 *   domain: technological/political
 *
 * SUMMARY:
 *   A proposed international framework, announced in a (fictional) 2026
 *   summit, designed to combat AI-generated disinformation. The framework
 *   combines technical standards for content authentication (a coordination
 *   function) with regulatory obligations for platforms to remove content
 *   deemed harmful or false (an enforcement/extraction function). It aims to
 *   protect democratic processes but creates a mechanism for centralized
 *   control over information.
 *
 * KEY AGENTS (by structural relationship):
 *   - Political Dissidents & Activists: Primary target (powerless/trapped) — their speech is most at risk of being suppressed under the label of "disinformation".
 *   - Small Tech Platforms: Secondary target (moderate/constrained) — face prohibitive compliance costs, creating a barrier to entry.
 *   - Coalition Governments: Primary beneficiary (institutional/arbitrage) — gain political stability and a tool to manage the information environment.
 *   - Major Technology Platforms: Inter-institutional beneficiary (institutional/constrained) — gain regulatory certainty and a competitive moat, but are also subject to the framework's obligations.
 *   - Analytical Observer: Sees the full dual-use structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coalition_disinfo_framework_2026, 0.48).
domain_priors:suppression_score(coalition_disinfo_framework_2026, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(coalition_disinfo_framework_2026, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coalition_disinfo_framework_2026, extractiveness, 0.48).
narrative_ontology:constraint_metric(coalition_disinfo_framework_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(coalition_disinfo_framework_2026, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(coalition_disinfo_framework_2026, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(coalition_disinfo_framework_2026). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(coalition_disinfo_framework_2026, coalition_governments).
narrative_ontology:constraint_beneficiary(coalition_disinfo_framework_2026, major_tech_platforms).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(coalition_disinfo_framework_2026, political_dissidents_and_activists).
narrative_ontology:constraint_victim(coalition_disinfo_framework_2026, small_tech_platforms).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% For a political dissident, the framework is a censorship tool. The coordination
% benefits are irrelevant compared to the threat of suppression.
% Engine derives d from victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.48 * 1.42 * 1.0 (national scope) ≈ 0.68. This high χ and high suppression (0.65) classify it as a Snare.
constraint_indexing:constraint_classification(coalition_disinfo_framework_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For a coalition government, this is a pure coordination tool for political stability.
% Engine derives d from beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.48 * -0.12 * 1.0 ≈ -0.06. This negative effective extraction classifies it as a Rope.
constraint_indexing:constraint_classification(coalition_disinfo_framework_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the genuine coordination function and the asymmetric extraction.
% Engine uses canonical d ≈ 0.72 for analytical → f(d) ≈ 1.15. Global scope amplifies χ.
% χ = 0.48 * 1.15 * 1.2 (global scope) ≈ 0.66.
% ε ≥ 0.30, suppression ≥ 0.40, and 0.40 ≤ χ ≤ 0.90, plus all structural flags are met -> Tangled Rope.
constraint_indexing:constraint_classification(coalition_disinfo_framework_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL ACTOR (ROPE)
% For a major tech platform, this is a costly but beneficial coordination mechanism.
% They are beneficiaries, but with constrained exit. The engine derives a higher d
% than for governments, but still low enough to perceive it as Rope.
% Derivation: beneficiary + constrained exit -> d ~ 0.3. f(d) ~ 0.2.
% χ = 0.48 * 0.2 * 1.1 (continental scope) ≈ 0.11. This is a Rope.
constraint_indexing:constraint_classification(coalition_disinfo_framework_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coalition_disinfo_framework_2026_tests).

test(perspectival_gap_target_vs_beneficiary, [nondet]) :-
    % Verify the core perspectival gap between the target (dissident) and beneficiary (government).
    constraint_indexing:constraint_classification(coalition_disinfo_framework_2026, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(coalition_disinfo_framework_2026, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(coalition_disinfo_framework_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_met) :-
    % Verify that all three structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(coalition_disinfo_framework_2026, _),
    narrative_ontology:constraint_victim(coalition_disinfo_framework_2026, _),
    domain_priors:requires_active_enforcement(coalition_disinfo_framework_2026).

:- end_tests(coalition_disinfo_framework_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect a dual-use system. The base extractiveness (ε=0.48) is high
 *   because the power to define and suppress "disinformation" is inherently
 *   extractive of speech rights. The suppression score (0.65) is high because
 *   the framework's effectiveness depends on its ability to remove non-compliant
 *   content and marginalize platforms that don't adhere to the standards. The
 *   theater ratio (0.20) is low because at its inception, the mechanism is
 *   intended to be functional, not merely performative. This combination of
 *   genuine coordination and high extractive potential is characteristic of a
 *   Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a government (beneficiary), the constraint is a Rope
 *   that solves the coordination problem of "how to maintain public trust and order".
 *   The extraction is seen as a necessary feature for enforcement. For a dissident
 *   (target), the constraint is a Snare that solves the government's problem of "how
 *   to silence dissent". The coordination aspect is merely the justification for a
 *   censorship machine. Their lived experience is one of pure, asymmetric coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'coalition_governments' and 'major_tech_platforms' benefit from stability, regulatory clarity, and control.
 *   - Victims: 'political_dissidents_and_activists' and 'small_tech_platforms' bear the costs through suppressed speech and prohibitive compliance burdens.
 *   This clear division of costs and benefits is what drives the directionality calculation and the resulting perspectival gap.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The framework illustrates a key inter-institutional dynamic between state and
 *   corporate actors. Both are 'institutional' beneficiaries, but with different
 *   exit options. Governments have 'arbitrage' (they set the rules). Major platforms
 *   have 'constrained' exit (they must comply to operate in major markets). This
 *   subtle difference is captured by the directionality derivation, resulting in
 *   both classifying the constraint as a Rope, but the platforms experiencing slightly
 *   higher (less negative) effective extraction due to their subordinate position.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework is a classic candidate for mandatrophy. It is presented as a
 *   Rope (solving a coordination problem) to legitimize what is, for many, a Snare
 *   (extracting rights). The Tangled Rope classification by the analytical observer
 *   is crucial because it correctly identifies BOTH functions are present and that
 *   the coordination mandate serves as cover for the extractive mechanism. It
 *   prevents the system from being mislabeled as either pure coordination or pure
 *   extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_coalition_disinfo_framework_2026,
    'Is the primary intent of the framework coordination (protecting democracy) or control (suppressing dissent)?',
    'Analysis of takedown requests and enforcement actions over a 5-year period. If enforcement disproportionately targets non-violent political opposition vs. foreign state actors, the intent is likely control.',
    'If primarily coordination, ε might be lower (~0.35). If primarily control, ε is accurate or even understated (~0.55). This could shift the analytical classification between a weak and strong Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(coalition_disinfo_framework_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is > 0.46, so temporal data is required.
% This models a typical lifecycle drift where a system, established with
% a mix of intentions, becomes progressively more extractive and theatrical
% as its original function is captured by institutional interests.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(cdfw_tr_t0, coalition_disinfo_framework_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cdfw_tr_t5, coalition_disinfo_framework_2026, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cdfw_tr_t10, coalition_disinfo_framework_2026, theater_ratio, 10, 0.20).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(cdfw_ex_t0, coalition_disinfo_framework_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(cdfw_ex_t5, coalition_disinfo_framework_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cdfw_ex_t10, coalition_disinfo_framework_2026, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: While it involves an information standard, the core
% function is coercive, making enforcement_mechanism the most appropriate type.
narrative_ontology:coordination_type(coalition_disinfo_framework_2026, enforcement_mechanism).

% Network relationships: This framework directly impacts and alters the legal
% landscape for platform liability, making it coupled with constraints like
% the USA's Section 230.
narrative_ontology:affects_constraint(coalition_disinfo_framework_2026, platform_liability_shield_cda230).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation chain,
% using beneficiary/victim declarations combined with the distinct exit options
% for governments (arbitrage) and platforms (constrained), accurately models
% the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */