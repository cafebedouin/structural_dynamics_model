% ============================================================================
% CONSTRAINT STORY: sk_ai_act_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_sk_ai_act_2026, []).

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
 *   constraint_id: sk_ai_act_2026
 *   human_readable: South Korea's Proposed AI Industry Promotion Act
 *   domain: technological/political
 *
 * SUMMARY:
 *   A proposed South Korean law designed to accelerate the national AI industry
 *   by creating a "legal safe harbor." The act grants AI developers broad
 *   exemptions from existing privacy and copyright law, most notably allowing
 *   them to collect and process personal data without consent for research
 *   purposes. This creates a direct conflict between industrial policy goals
 *   and individual rights.
 *
 * KEY AGENTS (by structural relationship):
 *   - South Korean Citizens: Primary target (powerless/trapped) — their personal data is extracted without consent or compensation.
 *   - South Korean AI Startups: Primary beneficiary (organized/mobile) — receive a significant reduction in data acquisition costs and legal risk.
 *   - South Korean Government: Architect & Secondary Beneficiary (institutional/constrained) — seeks national economic and technological advantage.
 *   - Civil Society & Privacy Advocates: Analytical observer (analytical/analytical) — identifies the full structure and opposes the rights infringement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sk_ai_act_2026, 0.55).
domain_priors:suppression_score(sk_ai_act_2026, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(sk_ai_act_2026, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sk_ai_act_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(sk_ai_act_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(sk_ai_act_2026, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sk_ai_act_2026, tangled_rope).
narrative_ontology:human_readable(sk_ai_act_2026, "South Korea's Proposed AI Industry Promotion Act").
narrative_ontology:topic_domain(sk_ai_act_2026, "technological/political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(sk_ai_act_2026). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(sk_ai_act_2026, sk_ai_startups).
narrative_ontology:constraint_beneficiary(sk_ai_act_2026, sk_government).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(sk_ai_act_2026, sk_citizens).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are met)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (CITIZENS)
% As victims with trapped exit, their derived d is high (~0.95), leading to
% high effective extraction (χ), classifying the law as a Snare.
constraint_indexing:constraint_classification(sk_ai_act_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (AI STARTUPS)
% As beneficiaries with mobile exit, their derived d is low, leading to
% low/negative effective extraction (χ). The law is a pure coordination
% mechanism (Rope) that solves the problem of costly data acquisition.
constraint_indexing:constraint_classification(sk_ai_act_2026, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (CIVIL SOCIETY)
% The analytical view sees both the coordination function and the asymmetric
% extraction. The high base extraction (ε) and suppression, combined with
% the existence of clear beneficiaries and victims, makes this a Tangled Rope.
constraint_indexing:constraint_classification(sk_ai_act_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% Perspective 4: THE ARCHITECT (GOVERNMENT)
% As an institutional beneficiary with constrained exit options, the government
% also sees the law as a Rope—a tool of industrial policy to coordinate and
% subsidize a strategic sector for national benefit. Its directionality is
% slightly less favorable than the startups' due to its constrained exit.
constraint_indexing:constraint_classification(sk_ai_act_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sk_ai_act_2026_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(sk_ai_act_2026, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(sk_ai_act_2026, rope, context(agent_power(organized), _, exit_options(mobile), _)),
    true.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(sk_ai_act_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(sk_ai_act_2026, _),
    narrative_ontology:constraint_victim(sk_ai_act_2026, _),
    domain_priors:requires_active_enforcement(sk_ai_act_2026).

:- end_tests(sk_ai_act_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. The law enables the direct, uncompensated appropriation of personal data, a valuable resource for training AI models. This is a significant transfer of value from citizens to corporations.
 *   - Suppression (S=0.75): High. The law is explicitly designed to override and create exemptions to existing privacy regulations (like PIPA), suppressing citizens' ability to seek recourse under those prior frameworks.
 *   - The combination of a genuine coordination function (reducing legal friction for startups) and high, asymmetric extraction from a non-consenting third party makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For AI startups (beneficiaries), the law is a Rope: a coordination tool that lowers barriers to entry and innovation. For citizens (victims), it's a Snare: a coercive mechanism that extracts their private data without consent or recourse. The government, as architect, also sees a Rope, viewing it as a legitimate instrument of national industrial policy. This disagreement is central to the political conflict around the law.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the explicit beneficiary/victim declarations.
 *   - `sk_citizens` are victims with `trapped` exit, yielding a high directionality `d` (~0.95) and high effective extraction χ.
 *   - `sk_ai_startups` are beneficiaries with `mobile` exit, yielding a low `d` (~0.15) and thus low/negative χ.
 *   - The `sk_government` is a beneficiary with `constrained` exit, resulting in a low `d` that is slightly higher than the startups', but still firmly in Rope territory.
 *   These structural relationships correctly model the flow of value and cost within the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two key errors. First, it doesn't dismiss the law as a pure Snare, which would ignore its genuine (and intended) coordination function for the AI industry. Second, it doesn't accept the "industrial promotion" framing at face value (a pure Rope), which would ignore the severe, coercive extraction imposed on citizens. The Tangled Rope classification captures this duality, identifying it as a system that achieves coordination *through* extraction. The analytical perspective correctly identifies the mixed nature that is hidden from the direct participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_sk_ai_act_2026,
    'Does the economic growth from AI industry promotion measurably outweigh the societal cost of systemic privacy erosion?',
    'A 10-year longitudinal study comparing South Korea''s AI sector performance and metrics of social trust/privacy against EU nations under GDPR.',
    'If growth is substantial, the law could be framed as a harsh but effective industrial policy (Tangled Rope). If growth is negligible, the law is a failed policy whose primary effect was extraction (a de facto Snare from all but the beneficiary view).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sk_ai_act_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for a high-extraction constraint (ε=0.55 > 0.46). Models
% the law's lifecycle from introduction (T=0) to maturity (T=10).

% Theater ratio over time: The "trustworthiness" aspect likely becomes more
% pronounced over time as a public justification.
narrative_ontology:measurement(sk_ai_act_2026_tr_t0, sk_ai_act_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sk_ai_act_2026_tr_t5, sk_ai_act_2026, theater_ratio, 5, 0.20).
narrative_ontology:measurement(sk_ai_act_2026_tr_t10, sk_ai_act_2026, theater_ratio, 10, 0.30).

% Extraction over time: Rent-seeking could slightly increase extraction as
% incumbent firms lobby to expand the law's scope.
narrative_ontology:measurement(sk_ai_act_2026_ex_t0, sk_ai_act_2026, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sk_ai_act_2026_ex_t5, sk_ai_act_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(sk_ai_act_2026_ex_t10, sk_ai_act_2026, base_extractiveness, 10, 0.58).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The law standardizes and allocates a key resource (data).
narrative_ontology:coordination_type(sk_ai_act_2026, resource_allocation).

% Network relationships: This act structurally weakens existing privacy law.
narrative_ontology:affects_constraint(sk_ai_act_2026, sk_pipa_privacy_law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% perspectival differences between citizens, startups, and the government.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */