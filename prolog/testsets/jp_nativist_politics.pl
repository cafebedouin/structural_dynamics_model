% ============================================================================
% CONSTRAINT STORY: jp_nativist_politics
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_jp_nativist_politics, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jp_nativist_politics
 *   human_readable: "Nativist Online Rhetoric as a Pathway to Political Power in Japan"
 *   domain: political
 *
 * SUMMARY:
 *   This constraint models the political strategy, exemplified by figures like Mio
 *   Sugita of Japan's Liberal Democratic Party (L.D.P.), of using inflammatory,
 *   nativist, and anti-minority rhetoric online to build a political base and
 *   gain mainstream influence. The mechanism leverages online outrage cycles and
 *   historical revisionism to coordinate a nationalist base, while imposing
 *   severe social and psychological costs on targeted minority groups. The
 *   ruling party's tolerance of this behavior makes it a viable, if
 *   controversial, career path.
 *
 * KEY AGENTS (by structural relationship):
 *   - Japanese minority groups (LGBTQ+, Zainichi Koreans, feminists): Primary target (powerless/trapped) — bears extraction of social standing, safety, and dignity.
 *   - Nationalist politicians (e.g., Mio Sugita) and the "net uyoku": Primary beneficiary (organized/arbitrage) — gains political capital, media attention, and electoral success.
 *   - L.D.P. establishment: Secondary beneficiary (institutional/arbitrage) — benefits from a mobilized right-wing base and a rightward shift in the Overton window, while maintaining plausible deniability.
 *   - External policy analyst: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jp_nativist_politics, 0.68).
domain_priors:suppression_score(jp_nativist_politics, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(jp_nativist_politics, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jp_nativist_politics, extractiveness, 0.68).
narrative_ontology:constraint_metric(jp_nativist_politics, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jp_nativist_politics, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(jp_nativist_politics, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(jp_nativist_politics). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(jp_nativist_politics, japanese_nationalist_politicians).
narrative_ontology:constraint_beneficiary(jp_nativist_politics, ldp_right_wing_establishment).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(jp_nativist_politics, japanese_lgbtq_community).
narrative_ontology:constraint_victim(jp_nativist_politics, zainichi_koreans).
narrative_ontology:constraint_victim(jp_nativist_politics, japanese_feminists).

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
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% From the perspective of a targeted minority, this is a coercive trap that
% extracts their sense of safety and social legitimacy for others' political gain.
constraint_indexing:constraint_classification(jp_nativist_politics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% For a politician like Sugita, this is a pure coordination tool (a Rope) for
% mobilizing a base and building a political career. The extractive costs are
% externalized and thus invisible from this index.
constraint_indexing:constraint_classification(jp_nativist_politics, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both the coordination function and the
% asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% The high scope modifier amplifies the perceived extraction.
constraint_indexing:constraint_classification(jp_nativist_politics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jp_nativist_politics_tests).

test(perspectival_gap) :-
    % Verify the core Rope/Snare gap between beneficiary and victim.
    constraint_indexing:constraint_classification(jp_nativist_politics, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(jp_nativist_politics, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(jp_nativist_politics, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements) :-
    % Verify that all structural requirements for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(jp_nativist_politics),
    narrative_ontology:constraint_beneficiary(jp_nativist_politics, _),
    narrative_ontology:constraint_victim(jp_nativist_politics, _).

:- end_tests(jp_nativist_politics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.68): High. The constraint extracts tangible political
 *     power, media attention, and campaign funding, concentrating it in the hands
 *     of nationalist actors. It simultaneously extracts intangible but critical
 *     resources—safety, dignity, social standing—from targeted groups.
 *   - Suppression Score (0.75): High. The strategy relies on online harassment,
 *     misinformation ("fake news"), and delegitimization campaigns to silence
 *     journalists, academics, and activists who present alternative viewpoints.
 *     This creates a hostile environment for fact-based discourse, effectively
 *     suppressing alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the beneficiary (e.g., Mio Sugita), the system is a
 *   highly effective Rope for coordinating a political base and achieving power.
 *   The extractive costs are entirely externalized onto others. For the victim
 *   (e.g., an LGBTQ+ person), the system is a Snare—a coercive environment that
 *   leverages their identity to create political energy for their opponents,
 *   with no escape within the national context. The analytical observer sees
 *   both the coordination and the extraction, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The flow is unambiguous. Political and social capital flows from the public
 *   sphere and targeted minority groups *to* the nationalist politicians and their
 *   backers. The `constraint_beneficiary` and `constraint_victim` declarations
 *   directly model this structural relationship, allowing the engine to derive
 *   the correct directionality (`d`) values that produce the Rope/Snare gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly captures the dual nature of the phenomenon,
 *   avoiding two critical errors. Classifying it as a pure Snare would miss its
 *   potent function as a coordination mechanism for the political right.
 *   Classifying it as a pure Rope would dangerously ignore the severe, targeted
 *   extraction imposed on vulnerable populations. The Tangled Rope classification
 *   is essential for understanding how a mechanism can be simultaneously
 *   functional for one group and predatory toward another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_jp_nativist_politics,
    'Is the L.D.P. establishment''s role one of active cultivation or passive tolerance of this strategy?',
    'Internal party communications, minutes from leadership meetings, or whistleblower testimony.',
    'If active cultivation, base extractiveness (ε) would be higher (~0.75), suggesting a more deliberately designed Snare. If passive tolerance, the current ε=0.68 holds, suggesting an emergent but still highly extractive phenomenon.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(jp_nativist_politics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This strategy has intensified over the last two decades with the rise of
% online political organization. This data models its evolution from a more
% performative, fringe activity to a functionally integrated political tool.

% Theater ratio over time (declined as the strategy became more effective):
narrative_ontology:measurement(jp_nativist_politics_tr_t0, jp_nativist_politics, theater_ratio, 0, 0.50).
narrative_ontology:measurement(jp_nativist_politics_tr_t5, jp_nativist_politics, theater_ratio, 5, 0.40).
narrative_ontology:measurement(jp_nativist_politics_tr_t10, jp_nativist_politics, theater_ratio, 10, 0.30).

% Extraction over time (increased as the pathway to power proved viable):
narrative_ontology:measurement(jp_nativist_politics_ex_t0, jp_nativist_politics, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jp_nativist_politics_ex_t5, jp_nativist_politics, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(jp_nativist_politics_ex_t10, jp_nativist_politics, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint functions by creating and enforcing a
% shared narrative and set of political priorities for a diffuse online group.
narrative_ontology:coordination_type(jp_nativist_politics, information_standard).

% Network relationships: This political strategy directly impacts policy and
% social frameworks concerning gender and minority rights.
narrative_ontology:affects_constraint(jp_nativist_politics, jp_gender_equality_policy).
narrative_ontology:affects_constraint(jp_nativist_politics, jp_minority_rights_framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately captures the
% directionality of the political and social capital flows.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */