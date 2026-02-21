% ============================================================================
% CONSTRAINT STORY: political_art_factional_alignment
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-25
% ============================================================================

:- module(constraint_political_art_factional_alignment, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: political_art_factional_alignment
 *   human_readable: "Factional Alignment Requirement for Political Art"
 *   domain: economic/technological
 *
 * SUMMARY:
 *   In a politically polarized media market, commercial art dealing with
 *   divisive figures or topics must align with a sufficiently large and
 *   monetizable audience faction to achieve success. Attempts to create
 *   "centrist" or "humanizing" portraits of such figures often fail, as they
 *   alienate both opposing factions, leading to box office failure. This
 *   constraint extracts the production and marketing budget from studios that
 *   miscalculate this alignment.
 *
 * KEY AGENTS (by structural relationship):
 *   - Independent Artist/Filmmaker: Primary powerless target (powerless/trapped) — faces creative and financial ruin from project failure.
 *   - Content Producers of Politically Ambivalent Art: Institutional target (institutional/constrained) — bears the financial extraction from the project's failure.
 *   - Risk-Averse Media Conglomerates: Primary beneficiary (institutional/arbitrage) — benefits from a predictable market where "safe," non-polarizing content is rewarded and competitors' risky projects fail.
 *   - Political Pundits/Activists: Secondary beneficiaries (organized/mobile) - leverage the failure to reinforce their own narratives.
 *   - Analytical Observer: Sees the full structure of market coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_art_factional_alignment, 0.62).
domain_priors:suppression_score(political_art_factional_alignment, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(political_art_factional_alignment, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_art_factional_alignment, extractiveness, 0.62).
narrative_ontology:constraint_metric(political_art_factional_alignment, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(political_art_factional_alignment, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(political_art_factional_alignment, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(political_art_factional_alignment).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(political_art_factional_alignment). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(political_art_factional_alignment, risk_averse_media_conglomerates).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(political_art_factional_alignment, content_producers_of_politically_ambivalent_art).
narrative_ontology:constraint_victim(political_art_factional_alignment, independent_artists_and_filmmakers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE POWERLESS TARGET (INDEPENDENT ARTIST)
% For an independent creator without institutional backing, this constraint is a
% near-total barrier to entry for nuanced political art. Failure means
% financial ruin. The market dynamic is an absolute Snare.
constraint_indexing:constraint_classification(political_art_factional_alignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTIONAL TARGET (THE FAILED STUDIO)
% As a victim with constrained exit options due to sunk costs, the studio
% also experiences the market dynamic as a highly extractive snare.
constraint_indexing:constraint_classification(political_art_factional_alignment, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (RIVAL CONGLOMERATE)
% As a beneficiary with arbitrage exit (they can choose to fund safe projects),
% a rival studio sees this market dynamic as a useful coordinating signal,
% hence a Rope. The negative effective extraction (χ) reflects that the
% constraint subsidizes their risk-averse strategy.
constraint_indexing:constraint_classification(political_art_factional_alignment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analyst sees both the coordination function (for beneficiaries) and the
% asymmetric extraction (from victims), classifying it as a Tangled Rope. This
% is the ground-truth classification for the system.
constraint_indexing:constraint_classification(political_art_factional_alignment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_art_factional_alignment_tests).

test(perspectival_gap_victim_beneficiary) :-
    % Verify perspectival gap between target studio and beneficiary conglomerate.
    constraint_indexing:constraint_classification(political_art_factional_alignment, snare, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(political_art_factional_alignment, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(political_art_factional_alignment, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that all three conditions for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(political_art_factional_alignment, _),
    narrative_ontology:constraint_victim(political_art_factional_alignment, _),
    domain_priors:requires_active_enforcement(political_art_factional_alignment).

:- end_tests(political_art_factional_alignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.62): Represents the high probability of losing a significant portion of a film's budget (production + marketing) if it fails to align with a political faction. The failure of a politically ambivalent film is a case in point, where the investment is extracted by market indifference.
 *   - Suppression (0.75): This market dynamic strongly suppresses nuanced, centrist, or artistically ambitious takes on polarized subjects, forcing creators towards safe, pre-approved narratives for a given faction. This high score reflects the chilling effect on creative freedom.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The studio that produces a failed ambivalent film (the institutional victim) experiences the constraint as a Snare—an unpredictable trap that consumed their capital. In contrast, a rival media conglomerate (the beneficiary) sees it as a Rope—a reliable coordination mechanism that validates their strategy of avoiding controversial projects and reinforces market stability. The difference is driven entirely by their structural relationship (victim vs. beneficiary) and exit options (constrained by sunk costs vs. arbitrage).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are large, risk-averse media companies that profit from a predictable market. The constraint helps them by punishing competitors who take risks on divisive content, thereby clearing the field. The victims are the producers, creators, and investors who attempt to bridge the political divide or misjudge their target audience, bearing the full cost of the market's rejection.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework prevents misclassification. A naive view might see this as simple market failure ("a bad movie flopped"). A cynical view might see it as a pure Snare ("the culture war kills art"). The Tangled Rope classification from the analytical perspective is more accurate because it captures the dual nature of the constraint: it simultaneously functions as a genuine coordination mechanism for incumbent players (a Rope) while acting as a brutal extraction mechanism on others (a Snare). It's a system of order, but that order is maintained via asymmetric punishment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_political_art_factional_alignment,
    'Is this factional alignment constraint a permanent feature of polarized information ecosystems, or a transient market phase?',
    'Observing the box office performance of politically-themed films over the next decade, particularly those released on new distribution platforms that bypass traditional media gatekeepers.',
    'If permanent, it approaches a Mountain of social physics for this era. If transient, it remains a Tangled Rope that could eventually be dismantled or replaced.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_political_art_factional_alignment, empirical, 'Is the constraint a permanent feature of polarized markets or a transient phase?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(political_art_factional_alignment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.62 > 0.46), so temporal data is
% required. The data models the intensification of media polarization
% from the mid-2000s (T=0) to the mid-2020s (T=10).

% Theater ratio over time:
narrative_ontology:measurement(pa_tr_t0, political_art_factional_alignment, theater_ratio, 0, 0.10).
narrative_ontology:measurement(pa_tr_t5, political_art_factional_alignment, theater_ratio, 5, 0.15).
narrative_ontology:measurement(pa_tr_t10, political_art_factional_alignment, theater_ratio, 10, 0.20).

% Extraction over time:
narrative_ontology:measurement(pa_ex_t0, political_art_factional_alignment, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(pa_ex_t5, political_art_factional_alignment, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(pa_ex_t10, political_art_factional_alignment, base_extractiveness, 10, 0.62).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint coordinates capital by signaling which projects are too risky.
narrative_ontology:coordination_type(political_art_factional_alignment, resource_allocation).

% Network relationships (structural influence edges)
% This constraint is likely influenced by broader social trends and, in turn,
% influences studio policy. For example:
% narrative_ontology:affects_constraint(social_media_algorithmic_polarization, political_art_factional_alignment).
% narrative_ontology:affects_constraint(political_art_factional_alignment, studio_film_greenlighting_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models
% the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */