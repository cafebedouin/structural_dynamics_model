% ============================================================================
% CONSTRAINT STORY: gaza_border_control_rafah
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_gaza_border_control_rafah, []).

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
 *   constraint_id: gaza_border_control_rafah
 *   human_readable: Control regime over the Gaza-Egypt (Rafah) border crossing
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint represents the policy of closure and controlled opening of the
 *   Rafah border crossing, the primary exit point for the 2 million residents
 *   of the Gaza Strip not leading to Israel. The policy is jointly enforced by
 *   Israel and Egypt as part of a broader blockade, citing security concerns.
 *   The closure extracts immense value (economic, humanitarian, freedom of movement)
 *   from the Gazan population while providing a security and political coordination
 *   mechanism for the enforcing states.
 *
 * KEY AGENTS (by structural relationship):
 *   - Gazan Civilians: Primary target (powerless/trapped) — bears full extraction of the closure.
 *   - Israeli Security Establishment: Primary beneficiary (institutional/arbitrage) — uses the constraint as a key tool for security control and political leverage.
 *   - Egyptian State Security: Inter-institutional beneficiary (institutional/constrained) — co-enforces the closure for its own security benefits but faces regional political constraints.
 *   - Gazan Governing Authority (e.g. Hamas): Secondary target (organized/constrained) — is a target of the policy but also has some agency in managing the Gazan side of the crossing.
 *   - Analytical Observer: A UN or NGO analyst — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaza_border_control_rafah, 0.55).
domain_priors:suppression_score(gaza_border_control_rafah, 0.90).   % Structural property (raw, unscaled). Alternatives (sea/air travel, other borders) are actively suppressed.
domain_priors:theater_ratio(gaza_border_control_rafah, 0.20).       % The closure has a direct, non-performative function (physical control).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaza_border_control_rafah, extractiveness, 0.55).
narrative_ontology:constraint_metric(gaza_border_control_rafah, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(gaza_border_control_rafah, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(gaza_border_control_rafah, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(gaza_border_control_rafah). % Mandatory for Tangled Rope. The border is physically guarded.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(gaza_border_control_rafah, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(gaza_border_control_rafah, egyptian_state_security).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(gaza_border_control_rafah, gazan_civilians).
narrative_ontology:constraint_victim(gaza_border_control_rafah, gazan_governing_authority).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)

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

% PERSPECTIVE 1: THE GAZAN CIVILIAN (SNARE)
% As a victim with trapped exit, the engine derives d ≈ 0.95, giving f(d) ≈ 1.42.
% χ = 0.55 * 1.42 * 1.0 (national scope) = 0.781. This high effective extraction
% (χ ≥ 0.66) combined with high suppression (0.90) classifies the constraint as a Snare.
constraint_indexing:constraint_classification(gaza_border_control_rafah, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ISRAELI SECURITY ESTABLISHMENT (ROPE)
% As a beneficiary with arbitrage exit, the engine derives d ≈ 0.05, giving f(d) ≈ -0.12.
% χ = 0.55 * -0.12 * 1.0 = -0.066. The negative effective extraction indicates
% the constraint is a subsidy, a pure coordination tool for security management.
constraint_indexing:constraint_classification(gaza_border_control_rafah, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the coordination function and the asymmetric extraction.
% Engine derives analytical d ≈ 0.72, giving f(d) ≈ 1.15.
% χ = 0.55 * 1.15 * 1.2 (global scope) = 0.759. This value of χ (0.40 ≤ χ ≤ 0.90)
% combined with high base extraction (ε > 0.30) and high suppression (S > 0.40)
% and the presence of both coordination and extraction functions meets the Tangled Rope criteria.
constraint_indexing:constraint_classification(gaza_border_control_rafah, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4A: EGYPTIAN STATE SECURITY (ROPE)
% As a beneficiary with constrained exit, Egypt experiences the policy as coordination,
% but with less freedom of action than Israel. The engine derives a d value higher than
% arbitrage but still low (e.g., d ≈ 0.30), resulting in low positive χ, classifying as Rope.
constraint_indexing:constraint_classification(gaza_border_control_rafah, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4B: GAZAN GOVERNING AUTHORITY (TANGLED ROPE)
% As a victim with organized power and constrained exit, this agent experiences
% high extraction but has more agency than a powerless civilian. The derived d is high
% but less than 'trapped' (e.g., d ≈ 0.75), yielding a χ ≈ 0.66. This borderline value
% reflects a coercive system they must navigate, a Tangled Rope.
constraint_indexing:constraint_classification(gaza_border_control_rafah, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gaza_border_control_rafah_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(gaza_border_control_rafah, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(gaza_border_control_rafah, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    writeln('Verified: Snare for powerless/trapped, Rope for institutional/arbitrage.').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(gaza_border_control_rafah, tangled_rope, context(agent_power(analytical), _, _, _)),
    writeln('Verified: Analytical perspective correctly identifies Tangled Rope.').

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(gaza_border_control_rafah, _),
    narrative_ontology:constraint_victim(gaza_border_control_rafah, _),
    domain_priors:requires_active_enforcement(gaza_border_control_rafah),
    writeln('Verified: All three structural requirements for Tangled Rope are declared.').

:- end_tests(gaza_border_control_rafah_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Set high to reflect the immense loss of economic
 *     opportunity, freedom, and humanitarian access imposed on the Gazan population.
 *     It is not higher because the enforcing states derive a genuine security
 *     benefit, not just pure rent.
 *   - Suppression (S=0.90): Extremely high, as alternative exit routes are nonexistent
 *     and actively blockaded by air, land, and sea. This lack of alternatives is
 *     central to the constraint's power.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a Gazan civilian (powerless, trapped), the border
 *   closure is a pure Snare — a coercive trap with no upside. For the Israeli
 *   security establishment (institutional, arbitrage), it's a Rope — a flexible
 *   tool for security coordination that subsidizes their strategic goals. The
 *   analytical observer sees both sides, classifying it as a Tangled Rope: a
 *   system with a real coordination function (for the enforcers) built upon
 *   severe, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The Israeli and Egyptian states benefit from enhanced security
 *     control and political leverage over Gaza. Their membership in the beneficiary
 *     group, combined with their powerful exit options, drives their directionality
 *     (d) low, resulting in a low or negative effective extraction (χ).
 *   - Victims: The Gazan population and its governing authority bear the costs.
 *     Their victim status and trapped/constrained exit options drive their
 *     directionality (d) high, resulting in a very high χ. This correctly models
 *     the constraint's asymmetric impact.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   Israel and Egypt are both institutional beneficiaries, but their experiences
 *   differ. Israel has 'arbitrage' exit, able to modify the policy unilaterally or
 *   use other leverage points. Egypt has 'constrained' exit, as its policy is
 *   bound by its treaty with Israel, regional pressures, and internal stability
 *   concerns. The framework captures this nuance, classifying the constraint as a
 *   Rope for both, but the underlying derived directionality (and thus χ) would be
 *   different, making the gap between them measurable.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It avoids labeling the
 *   blockade as pure coordination (a Rope), which would ignore the massive extraction
 *   from Gazans. It also avoids labeling it as a pure Snare from an analytical view,
 *   which would ignore the genuine (from the enforcers' perspective) security
 *   coordination function it serves for Israel and Egypt. The Tangled Rope
 *   classification acknowledges that a constraint can simultaneously be a tool of
 *   coordination for one group and a tool of extraction for another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_gaza_border_control_rafah,
    'Is the primary driver of the closure a proportionate security concern or a disproportionate punitive measure for political leverage?',
    'Declassified security assessments vs. economic impact studies over the closure period.',
    'If primarily security-driven, ε might be lower (~0.40). If primarily punitive, ε could be higher (~0.70), potentially making it a Snare even from the analytical view.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gaza_border_control_rafah, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The blockade has intensified since its imposition around 2007. This data
% models the accumulation of extractiveness over the period.
% (T=0 represents ~2007, T=10 represents ~2026).

% Theater ratio over time (stable and low):
narrative_ontology:measurement(gbcr_tr_t0, gaza_border_control_rafah, theater_ratio, 0, 0.20).
narrative_ontology:measurement(gbcr_tr_t5, gaza_border_control_rafah, theater_ratio, 5, 0.20).
narrative_ontology:measurement(gbcr_tr_t10, gaza_border_control_rafah, theater_ratio, 10, 0.20).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(gbcr_ex_t0, gaza_border_control_rafah, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gbcr_ex_t5, gaza_border_control_rafah, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(gbcr_ex_t10, gaza_border_control_rafah, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint coordinates the actions of two states
% to achieve a shared security/political goal.
narrative_ontology:coordination_type(gaza_border_control_rafah, enforcement_mechanism).

% Network relationships: The Rafah crossing policy is inextricably linked
% to the broader Gaza blockade.
narrative_ontology:affects_constraint(gaza_border_control_rafah, gaza_maritime_blockade).
narrative_ontology:affects_constraint(gaza_border_control_rafah, israeli_gaza_perimeter_control).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation based
% on beneficiary/victim status and the nuanced exit_options (arbitrage vs.
% constrained) accurately models the power dynamics and directionality for
% the key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */