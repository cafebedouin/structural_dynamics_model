% ============================================================================
% CONSTRAINT STORY: frontex_pushback_coordination
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_frontex_pushback_coordination, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: frontex_pushback_coordination
 *   human_readable: Frontex-Coordinated Asylum Suppression at EU Maritime Borders
 *   domain: political
 *
 * SUMMARY:
 *   This constraint models the operational reality of the EU's external border
 *   management, executed by Frontex in coordination with national authorities
 *   (e.g., Greece, Spain). Officially a system for border security and rescue,
 *   it functions as a mechanism for "pushbacks"—intercepting migrant vessels and
 *   returning them to non-EU waters, thereby preventing them from exercising
 *   their legal right to claim asylum. The constraint combines a genuine
 *   coordination function among member states with severe, asymmetric extraction
 *   targeted at asylum seekers.
 *
 * KEY AGENTS (by structural relationship):
 *   - Maritime Asylum Seekers: Primary target (powerless/trapped) — bear the full cost of extraction, including physical risk and the loss of legal rights.
 *   - EU Member States: Primary beneficiary (institutional/arbitrage) — benefit from reduced asylum applications and perceived border control, outsourcing political and operational costs to a coordinating body.
 *   - Frontex Agency: Institutional enabler (institutional/constrained) — benefits from an expanded mandate and budget, but is constrained by the political directives of member states.
 *   - Human Rights Monitors: Analytical observer — sees the dual function of coordination and illegal extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(frontex_pushback_coordination, 0.68).
domain_priors:suppression_score(frontex_pushback_coordination, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(frontex_pushback_coordination, 0.60).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(frontex_pushback_coordination, extractiveness, 0.68).
narrative_ontology:constraint_metric(frontex_pushback_coordination, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(frontex_pushback_coordination, theater_ratio, 0.60).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(frontex_pushback_coordination, tangled_rope).
narrative_ontology:human_readable(frontex_pushback_coordination, "Frontex-Coordinated Asylum Suppression at EU Maritime Borders").

% --- Binary flags ---
domain_priors:requires_active_enforcement(frontex_pushback_coordination). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(frontex_pushback_coordination, eu_member_states).
narrative_ontology:constraint_beneficiary(frontex_pushback_coordination, frontex_agency).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(frontex_pushback_coordination, maritime_asylum_seekers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)

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
% For asylum seekers, the system is a trap. It uses the promise of safety
% to intercept them and then extracts their fundamental rights.
% Engine derives d from: victim + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ = 0.68 * 1.42 * 0.9 (regional) ≈ 0.87. This is a clear Snare.
constraint_indexing:constraint_classification(frontex_pushback_coordination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For EU member states, the system is a pure coordination mechanism to manage a
% shared political problem with high efficiency and plausible deniability.
% Engine derives d from: beneficiary + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% χ = 0.68 * -0.12 * 1.1 (continental) ≈ -0.09. A subsidized Rope.
constraint_indexing:constraint_classification(frontex_pushback_coordination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view recognizes both the coordination function and the
% asymmetric extraction. It sees the system for what it is: a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.68 * 1.15 * 1.2 (global) ≈ 0.94.
constraint_indexing:constraint_classification(frontex_pushback_coordination, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The Frontex agency itself is an institutional beneficiary but operates with
% fewer degrees of freedom than member states. It is an instrument of policy.
% Engine derives d from: beneficiary + constrained → d ≈ 0.25 → f(d) ≈ 0.14.
% χ = 0.68 * 0.14 * 1.1 (continental) ≈ 0.10.
% This correctly classifies as a Rope, but the positive χ reflects the operational
% costs and political constraints it faces, unlike the net-negative χ for member states.
constraint_indexing:constraint_classification(frontex_pushback_coordination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(frontex_pushback_coordination_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(frontex_pushback_coordination, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(frontex_pushback_coordination, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Passed: Target sees Snare, Beneficiary sees Rope.').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(frontex_pushback_coordination, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    narrative_ontology:constraint_beneficiary(frontex_pushback_coordination, _), % Implies has_coordination_function
    narrative_ontology:constraint_victim(frontex_pushback_coordination, _),     % Implies has_asymmetric_extraction
    domain_priors:requires_active_enforcement(frontex_pushback_coordination).

test(inter_institutional_distinction) :-
    get_classification(frontex_pushback_coordination, context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(continental)), _, ChiState),
    get_classification(frontex_pushback_coordination, context(agent_power(institutional), time_horizon(generational), exit_options(constrained), spatial_scope(continental)), _, ChiAgency),
    ChiState < 0,
    ChiAgency > 0,
    format('Passed: Member state chi (~w) is negative, Frontex chi (~w) is positive.~n', [ChiState, ChiAgency]).


:- end_tests(frontex_pushback_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.68): This value is high because the constraint's
 *     primary effect is the nullification of a fundamental legal right—the
 *     right to seek asylum. It's not total (some people get through), but it's
 *     highly effective at extraction.
 *   - Suppression Score (0.85): Extremely high. For migrants at sea, there are
 *     no viable, safe alternatives once intercepted. The system actively
 *     suppresses the legal alternative (being taken to an EU port).
 *   - Theater Ratio (0.60): Significant. The entire operation is framed in the
 *     language of "border management," "security," and "fighting smugglers,"
 *     which serves as a theatrical layer obscuring the core function of
 *     denying asylum claims.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For EU member states (beneficiaries), Frontex is an
 *   efficient Rope, a coordination tool that externalizes a politically
 *   difficult task at low cost. For an asylum seeker (target), it is a Snare,
 *   a coercive system that traps them and strips them of their rights. The
 *   analytical perspective correctly identifies it as a Tangled Rope, a system
 *   that uses a genuine coordination function to enable and conceal a brutal
 *   extractive process.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `eu_member_states`, `frontex_agency`. Benefits flow to
 *     states as reduced processing burdens and political cover.
 *   - Victims: `maritime_asylum_seekers`. Costs are borne by this group in the
 *     form of physical danger, financial loss, and the denial of legal rights.
 *   This clear structural asymmetry is why the directionality `d` is so
 *   different between perspectives, driving the gap in classification.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between member states and the Frontex agency itself.
 *   Both are `institutional` beneficiaries. However, member states have
 *   `arbitrage` exit; they can choose to use Frontex, act unilaterally, or
 *   change policy. Frontex has `constrained` exit; it is an instrument of the
 *   states' collective will. The derived directionality `d` is therefore lower for
 *   states (more purely beneficial) than for the agency (which bears costs and
 *   constraints), resulting in a negative χ for states and a positive χ for Frontex.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors. It does not mistake the system
 *   for a pure Snare, which would ignore the real coordination problem member
 *   states are solving. It also does not accept the theatrical narrative that it
 *   is a pure Rope, which would ignore the violent extraction at its core. By
 *   classifying it as a Tangled Rope, the model captures its dual nature and
 *   highlights the way coordination is weaponized for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_frontex_pushback,
    'Is the primary intent of the Frontex-coordinated system border security (with pushbacks as an emergent, illegal side-effect) or is the primary intent to prevent asylum applications (with "border security" as the official justification)?',
    'Access to unredacted internal Frontex operational directives and communications between the agency and member state governments.',
    'If intent is security, it is a Rope that has degraded into a Tangled Rope due to operational pressures. If intent is suppression, it was designed as a Tangled Rope from the outset.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(frontex_pushback_coordination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Frontex's mandate and operational
% posture have intensified since its expansion around 2015-2016.
% Extraction has increased as pushback tactics became more systematic.
% Theater has also increased as the agency professionalized its PR functions
% in response to growing public scrutiny.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(fpc_tr_t0, frontex_pushback_coordination, theater_ratio, 0, 0.40).
narrative_ontology:measurement(fpc_tr_t5, frontex_pushback_coordination, theater_ratio, 5, 0.55).
narrative_ontology:measurement(fpc_tr_t10, frontex_pushback_coordination, theater_ratio, 10, 0.60).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(fpc_ex_t0, frontex_pushback_coordination, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(fpc_ex_t5, frontex_pushback_coordination, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(fpc_ex_t10, frontex_pushback_coordination, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This system allocates security assets and enforces a de facto policy.
narrative_ontology:coordination_type(frontex_pushback_coordination, enforcement_mechanism).

% Network relationships (structural influence edges)
% This constraint directly impacts the viability and pressure on other EU
% migration and free movement policies.
narrative_ontology:affects_constraint(frontex_pushback_coordination, dublin_regulation_iii).
narrative_ontology:affects_constraint(frontex_pushback_coordination, schengen_area_free_movement).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations combined with the different exit_options for
% member states (`arbitrage`) vs. the Frontex agency (`constrained`)
% accurately captures the directionality dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */