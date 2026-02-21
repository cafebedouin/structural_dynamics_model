% ============================================================================
% CONSTRAINT STORY: hk_nsl_civic_party_disbandment
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_hk_nsl_civic_party_disbandment, []).

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
 *   constraint_id: hk_nsl_civic_party_disbandment
 *   human_readable: "The Hong Kong National Security Law (NSL) leading to the dissolution of the Civic Party"
 *   domain: political
 *
 * SUMMARY:
 *   This constraint models the political environment created by the Beijing-imposed
 *   National Security Law (NSL) in Hong Kong. The law criminalizes secession,
 *   subversion, terrorism, and collusion with foreign forces, effectively
 *   eliminating space for pro-democracy opposition. The Civic Party, once a
 *   prominent pro-democracy group, voted to disband as its members faced
 *   imprisonment and the party saw no viable future for political participation.
 *
 * KEY AGENTS (by structural relationship):
 *   - HK Pro-Democracy Movements: Primary target (powerless/trapped) — bears the full suppressive and extractive force of the law, leading to their dissolution.
 *   - Beijing & HK Governments: Primary beneficiary (institutional/arbitrage) — benefits from the consolidation of power and elimination of political opposition.
 *   - Analytical Observer: External analyst — sees the full structure of coercion and power transfer.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hk_nsl_civic_party_disbandment, 0.85).
domain_priors:suppression_score(hk_nsl_civic_party_disbandment, 0.95).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(hk_nsl_civic_party_disbandment, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hk_nsl_civic_party_disbandment, extractiveness, 0.85).
narrative_ontology:constraint_metric(hk_nsl_civic_party_disbandment, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(hk_nsl_civic_party_disbandment, theater_ratio, 0.10).

% --- NL Profile Metrics are not applicable for this human-enforced constraint ---

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hk_nsl_civic_party_disbandment, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(hk_nsl_civic_party_disbandment).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hk_nsl_civic_party_disbandment, beijing_and_hk_governments).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hk_nsl_civic_party_disbandment, hk_pro_democracy_movements).

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
% Pro-democracy activists are victims with no exit. The engine derives a high d,
% resulting in extremely high effective extraction (χ), classifying this as a Snare.
% Their organized power has been neutralized, leaving them powerless.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The state government benefits from the constraint and has arbitrage exit (it sets
% the rules). The engine derives a low d, leading to negative effective extraction (χ).
% From this perspective, the law is a pure coordination tool for enforcing
% "stability" and "national unity."
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% An external analyst sees the high base extraction and suppression. Even with
% a neutral directionality, the metrics overwhelmingly point to a Snare.
% The global scope amplifies the perceived extraction due to geopolitical complexity.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hk_nsl_civic_party_disbandment_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    % Verify the core perspectival gap: the target sees a snare, beneficiary sees a rope.
    constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_confirms_snare, [nondet]) :-
    % Verify the analytical perspective converges on the target's classification.
    constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, snare, context(agent_power(analytical), _, _, _)).

test(thresholds_are_high_extraction_and_suppression) :-
    domain_priors:base_extractiveness(hk_nsl_civic_party_disbandment, E),
    domain_priors:suppression_score(hk_nsl_civic_party_disbandment, S),
    E >= 0.66, % Snare-level base extraction
    S >= 0.60. % Snare-level suppression

:- end_tests(hk_nsl_civic_party_disbandment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): Extremely high. The NSL extracts the fundamental
 *     political right to organize and oppose the government, transferring total
 *     political control to the state. This is not a partial tax but a near-complete
 *     confiscation of political agency.
 *   - Suppression Score (0.95): Extremely high. The law is explicitly designed to
 *     eliminate alternatives to compliance. With leaders jailed and the threat of
 *     life imprisonment, meaningful political opposition is rendered impossible.
 *     The Civic Party's dissolution is direct evidence of this near-total suppression.
 *   - Theater Ratio (0.10): Low. The law is not performative; it is actively and
 *     severely enforced, with tangible consequences for its targets.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal. For the `hk_pro_democracy_movements` (victims), the law is a
 *   Snare that has ended their political existence. For the `beijing_and_hk_governments`
 *   (beneficiaries), the law is a Rope, a tool that coordinates society towards their
 *   preferred outcome of "stability" by removing "disruptive" elements. This is a
 *   classic case where one group's Snare is another's coercive Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The costs (loss of freedom, political
 *   participation, risk of imprisonment) are borne entirely by the `hk_pro_democracy_movements`.
 *   The benefits (consolidated control, elimination of dissent) accrue entirely
 *   to the `beijing_and_hk_governments`. The beneficiary/victim declarations
 *   directly model this structural reality, allowing the engine to correctly
 *   calculate the extreme divergence in perceived extraction (χ).
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a primary example of how Deferential Realism prevents mandatrophy.
 *   A naive analysis might accept the state's framing of the NSL as a coordination
 *   mechanism for social order (a Rope). By indexing to the `powerless` and
 *   `trapped` victim, the framework correctly identifies the structure as a Snare.
 *   The high suppression and extraction scores, derived from the observed reality
 *   of party dissolution and arrests, make a Rope classification untenable from
 *   any perspective but the direct beneficiary's.
 *   [RESOLVED MANDATROPHY]: The extreme perspectival gap between a Snare (victim)
 *   and a Rope (beneficiary), combined with ε=0.85 and suppression=0.95, makes
 *   misclassification as a legitimate coordination mechanism impossible from any
 *   neutral or victim-centric index.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_hk_nsl_civic_party_disbandment,
    'Will the NSL lead to long-term stability (a coercive Rope) or will it drive resistance underground, creating future covert instability (a Snare whose failure mode is delayed)?',
    'Long-term (10-20 year) tracking of capital flight, professional emigration rates, and documented incidents of covert political/economic resistance.',
    'If it leads to stability, the beneficiary view of it as a harsh Rope gains credence over time. If it leads to covert instability, it confirms the Snare classification and its inherent fragility.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_hk_nsl_civic_party_disbandment, empirical, 'Whether the law creates long-term stability or drives covert resistance, determining its true nature as a stable coercive Rope or a fragile Snare.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hk_nsl_civic_party_disbandment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of the constraint since the 2019
% protests and the imposition of the NSL in 2020. This constraint did not
% appear fully formed but was the culmination of a political process.
% Base_extractiveness > 0.46 requires this data.

% Theater ratio over time (remains low as enforcement is real):
narrative_ontology:measurement(hk_nsl_tr_t0, hk_nsl_civic_party_disbandment, theater_ratio, 0, 0.20).
narrative_ontology:measurement(hk_nsl_tr_t5, hk_nsl_civic_party_disbandment, theater_ratio, 5, 0.15).
narrative_ontology:measurement(hk_nsl_tr_t10, hk_nsl_civic_party_disbandment, theater_ratio, 10, 0.10).

% Extraction over time (shows sharp increase with NSL implementation):
narrative_ontology:measurement(hk_nsl_ex_t0, hk_nsl_civic_party_disbandment, base_extractiveness, 0, 0.50). % Pre-NSL, but still a constrained environment
narrative_ontology:measurement(hk_nsl_ex_t5, hk_nsl_civic_party_disbandment, base_extractiveness, 5, 0.75).  % NSL implemented
narrative_ontology:measurement(hk_nsl_ex_t10, hk_nsl_civic_party_disbandment, base_extractiveness, 10, 0.85). % Full consolidation, opposition eliminated

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: From the beneficiary's perspective, this is a pure
% enforcement mechanism to create a desired political order.
narrative_ontology:coordination_type(hk_nsl_civic_party_disbandment, enforcement_mechanism).

% Network relationships: The political crackdown in Hong Kong is a key factor
% in the broader geopolitical competition and economic decoupling between the
% US and China.
narrative_ontology:affects_constraint(hk_nsl_civic_party_disbandment, us_china_tech_decoupling).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from the clear beneficiary/victim roles and their respective exit options
% (arbitrage vs. trapped) accurately models the power dynamics and produces
% the correct directionality (d) values for the perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */