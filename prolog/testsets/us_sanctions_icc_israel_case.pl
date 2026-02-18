% ============================================================================
% CONSTRAINT STORY: us_sanctions_icc_israel_case
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-25
% ============================================================================

:- module(constraint_us_sanctions_icc_israel_case, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_sanctions_icc_israel_case
 *   human_readable: US Sanctions Threat Against ICC Officials Investigating Israel
 *   domain: political
 *
 * SUMMARY:
 *   A bipartisan bill in the US Senate proposes sanctions (visa bans, asset
 *   freezes) against officials of the International Criminal Court (ICC)
 *   involved in prosecuting Israeli nationals for alleged war crimes. This
 *   constraint represents the use of national sovereign power to suppress
 *   the actions of an international legal body, creating a conflict between
 *   geopolitical interests and transnational justice.
 *
 * KEY AGENTS (by structural relationship):
 *   - ICC Officials: Primary target (institutional/constrained) — bear the coercive extraction.
 *   - US Government: Primary architect & beneficiary (institutional/arbitrage) — benefits from asserting power and protecting an ally.
 *   - Israeli Government: Primary beneficiary (institutional/arbitrage) — benefits from immunity from prosecution.
 *   - International Law Scholars: Analytical observer — sees the full structure of coercive coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_icc_israel_case, 0.60).
domain_priors:suppression_score(us_sanctions_icc_israel_case, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_sanctions_icc_israel_case, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, extractiveness, 0.60).
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, theater_ratio, 0.15).

% --- NL Profile Metrics are not applicable for this constraint type ---

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_sanctions_icc_israel_case, tangled_rope).
narrative_ontology:human_readable(us_sanctions_icc_israel_case, "US Sanctions Threat Against ICC Officials Investigating Israel").
narrative_ontology:topic_domain(us_sanctions_icc_israel_case, "political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_sanctions_icc_israel_case). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_sanctions_icc_israel_case, us_government).
narrative_ontology:constraint_beneficiary(us_sanctions_icc_israel_case, israeli_government).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_sanctions_icc_israel_case, icc_officials).

% Gate requirements check:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement -> MET.

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

% PERSPECTIVE 1: THE PRIMARY TARGET (ICC OFFICIALS)
% As individuals, they are powerless against a state actor and trapped by their professional mandate.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.60 * 1.42 * 1.2 (global scope) ≈ 1.02. This classifies as a Snare.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (US GOVERNMENT)
% The architect and beneficiary, using its power to create an exception to international norms.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.60 * -0.12 * 1.2 ≈ -0.086. Negative extraction -> classifies as a Rope.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the coordination function for the beneficiaries and the asymmetric extraction from the target.
% Engine derives canonical d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.60 * 1.15 * 1.2 ≈ 0.828. This value falls within the Tangled Rope range [0.40, 0.90].
% Given that the structural requirements (beneficiary, victim, enforcement) are met, it's a Tangled Rope.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% This constraint operates between two institutional actors with vastly different power and exit options.

% Perspective 4A: The ICC as an Institution
% The ICC is an institution but is the target of coercion and has limited ability to evade it.
% Engine derives d from: victim + institutional(constrained) -> high d ≈ 0.90 -> f(d) ≈ 1.36.
% χ = 0.60 * 1.36 * 1.2 ≈ 0.979. This is a Snare.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_icc_israel_case_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify perspectival gap between an individual target and the beneficiary.
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(perspectival_gap_institutional) :-
    % Verify the gap between the two institutional actors.
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_met) :-
    % Verifies that all three required flags for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(us_sanctions_icc_israel_case, _),
    narrative_ontology:constraint_victim(us_sanctions_icc_israel_case, _),
    domain_priors:requires_active_enforcement(us_sanctions_icc_israel_case).

:- end_tests(us_sanctions_icc_israel_case_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.60): The threat is not absolute but is designed to impose a severe cost on the ICC's core function, effectively extracting its agency in this specific domain.
 *   - Suppression Score (s=0.80): The sanctions aim to make the legally mandated path (prosecution) prohibitively costly, strongly suppressing this alternative in favor of inaction.
 *   - Theater Ratio (t=0.15): This is a credible threat, backed by past actions and bipartisan political will. It is not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the beneficiaries (US/Israel), the sanction threat is a 'Rope'—an effective coordination tool to protect strategic interests with low internal cost. For the target (ICC), it is a 'Snare'—a purely coercive, high-extraction constraint that paralyzes their function and for which they have no meaningful escape.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (us_government, israeli_government) gain strategic immunity and assert sovereign power. The victims (icc_officials) bear the direct personal and institutional costs of the coercion. The `exit_options` are key: the beneficiaries have `arbitrage` (they operate outside the rules they enforce on others), while the ICC has `constrained` exit (it cannot simply abandon its mandate). This asymmetry in exit options drives the stark difference in derived directionality (d) and classification, even between two institutional actors.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This is a classic example of inter-institutional power dynamics. Both the US and the ICC are 'institutional' actors, but they are not peers in this interaction. The US government's `arbitrage` exit option and beneficiary status gives it a low `d` value, classifying the constraint as a Rope. The ICC's `constrained` exit and victim status gives it a high `d` value, classifying the constraint as a Snare. This correctly models the asymmetric power relationship where one institution imposes its will upon another.
 *
 * MANDATROPHY ANALYSIS:
 *   This case could be mislabeled a pure Snare if one ignores its function for the beneficiaries. The Tangled Rope classification from the analytical view correctly identifies that the constraint possesses both a genuine (if perverse) coordination function (aligning US/Israeli policy against the ICC) and severe, asymmetric extraction. This prevents oversimplifying the dynamic as mere destruction, recognizing it as a tool with a clear strategic purpose for its architects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_us_sanctions_icc_israel_case,
    'Is the bipartisan legislative threat a fully credible deterrent intended for enforcement, or is it primarily a political signal that would not survive a presidential veto or executive inaction?',
    'Observation of US executive branch action (or inaction) if the ICC proceeds with issuing arrest warrants.',
    'If credible, the ε=0.60 and Snare classification for the target holds. If primarily a signal (higher theater), ε would be lower and the constraint might degrade towards a Piton from some perspectives.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_sanctions_icc_israel_case, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over time, from diplomatic pressure to active sanctions.
% T=0: Post-9/11 era, initial US hostility to ICC.
% T=5: Trump administration sanctions over Afghanistan investigation (c. 2020).
% T=10: Current bipartisan legislative threat (c. 2024).

% Theater ratio over time (remains low as threats are credible):
narrative_ontology:measurement(us_sanctions_icc_israel_case_tr_t0, us_sanctions_icc_israel_case, theater_ratio, 0, 0.10).
narrative_ontology:measurement(us_sanctions_icc_israel_case_tr_t5, us_sanctions_icc_israel_case, theater_ratio, 5, 0.12).
narrative_ontology:measurement(us_sanctions_icc_israel_case_tr_t10, us_sanctions_icc_israel_case, theater_ratio, 10, 0.15).

% Extraction over time (escalation from pressure to direct sanctions):
narrative_ontology:measurement(us_sanctions_icc_israel_case_ex_t0, us_sanctions_icc_israel_case, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(us_sanctions_icc_israel_case_ex_t5, us_sanctions_icc_israel_case, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(us_sanctions_icc_israel_case_ex_t10, us_sanctions_icc_israel_case, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: This is a coercive enforcement mechanism designed to
% impose a specific behavioral norm (inaction) on a target.
narrative_ontology:coordination_type(us_sanctions_icc_israel_case, enforcement_mechanism).

% Network relationships: This constraint directly impacts the perceived
% legitimacy and effectiveness of international law as a whole.
narrative_ontology:affects_constraint(us_sanctions_icc_israel_case, international_law_legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural derivation chain,
% using beneficiary/victim declarations combined with the distinct exit_options
% for the institutional actors (arbitrage vs. constrained), accurately
% computes the asymmetric directionality values (d) that reflect the
% underlying power dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */