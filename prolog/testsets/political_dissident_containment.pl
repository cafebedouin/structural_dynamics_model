% ============================================================================
% CONSTRAINT STORY: political_dissident_containment
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_political_dissident_containment, []).

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
 *   constraint_id: political_dissident_containment
 *   human_readable: The state carceral system for neutralizing political opposition
 *   domain: political
 *
 * SUMMARY:
 *   Based on the death of Russian opposition leader Alexei Navalny in an Arctic
 *   penal colony, this constraint models the legal and carceral framework used by
 *   an authoritarian state to neutralize political dissidents. This framework
 *   encompasses selective prosecution, harsh sentencing for politically-motivated
 *   charges (e.g., "extremism"), and punitive prison conditions designed to
 *   degrade health and eliminate political agency, culminating in the target's
 *   death. The system functions as a tool of power consolidation for the ruling
 *   regime.
 *
 * KEY AGENTS (by structural relationship):
 *   - Political Dissidents (e.g., Navalny): Primary target (powerless/trapped) — bears total extraction of freedom, health, and life.
 *   - The Authoritarian State Apparatus (e.g., The Kremlin, FSB, Prison Service): Primary beneficiary (institutional/arbitrage) — benefits from the elimination of political threats and the consolidation of power.
 *   - Western Governments / International Bodies: Secondary institutional actor (institutional/constrained) — experiences the constraint as a violation of international norms but has limited tools to intervene.
 *   - Analytical Observer: Sees the full structure of coercion and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_dissident_containment, 0.85).
domain_priors:suppression_score(political_dissident_containment, 0.95).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(political_dissident_containment, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_dissident_containment, extractiveness, 0.85).
narrative_ontology:constraint_metric(political_dissident_containment, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(political_dissident_containment, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint type.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(political_dissident_containment, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(political_dissident_containment).

% --- Emergence flag (required for mountain constraints) ---
% This constraint is human-designed and enforced; not natural.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(political_dissident_containment, the_russian_state_apparatus).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(political_dissident_containment, political_dissidents).
narrative_ontology:constraint_victim(political_dissident_containment, international_normative_order).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The political dissident, who experiences total extraction with no escape.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(political_dissident_containment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The state apparatus, which sees this as a tool for maintaining order and power.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(political_dissident_containment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The analyst sees the system for its primary function: pure, coercive extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(political_dissident_containment, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% Western governments and international bodies see the system as a violation
% of norms they uphold, but their ability to act is limited. They are a victim
% of the norm-breaking with constrained exit options.
constraint_indexing:constraint_classification(political_dissident_containment, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_dissident_containment_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap between the dissident and the state.
    constraint_indexing:constraint_classification(political_dissident_containment, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(political_dissident_containment, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary.

test(analytical_view_matches_target) :-
    % Verify the analytical view aligns with the victim's experience of a Snare.
    constraint_indexing:constraint_classification(political_dissident_containment, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_dissident_containment, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeTarget == TypeAnalytical.

test(high_extraction_and_suppression_thresholds, [nondet]) :-
    % Verify the constraint meets the numerical criteria for a Snare.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SupMetricName),
    narrative_ontology:constraint_metric(political_dissident_containment, ExtMetricName, E),
    narrative_ontology:constraint_metric(political_dissident_containment, SupMetricName, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(political_dissident_containment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): The system extracts the most valuable assets
 *     an individual possesses: political agency, freedom, health, and ultimately,
 *     life. The extraction is near-total.
 *   - Suppression Score (0.95): For an individual caught in this system (e.g.,
 *     imprisoned in a penal colony), there are no viable alternatives. Legal
 *     appeals are performative, and escape is impossible. The constraint is
 *     designed to foreclose all other paths.
 *   - Theater Ratio (0.30): The system maintains a veneer of legality (trials,
 *     charges, official statements), but this is secondary to its primary
 *     function of physical and political elimination. The theater is present but
 *     not the dominant feature.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the 'political_dissidents' (powerless/trapped), the
 *   system is a Snare—a coercive mechanism of pure extraction from which there
 *   is no escape. For 'the_russian_state_apparatus' (institutional/arbitrage),
 *   it is a Rope—an effective coordination mechanism to enforce state policy,
 *   maintain stability, and eliminate perceived threats, with benefits far
 *   outweighing any internal costs.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. 'the_russian_state_apparatus' is the
 *   sole beneficiary, using the constraint to consolidate power. 'political_dissidents'
 *   are the explicit victims, bearing the full cost. The engine's derivation
 *   of a low `d` for the beneficiary and a high `d` for the victim accurately
 *   reflects this severe power asymmetry.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The perspective of Western governments (institutional/constrained) highlights
 *   a key dynamic. Though they are powerful institutions, their ability to
 *   alter this specific constraint is limited, placing them in a `constrained`
 *   exit position. They are victims of the violation of international norms.
 *   Their classification as Snare, like the primary target's, shows that from
 *   the outside, the system is still seen as one of pure, illegitimate coercion,
 *   even if they don't bear the physical costs.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This classification correctly identifies the system as a Snare from the
 *   analytical and victim perspectives, preventing the state's "Rope" narrative
 *   (i.e., that this is merely "law and order") from obscuring the underlying
 *   extractive reality. The high ε (0.85) and suppression (0.95) scores ensure
 *   it cannot be misclassified as a flawed coordination tool (Tangled Rope).
 *   The high extraction is justified by the total loss of freedom, health, and
 *   life for the target.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_political_dissident_containment,
    'Was Navalny''s death the result of a direct order (targeted assassination) or the inevitable systemic outcome of the prison conditions (systemic negligence)?',
    'Access to internal Kremlin/FSIN communications or a credible whistleblower account.',
    'If direct order -> a maximally sharpened Snare, applied with specific intent. If systemic -> a Snare whose standard operating procedure is lethal over time. The classification remains Snare, but the intentionality changes.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_political_dissident_containment, empirical, 'Distinguishing between a direct assassination order and lethal systemic negligence requires empirical evidence like internal communications.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(political_dissident_containment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over the last decade. The data models this
% drift from a harsh but not necessarily lethal system to one where death
% is a predictable outcome. This reflects 'extraction_accumulation'.

% Theater ratio over time (legal pretenses eroding):
narrative_ontology:measurement(pdc_tr_t0, political_dissident_containment, theater_ratio, 0, 0.50).
narrative_ontology:measurement(pdc_tr_t5, political_dissident_containment, theater_ratio, 5, 0.40).
narrative_ontology:measurement(pdc_tr_t10, political_dissident_containment, theater_ratio, 10, 0.30).

% Extraction over time (conditions becoming more lethal):
narrative_ontology:measurement(pdc_ex_t0, political_dissident_containment, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(pdc_ex_t5, political_dissident_containment, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(pdc_ex_t10, political_dissident_containment, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This system coordinates the actions of the judiciary, security services,
% and penal system to achieve a political goal.
narrative_ontology:coordination_type(political_dissident_containment, enforcement_mechanism).

% Network relationship: Media censorship is a prerequisite for maintaining
% the public narrative that allows this containment system to operate with
% minimal domestic backlash.
narrative_ontology:affects_constraint(media_censorship_laws, political_dissident_containment).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural relationships
% are stark, and the default derivation from beneficiary/victim status and
% exit options accurately models the extreme power imbalance.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */