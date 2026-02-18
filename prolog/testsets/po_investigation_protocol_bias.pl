% ============================================================================
% CONSTRAINT STORY: po_investigation_protocol_bias
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Source: https://www.bbc.com/news/articles/cvgpd1x00exo
% ============================================================================

:- module(constraint_po_investigation_protocol_bias, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: po_investigation_protocol_bias
 *   human_readable: Post Office Investigation Protocol Bias (Presumption of Guilt)
 *   domain: legal / institutional
 *
 * SUMMARY:
 *   A procedural constraint within the UK Post Office that directed internal
 *   investigators to presume guilt and find evidence of a crime in cases of
 *   financial shortfalls, rather than conducting an impartial inquiry. This
 *   protocol served to protect the faulty Horizon IT system by systematically
 *   blaming sub-postmasters, suppressing evidence of system flaws, and
 *   coercively recovering phantom debts.
 *
 * KEY AGENTS (by structural relationship):
 *   - sub_postmasters: Primary target (powerless/trapped) — bore the full financial and legal extraction.
 *   - post_office_legal_and_management: Primary beneficiary (institutional/arbitrage) — benefited by externalizing IT system liability and maintaining institutional authority.
 *   - internal_investigators: Secondary victim/enforcer (organized/constrained) — were subject to "improper pressure" to build criminal cases rather than find facts.
 *   - public_inquiry: Analytical observer — sees the full structure of coercion and coordination failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(po_investigation_protocol_bias, 0.55).
domain_priors:suppression_score(po_investigation_protocol_bias, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(po_investigation_protocol_bias, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(po_investigation_protocol_bias, extractiveness, 0.55).
narrative_ontology:constraint_metric(po_investigation_protocol_bias, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(po_investigation_protocol_bias, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(po_investigation_protocol_bias, tangled_rope).
narrative_ontology:human_readable(po_investigation_protocol_bias, "Post Office Investigation Protocol Bias (Presumption of Guilt)").
narrative_ontology:topic_domain(po_investigation_protocol_bias, "legal / institutional").

% --- Binary flags ---
domain_priors:requires_active_enforcement(po_investigation_protocol_bias). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(po_investigation_protocol_bias, post_office_legal_and_management).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(po_investigation_protocol_bias, sub_postmasters).
narrative_ontology:constraint_victim(po_investigation_protocol_bias, internal_investigators).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
%   Snare:        victim required -> MET

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

% PERSPECTIVE 1: THE SUB-POSTMASTERS (PRIMARY TARGET)
% They experience overwhelming coercive force with no recourse or alternatives,
% making the protocol a pure Snare.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% Calculated χ = 0.55 * 1.42 * 1.0 = 0.781 (Snare, as χ >= 0.66)
constraint_indexing:constraint_classification(po_investigation_protocol_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POST OFFICE LEGAL & MANAGEMENT (PRIMARY BENEFICIARY)
% From their perspective, the protocol is a tool for coordination and financial
% control, shielding them from liability. The extraction is externalized.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% Calculated χ = 0.55 * -0.12 * 1.0 = -0.066 (Rope, as χ <= 0.35)
constraint_indexing:constraint_classification(po_investigation_protocol_bias, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PUBLIC INQUIRY)
% The observer sees both the stated coordination function (financial integrity)
% and the actual extractive function, identifying a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% Calculated χ = 0.55 * 1.15 * 1.2 = 0.759 (Tangled Rope, as 0.40 <= χ <= 0.90)
constraint_indexing:constraint_classification(po_investigation_protocol_bias, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNAL INVESTIGATORS (INTER-INSTITUTIONAL)
% As both enforcers and victims of "improper pressure," they are trapped in
% the middle. Their exit is constrained. They see the stated goal but also the
% coercive extraction they are forced to implement.
% Engine derives d from: victim + constrained exit + organized power -> d ≈ 0.75 -> f(d) ≈ 1.18
% Calculated χ = 0.55 * 1.18 * 1.0 = 0.649 (Tangled Rope)
constraint_indexing:constraint_classification(po_investigation_protocol_bias, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(po_investigation_protocol_bias_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(po_investigation_protocol_bias, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(po_investigation_protocol_bias, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_claim_is_tangled_rope) :-
    constraint_indexing:constraint_classification(po_investigation_protocol_bias, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_met) :-
    domain_priors:base_extractiveness(po_investigation_protocol_bias, E), E >= 0.30,
    domain_priors:suppression_score(po_investigation_protocol_bias, S), S >= 0.40,
    domain_priors:requires_active_enforcement(po_investigation_protocol_bias),
    narrative_ontology:constraint_beneficiary(po_investigation_protocol_bias, _),
    narrative_ontology:constraint_victim(po_investigation_protocol_bias, _).

:- end_tests(po_investigation_protocol_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): The protocol was highly effective at extracting funds and false confessions from sub-postmasters to cover Horizon's shortfalls. It's not higher because it wasn't perfectly efficient; some sub-postmasters resisted, and eventually the system broke.
 *   - Suppression Score (0.85): Extremely high. Sub-postmasters were contractually bound, faced a monolithic institution that refused to acknowledge IT flaws, and were told they were "the only one." Legal and procedural alternatives were effectively non-existent for individuals.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Snare and Rope is profound. For sub-postmasters (powerless, trapped), the protocol was an inescapable trap designed for extraction. For Post Office management (institutional, arbitrage), it was a business tool for enforcing accountability and protecting the institution's primary assets and reputation, with all costs externalized onto the targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the clear structural relationships. `post_office_legal_and_management` are beneficiaries as they designed and deployed the constraint to shield the institution from liability. `sub_postmasters` are the primary victims, bearing all costs. Critically, `internal_investigators` are also modeled as victims due to the "improper pressure" that coerced them into an unethical enforcement role, corrupting their professional function. This is reflected in their `constrained` exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical case of mandatrophy. A legitimate mandate (ensuring financial integrity) was corrupted into a purely extractive function to protect a failed IT system (Horizon). The 'Tangled Rope' classification is crucial because it correctly captures both the claimed coordination function and the overwhelmingly dominant asymmetric extraction. It avoids mislabeling the system as a pure Snare (which would ignore the institutional pretext) or a Rope (which would ignore the devastating, coercive harm). The "Dynamic Coalition" effect is also evident historically: individual sub-postmasters were powerless, but once they organized (e.g., via Alan Bates), their effective power shifted to `organized`, enabling them to challenge and ultimately break the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_po_bias_intent,
    'Was the investigative protocol designed with malicious extractive intent from inception, or did it degrade from a flawed coordination mechanism into an extractive one under institutional pressure to conceal Horizon''s failings?',
    'Unredacted minutes and internal communications from the protocol''s design phase (c. 1999-2001).',
    'If malicious from inception, the initial ε was high (Snare-like). If it degraded, the initial ε was lower (Rope-like), indicating a process of institutional decay (mandatrophy).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% The main period of prosecutions ran for roughly 15 years.
narrative_ontology:interval(po_investigation_protocol_bias, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the constraint intensifying over time as the Post
% Office doubled down on its strategy of prosecution to conceal IT flaws.
% Base_extractiveness is > 0.46, so this section is required.

% Theater ratio over time: Investigations became less about fact-finding and more
% about performative enforcement to maintain the institutional narrative.
narrative_ontology:measurement(po_bias_tr_t0, po_investigation_protocol_bias, theater_ratio, 0, 0.10).
narrative_ontology:measurement(po_bias_tr_t5, po_investigation_protocol_bias, theater_ratio, 5, 0.30).
narrative_ontology:measurement(po_bias_tr_t10, po_investigation_protocol_bias, theater_ratio, 10, 0.40).

% Extraction over time: The process became more ruthlessly extractive as the
% scale of the underlying problem grew.
narrative_ontology:measurement(po_bias_ex_t0, po_investigation_protocol_bias, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(po_bias_ex_t5, po_investigation_protocol_bias, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(po_bias_ex_t10, po_investigation_protocol_bias, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: Its claimed function was as an enforcement mechanism for
% financial accountability.
narrative_ontology:coordination_type(po_investigation_protocol_bias, enforcement_mechanism).

% Network relationships: This biased protocol was a direct consequence of the
% upstream failure of the Horizon IT system. The institution created this
% constraint to manage the fallout from the other one.
narrative_ontology:affects_constraint(horizon_it_system_flaws, po_investigation_protocol_bias).

% --- Network Decomposition (Constraint Families) ---
% DUAL FORMULATION NOTE:
% This constraint (the biased protocol) is a downstream effect of the
% technological constraint posed by the faulty Horizon IT system.
% Decomposed because they have different structures and ε values.
% Related stories:
%   - horizon_it_system_flaws (ε=~0.80, Snare from user perspective)
%
% This decomposition follows the ε-invariance principle: the IT system's flaws
% and the human protocol designed to conceal them are distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations combined with exit options (trapped,
% arbitrage, constrained) accurately models the power dynamics and produces
% the correct perspectival gaps.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */