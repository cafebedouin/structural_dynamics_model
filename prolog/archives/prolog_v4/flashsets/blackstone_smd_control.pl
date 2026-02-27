% ============================================================================
% CONSTRAINT STORY: blackstone_smd_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blackstone_smd_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: blackstone_smd_control
 *   human_readable: Blackstone Senior Managing Director Voting Control
 *   domain: economic
 *
 * SUMMARY:
 *   This corporate governance structure, established during Blackstone's 2007
 *   IPO, uses a multi-class unit structure to grant Senior Managing Directors
 *   (SMDs) absolute voting control over the firm's general partner. This
 *   arrangement ensures SMD control over strategic decisions but raises
 *   concerns about potential conflicts of interest with common shareholders
 *   and limited partners. While intended to align management with long-term
 *   firm performance, the concentration of power can lead to extraction.
 *
 * KEY AGENTS:
 *   - Senior Managing Directors: Primary beneficiary (institutional/arbitrage) — wield absolute voting control.
 *   - Common Shareholders: Primary victim (powerless/trapped) — lack influence over corporate governance.
 *   - Limited Partners: Secondary victim (moderate/constrained) — contractually limited ability to influence governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blackstone_smd_control, 0.6).
domain_priors:suppression_score(blackstone_smd_control, 0.7).
domain_priors:theater_ratio(blackstone_smd_control, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blackstone_smd_control, extractiveness, 0.6).
narrative_ontology:constraint_metric(blackstone_smd_control, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(blackstone_smd_control, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blackstone_smd_control, tangled_rope).
narrative_ontology:human_readable(blackstone_smd_control, "Blackstone Senior Managing Director Voting Control").
narrative_ontology:topic_domain(blackstone_smd_control, "economic").

domain_priors:requires_active_enforcement(blackstone_smd_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blackstone_smd_control, senior_managing_directors).
narrative_ontology:constraint_victim(blackstone_smd_control, common_shareholders).
narrative_ontology:constraint_victim(blackstone_smd_control, limited_partners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% SMDs benefit from the concentrated voting power, allowing them to maintain control and influence the firm's strategic direction. They can arbitrage this control for personal gain.
constraint_indexing:constraint_classification(blackstone_smd_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Common shareholders are largely trapped, with little recourse to influence firm direction given the SMDs' voting control. They are vulnerable to decisions that benefit SMDs at their expense.
constraint_indexing:constraint_classification(blackstone_smd_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% LPs are constrained by their investment agreements, which limit their ability to exit or directly influence firm governance. They experience a tangled rope: some influence but ultimate SMD control.
constraint_indexing:constraint_classification(blackstone_smd_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer recognizes the tangled rope nature: coordination and control for SMDs extracting value from other investors. There is coordination value but also asymmetric extraction.
constraint_indexing:constraint_classification(blackstone_smd_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_smd_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blackstone_smd_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_smd_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blackstone_smd_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(blackstone_smd_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is relatively high because SMD control can be used to extract value from the firm at the expense of common shareholders and limited partners. The suppression is also high, as shareholders have little recourse to challenge SMD decisions. The theater ratio is moderate, reflecting some degree of accountability to outside investors, though ultimately constrained by the voting structure.
 *
 * PERSPECTIVAL GAP:
 *   SMDs view the structure as coordination, enabling them to execute their strategy effectively. Common shareholders see it as a snare, limiting their influence. LPs experience the tangled rope, with some degree of influence but ultimately subject to SMD control. An analytical observer also views this as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   SMDs benefit significantly from the control structure, allowing them to direct firm strategy and capture value. Common shareholders bear the costs, lacking the ability to effectively challenge SMD decisions. LPs are in a mixed position, with some ability to influence decisions, but ultimately constrained by the governance structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_governance_structures,
    'What alternative governance structures could balance SMD control with shareholder rights?',
    'Comparative analysis of governance models in similar firms; shareholder proposals for governance reform.',
    'If viable alternatives exist: the entanglement may be reformed. If not, the dominance of the SMDs persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_governance_structures, conceptual, 'Are there viable governance structures that balance control with shareholder rights?').

omega_variable(
    smd_alignment,
    'To what extent are the SMDs'' interests aligned with those of other shareholders and LPs?',
    'Analysis of SMD compensation structure and investment decisions; tracking of insider trading activity.',
    'If interests are aligned: the extraction is seen as less problematic. If misaligned, then the value transfer is considered much worse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smd_alignment, empirical, 'How aligned are the SMDs'' interests with other stakeholders?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blackstone_smd_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blac_tr_t0, blackstone_smd_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(blac_tr_t5, blackstone_smd_control, theater_ratio, 5, 0.3).
narrative_ontology:measurement(blac_tr_t10, blackstone_smd_control, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(blac_be_t0, blackstone_smd_control, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(blac_be_t5, blackstone_smd_control, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(blac_be_t10, blackstone_smd_control, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blackstone_smd_control, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
