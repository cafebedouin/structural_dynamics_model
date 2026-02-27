% ============================================================================
% CONSTRAINT STORY: blackstone_smd_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
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
    narrative_ontology:affects_constraint/2,
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
 *   domain: economic/corporate_governance
 *
 * SUMMARY:
 *   Established during Blackstone's 2007 IPO, this corporate governance
 *   structure uses a multi-class unit system to grant Senior Managing
 *   Directors (SMDs) and founders absolute voting control over the firm's
 *   general partner. Public unitholders, who provide the majority of the
 *   firm's public capital, have no voting rights concerning the election of
 *   the general partner or its directors. This creates a permanent disconnect
 *   between economic ownership and control, a hallmark of a Tangled Rope
 *   structure.
 *
 * KEY AGENTS:
 *   - Senior Managing Directors (SMDs): Primary beneficiaries (institutional/arbitrage) who wield absolute voting control.
 *   - Public Unitholders: Primary victims (powerless/trapped) who provide capital but are denied agency or 'voice'.
 *   - Market Governance Advocates: Organized victims (organized/constrained) who challenge the structure based on normative principles of shareholder democracy.
 *   - Analytical Observer: Sees the dual function of coordination and extraction (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blackstone_smd_control, 0.65).
domain_priors:suppression_score(blackstone_smd_control, 0.8).
domain_priors:theater_ratio(blackstone_smd_control, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blackstone_smd_control, extractiveness, 0.65).
narrative_ontology:constraint_metric(blackstone_smd_control, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(blackstone_smd_control, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blackstone_smd_control, tangled_rope).
narrative_ontology:human_readable(blackstone_smd_control, "Blackstone Senior Managing Director Voting Control").
narrative_ontology:topic_domain(blackstone_smd_control, "economic/corporate_governance").

domain_priors:requires_active_enforcement(blackstone_smd_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blackstone_smd_control, senior_managing_directors).
narrative_ontology:constraint_beneficiary(blackstone_smd_control, founding_partners).
narrative_ontology:constraint_victim(blackstone_smd_control, public_unitholders).
narrative_ontology:constraint_victim(blackstone_smd_control, market_governance_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC UNITHOLDER (SNARE) — Provides capital but is stripped of voting rights. The only exit is selling units; there is no 'voice' to influence management or elect the general partner. The structure extracts agency completely. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.11.
constraint_indexing:constraint_classification(blackstone_smd_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SENIOR MANAGING DIRECTOR (ROPE) — Experiences the structure as a pure coordination mechanism. It insulates long-term strategy from the pressures of public markets and activist investors, enabling decisive management. For this group, it is a tool for effective governance. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09.
constraint_indexing:constraint_classification(blackstone_smd_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (enabling stable, long-term management) and the severe asymmetric extraction of voting power from capital providers. The structure is functional but highly coercive. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(blackstone_smd_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: MARKET GOVERNANCE ADVOCATE (SNARE) — Groups like the Council of Institutional Investors see this as a violation of governance norms that disenfranchises shareholders. They are organized but constrained, unable to force change directly, viewing the structure as pure extraction of rights. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.49. This is on the border of Tangled Rope and Snare, but from their normative position, the coordination function is illegitimate.
constraint_indexing:constraint_classification(blackstone_smd_control, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

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
 *   Extractiveness (ε=0.65) is high because the structure extracts the fundamental right of agency from capital providers. This control allows SMDs to dictate strategy, compensation, and fee structures that may not align with public unitholder interests. Suppression (0.80) is very high; the partnership agreement legally forecloses any possibility of unitholders exerting control. Their only option is to sell their stake. Theater (0.15) is low, as the structure does not pretend to be democratic; its control function is explicit.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the SMDs, the constraint is a Rope, a necessary tool to shield the firm from short-term market pressures and execute a long-term vision. For public unitholders, it is a Snare, trapping their capital in a vehicle where they have no say and their interests may be subordinated. The analytical view acknowledges both realities, classifying it as a Tangled Rope—a structure with a legitimate coordination purpose that is inextricably linked to a coercive extraction of power.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the structural roles. SMDs are declared beneficiaries with arbitrage exit, yielding a negative effective extraction (χ < 0) and a Rope classification. Public unitholders are victims with trapped exit options (within the governance system), yielding a very high positive χ and a Snare classification. The analytical observer's default parameters correctly identify the mixed nature of the constraint, resulting in the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case avoids mandatrophy by refusing to label the structure as either purely 'good governance' (Rope) or purely 'investor abuse' (Snare). The framework demonstrates that both are valid, indexed perspectives. The Tangled Rope classification from the analytical viewpoint correctly identifies the core tension: a mechanism that is simultaneously a tool for effective coordination for insiders and a means of coercive extraction for outsiders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_vs_extraction,
    'Is the absolute voting control by SMDs a necessary component for Blackstone''s long-term outperformance, or is it an extractive feature that benefits insiders at the expense of public unitholders?',
    'Comparative analysis of performance between dual-class/limited-partnership firms and single-class firms in the same sector over multiple economic cycles. Analysis of capital allocation decisions to see if they systematically favor SMD interests over unitholder returns.',
    'If necessary for performance, the constraint has a stronger Rope character from more perspectives. If not, it is more clearly a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_extraction, empirical, 'Whether SMD control is essential for performance or primarily extractive.').

omega_variable(
    cost_of_disenfranchisement,
    'What is the quantifiable economic cost (e.g., lower returns, higher fees, value destruction from insider-led decisions) to public unitholders from their lack of voting rights?',
    'Event studies around governance challenges, fee structure analysis compared to publicly-controlled peers, and quantification of any ''governance discount'' in the unit price.',
    'A high quantifiable cost would increase the base extractiveness (ε) and solidify the Snare classification from victim perspectives. A low or negligible cost would weaken it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_of_disenfranchisement, empirical, 'Quantifying the economic cost to unitholders from lack of voting rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blackstone_smd_control, 2007, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blac_tr_t2007, blackstone_smd_control, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(blac_tr_t2015, blackstone_smd_control, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(blac_tr_t2024, blackstone_smd_control, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(blac_be_t2007, blackstone_smd_control, base_extractiveness, 2007, 0.6).
narrative_ontology:measurement(blac_be_t2015, blackstone_smd_control, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(blac_be_t2024, blackstone_smd_control, base_extractiveness, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blackstone_smd_control, enforcement_mechanism).
narrative_ontology:affects_constraint(blackstone_smd_control, dual_class_share_prevalence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
