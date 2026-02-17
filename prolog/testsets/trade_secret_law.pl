% ============================================================================
% CONSTRAINT STORY: trade_secret_law
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_trade_secret_law, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trade_secret_law
 *   human_readable: Trade Secret Law (Information Ownership)
 *   domain: legal/economic
 *
 * SUMMARY:
 *   Trade Secret Law protects confidential business information (formulae,
 *   practices, designs) that provides an enterprise a competitive advantage.
 *   It functions as a coordination mechanism to encourage investment in
 *   innovation, but can also be used extractively to suppress employee
 *   mobility and whistleblowing activity through the threat of litigation.
 *
 * KEY AGENTS (by structural relationship):
 *   - Departing Employees & Whistleblowers: Primary target (powerless/trapped) — bears extraction via litigation risk.
 *   - Innovative Enterprises: Primary beneficiary (institutional/arbitrage) — benefits from IP protection.
 *   - The Courts: Institutional actor (institutional/arbitrage) — arbitrates disputes, balancing interests.
 *   - Analytical Observer: Sees the dual coordination/extraction function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction is significant. The cost of litigation, career disruption, and
% the chilling effect on mobility represent a substantial transfer of value
% (in the form of reduced options) from employees to employers.
domain_priors:base_extractiveness(trade_secret_law, 0.48).

% Suppression is high. The ambiguity of what constitutes a "trade secret" and
% the high cost of legal defense strongly suppress alternatives for employees,
% who may avoid changing jobs or reporting misconduct to avoid legal risk.
domain_priors:suppression_score(trade_secret_law, 0.65).

% Theater is moderate. While the law has a core function, litigation can be
% performative, aimed at signaling to other employees rather than just winning
% a specific case.
domain_priors:theater_ratio(trade_secret_law, 0.30).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trade_secret_law, extractiveness, 0.48).
narrative_ontology:constraint_metric(trade_secret_law, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(trade_secret_law, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(trade_secret_law, tangled_rope).
narrative_ontology:human_readable(trade_secret_law, "Trade Secret Law (Information Ownership)").

% --- Binary flags ---
% This is a legal framework that requires courts and litigation to function.
domain_priors:requires_active_enforcement(trade_secret_law).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(trade_secret_law, innovative_enterprises).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(trade_secret_law, departing_employees_and_whistleblowers).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% For a departing employee or whistleblower, the law is a Snare. The high
% base extraction (0.48) and suppression (0.65) combined with their trapped
% status (high legal costs, career risk) creates a coercive environment where
% their options are severely limited. The engine derives d≈0.95, leading to high χ.
constraint_indexing:constraint_classification(trade_secret_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For the innovative enterprise, the law is a Rope. It's a coordination tool
% that protects their investment in R&D, enabling them to secure funding and
% compete. From this perspective, the extraction is seen as a necessary cost
% of maintaining the system. The engine derives d≈0.05, leading to negative χ.
constraint_indexing:constraint_classification(trade_secret_law, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical observer sees both the coordination function (protecting
% innovation) and the asymmetric extraction (suppressing labor mobility).
% The metrics (ε=0.48, suppression=0.65) and structural data (beneficiary,
% victim, enforcement) meet the criteria for a Tangled Rope.
constraint_indexing:constraint_classification(trade_secret_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE COURTS (ROPE)
% For the judiciary, the law is a Rope—a framework for balancing the competing
% interests of innovation, competition, and individual mobility. They wield it
% as a tool for adjudication, reinforcing its function as a coordination mechanism.
constraint_indexing:constraint_classification(trade_secret_law, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trade_secret_law_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(trade_secret_law, snare, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(trade_secret_law, rope, context(agent_power(institutional), _, arbitrage, _)),
    constraint_indexing:constraint_classification(trade_secret_law, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_tangled_rope) :-
    narrative_ontology:constraint_metric(trade_secret_law, extractiveness, E),
    narrative_ontology:constraint_metric(trade_secret_law, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(trade_secret_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.48) and suppression (0.65) were set to reflect
 *   the significant, non-trivial costs imposed on employees. The original file's
 *   low scores (0.2, 0.3) understated the coercive power of litigation threats,
 *   which can effectively trap employees and chill labor mobility. These higher
 *   values are necessary for the Snare and Tangled Rope classifications to be
 *   structurally sound. The incorrect `emerges_naturally` flag was removed, as
 *   this is a human-enforced legal construct, not a natural law.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For an employee facing a lawsuit, the law is a Snare that
 *   threatens their livelihood. For the company using the law, it's a Rope that
 *   protects their assets. For the courts, it's a Rope for balancing societal
 *   interests. The analytical view must acknowledge both functions, hence the
 *   Tangled Rope classification, which is the only type that captures both a
 *   genuine coordination function and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `innovative_enterprises` directly benefit from the protection
 *     of their intellectual property, which is a capital asset.
 *   - Victim: `departing_employees_and_whistleblowers` bear the costs. They face
 *     legal threats that limit their career options and ability to expose
 *     wrongdoing, effectively subsidizing the beneficiary's security. The original
 *     victim group `industrial_spies` was too narrow and missed this core conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope prevents two errors. It avoids the naive
 *   view that this is a pure Rope (ignoring the harm to employees) and the cynical
 *   view that it's a pure Snare (ignoring its legitimate role in fostering
 *   innovation). The framework correctly identifies that the constraint's identity
 *   is defined by this dual, conflicting nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The core uncertainty is whether the law's application will drift towards
% greater extraction, effectively becoming a de-facto non-compete agreement.
omega_variable(
    omega_trade_secret_law,
    "Will courts increasingly use the 'Inevitable Disclosure Doctrine' to treat Trade Secret Law as a de-facto non-compete, shifting its balance from a Tangled Rope towards a pure Snare for high-skill labor?",
    "Monitoring case law trends in states with and without strong 'Inevitable Disclosure' doctrines (e.g., New York vs. California); legislative efforts to clarify or limit the doctrine's scope.",
    "If Yes: The constraint's base extractiveness increases, and it becomes a Snare from more perspectives. If No: It remains a balanced Tangled Rope.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_trade_secret_law, empirical, "The potential for the 'Inevitable Disclosure Doctrine' to transform trade secret law into a de-facto non-compete agreement.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(trade_secret_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction > 0.46 requires temporal data for drift detection.
% This models a slight increase in extraction over time, reflecting the
% "inevitable disclosure" drift mentioned in the omega variable.
%
% Theater ratio over time (stable):
narrative_ontology:measurement(tsl_tr_t0, trade_secret_law, theater_ratio, 0, 0.30).
narrative_ontology:measurement(tsl_tr_t5, trade_secret_law, theater_ratio, 5, 0.30).
narrative_ontology:measurement(tsl_tr_t10, trade_secret_law, theater_ratio, 10, 0.30).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(tsl_ex_t0, trade_secret_law, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(tsl_ex_t5, trade_secret_law, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(tsl_ex_t10, trade_secret_law, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a legal framework enforced by the state.
narrative_ontology:coordination_type(trade_secret_law, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */