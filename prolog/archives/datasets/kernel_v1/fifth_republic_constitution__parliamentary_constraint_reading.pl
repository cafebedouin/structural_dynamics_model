% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Constitutional Executive Coordination via Parliamentary Authorization
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The Fifth Republic Constitution of 1958 establishes a semi-presidential
 *   system in which executive power is vested in a president with prime
 *   ministerial oversight. This constraint story instantiates the
 *   parliamentary constraint reading: the executive's policy implementation
 *   requires authorization from the legislative Assembly, creating a
 *   coordination mechanism that prevents unilateral executive action while
 *   enabling effective governance. Under this reading, the president is
 *   constrained by the need to maintain legislative confidence or secure
 *   legislative majorities for key policy initiatives. The constraint is
 *   low-extractive (ε=0.28) because it solves a genuine collective action
 *   problem — ensuring executive accountability — rather than serving as an
 *   extraction mechanism. The constraint's legitimacy is grounded in the
 *   axiom that executive power requires ongoing democratic consent,
 *   instantiated through legislative authorization gates. This reading
 *   coexists with alternative constitutional interpretations
 *   (hyper-presidential reading emphasizing executive autonomy; cohabitation
 *   equilibrium reading describing power-balanced executive-legislative
 *   relations), but it represents a distinct and coherent reading of the
 *   constitutional kernel.
 *
 * KEY AGENTS:
 *   - The President: Powerful actor (mobile exit but constrained by authorization requirements) — initiates policy but requires legislative support; can be removed by no-confidence vote or forced to appoint opposition prime minister
 *   - The Legislative Assembly: Organized majority (mobile exit) — holds authorization gate; can block legislation or withdraw confidence; benefits from constraint structure preserving legislative power
 *   - Parliamentary Minority: Moderate power (constrained exit) — retains obstruction and voice but subordinate to majority; experiences mixed coordination and extraction
 *   - The Constitutional Court: Powerful institutional actor (mobile exit) — provides dispute resolution and constitutional interpretation; sunset-bounded role
 *   - The Electorate: Powerless actors (constrained to electoral windows) — coordinates government selection through elections; experiences extraction between electoral cycles
 *   - Analytical Observer: Views constraint as pure coordination mechanism solving democratic accountability problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.28).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.42).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Constitutional Executive Coordination via Parliamentary Authorization").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '92cd1664-efd3-474d-925a-eae57d965451').
narrative_ontology:cs_kernel_codification('92cd1664-efd3-474d-925a-eae57d965451', formalized).
narrative_ontology:cs_authority_grounding('92cd1664-efd3-474d-925a-eae57d965451', lineage).
narrative_ontology:cs_interpretation_layer_present('92cd1664-efd3-474d-925a-eae57d965451').
narrative_ontology:cs_reading_relation('92cd1664-efd3-474d-925a-eae57d965451', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('92cd1664-efd3-474d-925a-eae57d965451', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('92cd1664-efd3-474d-925a-eae57d965451', foundational, executive_requires_legislative_authorization).
narrative_ontology:cs_axiom_status(executive_requires_legislative_authorization, holdable).
narrative_ontology:cs_axiom_grounding('92cd1664-efd3-474d-925a-eae57d965451', executive_requires_legislative_authorization, deontological).
narrative_ontology:cs_axiom('92cd1664-efd3-474d-925a-eae57d965451', foundational, authorization_gates_are_binding_veto_points).
narrative_ontology:cs_axiom_status(authorization_gates_are_binding_veto_points, holdable).
narrative_ontology:cs_axiom_grounding('92cd1664-efd3-474d-925a-eae57d965451', authorization_gates_are_binding_veto_points, conventional).
narrative_ontology:cs_reference_frame('92cd1664-efd3-474d-925a-eae57d965451', constitutional_legislative_supremacy).
narrative_ontology:cs_drift_state('92cd1664-efd3-474d-925a-eae57d965451', contemporary_executive_power_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('92cd1664-efd3-474d-925a-eae57d965451', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, public_interest_in_stable_governance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESIDENT (ROPE) — Experiences the constraint as coordination with friction. Executive action requires legislative authorization, creating transaction costs and uncertainty but also legitimacy. The president is constrained by the need to build coalition support, not trapped — multiple pathways exist (government formation, confidence votes, legislative packages). Sees the constraint as the price of democratic legitimacy rather than extraction.
constraint_indexing:constraint_classification(fifth_republic_constitution__parliamentary_constraint_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE MAJORITY (ROPE) — Coordinates executive action through authorization gates. Majority has agency: it can withhold confidence, block legislation, or form alternative governments. No victim dynamics here — majority benefits from the gate structure because it preserves legislative power against executive overreach. Low extraction from this perspective because the coordination is genuinely symmetric.
constraint_indexing:constraint_classification(fifth_republic_constitution__parliamentary_constraint_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENTARY MINORITY (TANGLED ROPE) — Constrained by voting power but retains voice and obstruction options (legislative amendments, procedural delays, electoral mobilization). Experiences both coordination benefit (the constraint protects them from executive unilateral action) and extraction cost (majority can override them through normal legislative process). Mixed structure: genuine coordination function coexists with asymmetric legislative extraction.
constraint_indexing:constraint_classification(fifth_republic_constitution__parliamentary_constraint_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL COURT (SCAFFOLD) — Provides temporary support for constitutional interpretation when executive-legislative conflict reaches judicial threshold. Court's role is sunset-bounded: it resolves disputes but does not govern. As jurisprudence stabilizes around constitutional interpretation, the court's active role diminishes. Low extraction because judicial review is a coordination mechanism with boundaries, not an institutional grab for power.
constraint_indexing:constraint_classification(fifth_republic_constitution__parliamentary_constraint_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORATE (TANGLED ROPE) — Coordinates government selection through elections (genuine coordination function) but experiences asymmetric extraction: limited ability to dislodge entrenched coalitions between elections, gerrymanders, electoral law advantages, and media access concentration benefit established parties. Constrained by infrequent electoral windows. Mixed structure: elections ARE coordination; the intervals between them ARE extraction.
constraint_indexing:constraint_classification(fifth_republic_constitution__parliamentary_constraint_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From civilizational scale, the parliamentary constraint is a coordination mechanism that solves the deep structural problem: how to vest executive power without enabling dictatorship. Authorization gates, confidence votes, and legislative override ensure executive action requires ongoing consent. Low theater (0.35) reflects genuine coordination logic, not performative ritual. The constraint exists because it solves a real problem, not because it extracts value.
constraint_indexing:constraint_classification(fifth_republic_constitution__parliamentary_constraint_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fifth_republic_constitution__parliamentary_constraint_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fifth_republic_constitution__parliamentary_constraint_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low. The authorization requirement solves a genuine coordination problem — preventing executive unilateralism while enabling coherent governance — rather than functioning as an extraction mechanism. The constraint does not persistently transfer resources or power from one agent to another; instead, it structures the bargaining process for policy implementation. Beneficiaries (legislative majority, public interest in stable governance) are not parasitically receiving value; they are participating in the coordination mechanism. The president's constraint is high, but it is a governance friction cost, not extraction. Suppression (0.42): Moderate. The authorization gate is enforced through constitutional structure (no-confidence provisions, legislative override), but enforcement is not coercive — it operates through normal democratic procedures. Presidents have multiple pathways (coalition-building, executive decree within constitutional bounds, dissolution of Assembly to force new elections). Suppression reflects the difficulty of circumventing the authorization requirement, but not elimination of alternatives. Theater ratio (0.35): Low. Parliamentary debate and authorization votes perform a genuine coordination function — they aggregate preferences, create public record, and enforce legislative accountability. Theater is minimal because the votes actually determine outcomes (bills can fail, governments can fall). This differs from purely performative legislative process.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer sees pure coordination (Rope); the president sees coordination with friction (Rope, constrained exit); the legislative majority sees their power preserved (Rope, mobile exit); the opposition sees mixed coordination and extraction because they are constrained within the majority-set system (Tangled Rope); the electoral public sees elections as coordination with extraction in the intervals (Tangled Rope). The constitutional court sees its role as sunset-bounded (Scaffold). All perspectives except the hyper-presidential reading (absent here, as it is an alternative reading) converge on low-extractive classification. The unified classification across most perspectives indicates that this reading instantiates genuine coordination rather than disguised extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The president's directionality (d) as a powerful actor with constrained exit options who is a victim of the authorization requirement would derive to ~0.55-0.65 under the automatic pipeline, but the structural reality is more nuanced: the president is NOT a victim in the snare sense (not experiencing maximal extraction) because the constraint solves a problem the president themselves benefits from — unilateral presidential power creates backlash and constitutional crisis, whereas authorized executive action is more durable. The legislative majority's directionality as beneficiary with mobile exit derives to ~0.15, reflecting low extraction experience — they are not profiting from the constraint, but rather governing through it. The authorization gate is symmetric across time because both executive and legislative actors benefit from coordination, even though power distribution varies with electoral outcomes. No directionality overrides are needed because the structural data is clear: this is coordination, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy because the low extractiveness (0.28) and low theater ratio (0.35) cohere around a single structural claim: the constraint solves the democratic accountability problem through authorization gates that work. If extractiveness were high (>0.46) while theater remained low, mandatrophy would arise — the question would be 'is this coordination or extraction masquerading as coordination?' But at ε=0.28, the constraint is cleanly Rope from all perspectives except the opposition (which sees Tangled Rope due to their constrained power). The perspectival gap is small because the constraint is low-extractive. The analytical observer and the beneficiary roughly agree that this is coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    presidential_vs_parliamentary_axiom_stability,
    'Is the parliamentary constraint reading grounded in a stable foundational axiom about executive accountability, or is it contingent on specific French institutional configurations that could shift?',
    'Historical analysis of constitutional interpretation drift; comparative analysis with other semi-presidential systems (Germany, Portugal, Poland) showing whether the authorization gate is structurally necessary or politically contingent',
    'If axiom is stable: parliamentary_constraint_reading is holdable across regime transitions. If contingent: the reading is vulnerable to cohabitation collapse or hyper-presidential reinterpretation when power distributions change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(presidential_vs_parliamentary_axiom_stability, conceptual, 'Stability of parliamentary constraint as foundational principle vs. political contingency').

omega_variable(
    cohabitation_as_extreme_case_or_alternative_reading,
    'Does cohabitation (president and prime minister from different coalitions) represent an extreme edge case of the parliamentary constraint reading, or does it instantiate a distinct equilibrium reading with its own ε value?',
    'ε measurement for cohabitation periods: does extractiveness drop below 0.30 and theater ratio drop below 0.30 (indicating pure coordination), or does extractiveness remain 0.28-0.35 with elevated suppression (indicating same constraint operating under power-balanced conditions)?',
    'If edge case: single constraint story with contextual measurement variation. If alternative reading: three separate constraint stories (parliamentary, hyper-presidential, cohabitation) with different ε values and beneficiary structures. This affects how the engine treats constitutional drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_as_extreme_case_or_alternative_reading, empirical, 'Whether cohabitation represents an edge case or an alternative constitutional reading').

omega_variable(
    authorization_gate_actual_vs_ceremonial,
    'Is parliamentary authorization an actual veto point (bills fail, government falls) or largely ceremonial in practice (majority rubber-stamps executive proposals)?',
    'Legislative defeat rate analysis: frequency of government bills blocked or defeated; confidence vote outcomes; Prime Minister resignation triggers. High rubber-stamp rate (>90% passage) would indicate theater_ratio should be 0.6+; high defeat rate (<20% passage) indicates theater_ratio ≤ 0.35.',
    'If ceremonial: constraint is Piton or heavily Tangled Rope with high theater. If actual veto point: constraint is genuine Rope with low theater. Shapes whether the constraint solves its stated coordination problem or naturalizes executive dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authorization_gate_actual_vs_ceremonial, empirical, 'Whether parliamentary authorization is substantive veto point or ceremonial gate').

omega_variable(
    reading_identity_and_kernel_foreclosure,
    'Does the parliamentary constraint reading logically foreclose the hyper-presidential reading, or do they coexist as alternative constitutional interpretations held by different political actors?',
    'Jurisprudential analysis: can a single legal framework hold both ''executive action requires legislative authorization'' (parliamentary reading) AND ''executive can act unilaterally on emergency and foreign policy'' (hyper-presidential reading) simultaneously, or does one axiom directly contradict the other?',
    'If foreclosed: parliamentary reading is the sole valid interpretation and hyper-presidential is a constitutional violation. If coexist: both readings are live and the actual constitution contains the ambiguity that creates institutional conflict. Affects how DS constraints are linked in network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_and_kernel_foreclosure, conceptual, 'Whether parliamentary constraint reading forecloses or coexists with hyper-presidential reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr5const_tr_t0, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(fr5const_tr_t15, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(fr5const_tr_t30, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(fr5const_be_t0, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(fr5const_be_t15, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(fr5const_be_t30, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The Fifth Republic Constitution contains an irreducible ambiguity about executive power. Three constraint stories model three coherent readings: (1) parliamentary_constraint_reading (this file, ε=0.28, Rope) — authorization gates are binding; (2) hyper_presidential_reading (ε≈0.50, Tangled Rope expected) — executive can unilaterally act on emergency/decree; (3) cohabitation_equilibrium_reading (ε≈0.35, Tangled Rope expected) — power is balanced when president and PM derive from opposed coalitions. These are not measurement artifacts or observer-dependent phenomena. They instantiate genuinely different constitutional readings grounded in different axioms about executive accountability. The network linking shows how each reading influences the others: parliamentary reading creates conditions for cohabitation by constraining unilateral executive action; hyper-presidential reading challenges parliamentary dominance by asserting executive autonomy zones; cohabitation reading emerges when power distributions force all three logics into active competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
