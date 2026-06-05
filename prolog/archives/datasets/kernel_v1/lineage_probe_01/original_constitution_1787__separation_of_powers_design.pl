% ============================================================================
% CONSTRAINT STORY: original_constitution_1787__separation_of_powers_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_original_constitution_1787__separation_of_powers_design, []).

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
 *   constraint_id: original_constitution_1787__separation_of_powers_design
 *   human_readable: Separation of Powers Design (1787 Constitution)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The separation-of-powers design of the 1787 Constitution instantiates a
 *   constraint that exhibits both genuine coordination function and
 *   systematic extraction. The core logic is Madisonian: liberty is protected
 *   by giving each branch 'the constitutional means and personal motives to
 *   resist' the others. Veto mechanisms (presidential veto, Senate
 *   supermajority for treaties/appointments, judicial review) prevent any
 *   single branch from accumulating power. However, this same veto structure
 *   has systematic effects: it suppresses coalitional coordination for
 *   reform, particularly when status-quo interests control key veto points.
 *   The constraint has evolved over 235 years from a high-friction
 *   coordination mechanism (1787) toward an increasingly theatrical
 *   legitimacy system (contemporary) — the constitutional language of checks
 *   and balances is invoked to justify structures that bear little
 *   resemblance to the original veto logic (e.g., presidential war-making
 *   despite textual vesting in Congress). The separation of powers is one
 *   reading of the 1787 Constitution's kernel; other readings (Article V
 *   amendment procedure, federal supremacy, slavery compromises) contest
 *   whether this reading identifies the Constitution's true binding
 *   commitment or a rationalization layered atop a different core.
 *
 * KEY AGENTS:
 *   - Status Quo Interest / Veto-Holder Bloc: Primary beneficiary (institutional/arbitrage) — controls one or more constitutional veto points (Senate, courts, presidency) and uses separation-of-powers structure to block unwanted change
 *   - Reform Coalition: Primary victim (powerless/trapped) — seeking policy coordination (abolition, suffrage, labor rights, civil rights) faces supermajority requirements and veto points designed to resist change
 *   - Congressional Majority Faction: Secondary victim and partial beneficiary (moderate/constrained) — experiences extraction when blocked but also benefits from veto power over opposing initiatives
 *   - Amendment-Era Reform Movement: Organized victim (organized/constrained) — can overcome gridlock through generational effort and electoral coalition-building, showing the constraint is not absolute
 *   - Constitutional System Analyst: Analytical observer (analytical/analytical) — risks naturalizing the veto structure as necessary tyranny-check when empirical evidence suggests it systematically favors entrenched interests
 *   - Constitutional Rhetoric / Legitimacy System: Institutional actor (institutional/arbitrage) — maintains performative invocation of checks and balances while actual institutional function has degraded or been superseded in specific domains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(original_constitution_1787__separation_of_powers_design, 0.38).
domain_priors:suppression_score(original_constitution_1787__separation_of_powers_design, 0.52).
domain_priors:theater_ratio(original_constitution_1787__separation_of_powers_design, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(original_constitution_1787__separation_of_powers_design, extractiveness, 0.38).
narrative_ontology:constraint_metric(original_constitution_1787__separation_of_powers_design, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(original_constitution_1787__separation_of_powers_design, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(original_constitution_1787__separation_of_powers_design, tangled_rope).
narrative_ontology:human_readable(original_constitution_1787__separation_of_powers_design, "Separation of Powers Design (1787 Constitution)").
narrative_ontology:topic_domain(original_constitution_1787__separation_of_powers_design, "political/legal/constitutional").

domain_priors:requires_active_enforcement(original_constitution_1787__separation_of_powers_design).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(original_constitution_1787__separation_of_powers_design, '1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0').
narrative_ontology:cs_kernel_codification('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', formalized).
narrative_ontology:cs_authority_grounding('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', lineage).
narrative_ontology:cs_interpretation_layer_present('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0').
narrative_ontology:cs_reading_relation('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', original_constitution_1787__slavery_compromises, coexists_with).
narrative_ontology:cs_reading_relation('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', original_constitution_1787__federal_supremacy_design, coexists_with).
narrative_ontology:cs_reading_relation('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', original_constitution_1787__article_v_amendment_procedure, influences).
narrative_ontology:cs_axiom('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', foundational, liberty_secured_by_institutional_competition).
narrative_ontology:cs_axiom_status(liberty_secured_by_institutional_competition, holdable).
narrative_ontology:cs_axiom_grounding('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', liberty_secured_by_institutional_competition, deontological).
narrative_ontology:cs_axiom('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', secondary, veto_as_structural_necessity_not_extractive_accident).
narrative_ontology:cs_axiom_status(veto_as_structural_necessity_not_extractive_accident, holdable).
narrative_ontology:cs_axiom_grounding('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', veto_as_structural_necessity_not_extractive_accident, instrumental).
narrative_ontology:cs_reference_frame('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', competitive_institutional_liberty_framework).
narrative_ontology:cs_drift_state('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', contemporary_presidential_dominance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1e75d32a-0a5b-4988-a8b3-97db7bc6a3c0', '').
narrative_ontology:cs_kernel_id(original_constitution_1787__separation_of_powers_design, original_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(original_constitution_1787__separation_of_powers_design, status_quo_interests).
narrative_ontology:constraint_beneficiary(original_constitution_1787__separation_of_powers_design, institutional_veto_holders).
narrative_ontology:constraint_victim(original_constitution_1787__separation_of_powers_design, reform_coalitions).
narrative_ontology:constraint_victim(original_constitution_1787__separation_of_powers_design, rapid_policy_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REFORM COALITION (SNARE) — Trapped by the veto structure. A coalition seeking to coordinate a new policy (abolition, suffrage expansion, labor regulation) faces supermajority requirements across three branches plus two-chamber Senate design. Each chokepoint is controlled by interests benefiting from inaction. No exit — the reformers cannot simply opt out of the national constitutional framework. Maximum extraction: their energy is dissipated in negotiating vetoes rather than implementing coordinated policy.
constraint_indexing:constraint_classification(original_constitution_1787__separation_of_powers_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL MAJORITY FACTION (TANGLED ROPE) — Constrained by presidential veto, Senate supermajority requirements, and judicial review. But also coordinates through the separation-of-powers framework: the House can block Senate initiatives, the President can block both, the courts can invalidate legislation. This faction experiences both extraction (its policies are vetoed) and coordination benefit (it has veto power over others' initiatives). The constraint is hybrid — genuine coordination function (preventing tyranny of a single branch) combined with asymmetric extraction (costs are borne differentially by reform coalitions).
constraint_indexing:constraint_classification(original_constitution_1787__separation_of_powers_design, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATUS QUO INTEREST / VETO-HOLDER BLOC (ROPE) — For the coalition that controls one or more veto points (e.g., Southern slaveholding senators, later conservative justices), the separation of powers is pure coordination: it enables them to block unwanted change through institutional means (filibuster, judicial review, executive refusal) without needing to argue the merits. The constraint is experienced as coordination — the institutional structure lets their bloc resist without constant mobilization.
constraint_indexing:constraint_classification(original_constitution_1787__separation_of_powers_design, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL ANALYST / CHECKS-AND-BALANCES FRAME (ROPE) — From the design perspective, separation of powers is a coordination mechanism solving the tyranny problem: by giving each branch the means and motives to resist the others, the constitution prevents concentrated power. No single branch can dominate; each must negotiate with the others. This is purely coordination logic — no agent perceives their interests as extracted in the framework's own terms. The framework treats the veto function as a protective feature, not as extraction.
constraint_indexing:constraint_classification(original_constitution_1787__separation_of_powers_design, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 5: AMENDMENT-ERA REFORM MOVEMENT (TANGLED ROPE) — Organized reformers (abolitionists, suffragists, New Deal coalitions) coordinate through and against the separation-of-powers framework. They use the framework (mobilizing electoral coalitions, shifting Senate composition) to shift veto-holder composition, effectively re-weighting the separation of powers. This faction experiences both extraction (the initial gridlock) and coordination (the constitutional amendment pathway, though arduous, eventually enables major policy shifts). The 13th, 15th, 19th, and 26th Amendments show the constraint is not absolute — organized movements can overcome it through generational effort.
constraint_indexing:constraint_classification(original_constitution_1787__separation_of_powers_design, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL RHETORIC / LEGITIMACY SYSTEM (PITON) — Over centuries, the separation of powers has become a legitimacy incantation. Invoked equally by those defending the status quo and by those seeking change. 'The separation of powers requires...', 'The founders intended...', 'Checks and balances demand...' — the rhetoric persists with high theater_ratio because the actual institutional function has become rigid or has atrophied in specific areas (e.g., war powers: the President operates as unilateral war-maker despite textual vesting in Congress; the separation-of-powers doctrine is invoked theatrically to justify post-hoc Congressional acquiescence). The piton classification reflects degradation: the original design intent (mutual checking) has given way to presidential dominance in some domains, while the constitutional language is maintained performatively.
constraint_indexing:constraint_classification(original_constitution_1787__separation_of_powers_design, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(original_constitution_1787__separation_of_powers_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(original_constitution_1787__separation_of_powers_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(original_constitution_1787__separation_of_powers_design, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(original_constitution_1787__separation_of_powers_design, TR),
    TR >= 0.70.

:- end_tests(original_constitution_1787__separation_of_powers_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The separation-of-powers structure imposes genuine costs on coalitional policy-making — supermajority requirements, veto points, amendment difficulty. But the extraction is not maximal because: (1) the mechanism is transparent and legible; (2) organized movements can overcome gridlock over generational timescales through electoral coalition-building; (3) the structure does prevent some forms of power concentration. A Snare (ε≥0.46) would require more opaque suppression or permanent entrenchment. Suppression (0.52): Moderate-high. Significant barriers to reform coordination include: supermajority requirements (Senate filibuster, amendment two-thirds threshold); veto distribution (presidency, courts); geographical dispersion of veto-holder interests; implicit anti-majoritarian design. But suppression is not absolute — the amendment mechanism, though costly, exists. Theater ratio (0.58): Moderate-high. Over time, the separation-of-powers rhetoric has become increasingly performative. Contemporary invocation of 'checks and balances' often justifies structures that deviate significantly from the original veto logic (e.g., presidential dominance in war powers, delegation of legislative authority to executive agencies). The theater has grown as the functional veto mechanisms have become rigid or asymmetrical — the rhetoric maintains legitimacy despite functionality degradation.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap in this constraint is between the status-quo veto-holder (Rope) and the reform coalition (Snare). The veto-holder experiences the constraint as pure coordination — their institutional position lets them block unwanted change through structural means. The reformer experiences extraction — their electoral and policy energy is dissipated in negotiating vetoes they cannot overcome in a single generation. The institutional analyst risks seeing only the Rope perspective (checks and balances protect against tyranny) and missing the Snare perspective (systematic suppression of reform coalitions). The amendment-era reform movement shows the constraint is not truly a Snare — organized, generational effort can overcome the veto structure — but this requires treating the veto system as a problem to be solved rather than as a system working as designed.
 *
 * DIRECTIONALITY LOGIC:
 *   The separation-of-powers structure produces different directionality values for different agents: Status-quo interests that control veto points derive d≈0.15 (beneficiaries with arbitrage exit); reform coalitions derive d≈0.90 (victims with trapped exit until they achieve supermajority); moderate-power factions derive d≈0.55 (both beneficiary and victim, depending on initiative). The engine derives these automatically from beneficiary/victim declarations and exit options. No overrides are needed — the beneficiary/victim structure is clear, and the exit options differentiate the perspectives naturally.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_as_tyranny_check_vs_anti_majoritarian_extraction,
    'Is the separation-of-powers veto structure a protection against tyranny (coordination function) or an extraction mechanism favoring entrenched minorities (snare function)?',
    'Historical counterfactual analysis: trace patterns of reform delay and policy blockage to identify whether veto system reliably blocks majoritarian tyranny (protects minorities from numeric disadvantage) or reliably blocks majoritarian will in domains where numerical majorities favor reform (extracts by suppressing coordination). Key signal: does veto power concentrate on defending human rights, or on defending property/status interests?',
    'If tyranny-check framing is accurate: constraint is Rope from all non-revolutionary perspectives. If extraction framing is accurate: constraint is Snare from reform coalition perspective and Tangled Rope from moderate-power faction perspective. The empirical record (slavery, civil rights delays, suffrage) suggests extraction framing is more structurally accurate than tyranny-check framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_as_tyranny_check_vs_anti_majoritarian_extraction, empirical, 'Whether veto structure functions as tyranny-check or extraction mechanism').

omega_variable(
    friction_cost_vs_deliberate_obstruction,
    'Does the separation-of-powers structure slow policy-making to improve deliberation (friction cost), or deliberately obstruct policy change benefiting majorities in order to protect minority interests (extraction mechanism)?',
    'Comparative constitutional study: do other democracies (Westminster systems, proportional legislatures) achieve equivalent deliberation with lower veto points? Do policy delays in US system show temporal patterning correlated with minority-interest protection vs. with time for thoughtful deliberation?',
    'If friction-for-deliberation thesis holds: extractiveness is ≤0.25 and constraint is Rope. If deliberate-obstruction thesis holds: extractiveness is ≥0.40 and constraint is Tangled Rope or Snare depending on perspective. Current evidence (UK House of Lords delays, bicameral review in proportional systems) suggests veto structure exceeds deliberation-optimization requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(friction_cost_vs_deliberate_obstruction, empirical, 'Whether separation of powers optimizes for deliberation or enables extraction').

omega_variable(
    reading_boundary_between_sop_and_slavery_compromises,
    'Is the separation-of-powers reading of the 1787 Constitution the true kernel, or is it a rationalization layered atop the real kernel (slavery compromise protections)?',
    'Historical intentionality analysis: did the Framers emphasize separation of powers as the primary design principle in their writings and debates, or is it a post-hoc rationalization? Did the specific veto mechanisms (supermajority Senate, presidential veto, judicial review) receive equal emphasis in Federalist arguments, or was slavery protection the binding constraint? Did slaveholding states demand the veto mechanisms for slavery protection, or were they abstract tyranny-checks?',
    'If SOP is the true kernel: this reading is autonomous and the slavery-compromises reading is parasitic on it. If slavery compromise is the true kernel: this reading provides ideological cover for race-based extraction, and the two readings are not simply ''different perspectives'' but ''self-serving rationalization vs. structural truth.'' This is the deepest structural uncertainty in the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_between_sop_and_slavery_compromises, conceptual, 'Whether SOP is authentic reading or ideological cover for slavery protection').

omega_variable(
    constitutional_amendment_as_safety_valve_or_illusory_escape,
    'Does the existence of the Article V amendment procedure (difficult but possible) mean the separation-of-powers veto is not truly extractive, or does the amendment difficulty itself constitute the extraction mechanism?',
    'Quantitative: measure amendment success rates for majority-backed reforms (those that poll above 60% public support) and calculate average time-to-passage. Compare to parliamentary systems'' policy-implementation timelines for equivalent majorities. If US amendments for majority-backed reforms take >50 years average, the difficulty of amendment means majorities are structurally suppressed for at least 1–2 generations.',
    'If amendments are genuinely accessible: extractiveness drops to ≤0.30 and constraint becomes Scaffold with clear sunset. If amendments are systematically difficult for majority reforms: extractiveness remains ≥0.35 and the constraint is validly Tangled Rope (amendment exists but is subordinate veto point).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_amendment_as_safety_valve_or_illusory_escape, empirical, 'Whether amendment procedure is functional relief valve or illusory escape').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(original_constitution_1787__separation_of_powers_design, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sopd_tr_t0, original_constitution_1787__separation_of_powers_design, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sopd_tr_t50, original_constitution_1787__separation_of_powers_design, theater_ratio, 50, 0.48).
narrative_ontology:measurement(sopd_tr_t100, original_constitution_1787__separation_of_powers_design, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(sopd_be_t0, original_constitution_1787__separation_of_powers_design, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sopd_be_t50, original_constitution_1787__separation_of_powers_design, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(sopd_be_t100, original_constitution_1787__separation_of_powers_design, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sopd_su_t0, original_constitution_1787__separation_of_powers_design, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sopd_su_t50, original_constitution_1787__separation_of_powers_design, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(sopd_su_t100, original_constitution_1787__separation_of_powers_design, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(original_constitution_1787__separation_of_powers_design, enforcement_mechanism).
narrative_ontology:affects_constraint(original_constitution_1787__separation_of_powers_design, original_constitution_1787__slavery_compromises).
narrative_ontology:affects_constraint(original_constitution_1787__separation_of_powers_design, original_constitution_1787__federal_supremacy_design).
narrative_ontology:affects_constraint(original_constitution_1787__separation_of_powers_design, original_constitution_1787__article_v_amendment_procedure).

% DUAL FORMULATION NOTE:
% The separation-of-powers reading is one of four competing readings of the 1787 Constitution kernel. The other readings (slavery compromises, federal supremacy, Article V amendment procedure) are implemented as separate constraint stories with different ε values and beneficiary/victim structures. The network links establish that these readings are siblings in a kernel contest, not independent constraints. The separation-of-powers reading's ε (0.38) reflects the pure veto-suppression mechanism; the slavery-compromises reading's ε would reflect racial entrenchment through representation; the federal-supremacy reading's ε would reflect centralization and loss of state autonomy. Each reading produces its own classification profile from perspectives sensitive to that reading's core claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
