% ============================================================================
% CONSTRAINT STORY: sotu_1975_ford_veto_moratorium_new_programs
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1975_ford_veto_moratorium_new_programs, []).

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
 *   constraint_id: sotu_1975_ford_veto_moratorium_new_programs
 *   human_readable: Ford Administration Veto Moratorium on New Federal Programs (FY1976)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   President Gerald Ford's commitment in the January 1975 State of the Union
 *   to veto all new federal spending programs except energy, combined with an
 *   administrative freeze on new program initiation through FY1976,
 *   represents a structural constraint on legislative budgetary scope. The
 *   moratorium emerged from Ford's conviction that deficit spending (then
 *   running at $53 billion annually) required executive discipline over
 *   program expansion. The constraint manifests as a credible threat —
 *   Congress must either accept the moratorium or command a two-thirds
 *   supermajority to override vetoes on any initiative. The mechanism is both
 *   coordinative (it clarifies executive fiscal boundaries and consolidates
 *   executive authority) and extractive (it prevents legislative expansion of
 *   federal scope and shifts the burden of proof onto Congress to demonstrate
 *   veto-override consensus). The constraint exhibits properties of Tangled
 *   Rope: genuine coordination function (defining fiscal limits, signaling
 *   deficit seriousness) coexists with asymmetric extraction (constituencies
 *   seeking new programs face higher legislative bar; executive gains
 *   unilateral agenda-setting power). From different structural positions,
 *   the moratorium appears as pure coordination (Rope from executive
 *   perspective), temporary problem-solving (Scaffold from fiscal
 *   conservative perspective), performative ritual (Piton from appropriations
 *   machinery perspective), crushing barrier (Snare from unorganized
 *   constituencies perspective), mixed burden (Tangled Rope from Congress
 *   perspective), and constitutional immutability (Mountain from naturalized
 *   separation-of-powers perspective). The false summit detector identifies
 *   the mountain classification as naturalization of a discretionary policy
 *   choice.
 *
 * KEY AGENTS:
 *   - Executive Branch (Office of the President / OMB): Primary beneficiary (institutional/arbitrage) — consolidates budgetary authority, credibly commits to deficit control, gains unilateral veto power over legislative scope
 *   - Congressional Democratic Leadership: Secondary institutional actor (organized/constrained) — faces veto threat but can coordinate and occasionally override; benefits from clarity on executive boundaries; constrained by two-thirds threshold
 *   - Fiscal Conservative Coalition: Organized beneficiary (organized/mobile) — benefits from moratorium on deficit growth; temporary commitment aligns with FY1976 crisis narrative; exit visible (moratorium expires)
 *   - Program Expansion Constituencies (schools, welfare, environment, labor): Primary victims (powerless/trapped) — cannot exit constraint without coalition formation; face indefinite delay for new initiatives; no countervailing power during FY1976
 *   - Congressional Appropriations Process: Institutional actor (powerful/mobile) — maintains ceremonial function (hearings, markups) while real authority shifted to executive veto threat; process persists through inertia rather than substantive role
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing executive veto as constitutional immutability rather than discretionary policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1975_ford_veto_moratorium_new_programs, 0.52).
domain_priors:suppression_score(sotu_1975_ford_veto_moratorium_new_programs, 0.65).
domain_priors:theater_ratio(sotu_1975_ford_veto_moratorium_new_programs, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1975_ford_veto_moratorium_new_programs, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1975_ford_veto_moratorium_new_programs, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1975_ford_veto_moratorium_new_programs, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1975_ford_veto_moratorium_new_programs, tangled_rope).
narrative_ontology:human_readable(sotu_1975_ford_veto_moratorium_new_programs, "Ford Administration Veto Moratorium on New Federal Programs (FY1976)").
narrative_ontology:topic_domain(sotu_1975_ford_veto_moratorium_new_programs, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1975_ford_veto_moratorium_new_programs).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1975_ford_veto_moratorium_new_programs, executive_branch_deficit_control).
narrative_ontology:constraint_beneficiary(sotu_1975_ford_veto_moratorium_new_programs, fiscal_conservatives).
narrative_ontology:constraint_victim(sotu_1975_ford_veto_moratorium_new_programs, program_expansion_constituencies).
narrative_ontology:constraint_victim(sotu_1975_ford_veto_moratorium_new_programs, congressional_legislative_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNORGANIZED CONSTITUENCIES SEEKING NEW PROGRAMS (SNARE) — School districts, welfare advocates, environmental groups, and labor unions seeking new federal initiatives face a veto threat with no countervailing power. Congressional allies exist but lack override votes. Cannot exit the constraint without organizing into coalition or waiting out administration. Maximum experienced extraction — the moratorium forces a two-thirds supermajority requirement for any new initiative, effectively terminating legislative agenda.
constraint_indexing:constraint_classification(sotu_1975_ford_veto_moratorium_new_programs, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL DEMOCRATIC LEADERSHIP (TANGLED ROPE) — Benefits from the constraint's clarity (unambiguous veto signal enables strategic focus). Faces high cost to override (two-thirds threshold). Can coordinate internally and occasionally override on priority items. Extraction is real (legislative agenda constrained) but not maximal — some alternatives exist (budget amendments, rider insertion, post-FY1976 initiatives). Bipartisan logic: some Republicans want spending control, some Democrats want to preserve override possibility.
constraint_indexing:constraint_classification(sotu_1975_ford_veto_moratorium_new_programs, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE OFFICE OF THE PRESIDENT (ROPE) — Primary beneficiary. The veto moratorium achieves genuine coordination: consolidates executive authority over budgetary scope, clarifies fiscal boundaries, enables credible commitment to deficit control. The constraint is a coordination tool for the executive — it solves the problem of legislative scope-creep by establishing a bright-line rule. Extraction runs toward this agent (increased budgetary authority), but the mechanism is coordination, not coercion.
constraint_indexing:constraint_classification(sotu_1975_ford_veto_moratorium_new_programs, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FISCAL CONSERVATIVE COALITION (SCAFFOLD) — The moratorium is explicitly temporary policy for FY1976 (with implicit possibility of continuation). Organized agents (Office of Management and Budget, fiscal hawks in Congress, business groups) see the constraint as a temporary mechanism to break the deficit-spending cycle. Low extraction because exit is visible (moratorium lifts after FY1976) and alternative pathways exist (consolidation, efficiency, economic growth reducing deficit without new programs). Sunset clause is implicit in annual appropriation cycles.
constraint_indexing:constraint_classification(sotu_1975_ford_veto_moratorium_new_programs, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESSIONAL APPROPRIATIONS MACHINERY (PITON) — The veto moratorium invokes the formal legislative process (override votes, appropriation markup) but substantially short-circuits the normal authorization and committee deliberation functions. The appropriations process persists in ceremonial form (budget hearings, committee markups) while the real decision (veto threat) lies outside. Theater ratio high: procedure occurs (markup, floor votes) but outcomes predetermined by veto threat. The institutional apparatus continues through inertia because no alternative budgeting system has fully replaced it.
constraint_indexing:constraint_classification(sotu_1975_ford_veto_moratorium_new_programs, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL LOGIC VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, the veto power is constitutionally immutable — the President's ability to block legislation is a structural feature of separation of powers written into the Constitution. No agent can exit or override this constraint without constitutional amendment. From this view, the moratorium merely operationalizes an already-existing constitutional architecture. However, the structural data contradicts the mountain classification — identifiable beneficiaries (executive branch), victims (program constituencies), and active enforcement (threat) indicate a contingent political strategy, not a natural law. The engine will detect this as a false summit, revealing that constitutional framing naturalizes what is actually a discretionary policy choice.
constraint_indexing:constraint_classification(sotu_1975_ford_veto_moratorium_new_programs, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1975_ford_veto_moratorium_new_programs_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1975_ford_veto_moratorium_new_programs, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1975_ford_veto_moratorium_new_programs, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1975_ford_veto_moratorium_new_programs, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1975_ford_veto_moratorium_new_programs, TR),
    TR >= 0.70.

:- end_tests(sotu_1975_ford_veto_moratorium_new_programs_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The moratorium imposes real fiscal constraints on new initiatives, effectively elevating the legislative bar from simple majority (authorization) to supermajority (override). The extractiveness is not maximal (0.70+) because exit options exist (override votes, post-FY1976 legislation, workarounds via program expansion or executive agency reinterpretation). The rise from 0.35 at FY1975 start to 0.52 by mid-FY1976 reflects increasing congressional accommodation — agencies and advocates internalize the veto threat, reducing formal veto exercises needed. Suppression (0.65): Moderate-high. Constituencies face real barriers: two-thirds override threshold is empirically high (only 9 of 37 Ford vetoes were overridden during his presidency), Congressional Democrats control only simple majority, and political costs of override attempts discourage frequent challenges. But suppression is not total (0.85+) because override options exist and some initiatives do succeed. Theater ratio (0.48): Moderate. The moratorium has genuine fiscal function (deficit reduction, scope boundary) but also performative elements (SOTU rhetoric, budget theater, committee hearings that confirm predetermined veto outcomes). Theater rises slightly as the fiscal emergency narrative matures into routine practice.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how a single structural phenomenon — executive veto threat over fiscal scope — produces divergent classifications based on observer position. The Executive Office sees coordination (Rope): the moratorium solves the governance problem of credible fiscal commitment. Congressional Democrats see extraction under coordination (Tangled Rope): the threat creates real constraint on legislative agenda while some coordination benefit exists (clarified executive boundaries). Unorganized constituencies see pure extraction (Snare): they cannot organize, cannot override, cannot exit. The fiscal conservative coalition sees temporary control (Scaffold): the moratorium is explicitly FY1976-bounded, sunset is visible. The appropriations process sees institutional degradation (Piton): the machinery persists but outcomes are predetermined. The analytical observer risks seeing constitutional law (Mountain): the veto power is constitutionally immutable. But the structural data reveals this mountain as false — the discretionary invocation of veto power over program scope is a policy choice, not a constitutional requirement. The gap between the mountain perspective and the snare/rope perspectives is the most diagnostic — it reveals that constitutional framing naturalizes what is actually a political strategy.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation flows from beneficiary/victim status and exit options. Executive Office (beneficiary + arbitrage) gets d ≈ 0.12, producing f(d) ≈ -0.08, chi contribution is negative (extraction favors this agent). Unorganized constituencies (victim + trapped) get d ≈ 0.92, producing f(d) ≈ 1.38, chi contribution is maximized. Congressional Democrats (mixed, organized, constrained) get d ≈ 0.58, producing f(d) ≈ 0.65. Fiscal conservatives (beneficiary + mobile) get d ≈ 0.22, producing f(d) ≈ 0.08. The appropriations process (institutional, mobile but degraded function) gets d ≈ 0.65, producing f(d) ≈ 1.00. These d values encode real structural relationships: who can escape, who is trapped, who benefits, who bears costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through perspectival specificity. The question is not 'is this Rope or Snare?' but 'which structural position are you measuring from?' From the Executive Office perspective, the moratorium IS a coordination mechanism (Rope) — it solves the fiscal governance problem. From the constituency perspective, it IS pure extraction (Snare) — they face a barrier they cannot surmount. From the Congressional perspective, it IS a hybrid (Tangled Rope) — some coordination benefit, real extraction cost. From the crisis-management perspective, it IS temporary scaffolding (Scaffold). These are not contradictory — they are different instantiations of the same constraint evaluated at different points on the (P,T,E,S) lattice. The mandatrophy is resolved by accepting that a constraint can be any type depending on observer position, and that all positions are valid. What prevents mislabeling is the structural data: executive veto threat (enforcement), beneficiaries (deficit control advocates), victims (new program constituencies), and temporary framing (FY1976 bounds). These facts anchor the classification across perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_threat_vs_actual_exercise,
    'Is the moratorium''s extraction mechanism the veto threat itself, or does it require actual vetoes to function?',
    'Historical analysis: count actual vetoes issued in FY1976 vs threat-induced legislative withdrawals; identify bills that failed markup vs those that passed but without new program elements due to anticipatory accommodation',
    'If threat-based (preventative): extraction operates through coordination of expectations and can persist with zero actual vetoes. If exercise-based (punitive): extraction requires actual vetoes to signal credibility, making it resource-intensive and potentially less sustainable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_threat_vs_actual_exercise, empirical, 'Whether moratorium operates through threat or through actual vetoes').

omega_variable(
    definitional_scope_ambiguity,
    'What counts as a ''new program''? Does expansion of existing programs constitute a new program, or only addition of genuinely novel functions?',
    'Examination of OMB guidance memoranda and veto rationales; identification of edge cases where agencies or Congress attempted to redefine existing programs; analysis of bills that failed because of classification ambiguity',
    'If strictly defined (novel function only): many legislative workarounds available (program expansion, consolidation, redefinition). If broadly defined (any expansion): suppression is higher, extraction is more severe. Classification could shift from Tangled Rope to Snare if definition is expansive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_scope_ambiguity, empirical, 'Definitional boundary of ''new program''').

omega_variable(
    congressional_coalition_formation,
    'Can unified congressional action overcome the veto threat through high-salience program overrides, or is the two-thirds threshold insurmountable?',
    'Tracking of override attempts; identification of coalition patterns for successful vs unsuccessful overrides; assessment of whether program salience correlates with override success',
    'If coalitions can form reliably: scaffold classification confirmed, exits exist. If threshold is consistently insurmountable: snare classification likely, suppression increases, extraction becomes more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_coalition_formation, empirical, 'Coalition capacity to override veto on priority programs').

omega_variable(
    deficit_reduction_efficacy,
    'Does the moratorium actually reduce deficit growth, or does it merely shift spending to uncovered categories (mandatory entitlements, interest on debt, defense)?',
    'Comparative analysis: FY1976 deficit vs counterfactual projection without moratorium; tracking of spending changes by program category; assessment of whether excluded categories (mandatory spending, defense) increased proportionally',
    'If effective at reducing deficit: rope classification for beneficiaries confirmed (genuine coordination function). If merely categorical shifting: tangled rope classification more accurate (appearance of control without real fiscal impact, higher theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deficit_reduction_efficacy, empirical, 'Actual fiscal impact of moratorium on deficit reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1975_ford_veto_moratorium_new_programs, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ford_veto_tr_t0, sotu_1975_ford_veto_moratorium_new_programs, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ford_veto_tr_t3, sotu_1975_ford_veto_moratorium_new_programs, theater_ratio, 3, 0.42).
narrative_ontology:measurement(ford_veto_tr_t6, sotu_1975_ford_veto_moratorium_new_programs, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(ford_veto_be_t0, sotu_1975_ford_veto_moratorium_new_programs, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ford_veto_be_t3, sotu_1975_ford_veto_moratorium_new_programs, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ford_veto_be_t6, sotu_1975_ford_veto_moratorium_new_programs, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1975_ford_veto_moratorium_new_programs, resource_allocation).
narrative_ontology:affects_constraint(sotu_1975_ford_veto_moratorium_new_programs, congressional_spending_authority_limitation).
narrative_ontology:affects_constraint(sotu_1975_ford_veto_moratorium_new_programs, executive_budgetary_impoundment_authority).

% DUAL FORMULATION NOTE:
% The veto moratorium is downstream of the broader separation-of-powers architecture (executive veto power) and upstream of specific appropriation battles (education, social services, environmental programs). The moratorium represents a strategic invocation of existing constitutional authority, not a novel constraint creation. The network links track how the veto threat propagates to specific program debates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
