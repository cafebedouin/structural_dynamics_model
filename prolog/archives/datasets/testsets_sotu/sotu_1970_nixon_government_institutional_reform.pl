% ============================================================================
% CONSTRAINT STORY: sotu_1970_nixon_government_institutional_reform
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1970_nixon_government_institutional_reform, []).

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
 *   constraint_id: sotu_1970_nixon_government_institutional_reform
 *   human_readable: Nixon's Government Institutional Reform Agenda (1970)
 *   domain: governance/institutional_reform
 *
 * SUMMARY:
 *   President Nixon's 1970 institutional reform agenda represents a
 *   structural reframing of governance itself as a policy problem. Rather
 *   than proposing new programs or expanded capacity, the diagnosis
 *   identifies the government's institutional architecture as the source of
 *   ineffectiveness: 'Despite economic growth, despite continued expansion of
 *   government programs, we have not solved the problems these programs were
 *   designed to solve.' The reform agenda proposes comprehensive
 *   restructuring to improve efficiency and responsiveness — a tangled rope
 *   constraint because it combines genuine coordination functions (aligning
 *   government structure to current needs) with asymmetric extraction (costs
 *   borne by career bureaucrats and existing program constituencies; benefits
 *   captured by executive authority and future administrations). The
 *   mechanism creates a temporal asymmetry: immediate costs are concentrated
 *   and observable (job loss, program disruption), while efficiency gains are
 *   distributed and deferred (only realizable if implementation succeeds).
 *   Theater ratio rises over time as implementation reveals gap between
 *   ambitious reform vision and bureaucratic resistance, generating
 *   performative reorganizations that preserve old structures under new
 *   names.
 *
 * KEY AGENTS:
 *   - Career Civil Service: Primary victim (powerless/trapped) — identity fused with existing bureaucratic positions; pension and career security depend on preserving current structure
 *   - Existing Program Constituencies: Secondary victim (moderate/constrained) — dependent on current program arrangements; face disruption and uncertainty during restructuring; some potential for long-term benefit if reforms improve program effectiveness
 *   - Executive Office / Presidential Authority: Primary beneficiary (institutional/arbitrage) — centralized control over government apparatus; capacity to implement policy without navigating existing bureaucratic procedures; benefits from executive reorganization authority
 *   - Reform Coalition: Organized supporters (organized/constrained) — Congressional reformers and civic organizations advocating modernization; see reform as temporary problem-solving rather than permanent extraction
 *   - Congressional Committee System: Institutional actor (institutional/arbitrage) — maintains seniority-based jurisdictional authority; resistant to centralization that would diminish committee power; sees reform as threat to institutional position
 *   - Future Citizens: Abstract beneficiary (powerless/analytical) — ultimate beneficiaries of improved institutional effectiveness; cannot organize during reform period; bear no immediate costs but accrue benefits over generational timeframe
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1970_nixon_government_institutional_reform, 0.58).
domain_priors:suppression_score(sotu_1970_nixon_government_institutional_reform, 0.62).
domain_priors:theater_ratio(sotu_1970_nixon_government_institutional_reform, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1970_nixon_government_institutional_reform, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1970_nixon_government_institutional_reform, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sotu_1970_nixon_government_institutional_reform, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1970_nixon_government_institutional_reform, tangled_rope).
narrative_ontology:human_readable(sotu_1970_nixon_government_institutional_reform, "Nixon's Government Institutional Reform Agenda (1970)").
narrative_ontology:topic_domain(sotu_1970_nixon_government_institutional_reform, "governance/institutional_reform").

domain_priors:requires_active_enforcement(sotu_1970_nixon_government_institutional_reform).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1970_nixon_government_institutional_reform, future_citizens).
narrative_ontology:constraint_beneficiary(sotu_1970_nixon_government_institutional_reform, efficiency_gain_constituencies).
narrative_ontology:constraint_victim(sotu_1970_nixon_government_institutional_reform, existing_bureaucratic_arrangements).
narrative_ontology:constraint_victim(sotu_1970_nixon_government_institutional_reform, entrenched_program_constituencies).
narrative_ontology:constraint_victim(sotu_1970_nixon_government_institutional_reform, career_civil_service).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAREER CIVIL SERVICE / PROGRAM INCUMBENTS (SNARE) — Trapped within the existing bureaucratic structure with career paths, pension dependencies, and organizational identities fused to current arrangements. Reform threatens job security, promotional pathways, and institutional status. No exit option without career destruction. Maximum extraction — these agents bear full cost of restructuring with minimal voice in design.
constraint_indexing:constraint_classification(sotu_1970_nixon_government_institutional_reform, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CURRENT PROGRAM BENEFICIARIES (TANGLED ROPE) — Constrained by dependence on specific program arrangements (welfare, housing, employment programs) but also potential beneficiaries of improved institutional efficiency. Extraction is real and immediate (disruption of familiar programs, uncertainty about reformed alternatives) but mixed with genuine coordination benefit (long-term institutional effectiveness).
constraint_indexing:constraint_classification(sotu_1970_nixon_government_institutional_reform, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE OFFICE / PRESIDENTIAL AUTHORITY (ROPE) — Experiences reform agenda as coordination mechanism: reorganizing government apparatus enables more effective policy implementation and executive control. Benefits from centralized authority and streamlined decision-making. Low experienced extraction — this actor is net beneficiary of the reform logic.
constraint_indexing:constraint_classification(sotu_1970_nixon_government_institutional_reform, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION / ORGANIZED AGENTS (SCAFFOLD) — Congressional reformers and civic organizations advocating institutional modernization see reform as temporary reorganization with generational payoff (more effective government, restored citizen trust). Constrained by political resistance from entrenched interests but organized enough to advocate alternatives. Experience the constraint as solvable — with sunset logic: improved institutional arrangements should gradually replace the old structure.
constraint_indexing:constraint_classification(sotu_1970_nixon_government_institutional_reform, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESSIONAL COMMITTEE SYSTEM (PITON) — The existing committee structure persists largely through institutional inertia despite acknowledged inefficiency. Committees maintain jurisdictional boundaries and seniority-based authority that reflect 1960s needs rather than current problems. Reform diagnosis acknowledges the system's degradation — it is no longer functional — but institutional resistance (members' career investment in committee positions) maintains the performative machinery. Theater ratio high because much committee activity is procedurally mandated rather than substantively necessary.
constraint_indexing:constraint_classification(sotu_1970_nixon_government_institutional_reform, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some gap between public expectations and government capacity may be inherent to large-scale democratic coordination: no institutional arrangement perfectly aligns bureaucratic efficiency with democratic responsiveness, and all systems eventually accumulate procedures that outlive their justification. However, this perspective risks naturalizing what are actually contingent 1960s-specific institutional choices as immutable features of governance.
constraint_indexing:constraint_classification(sotu_1970_nixon_government_institutional_reform, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1970_nixon_government_institutional_reform_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1970_nixon_government_institutional_reform, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1970_nixon_government_institutional_reform, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1970_nixon_government_institutional_reform, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1970_nixon_government_institutional_reform, TR),
    TR >= 0.70.

:- end_tests(sotu_1970_nixon_government_institutional_reform_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, increasing over time. The reform agenda combines genuine coordination problem-solving (aligning government structure to current conditions) with meaningful extraction (restructuring costs concentrated on career civil service and existing program constituencies). Initial value (0.35) reflects that reform is presented as efficiency improvement without explicit extraction. Rising to 0.62 reflects that as implementation proceeds, the extractive consequences become visible: actual job loss, program disruption, and transfer of authority from career bureaucrats to executive appointments. Suppression (0.62): High. Career civil servants have substantial barriers to exit (age, specialized skills, pension dependencies, identity fusion with organizational roles). Existing program constituencies cannot easily shift to alternative support structures. Political resistance from entrenched interests suppresses information about alternatives. Theater ratio (0.68, rising to 0.72): Moderately high, increasing. Reform rhetoric emphasizes efficiency and responsiveness, but implementation reveals performative elements: reorganizations that shuffle jurisdictions without changing underlying procedures, new agencies that replicate functions of dissolved predecessors, and elaborate restructuring that accommodates political resistance rather than eliminating redundant structures.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Career civil service sees an existential threat (Snare) — the reform is extractive and they cannot escape. Executive office sees coordination improvement (Rope) — they benefit from streamlined authority. Program beneficiaries see mixed extraction and potential improvement (Tangled Rope) — immediate costs but possible long-term gains. Reform advocates see a solvable temporary problem (Scaffold) — with sunset logic that improved structures will gradually replace old arrangements. Congressional system sees its own degradation (Piton) — acknowledging that current procedures are inefficient but preserving them through institutional resistance. The analytical observer risks seeing an immutable feature of democratic government (Mountain) — that some gap between efficiency and responsiveness is inherent — but the structural data reveals this as naturalization of a specific institutional moment. The core perspectival gap emerges between agents with immediate costs (career civil service, current program constituencies) and agents with deferred benefits (executive authority, future citizens, efficiency-seeking constituencies) — a temporal asymmetry that defines the tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural relationship of each agent to the extraction flow. Career civil service (trapped, powerless) experiences maximum extractiveness — they face career costs with no exit option. Executive office (arbitrage, institutional) experiences minimal or negative extractiveness — they are beneficiaries of the reform mechanism, with maximum flexibility to adapt to or shape outcomes. Program beneficiaries (constrained, moderate) experience moderate extractiveness — they face disruption and uncertainty but retain some ability to organize politically and some potential for long-term benefit. The piton perspective derives from observing that the congressional committee system, while presenting itself as essential to governance, is largely performative — seniority-based authority and jurisdictional fragmentation persist despite acknowledged inefficiency, maintained by members' career investment in current structure. The mountain perspective risks naturalizing a 1970s-specific institutional arrangement as an inherent feature of democracy, when historical analysis would reveal that both the problem diagnosis and the proposed solutions are contingent on particular political pressures and administrative theories of the moment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the classification depends fundamentally on temporal perspective. From an immediate timeframe, the constraint is Snare for career civil service (pure extraction with maximum suppression). From an executive perspective, it is Rope (coordination mechanism). From an organized reform coalition perspective, it is Scaffold (temporary restructuring with sunset and generational payoff). From the committee system perspective, it is Piton (performative maintenance of inefficient structure). The mandatrophy is resolved by recognizing that all six types are simultaneously true from their respective observational positions. The extractiveness value (0.58) is stable across these perspectives because it measures the constraint's structural property (magnitude of asymmetric redistribution), not the experienced intensity (chi), which varies by directionality. The constraint does not resolve to a single type; rather, it demonstrates that indexical classification correctly captures the divergent structural realities these agents inhabit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_vs_responsiveness_tradeoff,
    'Does institutional consolidation and centralization increase efficiency at the cost of democratic responsiveness and accountability?',
    'Post-reform measurement of citizen satisfaction, responsiveness to constituent concerns, and documented policy feedback loops; comparison with decentralized governance models; analysis of whether centralization enables or constrains policy adaptation',
    'If true tradeoff: reform becomes Snare (efficiency extraction costs democratic voice). If false: reform achieves genuine coordination gain without extraction. If neutral: reform is purely redistributive (some gain efficiency, others lose responsiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_vs_responsiveness_tradeoff, empirical, 'Whether efficiency gains come at cost of responsiveness').

omega_variable(
    bureaucratic_resistance_structural,
    'Is resistance from career civil service structurally necessary (protection of expertise and institutional memory) or purely self-interested (preservation of privilege)?',
    'Historical analysis of similar reform efforts; documentation of whether restructured agencies retained critical expertise; measurement of performance outcomes for reorganized vs preserved units; study of whether early-career entry into restructured agencies attracted or repelled talented administrators',
    'If necessary: suppression value should be higher (institutional resistance protects genuine coordination function). If self-interested: suppression is extractive barrier; reform should face less resistance. Affects whether civil service perspective should be Snare (trapped) or Tangled Rope (mixed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bureaucratic_resistance_structural, empirical, 'Whether bureaucratic resistance is structural necessity or self-interest').

omega_variable(
    reform_implementation_fidelity,
    'Will proposed institutional reforms be implemented as designed, or will implementation drift produce a new layer of procedures atop the old structure (the infamous ''comprehensive reform that changes nothing'')?',
    'Compare formal reform proposals with actual reorganization over 5-year and 10-year windows; measure whether old organizational units were truly eliminated or merely renamed; track whether new decision-making structures become as procedurally complex as predecessors',
    'If high fidelity: reform function as Scaffold (temporary structural change). If low fidelity: reform becomes Piton (theatrical restructuring that preserves underlying arrangements); beneficiaries of old system co-opt the reform machinery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_implementation_fidelity, empirical, 'Whether reform will be implemented or co-opted into performative restructuring').

omega_variable(
    distributional_consequences_visibility,
    'Are the distributional consequences of reform (who bears restructuring costs, who gains efficiency benefits) transparent to affected populations or obscured by bureaucratic complexity?',
    'Public communication analysis: compare clarity of reform messaging to understanding measured in citizen surveys; track whether impacted communities (welfare recipients, federal employees, regional centers) could identify and organize against disproportionate costs',
    'If transparent: Tangled Rope holds (victims can organize, suppress the constraint). If obscured: constraint shifts toward Snare (victims trapped within opaque restructuring). Affects whether suppression persists or erodes as information spreads.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_consequences_visibility, empirical, 'Visibility of distributional consequences to affected populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1970_nixon_government_institutional_reform, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu70_tr_t0, sotu_1970_nixon_government_institutional_reform, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu70_tr_t2, sotu_1970_nixon_government_institutional_reform, theater_ratio, 2, 0.55).
narrative_ontology:measurement(sotu70_tr_t5, sotu_1970_nixon_government_institutional_reform, theater_ratio, 5, 0.68).
narrative_ontology:measurement(sotu70_tr_t10, sotu_1970_nixon_government_institutional_reform, theater_ratio, 10, 0.72).

% Extraction over time
narrative_ontology:measurement(sotu70_be_t0, sotu_1970_nixon_government_institutional_reform, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu70_be_t2, sotu_1970_nixon_government_institutional_reform, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(sotu70_be_t5, sotu_1970_nixon_government_institutional_reform, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(sotu70_be_t10, sotu_1970_nixon_government_institutional_reform, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1970_nixon_government_institutional_reform, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1970_nixon_government_institutional_reform, federal_agency_proliferation).
narrative_ontology:affects_constraint(sotu_1970_nixon_government_institutional_reform, civil_service_protection_doctrine).
narrative_ontology:affects_constraint(sotu_1970_nixon_government_institutional_reform, executive_authority_expansion).

% DUAL FORMULATION NOTE:
% This constraint is upstream of multiple specific institutional changes (EPA establishment, OSHA creation, cabinet-level restructuring) and downstream of broader political movements (Great Society program proliferation, administrative state growth, public skepticism toward government). Federal agency proliferation and executive authority expansion are direct outputs of this reform agenda's implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
