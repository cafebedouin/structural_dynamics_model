% ============================================================================
% CONSTRAINT STORY: sotu_1948_truman_federal_education_aid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1948_truman_federal_education_aid, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1948_truman_federal_education_aid
 *   human_readable: Federal Government Financial Assistance to States for Elementary and Secondary Education (1948 Truman Initiative)
 *   domain: education_policy/fiscal_federalism
 *
 * SUMMARY:
 *   Truman's 1948 proposal to have the Federal Government assume fiscal
 *   responsibility for addressing educational inadequacies represents a
 *   structural pivot in American federalism: from education as purely
 *   local/state concern to education as federal responsibility. The
 *   constraint embeds a genuine coordination problem (how to provide equal
 *   educational access across states with heterogeneous tax bases and
 *   resource availability) alongside an extraction mechanism (federal control
 *   of education policy, redistribution of tax revenue from wealthy to poor
 *   states, expansion of federal bureaucratic authority). This is a canonical
 *   Tangled Rope: coordinating a national solution to resource inequality
 *   while simultaneously centralizing control and extracting authority from
 *   local institutions. The extractiveness has risen over the measurement
 *   interval (0.22 → 0.38) as federal conditionality has expanded beyond
 *   fiscal transfers to curriculum, assessment, and accountability
 *   mechanisms. The theater ratio has increased (0.35 → 0.48) as federal
 *   education programs have accumulated procedural complexity and
 *   performative compliance requirements without proportional functional
 *   gain. Different structural positions perceive this constraint radically
 *   differently: underfunded rural districts see rescue (Snare perception
 *   from their position), wealthy states see extraction, the federal
 *   bureaucracy sees institutional growth and mission expansion (Rope
 *   perception), and the local school board sees hollowing authority (Piton).
 *   The false summit risk is high: naturalizing federalism as inevitable
 *   creates a mountain frame that obscures the contingent institutional
 *   choices embedded in the coordination mechanism.
 *
 * KEY AGENTS:
 *   - Underfunded School Districts (Powerless/Trapped): Primary victims — lack local tax base and cannot achieve educational adequacy without federal transfers. Dependent on federal mechanism with no exit.
 *   - Low-Income Student Population (Moderate/Constrained): Primary beneficiaries in access terms, but constrained by dependence on federal funding for their education. Cannot exercise school choice or local control without losing resources.
 *   - Wealthy State Governments (Institutional/Constrained): Experience extraction through progressive federal taxation and redistribution mechanism. Constrained by interstate coordination requirement and federal conditionality. Also benefit from standardization enabling labor mobility.
 *   - Federal Education Bureau (Institutional/Arbitrage): Primary institutional beneficiary. Controls program design, has funding discretion, can redefine mission and expand authority. Net consolidator of power.
 *   - Local School Boards (Institutional/Arbitrage): Nominal autonomy erodes as federal funding expands. Maintain performative authority while real decision-making migrates upward. Theater institution.
 *   - Teacher Supply Coalition (Organized/Mobile): Temporary beneficiary. Federal spending creates market for teacher training and construction services. Mobile role — benefit declines as shortage is addressed.
 *   - Analytical Observer (Analytical/Analytical): Risks naturalizing contingent federalism as immutable structure, converting coordination problem into a false summit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1948_truman_federal_education_aid, 0.38).
domain_priors:suppression_score(sotu_1948_truman_federal_education_aid, 0.52).
domain_priors:theater_ratio(sotu_1948_truman_federal_education_aid, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1948_truman_federal_education_aid, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1948_truman_federal_education_aid, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sotu_1948_truman_federal_education_aid, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1948_truman_federal_education_aid, tangled_rope).
narrative_ontology:human_readable(sotu_1948_truman_federal_education_aid, "Federal Government Financial Assistance to States for Elementary and Secondary Education (1948 Truman Initiative)").
narrative_ontology:topic_domain(sotu_1948_truman_federal_education_aid, "education_policy/fiscal_federalism").

domain_priors:requires_active_enforcement(sotu_1948_truman_federal_education_aid).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1948_truman_federal_education_aid, underfunded_school_districts).
narrative_ontology:constraint_beneficiary(sotu_1948_truman_federal_education_aid, low_income_students).
narrative_ontology:constraint_beneficiary(sotu_1948_truman_federal_education_aid, rural_and_poor_states).
narrative_ontology:constraint_victim(sotu_1948_truman_federal_education_aid, high_tax_states).
narrative_ontology:constraint_victim(sotu_1948_truman_federal_education_aid, local_education_autonomy).
narrative_ontology:constraint_victim(sotu_1948_truman_federal_education_aid, state_fiscal_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERFUNDED RURAL SCHOOL DISTRICT (SNARE) — Trapped by lack of local tax base and resource scarcity. Cannot build schools or attract teachers without federal aid. Dependent on federal largesse with no credible alternative. Experiences the constraint as pure extraction: must accept federal conditions and accountability mechanisms to access necessary funds.
constraint_indexing:constraint_classification(sotu_1948_truman_federal_education_aid, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOW-INCOME STUDENT POPULATION (TANGLED ROPE) — Experiences genuine coordination benefit (access to schools and teachers becomes possible) alongside asymmetric extraction (federal funding comes with conditions, standardization, and loss of local control over curriculum and pedagogy). Constrained by geography and family resources; can access education only through federal framework.
constraint_indexing:constraint_classification(sotu_1948_truman_federal_education_aid, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEDERAL EDUCATION BUREAU (ROPE) — Experiences the constraint as pure coordination: mobilizing distributed resources (federal tax revenue) to solve collective action problem (school construction and teacher supply) that no single state can solve alone. Has arbitrage options (can redirect funds, adjust programs, sunset initiatives). Net beneficiary in terms of institutional power consolidation and mission expansion.
constraint_indexing:constraint_classification(sotu_1948_truman_federal_education_aid, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WEALTHY STATE GOVERNMENT (TANGLED ROPE) — Constrained by the federal tax redistribution mechanism and the interstate coordination requirement. Experiences extraction (higher federal tax burden to fund other states' schools) alongside genuine coordination benefit (access to federal infrastructure expertise, standardization that enables interstate labor mobility, participation in expanding national education system). Cannot exit without isolation.
constraint_indexing:constraint_classification(sotu_1948_truman_federal_education_aid, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: SCHOOL CONSTRUCTION INDUSTRY / TEACHER TRAINING COALITION (SCAFFOLD) — Organized private actors benefit from federal spending stimulus for building and teacher preparation programs. Experience temporary coordination mechanism with sunset logic: federal aid mobilizes private-sector capacity to address acute shortage. As shortages are met and permanent funding mechanisms are built (state bonding, permanent appropriations), the federal stimulus role transitions to baseline support. Sunset estimated at 15-20 years as capacity is built.
constraint_indexing:constraint_classification(sotu_1948_truman_federal_education_aid, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: LOCAL SCHOOL BOARD AUTHORITY (PITON) — Nominally retains curriculum and pedagogical authority, but federal funding conditionality increasingly structures local decisions. Theater ratio reflects the performative autonomy: local boards make decisions within federal parameters, maintaining appearance of local control while actual authority migrates upward. Institutional inertia keeps local board structures despite reduced functional authority.
constraint_indexing:constraint_classification(sotu_1948_truman_federal_education_aid, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, educational inequality is an immutable consequence of heterogeneous property tax bases and labor supply constraints across geography. This view naturalizes federalism as unchangeable structural reality. However, the structural data reveals beneficiaries (underfunded districts, federal education bureaucracy) — false summit detection will reclassify this as a contingent institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(sotu_1948_truman_federal_education_aid, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1948_truman_federal_education_aid_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1948_truman_federal_education_aid, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1948_truman_federal_education_aid, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1948_truman_federal_education_aid, TR),
    TR >= 0.70.

:- end_tests(sotu_1948_truman_federal_education_aid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The federal aid mechanism does genuinely solve a coordination problem (no single poor state can fund adequate schools alone; aggregate federal taxation can transfer resources efficiently). However, the extraction component is significant: federal authority over local education, centralized control of curriculum and assessment, and political patronage in fund allocation. The value reflects the hybrid nature — meaningful coordination benefit alongside meaningful extraction cost. The trajectory rising from 0.22 to 0.38 over the interval reflects creeping expansion of federal conditionality beyond initial fiscal transfer to behavioral and curriculum mandates. Suppression (0.52): Moderate-high. Significant barriers to exit for dependent states and districts (no alternative funding source), but not total suppression — local boards retain nominal authority and some capacity to seek alternative funding or reduce spending. Wealthy states face tax burden constraint but can still pursue independent education policy. The suppression reflects fiscal dependence more than coercive mechanisms. Theater ratio (0.48): Moderate. Federal program administration includes genuine coordination functions (distributing funds, coordinating standards, training teachers) but increasingly theater-laden (performance metrics that may not reflect learning, compliance procedures that exceed functional necessity, federal oversight that maintains appearance of local control while centralizing decisions). The rise from 0.35 to 0.48 indicates theater growth outpacing function — a diagnostic of potential Piton drift.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional arrangement produces contradictory classifications depending on observer position. The underfunded district sees a rescue mechanism (Snare from their trapped position — high extraction but necessary for survival). The federal education bureau sees a coordination success (Rope — distributing resources efficiently solves collective action problem). The wealthy state sees mixed coordination and extraction (Tangled Rope — participates in national system but bears progressive tax burden). The local school board sees degraded autonomy (Piton — maintains formal authority while real authority migrates federally). The open science coalition sees temporary stimulus (Scaffold — federal spending addresses acute teacher shortage, with sunset as permanent supply mechanisms mature). The civilizational observer risks seeing immutable structure (Mountain — educational inequality inherent to geography) rather than contingent institution (Tangled Rope — federalism is chosen, not inevitable). The perspectival gap reveals that 'Is federal education aid good policy?' is not a factual question but a structural question: good for whom, from which time horizon, measured against which alternatives?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives because structural positions differ fundamentally. Underfunded districts are pure victims with no exit (d ≈ 0.95): powerless agents trapped by geography and demography, experiencing full extraction weight. Wealthy states are partial beneficiaries with constrained exit (d ≈ 0.55): they pay through progressive taxation but benefit from national standardization and interstate coordination. The Federal Education Bureau is a beneficiary with arbitrage options (d ≈ 0.15): can design programs, redirect funds, adjust scope. Local school boards are nominal beneficiaries (authorization to spend federal money) but structurally captured (d ≈ 0.65): their nominal authority masks real decision-making constraint. The sigmoid function f(d) maps these positions to effective extractiveness chi, producing the perspectival gap: underfunded districts perceive high chi (snare), federal bureaus perceive low/negative chi (rope), wealthy states perceive moderate chi (tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION MECHANISM: The constraint resolves mandatrophy by clarifying that federal education aid is structurally a hybrid (Tangled Rope), not a pure coordination mechanism (Rope) nor pure extraction (Snare). The mislabeling risk is high: proponents frame it as pure coordination ('solving the school building shortage'), opponents frame it as pure extraction ('federal takeover of education'). The engine's classification correctly identifies it as both — genuine coordination function (solving the collective action problem of underfunded schools) AND asymmetric extraction (federal authority consolidation, redistribution of fiscal autonomy, behavioral conditionality). The Tangled Rope classification prevents the rhetorical collapse into either pole. The mandatrophy-resolved state depends on which question is being asked: (1) Does the mechanism solve the education inadequacy problem? YES — coordination function succeeds. (2) Does the mechanism reduce local autonomy and extract authority? YES — extraction function succeeds. Both are true simultaneously. The constraint does not resolve to a single type because it is not a single structural mechanism — it is a hybrid that maintains its hybrid character across the measurement interval. The rising theater ratio indicates Piton drift (performative elements accumulating), which could eventually degrade the coordination function, shifting the constraint toward pure Snare. This is a transition risk, not a current condition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_vs_state_capacity,
    'Does federal funding actually expand total educational capacity, or does it primarily redistribute existing capacity from wealthy to poor states?',
    'Time-series analysis of total school construction, teacher supply, and per-pupil spending before and after federal aid implementation; comparison of growth rates in aided vs. non-aided districts controlling for initial resource levels',
    'If genuine expansion: constraint is cooperative Rope with redistribution component. If primarily redistributive: constraint is extractive Tangled Rope with larger suppression component against wealthy states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_vs_state_capacity, empirical, 'Whether federal funding expands total capacity or redistributes existing capacity').

omega_variable(
    conditionality_extraction_mechanism,
    'What proportion of federal education aid''s constraints flow from genuine coordination requirements (shared standards, accountability) versus patronage and political extraction (federal control of local decisions, pork barrel distribution)?',
    'Content analysis of federal education legislation and regulations; comparison of coordination requirements to counterfactual minimal requirements for fund transfer; tracking of discretionary federal funding allocation to states by political alignment and Congressional district concentration',
    'High coordination ratio: Tangled Rope with modest suppression. High patronage ratio: Snare or extractive Tangled Rope with elevated suppression and asymmetric benefit to federal apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_extraction_mechanism, empirical, 'Proportion of constraints from genuine coordination vs. political extraction').

omega_variable(
    state_autonomy_erosion_timeline,
    'Over what time horizon does federal conditionality cause measurable erosion of state and local curriculum autonomy, teaching methodology diversity, and educational experimentation?',
    'Longitudinal comparison of curriculum variance across states pre- and post-federal aid; tracking of pedagogical innovation rates by state funding source; interviews with state education officials on decision-making constraint perception',
    'If erosion < 10 years: suppression and extraction metrics should be higher. If erosion > 30 years: constraint may be genuinely reversible, lowering suppression score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_autonomy_erosion_timeline, empirical, 'Timeline of state autonomy erosion due to federal funding conditions').

omega_variable(
    teacher_labor_market_integration,
    'Does federal teacher aid create a single national labor market (reducing local autonomy but improving teacher mobility) or primarily subsidize regional differences in teacher compensation?',
    'Analysis of teacher migration patterns, wage convergence across states, and pedagogy standardization post-federal aid; comparison to pre-aid regional variation',
    'If national integration: genuine coordination function increases, Rope component strengthens. If regional subsidization: extraction mechanism clarifies, Snare or Tangled Rope component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_labor_market_integration, empirical, 'Whether federal teacher aid creates national labor market or regional subsidization').

omega_variable(
    false_summit_natural_law,
    'Is educational inequality presented as a natural consequence of geography and demography, or as a contingent political outcome of state-level tax financing that federal intervention could structurally alter?',
    'Comparative education policy analysis: do other nations with federal systems handle education financing differently? What would alternative models (full federal financing, regional compacts, equalization mechanisms without federal control) look like structurally?',
    'If natural: mountain classification persists as defensible. If contingent: false summit classification confirmed; constraint becomes Tangled Rope or Snare depending on extraction mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether educational inequality is presented as natural or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1948_truman_federal_education_aid, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_ed_aid_tr_t0, sotu_1948_truman_federal_education_aid, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fed_ed_aid_tr_t5, sotu_1948_truman_federal_education_aid, theater_ratio, 5, 0.42).
narrative_ontology:measurement(fed_ed_aid_tr_t10, sotu_1948_truman_federal_education_aid, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(fed_ed_aid_be_t0, sotu_1948_truman_federal_education_aid, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fed_ed_aid_be_t5, sotu_1948_truman_federal_education_aid, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(fed_ed_aid_be_t10, sotu_1948_truman_federal_education_aid, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1948_truman_federal_education_aid, resource_allocation).
narrative_ontology:affects_constraint(sotu_1948_truman_federal_education_aid, state_fiscal_autonomy).
narrative_ontology:affects_constraint(sotu_1948_truman_federal_education_aid, curriculum_standardization).
narrative_ontology:affects_constraint(sotu_1948_truman_federal_education_aid, teacher_labor_market_integration).
narrative_ontology:affects_constraint(sotu_1948_truman_federal_education_aid, federal_education_bureaucracy_authority).

% DUAL FORMULATION NOTE:
% Federal education aid as coordinating mechanism (resource redistribution solving underfunding) is structurally distinct from federal education aid as extraction mechanism (federal authority consolidation over local education). The constraint story treats these as aspects of a single Tangled Rope. Alternative decomposition: write separate stories for the fiscal coordination function (Rope, ε≈0.12) and the authority consolidation function (Snare, ε≈0.58). The current unified story is appropriate if the constraint's primary structural identity is the hybrid mechanism; decomposition is appropriate if research focuses on isolating the pure coordination or extraction components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1948_truman_federal_education_aid, institutional, 0.15).
constraint_indexing:directionality_override(sotu_1948_truman_federal_education_aid, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
