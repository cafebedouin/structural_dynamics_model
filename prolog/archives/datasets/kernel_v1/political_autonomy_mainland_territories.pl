% ============================================================================
% CONSTRAINT STORY: political_autonomy_mainland_territories
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_autonomy_mainland_territories, []).

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
 *   constraint_id: political_autonomy_mainland_territories
 *   human_readable: Political Autonomy Constraints in Mainland Territories
 *   domain: political_economy/territorial_governance
 *
 * SUMMARY:
 *   Political autonomy constraints in mainland territories structure the
 *   relationship between central state authority and geographically dispersed
 *   populations through formal hierarchy backed by administrative control,
 *   legislative override, budgetary subordination, military/security command,
 *   and cultural policy regulation. The constraint appears simultaneously as
 *   coordination (central state solves the problem of governing dispersed
 *   populations and maintaining territorial integrity), as extraction
 *   (territorial populations bear the cost of subordinated political
 *   authority), and as degraded performance (formal autonomy provisions are
 *   increasingly performative as central authority bypasses local
 *   governance). The measurement trajectory shows rising suppression (0.52 →
 *   0.68), rising extractiveness (0.42 → 0.58), and rising theater (0.48 →
 *   0.65), indicating that the constraint is evolving from mixed
 *   coordination-extraction toward dominance by coercive enforcement and
 *   performative governance rituals. This pattern is consistent with a
 *   constraint in the tangled_rope/snare boundary zone experiencing pressure
 *   toward pure snare through intensified enforcement and erosion of autonomy
 *   provisions.
 *
 * KEY AGENTS:
 *   - Central State Authority: Primary beneficiary (institutional/arbitrage) — controls budgetary allocation, legislative authority, executive appointments, military/security command, and cultural/educational policy. Experiences constraint as coordination mechanism for maintaining territorial control.
 *   - Territorial Populations: Primary victim (powerless/trapped) — subject to centrally-determined policy with limited exit options (geographic binding, asset immobility, social rootedness). Trapped within administrative boundaries with no legitimate secession or exit mechanism.
 *   - Local Political Elite: Secondary actor (moderate/constrained) — intermediary between central authority and territorial populations; benefits from delegated administrative authority and resource patronage but constrained by legislative override, executive removal, budget control. Captured by central institutions through career dependence.
 *   - Pro-Autonomy Coalition: Organized victims (organized/mobile) — political movements, civil society, diaspora networks advocating territorial self-determination. Possess exit options through international engagement and internal organizing capacity.
 *   - Administrative/Security Apparatus: Enforcement institution (institutional/arbitrage) — bureaucratic structures, military/security forces implementing central policy, monitoring compliance. Maintains hierarchy through coercive force and administrative procedure.
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing political hierarchy as inherent to large-scale governance rather than recognizing it as contingent institutional arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_autonomy_mainland_territories, 0.58).
domain_priors:suppression_score(political_autonomy_mainland_territories, 0.68).
domain_priors:theater_ratio(political_autonomy_mainland_territories, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_autonomy_mainland_territories, extractiveness, 0.58).
narrative_ontology:constraint_metric(political_autonomy_mainland_territories, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(political_autonomy_mainland_territories, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_autonomy_mainland_territories, tangled_rope).
narrative_ontology:human_readable(political_autonomy_mainland_territories, "Political Autonomy Constraints in Mainland Territories").
narrative_ontology:topic_domain(political_autonomy_mainland_territories, "political_economy/territorial_governance").

domain_priors:requires_active_enforcement(political_autonomy_mainland_territories).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_autonomy_mainland_territories, central_state_authority).
narrative_ontology:constraint_victim(political_autonomy_mainland_territories, territorial_populations).
narrative_ontology:constraint_victim(political_autonomy_mainland_territories, local_governance_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TERRITORIAL POPULATION (SNARE) — Trapped within administrative boundaries with no legitimate exit mechanism. Faces extraction through budgetary subordination, legislative override, and coercive enforcement of central policy. Cannot exit the territory without abandoning material assets, kinship networks, and cultural rootedness. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL POLITICAL ELITE (TANGLED ROPE) — Constrained by central override power (legislative veto, executive removal, budget control) but also benefits from administrative authority delegated by the center and resource flows channeled through central institutions. Can organize constituencies and capture local resources, but ultimate authority is contingent on central approval. Genuine coordination function (regional policy coordination) exists alongside asymmetric extraction (central authority extracts loyalty and compliance).
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRAL STATE AUTHORITY (ROPE) — Experiences the constraint as coordination: maintaining territorial cohesion, standardizing policy implementation, and collecting resources requires establishing hierarchical channels to local actors. The constraint solves a genuine collective action problem (how to govern dispersed populations). Net beneficiary — extraction flow runs toward this agent. Has highest degree of freedom in interpreting and modifying constraint terms.
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRO-AUTONOMY COALITION (TANGLED ROPE) — Organized agents (political movements, civil society organizations, diaspora networks) see the autonomy constraint as simultaneously providing coordination benefits (rule of law, infrastructure standards, market integration with center) and extracting political authority (decisions made in capital, not territory). This coalition has exit options through international engagement (UN, regional bodies, foreign support) and internal organization capacity. Sees a coordinated governance structure layered with extractive authority delegation.
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COLONIAL ADMINISTRATIVE APPARATUS (PITON) — The formal hierarchy itself (bureaucratic structures, legal codification, administrative procedures) has become largely performative. Many autonomy provisions are formally codified but practically overridden; local governance capacity exists on paper while actual decisions flow through central channels. The apparatus persists through institutional inertia and international legitimacy (appears as 'devolution' or 'federalism' in formal texts) rather than functional governance. Theater ratio high: elaborate administrative structures mask central control.
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, political hierarchy is sometimes framed as an immutable feature of large-scale governance: controlling dispersed populations requires vertical authority structures; 'true autonomy' is impossible at scale; territorial subordination is inherent to the state system. However, this perspective risks naturalizing what is structurally contingent — the engine's false summit detector will identify whether this naturalization serves the interests of the central authority.
constraint_indexing:constraint_classification(political_autonomy_mainland_territories, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_autonomy_mainland_territories_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_autonomy_mainland_territories, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_autonomy_mainland_territories, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_autonomy_mainland_territories, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_autonomy_mainland_territories, TR),
    TR >= 0.70.

:- end_tests(political_autonomy_mainland_territories_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The central state extracts political authority, budgetary control, and policy determination rights from territorial populations. The extraction is not total (some local governance capacity delegated, some resource flows directed locally) but substantial and non-negotiable — territorial populations cannot refuse subordination without facing coercion or exit. The measurement trajectory (0.42 → 0.58) indicates acceleration of extraction as suppression intensifies. Suppression (0.68): High. Multiple barriers prevent territorial exit: administrative prohibition of secession (legal barriers), military deployment (coercive barriers), economic integration dependency (cost barriers), and cultural/linguistic suppression (identity barriers). The rise from 0.52 to 0.68 indicates strengthening of enforcement mechanisms — increased military presence, tighter administrative control, expanded surveillance capacity. Theater ratio (0.65): Moderate-high. Formal autonomy provisions (written constitutions, devolution arrangements, local governance structures) are increasingly performative. Actual decisions flow through central channels while local institutions provide legitimacy cover. The rise from 0.48 to 0.65 reflects the growing gap between formal autonomy and actual central authority — elaborate administrative structures create appearance of devolved governance while substantive authority remains centralized.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between central authority and territorial populations is maximal. Central authority sees coordination (rope): establishing hierarchical channels solves genuine collective action problems (territorial cohesion, policy standardization, resource collection). The constraint enables central governance — without it, no large-scale state exists. Territorial populations see extraction (snare): they bear costs of subordinated political authority without corresponding benefits. Local elite see mixed coordination and extraction (tangled rope): they benefit from administrative authority and resource patronage delegated by center, but constrained by override power. Pro-autonomy coalition see time-bound extraction with exit options (tangled rope moving toward scaffold): they are building alternative institutional pathways (international legal frameworks, regional integration, diaspora networks) that could reduce central authority's enforcement capacity. The piton perspective reveals performative governance: formal autonomy structures exist but are bypassed by central authority, persisting through institutional inertia and international legitimacy rather than functional governance. The mountain perspective risks naturalizing what is structurally contingent: treating political hierarchy as inevitable obscures the constructed nature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from structural relationships. Central state authority (institutional/arbitrage) derives d ≈ 0.05-0.15 (full beneficiary with exit options to renegotiate terms) — experiences low effective extraction. Territorial populations (powerless/trapped) derive d ≈ 0.90-0.98 (full victim with no exit) — experience maximum extraction. Local political elite (moderate/constrained) derive d ≈ 0.60-0.75 (victim status constrained by benefit from patronage) — experience moderate-to-high extraction. Pro-autonomy coalition (organized/mobile) derive d ≈ 0.65-0.80 (organized victims with exit options through international mobilization) — experience moderate extraction. The engine scales effective extraction χ by spatial scope (national σ = 1.0, global σ = 1.2) and power-modulated directionality function f(d), producing visible perspectival gaps: institutional beneficiary sees rope (low χ), powerless victim sees snare (high χ), organized agents see tangled rope (moderate χ with exit options).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the classification depends critically on the observer's structural position within the constraint. From the central state's perspective (institutional/arbitrage), the constraint is purely coordinative (rope) — it solves the problem of governing dispersed populations. From the territorial population's perspective (powerless/trapped), the constraint is pure extraction (snare) — no coordination benefit, full cost of subordination. From the local elite's perspective (moderate/constrained with delegated authority), the constraint is mixed coordination-extraction (tangled rope) — some benefit from administrative authority, significant cost from override power. The mandatrophy is not 'which type is correct?' but 'which observation position are you measuring from?' The rising suppression trajectory (0.52 → 0.68) indicates the constraint is shifting toward snare dominance — the coordination function is degrading as enforcement mechanisms intensify. If this trajectory continues, the local elite's tangled rope classification will shift toward snare as their delegated authority erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_definition_boundary,
    'What constitutes genuine autonomy versus performative devolution? Where is the threshold between coordination and extraction in formal autonomy arrangements?',
    'Comparative analysis of actual decision-making authority: budget allocation decisions, legislative override frequency, appointment veto usage, military/security command authority. Historical tracking of when central authority overrides local decisions and cost to central authority of accepting local decisions.',
    'If threshold is low (minor decisions delegated): constraint classifies as snare from more perspectives. If threshold is high (substantial authority retained locally): constraint moves toward rope/scaffold from local elite perspective. The location of this boundary determines whether the autonomy provision is genuine coordination or facade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_definition_boundary, conceptual, 'The boundary between genuine autonomy and performative devolution').

omega_variable(
    coercion_necessity_empirical,
    'Is the level of suppression (military deployment, security apparatus, coercive enforcement) structurally necessary to maintain the autonomy arrangement, or does it reflect extractive intent beyond coordination needs?',
    'Comparative case analysis: territories with lower suppression levels and comparable autonomy arrangements; measurement of suppression intensity against resistance frequency; analysis of whether suppression precedes or follows autonomy claims.',
    'If suppression is necessary: constraint moves toward tangled_rope/scaffold (high-coordination, moderate-extraction). If suppression exceeds necessity: constraint remains snare (extraction-dominated). The empirical relationship between suppression level and territorial compliance reveals whether coercion is coordination cost or pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_necessity_empirical, empirical, 'Whether suppression is necessary for coordination or reflects extractive intent').

omega_variable(
    institutional_capture_of_local_elite,
    'To what degree are local political elites captured by central institutions (career dependence, resource patronage) versus capable of independent representation of territorial interests?',
    'Analysis of elite career trajectories, promotion pathways, resource allocation to locally-aligned vs centrally-aligned officials; measurement of legislative voting alignment with central preferences versus constituent interests; investigation of patronage dependency chains.',
    'If capture is high: local elite cannot function as genuine intermediaries; constraint appears as snare from territorial population perspective. If capture is moderate: local elite retains some bargaining power; tangled_rope classification holds. High capture strengthens false-summit mountain diagnosis (the apparent coordination structure dissolves into pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_local_elite, empirical, 'Degree of institutional capture of local political elite').

omega_variable(
    cultural_suppression_mechanism,
    'Does the autonomy constraint include explicit policy suppression of territorial cultural identity, language, or self-determination claims?',
    'Documentation of education policy (language of instruction, curriculum control), media regulation (censorship of autonomy discourse), civic space restrictions (protest rights, assembly), official identity recognition (minority status, language rights). Measurement of suppression intensity targeting autonomy advocacy specifically.',
    'If cultural suppression is strong: the constraint extracts political identity and self-conception, not merely administrative authority; moves toward snare. If moderate/absent: constraint may function as genuine coordination with asymmetric political authority. Cultural suppression targeting autonomy discourse is a signature of extractive intent beyond structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_suppression_mechanism, empirical, 'Role of cultural suppression in maintaining the autonomy constraint').

omega_variable(
    false_summit_benef_identification,
    'The mountain perspective naturalizes political hierarchy as inevitable to large-scale governance. Who specifically benefits from this framing remaining naturalized rather than subject to renegotiation?',
    'Structural analysis: which actors gain the most from treating territorial subordination as unchangeable? Central authority gains most from naturalizing hierarchy (avoids negotiation costs). Local elite gains from treating it as natural (avoids accountability to constituents for accepting subordination). Identification of vested interests in maintaining false-summit naturalization.',
    'If central authority benefits substantially: false-summit diagnosis is confirmed (naturalization serves extraction). If benefits are distributed: hierarchy may be genuinely coordinative. FSM triggers engine reclassification from mountain to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_benef_identification, preference, 'Who benefits from naturalizing political hierarchy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_autonomy_mainland_territories, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(patm_tr_t0, political_autonomy_mainland_territories, theater_ratio, 0, 0.48).
narrative_ontology:measurement(patm_tr_t3, political_autonomy_mainland_territories, theater_ratio, 3, 0.58).
narrative_ontology:measurement(patm_tr_t6, political_autonomy_mainland_territories, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(patm_be_t0, political_autonomy_mainland_territories, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(patm_be_t3, political_autonomy_mainland_territories, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(patm_be_t6, political_autonomy_mainland_territories, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(patm_su_t0, political_autonomy_mainland_territories, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(patm_su_t3, political_autonomy_mainland_territories, suppression_requirement, 3, 0.61).
narrative_ontology:measurement(patm_su_t6, political_autonomy_mainland_territories, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_autonomy_mainland_territories, enforcement_mechanism).
narrative_ontology:affects_constraint(political_autonomy_mainland_territories, resource_extraction_colonial_territories).
narrative_ontology:affects_constraint(political_autonomy_mainland_territories, cultural_suppression_linguistic_minorities).
narrative_ontology:affects_constraint(political_autonomy_mainland_territories, military_occupation_mainland_governance).

% DUAL FORMULATION NOTE:
% Political autonomy constraint operates at the level of formal governance structure and decision authority. Upstream constraints (colonial-era institutional inheritance, cultural suppression mechanisms) establish the power asymmetries; downstream constraints (specific budget control, military command, appointment veto) instantiate the extraction. These are decomposed as separate stories because their ε values differ: cultural suppression has higher extractiveness (ε ≈ 0.65) because it targets identity; budget control is moderate (ε ≈ 0.50) because it enables some local resource allocation; military command is high (ε ≈ 0.75) because coercion is the direct mechanism. The political autonomy constraint (ε ≈ 0.58) represents the aggregate structure binding these mechanisms together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
