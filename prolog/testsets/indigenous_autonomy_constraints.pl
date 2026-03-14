% ============================================================================
% CONSTRAINT STORY: indigenous_autonomy_constraints
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indigenous_autonomy_constraints, []).

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
 *   constraint_id: indigenous_autonomy_constraints
 *   human_readable: Indigenous Autonomy Constraints
 *   domain: political/colonial/institutional
 *
 * SUMMARY:
 *   Indigenous autonomy constraints represent the structural mechanism
 *   through which settler states maintain territorial control and resource
 *   extraction rights while managing indigenous political claims and
 *   international legitimacy. The constraint operates across three nested
 *   levels: institutional (state law and colonial administration defining
 *   'autonomy' as subordinate to state sovereignty), organizational
 *   (indigenous nations attempting to assert self-determination within
 *   state-imposed frameworks), and individual/community (indigenous people
 *   navigating identity and belonging within conditions of dispossession and
 *   cultural erasure). The constraint is a snare because it combines high
 *   extraction (states capture resources, territorial control, and legitimacy
 *   benefit from indigenous 'recognition') with high suppression (material
 *   barriers from land dispossession, legal barriers from sovereignty denial,
 *   and cognitive barriers from internalized colonial hierarchies). The
 *   theater ratio (0.65) reflects that a substantial portion of the autonomy
 *   architecture is performative: consultation requirements are honored in
 *   form but overridden in substance, recognition is granted and revoked with
 *   changing resource pressures, and co-management frameworks are structured
 *   to preserve ultimate state control. The extractiveness trend (rising from
 *   0.55 to 0.70 over 30 years) shows accumulation rather than plateau,
 *   indicating states have intensified constraints even as they have
 *   increased recognition theater—the performance has become more elaborate
 *   while actual autonomy has contracted.
 *
 * KEY AGENTS:
 *   - Indigenous Nations: Primary victim (powerless/trapped, generational timescale) — face legal denial of sovereignty, territorial dispossession, and dependence on state-controlled resource allocation. Exit blocked by land loss and state enforcement.
 *   - Indigenous Community Members: Primary victim (powerless/identity_locked, biographical timescale) — structurally mobile (could assimilate, relocate) but functionally trapped by identity fusion with ancestral place and community belonging. Exit requires abandoning self.
 *   - Indigenous Sovereignty Coalitions: Secondary agent (organized/constrained, generational timescale) — achieve some coordinating function and extract modest concessions but remain embedded in state-defined legal categories that deny fundamental sovereignty claims.
 *   - Settler States: Primary beneficiary (institutional/arbitrage, immediate timescale) — experience the constraint as pure coordination: extract resources and territorial control while reducing conflict through recognition theater. Have arbitrage options (international legitimacy, trade partner status) dependent on indigenous rights performance.
 *   - Extractive Industries: Secondary beneficiary (institutional/arbitrage, immediate timescale) — benefit from state-enforced autonomy constraints that prevent indigenous veto over resource extraction on traditional territories. Operate under state legal framework that subordinates indigenous claims.
 *   - Colonial Administrative Apparatus: Tertiary agent (institutional/arbitrage, civilizational timescale) — maintains the ritualized autonomy framework (consultation bodies, recognition protocols, co-management agreements) as legitimacy theater. Persists through institutional inertia despite low functional verification.
 *   - Analytical Observer: Structural analyst (analytical/analytical, civilizational timescale) — from global perspective, constraints constitute a snare because the state's coordination benefit coexists with genuine extraction from indigenous perspective. State's experience of coordination does not negate indigenous experience of extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indigenous_autonomy_constraints, 0.68).
domain_priors:suppression_score(indigenous_autonomy_constraints, 0.72).
domain_priors:theater_ratio(indigenous_autonomy_constraints, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indigenous_autonomy_constraints, extractiveness, 0.68).
narrative_ontology:constraint_metric(indigenous_autonomy_constraints, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(indigenous_autonomy_constraints, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indigenous_autonomy_constraints, snare).
narrative_ontology:human_readable(indigenous_autonomy_constraints, "Indigenous Autonomy Constraints").
narrative_ontology:topic_domain(indigenous_autonomy_constraints, "political/colonial/institutional").

domain_priors:requires_active_enforcement(indigenous_autonomy_constraints).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indigenous_autonomy_constraints, settler_states).
narrative_ontology:constraint_beneficiary(indigenous_autonomy_constraints, extractive_industries).
narrative_ontology:constraint_beneficiary(indigenous_autonomy_constraints, land_claim_adjudicators).
narrative_ontology:constraint_victim(indigenous_autonomy_constraints, indigenous_nations).
narrative_ontology:constraint_victim(indigenous_autonomy_constraints, indigenous_communities).
narrative_ontology:constraint_victim(indigenous_autonomy_constraints, traditional_land_stewardship).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED INDIGENOUS NATION (SNARE) — Indigenous nations face legal, territorial, and institutional barriers to genuine autonomy. They are locked into colonial administrative frameworks that define 'autonomy' as devolved management of poverty within externally controlled jurisdictions. Exit is blocked by land dispossession, legal sovereignty denial, and dependence on state-controlled resource allocation. Maximum extraction with minimal escape routes.
constraint_indexing:constraint_classification(indigenous_autonomy_constraints, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IDENTITY-LOCKED COMMUNITY MEMBER (SNARE) — Individual indigenous people face structural mobility (could relocate, adopt dominant-culture frameworks, pursue credentials outside the community) but are functionally trapped by identity fusion with ancestral land, community belonging, and cultural continuity. Exit would require abandoning the identity that constitutes their personhood. The binding is cognitive rather than purely material, but the extraction is identical to the trapped perspective — reinforced through cultural erasure narratives and 'civilization' pressure.
constraint_indexing:constraint_classification(indigenous_autonomy_constraints, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: ORGANIZED INDIGENOUS COALITION (TANGLED ROPE) — Indigenous sovereignty movements achieve some coordination function (land-back agreements, co-management frameworks, cultural protocol recognition) and generate some extraction resistance through organized advocacy. However, these agreements remain embedded within state-defined legal categories that extract consent and legitimacy from communities while preserving ultimate state control. The coalition has agency and can extract concessions, but within a frame that denies their fundamental claim to sovereignty. Mixed coordination and extraction.
constraint_indexing:constraint_classification(indigenous_autonomy_constraints, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SETTLER STATE (ROPE) — From the state's perspective, the constraint enables coordination of resource extraction, territorial administration, and political legitimation. Indigenous autonomy clauses, consultation requirements, and co-management agreements reduce conflict and create appearance of equity. The state experiences this as pure coordination: it solves the problem of governing indigenous territories without direct conflict. The state has arbitrage options (international recognition, trade partner status depend partly on indigenous rights performance) and sees the constraint as enabling, not constraining.
constraint_indexing:constraint_classification(indigenous_autonomy_constraints, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COLONIAL INSTITUTION RITUAL (PITON) — From a civilizational view, the entire apparatus of indigenous 'consultation,' 'recognition,' and 'autonomy' frameworks is substantially theatrical. Consultation outcomes are ignored, recognition is revoked under resource pressure, autonomy is overridden by state emergency powers. The institutional framework persists because it provides legitimacy theater without sacrificing state control — the performative gesture of inclusion maintains the colonial structure. Theater ratio (0.65) reflects high ratio of ritualized recognition to actual power transfer.
constraint_indexing:constraint_classification(indigenous_autonomy_constraints, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a global, civilizational view, indigenous autonomy constraints constitute a snare because: (1) extraction is genuine and asymmetric — states extract legitimacy, resources, and territorial control while granting symbolic autonomy; (2) suppression is structural and cognitive — material barriers (land dispossession) are reinforced by epistemic barriers (denial of indigenous sovereignty as legitimate category); (3) there is minimal coordination benefit to indigenous communities — any coordination that occurs is within a frame imposed by the extractor; (4) the constraint's existence depends entirely on suppressing the alternative (genuine indigenous sovereignty and territorial control). The state's experience of coordination does not negate the extraction from the indigenous perspective.
constraint_indexing:constraint_classification(indigenous_autonomy_constraints, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indigenous_autonomy_constraints_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indigenous_autonomy_constraints, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indigenous_autonomy_constraints, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indigenous_autonomy_constraints, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indigenous_autonomy_constraints, TR),
    TR >= 0.70.

:- end_tests(indigenous_autonomy_constraints_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Settler states extract multiple values from indigenous autonomy constraints: (1) territorial control over traditional lands and resources, (2) legitimacy benefit from appearing to respect indigenous rights internationally, (3) administrative simplification by devolving poverty management to indigenous authorities while retaining resource extraction and veto power, (4) cognitive extraction through narrative naturalization of colonial hierarchy. The extracted value is asymmetric — states gain strategic resources and legitimacy while indigenous communities receive subordinate governance of degraded jurisdictions. Suppression (0.72): High. Multiple overlapping suppression mechanisms: (1) material barriers from land dispossession (historical and ongoing), (2) legal barriers from denial of sovereign status, (3) economic barriers from poverty and resource dependence created by extraction, (4) institutional barriers from administrative structures that define autonomy within state categories, (5) cognitive barriers from internalized hierarchies and erasure narratives. The combination of structural and internalized suppression creates high exit cost. Theater ratio (0.65): Moderate-high. Approximately 65% of the autonomy apparatus is performative rather than functional. Consultation requirements are honored but ignored; recognition is symbolic; co-management agreements have built-in state veto; self-determination is bounded by state law. The performance has intensified over the interval (rising from 0.45 to 0.68) while actual autonomy has contracted, indicating Goodhart drift — the metrics (number of consultations, number of recognition frameworks) have become substitutes for actual outcomes (indigenous decision-making power, territorial control). Extractiveness trend (0.55→0.70): Accumulation. The constraint has intensified rather than stabilized. This reflects both increased state sophistication in managing indigenous claims (more recognition theater while tightening effective control) and the cyclical pressure of resource commodity cycles (autonomy is expanded during low-extraction periods, contracted during resource booms). The generational perspective from indigenous nations shows this as tightening vice rather than negotiated equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across positions. The settler state sees Rope (coordination enabling administration and legitimacy), while indigenous nations see Snare (pure extraction with suppressed exit). The organized indigenous coalition sees Tangled Rope (mixed coordination of cultural protocols and extraction through administrative subordination), while individual community members see Snare modified by identity lock (trapped by psychological fusion with place and community, structurally mobile but functionally immobilized). The piton perspective reveals that much of the recognition apparatus is ritualized theater maintained through institutional inertia. The analytical observer's Snare classification emphasizes that the state's genuine coordination benefit (reduced administrative cost, international legitimacy) is purchased through genuine extraction from indigenous communities — the two are not zero-sum but correlate positively: state coordination requires indigenous suppression. This perspectival gap is not a measurement ambiguity but a structural feature: the constraint transfers value from indigenous to settler institutions across multiple dimensions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position: who bears extraction cost, who reaps benefit, and what are the exit options. Indigenous nations as primary victims with trapped exit options derive high d (→ 0.92-0.98), producing maximum experienced extraction through f(d). Individual community members with identity_locked exit derive high d (→ 0.88-0.95), experiencing severe extraction filtered through cognitive mechanisms. Organized coalitions with constrained exit but some agency derive moderate d (→ 0.55-0.65), producing moderate extraction χ despite some coordination function. Settler states as primary beneficiaries with arbitrage exit options derive low d (→ 0.05-0.15), producing negative or near-zero χ from their perspective — they experience the constraint as enabling rather than costly. The pipeline automatically computes these from beneficiary/victim declarations and exit options; no overrides are needed because the structural data is unambiguous: extraction flows from indigenous to settler institutions, suppression mechanisms target indigenous autonomy, and beneficiaries are settler states and extractive industries.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in indigenous autonomy constraints arises from the state's false claim that recognizing indigenous 'autonomy' constitutes coordination rather than extraction management. The resolution is perspectival: from the state's position, the constraint is genuine coordination (it solves the legitimate problem of administering diverse territories with heterogeneous populations). From the indigenous position, the state's 'coordination benefit' is purchased through structural extraction. The mandatrophy is resolved by rejecting the premise that the state's experience of coordination invalidates the indigenous experience of extraction. Both are true: the constraint coordinates state-indigenous interaction (establishes shared institutional frames, reduces conflict) AND extracts resources and autonomy from indigenous communities (transfers control and benefit to settler institutions). The six perspectives resolve the mandatrophy by showing that Rope (state view), Snare (indigenous view), Tangled Rope (coalition view), and Piton (civilizational inertia view) are all accurate descriptions of the same constraint from different positions. The constraint is structurally a Snare because extraction asymmetry and suppression are primary, and the state's coordination benefit is secondary to and dependent upon that extraction. The piton classification at civilizational timescale reveals that much of the autonomy recognition apparatus has become decoupled from function — it persists because replacing it would require either genuine sovereignty concession (unacceptable to settler states) or explicit authoritarianism (unacceptable internationally), so the ritualized recognition persists as a stable compromise that serves nobody's interests well but everyone's interests adequately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_definition_boundary,
    'What constitutes genuine autonomy versus administrative devolution? Is co-management within state-defined boundaries autonomy or extraction dressed as inclusion?',
    'Test cases: Compare outcomes when indigenous communities exercise autonomous decisions against state preference. If state veto is exercised or agreements are superseded by state law, autonomy was illusory. Examine jurisdictional authority over: land use, resource extraction, law enforcement, taxation, membership definition.',
    'If autonomy is genuine: constraints downgrade to Tangled Rope from institutional perspective. If autonomy is illusory: classification remains Snare — the state experiences coordination, but indigenous communities experience pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_definition_boundary, empirical, 'Boundary between genuine autonomy and administrative devolution').

omega_variable(
    identity_lock_cognitive_capture,
    'To what degree are indigenous people''s constraints internalized as identity obligations versus experienced as external barriers? Is the binding mechanism psychological (identity fusion) or structural (material barriers)?',
    'Pre/post-exit trajectory analysis: Do indigenous people who relocate to non-indigenous contexts and assimilate still carry suppression-consistent behaviors? Do they describe their original constraints as external or as self-imposed? Comparative analysis of indigenous communities with different historical contact intensity.',
    'If primarily internalized: identity_locked is appropriate, and the constraint persists through cognitive capture rather than material force alone. If primarily material: exit_options should be trapped or constrained, and the identity lock is secondary rather than primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_cognitive_capture, empirical, 'Whether suppression is cognitive (internalized) or structural (external barriers)').

omega_variable(
    state_coordination_genuine,
    'Does the state''s experience of coordination (reduced conflict, simplified administration) constitute genuine mutual benefit, or is state benefit (extraction) while communities experience only cost (suppression)?',
    'Counterfactual: If indigenous autonomy constraints were removed, would administrative costs to the state increase significantly? Compare state transaction costs under current constraint versus hypothetical unconstrained scenario. Examine state budget allocation to indigenous administration versus resource extraction from indigenous territories.',
    'If state benefit is genuine mutual coordination: perspectives remain Rope from state view, Snare from indigenous view. If state benefit is pure extraction: state perspective should downgrade to Tangled Rope or Snare, revealing the false coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_coordination_genuine, empirical, 'Whether state coordination benefit is genuine or extraction dressed as coordination').

omega_variable(
    legal_sovereignty_empirical_status,
    'Do any settler states genuinely recognize indigenous legal sovereignty over traditional territories, or is all ''recognition'' limited to administrative autonomy within state-defined boundaries?',
    'Jurisdictional analysis: Can indigenous nations prosecute cases in their own courts against non-indigenous defendants? Can they override state environmental law? Can they exclude non-members from resource access? Do international treaties recognize indigenous nations as sovereign parties?',
    'If genuine sovereignty exists: constraints in those contexts may upgrade from Snare to genuine Tangled Rope or Rope. If sovereignty is universally denied: snare classification is empirically confirmed across all contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_sovereignty_empirical_status, empirical, 'Empirical status of indigenous legal sovereignty recognition').

omega_variable(
    coalition_power_threshold,
    'At what organizational scale and resource capacity do indigenous coalitions transition from powerless (trapped) to organized (constrained exit)? Does organizational capacity change the classification?',
    'Comparative analysis of well-resourced coalition (e.g., Haudenosaunee Confederacy, Ainu Federation) versus poorly-resourced communities. Track correlation between coalition institutional strength and actual autonomy outcomes. Identify threshold beyond which indigenous organizations can achieve binding agreements against state preference.',
    'If organizational power enables genuine resistance: organized perspective justified. If even well-resourced coalitions remain unable to override state veto: powerless classification remains appropriate, and organized label is premature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_threshold, empirical, 'Organizational power threshold for indigenous autonomy effectiveness').

omega_variable(
    extractiveness_accumulation_or_plateau,
    'Is the constraint''s extractiveness increasing over time (settler states layering more control mechanisms, tightening resource constraints) or plateauing (reaching an equilibrium where further extraction becomes counterproductive)?',
    'Temporal analysis: Compare extractiveness metrics across 30+ year intervals. Track: land claims success rate, resource extraction on indigenous territories, state budget allocation to indigenous services, co-management agreement overrides, revocation of recognition. If all metrics show worsening: extractiveness accumulating. If metrics show oscillation or stabilization: plateau.',
    'If accumulating: Snare classification confirmed, and measurements should show rising extractiveness over interval. If plateau: constraint may be stabilizing into Tangled Rope, indicating state has reached extraction ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_accumulation_or_plateau, empirical, 'Whether constraint extractiveness is accumulating or plateauing over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indigenous_autonomy_constraints, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indi_tr_t0, indigenous_autonomy_constraints, theater_ratio, 0, 0.45).
narrative_ontology:measurement(indi_tr_t10, indigenous_autonomy_constraints, theater_ratio, 10, 0.58).
narrative_ontology:measurement(indi_tr_t20, indigenous_autonomy_constraints, theater_ratio, 20, 0.65).
narrative_ontology:measurement(indi_tr_t30, indigenous_autonomy_constraints, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(indi_be_t0, indigenous_autonomy_constraints, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(indi_be_t10, indigenous_autonomy_constraints, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(indi_be_t20, indigenous_autonomy_constraints, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(indi_be_t30, indigenous_autonomy_constraints, base_extractiveness, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indigenous_autonomy_constraints, identity_coordination).
narrative_ontology:affects_constraint(indigenous_autonomy_constraints, land_dispossession_extraction).
narrative_ontology:affects_constraint(indigenous_autonomy_constraints, environmental_degradation_indigenous_territories).
narrative_ontology:affects_constraint(indigenous_autonomy_constraints, cultural_erasure_cognitive_capture).
narrative_ontology:affects_constraint(indigenous_autonomy_constraints, settler_colonial_legitimacy_theater).

% DUAL FORMULATION NOTE:
% Indigenous autonomy constraints are part of a settler colonial constraint family. The immediate constraint (autonomy administration) is downstream of material constraints (land dispossession, resource extraction) and upstream of cognitive constraints (epistemic erasure, identity capture). All family members should be decomposed and linked: each has distinct ε values reflecting empirical measurability. Land dispossession is historical (ε≈0.15, Mountain) — the dispossession occurred and is immutable from any perspective. Autonomy constraints are contemporary (ε≈0.68, Snare) — they are active extraction mechanisms dependent on suppression. Epistemic erasure and cultural capture are psychological (ε≈0.55-0.72, Snare/identity-locked variant) — constraints that operate through cognitive mechanisms rather than pure legal authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indigenous_autonomy_constraints, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
