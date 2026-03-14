% ============================================================================
% CONSTRAINT STORY: riot_gear_procurement_militarization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_riot_gear_procurement_militarization, []).

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
 *   constraint_id: riot_gear_procurement_militarization
 *   human_readable: Riot Gear Procurement and Police Militarization
 *   domain: political/law_enforcement/governance
 *
 * SUMMARY:
 *   Riot gear procurement represents a hybrid constraint combining genuine
 *   police coordination (managing large-scale civil unrest requires equipment
 *   standardization and training) with rent-seeking extraction (equipment
 *   manufacturers benefit from sustained demand; police departments
 *   accumulate capacity beyond documented threat levels; federal grants
 *   create path dependencies that lock in procurement). The constraint's
 *   theater has increased over the 20-year interval: early procurement
 *   rhetoric emphasized emergency response to specific threat events;
 *   contemporary procurement rhetoric emphasizes 'preparedness' decoupled
 *   from demonstrated need. The constraint operates across four structural
 *   levels: the immediate level (police equipment choices), the
 *   organizational level (department budgeting and federal grant
 *   administration), the political level (legislative framework enabling
 *   procurement), and the ideological level (naturalization of militarization
 *   as inherent to modern policing). Marginalized communities experiencing
 *   militarized policing in their neighborhoods bear extraction costs with no
 *   exit option; police departments experience the constraint as coordination
 *   enabling their core public safety function; equipment manufacturers
 *   benefit from steady demand; civil rights coalitions see both coordination
 *   requirement and extraction mechanism. The rising theater_ratio indicates
 *   Goodhart drift: grant language originally emphasized public safety
 *   coordination, but the metric (equipment acquisition, training hours,
 *   operational readiness) has become decoupled from actual public safety
 *   outcomes.
 *
 * KEY AGENTS:
 *   - Marginalized Communities: Primary victims (powerless/trapped) — residents in heavily policed neighborhoods; geographic entrapment creates permanent exposure to militarized enforcement without exit option
 *   - Protest Participants: Secondary victims (moderate/constrained) — face escalated force capacity during demonstrations; constrained by right to protest and fear of surveillance/retaliation
 *   - Police Departments: Primary beneficiaries (institutional/arbitrage) — experience constraint as coordination mechanism enabling response capacity; have budgetary flexibility and exit options
 *   - Protective Equipment Manufacturers: Secondary beneficiaries (institutional/arbitrage) — stable demand channel through federal grants; low market risk; true arbitrage options available
 *   - Federal Grant Administration System: Institutional intermediary (institutional/constrained) — maintains procurement apparatus through inertia; constrained by Congressional intent and existing contracts; degraded piton function
 *   - Civil Rights Organizations: Organized opposition (organized/constrained) — coalition capacity for advocacy and litigation; constrained by legal precedent and resource limitations; achieving incremental policy changes
 *   - Community Accountability Initiatives: Emergent alternative (organized/constrained) — local experiments with civilian oversight and equipment audits; showing alternative coordination pathways with sunset potential
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risk of naturalizing contingent institutional arrangements as inherent to policing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(riot_gear_procurement_militarization, 0.58).
domain_priors:suppression_score(riot_gear_procurement_militarization, 0.62).
domain_priors:theater_ratio(riot_gear_procurement_militarization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(riot_gear_procurement_militarization, extractiveness, 0.58).
narrative_ontology:constraint_metric(riot_gear_procurement_militarization, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(riot_gear_procurement_militarization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(riot_gear_procurement_militarization, tangled_rope).
narrative_ontology:human_readable(riot_gear_procurement_militarization, "Riot Gear Procurement and Police Militarization").
narrative_ontology:topic_domain(riot_gear_procurement_militarization, "political/law_enforcement/governance").

domain_priors:requires_active_enforcement(riot_gear_procurement_militarization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(riot_gear_procurement_militarization, police_departments).
narrative_ontology:constraint_beneficiary(riot_gear_procurement_militarization, protective_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(riot_gear_procurement_militarization, federal_grant_administrators).
narrative_ontology:constraint_victim(riot_gear_procurement_militarization, marginalized_communities).
narrative_ontology:constraint_victim(riot_gear_procurement_militarization, protest_participants).
narrative_ontology:constraint_victim(riot_gear_procurement_militarization, civil_liberties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED COMMUNITIES (SNARE) — Residents of heavily policed neighborhoods face escalated enforcement equipment with no exit option. The constraint operates through geographic entrapment and structural inequity: residents cannot move freely, cannot influence procurement decisions, and cannot opt out of militarized police presence. Maximum extraction experienced.
constraint_indexing:constraint_classification(riot_gear_procurement_militarization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROTEST PARTICIPANTS (TANGLED ROPE) — Face extraction through escalated force capacity during demonstrations, but also benefit from the coordination function riot gear provides to police (crowd management that prevents total chaos). Exit options are constrained by the right to protest and fear of retaliation. Moderate extraction with genuine coordination component.
constraint_indexing:constraint_classification(riot_gear_procurement_militarization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: POLICE DEPARTMENTS (ROPE) — Experience the constraint as coordination: riot gear enables coordinated response to civil unrest, mass protests, and emergency situations. Departments have arbitrage options (can choose equipment levels, can reallocate budgets). Net beneficiary position. The constraint solves their collective action problem of managing large-scale disorder.
constraint_indexing:constraint_classification(riot_gear_procurement_militarization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MANUFACTURERS (ROPE) — Pure coordination for equipment suppliers. Federal grant programs (1033 Program, COPS grants) standardize demand and reduce market uncertainty. Manufacturers see this as market-enabling coordination, not extraction. Arbitrage options are robust — can exit by producing for other sectors.
constraint_indexing:constraint_classification(riot_gear_procurement_militarization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FEDERAL GRANT ADMINISTRATION (PITON) — The grant apparatus (1033 Program, COPS Office) persists largely through institutional inertia despite contested effectiveness. Theater ratio is high: grant language emphasizes public safety coordination, but primary function is now supply channel for military-grade equipment. The system maintains performative oversight while equipment escalation continues. Constrained by Congressional intent and procurement law; doesn't fully function anymore but persists.
constraint_indexing:constraint_classification(riot_gear_procurement_militarization, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CIVIL RIGHTS COALITIONS (TANGLED ROPE) — Organized agents see both genuine coordination problem (police do need response capacity for genuine emergencies) and clear extraction mechanism (equipment procurement drives culture shift toward militarization independent of threat level). Coalition participants face constraints from legal precedent, institutional resistance, and competing reform priorities, but have organized capacity and exit paths (legislative advocacy, litigation). Moderate extraction with real coordination component.
constraint_indexing:constraint_classification(riot_gear_procurement_militarization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: COMMUNITY ACCOUNTABILITY INITIATIVES (SCAFFOLD) — Some jurisdictions are experimenting with alternative mechanisms: community oversight boards, civilian review authority, equipment audits, and transparency requirements. These create a sunset pathway for the traditional unconstrained procurement model. Low theater because accountability mechanisms are substantive rather than performative. This perspective shows the constraint as temporary, with genuine exit path emerging from local organizing.
constraint_indexing:constraint_classification(riot_gear_procurement_militarization, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From analytical distance, one might classify riot gear procurement as an immutable natural law of policing: 'Police forces always maximize available capacity; escalation spirals are inherent to security competition.' This naturalizes what is actually a contingent policy choice. The engine's false summit detector will flag this perspective as a naturalization of institutional arrangements that remain contestable.
constraint_indexing:constraint_classification(riot_gear_procurement_militarization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(riot_gear_procurement_militarization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(riot_gear_procurement_militarization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(riot_gear_procurement_militarization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(riot_gear_procurement_militarization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(riot_gear_procurement_militarization, TR),
    TR >= 0.70.

:- end_tests(riot_gear_procurement_militarization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint coordinates genuine police coordination (emergency response requires equipment standardization and training) while simultaneously extracting from marginalized communities through capacity deployment and intimidation. The base_extractiveness trajectory shows clear drift: starting at 0.32 (early 1990s, justified emergency response focus) to 0.58 (2010s+, disconnected from documented threat level). The risen value reflects the growing gap between stated public safety rationale and actual deployment patterns emphasizing preparedness divorced from specific threats. Suppression (0.62): High. Multiple suppression mechanisms operate: (1) geographic entrapment — marginalized communities cannot exit highly policed neighborhoods; (2) informational asymmetry — procurement and deployment decisions opaque to communities most affected; (3) legal precedent — courts have repeatedly upheld police equipment acquisition as administrative discretion; (4) retaliation risk — community advocacy against militarization faces surveillance and legal harassment. Suppression is structural and sustained. Theater ratio (0.68): High and rising. Grant administration and police department statements emphasize public safety coordination and emergency preparedness. Actual metrics (equipment stockpiles, training hours, equipment age) show maintenance of excess capacity disconnected from incident frequency. The gap has widened over the interval — public rhetoric has remained constant while functional deployment has shifted toward intimidation/capacity building over genuine emergency response. Theater is performative justification for path-dependent acquisition.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap: marginalized communities see snare (pure extraction, no exit, no coordination benefit), while police departments see rope (pure coordination, arbitrage options, enabling their core function). Both perceptions are structurally accurate from their respective positions. The gap is not a disagreement about facts but a difference in structural relationship — one group is the target, one is the beneficiary. The piton perspective (federal grants) bridges this gap by revealing that the constraint persists through inertia rather than functional need: equipment procurement continues because budget cycles and grant availability drive it, not because documented threats require escalated capacity. This suggests that the police/manufacturer rope perspective is partially supported by genuine coordination need, partially by institutional path-dependency. Community accountability experiments show scaffold perspective — civilians can shift procurement logic toward documentation and justification, creating sunset pathway. The analytical mountain perspective is a false summit: it naturalizes what is contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options per the signed derivation chain. Marginalized communities occupy d ≈ 0.92 (powerless/trapped/victim): full target status. They experience f(d) ≈ 1.35, producing high chi at police perspective. Police departments occupy d ≈ 0.12 (institutional/arbitrage/beneficiary): full beneficiary status. They experience f(d) ≈ -0.02, producing negative/zero chi at their own perspective. The f(d) sigmoid reflects that beneficiaries with exit options experience the constraint as low-extraction coordination; victims with no exit experience maximum extraction. Organized civil rights coalitions occupy intermediate d ≈ 0.50 (organized/constrained) derived from mixed victim/observer status with constrained exit: they experience f(d) ≈ 0.70, producing moderate chi. The piton perspective has institutional power but constrained exit (path-dependent federal grants), yielding d ≈ 0.45 and f(d) ≈ 0.55. These derivations explain why the constraint maps to Snare for powerless agents, Tangled Rope for moderate agents with mixed beneficiary/victim position, and Rope for institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is classified as Tangled Rope at the analytical level, which correctly captures both genuine coordination function (police emergency response capacity) and asymmetric extraction (equipment deployment concentrated on marginalized communities with no exit option). The mandatrophy is resolved by measuring the beneficiary/victim asymmetry: police departments benefit from coordination; marginalized communities bear extraction costs. The constraint is not pure coordination (rope) because marginalized communities experience snare — no coordination benefit, no exit option. The constraint is not pure extraction (snare) because police departments genuinely use equipment for emergency response (coordination function exists). Tangled Rope is the correct classification because (1) coordination function is present and necessary (emergency response), (2) asymmetric extraction is present and significant (equipment deployment driven by power asymmetry not threat level), (3) active enforcement is required (grant administration, departmental procurement decisions), and (4) the two functions are coupled — the coordination function is the legitimizing frame for the extraction. Theater_ratio rising from 0.42 to 0.68 indicates Goodhart drift: the stated metric (emergency preparedness) has decoupled from actual outcome metrics (public safety incident frequency). The gap between stated rationale and actual deployment is precisely the mandatrophy signal — the constraint naturalizes extraction as inherent coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_level_counterfactual,
    'Does riot gear procurement causally drive militarization culture, or does it respond to genuine threat escalation?',
    'Comparative historical analysis: jurisdictions with equipment availability vs matched controls without equipment access; correlation between procurement timeline and actual civil unrest frequency; measurement of force escalation timing relative to equipment acquisition',
    'If causal (procurement drives culture): extractiveness increases to 0.68+ (pure rent-seeking by police/manufacturers). If responsive: extractiveness decreases to 0.35 (legitimate coordination mechanism). Current 0.58 assumes bidirectional causality — equipment availability influences police readiness posture, which influences likelihood of escalated response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_level_counterfactual, empirical, 'Causal direction between equipment availability and militarization culture').

omega_variable(
    legitimate_coordination_quantity,
    'What fraction of riot gear procurement serves genuine emergency response vs excess capacity maintained for intimidation or revenue cycling?',
    'Equipment usage tracking: ratio of actual deployment to stored inventory; comparative analysis of equipment utilization rates across jurisdictions; accountability audit of equipment age and service life vs procurement rates',
    'If >60% utilization in genuine emergencies: rope classification more accurate, suppression decreases to 0.40. If <30% utilization: snare classification more accurate from police perspective, extractiveness increases to 0.75.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_coordination_quantity, empirical, 'Fraction of procurement serving legitimate vs excess capacity').

omega_variable(
    community_alternative_viability,
    'Can civilian oversight boards and community-based accountability mechanisms functionally replace top-down procurement controls?',
    'Longitudinal tracking of jurisdictions with accountability mechanisms: changes in procurement patterns, force escalation rates, community complaint resolution; comparison of public safety outcomes vs militarization metrics',
    'If viable: scaffold perspective confirmed, sunset is real, theater ratio decreases to <0.50. If ineffective: scaffold is aspirational rather than structural, theater ratio increases and constraint hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(community_alternative_viability, empirical, 'Viability of community-based alternatives to procurement controls').

omega_variable(
    federal_grant_intent_vs_function_gap,
    'How large is the gap between stated grant purposes (public safety coordination) and actual function (equipment supply channel for militarization)?',
    'Grant utilization audit: comparison of funded equipment against stated public safety objectives; congressional testimony vs actual deployment patterns; analysis of competitive disadvantage for non-militarized departments',
    'If gap is large: piton classification confirmed, theater_ratio justified at 0.68. If grant functions as designed: rope classification applies, theater drops to 0.35.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_grant_intent_vs_function_gap, empirical, 'Gap between federal grant stated purpose and actual function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(riot_gear_procurement_militarization, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(riot_tr_t0, riot_gear_procurement_militarization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(riot_tr_t10, riot_gear_procurement_militarization, theater_ratio, 10, 0.55).
narrative_ontology:measurement(riot_tr_t20, riot_gear_procurement_militarization, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(riot_be_t0, riot_gear_procurement_militarization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(riot_be_t10, riot_gear_procurement_militarization, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(riot_be_t20, riot_gear_procurement_militarization, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(riot_gear_procurement_militarization, enforcement_mechanism).
narrative_ontology:affects_constraint(riot_gear_procurement_militarization, police_accountability_gap).
narrative_ontology:affects_constraint(riot_gear_procurement_militarization, military_equipment_domestic_deployment).
narrative_ontology:affects_constraint(riot_gear_procurement_militarization, protest_suppression_infrastructure).

% DUAL FORMULATION NOTE:
% Riot gear procurement is downstream of federal grant programs (1033 Program, COPS Office) and upstream of specific police department militarization patterns. The constraint family includes: (1) federal grant structure (ε≈0.30, Rope), (2) riot gear procurement (ε≈0.58, Tangled Rope, this story), (3) police militarization culture (ε≈0.72, Snare). Each has distinct extractiveness and beneficiary/victim profiles. Network links show causal dependency: grants enable procurement, procurement drives culture shift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(riot_gear_procurement_militarization, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
