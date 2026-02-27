% ============================================================================
% CONSTRAINT STORY: bedouin_sedentary_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bedouin_sedentary_transition, []).

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
 *   constraint_id: bedouin_sedentary_transition
 *   human_readable: The Bedouin Sedentary Transition: From Pastoral Autonomy to Oil-Dependent Settlement
 *   domain: social/economic/technological
 *
 * SUMMARY:
 *   The Bedouin sedentary transition represents the structural transformation
 *   of nomadic pastoral tribes in the Rub' al Khali (Empty Quarter) of Saudi
 *   Arabia from autonomous herding communities into welfare-dependent urban
 *   and semi-urban settlements. Triggered by oil discovery (1938) and
 *   accelerated by state consolidation (1960s-1980s), this transition
 *   exhibits all six DR classifications across different perspectives. The
 *   constraint is not sedentarization per se — settlement is a neutral
 *   technological choice — but the asymmetric institutional mechanism through
 *   which it occurs: state apparatus + oil capital interests + welfare
 *   bureaucracy combine to make pastoral return structurally impossible while
 *   extracting labor mobility, cultural autonomy, and ecological knowledge
 *   from formerly independent tribes. The mechanism requires active
 *   enforcement: settlement subsidies, school attendance mandates, water
 *   allocation controls, and selective patronage of compliant tribal leaders.
 *   At the analytical level, the transition is often naturalized as
 *   'inevitable modernization,' but the structural data reveals it as a
 *   contingent policy choice enabled by oil rents and sustained by coercive
 *   state capacity.
 *
 * KEY AGENTS:
 *   - Sedentarized Bedouin Tribes: Primary victims (powerless/trapped) — lose pastoral autonomy, ecological knowledge, social self-determination; face structural impossibility of return after one generation
 *   - Tribal Leadership (Sheikhs, Elders): Secondary beneficiary/victim (organized/constrained) — gain state patronage and material resources but lose independent authority and become state intermediaries
 *   - Saudi State Apparatus: Primary beneficiary (institutional/arbitrage) — solves governance problem of peripheral territory; achieves administrative closure, conscription capacity, security surveillance
 *   - Oil Capital Interests: Secondary beneficiary (powerful/mobile) — requires land pacification and labor stability for hydrocarbon extraction; sedentarization clears pastoral claims
 *   - Welfare Bureaucracy: Institutional manager (institutional/constrained) — maintains settlement system through inertia; performs modernization narrative while perpetuating dependency
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy as inevitable development law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bedouin_sedentary_transition, 0.58).
domain_priors:suppression_score(bedouin_sedentary_transition, 0.72).
domain_priors:theater_ratio(bedouin_sedentary_transition, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bedouin_sedentary_transition, extractiveness, 0.58).
narrative_ontology:constraint_metric(bedouin_sedentary_transition, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bedouin_sedentary_transition, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bedouin_sedentary_transition, tangled_rope).
narrative_ontology:human_readable(bedouin_sedentary_transition, "The Bedouin Sedentary Transition: From Pastoral Autonomy to Oil-Dependent Settlement").
narrative_ontology:topic_domain(bedouin_sedentary_transition, "social/economic/technological").

domain_priors:requires_active_enforcement(bedouin_sedentary_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bedouin_sedentary_transition, state_apparatus).
narrative_ontology:constraint_beneficiary(bedouin_sedentary_transition, oil_capital_interests).
narrative_ontology:constraint_victim(bedouin_sedentary_transition, pastoral_autonomy).
narrative_ontology:constraint_victim(bedouin_sedentary_transition, ecological_resilience).
narrative_ontology:constraint_victim(bedouin_sedentary_transition, tribal_social_structure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEDENTARIZED BEDOUIN TRIBE (SNARE) — Once settlement occurs, nomadic exit becomes structurally impossible. Children educated in fixed settlements cannot rejoin pastoral life; pastoral skills atrophy; welfare dependency creates psychological and material lock-in. No viable return path to the desert ecology that sustained ancestors. Maximum experienced extraction — loss of autonomy, ecological knowledge, social self-determination with zero alternatives.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRIBAL LEADERSHIP COHORT (TANGLED ROPE) — Tribal sheikhs and elders experience the constraint as hybrid. They gain state recognition, material resources (housing subsidies, employment in military/security apparatus), and institutional legitimacy through sedentarization. But they lose independent authority over pastoral territories and must execute state directives in exchange for continued patronage. Active enforcement required: state monitors tribal settlements, controls water/welfare distribution, selects which leaders receive sinecures. Both coordination (state security apparatus needs organized tribal intermediaries) and asymmetric extraction (sheikhs become state agents, lose pastoral decision-making authority).
constraint_indexing:constraint_classification(bedouin_sedentary_transition, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE APPARATUS (ROPE) — The state experiences sedentarization as pure coordination. Nomadic tribes present governance and taxation challenges; settlement enables census, conscription, welfare administration, security surveillance. The state achieves administrative closure and control over peripheral territory. This is genuine coordination benefit — the state solves the 'how do we govern the Empty Quarter?' problem through settlement. Net beneficiary; extraction flows toward this agent.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: OIL INDUSTRY (SCAFFOLD) — Hydrocarbon extraction required pacifying the Rub' al Khali and removing nomadic claims to pastoral territories. Sedentarization was instrumental (temporary measure to clear land and stabilize labor). The sunset clause is implicit: once oil reserves deplete or technology/global economics shift toward renewable energy, the rationale for maintaining settled Bedouin populations vanishes. Oil money currently subsidizes settlement; the constraint is inherently temporary. Effective extraction is moderated by the knowledge that the underlying justification has a time horizon.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: WELFARE BUREAUCRACY (PITON) — The settlement maintenance system (housing, schooling, healthcare, unemployment transfers) persists through institutional inertia despite eroding functional rationale. Early settlement (1960s-1980s) served genuine social integration goals; current welfare apparatus is substantially theater — it maintains populations in economic dependency while claiming to facilitate 'development.' Theater ratio 0.65 reflects that welfare spending is performed as modernization/care while actually perpetuating structural immobility. The functional purpose (labor reserve for oil industry) has shifted; the institutional machinery persists.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the nomadic-to-sedentary transition is framed as a natural law of development: modernization requires settlement, state integration requires administrative closure, technological civilization requires fixed populations. This perspective naturalizes the transition as inevitable. However, the structural data contradicts mountain classification — the transition is contingent on oil discovery, state policy choices, and specific institutional arrangements. The engine will identify this as a false summit: the 'inevitability of development' discourse masks what is actually a extractive institutional arrangement sustained by state enforcement and rent-seeking capital.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bedouin_sedentary_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bedouin_sedentary_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bedouin_sedentary_transition, TR),
    TR >= 0.70.

:- end_tests(bedouin_sedentary_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The primary extraction is loss of autonomy and forced dependency on state-controlled resources (water, welfare, employment). However, it is not maximal (0.70+) because settlement also provides access to healthcare, education, and reduced mortality risk — genuine coordination benefits exist alongside extraction. The value reflects that the transition involves both coercive lock-in and real material improvement, with the lock-in component dominating. Suppression (0.72): High. Significant barriers to refusing settlement include military enforcement, welfare conditionality, school attendance mandates, water access controls, and social pressure from settled peers. Nomadic pastoralism is suppressed through both direct force and administrative mechanisms (no pastoral licensing, land enclosure, school enrollment requirements for welfare). Theater ratio (0.65): Moderate-high. Settlement is performed as 'development' and 'modernization,' with heavy rhetorical emphasis on improved living standards and integration into national life. The welfare apparatus is substantially performative — it claims to facilitate progress while actually perpetuating dependency. Educational curricula emphasize urban skills while extinguishing pastoral knowledge. The performance has increased as oil wealth enabled more visible settlement infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The sedentarized tribe experiences maximum snare (no exit, full extraction, trapped in dependency). The state experiences rope (pure coordination benefit — solving governance problem). The oil industry experiences scaffold (temporary measure with implicit sunset when oil depletes). The welfare bureaucracy experiences piton (degraded institution maintained through inertia). Tribal leaders experience tangled rope (genuine patronage benefits mixed with loss of independent authority). The analytical observer risks mountain (naturalizing policy as inevitable). The perspectival gap reveals the constraint's core injustice: what appears to the state as beneficial coordination appears to former nomads as irreversible extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position. Sedentarized tribes: beneficiary status is false (they are victims), trapped exit (d ≈ 0.95), powerless (canonical d = 1.0), → f(d) ≈ 1.42 → high experienced extractiveness. State apparatus: beneficiary status is true (gains governance capacity), arbitrage exit (low d ≈ 0.05), institutional power (canonical d = 0.0), → f(d) ≈ -0.12 → negative experienced extractiveness (net benefit flows toward state). Tribal leadership: bifurcated — as beneficiary of patronage (arbitrage exit, organized power) but constrained by state oversight, d ≈ 0.40 → mixed directionality. The constraint enforces power asymmetry: agents with weak exit capacity bear extraction; agents with arbitrage/mobility bear no extraction and gain benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves mandatrophy by demonstrating that sedentarization involves both genuine coordination (state solving governance problem) and asymmetric extraction (tribes losing autonomy). The coordination function is real: settled populations enable tax collection, security administration, labor mobilization. The extraction is real: tribes permanently lose pastoral self-determination and become dependency subjects. Both functions are structurally necessary — the state cannot achieve governance without settlement, and settlement cannot occur without coercion + welfare lock-in. The false summit at the analytical level ('inevitable modernization') is revealed as such: the civilizational perspective naturalizes what is actually a state policy choice, one of several possible transitions. A different policy regime (land rights restoration, pastoral education, water tenure negotiation) would have produced different outcomes without requiring sedentarization. The 'inevitability' framing masks power asymmetry. The tangled rope classification correctly identifies that coordination and extraction are inseparable — this is not a snare that could become a rope through better administration, nor a rope that could be cleaned up through extraction reduction. The hybrid is structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pastoral_reversibility_threshold,
    'Is the loss of pastoral autonomy irreversible after one generation of sedentarization, or could alternative education / land rights restoration enable a return to nomadic pastoralism?',
    'Historical case studies of sedentarized pastoral groups (Australian Aboriginal communities, Central Asian herders post-collectivization); assessment of whether skill transmission and land tenure restoration could restore nomadic viability within a 30-year window',
    'If irreversible: sedentarization is a permanent snare with no escape path. If reversible: the constraint is a temporary tangled rope with potential exit pathways through policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pastoral_reversibility_threshold, empirical, 'Whether pastoral autonomy can be restored after sedentarization').

omega_variable(
    oil_economy_counterfactual,
    'Would Saudi state have pursued Bedouin sedentarization absent hydrocarbon revenues to fund settlement and welfare?',
    'Comparative historical analysis of sedentarization patterns in oil-rich vs oil-poor states; econometric assessment of welfare spending correlation with oil revenue cycles; state policy documents from 1938-1970 revealing intent',
    'If no: sedentarization is contingent on oil rents (scaffold/snare hybrid). If yes: sedentarization reflects developmental necessity (stronger mountain claim). Oil dependency becomes the critical structural factor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oil_economy_counterfactual, conceptual, 'Counterfactual: would sedentarization occur without oil revenues').

omega_variable(
    tribal_leadership_agency,
    'To what extent did tribal sheikhs actively choose sedentarization (negotiated outcome) vs accept it under coercive pressure from state force?',
    'Oral history archives; tribal leadership memoirs and correspondence; historical records of negotiations between state and tribal authorities; assessment of whether sheikhs retained meaningful veto power',
    'If chosen: leadership capture is the mechanism (leaders become willing collaborators). If coerced: state enforcement is the mechanism (leaders are also victims). Affects whether tribal leadership perspective is primarily beneficiary or trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribal_leadership_agency, empirical, 'Degree of tribal leadership agency in sedentarization choice').

omega_variable(
    welfare_exit_mechanism_viability,
    'Are alternative economic pathways (micro-enterprise, renewable energy employment, diaspora remittances) viable for reducing welfare dependency without reconstituting pastoral autonomy?',
    'Assessment of economic diversification in Saudi rural settlements; skills transfer data; employability of second-generation Bedouin outside oil sector; remittance flows from diaspora populations',
    'If viable: scaffold sunset is real — welfare can be replaced without restoring herding. If not viable: welfare lock-in is permanent — only pastoral restoration or continued subsidies remain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_exit_mechanism_viability, empirical, 'Whether alternative economic pathways reduce welfare dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bedouin_sedentary_transition, 1938, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bedouin_tr_t0, bedouin_sedentary_transition, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bedouin_tr_t20, bedouin_sedentary_transition, theater_ratio, 20, 0.52).
narrative_ontology:measurement(bedouin_tr_t40, bedouin_sedentary_transition, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(bedouin_be_t0, bedouin_sedentary_transition, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bedouin_be_t20, bedouin_sedentary_transition, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(bedouin_be_t40, bedouin_sedentary_transition, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bedouin_sedentary_transition, resource_allocation).
narrative_ontology:affects_constraint(bedouin_sedentary_transition, water_scarcity_governance).
narrative_ontology:affects_constraint(bedouin_sedentary_transition, pastoral_commons_enclosure).
narrative_ontology:affects_constraint(bedouin_sedentary_transition, arabian_labor_market_segmentation).

% DUAL FORMULATION NOTE:
% The sedentary transition is upstream of three resource-governance constraints: water management systems (built on assumption of fixed populations), pastoral commons enclosure (enabled by sedentarization of formerly mobile claimants), and labor market segmentation (Bedouin populations channeled into low-mobility employment). Each downstream constraint has its own extractiveness values reflecting its specific institutional mechanism; the sedentary transition story captures the foundational mechanism that enables all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bedouin_sedentary_transition, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
