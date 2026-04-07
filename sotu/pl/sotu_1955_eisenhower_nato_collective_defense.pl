% ============================================================================
% CONSTRAINT STORY: sotu_1955_eisenhower_nato_collective_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1955_eisenhower_nato_collective_defense, []).

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
 *   constraint_id: sotu_1955_eisenhower_nato_collective_defense
 *   human_readable: NATO Collective Defense Integration with West German Participation (1955)
 *   domain: military/geopolitical/institutional
 *
 * SUMMARY:
 *   In the 1955 State of the Union address, President Eisenhower proposes
 *   integrating West Germany as an equal participant in NATO collective
 *   defense and command structure. This constraint operates at the
 *   intersection of geopolitical deterrence (Soviet expansion prevention),
 *   institutional integration (NATO unified command), and sovereignty
 *   transfer (German subordination to collective authority). The mechanism
 *   benefits NATO member states through increased military capacity, credible
 *   deterrent posture, and burden-sharing. It simultaneously extracts from
 *   West German sovereignty by subordinating strategic autonomy to collective
 *   command, binding German defense to American commitments, and creating
 *   risks of escalation through diffused nuclear decision-making authority.
 *   The constraint exhibits all characteristics of Tangled Rope: genuine
 *   coordination function (collective defense deters Soviet aggression),
 *   asymmetric extraction (German sovereignty costs exceed German security
 *   benefits in the short term), active enforcement (NATO command structure
 *   and force integration protocols), and suppression (limited exit options,
 *   military vulnerability to non-participation). The theater ratio is
 *   moderate (0.48-0.52) because the deterrent function is substantially
 *   credible — military integration and German rearmament do genuinely
 *   increase Western defensive capacity — but the institutional arrangement
 *   contains performative elements (ceremonial NATO councils, symbolic German
 *   equality masking U.S. strategic primacy, periodic reassurance displays to
 *   German and Soviet audiences).
 *
 * KEY AGENTS:
 *   - West German State: Primary victim (powerless/trapped) — sovereignty subordinated to NATO collective command; no exit without military vulnerability
 *   - West German Leadership: Secondary agent (moderate/constrained) — experiences mixed coordination-extraction; gains security benefit, pays in autonomy
 *   - NATO Member States: Primary beneficiary (institutional/arbitrage) — experience pure coordination; German participation increases collective capacity without disproportionate cost
 *   - United States: Primary beneficiary (institutional/arbitrage) — leverages German integration for extended influence and burden-shifting; controls alliance strategic doctrine
 *   - Soviet Union: Implicit external constraint (analytical/analytical) — the constraint is structured as deterrent against Soviet expansion; Soviet perspective is not directly modeled but informs constraint design
 *   - Western European Allies: Secondary beneficiary (moderate/constrained) — gain security but constrained by American-dominated alliance structure and German rearmament risks
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing Cold War institutional arrangements as immutable geopolitical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1955_eisenhower_nato_collective_defense, 0.52).
domain_priors:suppression_score(sotu_1955_eisenhower_nato_collective_defense, 0.58).
domain_priors:theater_ratio(sotu_1955_eisenhower_nato_collective_defense, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1955_eisenhower_nato_collective_defense, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1955_eisenhower_nato_collective_defense, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1955_eisenhower_nato_collective_defense, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1955_eisenhower_nato_collective_defense, tangled_rope).
narrative_ontology:human_readable(sotu_1955_eisenhower_nato_collective_defense, "NATO Collective Defense Integration with West German Participation (1955)").
narrative_ontology:topic_domain(sotu_1955_eisenhower_nato_collective_defense, "military/geopolitical/institutional").

domain_priors:requires_active_enforcement(sotu_1955_eisenhower_nato_collective_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1955_eisenhower_nato_collective_defense, nato_member_states).
narrative_ontology:constraint_beneficiary(sotu_1955_eisenhower_nato_collective_defense, united_states).
narrative_ontology:constraint_beneficiary(sotu_1955_eisenhower_nato_collective_defense, western_european_security_bloc).
narrative_ontology:constraint_victim(sotu_1955_eisenhower_nato_collective_defense, west_german_sovereignty).
narrative_ontology:constraint_victim(sotu_1955_eisenhower_nato_collective_defense, soviet_sphere_exclusion).
narrative_ontology:constraint_victim(sotu_1955_eisenhower_nato_collective_defense, european_escalation_risk).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEST GERMAN STATE (SNARE) — Structurally trapped within collective defense apparatus. Sovereignty is subordinated to NATO command structure; exit would mean military vulnerability and diplomatic isolation. Germany bears suppression through mandatory force integration and subordination to Allied councils. No genuine exit option: refusal to participate triggers Soviet aggression assumption and NATO sanctions. Maximum extraction from German sovereignty perspective.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_nato_collective_defense, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WEST GERMAN LEADERSHIP (TANGLED ROPE) — Constrained but not trapped. Participation provides genuine security benefit (Soviet deterrence) and legitimacy through NATO membership, but at cost of strategic autonomy and subordination to collective command. Leadership coordinates with Allied structure while bearing asymmetric extraction (cannot unilaterally set defense doctrine, nuclear strategy, or force deployment). Mixed experience: benefits from security umbrella, pays in sovereignty.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_nato_collective_defense, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATO MEMBER STATES (ROPE) — Experiences constraint as pure coordination mechanism. German rearmament and integration solve collective action problem: distributed military capacity, burden-sharing, credible deterrent against Soviet expansion. No meaningful extraction experienced — German participation subsidizes collective defense without disproportionate cost to existing members. Exit options abundant (other alliance formations, nuclear deterrence); German membership is chosen coordination.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_nato_collective_defense, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UNITED STATES (ROPE) — Experiences constraint as coordinating mechanism aligned with U.S. strategic interests. German integration extends American security perimeter, multiplies military capacity, and locks European allies into American leadership. Low suppression from U.S. perspective — the constraint functions as intended coordination. U.S. has exit option (withdrawal from NATO, unilateral deterrence) but exercises arbitrage (leverages German participation for extended influence).
constraint_indexing:constraint_classification(sotu_1955_eisenhower_nato_collective_defense, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DETERRENCE EQUILIBRIUM VIEW (SCAFFOLD) — From the perspective of Cold War stability analysis, NATO collective defense is a temporary coordination mechanism with an implicit sunset: mutual nuclear parity and spheres-of-influence agreements (Helsinki Accords 1975, détente logic) will eventually replace institutional integration with power-balance equilibrium. The scaffolding (military integration) is seen as temporary deterrent framework that will evolve or dissolve as nuclear stalemate becomes institutionalized. Theater relatively low — actual deterrent function is credible.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_nato_collective_defense, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: WESTERN EUROPEAN ALLIES (TANGLED ROPE) — France, UK, Belgium experience German rearmament as both coordinating mechanism (increases collective defense capacity) and extractive mechanism (raises risk of German-initiated escalation, constrains independent European diplomacy, subordinates European strategy to American Cold War doctrine). Constrained exit — leaving NATO is possible but costly. Moderate experienced extraction from alliance asymmetries.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_nato_collective_defense, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, the constraint appears immutable: any great power with geographic proximity to rival great power must either integrate with stronger alliance (subordinating sovereignty) or face military inferiority. The 'natural law' framing treats the constraint as an inevitable structural fact of great-power competition. However, the identifiable beneficiaries (NATO states, U.S.) and structured enforcement mechanism contradict the mountain classification — the engine's false-summit detector will identify this as naturalization of contingent Cold War institutional arrangements.
constraint_indexing:constraint_classification(sotu_1955_eisenhower_nato_collective_defense, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1955_eisenhower_nato_collective_defense_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1955_eisenhower_nato_collective_defense, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1955_eisenhower_nato_collective_defense, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1955_eisenhower_nato_collective_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1955_eisenhower_nato_collective_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. German participation in NATO involves genuine security coordination (deterrence is real) but coupled with permanent subordination of strategic autonomy. Extractiveness is not as high as pure coercion (Snare would be 0.66+) because Germany does benefit from collective defense framework and NATO membership provides legitimacy and economic integration prospects. The value reflects that coordination benefits partially offset sovereignty extraction. Theater ratio (0.48): Moderate. The deterrent mechanism is functionally credible — integrated forces do increase defensive capacity — but NATO's institutional arrangements contain performative elements (ceremonial councils, symbolic equality, reassurance displays). The ratio declines over the interval (0.52 → 0.42) as actual German military integration deepens and theater gives way to functional military coordination. Suppression (0.58): Moderate-high. Germany faces significant barriers to independent strategic action — military rearmament outside NATO framework is prohibited by Western occupation logic, exit from NATO would trigger Soviet aggression assumption, and command integration creates functional suppression of unilateral decision-making. However, suppression is not total (0.60+) — Germany retains voice in NATO councils and some ability to resist directives through diplomatic channels.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. NATO member states (including the U.S.) perceive pure coordination (Rope) — collective defense solves the burden-sharing problem and deterrence credibly. West German leadership perceives mixed coordination-extraction (Tangled Rope) — genuine security benefit coupled with sovereignty costs. The West German state (as abstract victim of sovereignty loss) perceives pure extraction (Snare) — subordination to collective command with no exit. The scaffold perspective (deterrence equilibrium view) treats the institutional integration as temporary scaffolding pending Cold War equilibrium stabilization (détente, spheres of influence, nuclear parity). The analytical observer risks naturalizing this contingent Cold War arrangement as an immutable geopolitical law — that proximity to rival great power necessitates sovereignty subordination. The false-summit signature fires because identifiable beneficiaries (NATO states) and structured enforcement mechanisms (NATO command, force integration treaties) contradict the naturalness claim.
 *
 * DIRECTIONALITY LOGIC:
 *   NATO beneficiaries experience low directionality (d ≈ 0.15-0.20) because they benefit from German participation without bearing corresponding costs. German leadership experiences moderate directionality (d ≈ 0.55-0.60) reflecting genuine security benefit (deterrence) offset by sovereignty extraction (command subordination). West Germany as sovereignty-bearing entity experiences high directionality (d ≈ 0.90), treating the constraint as nearly pure extraction — the security benefit accrues to 'the West' as abstract collective, not to German strategic agency. The U.S. as alliance architect experiences very low directionality (d ≈ 0.10) — the constraint serves American strategic interests through burden-sharing and extended influence. No directionality overrides are necessary; the structural derivation from beneficiary/victim declarations produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint exhibits the defining characteristics required for Tangled Rope classification: (1) Genuine coordination function — NATO collective defense does solve the deterrence and burden-sharing problem; (2) Asymmetric extraction — German sovereignty is subordinated while NATO members' strategic autonomy is preserved; (3) Active enforcement — NATO treaty commitments, force integration protocols, and American military presence enforce the arrangement. The mandatrophy does not arise here because the coordination function and extraction mechanism are structurally distinct and both genuine. German participation solves the collective action problem (free-riding on U.S. security umbrella) while simultaneously extracting German sovereignty (subordination to collective command). The constraint is not mislabeled as coordination (which would hide extraction) — it is correctly classified as hybrid. The key analytical distinction is that German participation is 'voluntary' under Cold War geopolitical pressure (exit is militarily irrational) but not 'free' in the sovereignty sense (strategic autonomy is permanently transferred). This is the signature of Tangled Rope: coordination that functions only because suppressed alternatives and asymmetric costs keep the target locked in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    german_escalation_risk_threshold,
    'What level of German autonomous military decision-making constitutes unacceptable escalation risk to NATO and Soviet deterrence stability?',
    'Historical analysis of German command authority disputes within NATO (e.g., nuclear release protocols, force deployment decisions); comparison of actual German autonomous actions vs. predicted escalation scenarios',
    'If threshold is very restrictive: German sovereignty constraint is high (more snare-like). If threshold is permissive: German agency increases, constraint shifts toward tangled rope or rope. Changes perceived extraction asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(german_escalation_risk_threshold, empirical, 'Threshold for German military autonomy that NATO tolerates').

omega_variable(
    soviet_sphere_collapse_mechanism,
    'Is Soviet sphere collapse (1989-1991) evidence that NATO integration succeeded in deterring expansion, or that the deterrent was redundant to internal Soviet decay?',
    'Counterfactual analysis: comparison of Soviet behavior toward other non-NATO neighbors (Finland, Yugoslavia, Afghanistan) vs. NATO members; modeling of alternative scenarios without German rearmament',
    'If NATO integration was causal to deterrence: constraint classification as Rope/Tangled Rope is validated. If collapse was driven by internal Soviet factors: deterrent function was theater (Piton). Affects evaluation of whether extraction was justified by genuine security benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(soviet_sphere_collapse_mechanism, conceptual, 'Whether NATO integration caused Soviet deterrence or was redundant to Soviet collapse').

omega_variable(
    sovereignty_transfer_permanence,
    'Is German subordination to NATO collective command a permanent transfer of sovereignty or a temporary institutional arrangement pending European federation?',
    'Trajectory analysis: does German voice in NATO councils increase over time (suggesting temporary subordination) or remain static (suggesting permanent transfer)? Analysis of Treaty of Rome (1957) and subsequent European integration logic vs. NATO sovereignty retention.',
    'If permanent: extraction on German sovereignty is chronic (Snare classification justified). If temporary: constraint is Scaffold-like with implicit sunset tied to European political integration. Affects long-term classification trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_transfer_permanence, preference, 'Whether German NATO subordination is permanent or transitional').

omega_variable(
    american_commitment_credibility_paradox,
    'Does integrating German forces into NATO command structure increase or decrease credibility of American nuclear umbrella commitment? (U.S. loses independent decision-making authority; Germany gains but Allied credibility may suffer if decision-making becomes diffused.)',
    'Analysis of strategic documents, force deployment decisions, and crisis behavior (Berlin Blockade, Cuban Missile Crisis analogues) showing whether collective command structure was invoked or bypassed in actual contingencies',
    'If integration increases credibility: benefits NATO members'' security payoff and justifies extraction costs. If integration decreases credibility: constraint is more extractive (moves toward Snare) because deterrence function is weakened while sovereignty costs persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(american_commitment_credibility_paradox, empirical, 'Whether NATO integration increases or decreases deterrent credibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1955_eisenhower_nato_collective_defense, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nato_cd_tr_t0, sotu_1955_eisenhower_nato_collective_defense, theater_ratio, 0, 0.52).
narrative_ontology:measurement(nato_cd_tr_t3, sotu_1955_eisenhower_nato_collective_defense, theater_ratio, 3, 0.48).
narrative_ontology:measurement(nato_cd_tr_t6, sotu_1955_eisenhower_nato_collective_defense, theater_ratio, 6, 0.42).

% Extraction over time
narrative_ontology:measurement(nato_cd_be_t0, sotu_1955_eisenhower_nato_collective_defense, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(nato_cd_be_t3, sotu_1955_eisenhower_nato_collective_defense, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(nato_cd_be_t6, sotu_1955_eisenhower_nato_collective_defense, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1955_eisenhower_nato_collective_defense, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1955_eisenhower_nato_collective_defense, warsaw_pact_formation_1955).
narrative_ontology:affects_constraint(sotu_1955_eisenhower_nato_collective_defense, european_defense_community_failure).
narrative_ontology:affects_constraint(sotu_1955_eisenhower_nato_collective_defense, berlin_blockade_resolution_logic).
narrative_ontology:affects_constraint(sotu_1955_eisenhower_nato_collective_defense, nuclear_deterrence_credibility_structure).

% DUAL FORMULATION NOTE:
% NATO collective defense exists in structural relationship with Soviet sphere formation (Warsaw Pact) and competing European integration proposals (EDC). The constraint is downstream of Berlin Blockade crisis (which demonstrated need for credible Western deterrent) and upstream of nuclear strategy debates (which depend on NATO force integration). Each linked constraint has its own extractiveness value: Warsaw Pact (ε=0.58, symmetric to NATO), EDC failure (ε=0.25, Rope coordination abandoned), nuclear strategy (ε=0.65, pure extraction risk).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1955_eisenhower_nato_collective_defense, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
