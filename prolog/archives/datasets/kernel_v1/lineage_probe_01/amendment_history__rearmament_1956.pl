% ============================================================================
% CONSTRAINT STORY: amendment_history__rearmament_1956
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amendment_history__rearmament_1956, []).

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
 *   constraint_id: amendment_history__rearmament_1956
 *   human_readable: The 1956 Rearmament Amendments: Military Authority Within the Constitution
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The 1956 Bundeswehr rearmament amendments represent a radical
 *   constitutional reversal: ten years after the Basic Law (1949)
 *   constitutionalized demilitarization as a founding norm, Article 143
 *   (later Article 87a) authorized the creation of a military force under
 *   strict parliamentary control. This constraint is structurally distinct
 *   from other postwar rearmaments because it bound military authority within
 *   democratic procedure — the 'citizen-in-uniform' doctrine explicitly
 *   rejected Prussian militarism and totalitarian conscription, positioning
 *   the Bundeswehr as subordinate to elected oversight. Yet this
 *   subordination is precisely what enables the extraction: coercive military
 *   service is legitimized by democratic procedure, suppressing the pacifist
 *   founding premise through constitutional amendment rather than coup or
 *   decree. The constraint operates as genuine coordination (NATO alliance,
 *   collective security, shared defense) layered with asymmetric extraction
 *   (conscripts bear bodily obligation, pacifists bear political loss,
 *   beneficiaries gain sovereignty and security). The theater_ratio (0.48) is
 *   moderate: the constitutional procedure was substantive (real Bundestag
 *   debate, explicit provisions), not pure ritual, but the outcome was
 *   substantially shaped by geopolitical necessity (Cold War bipolarity)
 *   rather than democratic choice. The suppression trajectory (0.58→0.65)
 *   shows rising enforcement requirement as the military matured and
 *   conscription expanded to meet NATO force requirements.
 *
 * KEY AGENTS:
 *   - Conscript Citizens: Primary victims (powerless/trapped) — subject to mandatory military service, 18 months to 2+ years depending on branch and period, with criminal penalty for refusal. No exit except emigration or imprisonment.
 *   - Pacifist Constitutional Constituency: Secondary victim (moderate/constrained) — the founding demilitarization norm is suppressed; pacifism remains a live political position but constitutional path to demilitarization is closed. Can protest, organize, refuse service with consequences, but cannot prevent rearmament once constitutionalized.
 *   - West German Federal Executive: Primary beneficiary (institutional/arbitrage) — gains military capacity and sovereign defense authority previously held by Allied occupation. Regains Hoheitsrechte (state sovereignty) in security domain.
 *   - NATO Alliance: Primary beneficiary (institutional/arbitrage) — integrates West Germany into Atlantic alliance as security partner; obtains German forces for forward defense against Soviet threat. Cold War necessity rather than choice.
 *   - Bundestag Majority Coalition: Active enforcer (organized/mobile) — the SPD opposed rearmament; the CDU/CSU and FDP supported it. Vote was 227-202 (narrow majority), reflecting genuine constitutional conflict.
 *   - Peace Movements and Left Opposition: Organized resisters (organized/constrained) — mobilized massive protest ('Ohne Mich' — 'Without Me' campaign), but lacked veto power. Organizational constraint (could not prevent Bundestag vote) but had voice (parliamentary debate, street mobilization).
 *   - Allied Occupation Authority (USSR implied, Western Allies explicit): Structural enablers (institutional/arbitrage) — formally suspended reserved rights; geopolitical interest in Western rearmament as implicit condition. Theater actor: constitutional procedure conceals that rearmament was geopolitical requirement.
 *   - Analytical Observer: Structural analyst (analytical/analytical) — sees both the real constitutional coordination (democratic oversight, civil control) and the suppression of the founding pacifist premise.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amendment_history__rearmament_1956, 0.52).
domain_priors:suppression_score(amendment_history__rearmament_1956, 0.65).
domain_priors:theater_ratio(amendment_history__rearmament_1956, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amendment_history__rearmament_1956, extractiveness, 0.52).
narrative_ontology:constraint_metric(amendment_history__rearmament_1956, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(amendment_history__rearmament_1956, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amendment_history__rearmament_1956, tangled_rope).
narrative_ontology:human_readable(amendment_history__rearmament_1956, "The 1956 Rearmament Amendments: Military Authority Within the Constitution").
narrative_ontology:topic_domain(amendment_history__rearmament_1956, "political/legal/constitutional").

domain_priors:requires_active_enforcement(amendment_history__rearmament_1956).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(amendment_history__rearmament_1956, '23455cc3-a7d1-418f-bfe4-2d82da4f74c0').
narrative_ontology:cs_kernel_codification('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', formalized).
narrative_ontology:cs_authority_grounding('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', lineage).
narrative_ontology:cs_interpretation_layer_present('23455cc3-a7d1-418f-bfe4-2d82da4f74c0').
narrative_ontology:cs_reading_relation('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', amendment_history__asylum_compromise_1993, coexists_with).
narrative_ontology:cs_reading_relation('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', amendment_history__debt_brake_2009, coexists_with).
narrative_ontology:cs_reading_relation('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', amendment_history__emergency_acts_1968, influences).
narrative_ontology:cs_reading_relation('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', amendment_history__reunification_amendments_1990, influences).
narrative_ontology:cs_axiom('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', foundational, amendment_authority_overrides_founding_norms).
narrative_ontology:cs_axiom_status(amendment_authority_overrides_founding_norms, holdable).
narrative_ontology:cs_axiom_grounding('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', amendment_authority_overrides_founding_norms, deontological).
narrative_ontology:cs_axiom('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', foundational, external_pressure_justifies_foundational_suppression).
narrative_ontology:cs_axiom_status(external_pressure_justifies_foundational_suppression, holdable).
narrative_ontology:cs_axiom_grounding('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', external_pressure_justifies_foundational_suppression, instrumental).
narrative_ontology:cs_reference_frame('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', demilitarized_constitutional_foundation).
narrative_ontology:cs_drift_state('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', cold_war_bipolarity_onset, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('23455cc3-a7d1-418f-bfe4-2d82da4f74c0', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(amendment_history__rearmament_1956, amendment_history).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amendment_history__rearmament_1956, nato_integration).
narrative_ontology:constraint_beneficiary(amendment_history__rearmament_1956, federal_executive_defense_capacity).
narrative_ontology:constraint_beneficiary(amendment_history__rearmament_1956, west_german_state_sovereignty).
narrative_ontology:constraint_victim(amendment_history__rearmament_1956, pacifist_constitutionalism).
narrative_ontology:constraint_victim(amendment_history__rearmament_1956, conscript_citizens).
narrative_ontology:constraint_victim(amendment_history__rearmament_1956, demilitarization_founding_norm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPT CITIZEN (SNARE) — No exit from military service obligation. The extraction is maximum: bodily obligation, loss of years, potential death, all determined by state decree hidden behind parliamentary procedure. Trapped at both immediate and biographical horizons. The constitutional language ('citizen-in-uniform') masks coercive extraction — the consent is formal (Bundestag vote) but substantive exit is foreclosed.
constraint_indexing:constraint_classification(amendment_history__rearmament_1956, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PACIFIST CONSTITUTIONALISM (TANGLED ROPE) — At generational time, the constitutionalized prohibition on rearmament has coordination function (pacifism as shared founding norm) AND is breached by extraction (the 1956 amendments suppress that norm). Constrained exit: pacifists could emigrate or refuse service, but both carry severe costs. The extractiveness is not maximal because the constraint does offer procedural participation (Bundestag debate) and the military is parliamentary-controlled, not autonomous.
constraint_indexing:constraint_classification(amendment_history__rearmament_1956, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATO INTEGRATION (ROPE) — Benefits from West German rearmament as essential to Atlantic alliance. Experiences the constraint as coordination: the Bundeswehr solves the shared-defense problem within NATO's multilateral framework. The 'citizen-in-uniform' doctrine and parliamentary oversight are genuine coordination benefits — they differentiate West German rearmament from Prussian militarism and embed military authority in democratic procedure. Arbitrage available: NATO states can redirect security partnerships if needed, but rearmament cements West Germany's role. Net beneficiary perspective.
constraint_indexing:constraint_classification(amendment_history__rearmament_1956, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FEDERAL EXECUTIVE (ROPE) — Gains defense capacity and state sovereignty. The amendments grant the Bundestag budgetary control and command authority, but the executive gains operational capacity previously denied by Allied occupation. The extraction runs toward this agent: rearmament amplifies federal power. At biographical horizon, the constraint is perceived as changeable if political will shifts — the executive can advocate for demilitarization in principle, though geopolitical pressure makes this hypothetical. Arbitrage possible through NATO or European alternatives, but rearmament locks in a preferred equilibrium.
constraint_indexing:constraint_classification(amendment_history__rearmament_1956, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PEACE MOVEMENTS (TANGLED ROPE) — Organized but structurally vulnerable. The constraint has coordination function for them (collective pacifist identity, activism networks), but the rearmament amendments suppress their founding premise. Constrained exit: they can protest, organize, and contest policy through electoral politics, but cannot prevent rearmament once constitutional. The extractiveness is moderate because parliamentary procedure does create a venue for voice, and the military is subject to constitutional oversight rather than autonomous — this is genuinely less extraction than totalitarian rearmament would be. But suppression is high: the constitutional path to demilitarization is effectively closed for a generation.
constraint_indexing:constraint_classification(amendment_history__rearmament_1956, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ALLIED AUTHORITY (PITON) — Formally suspends reserved rights by ceding authority to West German constitutional process, but the constraint is substantially performative: the Allies' geopolitical interest in Western rearmament IS the actual driver, and the constitutional procedure ratifies a decision made in Cold War geopolitics. The theater_ratio is high because the procedural machinery (Bundestag votes, constitutional debate) obscures the structural fact that rearmament was NATO requirement. The Allied authority degrades in this reading — it transforms from occupation command to background Cold War necessity — and the German constitution performs legitimacy for a decision made elsewhere.
constraint_indexing:constraint_classification(amendment_history__rearmament_1956, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL INEVITABILITY (MOUNTAIN) — From civilizational scope, Cold War bipolarity made rearmament structurally inevitable: Germany's geographic position between Soviet and Western blocs made neutrality impossible. No Western state could credibly remain demilitarized under Soviet threat. This perspective naturalizes rearmament as an immutable consequence of geopolitical structure. However, the beneficiary/victim structure contradicts the mountain classification — the NATO beneficiaries, executive, and peace movements form a real distributional conflict. The engine's false summit detector will identify this as naturalization of a political choice masquerading as geopolitical necessity.
constraint_indexing:constraint_classification(amendment_history__rearmament_1956, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amendment_history__rearmament_1956_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amendment_history__rearmament_1956, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amendment_history__rearmament_1956, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(amendment_history__rearmament_1956, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(amendment_history__rearmament_1956, TR),
    TR >= 0.70.

:- end_tests(amendment_history__rearmament_1956_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts coercive military service from conscripts at maximum rate (no exit, bodily obligation, years of service), but distributes extraction across society rather than concentrating it on a narrow target. The extraction is partially legitimate (compensated service, collective benefit) and partially coercive (no genuine choice, refusal criminalized). The suppression of pacifist constitutionalism adds political extraction — those committed to demilitarization as a founding principle lose that commitment through constitutional amendment. NATO integration is the beneficiary flow that justifies the extraction: the extraction is narrated as necessary for security, alliance membership, and sovereignty recovery. Suppression (0.65): High. Multiple barriers prevent exit: (a) conscription law makes refusal criminal; (b) emigration is costly and rare; (c) conscientious objection paths exist (Zivildienst — civilian service) but are narrowly construed and considered shameful by military culture; (d) the constitutional amendment closes the pacifist political path — demilitarization is no longer a defensible constitutional position. Theater ratio (0.48): Moderate. The constitutional procedure is substantive (real Bundestag debate, explicit provisions, parliamentary controls exist), but the outcome is driven by Cold War geopolitical necessity rather than authentic democratic choice. The 'citizen-in-uniform' doctrine is partly real (democratic controls, civil oversight) and partly performative (masks the coercive reality of conscription by narrating it as shared civic duty). The Bundestag voting was real but constrained by NATO pressure — a democratic choice, but not a free one. Rising suppression (0.58→0.65) as Bundeswehr matured reflects increasing enforcement capacity: military infrastructure grew, conscription became normalized, refusal became socially stigmatized, career incentives locked citizens into service.
 *
 * PERSPECTIVAL GAP:
 *   The rearmament constraint exhibits maximum perspectival divergence. Conscripts see pure extraction (Snare) — bodily obligation with no exit. Peace movements see mixed coordination and extraction (Tangled Rope) — their organizing principle (pacifism) is suppressed, but they retain voice through electoral politics. The federal executive sees pure coordination (Rope) — rearmament solves the defense capacity problem within NATO framework, yielding sovereignty. NATO sees pure coordination (Rope) — rearmament integrates West Germany into collective security. The Allied authority sees performative ritual (Piton) — constitutional procedure legitimizes a geopolitical decision made outside the constitutional process. The analytical observer risks naturalizing as immutable law (Mountain) what is actually a contingent response to Cold War structure. The perspectival gaps trace directly to the distributional conflict: conscripts bear extraction; beneficiaries capture security and sovereignty gains; coordination is real (democratic control) but incomplete (pacifist premise suppressed). The constraint resolves mandatrophy only by recognizing that all perspectives are simultaneously true — the same constitutional framework is coordination for the executive, extraction for conscripts, theater for Allies, suppression for pacifists.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is derived from base extractiveness (0.52), their power level, their exit options, and the beneficiary/victim status. Conscripts: powerless/trapped → d ≈ 0.95 → f(d) ≈ 1.42 → χ ≈ 0.74 (maximum experienced extraction). Federal Executive: institutional/arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 → χ ≈ -0.06 (negative extraction; beneficiary). Peace Movements: organized/constrained → d ≈ 0.50 → f(d) ≈ 0.65 → χ ≈ 0.34 (moderate extraction). NATO: institutional/arbitrage → d ≈ 0.10 → f(d) ≈ -0.05 → χ ≈ -0.03 (net beneficiary). Scope modifier σ(S=national) = 1.0, so no additional scaling. The directionality derivation confirms the perspectival gap: conscripts experience nearly three times the extractiveness of organized resisters, because their structural position offers zero degrees of freedom.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demilitarization_founding_status,
    'Was the 1949 demilitarized founding premise a foundational constitutional commitment or a provisional Allied imposition pending geopolitical change?',
    'Constitutional history analysis: examine Basic Law drafting debates, Allied influence on demilitarization clause, explicit statements about the clause''s permanence or revocability. Compare deliberative intent (whether drafters saw demilitarization as indefinite or conditional on security environment).',
    'If foundational: rearmament is constitutional betrayal requiring higher justification (mandatrophy case). If provisional: rearmament is legitimate response to changed conditions, reducing extraction classification for moderate/organized agents. Classification shifts from Snare/Tangled Rope to Rope for many perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demilitarization_founding_status, conceptual, 'Whether demilitarization was foundational or provisional').

omega_variable(
    parliamentary_control_real_vs_formal,
    'Does parliamentary control (Bundestag budget authority, command oversight) constitute genuine structural constraint on military autonomy, or is it performative ritual concealing executive military prerogative?',
    'Institutional analysis: trace actual Bundestag interventions in Bundeswehr operations (budget vetoes, command decisions overturned, deployments blocked). Compare formal authority with historical military autonomy claims and ALG (Auftragstak Taktik — mission command doctrine). Measure frequency and magnitude of parliamentary constraint vs. executive acquiescence.',
    'If real constraint: theater_ratio drops to ~0.35, classification shifts to Rope for many perspectives. If performative: theater_ratio rises to ~0.65, classification solidifies as Snare/Tangled Rope. Suppression interpretation changes fundamentally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliamentary_control_real_vs_formal, empirical, 'Whether Bundestag control is real or performative').

omega_variable(
    citizen_uniform_doctrine_binding_force,
    'Does the ''citizen-in-uniform'' constitutional doctrine functionally prevent militarism and democratic breakdown, or is it rhetorical cover for standard conscript military authority?',
    'Doctrinal and institutional analysis: examine whether citizen-in-uniform principle constrains Bundeswehr autonomy, prevents coup risk, enforces democratic loyalty better than comparable militaries. Evaluate through: (a) coup risk indices, (b) military intervention in civilian politics, (c) adoption of the doctrine by other postwar democracies, (d) scholarly consensus on its preventive force.',
    'If binding: extraction classification for conscripts drops; mountain perspective becomes more plausible. If rhetorical: extraction remains high; suppression is more severe because the doctrine masks coercion. Mandatrophy status depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_uniform_doctrine_binding_force, empirical, 'Binding force of citizen-in-uniform doctrine').

omega_variable(
    alternative_defense_structures_available,
    'Could West Germany have achieved NATO membership and geopolitical security in 1956 without military conscription, using volunteer professional forces or collective security arrangements short of rearmament?',
    'Counterfactual institutional analysis: examine NATO alternatives available in 1956 (forward deployment of Allied forces, extended deterrence without German rearmament, volunteer Bundeswehr models tested elsewhere). Assess whether Soviet threat made conscription logically necessary or whether it reflected military doctrine preference and resource constraints.',
    'If alternatives existed: rearmament is contingent political choice, not structural necessity. Classification shifts toward higher extraction for conscripts, mandatrophy for the constraint. If rearmament was necessary: classification remains moderate extraction, mountain perspective gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_defense_structures_available, conceptual, 'Whether military conscription was necessary or contingent').

omega_variable(
    contested_kernel_reading_status,
    'This constraint is one reading of the amendment_history kernel. How does rearmament_1956 relate structurally to the sibling readings (asylum_compromise_1993, debt_brake_2009, emergency_acts_1968, reunification_amendments_1990)? Do these readings coexist as live partisan positions, or does one reading''s logic foreclose another''s?',
    'Constitutional interpretation history: trace which readings are held simultaneously by different political coalitions. Identify whether any reading (e.g., pacifism) logically excludes another (e.g., NATO membership), or whether both remain live options defended by different parties. Examine whether doctrine drift (authority_erosion, practice_drift) affects how readings evolve over time.',
    'If coexist: multiple readings remain live constraints, each with its own classification and omega structure. If forecloses: the rearmament reading logically rules out pacifist readings within a single coherent framework. Affects how the engine computes reading_relations in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_reading_status, conceptual, 'Logical relationship between rearmament_1956 and sibling kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amendment_history__rearmament_1956, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rearm56_theater_t0, amendment_history__rearmament_1956, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rearm56_theater_t2, amendment_history__rearmament_1956, theater_ratio, 2, 0.5).
narrative_ontology:measurement(rearm56_theater_t5, amendment_history__rearmament_1956, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(rearm56_extract_t0, amendment_history__rearmament_1956, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rearm56_extract_t2, amendment_history__rearmament_1956, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(rearm56_extract_t5, amendment_history__rearmament_1956, base_extractiveness, 5, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(rearm56_suppress_t0, amendment_history__rearmament_1956, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(rearm56_suppress_t2, amendment_history__rearmament_1956, suppression_requirement, 2, 0.62).
narrative_ontology:measurement(rearm56_suppress_t5, amendment_history__rearmament_1956, suppression_requirement, 5, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amendment_history__rearmament_1956, enforcement_mechanism).
narrative_ontology:affects_constraint(amendment_history__rearmament_1956, amendment_history__emergency_acts_1968).
narrative_ontology:affects_constraint(amendment_history__rearmament_1956, amendment_history__reunification_amendments_1990).

% DUAL FORMULATION NOTE:
% The rearmament_1956 constraint is embedded in the amendment_history kernel along with four sibling readings. Each reading represents a different interpretation of what the Basic Law permits when exterior pressures (Cold War security, refugee flows, fiscal discipline, state emergency, national reunification) collide with founding principles (demilitarization, open borders, fiscal independence, civil liberties). The rearmament reading is upstream of emergency_acts_1968 (both suppress pacifist/libertarian premises through constitutional procedure) and conjoint with reunification_amendments_1990 (both use amendment procedures to absorb external pressures). Decomposing these readings into separate constraint stories reveals that the amendment_history kernel does not have one ε — each reading has its own extractiveness based on what gets suppressed, who bears the cost, and what structure gets embedded in the constitution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amendment_history__rearmament_1956, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
