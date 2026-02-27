% ============================================================================
% CONSTRAINT STORY: ibm_shield_contract_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ibm_shield_contract_2026, []).

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
 *   constraint_id: ibm_shield_contract_2026
 *   human_readable: IBM SHIELD IDIQ Program Contract
 *   domain: technological/political
 *
 * SUMMARY:
 *   The IBM SHIELD IDIQ contract represents a structural constraint emerging
 *   from the intersection of military modernization imperatives,
 *   technological lock-in through classified specifications, and the
 *   compression of human decision authority below the timescale required for
 *   deliberation. SHIELD is intended to automate the military OODA loop via
 *   AI-enabled sensor fusion and command recommendation, collapsing the
 *   decision cycle from minutes to seconds. This creates a constraint binding
 *   multiple actors with radically different structural positions: IBM
 *   benefits from an indefinite, minimally-competed contract; the US military
 *   command gains accelerated decision capability but loses human override
 *   capacity; allied nations depend on US system integration; human
 *   decision-makers face structural elimination; civilian populations in
 *   conflict zones experience accelerated targeting; and adversarial states
 *   face strategic asymmetry. The constraint exhibits all characteristics of
 *   a Tangled Rope: it has a genuine coordination function (unified
 *   operational picture, integrated sensor fusion) bundled inseparably with
 *   asymmetric extraction (loss of human authority, lock-in to IBM systems,
 *   escalation risk, suppression of oversight). Theater ratio has increased
 *   from 0.35 to 0.55 over the initial deployment window as congressional
 *   oversight (formally required by IDIQ rules) proves increasingly
 *   performative—briefings provide insufficient technical detail to enable
 *   genuine review, and classification barriers prevent meaningful
 *   deliberation. Extractiveness has risen correspondingly from 0.42 to 0.58
 *   as the lock-in tightens and deployment deepens.
 *
 * KEY AGENTS:
 *   - IBM Corporation: Primary beneficiary (institutional/arbitrage) — IDIQ contract with minimal re-compete pressure, system lock-in, classified specifications preventing competitor entry
 *   - US Military Command: Primary beneficiary and secondary victim (organized/constrained) — gains accelerated OODA loop (coordination benefit) but loses human override authority and decision autonomy (extraction)
 *   - Human Decision Authority: Primary victim (powerless/trapped) — nominal authority over targeting and escalation rendered structurally infeasible; no exit option
 *   - Civilian Populations in Conflict Zones: Primary victim (powerless/trapped) — experience accelerated targeting below deliberation thresholds; no agency or recourse
 *   - Adversarial State Actors: Secondary victim (moderate/constrained) — face strategic disadvantage; forced into expensive counter-response or inferiority
 *   - Allied Nations (NATO, Five Eyes): Secondary victim (moderate/constrained) — depend on US SHIELD integration; lose independent decision capacity in joint operations
 *   - Congressional Oversight: Nominal constraint (institutional/constrained) — maintains ritualistic notification authority but faces classification barriers and path dependency
 *   - International Legal/Norms Community: Emergent constraint (organized/constrained) — attempting to impose sunset conditions via AI governance frameworks and arms control norms
 *   - Defense Industrial Complex: Secondary beneficiary (powerful/mobile) — benefits from SHIELD procurement demand; faces lock-in to SHIELD architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ibm_shield_contract_2026, 0.58).
domain_priors:suppression_score(ibm_shield_contract_2026, 0.68).
domain_priors:theater_ratio(ibm_shield_contract_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ibm_shield_contract_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(ibm_shield_contract_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ibm_shield_contract_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ibm_shield_contract_2026, tangled_rope).
narrative_ontology:human_readable(ibm_shield_contract_2026, "IBM SHIELD IDIQ Program Contract").
narrative_ontology:topic_domain(ibm_shield_contract_2026, "technological/political").

domain_priors:requires_active_enforcement(ibm_shield_contract_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ibm_shield_contract_2026, ibm_corporation).
narrative_ontology:constraint_beneficiary(ibm_shield_contract_2026, us_military_command).
narrative_ontology:constraint_beneficiary(ibm_shield_contract_2026, defense_industrial_establishment).
narrative_ontology:constraint_victim(ibm_shield_contract_2026, human_decision_authority).
narrative_ontology:constraint_victim(ibm_shield_contract_2026, adversarial_state_actors).
narrative_ontology:constraint_victim(ibm_shield_contract_2026, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(ibm_shield_contract_2026, technological_sovereignty_of_allied_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HUMAN DECISION AUTHORITY (SNARE) — Military commanders and civilian policymakers nominally retain decision authority over targeting and escalation, but the SHIELD automation compresses decision windows below human cognition speed. The constraint operates by making human deliberation structurally impossible: OODA loop acceleration renders human override infeasible. Exit options: none. The human authority bearer is trapped in a system designed to bypass them.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: US MILITARY COMMAND STRUCTURE (TANGLED ROPE) — The military benefits from accelerated decision cycles and integrated sensor fusion (coordination function: unified operational picture). But the coordination comes bundled with extraction: loss of human override capacity, dependency on IBM systems for critical functions, escalation risk from compressed decision windows. Constrained exit: cannot abandon the system without strategic disadvantage, but can negotiate terms and maintain backup human decision paths. The constraint is both enabling and extractive.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: IBM CORPORATION (ROPE) — IDIQ contract structure (unlimited renewal, minimal competitive pressure, classified specifications preventing challenger entry) provides revenue certainty and technology lock-in. IBM's exit options are optimal: they can divest, renegotiate, or leverage the contract for adjacent markets. The constraint appears to IBM as coordination: they are solving the military's integration problem, earning systemically necessary rents for maintaining critical infrastructure. Net beneficiary with high agency.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERSARIAL STATE ACTORS (SNARE) — Competing military powers (China, Russia) face a structural disadvantage: if SHIELD proves effective, the US OODA loop becomes irreversibly faster, creating a strategic asymmetry. Exit option: expensive—develop equivalent autonomous systems or invest in counter-measures (cyber offense, jamming, deception). But the trap is real: the constraint (SHIELD acceleration) forces expensive response or strategic inferiority. Constrained but not freely mobile; extraction runs toward the US military's strategic advantage.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIED NATIONS (TANGLED ROPE) — Allies benefit from US military capability acceleration and integrated command networks (coordination). But the constraint embeds them in a US-dominated technological ecosystem: they depend on IBM systems for interoperability, lose independent decision capacity during integrated operations, and cede strategic autonomy to US command rhythm. Constrained exit: cannot easily develop parallel systems without breaking alliance integration. Extraction runs toward US technological dominance; coordination benefits are real but asymmetric.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: CIVILIAN POPULATIONS IN CONFLICT ZONES (SNARE) — SHIELD automation compresses targeting decision timescales below the human deliberation threshold required for proportionality assessment and civilian protection. Civilians in conflict-adjacent zones experience extraction: increased lethality from accelerated targeting, reduced opportunity for surrender/evacuation, higher collateral damage from compressed decision cycles. Exit option: none. No agency, no protection, no recourse. Maximal experienced extraction.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 7: DEFENSE INDUSTRIAL COMPLEX (TANGLED ROPE) — Broader defense industrial ecosystem (sensors, weapons platforms, communications, logistics) benefits from SHIELD integration demand (coordination: unified operational picture drives procurement across multiple suppliers). But the constraint extracts: creates lock-in to SHIELD architecture, forces standardization on IBM's platform, reduces margins for smaller vendors, accelerates arms race dynamics. Powerful actors with some mobility: can lobby, reshape contracts, develop alternatives. Extraction is real but negotiable.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: CONGRESSIONAL OVERSIGHT (PITON) — The IDIQ contract structure includes formal congressional notification and budgeting checkpoints (ritualistic oversight). But the check is largely performative: classified program scope prevents meaningful review, sunk costs create path dependency, military necessity framing preempts genuine deliberation. Congress maintains nominal authority but faces suppression (classification barriers) and theater (security theater preventing actual understanding). Theater ratio high; functional oversight low. Degraded constraint maintained through institutional inertia.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 9: INTERNATIONAL LEGAL/NORMS COMMUNITY (SCAFFOLD) — Human rights law (Geneva Conventions, IHL), arms control frameworks (UNAMA, OSCE), and emerging AI governance norms (EU AI Act, Beijing Framework) attempt to impose sunset conditions on autonomous weapon system deployment and decision compression. These constraints are temporary support structures: they lack enforcement mechanisms (no global authority), face pressure from national security exemptions, and depend on sustained coordination among signatories. Sunset clause: IF international consensus on meaningful autonomous weapon limitations matures (estimated 5-10 years), SHIELD's expansion may face contractual/legal limits. BUT if consensus fails, SHIELD persists unchecked.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a civilizational timescale, SHIELD's constraint could be reframed as an immutable law: the speed of light limits communication; entropy increases; adversaries adapt. The observer might argue that OODA loop acceleration is a structural imperative of military competition, not a contingent institutional choice. However, this mountain framing naturalizes what is actually a political choice: the decision to automate human decision authority, the contract structure that enables it, and the suppression of oversight are all contingent on institutions, not physics. Engine false summit detection should flag this.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ibm_shield_contract_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ibm_shield_contract_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ibm_shield_contract_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ibm_shield_contract_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ibm_shield_contract_2026, TR),
    TR >= 0.70.

:- end_tests(ibm_shield_contract_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts value at multiple points: IBM captures indefinite contract rents from a minimally-competitive IDIQ structure; the US military gains strategic advantage over adversaries; human decision authority is eliminated; allied nations lose autonomy; civilian populations experience accelerated lethality. The value 0.58 reflects high extractiveness constrained by the genuine coordination function (OODA loop integration is not pure overhead) and the fact that the US military does gain functional benefits alongside the extraction. If SHIELD were pure overhead with no real capability gain, extractiveness would exceed 0.70. Suppression (0.68): Significant institutional suppression mechanisms prevent alternatives: classified specifications block competitive entry; IDIQ contract structure minimizes re-compete pressure; classification barriers prevent congressional deliberation; national security framing preempts legal/normative challenge. But suppression is not absolute: alternatives exist (develop parallel systems, international legal constraints, technological counter-measures), making suppression 0.68 rather than 0.85+. Theater ratio (0.55): Congressional oversight maintains performative ritual (required briefings, notification procedures) but the ritual prevents genuine understanding due to classification depth and technical complexity. The theater has grown from 0.35 to 0.55 as deployment deepens and classification becomes more entrenched. At 0.55, the constraint is past the Piton threshold (0.70) for the oversight layer alone, but the overall constraint remains primarily Tangled Rope because the coordination function is substantial, not purely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radical perspectival divergence across structural positions. For IBM, it appears as pure coordination (Rope): they solve the military integration problem and earn systemically necessary rents. For the US military, it appears as mixed (Tangled Rope): real capability gains bundled with loss of human override. For allied nations, it appears as mixed extraction (Tangled Rope): coordination benefits offset by loss of autonomy. For human decision-makers, it appears as pure extraction (Snare): structural elimination of authority with no exit. For civilians in conflict zones, it appears as pure extraction (Snare): accelerated targeting below deliberation thresholds. For adversaries, it appears as strategic disadvantage (Snare): forced expensive counter-response. For congressional oversight, it appears as degraded ritual (Piton): nominal authority rendered performative by classification. For the international norms community, it appears as temporary problem (Scaffold): IF international consensus on autonomous weapons materializes, SHIELD faces legal sunset. The perspectival gap reveals that the constraint's classification depends entirely on the observer's structural position relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) captures each agent's structural position relative to the extraction mechanism. IBM experiences d ≈ 0.05 (full beneficiary + arbitrage exit) → f(d) ≈ -0.12 → negative χ, meaning the constraint subsidizes IBM. The US military experiences d ≈ 0.50-0.55 (both benefits and costs, constrained exit) → f(d) ≈ 0.65-0.75 → moderate χ. Human decision authority experiences d ≈ 0.95 (full victim, trapped exit) → f(d) ≈ 1.42 → maximum χ. Civilians in conflict zones experience d ≈ 0.95 (full victim, trapped exit) → f(d) ≈ 1.42 → maximum χ. Adversaries experience d ≈ 0.85 (victim with constrained exit options) → f(d) ≈ 1.15 → high χ. Scope modifier σ(S) applies: SHIELD operates at global scope (σ = 1.2), amplifying effective extraction—a global system is harder to escape than a local one, and the distributed nature makes verification of alternatives difficult. This scope amplification helps explain why extraction (0.58) persists despite the coordination function—the global scale makes alternatives structurally difficult to develop or coordinate.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint's classification as Tangled Rope (not pure Snare or Rope) is justified by the irreducible presence of BOTH coordination function AND asymmetric extraction. The coordination benefit is genuine and substantial—SHIELD does solve the real military problem of integrating distributed sensors into unified command (that is not theater, it is functional). But the coordination is inseparable from extraction: lock-in to IBM, loss of human override, escalation risk compression, and suppression of alternative paths. The mandatrophy arises from asking: 'Is this primarily a coordination solution that extracts as a side effect, or primarily an extraction mechanism that coordinates as cover?' The resolution is that the question is malformed. The constraint is structurally tangled—both functions are present and irreducibly coupled. Separating them would require fundamentally different system architecture (e.g., maintaining human override at all speeds, ensuring real competition for SHIELD services, allowing override of OODA loop acceleration). The Tangled Rope classification prevents the false naturalizing move (treating SHIELD acceleration as inevitable) and the false pure-extraction move (treating the coordination benefits as mere theater). The lived experience varies radically by perspective because the coupling is asymmetric: beneficiaries experience coordination; victims experience extraction; the coupling itself is the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_override_feasibility,
    'Can human operators realistically override SHIELD targeting decisions once the system is deployed at scale, or is human override technically infeasible once the OODA loop is compressed below ~2-second decision windows?',
    'Technical analysis of system architecture (is there a hard-stop override protocol?); operational testing of override latency under realistic combat stress; post-deployment incident analysis of human-system interaction',
    'If override is feasible: reduces from Snare (powerless agent perspective) to Tangled Rope (constrained but with agency). If override is infeasible: confirms Snare classification; human decision authority is structurally eliminated. This is the mandatrophy pivot: coordination vs elimination of human agency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_override_feasibility, empirical, 'Technical feasibility of human override of SHIELD targeting decisions').

omega_variable(
    idiq_competitive_dynamics,
    'Does the IDIQ contract structure with minimal re-compete windows create genuine lock-in (monopoly extraction), or can alternative vendors realistically develop competing systems that break IBM''s dominance within 5-10 years?',
    'Analysis of classified system specifications (are they truly proprietary or could they be reverse-engineered?); tracking of competitor R&D (defense contractors, foreign militaries, open-source projects); measurement of actual re-compete frequency and competitive pressure',
    'If lock-in is real: extraction ≥ 0.58 is sustainable. If competitive pressure emerges: extraction degrades toward 0.40-0.45, shifting from Tangled Rope toward Rope. This affects IBM''s perspective classification and the beneficiary/victim balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(idiq_competitive_dynamics, empirical, 'Long-term competitive dynamics of the IDIQ contract structure').

omega_variable(
    escalation_risk_from_compression,
    'Does SHIELD''s OODA loop compression actually increase the probability of accidental escalation, or does accelerated decision-making reduce human error and de-escalation failures?',
    'Game-theoretic analysis of decision compression under uncertainty; simulation of human-vs-autonomous decision sequences; historical analysis of escalation failures in past conflicts (did they result from slow decision cycles or poor information?)',
    'If compression increases escalation risk: extraction experienced by civilian populations and adversaries is validated; suppression mechanism is strategic instability. If compression reduces escalation: the constraint shifts from Snare (for civilians) toward Rope (coordination benefit for all). This is the core contested claim—whether speed is inherently safer or inherently more dangerous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_risk_from_compression, empirical, 'Whether OODA loop compression increases or decreases escalation risk').

omega_variable(
    international_norms_materialization,
    'Will international consensus on autonomous weapon system limitations materialize with teeth (enforcement, inspection, verification) before SHIELD reaches full global deployment, or will national security exemptions prevent binding constraints?',
    'Tracking of treaty negotiations (UNAMA, OSCE, UN GGE reports); measurement of state compliance with existing arms control frameworks; analysis of enforcement mechanisms in proposed AI governance frameworks',
    'If norms materialize: Scaffold perspective is validated; sunset clause has real force; extraction faces external limit. If norms fail: Scaffold is aspirational theater; extraction persists indefinitely; SHIELD remains unconstrained. This determines whether the constraint is temporary or permanent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_norms_materialization, preference, 'Whether international AI weapon limitations will achieve enforcement before SHIELD global deployment').

omega_variable(
    congressional_classification_authority,
    'Do existing congressional notification requirements for classified programs provide meaningful oversight authority, or is the classification system structured to prevent genuine deliberation?',
    'Analysis of congressional briefing protocols (how much technical detail is actually provided?); tracking of successful program terminations or restructurings by classified program oversight (how many have occurred in past 20 years?); interviews with cleared congressional staff on effective review capacity',
    'If congressional review is meaningful: Piton classification is incorrect; oversight provides some constraint on extraction. If review is theater: Piton is confirmed; the nominal oversight layer is a suppression mechanism itself (creating false legitimacy while preventing actual check). This affects the human_decision_authority victim classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_classification_authority, empirical, 'Effectiveness of congressional oversight of classified IDIQ contracts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ibm_shield_contract_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shield_tr_t0, ibm_shield_contract_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(shield_tr_t3, ibm_shield_contract_2026, theater_ratio, 3, 0.45).
narrative_ontology:measurement(shield_tr_t6, ibm_shield_contract_2026, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(shield_be_t0, ibm_shield_contract_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(shield_be_t3, ibm_shield_contract_2026, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(shield_be_t6, ibm_shield_contract_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ibm_shield_contract_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(ibm_shield_contract_2026, autonomous_weapons_international_law).
narrative_ontology:affects_constraint(ibm_shield_contract_2026, military_ooda_loop_acceleration).
narrative_ontology:affects_constraint(ibm_shield_contract_2026, congressional_classified_oversight).
narrative_ontology:affects_constraint(ibm_shield_contract_2026, allied_technological_sovereignty).

% DUAL FORMULATION NOTE:
% SHIELD is downstream of both military modernization imperatives (upstream: adversarial state military capability acceleration) and upstream of international autonomous weapons governance (downstream: legal/normative constraints on system deployment). The constraint's extractiveness is sensitive to which upstream/downstream constraints mature first: if international AI weapons limits emerge before full SHIELD deployment, the constraint faces real sunset (Scaffold case); if adversarial capability acceleration continues unchecked, extraction persists (Snare case for victims). The network links capture these interdependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ibm_shield_contract_2026, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
