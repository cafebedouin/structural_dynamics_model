% ============================================================================
% CONSTRAINT STORY: autocratic_rule__military_junta
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_autocratic_rule__military_junta, []).

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
 *   constraint_id: autocratic_rule__military_junta
 *   human_readable: Military Junta Rule: Crisis Autocracy
 *   domain: political/comparative/autocratic_governance
 *
 * SUMMARY:
 *   Military junta rule represents autocracy's crisis form: a committee of
 *   officers governing under the claim of standing above factional politics,
 *   justified by emergency and national necessity. The junta dissolves
 *   civilian political institutions (parties, unions, legislatures) and
 *   consolidates military institutional power and budgets. The constraint
 *   exhibits tangled rope structure: the junta genuinely coordinates some
 *   state functions (continuity of bureaucratic services, tax collection,
 *   infrastructure maintenance) while extracting massive institutional
 *   benefits for the officer corps (budget expansion, autonomous rule without
 *   civilian oversight, elimination of rival power centers). Extractiveness
 *   (0.58) reflects asymmetric distribution: the officer corps gains
 *   institutional power and budget authority; civilians face wholesale
 *   suppression of political activity. Suppression (0.78) is structural and
 *   pervasive: assembly banned, media censored, leadership imprisoned, unions
 *   dissolved. Theater ratio (0.65) indicates that the junta's legitimating
 *   narratives (emergency, national unity, standing above faction) are
 *   substantially performed rather than operationally true — juntas routinely
 *   maintain proclaimed emergencies indefinitely, suppress evidence of
 *   internal factional conflict, and extract patronage benefits while
 *   claiming unity. The constraint is one reading of a contested kernel
 *   (autocratic rule) that has multiple stable forms: hereditary monarchy
 *   (stable succession via blood), military junta (crisis form via officer
 *   committee), party autocracy (organized form via disciplined apparatus),
 *   and personalist dictatorship (modern form via one ruler above
 *   institutions). This reading differs from its siblings in the mechanism of
 *   legitimation (emergency + military unity claim), the beneficiary set
 *   (officer corps specifically, not hereditary line or party elite), and the
 *   victim set (wholesale suppression of civilian order, not succession wars
 *   or party discipline).
 *
 * KEY AGENTS:
 *   - Officer Corps: Primary beneficiary (institutional/arbitrage) — gains institutional autonomy, budget expansion, and political monopoly; experiences rule as coordination and power consolidation
 *   - Civilian Political Parties: Primary victim (powerless/trapped) — banned or subordinated, leadership imprisoned, cannot operate politically; wholesale suppression with no exit short of exile
 *   - Organized Labor / Unions: Secondary victim (powerless/trapped) — dissolved or state-controlled, leaders arrested, strike activity prohibited; structural suppression via institutional dismantling
 *   - Professional Civil Service: Secondary beneficiary and victim (moderate/constrained) — retains employment and coordination functions under junta authority, but must accept political subordination and comply with junta directives
 *   - Junta Legitimacy Apparatus: Institutional actor (institutional/constrained) — maintains emergency claim and unity narrative through media control and suppression of counternarratives; constrains its own actors to perpetuate the claim
 *   - Non-Military Civil Society: Victim (moderate/constrained) — NGOs, churches, professional associations face restrictions on independence; some coordination functions continue under junta oversight
 *   - International State Actors: Mixed (powerful/mobile to constrained) — face tradeoff between stability benefits (reduced revolutionary risk, predictable military-led governance) and extraction costs (human rights violations, democratic regression, sanctions risk)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(autocratic_rule__military_junta, 0.58).
domain_priors:suppression_score(autocratic_rule__military_junta, 0.78).
domain_priors:theater_ratio(autocratic_rule__military_junta, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(autocratic_rule__military_junta, extractiveness, 0.58).
narrative_ontology:constraint_metric(autocratic_rule__military_junta, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(autocratic_rule__military_junta, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(autocratic_rule__military_junta, tangled_rope).
narrative_ontology:human_readable(autocratic_rule__military_junta, "Military Junta Rule: Crisis Autocracy").
narrative_ontology:topic_domain(autocratic_rule__military_junta, "political/comparative/autocratic_governance").

domain_priors:requires_active_enforcement(autocratic_rule__military_junta).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(autocratic_rule__military_junta, 'a63261a0-41a4-4fe7-94eb-c1c38b2a3c82').
narrative_ontology:cs_kernel_codification('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', implicit).
narrative_ontology:cs_authority_grounding('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', extraction).
narrative_ontology:cs_reading_relation('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', autocratic_rule__hereditary_monarchy, coexists_with).
narrative_ontology:cs_reading_relation('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', autocratic_rule__party_autocracy, influences).
narrative_ontology:cs_reading_relation('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', autocratic_rule__personalist_dictatorship, influences).
narrative_ontology:cs_axiom('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', foundational, military_committee_rule_above_faction).
narrative_ontology:cs_axiom_status(military_committee_rule_above_faction, holdable).
narrative_ontology:cs_axiom_grounding('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', military_committee_rule_above_faction, conventional).
narrative_ontology:cs_axiom('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', foundational, emergency_justifies_civilian_suppression).
narrative_ontology:cs_axiom_status(emergency_justifies_civilian_suppression, holdable).
narrative_ontology:cs_axiom_grounding('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', emergency_justifies_civilian_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', national_security_emergency).
narrative_ontology:cs_drift_state('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', indefinite_emergency_perpetuation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a63261a0-41a4-4fe7-94eb-c1c38b2a3c82', '').
narrative_ontology:cs_kernel_id(autocratic_rule__military_junta, autocratic_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(autocratic_rule__military_junta, officer_corps).
narrative_ontology:constraint_beneficiary(autocratic_rule__military_junta, military_institutional_budget).
narrative_ontology:constraint_victim(autocratic_rule__military_junta, civilian_political_parties).
narrative_ontology:constraint_victim(autocratic_rule__military_junta, organized_labor).
narrative_ontology:constraint_victim(autocratic_rule__military_junta, democratic_norms).
narrative_ontology:constraint_victim(autocratic_rule__military_junta, non_military_civil_society).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUPPRESSED CIVILIANS (SNARE) — Parties, unions, civil society organizations face wholesale suppression of political activity. Assembly banned, leadership imprisoned, media censored. No exit from the system short of exile or collaboration. Maximum extraction with minimal coordination function — the junta coordinates military hierarchy, not civilian welfare. Civilians experience pure coercive extraction.
constraint_indexing:constraint_classification(autocratic_rule__military_junta, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROFESSIONAL CIVIL SERVICE (TANGLED ROPE) — Bureaucrats, technocrats, and civil administrators continue some coordination function (infrastructure, taxation, service delivery) under junta authority. Constrained exit — continuing employment requires accepting junta legitimacy. Mix of genuine coordination (keeping the state functional) and extraction (subordination to military authority, salary control, political compliance). Moderate experienced extraction with real benefits from the coordination infrastructure.
constraint_indexing:constraint_classification(autocratic_rule__military_junta, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OFFICER CORPS (ROPE) — Military institution rules as committee, justified by emergency and claim of standing above faction. Officers experience the constraint as pure coordination: consolidating power, eliminating rivals, securing budgets and institutional autonomy. Net beneficiary with exit options (exit to power consolidation, not from it). Extraction runs toward this agent; they perceive coordination benefits.
constraint_indexing:constraint_classification(autocratic_rule__military_junta, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUNTA LEGITIMACY APPARATUS (PITON) — The emergency claim and 'above faction' framing are substantially performative. Juntas routinely extract wealth and patronage; factions within the military persist despite the unity claim; emergencies are selectively invoked and prolonged. The legitimating narrative persists through institutional control of media and suppression of counternarratives, not through genuine emergency or unified command. Theater ratio high because the constraint's justification is maintained through theatrical performance of military unity and emergency.
constraint_indexing:constraint_classification(autocratic_rule__military_junta, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL STATE ACTORS (TANGLED ROPE) — Foreign powers and international institutions face mixed coordination and extraction. Junta regimes offer stability (coordination benefit for trade and security partnerships), but at the cost of human rights violations and democratic regression (extraction from international liberal order norms). Mobile exit for some states (sanctions, diplomatic isolation); constrained for others (regional security dependencies). Perspectival gap reflects whether the external actor prioritizes stability or democratic norms.
constraint_indexing:constraint_classification(autocratic_rule__military_junta, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, military intervention in politics is treated as a natural response to state failure or democratic breakdown: whenever civilian order collapses, the military (the only organized institution) 'naturally' steps in. This perspective sees junta rule as an inevitable structural feature of state crisis — immutable and independent of policy choice. However, this reading naturalizes a contingent political outcome (officer corps chooses intervention; alternatives exist), potentially masking the junta's constructed extractive mechanism as if it were an unavoidable law.
constraint_indexing:constraint_classification(autocratic_rule__military_junta, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autocratic_rule__military_junta_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(autocratic_rule__military_junta, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(autocratic_rule__military_junta, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(autocratic_rule__military_junta, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(autocratic_rule__military_junta, TR),
    TR >= 0.70.

:- end_tests(autocratic_rule__military_junta_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The junta extracts substantial institutional benefits for the officer corps (autonomous rule without civilian oversight, budget control, elimination of rival power centers) while suppressing the political activities of all non-military actors. The extraction is not total because some coordination functions continue (bureaucracy, infrastructure, taxation), distinguishing junta rule from pure predation. The 0.58 value reflects tangled rope structure: genuine coordination overhead exists, but asymmetric benefits accrue to the beneficiary (officer corps). Suppression (0.78): High. Wholesale suppression of civilian political activity — parties banned or subordinated, unions dissolved or state-controlled, media censored, assembly restricted, leadership imprisoned. Suppression persists through active enforcement (military police, surveillance, detention) and institutional control. The 0.78 reflects sustained, pervasive suppression infrastructure. Theater ratio (0.65): Moderate-high. The junta's legitimating claims (emergency justifying suspension of civilian rule, military standing above faction, national unity) are substantially performed rather than operationally true. Juntas routinely maintain proclaimed emergencies indefinitely despite absence of triggering crisis conditions; suppress evidence of internal military factions despite the 'unity' claim; reinvent emergency justifications as prior ones fade. The theater increases over time (0.50 → 0.65 over the interval) as the junta's initial crisis legitimacy erodes and relies increasingly on performance of unity. The measuring interval (0 to 6) represents early junta period to maturation — initial emergency claim is more credible; later theater intensifies as the regime seeks to perpetuate itself.
 *
 * PERSPECTIVAL GAP:
 *   The suppressed civilian population (powerless/trapped) sees snare: pure coercive extraction with no coordination function serving them. The officer corps (institutional/arbitrage) sees rope: consolidating power, securing budgets, removing rivals — all coordination benefits from their structural position. The professional civil service (moderate/constrained) sees tangled rope: some coordination work continues (infrastructure, taxation), but subordinated to junta authority. The junta's legitimacy apparatus (institutional/constrained) sees piton: the performance of unity and emergency must be maintained, but the underlying emergency may be manufactured or prolonged artificially. International actors (powerful/mobile) see tangled rope with perspectival gap: stability benefit if security interests align, extraction cost if democratic norms are prioritized. The analytical observer (analytical/analytical) risks naturalizing the junta as immutable response to state failure, masking the officer corps's constructed choice to intervene as if it were inevitable — this is the false summit risk, where contingent political outcomes are treated as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: Officer corps gains institutional autonomy, budget expansion, and political monopoly. As primary beneficiary with arbitrage exit options (exit into power consolidation, not from it), their d is low (~0.20), producing negative or low f(d) → low experienced extractiveness from their perspective. They perceive coordination and benefit. Victim directionality: Civilian parties, unions, and civil society face wholesale suppression with trapped exit options (no exit short of exile or collaboration). As primary victims with no structural exit, their d is high (~0.92), producing high f(d) → high experienced extractiveness. They perceive pure extraction. Mixed directionality: Professional civil service retains some coordination function and employment, but constrained by political subordination. Their d is moderate (~0.55), producing moderate f(d) → moderate experienced extractiveness. Constrained exit (costly to leave, safe only if compliant) defines their perspectival position. The engine's derivation chain maps these beneficiary/victim + exit parameters to directionality values and applies the sigmoid f(d) per the formula χ = ε × f(d) × σ(S). No overrides are needed — the structural data sufficiently captures the perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   The junta constraint resolves the mandatrophy by demonstrating how a single structural phenomenon (military rule) can be classified as multiple types depending on the observer's structural position. The suppressed civilian sees snare (pure extraction, no coordination benefit). The officer corps sees rope (coordination and power consolidation). The civil service sees tangled rope (mixed coordination and constraint). The junta's own narrative apparatus sees piton (performative maintenance of unity claim). International observers see tangled rope with perspectival variation (stability vs democracy tradeoff). The analytical observer risks mountain (naturalizing as inevitable response to state failure). No single type is 'correct' — the perspectival reading from each structural position IS the analytical result. The tangled rope claimed type is correct from the most analytically complete view: junta rule genuinely coordinates some state functions (bureaucratic continuity, infrastructure, taxation) while asymmetrically extracting institutional benefits for the officer corps. This satisfies tangled rope gates: genuine coordination function exists, asymmetric extraction is demonstrable, and active enforcement is required to suppress civilian alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_legitimacy_durability,
    'Does the emergency claim that justifies junta rule persist because genuine emergencies are continuous, or because the junta indefinitely prolongs/reinvents emergency to sustain legitimacy?',
    'Longitudinal analysis of declared emergencies; comparison of objective crisis indicators (economic collapse, foreign threat, internal violence) against junta tenure duration; documentation of emergency decree renewal cycles',
    'If genuine ongoing emergency: junta is legitimately temporary (scaffold reading). If prolonged/invented emergency: the emergency claim is theatrical, and junta is snare or piton (institutionalized extraction masked as crisis response).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_legitimacy_durability, empirical, 'Whether emergency justification is durable necessity or sustained performance').

omega_variable(
    officer_corps_unity_versus_factions,
    'Is the junta genuinely unified command (''standing above faction''), or do military factions extract different benefits and pursue competing interests masked by the unity claim?',
    'Institutional analysis of junta decision-making; documentation of internal military conflicts over budgets, deployment, and succession; examination of patronage networks within officer corps',
    'If unified: junta is primarily coordination mechanism (rope/tangled_rope). If factional: unity is performance, and the junta is primarily extraction mechanism benefiting some officers over others (snare/piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(officer_corps_unity_versus_factions, empirical, 'Whether officer unity is structural or theatrical').

omega_variable(
    civilian_order_restoration_pathway,
    'Can civilian democratic order be restored from junta rule through negotiated transition, or does junta institutional entrenchment make restoration require rupture (revolution, external intervention, generational change)?',
    'Comparative analysis of junta transitions (Spain, Greece, Argentina, Thailand, Myanmar); identification of structural barriers to negotiated return to civilian rule; documentation of junta officer stakes in perpetuating military governance',
    'If negotiable: junta is temporary (scaffold). If requiring rupture: junta is fundamentally incompatible with restoration (snare), and extraction cannot end through normal institutional process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_order_restoration_pathway, empirical, 'Feasibility of civilian order restoration from junta rule').

omega_variable(
    reading_boundaries_junta_versus_personalism,
    'Where is the boundary between military junta rule (committee of officers) and personalist dictatorship (one officer above the junta, governing through patronage)? Can this reading coexist with personalist dictatorship, or does junta committee rule foreclose one-person rule?',
    'Examination of actual junta transitions: some junior officers consolidate power (Turkey 1980-1983 → personalist Özal; Myanmar 2021-present → personalist Min Aung Hlaing); others sustain committee rule (Argentina 1976-1983). The boundary is empirical — does the constraint you are authoring describe committee structure that actively forbids personalist consolidation, or does it describe the initial junta form that typically transitions to personalism?',
    'If junta committee structure durably forecloses personalism: relation is ''forecloses''. If junta is transition form to personalism: relation is ''influences''. If both coexist (committee in name, patronage in fact): relation is ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundaries_junta_versus_personalism, conceptual, 'Structural relationship between junta committee rule and personalist autocracy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(autocratic_rule__military_junta, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(junta_tr_t0, autocratic_rule__military_junta, theater_ratio, 0, 0.5).
narrative_ontology:measurement(junta_tr_t3, autocratic_rule__military_junta, theater_ratio, 3, 0.58).
narrative_ontology:measurement(junta_tr_t6, autocratic_rule__military_junta, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(junta_be_t0, autocratic_rule__military_junta, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(junta_be_t3, autocratic_rule__military_junta, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(junta_be_t6, autocratic_rule__military_junta, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(junta_su_t0, autocratic_rule__military_junta, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(junta_su_t3, autocratic_rule__military_junta, suppression_requirement, 3, 0.74).
narrative_ontology:measurement(junta_su_t6, autocratic_rule__military_junta, suppression_requirement, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(autocratic_rule__military_junta, enforcement_mechanism).
narrative_ontology:affects_constraint(autocratic_rule__military_junta, autocratic_rule__hereditary_monarchy).
narrative_ontology:affects_constraint(autocratic_rule__military_junta, autocratic_rule__party_autocracy).
narrative_ontology:affects_constraint(autocratic_rule__military_junta, autocratic_rule__personalist_dictatorship).

% DUAL FORMULATION NOTE:
% The military junta reading is one constraint in a three-constraint family decomposing the contested kernel 'autocratic rule'. The three readings (military junta, hereditary monarchy, party autocracy, personalist dictatorship) share the same kernel (autocracy) but have structurally distinct ε values, beneficiary/victim sets, legitimacy mechanisms, and suppression methods. Each is a separate constraint story linked via network.affects_constraints to its siblings. The relationships are coexists_with (all remain live forms across different historical and geographic contexts) and influences (junta rule can transition to personalism, monarchy can transition to junta, party autocracy can transition to personalism — the upstream reading creates structural conditions that influence successor forms). Decomposition is required by ε-invariance: the four readings would have different extractiveness values if measured with observable-dependent variables (e.g., 'stability of succession' favors monarchy; 'organizational capacity' favors party autocracy; 'speed of consolidation' favors junta). Each reading stands as a self-contained constraint with stable ε across all relevant observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(autocratic_rule__military_junta, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
