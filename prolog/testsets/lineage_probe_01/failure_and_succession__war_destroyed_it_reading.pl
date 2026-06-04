% ============================================================================
% CONSTRAINT STORY: failure_and_succession__war_destroyed_it_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_failure_and_succession__war_destroyed_it_reading, []).

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
 *   constraint_id: failure_and_succession__war_destroyed_it_reading
 *   human_readable: War Destroyed the 1791 Settlement (Reading: Military Time Consumed Constitutional Time)
 *   domain: political/historical/constitutional
 *
 * SUMMARY:
 *   The 1791 Constitution attempted to stabilize a revolutionary transition
 *   through institutional compromise: separation of powers, hereditary
 *   executive with constrained authority, national assembly with legislative
 *   supremacy, and property-qualified franchise. This settlement embodied
 *   gradualist assumption — constitutional time would allow competing
 *   revolutionary factions to adapt to new legal structures, hereditary
 *   monarchy to evolve into constitutional kingship, and property claims to
 *   stabilize into a new social order. The declaration of war in April 1792
 *   destroyed this framework by transmuting every internal compromise into a
 *   security question. The king, designed by the constitution to be an
 *   executive servant of the nation, became a suspected enemy commander whose
 *   foreign family connections made him a security liability. The
 *   legislature, designed to deliberate and legislate, became a ratification
 *   body for military necessity. The franchise, designed to balance property
 *   interests against popular participation, became irrelevant as war
 *   mobilization required total population commitment. This reading traces
 *   how military time (the urgency of coordinating mobilized populations
 *   against external threat) consumed constitutional time (the deliberative
 *   pace of institutional evolution). The constraint is extractive: emergency
 *   executive apparatus benefits from the subordination of constitutional
 *   oversight; the settlement's victims are gradualist time, deliberative
 *   process, and the king's constitutional legitimacy. War did not destroy
 *   the settlement through inevitable natural law — it destroyed it through a
 *   choice to treat security logic as overriding constitutional design.
 *
 * KEY AGENTS:
 *   - The 1791 Constitution as Institutional Design: Primary victim (powerless/trapped) — trapped in a transformed security landscape where every compromise becomes a liability; victim of the constraint's extraction
 *   - National Assembly and Successor Legislatures: Secondary victims (moderate/constrained) — face constrained choice between resistance (lethal) and compliance (surrender of legislative authority); bear the extraction of deliberative time consumed by military ratification
 *   - Louis XVI and Hereditary Executive: Tertiary victim and tangled beneficiary (powerful/arbitrage) — victim of security suspicion classification, beneficiary of magnified executive authority in wartime; trapped in paradox of possessing more power but less legitimacy
 *   - Emergency Executive Apparatus (War Council, General Staff, Supply Administration): Primary beneficiary (organized/constrained) — benefits from unified command and rapid decision-making; experiences genuine coordination function; lowest experienced extraction of any agent
 *   - Military Command Structure: Beneficiary (institutional/arbitrage) — gains authority and resource control under war mobilization; defines security logic that overrides civilian constraint
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional choice (emergency logic's decision to suppress constitutional time) as inevitable consequence of war
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(failure_and_succession__war_destroyed_it_reading, 0.68).
domain_priors:suppression_score(failure_and_succession__war_destroyed_it_reading, 0.72).
domain_priors:theater_ratio(failure_and_succession__war_destroyed_it_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(failure_and_succession__war_destroyed_it_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(failure_and_succession__war_destroyed_it_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(failure_and_succession__war_destroyed_it_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(failure_and_succession__war_destroyed_it_reading, snare).
narrative_ontology:human_readable(failure_and_succession__war_destroyed_it_reading, "War Destroyed the 1791 Settlement (Reading: Military Time Consumed Constitutional Time)").
narrative_ontology:topic_domain(failure_and_succession__war_destroyed_it_reading, "political/historical/constitutional").

domain_priors:requires_active_enforcement(failure_and_succession__war_destroyed_it_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(failure_and_succession__war_destroyed_it_reading, 'f2e63f66-bce2-45b5-b343-46b6bf13e0a1').
narrative_ontology:cs_kernel_codification('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', fixed_text).
narrative_ontology:cs_authority_grounding('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', extraction).
narrative_ontology:cs_interpretation_layer_present('f2e63f66-bce2-45b5-b343-46b6bf13e0a1').
narrative_ontology:cs_reading_relation('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', failure_and_succession__internal_contradiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', failure_and_succession__template_for_successors_reading, influences).
narrative_ontology:cs_axiom('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', foundational, military_necessity_overrides_constitutional_time).
narrative_ontology:cs_axiom_status(military_necessity_overrides_constitutional_time, holdable).
narrative_ontology:cs_axiom_grounding('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', military_necessity_overrides_constitutional_time, empirically_contingent).
narrative_ontology:cs_axiom('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', foundational, king_incompatible_with_national_security).
narrative_ontology:cs_axiom_status(king_incompatible_with_national_security, holdable).
narrative_ontology:cs_axiom_grounding('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', king_incompatible_with_national_security, empirically_contingent).
narrative_ontology:cs_reference_frame('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', constitutional_gradualism_framework).
narrative_ontology:cs_drift_state('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', april_1792_war_declaration, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f2e63f66-bce2-45b5-b343-46b6bf13e0a1', '').
narrative_ontology:cs_kernel_id(failure_and_succession__war_destroyed_it_reading, failure_and_succession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(failure_and_succession__war_destroyed_it_reading, emergency_executive).
narrative_ontology:constraint_beneficiary(failure_and_succession__war_destroyed_it_reading, military_command_structure).
narrative_ontology:constraint_victim(failure_and_succession__war_destroyed_it_reading, gradualist_constitutional_settlement).
narrative_ontology:constraint_victim(failure_and_succession__war_destroyed_it_reading, deliberative_legislative_time).
narrative_ontology:constraint_victim(failure_and_succession__war_destroyed_it_reading, king_as_constitutional_actor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE GRADUALIST SETTLEMENT (SNARE) — The 1791 Constitution itself, as designed institution, cannot exit the war declaration. Every compromise it contained (separation of powers, hereditary executive with legislative constraints, property-qualified franchise) becomes a liability once military logic takes precedence. The settlement's victims — those who invested in its gradualist time frame — face coerced abandonment of the framework they constructed. No exit option exists for the constitutional text itself; it is trapped in a transformed security landscape.
constraint_indexing:constraint_classification(failure_and_succession__war_destroyed_it_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DELIBERATIVE LEGISLATIVE BODIES (SNARE) — The National Assembly and its successors face constrained exit: they can resist war declarations (costly, career-ending, possibly lethal) or comply and watch their deliberative authority collapse into military ratification. The extraction is severe: legislative time — the medium of the constitutional settlement — becomes subordinate to military time. Suppression is high because resistance is politically lethal; alternatives (legislative oversight of war powers, proportional military authority) are suppressed by the security logic that now dominates.
constraint_indexing:constraint_classification(failure_and_succession__war_destroyed_it_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE KING AS CONSTITUTIONAL ACTOR (TANGLED ROPE) — Louis XVI occupies a paradoxical structural position. The 1791 settlement granted him real executive power (veto, command of military, treaty authority) but made that power subject to legislative constraint — the core constitutional compromise. War declaration transforms his legal position into suspected enemy commander. His arbitrage exit (he could flee, negotiate separately with foreign powers, invoke his hereditary legitimacy against the constitution) converts the constraint from binding to catastrophic. He benefits from emergency executive power (executive authority exceeds constitutional bounds in wartime) but loses the legitimacy framework that made his power durable. The constraint contains genuine coordination function (war requires unified command) and asymmetric extraction (his constitutional role is destroyed by the warfare that magnifies his military authority).
constraint_indexing:constraint_classification(failure_and_succession__war_destroyed_it_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EMERGENCY EXECUTIVE APPARATUS (ROPE) — War declaration creates genuine coordination need: unified command, rapid decision-making, centralized resource allocation. Military structure solves real problems of coordination under threat. The organized actors who staff emergency executive machinery (war council, general staff, supply administration) experience the constraint as enabling rather than extractive. They have constrained exit (leaving the war effort is desertion or betrayal) but genuine coordination function justifies the constraints. Theater ratio is low (0.35) because military command has functional verification mechanisms — supply reaches troops, orders execute, enemies are engaged — not performative ritual.
constraint_indexing:constraint_classification(failure_and_succession__war_destroyed_it_reading, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL SUCCESSION FRAMEWORK — Viewed from the generational time horizon, war declares a structural sunset: the 1791 settlement is explicitly transcended by military necessity, creating a legitimate expectation that post-war politics will construct a successor framework. The declaration of April 1792 is not permanent constitutional abolition but a suspension triggering renewal. Scaffold classification reflects that the emergency apparatus itself is theoretically temporary — war ends, normal constitutional time resumes. However, this reading overestimates the settlement's capacity for succession; the actual history shows multiple regime cycles (1792, 1795, 1799, 1804, 1815...) with no stable return to 1791 structures. The scaffold sunset is aspirational rather than structural.
constraint_indexing:constraint_classification(failure_and_succession__war_destroyed_it_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some might frame this as an immutable law of politics: war destroys the conditions that peace-time constitutions assume. When external threat reaches critical intensity, internal constitutional time is necessarily subordinated to military time — this appears as a structural inevitability. However, this reading naturalizes what is actually a contingent institutional choice: the war *could* have been conducted under constitutional constraints (war powers oversight, legislative approval of major operations, legal constraints on executive emergency powers). The fact that these constraints were overridden reveals that war did not *necessitate* constitutional collapse — emergency logic actively chose to treat constitutional time as dispensable.
constraint_indexing:constraint_classification(failure_and_succession__war_destroyed_it_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(failure_and_succession__war_destroyed_it_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(failure_and_succession__war_destroyed_it_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(failure_and_succession__war_destroyed_it_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(failure_and_succession__war_destroyed_it_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(failure_and_succession__war_destroyed_it_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High. The constraint extracts massive benefits to emergency executive apparatus at the cost of constitutional settlement. The king loses legitimacy, the legislature loses authority, the settlement loses institutional viability. The war declaration creates structural asymmetry: military command benefits from unified authority; constitutional actors lose the deliberative capacity they were designed to exercise. Suppression (0.72): High. Alternatives to military subordination of constitutional time are suppressed: legislative oversight of war operations is suppressed by security logic; public debate on war strategy is suppressed by security classification; the king's role in peace-time executive authority is suppressed by suspicion of enemy sympathy; property-based franchise is suppressed by total mobilization logic. Theater ratio (0.35): Low. Military mobilization is functionally oriented — supplies must reach troops, orders must execute, forces must engage enemies. The verification mechanisms are outcome-based (did military objectives advance?) rather than procedural-ritual. This is why the constraint is classified as snare rather than piton: extraction is genuine and functional, not performative. The measurements show the trajectory: pre-war extractiveness (0.32) reflects constitutional settlement in operation, with some friction between executive and legislature but genuine separation of powers function. Post-April 1792 extractiveness (0.68) shows dramatic increase reflecting war declaration's reordering of priorities. Suppression rises from 0.28 (pre-war constitutional constraints exist but are negotiable) to 0.72 (military necessity suppresses alternatives). Theater drops from 0.48 (legislative ritual persists) to 0.35 (military command dominates) because military functions are outcome-verified, not ritual-validated.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces classification divergence across contexts: emergency apparatus sees rope (genuine coordination function); king sees tangled rope (mixed coordination and extraction, but with paradoxical power dynamics); gradualists see snare (pure extraction); analytical observer risks seeing mountain (naturalizing contingent choice as inevitable). The critical perspectival gap lies between the emergency apparatus (organized/constrained) and the constitutional settlement (powerless/trapped). Both experience the same war declaration, but the emergency apparatus benefits from unified command while the settlement loses all its designed functions. This gap reveals that the constraint is not a natural law but an extractive apparatus justified by security logic — the beneficiaries have agency and alternatives; the victims do not.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality from structural position. Emergency apparatus (beneficiary + organized power + constrained exit) experiences low effective extraction — they gain authority and solve genuine coordination problems. The king (complex position: beneficiary of magnified executive power + powerful + arbitrage exit) experiences tangled extraction — the magnified power is real but delegitimized. Legislators (victim of authority loss + moderate power + constrained exit) experience high extraction — they cannot resist war without career-ending consequences. The settlement itself (victim + powerless + trapped) experiences maximum extraction — it cannot exit the transformed security landscape. The analytical observer (analytical position + universal scope) risks deriving mountain classification by treating war as a natural limit on constitutional time, when the structural data reveals this as a choice to suppress constitutional alternatives in favor of military command.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    war_necessity_vs_choice,
    'Did the April 1792 war declaration necessarily destroy the constitutional settlement, or did the declaration *choose* to treat constitutional constraints as disposable under emergency logic?',
    'Comparative analysis: contrast with constitutional democracies that maintained legislative oversight and legal constraints during existential wars (UK 1939-45, US 1941-45, Israel 1948-present). If parallel wars preserved constitutional time, war did not necessitate collapse.',
    'If necessary: war is a natural limit on constitutional governance (mountain reading). If choice: emergency executive is an extractive apparatus using war logic to justify constitutional transgression (snare reading persists). Classification hinges on whether alternatives existed and were rejected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(war_necessity_vs_choice, empirical, 'Whether war necessarily destroyed the settlement or whether destruction was a contingent choice').

omega_variable(
    king_as_security_threat_vs_constitutional_actor,
    'Was the king classified as a security threat because his constitutional position was inherently incompatible with war mobilization, or because specific actions (failed flight, contact with foreign powers, resistance to Assembly control) created the threat classification?',
    'Chronological analysis of when king-as-suspected-enemy classification solidified relative to specific royal actions. If classification preceded actions, it was ideological. If actions preceded and drove classification, it was responsive to actual behavior.',
    'If ideological: suspicion of the king was built into the emergency logic from the start (constitutional monarchy was always seen as incompatible with national security). If behavioral: the king could have remained a constitutional actor by accepting subordination to war mobilization. This affects whether the constraint targets the king''s constitutional role (snare from his perspective) or his resistance to military subordination (extraction justified by security).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(king_as_security_threat_vs_constitutional_actor, empirical, 'Whether king-threat classification was structural or behavioral').

omega_variable(
    emergency_logic_permanence,
    'Was emergency executive authority genuinely temporary (enabling for war, sunset after peace), or did it establish a structural precedent that made emergency logic permanently available to future executives?',
    'Analysis of constitutional texts post-1792: do they reassert civilian supremacy and legislative oversight, or do they encode emergency executive authority as a permanent institutional feature? What constraints were actually reinstated after military victory?',
    'If genuinely temporary: scaffold reading is accurate (sunset is real). If permanent precedent: 1792 established that war-time suspension of constitutional time is repeatable by any executive claiming security necessity. This affected every subsequent French regime and constitutional cycle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_logic_permanence, empirical, 'Whether emergency executive authority became a permanent institutional feature').

omega_variable(
    reading_kernel_distinction,
    'This reading (war destroyed the settlement) is one of three framings of the same kernel. How do the three readings relate: do they foreclose each other, coexist as competing interpretations, or influence each other''s validity?',
    'Structural analysis: the internal_contradiction_reading claims the settlement collapsed because of inherent contradictions (king vs nation, rights vs franchise limits). The war_destroyed_it reading claims war was the killing blow. The template_for_successors reading claims the settlement was always temporary and exemplary. If all three are factually defensible from different archive sources or interpretive traditions, they coexist. If they make contradictory empirical claims (e.g., contradiction reading requires settlement to have failed even without war), one forecloses the other.',
    'Determines the cs_structure.reading_relations values: forecloses (contradictory), coexists_with (simultaneous live positions), or influences (one shapes conditions for the other). This affects how the engine models the contested kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Structural relationship between war_destroyed_it reading and sibling readings').

omega_variable(
    suppression_mechanism_clarity,
    'The high suppression value (0.72) reflects constraints on constitutional alternatives under war logic. Are these constraints structural (genuine incompatibility between constitution and war mobilization) or enforced (choices to suppress alternatives by military leadership)?',
    'Documentation of suppression mechanisms: were alternative proposals (constitutional war powers oversight, legislative veto on major operations, legal constraints on executive) formally proposed and rejected? If proposals exist and were suppressed, suppression is enforced. If proposals never emerged, alternatives were foreclosed by the structure itself.',
    'If enforced: the constraint is a snare (extractive apparatus suppressing alternatives). If structural: the constraint approaches mountain classification (alternatives are genuinely incompatible). Current assessment assumes enforcement dominates (snare); if structural foreclosure is primary, classification shifts toward tangled_rope or even mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_clarity, empirical, 'Whether suppression of constitutional alternatives was structural or enforced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(failure_and_succession__war_destroyed_it_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_pre_war_legislative_ritual, failure_and_succession__war_destroyed_it_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(theater_war_mobilization_functional, failure_and_succession__war_destroyed_it_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(theater_terror_rituals_escalate, failure_and_succession__war_destroyed_it_reading, theater_ratio, 3, 0.62).

% Extraction over time
narrative_ontology:measurement(extract_pre_declaration_1791_settlement_baseline, failure_and_succession__war_destroyed_it_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(extract_post_declaration_april_1792_immediate, failure_and_succession__war_destroyed_it_reading, base_extractiveness, 1, 0.68).
narrative_ontology:measurement(extract_september_massacres_escalation, failure_and_succession__war_destroyed_it_reading, base_extractiveness, 3, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(suppress_pre_war_constitutional_constraints, failure_and_succession__war_destroyed_it_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(suppress_post_declaration_military_necessity, failure_and_succession__war_destroyed_it_reading, suppression_requirement, 1, 0.72).
narrative_ontology:measurement(suppress_terror_period_maximum, failure_and_succession__war_destroyed_it_reading, suppression_requirement, 3, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(failure_and_succession__war_destroyed_it_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(failure_and_succession__war_destroyed_it_reading, failure_and_succession__internal_contradiction_reading).
narrative_ontology:affects_constraint(failure_and_succession__war_destroyed_it_reading, failure_and_succession__template_for_successors_reading).

% DUAL FORMULATION NOTE:
% The failure_and_succession kernel has three readings, each with different ε values reflecting different causal claims about why the 1791 Constitution collapsed. This reading (war_destroyed_it) produces ε=0.68 for extraction from emergency logic subordinating constitutional time. The internal_contradiction_reading would produce lower ε (structural collapse inherent to design contradictions). The template_for_successors_reading treats the settlement as never intended to be permanent and thus produces different victim/beneficiary structure. All three are linked to the same kernel but represent distinct constraint stories with different extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(failure_and_succession__war_destroyed_it_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
