% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universal Jurisdiction Mandate (Universalist Reading)
 *   domain: international_law/justice
 *
 * SUMMARY:
 *   This constraint represents the 'universalist reading' of the Rome
 *   Statute, which posits that the International Criminal Court (ICC)
 *   possesses a mandate for international criminal justice that can transcend
 *   the explicit consent of states, particularly concerning jurisdiction over
 *   nationals of non-party states via territorial or UN Security Council
 *   triggers. This reading emphasizes the gravity of core international
 *   crimes and the imperative of accountability over traditional notions of
 *   state sovereignty. The high extractiveness and suppression reflect the
 *   imposition of jurisdiction on non-consenting entities, while the low
 *   theater ratio indicates that the ICC actively pursues its mandate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.85).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.78).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universal Jurisdiction Mandate (Universalist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/justice").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, 'c224d414-f87f-47b7-a117-875ae7ed44b9').
narrative_ontology:cs_kernel_codification('c224d414-f87f-47b7-a117-875ae7ed44b9', fixed_text).
narrative_ontology:cs_authority_grounding('c224d414-f87f-47b7-a117-875ae7ed44b9', lineage).
narrative_ontology:cs_interpretation_layer_present('c224d414-f87f-47b7-a117-875ae7ed44b9').
narrative_ontology:cs_reading_relation('c224d414-f87f-47b7-a117-875ae7ed44b9', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('c224d414-f87f-47b7-a117-875ae7ed44b9', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('c224d414-f87f-47b7-a117-875ae7ed44b9', foundational, universal_jurisdiction_for_core_crimes).
narrative_ontology:cs_axiom_status(universal_jurisdiction_for_core_crimes, holdable).
narrative_ontology:cs_axiom_grounding('c224d414-f87f-47b7-a117-875ae7ed44b9', universal_jurisdiction_for_core_crimes, deontological).
narrative_ontology:cs_axiom('c224d414-f87f-47b7-a117-875ae7ed44b9', foundational, sovereignty_subordinate_to_jus_cogens).
narrative_ontology:cs_axiom_status(sovereignty_subordinate_to_jus_cogens, holdable).
narrative_ontology:cs_axiom_grounding('c224d414-f87f-47b7-a117-875ae7ed44b9', sovereignty_subordinate_to_jus_cogens, deontological).
narrative_ontology:cs_reference_frame('c224d414-f87f-47b7-a117-875ae7ed44b9', post_nuremberg_international_justice).
narrative_ontology:cs_drift_state('c224d414-f87f-47b7-a117-875ae7ed44b9', contemporary_geopolitical_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c224d414-f87f-47b7-a117-875ae7ed44b9', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_atrocities).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_justice_advocates).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_criminal_court).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states_nationals).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, sovereigntist_legal_scholars).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, states_asserting_absolute_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institution asserting and applying this universalist interpretation of its jurisdiction, seeking to hold individuals accountable for core international crimes regardless of state consent. It actively prosecutes cases and defends its mandate against challenges.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% Individuals who have suffered core international crimes and seek justice, especially when their national systems are unwilling or unable to provide it. This reading offers them a potential avenue for accountability that transcends state borders.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_atrocities, beneficiary,
    powerless, immediate, trapped, global).

% Individuals from states not party to the Rome Statute who may face ICC jurisdiction if their alleged crimes occur on the territory of a state party or are referred by the UN Security Council. They bear the cost of potential prosecution without their state's consent.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states_nationals, payer,
    moderate, biographical, constrained, global).

% States that are not party to the Rome Statute and/or strongly assert their absolute national sovereignty, viewing ICC jurisdiction over their nationals or territory without explicit consent as an infringement. They bear diplomatic, legal, and reputational costs in resisting the ICC's claims.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, states_asserting_absolute_sovereignty, payer,
    institutional, generational, constrained, global).

% Human rights organizations, NGOs, and legal scholars who champion the principle of universal jurisdiction for international crimes and support the ICC's broad interpretation of its mandate. They benefit from the expansion of international accountability.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_justice_advocates, beneficiary,
    organized, generational, mobile, global).

% Can refer situations to the ICC, thereby extending its jurisdiction to non-party states. Its actions are a key mechanism for the universalist reading to be enacted, though its decisions are often driven by political considerations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% Legal academics and practitioners who argue that international criminal jurisdiction must be strictly based on state consent and that the ICC's universalist claims undermine foundational principles of international law. They are excluded from the ICC's internal interpretive process but actively contest its legitimacy in academic and policy discourse.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, sovereigntist_legal_scholars, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a standing international mechanism for prosecuting individuals responsible for the most serious international crimes, ensuring accountability where national systems fail or are unwilling to act, thereby coordinating a global response to impunity.
% TRANSFER_FUNCTION: Transfers the authority to investigate and prosecute core international crimes from national states (especially non-parties) to the International Criminal Court, imposing legal and political costs on those who resist this transfer of jurisdiction.
% ABSENT_VOICES: States and individuals who assert absolute national sovereignty and reject the ICC's jurisdiction without their explicit consent. They would argue for a strictly consensual framework for international justice, but their objections are often overridden by the universalist interpretation.
% DISAPPEARANCE_RATIONALE: If the universalist reading of ICC jurisdiction vanished, international criminal justice would largely revert to a state-centric model, severely limiting accountability for atrocities, particularly in non-party states or where powerful states protect their nationals. The global landscape of human rights and international law would fundamentally shift.
% FOUNDING_PROBLEM: The persistent impunity for mass atrocities (genocide, crimes against humanity, war crimes) due to the failure or unwillingness of national justice systems, leading to a cycle of violence and injustice.
% FOUNDING_PROBLEM_CORROBORATION: Reports from the UN, human rights organizations (e.g., Human Rights Watch, Amnesty International), and a broad consensus among international legal scholars (outside the sovereigntist camp) consistently document ongoing atrocities and the continued need for international accountability mechanisms.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) stems from the universalist claim to assert jurisdiction over individuals from non-party states, effectively extracting their immunity from national sovereignty. Suppression (0.78) is high because the ICC actively works to overcome resistance from non-cooperating states, using legal and diplomatic tools to enforce its mandate. Resistance (0.88) is also high, reflecting the significant pushback from states that view this as an infringement on their sovereignty. The claimed type is 'tangled_rope' because it genuinely aims to coordinate international justice (a benefit for victims) but does so through a mechanism (transcending consent) that is highly extractive and requires active enforcement against non-consenting parties.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ICC and international justice advocates, this reading is a necessary and legitimate evolution of international law to combat impunity. From the perspective of non-party states and sovereigntist scholars, it represents an overreach of treaty law and an infringement on fundamental principles of state sovereignty. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC and victims of atrocities are clear beneficiaries, gaining an avenue for justice. International justice advocates also benefit from the expansion of accountability. Non-party states and their nationals are the primary targets/victims, as jurisdiction is asserted over them without their consent. The UN Security Council acts as an agenda-setter, capable of triggering this jurisdiction, but also subject to political constraints.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_transcending_consent,
    'Is the ICC''s assertion of jurisdiction over non-party nationals, transcending explicit state consent, a legitimate development of international law (e.g., based on jus cogens norms) or an overreach of treaty-based authority?',
    'Evolution of customary international law, widespread state practice and opinio juris, or a definitive ruling by the International Court of Justice on the scope of ICC jurisdiction vis-à-vis non-party states.',
    'If deemed legitimate, the constraint''s extractiveness might be re-evaluated as a necessary cost of a higher-order coordination; if deemed an overreach, its extractiveness and suppression would be confirmed as illegitimate, strengthening the ''snare'' aspect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_transcending_consent, conceptual, 'Ambiguity regarding the legal basis for universal jurisdiction transcending consent.').

omega_variable(
    deterrence_vs_backlash_impact,
    'What is the actual impact of the ICC''s universalist claims on deterring atrocities in non-party states, versus provoking political backlash, non-cooperation, and withdrawal from international legal frameworks?',
    'Empirical studies analyzing long-term trends in atrocity commission, state cooperation with the ICC, and diplomatic relations in response to ICC interventions, controlling for other geopolitical factors.',
    'If deterrence is minimal and backlash is significant, the ''coordination'' function of the tangled rope is weakened, pushing it closer to a pure ''snare'' where the costs outweigh the claimed benefits. If deterrence is substantial, the coordination function is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_backlash_impact, empirical, 'The effectiveness of universal jurisdiction in achieving its stated goals versus its unintended negative consequences.').

omega_variable(
    kernel_reading_distinction,
    'This constraint is one reading of the ''rome_statute_jurisdiction'' kernel. What specific structural elements would change if a different reading (e.g., ''sovereigntist_reading'' or ''hybrid_complementarity_reading'') were adopted?',
    'Comparative legal analysis of judicial decisions, state practice, and scholarly interpretations under each reading, identifying divergent jurisdictional triggers, victim definitions, and enforcement mechanisms.',
    'A shift to the ''sovereigntist_reading'' would drastically lower extractiveness and suppression, likely reclassifying to a ''rope'' or even ''mountain'' (if consent is seen as natural law). A shift to the ''hybrid_complementarity_reading'' would moderate extractiveness and suppression, emphasizing national primacy and potentially reclassifying to a ''rope'' or ''scaffold''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Documents the structural differences between this universalist reading and its sibling readings of the Rome Statute''s jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 2002, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(rome_tr_t2007, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2007, 0.12).
narrative_ontology:measurement(rome_tr_t2012, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2012, 0.15).
narrative_ontology:measurement(rome_tr_t2017, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(rome_tr_t2022, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2022, 0.15).
narrative_ontology:measurement(rome_tr_t2027, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2027, 0.15).

% Extraction over time
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2002, 0.7).
narrative_ontology:measurement(rome_be_t2007, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2007, 0.75).
narrative_ontology:measurement(rome_be_t2012, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2012, 0.8).
narrative_ontology:measurement(rome_be_t2017, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2017, 0.83).
narrative_ontology:measurement(rome_be_t2022, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2022, 0.85).
narrative_ontology:measurement(rome_be_t2027, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2027, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2002, 0.65).
narrative_ontology:measurement(rome_su_t2007, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2007, 0.7).
narrative_ontology:measurement(rome_su_t2012, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2012, 0.75).
narrative_ontology:measurement(rome_su_t2017, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2017, 0.78).
narrative_ontology:measurement(rome_su_t2022, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2022, 0.78).
narrative_ontology:measurement(rome_su_t2027, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2027, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__universalist_reading, 0.1).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, international_humanitarian_law_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Rome Statute's jurisdictional framework. Each reading has a different ε value and structural profile, reflecting the ongoing contestation over the ICC's authority. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
