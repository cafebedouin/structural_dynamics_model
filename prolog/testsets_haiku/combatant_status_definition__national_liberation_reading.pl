% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: Combatant Status for National Liberation Movements (AP I Article 1(4))
 *   domain: international_law/humanitarian
 *
 * SUMMARY:
 *   AP I Article 1(4) of the 1977 Additional Protocols extends combatant
 *   status to non-state armed groups fighting against colonial, occupation,
 *   and racist regimes if they meet criteria of organized structure,
 *   responsible command, distinctive marking, and conduct in accordance with
 *   the laws of war. This reading instantiates the constraint as a TANGLED
 *   ROPE: it coordinates humanitarian protection for liberation movements
 *   (the coordination function) while extracting from occupying powers the
 *   obligation to grant legal status and protections they would prefer to
 *   deny (the asymmetric extraction). The constraint's persistence depends on
 *   active enforcement by the ICRC and international pressure; without
 *   enforcement, occupying powers revert to unilateral denial of combatant
 *   status. The claiming reading (national_liberation_reading) is
 *   distinguished from the state_centric_reading (which denies non-state
 *   combatant status categorically) and the functional_protection_reading
 *   (which grants minimum protections without combatant status). This JSON
 *   generates only the national_liberation_reading constraint; the other
 *   readings are separate constraint stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - liberation_movement_combatants — organized insurgents fighting colonial/occupation regimes; benefit from combatant immunity and POW protections if Article 1(4) criteria met; identity-locked to the liberation cause.
 *   - occupying_state_military — institutional payer; bears obligation to treat insurgents as combatants (if criteria met) rather than criminals; loses interrogation and detention discretion.
 *   - occupied_populations — powerless beneficiaries; the constraint legitimizes their armed struggle and provides humanitarian framework for their resistance.
 *   - occupying_state_government — institutional agenda-setter and payer; can contest the interpretation of criteria but must abide by the constraint if criteria are satisfied.
 *   - ICRC and humanitarian agencies — observers; monitor and certify whether organizational and conduct criteria are met; enforce the constraint through prison inspections and advocacy.
 *   - State-centric reading advocates — excluded; argue combatant status is reserved for state militaries and non-state groups cannot qualify regardless of organization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.68).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.72).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "Combatant Status for National Liberation Movements (AP I Article 1(4))").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_law/humanitarian").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '7d5211c4-dff9-49d2-bf09-d529bbcad5b2').
narrative_ontology:cs_kernel_codification('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', fixed_text).
narrative_ontology:cs_authority_grounding('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', lineage).
narrative_ontology:cs_interpretation_layer_present('7d5211c4-dff9-49d2-bf09-d529bbcad5b2').
narrative_ontology:cs_reading_relation('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', foundational, non_state_combatant_eligibility).
narrative_ontology:cs_axiom_status(non_state_combatant_eligibility, holdable).
narrative_ontology:cs_axiom_grounding('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', non_state_combatant_eligibility, deontological).
narrative_ontology:cs_axiom('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', foundational, self_determination_justifies_armed_struggle).
narrative_ontology:cs_axiom_status(self_determination_justifies_armed_struggle, holdable).
narrative_ontology:cs_axiom_grounding('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', self_determination_justifies_armed_struggle, deontological).
narrative_ontology:cs_reference_frame('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', combatant_status_universal_applicability).
narrative_ontology:cs_drift_state('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', post_2001_security_framing, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7d5211c4-dff9-49d2-bf09-d529bbcad5b2', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, liberation_movement_combatants).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, occupied_populations).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_state_military).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_state_civilians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_state_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Armed groups fighting colonial or occupying regimes gain conditional access to combatant immunity and POW protections if they satisfy Article 1(4) criteria: responsible command structure, fixed distinctive sign, carrying arms openly, conducting operations in accordance with laws of war. Their structural identity fuses with the liberation cause; exit is not feasible as a negotiated choice but only as political defeat or victory. The constraint extends to them the legal status that previously applied only to state armies, conditional on organizational and conduct criteria.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, liberation_movement_combatants, beneficiary,
    organized, generational, identity_locked, national).

% Bears the obligation to treat captive insurgents as POWs (if Article 1(4) criteria are met) rather than as common criminals or unlawful combatants, triggering full Geneva Convention protections including humane treatment, fair trial, and repatriation. This obligation constrains interrogation methods, detention conditions, and judicial proceedings. The occupying power's operational advantage and legal discretion are curtailed by the constraint's extension of combatant status.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_state_military, payer,
    institutional, generational, constrained, national).

% Benefit from the legal framework that recognizes resistance movements as legitimate combatants rather than terrorists or criminals, which legitimizes their struggle for self-determination and provides humanitarian protections for their armed groups. They cannot exit the occupation or the conflict; the constraint defines the legal status of their representatives in armed struggle.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupied_populations, beneficiary,
    powerless, generational, trapped, national).

% Under this reading, must recognize and negotiate with organized liberation movements rather than unilaterally designate them as terrorist organizations. The constraint imposes legal obligations regarding POW status, fair trial, and humane treatment that the occupying power would prefer to avoid. However, the occupying power retains the role of agenda-setter insofar as it can contest the interpretation of Article 1(4) criteria and claim that particular groups do not meet the organizational and conduct standards.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_state_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, occupying_state_government, agenda_setter).

% Monitor and certify whether combatant-status criteria are met, conduct prison inspections, and advocate for application of the constraint. They take no direct material benefit or cost but serve as neutral arbiters of whether organizational and conduct standards are satisfied, and they mediate between the occupying power and the liberation movement.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, icrc_humanitarian_protection_agencies, observer,
    institutional, generational, analytical, global).

% Are formally bound by AP I Article 1(4) but often do not enforce it, particularly powerful states supporting the occupying regime or fearing precedent for their own territory. Their exclusion from the constraint's application (in practice, though not in law) is a key dynamic: the constraint is formally universal but selectively enforced based on geopolitical alignment. They would contest the constraint's applicability to movements they label 'terrorist' rather than 'national liberation.'
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_community_states, excluded,
    institutional, generational, constrained, global).

% Argue that combatant status must be limited to formal state militaries under Article 4 criteria, and that non-state armed groups cannot qualify regardless of their organizational or conduct compliance. This reading would deny POW status to liberation movement fighters and classify them as unlawful combatants. Their position is excluded from the constraint's framework but not extinguished; it remains an active competing interpretation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, state_centric_reading_advocates, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, occupying_state_military).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends uniform humanitarian protections and legal status to organized armed groups in legitimate anti-colonial and anti-occupation struggles, solving the problem of legal classification for non-state combatants who meet organizational and conduct standards. Without this extension, liberation movements operate in a legal vacuum where occupying powers can unilaterally deny them any protective status.
% TRANSFER_FUNCTION: Moves operational and legal advantage from the occupying state to the liberation movement: the occupying state must treat captured insurgents as POWs rather than criminals, provide humane conditions, conduct fair trials, and repatriate at war's end. The occupying state transfers discretionary power over classification and detention to international humanitarian law standards that it cannot unilaterally override.
% ABSENT_VOICES: Occupying-power governments and their allies are formally obligated but practically excluded from the constraint's enforcement apparatus; they contest Article 1(4) interpretation at every application. State-centric legal traditions and scholars who argue combatant status is reserved for state militaries are excluded from authority over the constraint's interpretation, though they remain active in competing frameworks. Movements that do not meet the organizational and command-control criteria are excluded from the constraint's protection entirely—they receive no POW status, which is presented as a neutral technical requirement but functions as a gate.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared and combatant status reverted to state-centric exclusivity, liberation movements would be universally classified as terrorists or unlawful combatants; captured fighters would lose POW protections; occupying powers would have unilateral discretion over detention conditions and trials; international humanitarian law would revert to a regime where non-state armed actors have no protected status. The entire architecture of protection for anti-colonial and anti-occupation struggles would collapse.
% FOUNDING_PROBLEM: Decolonization and national liberation movements in the mid-20th century produced armed struggles that were not state militaries but were organized, command-controlled, and fighting against colonial and racist occupation regimes. Existing IHL (particularly the Geneva Conventions) applied combatant status only to state militaries under Article 4, leaving liberation fighters without legal protection. The founding problem was how to extend humanitarian protections to non-state combatants engaged in legitimate struggles for self-determination without diluting the combatant status of state militaries.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and humanitarian scholars attest the problem was real and justified the extension of Article 1(4) protections. However, occupying-power governments and state-centric legal traditions contest whether liberation movements genuinely constitute combatants rather than insurgents or terrorists. The International Court of Justice has recognized the constraint in principle, but enforcement remains contested and selective—powerful states support occupying regimes against liberation movements they label terrorist, contradicting the constraint's universal application.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint moves significant operational and legal advantage from occupying powers to liberation movements: the occupying power loses unilateral discretion over detention, classification, and prosecution. Suppression is also high (0.72) because enforcement depends on active intervention by the ICRC and international pressure; without this enforcement machinery, occupying powers simply deny combatant status and treat insurgents as unlawful combatants. Theater ratio is moderate (0.28) because the constraint's functional content (humanitarian protection) is real, but a growing share of dispute centers on whether organizations actually meet the Article 1(4) criteria—occupying powers use technical objections about command structure and discipline as a way to deny status without formally rejecting the constraint. The measurement series shows a steady rise in suppression_requirement and a plateau in extractiveness after 2010, indicating that the constraint's operative extraction has stabilized even as enforcement demands have hardened (occupying powers resist more actively). The trajectory reflects the constraint's maturation: it is now firmly embedded in international law but faces increasing pressure from occupying states and powerful allies that support them.
 *
 * PERSPECTIVAL GAP:
 *   The occupying state and the liberation movement sit at opposite ends of directionality, producing fundamentally different experienced types. The occupying state's institutional power does not reduce its extraction exposure because the constraint's enforcement mechanism (international humanitarian law + ICRC + diplomatic pressure) operates outside its direct control. A more powerful institutional actor would need arbitrage-grade exit (ability to opt out of international legal systems) to escape; the occupying state has 'constrained' exit because opting out incurs international isolation costs. This structural gap is where the tangled-rope classification lives: genuine coordination (humanitarian protection) is layered with asymmetric extraction (operative obligation imposed on one side but not the other).
 *
 * DIRECTIONALITY LOGIC:
 *   Liberation movement combatants and occupied populations are structural beneficiaries of the constraint: it extends to them legal status and protections they would not otherwise have. Their directionality is low (full beneficiary end). Occupying powers are structural targets: they bear the obligation to grant status and protections and lose discretionary power over detention and prosecution. Their directionality is high (full target end). The beneficiary/victim declarations reflect this asymmetry: beneficiaries are the liberation movements and occupied populations; victims (those bearing costs) are the occupying state's military and government. The occupying power's 'constrained' exit option (it cannot simply leave or renegotiate unilaterally without political consequence) and institutional power level place it in a position of high extraction exposure despite its formal strength.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by maintaining a live founding problem: occupying powers still contest whether liberation movements meet Article 1(4) criteria, and powerful states still selectively enforce the constraint. The theater ratio's gradual rise (from 0.12 to 0.28) indicates some drift toward performance (occupying powers perform compliance with the organizational-criteria gate while denying it on technical grounds), but the functional core remains: the constraint genuinely extends protections when organizational criteria are met. The constraint does not yet exhibit the characteristic piton signature (low or zero beneficiary capture, pure inertial persistence). If the theater ratio continued to rise above 0.5 while extractiveness plateaued, that would signal mandatrophy—the constraint becoming primarily a performance of deference to international law while occupying powers find de facto workarounds (designating movements as terrorist, denying recognition regardless of criteria satisfaction). The current trajectory suggests the constraint remains operatively contested rather than theatrically maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_1_4_criteria_contestation,
    'What constitutes sufficient ''organized structure'' and ''responsible command'' under Article 1(4), and who adjudicates when these criteria are met?',
    'Case law from international tribunals and ICRC determinations; documented enforcement patterns showing which movements receive recognition and which are denied.',
    'If criteria are interpreted narrowly, many liberation movements fail to qualify and the constraint''s protective scope narrows to only the most hierarchically organized groups. If interpreted generously, nearly all organized insurgencies qualify and occupying powers face near-universal obligation to grant combatant status. The ε for occupying powers hinges on this: narrow interpretation → lower extraction (fewer groups qualify); generous interpretation → higher extraction (more groups must be recognized).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_1_4_criteria_contestation, empirical, 'The operative definition of organizational sufficiency for Article 1(4) qualification.').

omega_variable(
    selective_enforcement_geopolitical_bias,
    'Does the constraint operate uniformly across all occupation scenarios, or is enforcement selective based on geopolitical alignment and power asymmetries?',
    'Comparative analysis of ICRC determinations, state recognition patterns, and international pressure across different occupation contexts (Palestinians, Kurds, Uyghurs, etc.)—mapping which movements receive combatant-status recognition and which are denied despite similar organizational profiles.',
    'If enforcement is geopolitically biased (powerful states'' allies deny combatant status even when criteria are met, while others grant it readily), the constraint''s extractiveness is actually much higher for occupying powers aligned with powerful states (they escape obligation through political protection) and much lower for isolated occupying powers (they face full enforcement). This would reframe the constraint as selectively extractive—a structural cover story where formal equality masks actual power-based enforcement. Current measurement assumes uniform enforcement; evidence of bias would necessitate per-seat directionality overrides.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_geopolitical_bias, empirical, 'Whether the constraint''s enforcement mechanism is neutral across geopolitical contexts or biased toward powerful-state preferences.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can the national_liberation_reading coexist with the state_centric_reading, or does one logically foreclose the other?',
    'Theoretical analysis: the readings rest on different foundational premises about who can be a combatant (non-state groups can meet criteria vs. only states qualify by definition). Within a single legal framework, can both be true? Or must a state choose one and reject the other?',
    'If foreclosure is correct, the two readings are incompatible and the constraint cannot simultaneously recognize national liberation movements as combatants while maintaining the state-centric position. If coexistence is correct, they can both be held across different parties and different time periods. The classification hinges on this: if the readings foreclose each other, the judicial and regulatory contestation over which reading applies is itself a high-extraction mechanism (occupying powers must fight to maintain the state-centric reading to deny combatant status). If they coexist, the constraint permits peaceful pluralism where different states apply different readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether the national_liberation_reading and state_centric_reading are logically incompatible or can both be held as legitimate positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.12).
narrative_ontology:measurement_basis(comb_tr_t1977, observed).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__national_liberation_reading, theater_ratio, 1990, 0.16).
narrative_ontology:measurement_basis(comb_tr_t1990, observed).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__national_liberation_reading, theater_ratio, 2001, 0.21).
narrative_ontology:measurement_basis(comb_tr_t2001, observed).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__national_liberation_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement_basis(comb_tr_t2010, observed).
narrative_ontology:measurement(comb_tr_t2018, combatant_status_definition__national_liberation_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement_basis(comb_tr_t2018, observed).
narrative_ontology:measurement(comb_tr_t2026, combatant_status_definition__national_liberation_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(comb_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.52).
narrative_ontology:measurement_basis(comb_be_t1977, observed).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__national_liberation_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement_basis(comb_be_t1990, observed).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__national_liberation_reading, base_extractiveness, 2001, 0.63).
narrative_ontology:measurement_basis(comb_be_t2001, observed).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__national_liberation_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement_basis(comb_be_t2010, observed).
narrative_ontology:measurement(comb_be_t2018, combatant_status_definition__national_liberation_reading, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement_basis(comb_be_t2018, observed).
narrative_ontology:measurement(comb_be_t2026, combatant_status_definition__national_liberation_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(comb_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.55).
narrative_ontology:measurement_basis(comb_su_t1977, observed).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__national_liberation_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement_basis(comb_su_t1990, observed).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__national_liberation_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement_basis(comb_su_t2001, observed).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__national_liberation_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement_basis(comb_su_t2010, observed).
narrative_ontology:measurement(comb_su_t2018, combatant_status_definition__national_liberation_reading, suppression_requirement, 2018, 0.71).
narrative_ontology:measurement_basis(comb_su_t2018, observed).
narrative_ontology:measurement(comb_su_t2026, combatant_status_definition__national_liberation_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(comb_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__national_liberation_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% The constraint family 'combatant_status_definition' decomposes into three structurally distinct readings of the AP I Article 1(4) kernel. Each reading instantiates a different constraint with a different ε and beneficiary/victim structure. The national_liberation_reading (this constraint) extends combatant status to non-state actors meeting organizational criteria (moderate ε for liberation movements, high ε for occupying powers). The state_centric_reading denies non-state combatant status categorically (low ε for liberation movements, low ε for occupying powers—no extraction because no obligation). The functional_protection_reading grants minimum humanitarian protections regardless of combatant status (low ε overall—the constraint operates as human dignity rather than extraction). The three readings are linked because they offer competing interpretations of the same legal kernel and parties invoke whichever reading serves their interests. The national_liberation_reading influences both siblings: if liberation movements are recognized as combatants (this reading), the state-centric reading becomes harder to defend, and the functional-protection reading becomes partially superseded (better protections available via combatant status than via minimum standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
