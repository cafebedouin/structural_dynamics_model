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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: Combatant Status for National Liberation Movements (AP I Article 1(4))
 *   domain: international_humanitarian_law/armed_conflict
 *
 * SUMMARY:
 *   AP I Article 1(4) extends combatant status to non-state armed groups
 *   fighting 'colonial and other forms of domination and foreign occupation'
 *   and 'racist regimes' if they meet specified criteria: organized command
 *   structure, responsible leadership, distinctive insignia, and conducting
 *   operations in accordance with the laws of war. The national-liberation
 *   reading takes this text as a mandate to recognize liberation movements'
 *   combatant status and POW protections, legitimating armed resistance to
 *   colonialism, occupation, and racial rule. This is contested by
 *   state-centric readings (which hold that only formal state militaries
 *   qualify for combatant status) and by functional-protection readings
 *   (which hold that all detainees receive humanitarian protections
 *   regardless of status). The constraint story authored here is the
 *   national-liberation reading alone, not the contest; the reading's ε
 *   reflects what the constraint extracts from occupying powers' perspective
 *   (obligation to grant status) and what it extends to liberation movements
 *   (legal recognition and protections).
 *
 * KEY AGENTS:
 *   - national_liberation_movements: armed groups fighting colonial, occupying, or racist regimes; gain combatant status and POW protections if meeting AP I 1(4) criteria; must maintain organized command, distinctive insignia, and law-of-war compliance
 *   - occupying_state_militaries: must grant combatant status and POW protections to qualifying liberation movements; constrained in operational latitude; face international accountability for violations
 *   - occupied_or_colonized_populations: gain legal recognition of their right to armed resistance and self-determination through the reading; benefit from combatants' law-of-war constraints; depend on their liberation movement's actual compliance
 *   - state_actors_resisting_liberation_recognition: states that deny liberation status or refuse AP I 1(4) compliance; constrained by international legal obligations; face diplomatic and legal pressure
 *   - international_humanitarian_law_bodies: ICRC, UN bodies, international courts; interpret and enforce AP I 1(4); verify criteria and assess compliance
 *   - captured_combatants: gain POW protections upon capture if status is recognized; vulnerable to denial if occupying power contests their status; protected by international humanitarian law if compliance holds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.68).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.72).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "Combatant Status for National Liberation Movements (AP I Article 1(4))").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law/armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '24a6b6fa-fcf7-473b-b2d4-4bb36c579c17').
narrative_ontology:cs_kernel_codification('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', fixed_text).
narrative_ontology:cs_authority_grounding('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', lineage).
narrative_ontology:cs_interpretation_layer_present('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17').
narrative_ontology:cs_reading_relation('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', foundational, right_to_armed_resistance_against_colonial_occupation_racism).
narrative_ontology:cs_axiom_status(right_to_armed_resistance_against_colonial_occupation_racism, holdable).
narrative_ontology:cs_axiom_grounding('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', right_to_armed_resistance_against_colonial_occupation_racism, deontological).
narrative_ontology:cs_axiom('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', secondary, non_state_combatant_status_conditional_on_criteria).
narrative_ontology:cs_axiom_status(non_state_combatant_status_conditional_on_criteria, holdable).
narrative_ontology:cs_axiom_grounding('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', non_state_combatant_status_conditional_on_criteria, conventional).
narrative_ontology:cs_reference_frame('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', ap_i_article_1_4_mandate).
narrative_ontology:cs_drift_state('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', contemporary_conflicts_2020_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('24a6b6fa-fcf7-473b-b2d4-4bb36c579c17', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, occupied_or_colonized_populations).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_state_militaries).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, state_actors_resisting_liberation_recognition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, captured_combatants).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_state_governments).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonizing_and_racist_regimes).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, civilian_populations_in_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Armed groups fighting colonial, occupation, or racist-regime rule gain conditional combatant status and POW protections if meeting AP I Article 1(4) criteria: organized command structure, responsible leadership, distinctive insignia, open carrying of arms, and conducting operations in accordance with laws of war. Recognizing their combatant status confers legal immunity for lawful acts of war and POW protections upon capture, transforming their status from 'unlawful combatants' or 'terrorists' to lawful combatants. This reading legitimates their resistance framework and provides legal protections, but requires them to meet and maintain discipline criteria and accept the constraint's verification burden.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, national_liberation_movements, payer).

% Populations under colonial, occupation, or racist-regime rule gain the legal framing that armed resistance on their behalf is a legitimate exercise of the right to self-determination. The constraint's operation vindicates their claim to self-government and provides a legal vocabulary for their struggle. However, they also carry the risk that combatants meeting the criteria will be held to military discipline, and the beneficiary framing depends on the liberation movement's actual adherence to the law-of-war criteria.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupied_or_colonized_populations, beneficiary,
    moderate, generational, identity_locked, regional).

% Must grant combatant status and POW protections to non-state armed groups that meet AP I Article 1(4) criteria, even though these groups may be fighting to end their occupation or rule. This obligation constrains their operational latitude (they cannot categorically deny POW status to captured combatants) and exposes them to international accountability if they violate the duty. The constraint extracts recognition and legal concessions from the occupying power's perspective, which is why it is heavily resisted by many occupying states.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_state_militaries, payer,
    institutional, generational, constrained, regional).

% States that reject the legitimacy of national liberation movements or wish to categorically deny POW status to insurgents are structurally disadvantaged by this reading. They must either accept the constraint (granting combatant status and POW protections) or explicitly violate AP I Article 1(4), which exposes them to International Court claims and diplomatic pressure. Their exit is constrained by international legal obligations and the political cost of flagrant non-compliance.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, state_actors_resisting_liberation_recognition, payer,
    institutional, generational, constrained, global).

% The International Committee of the Red Cross, UN bodies, and international courts interpret and enforce AP I Article 1(4). They determine whether a movement meets the criteria (organized command, distinctive insignia, open arms, law-of-war compliance), verify adherence, and adjudicate disputes. Their interpretation of the criteria has substantial de facto authority, though actual enforcement depends on state cooperation and political will.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_humanitarian_law_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Political leadership of occupying or colonial powers faces pressure to grant combatant status to movements meeting the criteria, constraining their ability to frame insurgents as criminals or terrorists. This political constraint is part of the extraction the reading exerts: legitimacy is transferred from the occupier to the liberation movement, reducing the occupier's narrative control over the conflict.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_state_governments, payer,
    institutional, generational, constrained, regional).

% Regimes built on colonial domination or racial hierarchy must either accept the reading's implication that liberation movements have a right to armed resistance, or reject AP I entirely and face international isolation. The constraint denies them the legal framing that colonialism or racist rule is legitimate governance, extracting de facto recognition of the populations' right to self-determination.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, colonizing_and_racist_regimes, payer,
    institutional, generational, constrained, regional).

% Combatants from liberation movements who meet AP I Article 1(4) criteria gain POW protections upon capture: immunity from prosecution for lawful acts of war, humane treatment, fair trial before any criminal prosecution, and repatriation after conflict end. Without this status, they face summary execution, torture, or indefinite detention. The constraint provides legal protection but only if the capturing power acknowledges their combatant status — enforcement depends on international monitoring and the capturing power's compliance.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, captured_combatants, beneficiary,
    powerless, biographical, trapped, local).

% Gain protection because lawful combatants are subject to the laws of war (targeting distinction, proportionality); but also bear costs of armed conflict. The constraint creates legal obligation for combatants to follow targeting rules, which is a beneficiary frame, but civilians remain exposed to the conflict itself. They benefit from the law-of-war constraint on combatants, but the constraint does not reduce their exposure to combat.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, civilian_populations_in_conflict_zones, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, civilian_populations_in_conflict_zones, beneficiary).

% Armed groups fighting non-colonial, non-occupying states, or groups fighting within a state they do not claim to be liberating (e.g., secessionist movements in non-colonial contexts, religious extremist groups) are excluded from the national-liberation reading's scope. They do not meet the AP I Article 1(4) criteria because they are not resisting colonialism, occupation, or racist regimes. They would argue the reading should extend to their struggle, but are categorically excluded by the reading's structural definition of who has a right to use the combatant-status pathway.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, excluded_non_liberation_insurgencies, excluded,
    organized, biographical, constrained, regional).

% International courts, human rights bodies, and fact-finding commissions assess compliance with the reading's terms and publish findings. They have analytical standing but limited enforcement power without state cooperation. Their role is to characterize the constraint's operation and flag violations, not to adjudicate the reading's legitimacy.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_court_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, occupying_state_governments).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the conduct of armed conflict by defining who qualifies as a lawful combatant and thus who is subject to the laws of war, targeting rules, and POW protections. Solves the problem of distinguishing lawful from unlawful combatants, establishing a legal framework for warfare, and creating incentives for compliance with humanitarian law through the grant of immunity and protections.
% TRANSFER_FUNCTION: Transfers legal status and protections from the occupying/colonizing state to the liberation movement and its combatants: recognition of combatant status, POW protections upon capture, immunity from prosecution for lawful acts of war, and legal legitimacy for the armed struggle. Moves de facto recognition of the right to self-determination from the occupier's claimed sovereignty to the occupied population's claim to liberation.
% ABSENT_VOICES: Non-liberation armed groups (separatists not resisting colonialism, criminal insurgencies, religious extremist organizations) would object to their exclusion from the combatant-status pathway; they are structurally kept outside the reading's scope. Occupying powers and colonial regimes would contest the reading's premise that liberation movements have a right to armed resistance; they are present in the debate but on the losing end of the reading's logic. Civilians in conflict zones might argue the law-of-war distinction is insufficient protection given the casualty asymmetries in counter-insurgency warfare.
% DISAPPEARANCE_RATIONALE: If AP I Article 1(4) and its national-liberation reading disappeared, the legal status of non-state armed groups fighting occupying powers would revert to categorical non-combatant status under the state-centric reading. Captured insurgents would lose POW protections and face criminal prosecution without immunity for lawful acts of war. Occupying and colonial powers would regain narrative and legal control over framing resistance as terrorism or crime. The political landscape of decolonization and anti-occupation struggles would reorganize around illegality rather than rights, and international humanitarian law would no longer recognize a pathway for non-state actors to achieve combatant status.
% FOUNDING_PROBLEM: Colonial and occupying powers denied recognition of legitimate resistance, framing anti-colonial and anti-occupation armed movements as criminal or terrorist organizations and refusing to grant captured combatants POW protections. This left insurgents with no legal status and no protections, creating systematic violations of humanitarian law and removing the legal framework that incentivized compliance with the laws of war.
% FOUNDING_PROBLEM_CORROBORATION: International humanitarian law scholars, the International Committee of the Red Cross, and UN bodies affirm that the founding problem was real: colonial and occupying powers systematically denied POW status to resistance fighters. However, occupying and colonial powers argue that the problem was not systematic denial but rather the refusal of insurgents to meet combatant criteria (distinctive insignia, open arms, organized command, law-of-war compliance). Post-colonial states and liberation movements affirm the problem was real and that AP I Article 1(4) was necessary to remedy it; occupying powers in contemporary conflicts (Israel regarding Palestinian armed groups, Myanmar regarding armed resistance, etc.) argue the problem has been overstated and that the criteria remain unmet by the groups in question.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.68 because the reading imposes a substantial obligation on occupying powers to grant combatant status and POW protections to non-state armed groups, which is framed by occupying powers as an extraction (loss of legal control, mandatory protections for insurgents). From the reading's own perspective, this is not extractive — it is correcting a structural imbalance where occupying powers denied protections. But from the occupying power's perspective, the obligation to recognize status is extractive because it removes their ability to categorically deny POW protections. Suppression is high (0.72) because the constraint's persistence depends on active enforcement by international bodies against occupying powers' resistance: occupying powers frequently deny or contest combatant status despite meeting criteria, requiring ICRC intervention, fact-finding missions, and court proceedings. Theater is moderate (0.42) because the reading includes genuine law-of-war coordination (rules for conduct of armed conflict, targeting distinctions) but also ceremonial aspects: many occupying powers grant nominal compliance with AP I 1(4) language while denying status in practice through contested criteria or delayed recognition. Accessibility collapse is moderate (0.61) because the reading does provide an alternative pathway for non-state actors (the criteria are knowable and achievable in principle), but occupying powers' power to contest verification creates a collapse in practical accessibility. Resistance is high (0.74) because occupying powers actively resist the reading through denial of status, legal contests, and non-compliance with POW protections.
 *
 * PERSPECTIVAL GAP:
 *   The national-liberation-movement seat and the occupied-population seat compute the reading as genuinely coordinating (establishing rules for conduct and protections for fighters), with asymmetry being corrective rather than extractive. The occupying-power seat computes the reading as extractive (losing legal control, mandatory recognitions, constrained operational freedom). The international-humanitarian-law-bodies seat computes the reading as a coordination mechanism that has been persistently violated by occupying powers. From the state-centric-reading seat, the constraint is a false liberation of non-state actors from lawfulness. The engine computes directionality from the structural data: occupying powers are targets (high d, high extraction); liberation movements are beneficiaries (low d, positive coordination); captured combatants are trapped beneficiaries (identity-locked, powerless, gain protections but depend on recognition). This divergence shows why the constraint is contested and why occupying powers resist even as international bodies affirm the reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (national_liberation_movements, occupied_or_colonized_populations, captured_combatants): gain legal status, POW protections, immunity for lawful acts of war, and recognition of right to self-determination. Their directionality should be low (near 0.0) because they receive benefits without running the enforcement mechanism. However, identity_locked exit modulates this: liberation movements cannot exit the struggle (it is their identity), so they are constrained-exit actors depending on international recognition and occupying-power compliance. This pushes directionality upward from the pure beneficiary position (toward 0.3–0.4) because the benefit is conditional on ongoing verification and occupying-power cooperation. Victims (occupying_state_militaries, state_actors_resisting_liberation_recognition): bear the cost of recognizing and granting status, constrained exit (cannot deny AP I 1(4) obligations without international violation), institutional power (high). Directionality high (near 0.8–0.9) because they are the targets of the obligation and cannot easily exit. Overrides: None declared, as the structural derivation from beneficiary/victim declarations and exit options produces accurate d values. The asymmetry in exit options (occupying powers constrained by international law; liberation movements identity-locked to their struggle) creates the directionality spread.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (colonial and occupying powers denied POW status to resistance fighters, creating humanitarian violations) is live and contested. Occupying powers still resist AP I 1(4) in contemporary conflicts (Israel/Palestine, Myanmar/Rohingya conflicts, etc.), contesting whether movements meet criteria or arguing criteria are unmet. The founding problem persists because verification is weak and occupying powers have power to contest. This prevents mandatrophy: the constraint is not a zombie (the problem it addresses is still live), but active enforcement is required to maintain it. The reading functions as tangled_rope because it has both genuine coordination (the law-of-war framework for conduct) and extraction (the obligation imposed on occupying powers to grant status). The coordination is not separable from the extraction: the protections for combatants (and thus the incentive to comply with laws of war) depend on the same status that occupying powers are obligated to grant. This is the defining characteristic of tangled_rope: both parties are part of the same legal structure, one benefits from coordination (rule-based conduct), and one bears extraction (obligation to recognize status). Mandatrophy is not resolved because the founding problem remains live in contemporary conflicts and the constraint continues to require active international enforcement against occupying-power resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__national_liberation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comb_tr_t8, combatant_status_definition__national_liberation_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(comb_tr_t16, combatant_status_definition__national_liberation_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(comb_tr_t24, combatant_status_definition__national_liberation_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(comb_tr_t32, combatant_status_definition__national_liberation_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(comb_tr_t40, combatant_status_definition__national_liberation_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(comb_tr_t50, combatant_status_definition__national_liberation_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__national_liberation_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(comb_be_t8, combatant_status_definition__national_liberation_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(comb_be_t16, combatant_status_definition__national_liberation_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(comb_be_t24, combatant_status_definition__national_liberation_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(comb_be_t32, combatant_status_definition__national_liberation_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(comb_be_t40, combatant_status_definition__national_liberation_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(comb_be_t50, combatant_status_definition__national_liberation_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__national_liberation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comb_su_t8, combatant_status_definition__national_liberation_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(comb_su_t16, combatant_status_definition__national_liberation_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(comb_su_t24, combatant_status_definition__national_liberation_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(comb_su_t32, combatant_status_definition__national_liberation_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(comb_su_t40, combatant_status_definition__national_liberation_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(comb_su_t50, combatant_status_definition__national_liberation_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__national_liberation_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, pow_protections_enforcement).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, captured_fighter_legal_status).

% DUAL FORMULATION NOTE:
% The combatant_status_definition kernel has three readings, each instantiating a different constraint: national_liberation_reading (this story) extends combatant status to non-state groups meeting AP I 1(4) criteria; state_centric_reading restricts status to formal state militaries; functional_protection_reading makes the status question secondary to minimum humanitarian protections for all detainees. These three readings are linked as constraint family members, with the national-liberation reading upstream of the state-centric reading (the liberation reading's affirmation of non-state combatant status directly challenges the state-centric premise). The functional-protection reading coexists with the national-liberation reading: both can be affirmed in the same legal framework. Each reading has its own ε and stakeholder structure, reflecting different interpretations of the same kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
