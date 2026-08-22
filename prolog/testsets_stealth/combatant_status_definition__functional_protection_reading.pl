% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Status-Gated Detainee Protection Regime (Functional Protection Reading)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   International humanitarian law ties a detainee's protections to a
 *   combatant-status determination: members of state forces captured in
 *   international armed conflict receive POW treatment under Geneva
 *   Convention III, persons in non-international armed conflict hold the
 *   Common Article 3 minimum, and the gaps between and within these regimes —
 *   transnational conflicts, non-state fighters, unilaterally created
 *   categories — are where detainees lose acknowledged protection entirely.
 *   This story authors that standing status-gated arrangement through the
 *   functional_protection_reading of the combatant_status_definition kernel:
 *   the reading holds the humane-treatment and fair-trial floor
 *   status-independent and treats status determination as allocating only the
 *   privileges above that floor. Per the fixed epsilon-referent rule for
 *   kernel readings, extractiveness is authored for the standing arrangement
 *   under contest (the gate itself) as this reading sees it — not for the
 *   reading's endorsed unconditional floor, which is a different constraint;
 *   authoring for the endorsed alternative would flatten every advocacy
 *   reading to near-zero. Constraint family: this story links to its sibling
 *   readings via network edges; the three readings share one referent and
 *   differ in the extractiveness they author against it (the state-centric
 *   reading, which endorses the gate, would author a low value for the same
 *   referent).
 *
 * KEY AGENTS:
 *   - detaining_states: agenda-setting seat (institutional/arbitrage) — controls classification policy and captures the flexibility the gate opens
 *   - status_determination_tribunals: administering seat (organized/constrained) — runs the determinations that gate every protection, inside the detaining power's framework
 *   - state_armed_forces: protected class (institutional/constrained) — their members' immunity and POW treatment ride on status
 *   - lawful_combatant_pows: primary protected beneficiaries (powerless/trapped) — hold protections only at their captor's concession
 *   - nonstate_fighter_detainees: primary bearing seat (powerless/trapped) — hold only the floor their captor acknowledges
 *   - nonstate_armed_group_representatives: excluded seat — no voice in the rules governing their fighters
 *   - icrc: monitoring beneficiary (organized/constrained) — custodian whose mandate and access ride on the regime
 *   - international_courts: analytical observer (institutional/analytical) — case-by-case check on the reading contests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.66).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.6).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Status-Gated Detainee Protection Regime (Functional Protection Reading)").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '49fb79f5-486b-4aa9-af4e-dc9bd0b5c728').
narrative_ontology:cs_kernel_codification('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', fixed_text).
narrative_ontology:cs_authority_grounding('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', distributed).
narrative_ontology:cs_reading_relation('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', foundational, humane_treatment_floor_status_independent).
narrative_ontology:cs_axiom_status(humane_treatment_floor_status_independent, holdable).
narrative_ontology:cs_axiom_grounding('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', humane_treatment_floor_status_independent, deontological).
narrative_ontology:cs_axiom('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', foundational, status_determination_not_precondition_for_protection).
narrative_ontology:cs_axiom_status(status_determination_not_precondition_for_protection, holdable).
narrative_ontology:cs_axiom_grounding('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', status_determination_not_precondition_for_protection, conventional).
narrative_ontology:cs_reference_frame('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', status_independent_protection_floor).
narrative_ontology:cs_drift_state('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', contemporary_detention_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('49fb79f5-486b-4aa9-af4e-dc9bd0b5c728', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detaining_states).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, state_armed_forces).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, lawful_combatant_pows).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, nonstate_fighter_detainees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, status_determination_tribunals).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, icrc).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, combatant_immunity_doctrine).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, iac_niac_classification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sign the Geneva corpus and administer detention in the armed conflicts they fight. They write the classification policy that decides which conflict framework applies, appoint the bodies that determine detainee status, and decide whether to grant ICRC access. Where they concede status, detainees receive the full POW or internment regime; where they contest status, detainees hold only whatever floor the detaining state acknowledges. Their flexibility to reinterpret the corpus — new categories, contested conflict classifications, reservations to protocols — is the arrangement's main pressure valve, and no external body can override their reading.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Run the hearings that decide whether a detainee qualifies for combatant or POW status: status review tribunals, Article 5 inquiries, commission pre-screening. Their determinations gate every protection that follows, yet they operate inside the detaining power's framework — judges, rules of evidence, and appeal routes are set by the same authority whose classification they review. Their continued existence and staffing depend on the determination machinery remaining in use.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, status_determination_tribunals, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, status_determination_tribunals, beneficiary).

% Fight the conflicts in which the status rules operate. Their members, when captured while serving in state forces, receive POW treatment: combatant immunity, named-camp detention, repatriation at hostilities' end. The officer corps trains on the law-of-war framework and its professional identity is bound up with lawful-conduct standards; abandoning the framework entirely would expose their own captured soldiers to the captor's unstructured discretion.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_armed_forces, beneficiary,
    institutional, generational, constrained, global).

% Captured state soldiers in enemy hands. Their treatment — camp conditions, no interrogation beyond name and rank, repatriation — flows entirely from the status their captor concedes them. They cannot exit detention, cannot contest their captor's classification decisions, and hold no voice in the rules that determine their status; their protection is wholly derivative of the determination going their way.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, lawful_combatant_pows, beneficiary,
    powerless, biographical, trapped, regional).

% Captured members of non-state armed groups. No treaty text concedes them POW status, and in conflicts their captor classifies outside the standard frameworks they have been held to receive no acknowledged floor at all: indefinite detention, interrogation without counsel, prosecution by commission. Their protections are whatever their captor concedes after whatever determination process the captor designs; they cannot exit, cannot reach a forum their captor recognizes, and their groups cannot negotiate on their behalf.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, nonstate_fighter_detainees, payer,
    powerless, immediate, trapped, regional).

% Command structures and political wings of the armed groups whose fighters fill the detention facilities. They have no seat in the treaty conferences where status categories are drafted, no standing in the detaining power's determination proceedings, and no channel to negotiate protection terms for their captured members; their only leverage is the continuation of the conflict itself.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, nonstate_armed_group_representatives, excluded,
    organized, biographical, trapped, regional).

% Custodian of the Geneva corpus and the only institution with a treaty mandate to visit detainees in armed conflict. Its access agreements, funding, and institutional purpose all flow from the regime it monitors, and its confidential reporting is the main external check on detaining-power conduct. It argues the floor applies to every detainee regardless of status, but it cannot compel access where a detaining power withholds consent, and its mandate depends on staying inside the consent-based framework.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, icrc, beneficiary,
    organized, generational, constrained, global).

% Adjudicate the reading contests when litigation reaches them: whether Common Article 3 applies to a transnational conflict, whether detainees may challenge their classification, what a determination process must look like. They can check a detaining power's reading case by case but cannot set classification policy itself, and their jurisdiction depends on the parties' consent structures.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_courts, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__functional_protection_reading, detaining_states).
narrative_ontology:fixing_cost_class(combatant_status_definition__functional_protection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates combatant immunity and detention-treatment obligations across parties to an armed conflict: status determination decides who may be prosecuted for fighting, who must be repatriated without prosecution, and which treatment regime (POW, civilian internment, or non-international-armed-conflict floor) applies to each captured person.
% TRANSFER_FUNCTION: Moves protection itself. The standing arrangement makes the scope of a detainee's protections a function of a status determination the detaining power controls: protection flows toward detainees whose status is conceded (state forces' members in international armed conflict) and away from detainees whose status the detaining power contests. Above the floor, it moves prosecutorial discretion and interrogation latitude toward detaining powers.
% ABSENT_VOICES: Non-state armed groups and the detainees themselves have no seat in the rule-setting conversation: the status categories were negotiated among states; new categories are created unilaterally by detaining powers; determination proceedings have run without counsel. The people whose protections the gate allocates do not participate in defining it. The ordinary coalition route for a powerless class — organized negotiation through its representatives — is itself foreclosed, because the groups are excluded and their only leverage is the conflict's continuation.
% DISAPPEARANCE_RATIONALE: Every party's treatment and prosecution of captured fighters currently runs through the status determination. Remove the arrangement overnight and combatant immunity, POW repatriation obligations, and the international/non-international treatment split all lose their allocation mechanism — captured fighters everywhere would hold protections only at their captor's unstructured discretion, which is the pre-Geneva problem the arrangement was built to manage.
% FOUNDING_PROBLEM: Nineteenth- and early-twentieth-century war left captured fighters at the captor's discretion: summary execution, reprisal, unlimited interrogation. The Geneva corpus was built to fix minimum treatment and, for lawful combatants, full POW status; Common Article 3 was written in 1949 to extend a minimum floor to conflicts not between states, where no POW regime exists.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: ICRC detention visits and reporting (a mandate independent of any party to the conflicts), UN human-rights mechanisms, and the litigation record all attest that the founding problem — captured persons at captor discretion — recurs wherever the floor is contested. Detaining states contest the floor's scope but do not dispute that unregulated wartime detention was the problem the corpus was built to solve.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66 is authored for the standing status-gated arrangement per this reading's own lights: the gate converts protection from an attachment of detention into a concession, and the concession is withheld precisely in the conflicts where latitude is most sought (asymmetric and transnational conflicts). Suppression 0.6 is a raw structural value, unscaled by power or scope: the gate's force is the closure of detainee-side alternatives — determination proceedings without counsel, classification gaps constructed so no framework applies, ICRC access withheld — not any internalized acceptance; detainees litigate against the gate wherever a forum will hear them. Theater 0.45: status determination has a real function in ordinary conflicts (Article 5 inquiries, POW registration), but in the contested cases the machinery is largely performative — review boards convened after the classification is fixed. Accessibility_collapse 0.35: the alternatives to the gate are legible and live — Common Article 3's text is status-independent, Additional Protocol I extends status for ratifiers, the customary-law argument is on the table — so the arrangement persists by contesting them, not by collapsing them. Resistance 0.6: ICRC advocacy, UN mechanisms, and the Hamdan and Boumediene litigation line meet the gate continuously. The claimed type (tangled_rope) is an independent structural judgment: the gate does real coordination work — combatant-immunity allocation, POW privileges, the international/non-international treatment split — and the same structure carries the asymmetric bearing described above; neither a pure-coordination nor a pure-cover reading fits. The measurement series run on one shared time grid (1949-2025) so every tracked metric is authored at every examined point; the series show the 2001-2006 enforcement build-up and extraction spike, the partial judicial check, and the residual elevated state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the detaining-state seat the arrangement is a coordination framework it administers: status rules let it distinguish prosecutable fighters from immune soldiers and honor reciprocity for its own captured personnel. From the lawful-combatant seat the same structure is protective — their immunity and camp regime exist only because status runs their way. From the nonstate-fighter seat the identical structure operates as a gate that withholds every protection its captor chooses to withhold. The engine computes these per-seat classifications from the structural data; the divergence between the state seat and the detainee seats is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: detaining_states (capture the flexibility the gate opens — interpretive arbitrage, interrogation and prosecutorial latitude), state_armed_forces (their members' immunity and POW treatment), lawful_combatant_pows (protected while status runs their way). Victim declaration: nonstate_fighter_detainees (no conceded status, contested floor, prosecution exposure). The derivation maps detaining_states near the beneficiary end (declared beneficiary plus arbitrage-grade exit: they reinterpret, reserve, and reclassify at will) and nonstate_fighter_detainees near the full-target end (declared victim plus trapped: detention until the conflict ends or the commission convenes). Lawful_combatant_pows are trapped but beneficiaries; the derivation should read their direction from the beneficiary declaration rather than from their capture. No directionality override is authored because overrides key on power atom, and this story has both a powerless beneficiary and a powerless victim at the same atom — the beneficiary and victim declarations carry the differentiation instead. The ICRC sits near-symmetric: it collects mandate and access from the regime while spending that access contesting the gate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — captured persons at the captor's unregulated discretion — is live: every current armed conflict detains someone, and the status contest recurs in each. The classification therefore must not resolve to a degraded type. The tangled_rope reading does preventive work in both directions: against mislabeling the arrangement a snare (the coordination story is not cover — immunity allocation and the treatment-regime split are real functions no alternative currently performs), and against mislabeling it a rope (the gate's bearing is asymmetric, actively enforced, and concentrated on a named class). The genealogy mismatch check runs clean: founding_problem_status live with disappearance_verdict world_rearranges — the arrangement persists because the problem persists, not because a dead mandate is performed theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the functional_protection_reading of kernel combatant_status_definition. What does each sibling reading change structurally, and how does the instantiated rule''s profile differ from the standing arrangement''s profile under this reading?',
    'Comparative classification of the sibling stories: the state-centric reading gates protection behind Article 4 organization criteria and, in its practiced form, behind conflict classification; the national-liberation reading extends status to AP I Article 1(4) forces; this reading severs the Common Article 3 floor from status entirely, so the rule it instantiates would carry near-zero burden on detainees.',
    'The authored extractiveness (0.66) is for the standing status-gated arrangement per this reading''s own lights, not for the reading''s endorsed unconditional floor (which would sit near zero); resolving the delta question locates which protection layer each reading actually contests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings of the combatant-status kernel; the siblings change which protection layer status gates.').

omega_variable(
    status_gate_separability,
    'Is the protection function of the humane-treatment floor separable from the status-determination machinery — can combatant-immunity allocation continue while the floor detaches from status?',
    'Observe non-international armed conflict practice where the Common Article 3 floor is honored for all detainees while domestic prosecution of fighters proceeds on conduct grounds rather than status grounds; compare protection outcomes with status-gated practice in comparable conflicts.',
    'If separable, the gate''s burden on non-state detainees is removable without losing the immunity-allocation function; if inseparable, part of the measured burden is the price of the coordination the gate performs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_gate_separability, conceptual, 'Whether the floor''s protective function and the status machinery are structurally separable.').

omega_variable(
    customary_floor_status,
    'Is the status-independent humane-treatment floor customary international law binding all states, or only treaty law binding ratifiers with contested scope?',
    'State practice and opinio juris surveys, including the ICRC customary international humanitarian law study and catalogued state objections and reservations.',
    'If customary, the gate operates in violation of standing law and the arrangement''s suppressive force reads higher; if treaty-only, the gate persists lawfully for non-ratifying detaining powers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_floor_status, empirical, 'Legal source of the status-independent floor: custom or treaty.').

omega_variable(
    enforcement_rebuild_risk,
    'Will the judicial check on the gate (the Hamdan and Boumediene line) hold through the next major detention episode involving a non-state adversary?',
    'Observe the classification machinery constructed in the next major armed conflict with non-state detainees: whether determination tribunals, new categories, and access denials reappear.',
    'Machinery rebuild turns the suppression_requirement trajectory upward again and pushes the arrangement toward heavier bearing; a durable check stabilizes it as contested coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_rebuild_risk, empirical, 'Durability of the post-2006 judicial check against enforcement rebuild.').

omega_variable(
    reciprocity_anchor_weight,
    'How much of the status gate''s stability rests on reciprocity (each state''s own captured soldiers protected when it honors status) versus on the latitude the gate opens?',
    'Compare state conduct in reciprocal international armed conflicts against conduct in asymmetric conflicts where the state holds no reciprocity stake in its captured personnel.',
    'If reciprocity-dominant, the gate is mostly protective and the state seat computes a low-burden coordination framework; if latitude-dominant, the gate''s burden concentrates exactly where reciprocity is absent, supporting the higher reading of its extractive component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocity_anchor_weight, empirical, 'Relative weight of reciprocity versus latitude in sustaining the gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__functional_protection_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement_basis(comb_tr_t1949, observed).
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__functional_protection_reading, theater_ratio, 1977, 0.18).
narrative_ontology:measurement_basis(comb_tr_t1977, observed).
narrative_ontology:measurement(comb_tr_t1991, combatant_status_definition__functional_protection_reading, theater_ratio, 1991, 0.2).
narrative_ontology:measurement_basis(comb_tr_t1991, observed).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__functional_protection_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement_basis(comb_tr_t2001, observed).
narrative_ontology:measurement(comb_tr_t2006, combatant_status_definition__functional_protection_reading, theater_ratio, 2006, 0.5).
narrative_ontology:measurement_basis(comb_tr_t2006, observed).
narrative_ontology:measurement(comb_tr_t2014, combatant_status_definition__functional_protection_reading, theater_ratio, 2014, 0.42).
narrative_ontology:measurement_basis(comb_tr_t2014, observed).
narrative_ontology:measurement(comb_tr_t2025, combatant_status_definition__functional_protection_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(comb_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__functional_protection_reading, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement_basis(comb_be_t1949, observed).
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__functional_protection_reading, base_extractiveness, 1977, 0.32).
narrative_ontology:measurement_basis(comb_be_t1977, observed).
narrative_ontology:measurement(comb_be_t1991, combatant_status_definition__functional_protection_reading, base_extractiveness, 1991, 0.3).
narrative_ontology:measurement_basis(comb_be_t1991, observed).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__functional_protection_reading, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement_basis(comb_be_t2001, observed).
narrative_ontology:measurement(comb_be_t2006, combatant_status_definition__functional_protection_reading, base_extractiveness, 2006, 0.7).
narrative_ontology:measurement_basis(comb_be_t2006, observed).
narrative_ontology:measurement(comb_be_t2014, combatant_status_definition__functional_protection_reading, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement_basis(comb_be_t2014, observed).
narrative_ontology:measurement(comb_be_t2025, combatant_status_definition__functional_protection_reading, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(comb_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__functional_protection_reading, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement_basis(comb_su_t1949, observed).
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__functional_protection_reading, suppression_requirement, 1977, 0.25).
narrative_ontology:measurement_basis(comb_su_t1977, observed).
narrative_ontology:measurement(comb_su_t1991, combatant_status_definition__functional_protection_reading, suppression_requirement, 1991, 0.25).
narrative_ontology:measurement_basis(comb_su_t1991, observed).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__functional_protection_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement_basis(comb_su_t2001, observed).
narrative_ontology:measurement(comb_su_t2006, combatant_status_definition__functional_protection_reading, suppression_requirement, 2006, 0.7).
narrative_ontology:measurement_basis(comb_su_t2006, observed).
narrative_ontology:measurement(comb_su_t2014, combatant_status_definition__functional_protection_reading, suppression_requirement, 2014, 0.62).
narrative_ontology:measurement_basis(comb_su_t2014, observed).
narrative_ontology:measurement(comb_su_t2025, combatant_status_definition__functional_protection_reading, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement_basis(comb_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'combatant status in international humanitarian law' decomposes into three structurally distinct readings of one kernel (combatant_status_definition): the state-centric reading gates POW protection behind Article 4 organization criteria; the national-liberation reading extends status to Additional Protocol I Article 1(4) forces; the functional-protection reading (this file) severs the Common Article 3 floor from status entirely. The readings share one referent — the status-gated protection arrangement — and author different extractiveness against it. They are linked as a constraint family, with the 1949 state-centric text upstream of both later readings; this reading's judicial wins (the Hamdan line) exert structural downstream pressure on the state-centric reading's operating environment without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
