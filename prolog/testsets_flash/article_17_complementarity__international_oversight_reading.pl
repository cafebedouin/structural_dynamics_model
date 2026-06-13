% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: ICC Article 17 Complementarity (International Oversight Reading)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'international oversight' reading of ICC
 *   Article 17's complementarity principle, where the ICC acts as a guardian
 *   against impunity when states fail to genuinely prosecute atrocity crimes.
 *   The 'unwilling or unable' standard is interpreted broadly, allowing the
 *   ICC to intervene in cases of victor's justice, sham proceedings, or elite
 *   immunity. This reading emphasizes the ICC's role in ensuring
 *   accountability over strict deference to national sovereignty.
 *
 * KEY AGENTS:
 *   - international_criminal_court: Agenda setter (institutional/analytical) — interprets and enforces complementarity.
 *   - complicit_states: Payer (institutional/constrained) — targeted by ICC intervention, faces loss of sovereignty.
 *   - victims_in_complicit_states: Beneficiary (powerless/immediate) — receives justice when domestic systems fail.
 *   - perpetrators_of_atrocities: Payer (powerful/constrained) — faces prosecution despite national protection.
 *   - national_sovereignty_advocates: Payer (organized/generational) — resists ICC overreach, defends state autonomy.
 *   - human_rights_advocates: Beneficiary (organized/generational) — supports ICC intervention, pushes for accountability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.65).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.7).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "ICC Article 17 Complementarity (International Oversight Reading)").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, 'eb0052b8-c9fb-444e-9f8b-827fb937dd29').
narrative_ontology:cs_kernel_codification('eb0052b8-c9fb-444e-9f8b-827fb937dd29', fixed_text).
narrative_ontology:cs_authority_grounding('eb0052b8-c9fb-444e-9f8b-827fb937dd29', lineage).
narrative_ontology:cs_interpretation_layer_present('eb0052b8-c9fb-444e-9f8b-827fb937dd29').
narrative_ontology:cs_reading_relation('eb0052b8-c9fb-444e-9f8b-827fb937dd29', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('eb0052b8-c9fb-444e-9f8b-827fb937dd29', foundational, icc_as_court_of_last_resort).
narrative_ontology:cs_axiom_status(icc_as_court_of_last_resort, holdable).
narrative_ontology:cs_axiom_grounding('eb0052b8-c9fb-444e-9f8b-827fb937dd29', icc_as_court_of_last_resort, deontological).
narrative_ontology:cs_axiom('eb0052b8-c9fb-444e-9f8b-827fb937dd29', foundational, genuine_proceedings_test).
narrative_ontology:cs_axiom_status(genuine_proceedings_test, holdable).
narrative_ontology:cs_axiom_grounding('eb0052b8-c9fb-444e-9f8b-827fb937dd29', genuine_proceedings_test, empirically_contingent).
narrative_ontology:cs_reference_frame('eb0052b8-c9fb-444e-9f8b-827fb937dd29', rome_statute_original_intent).
narrative_ontology:cs_drift_state('eb0052b8-c9fb-444e-9f8b-827fb937dd29', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eb0052b8-c9fb-444e-9f8b-827fb937dd29', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, human_rights_advocates).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, complicit_states).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, perpetrators_of_atrocities).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, national_sovereignty_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies Article 17, asserting jurisdiction when national systems are 'unwilling or unable' to genuinely prosecute. Drives investigations and seeks cooperation from states. Its legitimacy depends on upholding international justice.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% States that are unwilling or unable to genuinely prosecute atrocity crimes committed on their territory or by their nationals. They face potential loss of jurisdiction to the ICC, international scrutiny, and demands for cooperation. Their preference is to retain full sovereignty over criminal justice.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, complicit_states, payer,
    institutional, biographical, constrained, national).

% Individuals who have suffered atrocity crimes and whose national justice systems are failing to provide accountability. They rely on the ICC's intervention for justice, reparations, and recognition of their suffering.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_complicit_states, beneficiary,
    powerless, immediate, trapped, local).

% Individuals (often high-ranking officials or military leaders) responsible for atrocity crimes who benefit from national systems that protect them from prosecution. They face arrest warrants, travel bans, and potential trial by the ICC.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, perpetrators_of_atrocities, payer,
    powerful, biographical, constrained, national).

% Legal scholars, political movements, and states that prioritize national sovereignty and resist what they perceive as ICC overreach. They argue for strict deference to national courts and minimal international intervention.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, national_sovereignty_advocates, payer,
    organized, generational, mobile, global).

% NGOs, legal experts, and international organizations that champion human rights and accountability. They actively support the ICC's broad interpretation of complementarity and push for its intervention in cases of impunity.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__international_oversight_reading, international_criminal_court).
narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to ensure accountability for atrocity crimes by establishing a mechanism for the ICC to intervene when national jurisdictions fail to genuinely prosecute, thereby closing impunity gaps.
% TRANSFER_FUNCTION: Transfers jurisdiction and the power to prosecute from national courts to the International Criminal Court in cases where states are 'unwilling or unable' to genuinely carry out investigations or prosecutions. This transfers the burden of justice from failed states to the international community.
% ABSENT_VOICES: Victims of atrocity crimes in states that actively resist ICC jurisdiction, and whose voices are suppressed by national authorities, would strongly advocate for ICC intervention. Their absence from national proceedings is often a sign of the very impunity this reading seeks to address.
% DISAPPEARANCE_RATIONALE: If this broad interpretation of complementarity vanished, the ICC's ability to intervene in cases of state failure would be severely curtailed, leading to a resurgence of impunity for atrocity crimes. National systems, particularly in complicit states, would face less pressure to genuinely prosecute, and victims would lose a crucial avenue for justice. The international criminal justice landscape would fundamentally shift.
% FOUNDING_PROBLEM: The problem of widespread impunity for atrocity crimes, where national justice systems either lacked the capacity or the political will to prosecute, leading to a cycle of violence and injustice.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal scholars, and numerous UN reports consistently corroborate that the problem of impunity remains live, particularly in conflict zones and authoritarian states. While some states have improved their capacity, the political will to prosecute powerful actors often remains absent, necessitating international oversight.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a Tangled Rope because it genuinely coordinates international efforts to end impunity (beneficiaries: victims, human rights advocates) while simultaneously extracting sovereignty and autonomy from states that fail to prosecute (victims: complicit states, perpetrators). The high extractiveness (0.65) reflects the significant cost to states of losing jurisdiction and the potential for prosecution of powerful individuals. Suppression (0.70) is high due to the ICC's active enforcement and the pressure it exerts on states, even if direct enforcement is challenging. Theater ratio (0.20) is relatively low, as this reading emphasizes genuine intervention over symbolic gestures, though some performative deference to national processes may still occur. Resistance (0.75) is high, reflecting strong opposition from states and powerful actors who prefer national primacy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ICC and human rights advocates, this reading of complementarity is a necessary mechanism for justice, ensuring that the most serious crimes do not go unpunished. From the perspective of complicit states and national sovereignty advocates, it is an overreach that infringes on state sovereignty and can be perceived as a form of 'victor's justice' or external imposition. The ICC's power to intervene is seen as a benefit by some and a cost by others.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC and human rights advocates are clear beneficiaries (d near 0.0) as this reading empowers their mission. Victims in complicit states are also beneficiaries, as the constraint provides a path to justice otherwise denied. Complicit states and perpetrators are targets (d near 1.0) as they bear the direct costs of lost jurisdiction and potential prosecution. National sovereignty advocates are also targets, as their core principle is challenged. The active enforcement and the broad interpretation of 'unwilling or unable' drive these directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (which would ignore the significant extraction from states) or a pure Snare (which would ignore the genuine coordination function of ensuring accountability). The 'unwilling or unable' standard, broadly interpreted, is the mechanism that allows the ICC to act as an accountability trigger, preventing mandatrophy where national systems become inert or complicit. The ongoing contestation over its interpretation is central to its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_interpretation_ambiguity,
    'Is the ''unwilling or unable'' standard primarily a shield for state sovereignty or a sword for international accountability?',
    'Consistent jurisprudence from the ICC Appeals Chamber affirming broad interpretation of ''unwilling or unable'' and frequent exercise of jurisdiction over states with ongoing domestic proceedings.',
    'If resolved towards international oversight, the constraint operates as a more effective Snare against impunity; if towards national primacy, it functions more as a Rope for state sovereignty with limited ICC intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_interpretation_ambiguity, conceptual, 'Ambiguity in the interpretation of ICC Article 17''s ''unwilling or unable'' standard.').

omega_variable(
    state_cooperation_enforcement_gap,
    'To what extent can the ICC effectively compel cooperation from states that resist its jurisdiction under this broad interpretation?',
    'Analysis of enforcement mechanisms (e.g., UN Security Council referrals, diplomatic pressure, targeted sanctions) and their success rate in securing arrests and evidence from non-cooperating states.',
    'If enforcement is weak, the constraint''s effective suppression is lower, reducing its Snare-like qualities and allowing states to maintain impunity through non-cooperation. If strong, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_cooperation_enforcement_gap, empirical, 'The practical limits of ICC''s enforcement power against resistant states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__international_oversight_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__international_oversight_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__international_oversight_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__international_oversight_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__international_oversight_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__international_oversight_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__international_oversight_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__international_oversight_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__international_oversight_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of ICC Article 17 complementarity. The 'national primacy' reading is a sibling constraint, linked by this network edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
