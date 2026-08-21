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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: ICC Article 17 Complementarity (International Oversight Reading)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'international oversight' reading of
 *   Article 17 complementarity, where the International Criminal Court (ICC)
 *   acts as a guardian against impunity. It interprets 'unwilling or unable'
 *   broadly, allowing the ICC to intervene when national proceedings are not
 *   genuine, independent, or intended to capture victor's justice and elite
 *   immunity. This reading emphasizes accountability and victim's rights over
 *   strict state sovereignty, leading to a lower admissibility threshold for
 *   ICC jurisdiction.
 *
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
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '7c050325-71a8-4e56-b143-5d6849433c64').
narrative_ontology:cs_kernel_codification('7c050325-71a8-4e56-b143-5d6849433c64', fixed_text).
narrative_ontology:cs_authority_grounding('7c050325-71a8-4e56-b143-5d6849433c64', lineage).
narrative_ontology:cs_interpretation_layer_present('7c050325-71a8-4e56-b143-5d6849433c64').
narrative_ontology:cs_reading_relation('7c050325-71a8-4e56-b143-5d6849433c64', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('7c050325-71a8-4e56-b143-5d6849433c64', foundational, international_justice_as_backstop).
narrative_ontology:cs_axiom_status(international_justice_as_backstop, holdable).
narrative_ontology:cs_axiom_grounding('7c050325-71a8-4e56-b143-5d6849433c64', international_justice_as_backstop, deontological).
narrative_ontology:cs_axiom('7c050325-71a8-4e56-b143-5d6849433c64', foundational, genuine_prosecution_as_substantive_test).
narrative_ontology:cs_axiom_status(genuine_prosecution_as_substantive_test, holdable).
narrative_ontology:cs_axiom_grounding('7c050325-71a8-4e56-b143-5d6849433c64', genuine_prosecution_as_substantive_test, conventional).
narrative_ontology:cs_reference_frame('7c050325-71a8-4e56-b143-5d6849433c64', icc_as_court_of_last_resort).
narrative_ontology:cs_drift_state('7c050325-71a8-4e56-b143-5d6849433c64', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7c050325-71a8-4e56-b143-5d6849433c64', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, human_rights_advocates).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, complicit_states).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, elites_with_impunity).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, states_asserting_absolute_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 17 broadly to assert jurisdiction when national proceedings are not genuine, independent, or intended to bring justice. Seeks to hold states accountable and prosecute individuals for grave international crimes, acting as a guardian against impunity.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the ICC's intervention when their own states fail to provide justice, offering a pathway to accountability that would otherwise be denied. They are often trapped within national systems that offer no genuine recourse.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_complicit_states, beneficiary,
    powerless, biographical, trapped, national).

% Support and leverage the ICC's broad interpretation of complementarity to push for greater accountability for international crimes and to challenge state impunity. They benefit from the ICC's role as a backstop for justice.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, human_rights_advocates, beneficiary,
    organized, generational, constrained, global).

% Are targeted by the ICC's jurisdiction when their national systems are deemed unwilling or unable to genuinely prosecute. They face pressure to cooperate, potential loss of sovereignty over criminal matters, and reputational damage.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, complicit_states, payer,
    institutional, immediate, constrained, national).

% Are the ultimate targets of this reading, as it seeks to dismantle systems of 'victor's justice' or 'elite immunity' where powerful individuals evade prosecution domestically. They face the risk of international arrest warrants and prosecution.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, elites_with_impunity, payer,
    powerful, biographical, constrained, national).

% Resist the ICC's broad interpretation, viewing it as an infringement on their national sovereignty and a challenge to the primacy of their domestic legal systems. They bear the cost of perceived loss of control over their internal affairs.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, states_asserting_absolute_sovereignty, payer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to ensure accountability for grave international crimes by establishing the ICC as a court of last resort, intervening when national systems fail to genuinely prosecute.
% TRANSFER_FUNCTION: Transfers jurisdiction over international crimes from national courts to the ICC when states are 'unwilling or unable' to genuinely prosecute, moving the power to deliver justice from national to international bodies.
% ABSENT_VOICES: Victims of international crimes in states actively obstructing justice are often silenced or unable to participate in national proceedings; this reading aims to give them a voice through the ICC.
% DISAPPEARANCE_RATIONALE: If this broad interpretation of complementarity vanished, states would face less pressure to genuinely prosecute international crimes, leading to increased impunity for perpetrators and a significant setback for international criminal justice. Victims would lose a critical avenue for redress.
% FOUNDING_PROBLEM: The problem of impunity for grave international crimes (genocide, crimes against humanity, war crimes) when national states are unwilling or unable to prosecute, leading to a justice gap.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and numerous victim groups consistently attest to the ongoing problem of impunity and the necessity of international oversight. Independent legal scholars and international law experts corroborate the persistent challenges to national accountability mechanisms.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial because this reading actively challenges state control over criminal justice, imposing international standards and potentially transferring jurisdiction. Suppression (0.7) is high due to the ICC's enforcement mechanisms (arrest warrants, cooperation demands) that compel states to comply or face international pressure. Theater ratio (0.2) is low, as the ICC's actions under this reading are generally aimed at genuine accountability, though some performative resistance from states exists. The constraint is claimed as a Tangled Rope because it genuinely coordinates international justice efforts while simultaneously extracting sovereignty and autonomy from states that fail to meet their obligations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of victims and human rights advocates, this reading is a vital Rope, providing a necessary backstop for justice. From the perspective of states asserting absolute sovereignty or elites benefiting from impunity, it is a Snare, extracting their power and exposing them to international prosecution. The ICC itself views it as a necessary coordination mechanism to fulfill its mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC, victims, and human rights advocates are beneficiaries (low d) as they gain from the ICC's expanded role in ensuring accountability. Complicit states, elites with impunity, and states asserting absolute sovereignty are targets (high d) as they bear the costs of intervention, loss of jurisdiction, and potential prosecution. The 'unwilling or unable' clause, interpreted broadly, is the mechanism for this asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine international oversight as pure extraction by acknowledging the coordination function of ensuring justice where national systems fail. However, it also guards against mislabeling state impunity as legitimate national primacy by emphasizing the 'unwilling or unable' criteria. The rising extractiveness and suppression over time reflect the ICC's increasing assertiveness in challenging state failures and the growing resistance from states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuineness_of_national_proceedings,
    'How objectively can the ICC assess the ''genuineness'' and ''independence'' of national proceedings without infringing on state sovereignty or being perceived as politically motivated?',
    'Development of clearer, universally accepted criteria and methodologies for assessing national proceedings, coupled with independent expert review and robust due process for states challenging admissibility.',
    'If objective criteria are established and accepted, the legitimacy of ICC interventions increases, reducing resistance. If assessment remains subjective, it fuels accusations of political bias, increasing resistance and potentially undermining the ICC''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_of_national_proceedings, conceptual, 'Ambiguity in assessing the ''genuineness'' of national proceedings.').

omega_variable(
    scope_of_unwilling_or_unable,
    'Is the broad interpretation of ''unwilling or unable'' truly consistent with the original intent and drafting history of the Rome Statute, or does it represent an evolutionary interpretation?',
    'Detailed historical and legal analysis of the Rome Statute''s drafting records, coupled with a review of early state practice and subsequent jurisprudence from other international tribunals.',
    'If inconsistent with original intent, it strengthens arguments for national primacy and could lead to states withdrawing from the Rome Statute. If consistent or a legitimate evolution, it reinforces the ICC''s current approach.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_unwilling_or_unable, empirical, 'Debate over the original vs. evolutionary interpretation of ''unwilling or unable''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 2002, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2002, article_17_complementarity__international_oversight_reading, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(arti_tr_t2008, article_17_complementarity__international_oversight_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(arti_tr_t2014, article_17_complementarity__international_oversight_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(arti_tr_t2020, article_17_complementarity__international_oversight_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(arti_tr_t2024, article_17_complementarity__international_oversight_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t2002, article_17_complementarity__international_oversight_reading, base_extractiveness, 2002, 0.5).
narrative_ontology:measurement(arti_be_t2008, article_17_complementarity__international_oversight_reading, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement(arti_be_t2014, article_17_complementarity__international_oversight_reading, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement(arti_be_t2020, article_17_complementarity__international_oversight_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(arti_be_t2024, article_17_complementarity__international_oversight_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2002, article_17_complementarity__international_oversight_reading, suppression_requirement, 2002, 0.55).
narrative_ontology:measurement(arti_su_t2008, article_17_complementarity__international_oversight_reading, suppression_requirement, 2008, 0.63).
narrative_ontology:measurement(arti_su_t2014, article_17_complementarity__international_oversight_reading, suppression_requirement, 2014, 0.67).
narrative_ontology:measurement(arti_su_t2020, article_17_complementarity__international_oversight_reading, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(arti_su_t2024, article_17_complementarity__international_oversight_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 17 complementarity kernel. Its ε value differs significantly from the 'national primacy' reading due to its emphasis on international oversight and lower admissibility threshold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
