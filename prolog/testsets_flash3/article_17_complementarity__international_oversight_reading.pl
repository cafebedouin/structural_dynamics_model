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
 *   This constraint represents the 'international oversight' reading of ICC
 *   Article 17 complementarity, where the ICC acts as a guardian against
 *   impunity when states are 'unwilling or unable' to genuinely prosecute
 *   grave international crimes. This interpretation broadens the ICC's
 *   jurisdiction, intensifying demands on states to cooperate and expanding
 *   the victim set to include those subject to sham prosecutions or elite
 *   immunity. It is a contested reading, often clashing with a 'national
 *   primacy' interpretation that emphasizes state sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.65).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.78).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "ICC Article 17 Complementarity (International Oversight Reading)").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '687281a0-b645-495b-a669-70fb2cc15fff').
narrative_ontology:cs_kernel_codification('687281a0-b645-495b-a669-70fb2cc15fff', fixed_text).
narrative_ontology:cs_authority_grounding('687281a0-b645-495b-a669-70fb2cc15fff', lineage).
narrative_ontology:cs_interpretation_layer_present('687281a0-b645-495b-a669-70fb2cc15fff').
narrative_ontology:cs_reading_relation('687281a0-b645-495b-a669-70fb2cc15fff', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('687281a0-b645-495b-a669-70fb2cc15fff', foundational, impunity_is_unacceptable).
narrative_ontology:cs_axiom_status(impunity_is_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('687281a0-b645-495b-a669-70fb2cc15fff', impunity_is_unacceptable, deontological).
narrative_ontology:cs_axiom('687281a0-b645-495b-a669-70fb2cc15fff', foundational, icc_is_guardian_of_international_justice).
narrative_ontology:cs_axiom_status(icc_is_guardian_of_international_justice, holdable).
narrative_ontology:cs_axiom_grounding('687281a0-b645-495b-a669-70fb2cc15fff', icc_is_guardian_of_international_justice, conventional).
narrative_ontology:cs_reference_frame('687281a0-b645-495b-a669-70fb2cc15fff', icc_as_court_of_last_resort).
narrative_ontology:cs_drift_state('687281a0-b645-495b-a669-70fb2cc15fff', contemporary_political_resistance_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('687281a0-b645-495b-a669-70fb2cc15fff', '').
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

% Interprets and applies Article 17, asserting jurisdiction when states are 'unwilling or unable' to genuinely prosecute. Benefits from expanded jurisdiction and legitimacy as a guardian against impunity. Bears the cost of political resistance from states.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the ICC's intervention when their own states fail to provide justice for grave crimes. Their access to justice is otherwise suppressed by state complicity or incapacity. This reading expands their avenues for accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_complicit_states, beneficiary,
    powerless, biographical, trapped, national).

% Advocate for a robust interpretation of complementarity that prioritizes victim accountability over state sovereignty. Benefit from the ICC's expanded role in deterring impunity and strengthening international justice norms.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Face ICC intervention and potential loss of sovereign control over prosecutions when their domestic proceedings are deemed sham or insufficient. Bear the costs of international scrutiny, reputational damage, and potential loss of jurisdiction. Often resist ICC cooperation.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, complicit_states, payer,
    institutional, immediate, constrained, national).

% Are the direct targets of ICC prosecution when domestic systems fail to hold them accountable. Their impunity, often secured through political influence or control of state apparatus, is directly challenged by this reading of complementarity. Exit means facing justice.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, elites_with_impunity, payer,
    powerful, biographical, identity_locked, national).

% Resist any interpretation of complementarity that infringes on their perceived absolute right to prosecute or not prosecute within their borders. Bear the cost of having their sovereignty claims challenged by international legal norms and institutions.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, states_asserting_absolute_sovereignty, payer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international and national jurisdictions to ensure accountability for grave international crimes, preventing impunity when states are unwilling or unable to genuinely prosecute.
% TRANSFER_FUNCTION: Transfers jurisdiction and the power to prosecute from national authorities to the ICC when domestic proceedings are found to be a sham or genuinely ineffective, moving accountability from state control to international oversight.
% ABSENT_VOICES: Victims of crimes in states with weak or complicit justice systems, who would otherwise have no recourse, are given a voice through the ICC's intervention. Their absence from national justice processes is precisely what this reading seeks to remedy.
% DISAPPEARANCE_RATIONALE: If this interpretation of complementarity vanished, states would face significantly less pressure to genuinely prosecute grave crimes, leading to increased impunity for elites and a collapse of accountability for victims in complicit states. The international criminal justice system would revert to a purely national model, with significant consequences for human rights.
% FOUNDING_PROBLEM: The problem of impunity for grave international crimes (genocide, war crimes, crimes against humanity) when national justice systems are unwilling or unable to genuinely prosecute, often due to state complicity or collapse.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, victim groups, and numerous international legal scholars attest that the problem of impunity remains live, citing ongoing conflicts and instances of state failure to prosecute. The ICC's caseload itself corroborates the persistence of this problem.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) reflects the transfer of sovereign prosecutorial power from states to the ICC, which is a significant cost for states that prefer to control their own justice systems. Suppression (0.78) is high because this reading actively challenges state sovereignty and elite immunity, requiring robust enforcement by the ICC against state resistance. The theater ratio (0.4) indicates that while some state proceedings are genuine, a substantial portion of domestic 'justice' efforts are performative, designed to avoid ICC intervention without delivering true accountability. The metrics reflect the ongoing struggle to assert international jurisdiction over resistant states.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ICC and victims, this reading is a necessary mechanism for justice, ensuring accountability where states fail. From the perspective of states asserting sovereignty, it is an overreach that infringes on national prerogatives. The engine's classification will reflect this divergence, showing a coordination function for beneficiaries and extraction for targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC, victims in complicit states, and human rights advocates are beneficiaries, as this reading empowers them and provides avenues for justice. Complicit states, elites benefiting from impunity, and states asserting absolute sovereignty are victims, as they bear the costs of diminished sovereignty, potential prosecution, and international scrutiny. The ICC's directionality is toward benefiting from expanded jurisdiction, while resistant states are targeted by its enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unwilling_unable_interpretation_ambiguity,
    'How broadly should ''unwilling or unable'' be interpreted to trigger ICC jurisdiction?',
    'Further ICC jurisprudence and state practice, particularly in cases involving victor''s justice or elite immunity, establishing clearer precedents for what constitutes a ''sham'' prosecution or genuine inability.',
    'A broader interpretation (as in this reading) increases ICC jurisdiction and extractiveness from states; a narrower interpretation (national primacy reading) limits ICC intervention and reduces extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unwilling_unable_interpretation_ambiguity, conceptual, 'Ambiguity in the scope of ICC intervention under Article 17.').

omega_variable(
    state_cooperation_enforcement_capacity,
    'What is the ICC''s actual capacity to enforce its jurisdiction and compel state cooperation against resistant states?',
    'Empirical analysis of ICC''s success rate in securing arrests, evidence, and cooperation from non-compliant states, particularly those with strong political backing.',
    'If enforcement capacity is low, the constraint''s effective suppression and extractiveness are lower than measured, as states can more easily resist. If capacity is high, the measured values are accurate or even understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_cooperation_enforcement_capacity, empirical, 'The practical limits of ICC enforcement against sovereign states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__international_oversight_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__international_oversight_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__international_oversight_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__international_oversight_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__international_oversight_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__international_oversight_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__international_oversight_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__international_oversight_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__international_oversight_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__international_oversight_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__international_oversight_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__international_oversight_reading, suppression_requirement, 5, 0.73).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__international_oversight_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__international_oversight_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__international_oversight_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 17 complementarity kernel. Its sibling, 'national_primacy_reading,' emphasizes state sovereignty and a higher admissibility threshold for ICC intervention. Both are distinct constraints arising from the same legal text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
