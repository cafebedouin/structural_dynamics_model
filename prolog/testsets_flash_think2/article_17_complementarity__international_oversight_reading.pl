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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Article 17 Complementarity: International Oversight Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'international oversight' reading of
 *   Article 17 complementarity within the Rome Statute, which governs the
 *   International Criminal Court's jurisdiction. In this reading,
 *   complementarity functions as an accountability-trigger mechanism,
 *   empowering the ICC to act as a guardian against impunity when states fail
 *   to genuinely prosecute atrocity crimes. The 'unwilling or unable'
 *   criteria are interpreted broadly to capture scenarios of victor's
 *   justice, elite immunity, and sham proceedings, ensuring that the ICC can
 *   intervene effectively to secure justice for victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.8).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.85).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity: International Oversight Reading").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '08b8821c-26c1-4426-b2cf-2f45eadcf08e').
narrative_ontology:cs_kernel_codification('08b8821c-26c1-4426-b2cf-2f45eadcf08e', fixed_text).
narrative_ontology:cs_authority_grounding('08b8821c-26c1-4426-b2cf-2f45eadcf08e', lineage).
narrative_ontology:cs_interpretation_layer_present('08b8821c-26c1-4426-b2cf-2f45eadcf08e').
narrative_ontology:cs_reading_relation('08b8821c-26c1-4426-b2cf-2f45eadcf08e', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('08b8821c-26c1-4426-b2cf-2f45eadcf08e', foundational, effective_accountability_over_formal_sovereignty).
narrative_ontology:cs_axiom_status(effective_accountability_over_formal_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('08b8821c-26c1-4426-b2cf-2f45eadcf08e', effective_accountability_over_formal_sovereignty, deontological).
narrative_ontology:cs_axiom('08b8821c-26c1-4426-b2cf-2f45eadcf08e', foundational, broad_interpretation_of_unwilling_unable).
narrative_ontology:cs_axiom_status(broad_interpretation_of_unwilling_unable, holdable).
narrative_ontology:cs_axiom_grounding('08b8821c-26c1-4426-b2cf-2f45eadcf08e', broad_interpretation_of_unwilling_unable, conventional).
narrative_ontology:cs_reference_frame('08b8821c-26c1-4426-b2cf-2f45eadcf08e', post_rwanda_yugoslavia_impunity_era).
narrative_ontology:cs_drift_state('08b8821c-26c1-4426-b2cf-2f45eadcf08e', contemporary_challenges_to_icc_legitimacy, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('08b8821c-26c1-4426-b2cf-2f45eadcf08e', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_of_atrocities).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_justice_advocates).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, complicit_states).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, elites_seeking_impunity).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, end_to_impunity_norm).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, rule_of_law_principle).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, human_rights_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The International Criminal Court, which interprets Article 17 broadly to assert jurisdiction when states are genuinely unwilling or unable to prosecute. It actively monitors domestic proceedings and intervenes to prevent impunity, acting as a guardian of international justice.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc, agenda_setter,
    institutional, generational, constrained, global).

% Individuals who have suffered atrocity crimes and seek justice. This reading of complementarity offers them a pathway to accountability when their own states fail to provide it, expanding the scope of protection against impunity.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_of_atrocities, beneficiary,
    powerless, biographical, trapped, global).

% States that are unwilling or unable to genuinely prosecute atrocity crimes committed on their territory or by their nationals. This reading extracts their exclusive jurisdiction, forcing them to either genuinely prosecute or face ICC intervention, challenging their traditional notions of sovereignty.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, complicit_states, payer,
    institutional, generational, constrained, national).

% Political or military leaders who might otherwise escape prosecution through victor's justice, sham trials, or elite immunity. This reading targets their impunity, making them vulnerable to international prosecution and extracting their perceived right to avoid accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, elites_seeking_impunity, payer,
    powerful, biographical, identity_locked, national).

% NGOs, legal scholars, and international organizations that champion the cause of international criminal justice. This reading aligns with their mission to end impunity and strengthens the ICC's capacity to fulfill its mandate.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_justice_advocates, beneficiary,
    organized, generational, mobile, global).

% States and political movements that prioritize absolute state sovereignty and resist any international intervention in domestic affairs. Their arguments for national primacy are actively challenged and suppressed by this expansive interpretation of complementarity.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, sovereignty_absolutists, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to ensure accountability for atrocity crimes by establishing the ICC as a court of last resort, intervening when national systems fail to genuinely prosecute, thereby filling accountability gaps.
% TRANSFER_FUNCTION: Transfers jurisdiction and the right to prosecute from complicit or failing states to the ICC, and transfers the burden of proof for genuine prosecution to national authorities, in exchange for the promise of justice for victims.
% ABSENT_VOICES: States and elites who benefit from impunity, as well as those who advocate for an absolute interpretation of state sovereignty, are structurally excluded from shaping this reading. They would argue for a much higher threshold for ICC intervention and a narrower interpretation of 'unwilling or unable'.
% DISAPPEARANCE_RATIONALE: If this broad interpretation of complementarity vanished, the ICC's ability to intervene would be severely curtailed, leading to a resurgence of impunity for atrocity crimes. Victims would lose a crucial avenue for justice, and the international legal framework for accountability would be significantly weakened, reorganizing around state-centric power dynamics.
% FOUNDING_PROBLEM: The problem of widespread impunity for atrocity crimes, where national courts were unwilling or unable to prosecute, leading to cycles of violence and injustice, particularly after the failures of ad hoc tribunals.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN reports, and independent legal analyses consistently corroborate that impunity remains a live problem, especially in contexts of state fragility or political obstruction. While some states contest the ICC's role, the persistence of unaddressed atrocity crimes is widely acknowledged outside of benefiting parties.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.8) reflects how this reading actively takes away the exclusive right of states to prosecute, particularly when they are complicit or failing. Suppression (0.85) is high because the ICC, under this interpretation, actively works to suppress alternatives to genuine prosecution, such as sham trials or political interference. The theater ratio (0.4) is moderate; while the ICC's actions are genuinely aimed at justice, there's also a performative aspect in asserting its authority against state resistance. Resistance (0.8) is high due to strong pushback from states asserting sovereignty. Accessibility collapse (0.7) is substantial for those seeking impunity, as the ICC's broad interpretation closes off many avenues for evasion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of victims and international justice advocates, this reading is a vital mechanism for accountability, ensuring that justice is served. For complicit states and elites seeking impunity, it is an intrusive and extractive constraint that challenges their sovereignty and personal immunity. The ICC, as the agenda-setter, views it as fulfilling its mandate to end impunity. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC, as the agenda-setter, benefits from this reading as it empowers its mandate. Victims and international justice advocates are clear beneficiaries, gaining a pathway to justice. Complicit states and elites seeking impunity are the primary targets/payers, as this reading extracts their ability to avoid genuine prosecution. Sovereignty absolutists are excluded, as their position is directly challenged by this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope (or Snare from the state/elite perspective) prevents mislabeling the ICC's intervention as pure coordination. While it coordinates international efforts against impunity, its broad interpretation of 'unwilling or unable' involves significant extraction of state sovereignty and elite immunity. The high extractiveness and suppression metrics, coupled with active enforcement, highlight the coercive aspect necessary to overcome state resistance and ensure accountability, rather than merely facilitating voluntary cooperation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unwilling_unable_interpretation_ambiguity,
    'How consistently and objectively can ''unwilling or unable'' be applied without appearing politically motivated or infringing on legitimate state functions?',
    'Analysis of ICC admissibility decisions over time, focusing on the consistency of criteria application across diverse political contexts and the degree of deference shown to genuine (non-sham) domestic efforts.',
    'If application is perceived as inconsistent or politically biased, the ICC''s legitimacy and the constraint''s effective suppression could be undermined, potentially shifting its classification towards a more performative Piton or a less effective Tangled Rope. If consistently applied, it reinforces the constraint''s legitimacy and effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwilling_unable_interpretation_ambiguity, empirical, 'Ambiguity in the practical application of the ''unwilling or unable'' criteria.').

omega_variable(
    national_primacy_vs_international_oversight,
    'Is the ICC''s role primarily to defer to national systems (national primacy reading) or to actively ensure accountability when states fail (international oversight reading)?',
    'Evolution of state practice, ICC jurisprudence, and treaty interpretation by state parties. A shift towards greater state deference would validate the national primacy reading; continued ICC intervention would validate the international oversight reading.',
    'If the national primacy reading gains dominance, the constraint''s extractiveness and suppression would decrease, potentially reclassifying it as a Rope or even a Mountain (if state capacity is universally high). If the international oversight reading prevails, the current classification as a Tangled Rope is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(national_primacy_vs_international_oversight, conceptual, 'Fundamental conceptual disagreement over the balance between state sovereignty and international criminal justice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 2002, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2002, article_17_complementarity__international_oversight_reading, theater_ratio, 2002, 0.2).
narrative_ontology:measurement(arti_tr_t2008, article_17_complementarity__international_oversight_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(arti_tr_t2014, article_17_complementarity__international_oversight_reading, theater_ratio, 2014, 0.35).
narrative_ontology:measurement(arti_tr_t2020, article_17_complementarity__international_oversight_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(arti_tr_t2024, article_17_complementarity__international_oversight_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t2002, article_17_complementarity__international_oversight_reading, base_extractiveness, 2002, 0.6).
narrative_ontology:measurement(arti_be_t2008, article_17_complementarity__international_oversight_reading, base_extractiveness, 2008, 0.7).
narrative_ontology:measurement(arti_be_t2014, article_17_complementarity__international_oversight_reading, base_extractiveness, 2014, 0.75).
narrative_ontology:measurement(arti_be_t2020, article_17_complementarity__international_oversight_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(arti_be_t2024, article_17_complementarity__international_oversight_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2002, article_17_complementarity__international_oversight_reading, suppression_requirement, 2002, 0.65).
narrative_ontology:measurement(arti_su_t2008, article_17_complementarity__international_oversight_reading, suppression_requirement, 2008, 0.75).
narrative_ontology:measurement(arti_su_t2014, article_17_complementarity__international_oversight_reading, suppression_requirement, 2014, 0.8).
narrative_ontology:measurement(arti_su_t2020, article_17_complementarity__international_oversight_reading, suppression_requirement, 2020, 0.83).
narrative_ontology:measurement(arti_su_t2024, article_17_complementarity__international_oversight_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, rome_statute_admissibility_rules).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, international_humanitarian_law_compliance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 17 complementarity kernel. Its sibling, 'national_primacy_reading', offers a different interpretation of the balance between state sovereignty and ICC jurisdiction, leading to different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
