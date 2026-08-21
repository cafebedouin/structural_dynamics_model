% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: ICC Complementarity: National Primacy Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint story describes the 'national primacy' reading of Article
 *   17 complementarity in the Rome Statute, which governs the jurisdiction of
 *   the International Criminal Court (ICC). This reading emphasizes state
 *   sovereignty, presuming national courts are adequate unless proven to be a
 *   'sham,' and places a high burden on the ICC to demonstrate
 *   inadmissibility. It effectively limits the ICC's intervention to cases of
 *   complete judicial collapse, prioritizing state cooperation and protecting
 *   national judiciaries from international oversight.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.78).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.85).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "ICC Complementarity: National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '189b64cd-7e21-4549-b160-cf5fdc2ced98').
narrative_ontology:cs_kernel_codification('189b64cd-7e21-4549-b160-cf5fdc2ced98', fixed_text).
narrative_ontology:cs_authority_grounding('189b64cd-7e21-4549-b160-cf5fdc2ced98', lineage).
narrative_ontology:cs_interpretation_layer_present('189b64cd-7e21-4549-b160-cf5fdc2ced98').
narrative_ontology:cs_reading_relation('189b64cd-7e21-4549-b160-cf5fdc2ced98', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('189b64cd-7e21-4549-b160-cf5fdc2ced98', foundational, state_sovereignty_is_primary).
narrative_ontology:cs_axiom_status(state_sovereignty_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('189b64cd-7e21-4549-b160-cf5fdc2ced98', state_sovereignty_is_primary, conventional).
narrative_ontology:cs_axiom('189b64cd-7e21-4549-b160-cf5fdc2ced98', foundational, icc_is_court_of_last_resort).
narrative_ontology:cs_axiom_status(icc_is_court_of_last_resort, holdable).
narrative_ontology:cs_axiom_grounding('189b64cd-7e21-4549-b160-cf5fdc2ced98', icc_is_court_of_last_resort, conventional).
narrative_ontology:cs_reference_frame('189b64cd-7e21-4549-b160-cf5fdc2ced98', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('189b64cd-7e21-4549-b160-cf5fdc2ced98', contemporary_icc_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('189b64cd-7e21-4549-b160-cf5fdc2ced98', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_of_state_impunity).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, international_criminal_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Presumptively retain jurisdiction over international crimes, avoiding ICC intervention. They benefit from the high threshold for inadmissibility, which protects their autonomy and reduces external scrutiny, even if their proceedings are weak but not outright 'sham'.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, national_judiciaries, agenda_setter).

% Benefit from the interpretation that prioritizes state sovereignty and limits the ICC's reach. They actively enforce this reading by asserting national jurisdiction and challenging ICC admissibility, protecting their officials from international prosecution.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, agenda_setter).

% Bear the cost of this reading, as their access to international justice is severely restricted. If national proceedings are weak or ineffective but not demonstrably a 'sham,' they remain without effective remedy for grave international crimes.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_of_state_impunity, payer,
    powerless, biographical, trapped, local).

% Bears the heavy burden of demonstrating a state's 'unwillingness or inability' to genuinely prosecute. This reading significantly curtails its jurisdiction and ability to fulfill its mandate, forcing it to expend considerable resources on admissibility challenges rather than prosecutions.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_criminal_court, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, international_criminal_court, agenda_setter).

% Monitor the application of complementarity, documenting cases where national proceedings fail to deliver justice but do not meet the high 'sham' threshold. They advocate for a more robust interpretation of ICC jurisdiction but are outside the formal decision-making process of this reading.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, human_rights_advocates, observer,
    organized, generational, mobile, global).

% Advocate for a broader interpretation of ICC jurisdiction to ensure accountability for grave crimes. Their arguments for a lower inadmissibility threshold and a more active ICC role are structurally excluded by the national primacy reading, which prioritizes state consent and sovereignty.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_oversight_proponents, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the division of labor between national and international criminal justice systems, ensuring that states retain primary responsibility for prosecuting international crimes, thereby protecting state sovereignty.
% TRANSFER_FUNCTION: Transfers the burden of proof for inadmissibility from states to the ICC, and effectively transfers the responsibility for accountability from the international to the national level, even when national systems are weak. It also transfers the cost of impunity to victims.
% ABSENT_VOICES: Victims of state impunity and proponents of stronger international accountability are effectively marginalized; they would argue for a lower threshold for ICC intervention and a more robust international oversight role, but their concerns are subordinated to state sovereignty in this reading.
% DISAPPEARANCE_RATIONALE: If this reading of complementarity vanished, the ICC would likely adopt a more expansive interpretation of its jurisdiction, leading to more admissibility challenges against states and potentially more international prosecutions. States would face increased scrutiny, and the balance of power in international criminal justice would shift significantly.
% FOUNDING_PROBLEM: The Rome Statute sought to establish an international court to prosecute the gravest crimes while respecting the principle of state sovereignty and the primary responsibility of states to prosecute such crimes.
% FOUNDING_PROBLEM_CORROBORATION: Sovereignty-maximizing states and some legal scholars argue that the founding problem of balancing sovereignty and accountability is still live and this reading correctly upholds it. Human rights organizations and proponents of international accountability argue that the problem of impunity persists due to this reading, indicating the founding problem is not adequately addressed.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because this reading significantly limits access to international justice for victims of state impunity, effectively allowing weak-but-genuine national proceedings to shield perpetrators. Suppression (0.85) is also high, as it actively suppresses the ICC's ability to intervene and alternative avenues for justice. The theater ratio (0.45) reflects that while some national proceedings are genuine, others may be performative, designed to avoid ICC jurisdiction without genuinely delivering justice. Accessibility collapse (0.70) is substantial, as alternatives to national proceedings are largely foreclosed. Resistance (0.60) comes from human rights groups and victims advocating for stronger international accountability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereignty-maximizing states, this reading is a legitimate and necessary protection of national sovereignty and a proper interpretation of the Rome Statute. From the perspective of victims and human rights advocates, it is a mechanism that enables impunity and undermines the ICC's core mission. The engine's classification as a Tangled Rope reflects this dual function: coordination for states, extraction for victims and the ICC.
 *
 * DIRECTIONALITY LOGIC:
 *   National judiciaries and sovereignty-maximizing states are clear beneficiaries, as this reading protects their autonomy and limits external intervention. Victims of state impunity are the primary targets, as their access to justice is severely curtailed. The International Criminal Court itself is also a target, as it bears a heavy burden of proof and its mandate is constrained by this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by highlighting the active enforcement and beneficiaries. While it claims to coordinate state sovereignty with international justice, its high extractiveness and suppression reveal that it also functions to protect states from accountability, even when their domestic systems are inadequate. The 'sovereignty-protection' narrative serves as a cover for the extraction of justice from victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sham_proceedings_threshold,
    'What constitutes a ''sham'' national proceeding, and is the current threshold for ICC intervention too high to genuinely address impunity?',
    'Empirical analysis of national proceedings deemed ''genuine'' by this reading but which failed to deliver justice, coupled with expert legal consensus on minimum standards for effective prosecution.',
    'If the threshold is found to be too high, the effective extractiveness from victims is higher than currently measured, and the constraint leans more towards a Snare. If the threshold is appropriate, the coordination function for states is more legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_proceedings_threshold, empirical, 'Ambiguity in the definition and application of ''sham'' proceedings.').

omega_variable(
    sovereignty_vs_accountability_balance,
    'Is the balance between state sovereignty and international accountability, as interpreted by this reading, consistent with the Rome Statute''s overall object and purpose?',
    'A re-evaluation of the Rome Statute''s drafting history, subsequent state practice, and evolving international legal norms regarding human rights and criminal accountability.',
    'If the balance is found to unduly favor sovereignty, the constraint''s legitimacy as a coordination mechanism is weakened, and its extractive nature is amplified. If it is found to be consistent, the ''tangled_rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_accountability_balance, conceptual, 'Conceptual contest over the normative balance between state sovereignty and international accountability.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of ICC jurisdiction primarily structural (legal thresholds, burden of proof) or internalized (states'' political will to resist ICC intervention)?',
    'Analysis of state behavior in admissibility challenges: if states consistently resist ICC intervention even when their domestic capacity is demonstrably weak, it suggests internalized resistance to international oversight.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as states actively leverage the legal framework to avoid accountability. If purely structural, changes to legal interpretation could more easily alter the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ICC jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 2002, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2002, article_17_complementarity__national_primacy_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(arti_tr_t2006, article_17_complementarity__national_primacy_reading, theater_ratio, 2006, 0.35).
narrative_ontology:measurement(arti_tr_t2010, article_17_complementarity__national_primacy_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(arti_tr_t2014, article_17_complementarity__national_primacy_reading, theater_ratio, 2014, 0.42).
narrative_ontology:measurement(arti_tr_t2018, article_17_complementarity__national_primacy_reading, theater_ratio, 2018, 0.44).
narrative_ontology:measurement(arti_tr_t2024, article_17_complementarity__national_primacy_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(arti_be_t2002, article_17_complementarity__national_primacy_reading, base_extractiveness, 2002, 0.65).
narrative_ontology:measurement(arti_be_t2006, article_17_complementarity__national_primacy_reading, base_extractiveness, 2006, 0.68).
narrative_ontology:measurement(arti_be_t2010, article_17_complementarity__national_primacy_reading, base_extractiveness, 2010, 0.71).
narrative_ontology:measurement(arti_be_t2014, article_17_complementarity__national_primacy_reading, base_extractiveness, 2014, 0.74).
narrative_ontology:measurement(arti_be_t2018, article_17_complementarity__national_primacy_reading, base_extractiveness, 2018, 0.76).
narrative_ontology:measurement(arti_be_t2024, article_17_complementarity__national_primacy_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2002, article_17_complementarity__national_primacy_reading, suppression_requirement, 2002, 0.7).
narrative_ontology:measurement(arti_su_t2006, article_17_complementarity__national_primacy_reading, suppression_requirement, 2006, 0.75).
narrative_ontology:measurement(arti_su_t2010, article_17_complementarity__national_primacy_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(arti_su_t2014, article_17_complementarity__national_primacy_reading, suppression_requirement, 2014, 0.82).
narrative_ontology:measurement(arti_su_t2018, article_17_complementarity__national_primacy_reading, suppression_requirement, 2018, 0.84).
narrative_ontology:measurement(arti_su_t2024, article_17_complementarity__national_primacy_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two structurally distinct readings of Article 17 complementarity. This 'national primacy' reading emphasizes state sovereignty and limits ICC intervention, while the 'international oversight' reading (a sibling constraint) emphasizes accountability and a broader ICC role. Their ε values and stakeholder impacts differ significantly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
