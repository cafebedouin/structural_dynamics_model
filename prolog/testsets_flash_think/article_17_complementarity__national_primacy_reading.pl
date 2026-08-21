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
 *   This constraint describes the 'national primacy' reading of Article 17 of
 *   the Rome Statute, which governs the International Criminal Court's (ICC)
 *   jurisdiction. This reading emphasizes state sovereignty, presuming
 *   national courts are adequate unless proven to be a 'sham,' and places a
 *   high burden on the ICC to demonstrate a state's 'unwillingness or
 *   inability' to genuinely prosecute atrocity crimes. This interpretation
 *   prioritizes state cooperation and limits the ICC's intervention, often at
 *   the expense of victims in states with weak but not entirely collapsed
 *   justice systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.7).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.8).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "ICC Complementarity: National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, 'b9469718-f84d-4255-a760-581f55617d7d').
narrative_ontology:cs_kernel_codification('b9469718-f84d-4255-a760-581f55617d7d', fixed_text).
narrative_ontology:cs_authority_grounding('b9469718-f84d-4255-a760-581f55617d7d', lineage).
narrative_ontology:cs_interpretation_layer_present('b9469718-f84d-4255-a760-581f55617d7d').
narrative_ontology:cs_reading_relation('b9469718-f84d-4255-a760-581f55617d7d', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('b9469718-f84d-4255-a760-581f55617d7d', foundational, state_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(state_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b9469718-f84d-4255-a760-581f55617d7d', state_sovereignty_is_paramount, conventional).
narrative_ontology:cs_axiom('b9469718-f84d-4255-a760-581f55617d7d', foundational, icc_is_court_of_last_resort).
narrative_ontology:cs_axiom_status(icc_is_court_of_last_resort, holdable).
narrative_ontology:cs_axiom_grounding('b9469718-f84d-4255-a760-581f55617d7d', icc_is_court_of_last_resort, conventional).
narrative_ontology:cs_reference_frame('b9469718-f84d-4255-a760-581f55617d7d', state_centric_international_law).
narrative_ontology:cs_drift_state('b9469718-f84d-4255-a760-581f55617d7d', contemporary_icc_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b9469718-f84d-4255-a760-581f55617d7d', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_of_atrocity_crimes_in_weak_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, international_justice_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary jurisdiction over atrocity crimes, avoiding ICC intervention. This reading reinforces their authority and autonomy, even if their capacity or willingness to prosecute is limited, so long as proceedings are not a 'sham'.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, mobile, national).

% Benefit from a high threshold for ICC intervention, protecting their sovereign right to prosecute (or not prosecute) their own nationals. They actively defend this interpretation to limit external accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the cost of this reading when their national justice systems are genuinely weak or unwilling but not demonstrably a 'sham.' Their access to international justice through the ICC is severely curtailed, leaving them without effective remedy.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_of_atrocity_crimes_in_weak_states, payer,
    powerless, biographical, trapped, local).

% Work to expand the reach of international criminal justice and hold perpetrators accountable. This reading makes their work harder by limiting the ICC's ability to intervene, forcing them to prove a high bar of state failure.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_justice_advocates, payer,
    organized, generational, constrained, global).

% As the court, it must apply Article 17. This reading places a heavy burden on the ICC Prosecutor to demonstrate a state's 'unwillingness or inability' to genuinely prosecute, often leading to lengthy admissibility challenges and limiting the court's caseload.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc, agenda_setter,
    institutional, generational, constrained, global).

% Advocate for victims and push for accountability. While they can submit information to the ICC, their arguments for broader ICC intervention are often curtailed by this reading's emphasis on national primacy.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, human_rights_organizations, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the division of labor between national courts and the International Criminal Court, ensuring that states retain primary responsibility for prosecuting atrocity crimes, thereby respecting state sovereignty.
% TRANSFER_FUNCTION: Transfers the burden of proof for inadmissibility from the state to the ICC Prosecutor, effectively transferring the cost of inaction or weak national proceedings onto victims and international justice mechanisms.
% ABSENT_VOICES: Victims of atrocity crimes in states with weak-but-genuine proceedings are effectively silenced, as their national systems offer no real recourse and the ICC is barred from intervening. Human rights organizations, while present, find their advocacy constrained by this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the ICC's jurisdiction would expand significantly, leading to more cases, greater international scrutiny of national justice systems, and a rebalancing of sovereignty claims against accountability. States would face increased pressure to genuinely prosecute or risk ICC intervention.
% FOUNDING_PROBLEM: The Rome Statute sought to establish an international court for atrocity crimes while respecting state sovereignty, ensuring the ICC would be a court of last resort, not a replacement for national systems.
% FOUNDING_PROBLEM_CORROBORATION: States (especially those wary of international intervention) and many legal scholars attest that the balance between sovereignty and international justice remains a live and complex problem. International justice advocates, while disagreeing with the outcome, acknowledge the historical tension the complementarity principle was designed to address.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.7) because this reading effectively shields states from ICC intervention, allowing them to avoid accountability for atrocity crimes if their national proceedings, however weak, are not deemed a 'sham.' Suppression is also high (0.8) as it actively suppresses the ICC's ability to act as a backstop for justice, requiring extensive legal battles to overcome the presumption of national primacy. The theater ratio is moderate (0.4) because some national proceedings, while not outright shams, may be performative or insufficient, yet still satisfy the high bar for inadmissibility under this reading. Accessibility collapse is high (0.75) for victims, as the ICC is their primary alternative to national systems. Resistance is moderate (0.6) from international justice advocates and human rights organizations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereignty-maximizing states and national judiciaries, this reading is a legitimate and necessary safeguard of state sovereignty and the principle of subsidiarity in international law. From the perspective of victims and international justice advocates, it is a mechanism that enables impunity by creating an excessively high bar for ICC intervention, effectively extracting justice from those most in need.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereignty-maximizing states and national judiciaries are clear beneficiaries, as this reading protects their jurisdiction and limits external accountability. Victims of atrocity crimes in weak states and international justice advocates are the primary targets, as their access to international justice is curtailed. The ICC itself, while the 'agenda-setter' in applying the rule, is constrained by this reading's high burden of proof, making its role more difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope highlights that while the complementarity principle has a genuine coordination function (respecting state sovereignty and subsidiarity), this specific reading of it also facilitates asymmetric extraction. It prevents mislabeling it as a pure Rope (ignoring the victims) or a pure Snare (ignoring the legitimate coordination function of state sovereignty). The 'founding problem' of balancing sovereignty and accountability is still 'live,' but the 'national primacy' reading's high threshold for ICC intervention means the constraint's operation often favors sovereignty over accountability, leading to the observed extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sham_proceedings_definition,
    'What constitutes a ''sham'' national proceeding, and how is this objectively determined in practice?',
    'Development of clearer, universally accepted legal standards and empirical indicators for assessing the genuineness and effectiveness of national prosecutions, potentially through a UN-mandated expert body.',
    'A clearer, lower bar for ''sham'' proceedings would reduce extraction from victims and shift the constraint closer to a Rope by allowing the ICC to intervene more readily. A high, ambiguous bar maintains the current extractive dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_proceedings_definition, conceptual, 'Ambiguity in defining ''sham'' proceedings allows weak national systems to avoid ICC intervention.').

omega_variable(
    sovereignty_vs_accountability_balance,
    'What is the optimal balance between state sovereignty and international accountability for atrocity crimes?',
    'This is a preference-based question, resolvable through international political consensus, treaty amendment, or evolving customary international law reflecting a shift in state practice and values.',
    'A shift towards greater international accountability would reduce the extractiveness of this reading, potentially reclassifying it towards a Rope or even a Scaffold (if temporary support for national systems is prioritized). A continued emphasis on sovereignty maintains the current Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_accountability_balance, preference, 'The fundamental normative tension between state sovereignty and international accountability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1998, article_17_complementarity__national_primacy_reading, theater_ratio, 1998, 0.25).
narrative_ontology:measurement(arti_tr_t2004, article_17_complementarity__national_primacy_reading, theater_ratio, 2004, 0.3).
narrative_ontology:measurement(arti_tr_t2010, article_17_complementarity__national_primacy_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(arti_tr_t2016, article_17_complementarity__national_primacy_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement(arti_tr_t2024, article_17_complementarity__national_primacy_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t1998, article_17_complementarity__national_primacy_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(arti_be_t2004, article_17_complementarity__national_primacy_reading, base_extractiveness, 2004, 0.6).
narrative_ontology:measurement(arti_be_t2010, article_17_complementarity__national_primacy_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(arti_be_t2016, article_17_complementarity__national_primacy_reading, base_extractiveness, 2016, 0.68).
narrative_ontology:measurement(arti_be_t2024, article_17_complementarity__national_primacy_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1998, article_17_complementarity__national_primacy_reading, suppression_requirement, 1998, 0.65).
narrative_ontology:measurement(arti_su_t2004, article_17_complementarity__national_primacy_reading, suppression_requirement, 2004, 0.7).
narrative_ontology:measurement(arti_su_t2010, article_17_complementarity__national_primacy_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(arti_su_t2016, article_17_complementarity__national_primacy_reading, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement(arti_su_t2024, article_17_complementarity__national_primacy_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 17 complementarity principle. Its sibling, 'international_oversight_reading,' offers a contrasting interpretation that prioritizes accountability over national primacy. Both are distinct constraints arising from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
