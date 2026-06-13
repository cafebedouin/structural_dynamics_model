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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: ICC Article 17 Complementarity (National Primacy Reading)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'national primacy' reading of Article 17
 *   of the Rome Statute, which establishes the principle of complementarity
 *   for the International Criminal Court (ICC). Under this reading, national
 *   courts are presumed adequate to prosecute international crimes, and the
 *   ICC has a high burden to demonstrate a state's 'unwillingness or
 *   inability' to genuinely carry out proceedings before it can assert
 *   jurisdiction. This interpretation prioritizes state sovereignty and
 *   cooperation, limiting the ICC's reach to cases of complete judicial
 *   collapse, thereby protecting national judiciaries and
 *   sovereignty-maximizing states from international intervention.
 *
 * KEY AGENTS:
 *   - national_judiciaries: Primary beneficiary (institutional/constrained) — protected from ICC intervention.
 *   - sovereignty_maximizing_states: Primary beneficiary (institutional/arbitrage) — maintain control over domestic justice.
 *   - victims_in_weak_states: Primary victim (powerless/trapped) — denied international justice when national systems are weak but not 'sham'.
 *   - international_criminal_court: Agenda setter (institutional/constrained) — bound by the high inadmissibility threshold.
 *   - international_justice_advocates: Victim (organized/constrained) — their goals for accountability are limited by this interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.65).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.7).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "ICC Article 17 Complementarity (National Primacy Reading)").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '1c125389-54ec-48f5-8a89-ccc1d2a20fab').
narrative_ontology:cs_kernel_codification('1c125389-54ec-48f5-8a89-ccc1d2a20fab', fixed_text).
narrative_ontology:cs_authority_grounding('1c125389-54ec-48f5-8a89-ccc1d2a20fab', lineage).
narrative_ontology:cs_interpretation_layer_present('1c125389-54ec-48f5-8a89-ccc1d2a20fab').
narrative_ontology:cs_reading_relation('1c125389-54ec-48f5-8a89-ccc1d2a20fab', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('1c125389-54ec-48f5-8a89-ccc1d2a20fab', foundational, national_sovereignty_primacy).
narrative_ontology:cs_axiom_status(national_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('1c125389-54ec-48f5-8a89-ccc1d2a20fab', national_sovereignty_primacy, deontological).
narrative_ontology:cs_axiom('1c125389-54ec-48f5-8a89-ccc1d2a20fab', foundational, icc_burden_of_proof_high).
narrative_ontology:cs_axiom_status(icc_burden_of_proof_high, holdable).
narrative_ontology:cs_axiom_grounding('1c125389-54ec-48f5-8a89-ccc1d2a20fab', icc_burden_of_proof_high, conventional).
narrative_ontology:cs_reference_frame('1c125389-54ec-48f5-8a89-ccc1d2a20fab', state_centric_international_law).
narrative_ontology:cs_drift_state('1c125389-54ec-48f5-8a89-ccc1d2a20fab', contemporary_icc_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1c125389-54ec-48f5-8a89-ccc1d2a20fab', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_weak_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, international_justice_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their authority and jurisdiction over domestic criminal matters are preserved, as the ICC is reluctant to intervene unless their proceedings are a complete 'sham'. This protects them from external oversight and potential loss of prestige or power.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, national).

% These states actively promote and benefit from an interpretation of complementarity that minimizes international intervention in their domestic affairs. They can leverage this reading to avoid ICC scrutiny, even if their national justice systems are weak, as long as they maintain a semblance of genuine proceedings.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    institutional, generational, arbitrage, global).

% These individuals are victims of international crimes in states where national justice systems are too weak, corrupt, or politically compromised to genuinely prosecute, but not so utterly collapsed as to be deemed a 'sham' by the ICC under this reading. They are effectively denied access to international justice.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_in_weak_states, payer,
    powerless, biographical, trapped, national).

% The ICC is the institution tasked with applying Article 17, but under this reading, it bears a heavy burden to prove a state's 'unwillingness or inability'. This limits its ability to assert jurisdiction and fulfill its mandate to end impunity, forcing it to prioritize state cooperation over immediate accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% These NGOs, legal scholars, and activists champion the cause of international accountability for grave crimes. This reading of complementarity frustrates their efforts by creating a high bar for ICC intervention, effectively narrowing the scope of international justice and allowing many perpetrators to escape accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_justice_advocates, payer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the division of labor between national and international criminal justice systems, ensuring that states retain primary responsibility for prosecuting international crimes while the ICC acts as a court of last resort.
% TRANSFER_FUNCTION: Transfers the burden of proof for inadmissibility from the state to the ICC, and effectively transfers accountability for international crimes from the international to the national level, even when national systems are weak. It also transfers the 'cost' of impunity to victims in states with weak but not 'sham' proceedings.
% ABSENT_VOICES: Victims of international crimes in states with weak but not 'sham' justice systems are effectively silenced, as their pleas for international intervention are often deemed inadmissible. Their voices are marginalized by the legalistic interpretation that prioritizes state sovereignty.
% DISAPPEARANCE_RATIONALE: If this reading of complementarity vanished, the ICC would likely assert jurisdiction more readily, leading to increased international prosecutions and a significant shift in the balance of power between national and international justice. States would face greater scrutiny, and victims would have more avenues for redress, fundamentally reorganizing the landscape of international criminal justice.
% FOUNDING_PROBLEM: The Rome Statute sought to balance the need for international accountability with respect for state sovereignty, avoiding a situation where the ICC would become a primary court, undermining national systems.
% FOUNDING_PROBLEM_CORROBORATION: States and some legal scholars argue the founding problem of balancing sovereignty and accountability remains live. However, many international justice advocates and human rights organizations, citing the persistent impunity for grave crimes in states with weak systems, argue that this reading has allowed the problem to persist, effectively making the founding problem 'dead' in practice for many victims. Independent legal analysis and human rights reports corroborate the latter view.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because this reading effectively shields states with weak but not entirely collapsed justice systems from ICC intervention, denying justice to victims in those states. Suppression (0.70) is high as it actively suppresses the ICC's ability to intervene and alternative avenues for international accountability. The theater ratio (0.40) reflects that while the principle of complementarity has a genuine function (respecting sovereignty), a significant portion of its application under this reading serves to performatively maintain state control rather than genuinely advance justice where national systems are failing. The metrics show a gradual increase in extractiveness and suppression over time, suggesting a hardening of this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national judiciaries and sovereignty-maximizing states, this constraint is a legitimate Rope, coordinating respect for sovereignty. From the perspective of victims in weak states and international justice advocates, it operates as a Snare, denying accountability and protecting perpetrators under the guise of state primacy. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   National judiciaries and sovereignty-maximizing states are clear beneficiaries (d near 0.0) as the constraint protects their authority and autonomy. Victims in weak states are targets (d near 1.0) as they are denied access to international justice. The ICC is an agenda-setter but also constrained by the high burden of proof, placing its d closer to symmetric or slightly targeted. International justice advocates are targets as their mission is directly impeded.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of complementarity prevents mislabeling a sovereignty-respecting coordination mechanism as pure extraction. However, the high extractiveness and suppression metrics, coupled with the rising theater ratio, suggest a risk of mandatrophy where the 'sovereignty protection' mandate could become a cover for impunity, shifting the constraint towards a Snare if the 'sham' threshold becomes too high to ever meet in practice. The 'contested' status of the founding problem corroborates this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_interpretation_ambiguity,
    'Is this constraint a genuine mechanism for respecting national sovereignty, or a cover for states to avoid international scrutiny?',
    'Empirical analysis of ICC admissibility decisions over time: if a high proportion of cases from states with demonstrably weak but not ''sham'' proceedings are declared inadmissible, it supports the ''cover'' interpretation.',
    'If a cover, the constraint''s effective extractiveness (from victims) and suppression (of international accountability) are higher than currently measured, reclassifying it closer to a Snare. If genuine, it remains a Tangled Rope with a strong coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_interpretation_ambiguity, empirical, 'Ambiguity in the true function of complementarity.').

omega_variable(
    national_primacy_vs_oversight_reading,
    'This constraint is the ''national_primacy_reading'' of the ''article_17_complementarity'' kernel. How would the classification change under the ''international_oversight_reading''?',
    'Adopting the ''international_oversight_reading'' would shift the burden of proof, lower the inadmissibility threshold, and expand the victim set to include those denied justice by ''unwilling or unable'' states, likely increasing extractiveness and suppression from the perspective of sovereignty-maximizing states, and decreasing it from the perspective of victims.',
    'The ''international_oversight_reading'' would likely result in a lower extractiveness score for victims and a higher one for states, potentially reclassifying the constraint as a Rope or Scaffold from the victim''s perspective, and a Snare from the state''s perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(national_primacy_vs_oversight_reading, conceptual, 'Impact of alternative kernel reading on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__national_primacy_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__national_primacy_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__national_primacy_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__national_primacy_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__national_primacy_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__national_primacy_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__national_primacy_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__national_primacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__national_primacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 17 complementarity kernel. Its sibling, 'international_oversight_reading', offers an alternative interpretation with different structural implications for state sovereignty and international accountability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
