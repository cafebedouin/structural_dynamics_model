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
 *   human_readable: ICC Article 17 Complementarity (National Primacy Reading)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'national primacy' reading of Article 17
 *   complementarity in the Rome Statute, which governs the division of labor
 *   between national courts and the International Criminal Court (ICC). Under
 *   this reading, national courts are presumed adequate unless proven to be a
 *   'sham,' placing a high burden on the ICC to demonstrate inadmissibility.
 *   This interpretation prioritizes state sovereignty and cooperation, often
 *   at the expense of broader international accountability, particularly for
 *   victims in states with weak but not entirely collapsed judicial systems.
 *
 * KEY AGENTS:
 *   - national_judiciaries: Beneficiary (institutional/constrained)
 *   - sovereignty_maximizing_states: Beneficiary (institutional/mobile)
 *   - international_criminal_court: Agenda-setter (institutional/constrained)
 *   - victims_in_weak_but_genuine_states: Payer (powerless/trapped)
 *   - international_justice_advocates: Payer (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.65).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.7).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "ICC Article 17 Complementarity (National Primacy Reading)").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, 'cc230eb1-e084-4b87-b1d4-f35033bdd9cd').
narrative_ontology:cs_kernel_codification('cc230eb1-e084-4b87-b1d4-f35033bdd9cd', fixed_text).
narrative_ontology:cs_authority_grounding('cc230eb1-e084-4b87-b1d4-f35033bdd9cd', lineage).
narrative_ontology:cs_interpretation_layer_present('cc230eb1-e084-4b87-b1d4-f35033bdd9cd').
narrative_ontology:cs_reading_relation('cc230eb1-e084-4b87-b1d4-f35033bdd9cd', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('cc230eb1-e084-4b87-b1d4-f35033bdd9cd', foundational, national_sovereignty_primacy_in_justice).
narrative_ontology:cs_axiom_status(national_sovereignty_primacy_in_justice, holdable).
narrative_ontology:cs_axiom_grounding('cc230eb1-e084-4b87-b1d4-f35033bdd9cd', national_sovereignty_primacy_in_justice, deontological).
narrative_ontology:cs_axiom('cc230eb1-e084-4b87-b1d4-f35033bdd9cd', foundational, icc_as_court_of_last_resort_only).
narrative_ontology:cs_axiom_status(icc_as_court_of_last_resort_only, holdable).
narrative_ontology:cs_axiom_grounding('cc230eb1-e084-4b87-b1d4-f35033bdd9cd', icc_as_court_of_last_resort_only, conventional).
narrative_ontology:cs_reference_frame('cc230eb1-e084-4b87-b1d4-f35033bdd9cd', state_centric_international_law).
narrative_ontology:cs_drift_state('cc230eb1-e084-4b87-b1d4-f35033bdd9cd', contemporary_icc_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cc230eb1-e084-4b87-b1d4-f35033bdd9cd', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_weak_but_genuine_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, international_justice_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Presumed competent and primary jurisdiction over international crimes. This reading protects their authority and reduces the likelihood of ICC intervention, even if their capacity is limited. They benefit from the high bar for ICC inadmissibility challenges.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, national).

% Benefit from the strong presumption of national jurisdiction, which limits the ICC's reach and protects state sovereignty. They prioritize state cooperation and non-interference in domestic affairs, even at the cost of some accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    institutional, generational, mobile, global).

% Administers Article 17, but under this reading, bears a heavy burden to prove a national proceeding is a 'sham' to assert jurisdiction. Its mandate for international justice is constrained by the deference to national systems.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_criminal_court, agenda_setter,
    institutional, civilizational, constrained, global).

% Are the primary victims of this reading, as their cases are less likely to be heard by the ICC if their national courts, despite being weak or slow, are deemed 'genuine' and not a complete sham. They face impunity for severe crimes due to the high inadmissibility threshold.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_in_weak_but_genuine_states, payer,
    powerless, biographical, trapped, local).

% Advocate for broader ICC jurisdiction to ensure accountability for grave crimes. This reading frustrates their goals by limiting the ICC's ability to intervene, forcing them to rely on often inadequate national systems.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_justice_advocates, payer,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the division of labor between national courts and the International Criminal Court, ensuring that national systems retain primary responsibility for prosecuting international crimes.
% TRANSFER_FUNCTION: Transfers the burden of proof for inadmissibility to the ICC, effectively transferring accountability for many cases from the international to the national level, even when national capacity is limited. It also transfers a degree of sovereignty protection to states.
% ABSENT_VOICES: Victims' rights groups and human rights organizations who advocate for a more robust international oversight role for the ICC are often marginalized in the interpretation of complementarity, as their concerns about impunity are subordinated to state sovereignty.
% DISAPPEARANCE_RATIONALE: If this reading of complementarity vanished, the ICC would likely assert jurisdiction more frequently, leading to a significant shift in international criminal justice. National judiciaries would face increased scrutiny and potential intervention, and the balance between sovereignty and accountability would be fundamentally altered.
% FOUNDING_PROBLEM: The Rome Statute sought to establish an international court that would complement, rather than supersede, national jurisdictions, respecting state sovereignty while addressing impunity for the most serious international crimes.
% FOUNDING_PROBLEM_CORROBORATION: States parties to the Rome Statute, particularly those wary of international overreach, consistently affirm the importance of national primacy. Legal scholars specializing in international criminal law also corroborate the ongoing tension between sovereignty and accountability, which this reading attempts to balance.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial because this reading effectively shields many cases from ICC scrutiny, leading to impunity for victims where national systems are inadequate but not 'sham.' Suppression (0.70) is high due to the active legal and political efforts by states to maintain this high inadmissibility threshold, limiting the ICC's ability to intervene. The theater ratio (0.20) is moderate; while national proceedings are genuinely conducted, the 'sham' standard can sometimes mask insufficient efforts. Accessibility collapse (0.40) is moderate, as alternatives (ICC intervention) are significantly constrained but not entirely absent. Resistance (0.50) is also moderate, reflecting ongoing advocacy and legal challenges by victims' groups and some states against this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national judiciaries and sovereignty-maximizing states, this reading is a legitimate and necessary protection of sovereignty and a proper division of labor. From the perspective of victims and international justice advocates, it is an extractive mechanism that enables impunity by creating an overly high bar for ICC intervention. The ICC itself, as the agenda-setter, experiences the constraint as a constant tension between its mandate and the deference required by this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   National judiciaries and sovereignty-maximizing states are clear beneficiaries, as the constraint protects their authority and limits external interference. The ICC, while an agenda-setter, is also constrained by the high burden of proof, making its directionality closer to symmetric or even slightly targeted in its ability to fulfill its mandate. Victims in states with weak but genuine proceedings are clear targets, as their access to justice is significantly curtailed. International justice advocates are also targets, as their efforts to expand accountability are frustrated.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of complementarity prevents mislabeling by clarifying that the constraint's primary function, from this perspective, is sovereignty protection, not pure extraction. However, the high extractiveness and suppression indicate that this protection comes at a significant cost to victims and the broader goal of international accountability. The 'live' status of the founding problem, coupled with the 'world rearranges' disappearance verdict, suggests the constraint is not a piton, but a tangled rope where the coordination function (sovereignty protection) is intertwined with asymmetric extraction (impunity for victims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sham_standard_objectivity,
    'Is the ''sham'' standard for inadmissibility objectively verifiable, or is it subject to political influence and judicial discretion, leading to inconsistent application?',
    'Empirical analysis of ICC admissibility decisions over time, correlating outcomes with political pressure on the Court and the specific judicial capacity of the state in question.',
    'If highly subjective, the effective extractiveness and suppression would be higher than measured, as the standard would serve as a flexible tool for states to avoid ICC intervention. If objective, the constraint would be closer to a genuine rope, albeit one with a high coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_standard_objectivity, empirical, 'Ambiguity in the application of the ''sham'' standard for ICC inadmissibility.').

omega_variable(
    sovereignty_accountability_balance,
    'What is the optimal balance between state sovereignty and international accountability in the prosecution of international crimes, and does this reading achieve it?',
    'Conceptual analysis and normative debate among international legal scholars and policymakers, weighing the values of state autonomy against the imperative to end impunity.',
    'If this reading is deemed to unduly prioritize sovereignty, it would be reclassified closer to a snare. If it is seen as a necessary compromise, its ''tangled rope'' classification would be reinforced, but with a higher acknowledged cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_accountability_balance, preference, 'The normative trade-off between state sovereignty and international accountability.').

omega_variable(
    reading_impact_on_national_capacity,
    'Does this reading, by deferring to national courts, incentivize states to genuinely improve their judicial capacity, or does it allow them to maintain weak systems without fear of ICC intervention?',
    'Longitudinal study of judicial reform efforts in states where ICC intervention was considered but ultimately deferred due to this reading of complementarity.',
    'If it incentivizes improvement, the long-term extractiveness for victims might decrease. If it enables complacency, the extractiveness remains high, and the ''tangled rope'' classification is strengthened, potentially leaning towards a snare if the coordination function becomes purely theatrical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_impact_on_national_capacity, empirical, 'Impact of national primacy reading on state judicial capacity building.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__national_primacy_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__national_primacy_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__national_primacy_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__national_primacy_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__national_primacy_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__national_primacy_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__national_primacy_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__national_primacy_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__national_primacy_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of two primary readings of ICC Article 17 complementarity. The 'international oversight' reading emphasizes the ICC's role in ensuring accountability, while this 'national primacy' reading prioritizes state sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
