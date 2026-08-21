% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdiction (Sovereigntist Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents the 'sovereigntist reading' of the Rome
 *   Statute's jurisdictional framework, emphasizing strict adherence to state
 *   consent as a prerequisite for ICC intervention. It views the ICC as a
 *   conditional mechanism that defers to national sovereignty and legal
 *   systems, with exceptions primarily through UN Security Council referrals.
 *   This reading limits the ICC's ability to act independently of state will,
 *   thereby protecting state autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.25).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.15).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction (Sovereigntist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '0d71980c-4c44-41a9-af9f-42c4dfeb0732').
narrative_ontology:cs_kernel_codification('0d71980c-4c44-41a9-af9f-42c4dfeb0732', formalized).
narrative_ontology:cs_authority_grounding('0d71980c-4c44-41a9-af9f-42c4dfeb0732', lineage).
narrative_ontology:cs_interpretation_layer_present('0d71980c-4c44-41a9-af9f-42c4dfeb0732').
narrative_ontology:cs_reading_relation('0d71980c-4c44-41a9-af9f-42c4dfeb0732', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d71980c-4c44-41a9-af9f-42c4dfeb0732', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('0d71980c-4c44-41a9-af9f-42c4dfeb0732', foundational, state_sovereignty_primacy).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('0d71980c-4c44-41a9-af9f-42c4dfeb0732', state_sovereignty_primacy, deontological).
narrative_ontology:cs_axiom('0d71980c-4c44-41a9-af9f-42c4dfeb0732', foundational, consent_as_basis_of_jurisdiction).
narrative_ontology:cs_axiom_status(consent_as_basis_of_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('0d71980c-4c44-41a9-af9f-42c4dfeb0732', consent_as_basis_of_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('0d71980c-4c44-41a9-af9f-42c4dfeb0732', westphalian_state_centric_order).
narrative_ontology:cs_drift_state('0d71980c-4c44-41a9-af9f-42c4dfeb0732', contemporary_international_law, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('0d71980c-4c44-41a9-af9f-42c4dfeb0732', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_party_nationals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, international_criminal_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States retain primary jurisdiction over their nationals and territory, consenting to ICC jurisdiction only under specific conditions. They benefit from the preservation of their sovereignty and the principle of non-interference in internal affairs.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, sovereign_states, agenda_setter,
    institutional, generational, mobile, global).

% Nationals of states not party to the Rome Statute are generally immune from ICC jurisdiction, unless their state consents or the UN Security Council refers a situation. They benefit from this jurisdictional limitation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_nationals, beneficiary,
    powerless, biographical, arbitrage, global).

% The ICC's jurisdiction is strictly limited by sovereign consent, requiring it to defer to national courts and obtain referrals for non-party states. This constrains its operational scope and resource allocation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, international_criminal_court, payer,
    institutional, generational, constrained, global).

% Retains the power to refer situations to the ICC, including those involving non-party states, thereby overriding sovereign consent in specific cases. This power is a key mechanism for balancing sovereignty with international justice.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% Their access to international justice is contingent on state consent or UNSC action, which can limit accountability for crimes committed in non-consenting states. They are often excluded from the jurisdictional decision-making process.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, victims_of_atrocity_crimes, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of international criminal jurisdiction with the foundational principle of state sovereignty, ensuring that international intervention respects national legal systems and political autonomy.
% TRANSFER_FUNCTION: Transfers the primary authority for prosecuting international crimes from the ICC back to national legal systems, except where states consent or the UNSC intervenes. It also transfers the burden of proof for ICC jurisdiction to demonstrate state consent or UNSC referral.
% ABSENT_VOICES: Victims of atrocity crimes in non-consenting states, and advocates for universal jurisdiction, are largely absent from the decision-making process that defines the ICC's jurisdictional limits. They would argue for a more expansive interpretation of the ICC's mandate.
% DISAPPEARANCE_RATIONALE: If this sovereigntist reading of jurisdiction vanished, the ICC's operational scope would expand dramatically, potentially leading to more frequent interventions in non-consenting states. This would fundamentally alter the balance of power between international institutions and sovereign states, leading to significant geopolitical rearrangements and legal challenges.
% FOUNDING_PROBLEM: The problem of establishing an international criminal court that could address grave crimes without undermining the fundamental principles of state sovereignty and non-interference.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and many states (especially non-parties to the Rome Statute) corroborate that balancing sovereignty with international justice remains a live and contentious problem. The ongoing debates at the UN and in national legislatures attest to this.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).
:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.25) because this reading primarily protects sovereign states from unwanted international intervention, rather than extracting from them. Suppression is also low (0.15) as it reflects the inherent limitations on international law enforcement in a state-centric system, rather than active coercion. The constraint functions as a 'rope' by coordinating international justice efforts with the established norms of state sovereignty, providing a framework for cooperation rather than extraction. The slight increase in extractiveness and suppression over time reflects the ongoing tension and occasional challenges to this reading from more universalist perspectives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereign states, this reading is a necessary safeguard of their autonomy and a legitimate framework for international cooperation. From the perspective of victims of atrocity crimes or universalist advocates, it represents a significant barrier to justice, prioritizing state interests over individual accountability. The ICC itself, as an institution, experiences this reading as a constraint on its mandate and operational capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and non-party nationals are beneficiaries, as their jurisdictional autonomy is preserved. The ICC is a payer, as its power is constrained by this reading. The UN Security Council acts as an agenda-setter, capable of altering the jurisdictional landscape through referrals. Victims of atrocity crimes are excluded, as their access to justice is mediated by state consent or UNSC action.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the Rome Statute as a 'snare' for states, by emphasizing its consensual and deferential nature. It highlights the coordination function of balancing international justice with sovereignty, rather than portraying it as a purely extractive mechanism. The 'live' status of the founding problem indicates that the constraint's mandate is still relevant, preventing it from being classified as a 'piton' due to obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_universal_justice,
    'Is the strict sovereign consent framework a necessary condition for international legal order, or an impediment to universal justice for atrocity crimes?',
    'Analysis of state practice and international legal developments over time, particularly regarding the willingness of states to waive consent or accept UNSC referrals without political obstruction.',
    'If deemed an impediment, the extractiveness for victims would be re-evaluated as higher, and the constraint might shift towards a ''tangled_rope'' or ''snare'' for those seeking justice. If necessary, its ''rope'' classification would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_universal_justice, conceptual, 'Ambiguity regarding the fundamental balance between state sovereignty and the pursuit of universal justice.').

omega_variable(
    unsc_referral_politicization,
    'To what extent are UN Security Council referrals of non-party states to the ICC politicized, rather than purely justice-driven?',
    'Empirical study of UNSC referral patterns, veto usage, and the geopolitical context surrounding each referral, assessing consistency with humanitarian principles versus national interests.',
    'If highly politicized, the ''sovereigntist reading'' would be seen as enabling selective justice, increasing its effective extractiveness for victims and potentially shifting its classification towards a ''tangled_rope'' due to asymmetric application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_referral_politicization, empirical, 'The role of political considerations in UNSC referrals affecting ICC jurisdiction.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''sovereigntist reading'' of the Rome Statute, or does it contain elements that align more closely with a ''hybrid complementarity'' or ''universalist'' reading?',
    'Detailed textual analysis of state declarations, judicial interpretations, and scholarly commentary, comparing the emphasis on consent, deference, and the scope of ICC action against the core tenets of each reading.',
    'If elements of other readings are dominant, the classification, beneficiaries, and victims would need to be re-evaluated under the appropriate sibling constraint, potentially altering the perceived extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Clarifying the precise boundaries and emphasis of this specific reading within the broader kernel of Rome Statute jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(rome_tr_t2005, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(rome_tr_t2012, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2012, 0.09).
narrative_ontology:measurement(rome_tr_t2018, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.2).
narrative_ontology:measurement(rome_be_t2005, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(rome_be_t2012, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2012, 0.23).
narrative_ontology:measurement(rome_be_t2018, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2018, 0.24).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.1).
narrative_ontology:measurement(rome_su_t2005, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2005, 0.12).
narrative_ontology:measurement(rome_su_t2012, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2012, 0.13).
narrative_ontology:measurement(rome_su_t2018, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2018, 0.14).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, international_criminal_court_mandate).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, principle_of_complementarity).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Rome Statute's jurisdictional framework. The 'sovereigntist reading' emphasizes state consent, while the 'universalist reading' prioritizes international justice, and the 'hybrid complementarity reading' seeks a balance. Each reading constitutes a separate constraint due to differing ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
