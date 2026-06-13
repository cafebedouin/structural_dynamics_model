% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction (Hybrid Complementarity Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid complementarity' reading of the
 *   Rome Statute's jurisdictional framework. It acknowledges the ICC's
 *   residual universal authority for international criminal justice while
 *   recognizing its operational constraints due to the complementarity
 *   principle, which defers to national jurisdictions, and its reliance on
 *   state cooperation for enforcement. The ICC's jurisdiction exists, but its
 *   effective reach is limited by political will and sovereign consent. This
 *   reading attempts to balance the universal aspiration of international
 *   justice with the realities of state sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.45).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.3).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction (Hybrid Complementarity Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '6a2dfc9f-f883-4084-b7f6-785c8502fd2d').
narrative_ontology:cs_kernel_codification('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', formalized).
narrative_ontology:cs_authority_grounding('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', lineage).
narrative_ontology:cs_interpretation_layer_present('6a2dfc9f-f883-4084-b7f6-785c8502fd2d').
narrative_ontology:cs_reading_relation('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', foundational, icc_has_residual_universal_authority).
narrative_ontology:cs_axiom_status(icc_has_residual_universal_authority, holdable).
narrative_ontology:cs_axiom_grounding('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', icc_has_residual_universal_authority, deontological).
narrative_ontology:cs_axiom('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', foundational, complementarity_defers_to_capable_national_jurisdictions).
narrative_ontology:cs_axiom_status(complementarity_defers_to_capable_national_jurisdictions, holdable).
narrative_ontology:cs_axiom_grounding('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', complementarity_defers_to_capable_national_jurisdictions, conventional).
narrative_ontology:cs_reference_frame('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', post_rome_statute_era_balancing_act).
narrative_ontology:cs_drift_state('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', contemporary_political_challenges, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6a2dfc9f-f883-4084-b7f6-785c8502fd2d', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_atrocity_crimes).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, non_cooperating_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_to_rome_statute).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, principle_of_complementarity).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, universal_jurisdiction_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institution responsible for investigating and prosecuting individuals for atrocity crimes. Its jurisdiction is activated when national courts are unwilling or unable to do so, but its enforcement capacity relies heavily on state cooperation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the collective security and justice provided by the ICC, which acts as a court of last resort. They consent to the ICC's jurisdiction but retain primary responsibility for prosecuting crimes within their borders. They can withdraw from the Statute, but face diplomatic costs.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_to_rome_statute, beneficiary,
    organized, generational, mobile, global).

% States that are not parties to the Rome Statute or those that are parties but refuse to cooperate with ICC investigations or arrest warrants. They bear the cost of international condemnation, potential sanctions, and the risk of their nationals being subject to ICC jurisdiction if crimes occur on the territory of a State Party or are referred by the UN Security Council.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_cooperating_states, payer,
    powerful, biographical, constrained, national).

% Benefit from the ICC's mandate to provide justice where national systems fail. Their access to justice is contingent on the ICC's ability to assert jurisdiction and secure cooperation, which is often limited by political realities.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_atrocity_crimes, beneficiary,
    powerless, generational, trapped, local).

% Individuals accused of atrocity crimes, who face prosecution by the ICC if their national courts are unwilling or unable to act. Their fate is determined by the interplay of ICC jurisdiction and state cooperation, often with limited recourse.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals, payer,
    powerless, immediate, trapped, local).

% Monitor the ICC's effectiveness, advocate for stronger enforcement, and push for universal ratification of the Rome Statute. They provide critical analysis of the balance between universal justice and state sovereignty.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to prosecute atrocity crimes, ensuring that perpetrators do not escape justice when national systems are unwilling or unable to act, thereby upholding a minimum standard of international criminal justice.
% TRANSFER_FUNCTION: Transfers the authority to prosecute individuals for atrocity crimes from national jurisdictions (when they fail) to the ICC, and transfers the burden of enforcement (arrests, evidence collection) back to states.
% ABSENT_VOICES: States that actively resist international criminal justice or seek to shield their nationals from prosecution are structurally absent from the ICC's operational decision-making, though their non-cooperation is a constant factor. They would argue for absolute state sovereignty and non-interference.
% DISAPPEARANCE_RATIONALE: If the Rome Statute and its complementarity mechanism vanished, there would be a significant vacuum in international criminal justice. Perpetrators of atrocity crimes would face fewer avenues for prosecution, potentially leading to increased impunity and a weakening of international norms against such crimes. States would lose a critical backstop for justice.
% FOUNDING_PROBLEM: The problem of impunity for atrocity crimes (genocide, crimes against humanity, war crimes) where national courts were unwilling or unable to prosecute, leading to a breakdown of justice and a cycle of violence.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and numerous victim groups consistently attest that the problem of impunity remains live, even with the ICC's existence. While the ICC has secured some convictions, many perpetrators still evade justice due to political obstacles and non-cooperation from states, corroborating the ongoing need for the mechanism.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).
:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates international efforts against impunity (beneficiaries: ICC, victims) but also involves asymmetric extraction from non-cooperating states and accused individuals, requiring active enforcement to maintain its authority. Extractiveness (0.45) is moderate, reflecting the ICC's limited but real power to compel action. Suppression (0.30) is relatively low, as the ICC lacks its own enforcement arm and relies on states, which can choose to resist. Theater ratio (0.20) is also low, as the ICC's operations are largely functional, though political posturing around its legitimacy can introduce some performativity.
 *
 * PERSPECTIVAL GAP:
 *   The ICC and victims experience this as a vital, albeit imperfect, mechanism for justice. Non-cooperating states and accused individuals experience it as an imposition on sovereignty or a threat to their freedom. State parties navigate a balance between their commitment to international justice and their sovereign interests. The hybrid complementarity reading attempts to bridge these perspectives by acknowledging both the universal mandate and the practical limitations.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC and victims are beneficiaries (d near 0.0) as the constraint provides a mechanism for justice. State parties are also beneficiaries, gaining collective security and a mechanism for accountability without fully ceding sovereignty. Non-cooperating states and accused individuals are targets (d near 1.0) as the constraint imposes obligations or threats of prosecution against their will. The 'hybrid' nature means that even beneficiaries bear some costs (e.g., potential loss of sovereignty for State Parties), and targets may occasionally benefit from the rule of law, but the overall flow is clear.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ending impunity for atrocity crimes) remains live, as attested by external observers. The complementarity mechanism, while sometimes seen as a weakness, is also what allows the ICC to function within a system of sovereign states. The classification as Tangled Rope prevents mislabeling it as a pure Snare (ignoring its coordination function) or a pure Rope (ignoring its extractive aspects and enforcement challenges). The ongoing contestation over its reach and enforcement prevents it from becoming a Piton, as there are active parties (victims, advocates) pushing for its full realization and states actively resisting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_as_deference_or_loophole,
    'Is the complementarity principle primarily a mechanism for deference to national sovereignty, or a loophole for states to avoid ICC jurisdiction?',
    'Empirical analysis of national prosecutions: if states consistently prosecute atrocity crimes when the ICC defers, it''s deference; if they consistently fail to prosecute, it''s a loophole.',
    'If primarily a loophole, the constraint''s effective extractiveness from victims (denied justice) is higher, and its theater_ratio increases, pushing it closer to a Snare. If genuine deference, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_as_deference_or_loophole, empirical, 'Ambiguity in the function of the complementarity principle.').

omega_variable(
    universal_mandate_vs_state_cooperation,
    'To what extent can the ICC''s universal mandate for justice be realized without universal state cooperation for enforcement?',
    'Longitudinal study of ICC cases: track the success rate of investigations and prosecutions in the face of non-cooperation versus full cooperation. Analyze the impact of UN Security Council referrals.',
    'If the mandate is largely unrealizable without cooperation, the constraint''s effective suppression of justice for victims is higher, and its theater_ratio increases. If the ICC can find effective workarounds, it reinforces its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_mandate_vs_state_cooperation, empirical, 'Tension between the ICC''s aspirational mandate and its practical reliance on states.').

omega_variable(
    reading_framing_impact,
    'Does framing the Rome Statute''s jurisdiction as ''hybrid complementarity'' accurately reflect its operational reality, or does it obscure a more extractive or purely coordinative structure?',
    'Comparative analysis with ''universalist_reading'' and ''sovereigntist_reading'': which reading''s metrics (extractiveness, suppression) best align with observed outcomes for victims and states?',
    'If a different reading''s metrics align better, this ''hybrid'' framing might be a conceptual cover for a different structural reality, requiring reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'Conceptual ambiguity in the most accurate framing of the Rome Statute''s jurisdictional balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(rome_tr_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(rome_tr_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(rome_tr_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 1998, 0.3).
narrative_ontology:measurement(rome_be_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2004, 0.35).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(rome_be_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2016, 0.43).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 1998, 0.2).
narrative_ontology:measurement(rome_su_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2004, 0.23).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2010, 0.26).
narrative_ontology:measurement(rome_su_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2016, 0.28).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, principle_of_universal_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'rome_statute_jurisdiction' kernel, focusing on the balance between universal justice and state sovereignty via complementarity. It is linked to the 'universalist_reading' and 'sovereigntist_reading' which represent alternative interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
