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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction: Hybrid Complementarity Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid complementarity' reading of the
 *   Rome Statute's jurisdictional framework. It acknowledges the ICC's
 *   residual universal authority to prosecute atrocity crimes, but emphasizes
 *   that this authority is operationally constrained by the principle of
 *   complementarity, which defers to national jurisdictions. Jurisdiction
 *   exists, but its enforcement heavily depends on state cooperation. This
 *   reading sees the authority as grounded in a blend of natural law
 *   aspirations for universal justice and the practical realities of treaty
 *   consent and state sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.45).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.6).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction: Hybrid Complementarity Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '991287f1-05ee-4cea-9411-45a47dfed520').
narrative_ontology:cs_kernel_codification('991287f1-05ee-4cea-9411-45a47dfed520', fixed_text).
narrative_ontology:cs_authority_grounding('991287f1-05ee-4cea-9411-45a47dfed520', lineage).
narrative_ontology:cs_interpretation_layer_present('991287f1-05ee-4cea-9411-45a47dfed520').
narrative_ontology:cs_reading_relation('991287f1-05ee-4cea-9411-45a47dfed520', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('991287f1-05ee-4cea-9411-45a47dfed520', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('991287f1-05ee-4cea-9411-45a47dfed520', foundational, universal_justice_aspiration_constrained_by_consent).
narrative_ontology:cs_axiom_status(universal_justice_aspiration_constrained_by_consent, holdable).
narrative_ontology:cs_axiom_grounding('991287f1-05ee-4cea-9411-45a47dfed520', universal_justice_aspiration_constrained_by_consent, deontological).
narrative_ontology:cs_axiom('991287f1-05ee-4cea-9411-45a47dfed520', foundational, complementarity_as_operational_deference).
narrative_ontology:cs_axiom_status(complementarity_as_operational_deference, holdable).
narrative_ontology:cs_axiom_grounding('991287f1-05ee-4cea-9411-45a47dfed520', complementarity_as_operational_deference, conventional).
narrative_ontology:cs_reference_frame('991287f1-05ee-4cea-9411-45a47dfed520', post_cold_war_international_criminal_justice_consensus).
narrative_ontology:cs_drift_state('991287f1-05ee-4cea-9411-45a47dfed520', contemporary_geopolitical_fragmentation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('991287f1-05ee-4cea-9411-45a47dfed520', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_atrocity_crimes).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, non_cooperating_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals_in_non_cooperating_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_to_rome_statute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institution responsible for prosecuting individuals for international crimes. Its jurisdiction is activated by complementarity, meaning it defers to national courts unless they are unwilling or unable to genuinely investigate or prosecute. It benefits from the moral authority of universal justice but is constrained by state cooperation for enforcement.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% These states have consented to the ICC's jurisdiction and benefit from the collective security and deterrence against atrocity crimes. They retain primary jurisdiction over their own nationals and territories, with the ICC acting as a court of last resort. Their cooperation is essential for the ICC's effectiveness.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_to_rome_statute, beneficiary,
    institutional, generational, mobile, global).

% States that are not parties to the Rome Statute or refuse to cooperate with the ICC. They bear the cost of potential international condemnation and the risk of their nationals being subject to ICC jurisdiction through UN Security Council referrals or other mechanisms, despite their claims of sovereign immunity. They actively resist ICC intervention.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_cooperating_states, payer,
    institutional, biographical, constrained, global).

% These individuals benefit from the ICC's mandate to provide justice where national systems fail. The ICC offers a potential avenue for accountability and redress, even if enforcement remains challenging. Their access to justice is often contingent on the political will of states.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_atrocity_crimes, beneficiary,
    powerless, biographical, trapped, local).

% Individuals accused of international crimes who reside in states unwilling to cooperate with the ICC. They face the threat of ICC prosecution and arrest warrants, but are often shielded by their state's non-cooperation, creating a gap between legal jurisdiction and practical enforcement. Their fate is highly dependent on geopolitical shifts.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals_in_non_cooperating_states, payer,
    powerless, immediate, trapped, local).

% Analyze the evolving interpretation and application of the Rome Statute, particularly the complementarity principle. They assess the balance between universal justice and state sovereignty, and the effectiveness of the ICC's hybrid approach. Their work influences legal discourse and policy debates.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to prosecute the most serious international crimes by establishing a permanent court that acts as a backstop when national jurisdictions are unwilling or unable to do so, thereby preventing impunity and promoting rule of law.
% TRANSFER_FUNCTION: Transfers the authority to prosecute international crimes from national jurisdictions (when they fail) to the ICC, and transfers the burden of enforcement (arrests, evidence collection) from the ICC back to cooperating states.
% ABSENT_VOICES: States that vehemently reject any international jurisdiction over their nationals, viewing it as an infringement on absolute sovereignty, are largely absent from the formal mechanisms of the Rome Statute, though their resistance shapes its operational limits. Victims in non-cooperating states often lack direct voice in the ICC's jurisdictional decisions.
% DISAPPEARANCE_RATIONALE: If the Rome Statute and its complementarity mechanism vanished, the international legal landscape for atrocity crimes would revert to ad hoc tribunals or purely national prosecutions, leading to greater impunity, reduced deterrence, and a significant setback for universal justice. The balance between sovereignty and international justice would be fundamentally altered.
% FOUNDING_PROBLEM: The problem of impunity for genocide, war crimes, crimes against humanity, and aggression, where national courts were either unwilling or unable to prosecute, leading to a cycle of violence and injustice.
% FOUNDING_PROBLEM_CORROBORATION: The problem of impunity remains live, as evidenced by ongoing conflicts and atrocities where national justice systems are compromised. Human rights organizations, UN bodies, and victim groups consistently corroborate the continued need for an international court, even with its limitations.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) reflects the tension: the ICC can assert jurisdiction, but its ability to 'extract' individuals for trial is limited by state cooperation. Suppression (0.6) is moderate because while the ICC can issue warrants, it lacks its own enforcement arm, relying on states. The theater ratio (0.2) indicates that while there's genuine legal work, some actions (e.g., warrants against heads of state without enforcement prospects) can appear performative, highlighting the gap between legal authority and political reality. The claimed type is Tangled Rope because it genuinely coordinates international justice efforts (beneficiaries: ICC, victims) but also involves asymmetric extraction from non-cooperating states and accused individuals, requiring active enforcement (even if often frustrated).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ICC and victims, the constraint is a vital mechanism for justice, albeit one with operational challenges. From non-cooperating states, it is an overreach of international power. This reading attempts to bridge these perspectives by emphasizing the 'hybrid' nature of complementarity, where universal aspirations meet sovereign realities.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC and victims are beneficiaries, as the constraint provides a mechanism for justice. State parties are also beneficiaries, as they participate in a system that deters crimes and offers a backstop. Non-cooperating states and accused individuals within them are victims, as they face the assertion of ICC jurisdiction against their will or without their state's consent. International legal scholars act as observers, analyzing the system's effectiveness and legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ending impunity for atrocity crimes) is still very much live. The classification as Tangled Rope prevents mislabeling it as a pure Snare (which would ignore its genuine coordination function for justice) or a pure Rope (which would ignore the significant extraction from non-cooperating states and the active enforcement required to maintain its jurisdictional claims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_genuineness,
    'Is the complementarity principle genuinely deferential to national systems, or does it primarily serve as a legalistic cover for the ICC''s assertion of residual universal jurisdiction?',
    'Empirical analysis of ICC admissibility decisions: a high rate of deference to national proceedings would support genuine deference; a high rate of ICC intervention would suggest a more assertive universalist interpretation in practice.',
    'If primarily a cover, the constraint''s effective extractiveness from non-cooperating states is higher, pushing it closer to a Snare. If genuinely deferential, it remains a Tangled Rope with a stronger coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_genuineness, empirical, 'Ambiguity regarding the practical application and intent of the complementarity principle.').

omega_variable(
    enforcement_gap_legitimacy,
    'Does the persistent gap between ICC jurisdiction and actual enforcement (due to state non-cooperation) undermine the constraint''s legitimacy, or is it an acceptable cost of asserting universal justice?',
    'Analysis of state practice and international legal discourse over time: if the gap leads to widespread repudiation of ICC authority, legitimacy is undermined. If the assertion of jurisdiction, even without immediate enforcement, is seen as upholding a norm, legitimacy holds.',
    'If legitimacy is undermined, the constraint''s theater_ratio increases, and its effective suppression decreases, pushing it towards a Piton. If it holds, it remains a Tangled Rope, albeit one with significant operational challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_legitimacy, conceptual, 'The impact of enforcement challenges on the ICC''s perceived legitimacy.').


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
narrative_ontology:measurement(rome_tr_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 1998, 0.3).
narrative_ontology:measurement(rome_be_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2004, 0.35).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(rome_be_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2016, 0.43).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 1998, 0.4).
narrative_ontology:measurement(rome_su_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2004, 0.48).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(rome_su_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2016, 0.58).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'rome_statute_jurisdiction' kernel. This 'hybrid_complementarity_reading' balances universal aspirations with sovereign primacy. It coexists with the 'universalist_reading' and 'sovereigntist_reading', which emphasize different aspects of the Statute's authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
