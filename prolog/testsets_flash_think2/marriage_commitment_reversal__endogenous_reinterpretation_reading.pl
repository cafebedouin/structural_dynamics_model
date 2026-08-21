% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Divine Revelation Reinterpreting Marriage Commitment (Endogenous Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the reinterpretation of a core religious
 *   commitment (marriage practice) through an internal divine revelation
 *   (Woodruff's 1890 Manifesto) in response to external pressures. This
 *   reading emphasizes the prophet's continued interpretive authority and the
 *   endogenous nature of the doctrinal shift, preserving institutional
 *   legitimacy. The constraint functions as a 'tangled rope' because it
 *   coordinates the community around a new understanding of divine will while
 *   extracting costs from theological consistency and dissenting members.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.6).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.7).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Divine Revelation Reinterpreting Marriage Commitment (Endogenous Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e72327a5-e511-43c5-953c-e46fd08eca5b').
narrative_ontology:cs_kernel_codification('e72327a5-e511-43c5-953c-e46fd08eca5b', formalized).
narrative_ontology:cs_authority_grounding('e72327a5-e511-43c5-953c-e46fd08eca5b', lineage).
narrative_ontology:cs_interpretation_layer_present('e72327a5-e511-43c5-953c-e46fd08eca5b').
narrative_ontology:cs_reading_relation('e72327a5-e511-43c5-953c-e46fd08eca5b', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('e72327a5-e511-43c5-953c-e46fd08eca5b', marriage_commitment_reversal__practice_doctrine_gap, forecloses).
narrative_ontology:cs_axiom('e72327a5-e511-43c5-953c-e46fd08eca5b', foundational, prophetic_infallibility_in_revelation).
narrative_ontology:cs_axiom_status(prophetic_infallibility_in_revelation, holdable).
narrative_ontology:cs_axiom_grounding('e72327a5-e511-43c5-953c-e46fd08eca5b', prophetic_infallibility_in_revelation, theological).
narrative_ontology:cs_axiom('e72327a5-e511-43c5-953c-e46fd08eca5b', secondary, divine_will_is_dynamic).
narrative_ontology:cs_axiom_status(divine_will_is_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('e72327a5-e511-43c5-953c-e46fd08eca5b', divine_will_is_dynamic, theological).
narrative_ontology:cs_reference_frame('e72327a5-e511-43c5-953c-e46fd08eca5b', prophetic_revelation_as_supreme_authority).
narrative_ontology:cs_drift_state('e72327a5-e511-43c5-953c-e46fd08eca5b', woodruff_sept_23_vision, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e72327a5-e511-43c5-953c-e46fd08eca5b', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissenting_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_members).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_members).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_authority).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, divine_revelation_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The prophet and senior ecclesiastical leaders who receive and promulgate the new revelation. They maintain interpretive authority and institutional legitimacy by framing the change as God's will, thereby coordinating the community around their continued leadership.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership, beneficiary).

% The general membership who accept the new revelation. They bear the cognitive cost of reconciling past doctrine with present practice, but benefit from institutional stability and continued participation in the community. Their identity is deeply intertwined with the institution.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_members, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_members, beneficiary).

% Members who struggle with or reject the reinterpretation, finding it inconsistent with prior divine commands. They face social pressure, spiritual isolation, and potential excommunication, bearing the cost of theological inconsistency or loss of community.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissenting_members, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissenting_members, excluded).

% The abstract principle of coherence and immutability in divine doctrine. From this reading's perspective, it is 'paid' or reinterpreted to accommodate new divine will, rather than being a fixed, unchangeable standard against which revelation is judged.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).

% Historians, sociologists of religion, and other scholars who analyze the event from an academic perspective, examining the social, political, and theological dynamics without being bound by institutional commitments.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the religious community's understanding of divine will and institutional practice, ensuring continued adherence to prophetic authority and maintaining institutional unity amidst changing circumstances.
% TRANSFER_FUNCTION: Transfers interpretive authority and theological flexibility to the institutional leadership, and transfers the burden of reconciling doctrinal shifts to the membership. It also transfers the 'cost' of inconsistency to the abstract principle of theological consistency itself.
% ABSENT_VOICES: Those who would insist on a literal, unchanging interpretation of prior divine commands, or who would challenge the prophet's authority to reinterpret God's will. They are excluded by the framing of the change as direct divine revelation, which delegitimizes dissent.
% DISAPPEARANCE_RATIONALE: If the revelation and its acceptance vanished, the religious institution would face an existential crisis regarding its foundational doctrines, prophetic authority, and historical integrity. The community would likely fracture, and its legitimacy would be severely undermined.
% FOUNDING_PROBLEM: The institution faced an irreconcilable conflict between a deeply held religious practice (plural marriage) and external legal/social pressures, threatening its existence and the freedom of its members.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, contemporary accounts, and legal documents from outside the institutional leadership (e.g., federal government records, newspaper reports, non-member testimonies) corroborate the severe external pressures and existential threat faced by the institution at the time, confirming the founding problem was genuinely live.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.6) as the reinterpretation requires members to accept a shift in divine command, which can be a significant cognitive and spiritual cost, but it also preserves the institution. Suppression is moderate-high (0.7) due to the strong social and spiritual pressures within a high-demand religious community to conform to prophetic guidance; dissent, while not met with physical force, leads to social marginalization or excommunication. Theater ratio is low (0.2) because the revelation is presented and largely accepted as a genuine divine communication, not a performance. Accessibility collapse is moderate (0.6) as leaving the institution is a high-cost option due to identity lock, but other religious paths exist. Resistance is moderate (0.5) with some members leaving or expressing private doubts, but no widespread organized rebellion.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's perspective, this is a necessary and divinely sanctioned act of coordination to preserve the church. From the perspective of dissenting members, it is a betrayal of prior divine commands and an act of institutional pragmatism disguised as revelation. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional leadership is the primary beneficiary (d=0.0-0.1) as their authority is reaffirmed and the institution is preserved. Church members are beneficiaries of institutional stability but also payers of cognitive/spiritual costs (d=0.4-0.5). Dissenting members and the abstract principle of theological consistency are targets (d=0.8-1.0), bearing the costs of the reinterpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the reinterpretation as a 'mountain' (unchanging divine will) or a 'snare' (pure extraction). It acknowledges the genuine coordination function of maintaining institutional unity and prophetic authority, while also recognizing the asymmetric extraction of theological consistency and the costs borne by dissenting members. The 'tangled rope' accurately captures this hybrid nature, preventing the naturalization of a constructed reinterpretation or the oversimplification of its complex effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_consistency_ambiguity,
    'Is the reinterpretation a genuine evolution of divine understanding, or a pragmatic capitulation to external pressure framed as revelation?',
    'Comparative theological analysis of similar historical shifts in other religious traditions, or internal institutional documents revealing strategic discussions prior to the revelation.',
    'If primarily pragmatic, the extractiveness from ''theological_consistency'' is higher, and the ''theater_ratio'' might be understated. If genuinely theological, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_consistency_ambiguity, conceptual, 'Ambiguity regarding the true nature of the doctrinal shift.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of dissent primarily structural (fear of excommunication, social ostracism) or internalized (deeply held belief in prophetic infallibility)?',
    'Longitudinal studies of ex-members'' post-exit psychological trajectories, or analysis of internal institutional messaging regarding dissent.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as members carry the suppression with them even after leaving. If structural, removing the institutional threat would more readily enable dissent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissent.').

omega_variable(
    framing_under_determination,
    'Does this ''endogenous reinterpretation'' framing fully capture the event, or do alternative framings (exogenous override, practice-doctrine gap) offer equally coherent, but structurally different, classifications?',
    'Analysis of historical evidence for external coercion (exogenous override) or ongoing doctrinal ambiguity (practice-doctrine gap) that this reading''s narrative obscures.',
    'If the ''exogenous override'' framing is dominant, the constraint''s ''suppression'' and ''extractiveness'' might be higher, driven by external forces. If the ''practice-doctrine gap'' is dominant, the ''claimed_type'' might shift to a ''piton'' or ''snare'' reflecting an unresolved tension rather than a resolved reinterpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Alternative coherent framings of the marriage commitment reversal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1890, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1895, 0.18).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(marr_tr_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1905, 0.19).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.2).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(marr_be_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1895, 0.58).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(marr_be_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1905, 0.59).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.6).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1895, 0.68).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(marr_su_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1905, 0.69).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.7).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_reversal' kernel, each representing a distinct structural interpretation of the same historical event. This reading emphasizes endogenous reinterpretation via divine revelation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
