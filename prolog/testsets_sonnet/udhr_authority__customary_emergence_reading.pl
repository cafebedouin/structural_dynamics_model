% ============================================================================
% CONSTRAINT STORY: udhr_authority__customary_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__customary_emergence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: udhr_authority__customary_emergence_reading
 *   human_readable: UDHR as Emergent Customary International Law (State Practice + Opinio Juris)
 *   domain: international_law/political_philosophy/human_rights_doctrine
 *
 * SUMMARY:
 *   Adopted in 1948 as a General Assembly resolution with explicitly
 *   non-binding status, the UDHR's normative career did not stop at
 *   aspiration. Over the following decades, tribunals, international courts,
 *   and legal scholars began asserting that certain UDHR provisions — the
 *   prohibitions on torture, slavery, and genocide chief among them — had,
 *   through consistent state practice and expressions of legal obligation
 *   (opinio juris), crystallized into binding customary international law.
 *   This reading treats that claimed transition as the operative constraint:
 *   an evolving, moderately extractive authority structure that grants real
 *   coordination benefits (a mechanism for updating international obligations
 *   without renegotiating treaties) while also transferring interpretive
 *   power to the institutions that certify the transition and imposing costs
 *   on states and populations that never participated in the practice said to
 *   have generated it.
 *
 * KEY AGENTS:
 *   - international_law_scholars: agenda_setter/beneficiary (institutional/analytical) — certify the customary transition through scholarship and briefs
 *   - human_rights_tribunals: agenda_setter/beneficiary (institutional/arbitrage) — invoke customary status to ground jurisdiction
 *   - powerful_states_with_favorable_practice_records: beneficiary (institutional/arbitrage) — selectively wield customary claims against rivals
 *   - states_with_persistent_objector_status_denied: payer (moderate/constrained) — objections treated as insufficient once practice accumulates
 *   - newly_formed_states_bound_without_participation: payer (powerless/trapped) — bound by a practice record they had no part in
 *   - populations_in_states_selectively_cited_as_violators: payer (powerless/trapped) — bear material consequences of selective invocation
 *   - dissenting_legal_theorists: excluded (moderate/constrained) — contest the sufficiency of the practice record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__customary_emergence_reading, 0.42).
domain_priors:suppression_score(udhr_authority__customary_emergence_reading, 0.38).
domain_priors:theater_ratio(udhr_authority__customary_emergence_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__customary_emergence_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__customary_emergence_reading, "UDHR as Emergent Customary International Law (State Practice + Opinio Juris)").
narrative_ontology:topic_domain(udhr_authority__customary_emergence_reading, "international_law/political_philosophy/human_rights_doctrine").

domain_priors:requires_active_enforcement(udhr_authority__customary_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__customary_emergence_reading, 'ede37b79-4c56-4aac-a7e2-fc3258451416').
narrative_ontology:cs_kernel_codification('ede37b79-4c56-4aac-a7e2-fc3258451416', distributed).
narrative_ontology:cs_authority_grounding('ede37b79-4c56-4aac-a7e2-fc3258451416', practice).
narrative_ontology:cs_interpretation_layer_present('ede37b79-4c56-4aac-a7e2-fc3258451416').
narrative_ontology:cs_reading_relation('ede37b79-4c56-4aac-a7e2-fc3258451416', udhr_authority__binding_universalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ede37b79-4c56-4aac-a7e2-fc3258451416', udhr_authority__aspirational_sovereignty_reading, influences).
narrative_ontology:cs_axiom('ede37b79-4c56-4aac-a7e2-fc3258451416', foundational, authority_accrues_through_accumulated_practice_not_original_intent).
narrative_ontology:cs_axiom_status(authority_accrues_through_accumulated_practice_not_original_intent, holdable).
narrative_ontology:cs_axiom_grounding('ede37b79-4c56-4aac-a7e2-fc3258451416', authority_accrues_through_accumulated_practice_not_original_intent, conventional).
narrative_ontology:cs_axiom('ede37b79-4c56-4aac-a7e2-fc3258451416', secondary, opinio_juris_evidentiary_threshold_is_objectively_determinable).
narrative_ontology:cs_axiom_status(opinio_juris_evidentiary_threshold_is_objectively_determinable, holdable).
narrative_ontology:cs_axiom_grounding('ede37b79-4c56-4aac-a7e2-fc3258451416', opinio_juris_evidentiary_threshold_is_objectively_determinable, empirically_contingent).
narrative_ontology:cs_reference_frame('ede37b79-4c56-4aac-a7e2-fc3258451416', gradual_crystallization_via_state_practice).
narrative_ontology:cs_drift_state('ede37b79-4c56-4aac-a7e2-fc3258451416', post_cold_war_tribunal_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ede37b79-4c56-4aac-a7e2-fc3258451416', '').
narrative_ontology:cs_kernel_id(udhr_authority__customary_emergence_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, international_law_scholars).
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, human_rights_tribunals).
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, powerful_states_with_favorable_practice_records).
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, customary_law_doctrine_itself).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, states_with_persistent_objector_status_denied).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, newly_formed_states_bound_without_participation).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, populations_in_states_selectively_cited_as_violators).
narrative_ontology:constraint_vindicates(udhr_authority__customary_emergence_reading, customary_international_law_formation_doctrine).
narrative_ontology:constraint_vindicates(udhr_authority__customary_emergence_reading, opinio_juris_as_evidentiary_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compile state practice and opinio juris evidence, author treatises and tribunal briefs asserting which UDHR provisions have 'crystallized' into custom. Their interpretive labor is what performs the transition from aspiration to binding law; the ambiguity of the transition point is the raw material of their professional output and citation authority.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, international_law_scholars, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__customary_emergence_reading, international_law_scholars, beneficiary).

% Cite the UDHR as evidence of customary law to ground jurisdiction and judgments against states that never ratified a binding instrument covering the claim. Gain adjudicatory reach and legitimacy from an authority source that requires no fresh state consent to invoke.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, human_rights_tribunals, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__customary_emergence_reading, human_rights_tribunals, beneficiary).

% Have long, well-documented diplomatic and legal practice consistent with UDHR norms; they invoke customary status selectively to pressure rival states while their own gaps in compliance are less scrutinized due to their role in the very tribunals and scholarship that certify custom. Can exit or ignore inconvenient customary claims when it suits them (persistent objector doctrine, political leverage) in ways weaker states cannot.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, powerful_states_with_favorable_practice_records, beneficiary,
    institutional, civilizational, arbitrage, global).

% Explicitly objected to specific UDHR provisions being treated as binding, but find that once enough time and cited practice accumulate, their objection is treated as insufficient to prevent the norm attaching to them anyway. They bear reputational and diplomatic costs of noncompliance with a standard they never consented to as binding law.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, states_with_persistent_objector_status_denied, payer,
    moderate, generational, constrained, national).

% Came into existence after 1948 and after the alleged customary crystallization occurred; had no voice in the state practice that supposedly generated the custom, yet are held to the resulting standard as a condition of international recognition and participation in multilateral institutions.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, newly_formed_states_bound_without_participation, payer,
    powerless, generational, trapped, national).

% Live under governments singled out in customary-law-based condemnations while comparable violations elsewhere go uncited due to geopolitical alignment. The selective invocation of custom does not reliably translate into improved conditions for them, but does shape sanctions, aid conditionality, and international pressure that affects their material lives.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, populations_in_states_selectively_cited_as_violators, payer,
    powerless, biographical, trapped, national).

% The doctrinal apparatus of customary international law formation (state practice + opinio juris) gains a marquee validating case in the UDHR narrative, reinforcing the doctrine's own claimed capacity to generate binding law without treaty ratification.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, customary_law_doctrine_itself, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(udhr_authority__customary_emergence_reading, customary_law_doctrine_itself).

% Argue the customary-emergence account retrofits consent onto a process that was actually selective, politically motivated citation-building rather than genuine widespread and consistent practice plus a sense of legal obligation. Their critiques appear in academic literature but rarely shape tribunal reasoning or diplomatic practice.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, dissenting_legal_theorists, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_authority__customary_emergence_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_authority__customary_emergence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which a widely-endorsed but originally non-binding moral statement can acquire binding legal force through accumulated state conduct, allowing the international system to update its hard-law obligations without requiring universal treaty renegotiation.
% TRANSFER_FUNCTION: Moves interpretive and adjudicatory authority from state consent (treaty ratification) to accumulated practice as certified by scholars and tribunals; moves compliance costs onto states and populations who never consented to the specific provisions now treated as binding, while transferring legitimacy and jurisdictional reach to the certifying institutions.
% ABSENT_VOICES: Newly formed states and states with formally lodged objections had no seat at the moment the 'accumulation' of practice was said to occur, and their present-day objections are treated by tribunals as arriving too late to prevent attachment; dissenting legal theorists who contest the sufficiency of the practice record are cited in footnotes but rarely change tribunal outcomes.
% DISAPPEARANCE_RATIONALE: If the customary-emergence reading disappeared, tribunals would lose a major citation basis for holding non-ratifying states to UDHR-derived standards, and scholars would lose a significant doctrinal project; some argue actual state behavior (treaty accession, domestic incorporation) would fill the gap with little practical difference, while others argue whole categories of human rights litigation against non-consenting states would become untenable.
% FOUNDING_PROBLEM: The UDHR itself was adopted as a non-binding General Assembly resolution in 1948 explicitly because states would not accept binding human rights obligations by treaty at that time; the customary-emergence account was developed to explain how, decades later, courts and scholars could treat at least some UDHR provisions as binding without a new treaty-based consent event.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars and tribunals that rely on the customary-emergence account attest the transition occurred and is now settled doctrine for core provisions (prohibition on torture, genocide, slavery). Independent legal historians and states maintaining persistent-objector claims contest whether the underlying practice record meets the traditional 'widespread, representative, and consistent' threshold, arguing the doctrine functions more as retrospective legitimation than as an accurate description of an actual consensus-formation process; this dissent exists partly outside the institutions that benefit from the customary reading, but the tribunals and much of the academy that adjudicate the question are the same community asserting the transition occurred.
narrative_ontology:disappearance_verdict(udhr_authority__customary_emergence_reading, contested).
narrative_ontology:founding_problem_status(udhr_authority__customary_emergence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__customary_emergence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_authority__customary_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__customary_emergence_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__customary_emergence_reading_tests).
:- end_tests(udhr_authority__customary_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.10 in 1948, when the UDHR was uncontroversially non-binding) and rises steadily to 0.42 by 2025 as the customary-law claim hardens into settled doctrine for an expanding list of provisions, generating real interpretive rents for the institutions that certify the transition. Theater ratio also rises (0.20 to 0.46) because an increasing share of the 'evidence of custom' consists of citation to prior tribunal decisions and scholarly consensus rather than fresh, independently verifiable state practice — the doctrine increasingly cites itself. Suppression is moderate and rising (0.15 to 0.38): persistent-objector states and newly formed states experience real diplomatic and reputational pressure to conform, but this is softer and less coercive than a treaty-based enforcement regime, consistent with a tangled_rope rather than a snare. Accessibility collapse (0.40) and resistance (0.55) reflect that alternative framings (pure aspiration, or a stricter treaty-consent requirement) remain live and actively argued by dissenting theorists and objecting states — the customary reading has not fully foreclosed its competitors, which is itself part of what distinguishes this reading from the binding_universalism sibling.
 *
 * DIRECTIONALITY LOGIC:
 *   International law scholars and tribunals sit closest to the beneficiary end: they generate and certify the customary-law claim and gain professional and jurisdictional authority from it, with essentially analytical or arbitrage-grade exit from any consequence of being wrong. Powerful states with long, favorable practice records benefit doubly — their conduct supplies the 'evidence' and they retain enough leverage to invoke persistent-objector status themselves when a customary claim would bind them unfavorably, which the derivation captures via their institutional power and arbitrage exit. States that formally objected but are held bound anyway, and states that did not exist during the alleged practice-accumulation window, sit at the target end: they bear the costs of a standard whose formation they could not shape, with constrained-to-trapped exit. Populations in selectively-cited states are the most powerless payers, bearing downstream material consequences (sanctions, conditionality) from an invocation pattern shaped by geopolitics as much as by the underlying practice record itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_point_indeterminacy,
    'At what specific point, if any, did specific UDHR provisions actually cross from non-binding aspiration to binding custom, and is that point empirically locatable or an artifact of retrospective doctrinal narrative-building?',
    'A rigorous historical audit of the actual state practice record (diplomatic correspondence, domestic incorporation timing, actual compliance behavior independent of tribunal citation) against the traditional customary-law threshold of ''widespread, representative, and consistent'' practice plus genuine opinio juris, conducted by historians independent of the international law tribunals and scholarship that assert the transition.',
    'If the practice record does not meet the threshold at any locatable point, the customary_emergence_reading is itself a constructed retrospective legitimation device functioning closer to a snare (extraction dressed as gradual consensus) than a tangled_rope with genuine coordination value; if the record does meet the threshold, the reading''s coordination function is more clearly genuine and the extraction is a byproduct rather than the core mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_point_indeterminacy, empirical, 'Whether the claimed customary transition is empirically locatable or a doctrinal narrative construction.').

omega_variable(
    selective_invocation_pattern,
    'Is the customary-law status of UDHR provisions invoked and enforced consistently across all states, or disproportionately against geopolitically weaker or disfavored states while comparable conduct by powerful states is left uncited?',
    'Comparative analysis of tribunal citations and diplomatic condemnations invoking UDHR-derived customary norms, cross-referenced against comparable violations by states with greater geopolitical leverage over the citing institutions.',
    'A strongly asymmetric invocation pattern would support classifying the constraint''s operative effect as closer to tangled_rope-with-heavy-extraction or even snare for the disfavored-state subset, since the ''universal custom'' framing would function primarily as selective leverage rather than a genuinely universal legal update mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_invocation_pattern, empirical, 'Whether customary-law invocation is applied even-handedly or selectively along geopolitical lines.').

omega_variable(
    cs_framing_alternative,
    'Should this reading''s kernel be framed as the UDHR text itself (fixed_text), or as the doctrine of customary international law formation (distributed, practice-based) that happens to be applied to the UDHR as one instance among many customary claims?',
    'Compare classification outcomes under both framings: fixed_text+practice authority (UDHR-as-text gradually reinterpreted) versus distributed+practice authority (customary law doctrine as the kernel, UDHR as an applied case). Examine whether tribunal reasoning treats the UDHR''s specific text as authoritative or treats the general customary-law formation test as authoritative with UDHR as illustrative material.',
    'Under the fixed_text framing, drift would concentrate in interpretation of specific UDHR clauses (closer to codification_collapse risk). Under the distributed framing chosen here, drift concentrates in the evidentiary standard for opinio juris itself (practice_drift, as authored) — a different failure mode and a different set of institutions bearing responsibility for drift management.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative, conceptual, 'Alternative kernel framings (fixed-text vs. distributed-doctrine) that would route this reading''s drift differently.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__customary_emergence_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_authority__customary_emergence_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(udhr_tr_t1966, udhr_authority__customary_emergence_reading, theater_ratio, 1966, 0.28).
narrative_ontology:measurement(udhr_tr_t1980, udhr_authority__customary_emergence_reading, theater_ratio, 1980, 0.34).
narrative_ontology:measurement(udhr_tr_t1995, udhr_authority__customary_emergence_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(udhr_tr_t2010, udhr_authority__customary_emergence_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(udhr_tr_t2025, udhr_authority__customary_emergence_reading, theater_ratio, 2025, 0.46).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_authority__customary_emergence_reading, base_extractiveness, 1948, 0.1).
narrative_ontology:measurement(udhr_be_t1966, udhr_authority__customary_emergence_reading, base_extractiveness, 1966, 0.18).
narrative_ontology:measurement(udhr_be_t1980, udhr_authority__customary_emergence_reading, base_extractiveness, 1980, 0.26).
narrative_ontology:measurement(udhr_be_t1995, udhr_authority__customary_emergence_reading, base_extractiveness, 1995, 0.33).
narrative_ontology:measurement(udhr_be_t2010, udhr_authority__customary_emergence_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(udhr_be_t2025, udhr_authority__customary_emergence_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_authority__customary_emergence_reading, suppression_requirement, 1948, 0.15).
narrative_ontology:measurement(udhr_su_t1966, udhr_authority__customary_emergence_reading, suppression_requirement, 1966, 0.22).
narrative_ontology:measurement(udhr_su_t1980, udhr_authority__customary_emergence_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement(udhr_su_t1995, udhr_authority__customary_emergence_reading, suppression_requirement, 1995, 0.32).
narrative_ontology:measurement(udhr_su_t2010, udhr_authority__customary_emergence_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(udhr_su_t2025, udhr_authority__customary_emergence_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__customary_emergence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_authority__customary_emergence_reading, 0.12).
narrative_ontology:affects_constraint(udhr_authority__customary_emergence_reading, binding_universalism_reading).
narrative_ontology:affects_constraint(udhr_authority__customary_emergence_reading, aspirational_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the udhr_authority kernel, decomposed per the ε-invariance principle: binding_universalism_reading claims justiciable individual rights independent of state consent (highest and most stable ε, contested but not gradually rising); aspirational_sovereignty_reading claims no binding force attaches absent fresh treaty consent (lowest ε, near-mountain in its own framework); customary_emergence_reading (this story) claims a gradual, ambiguous transition with moderate and rising ε. The three are not the same constraint measured three ways — each has a distinct victim/beneficiary structure and a distinct claimed mechanism of authority. They are linked here because state practice and tribunal decisions asserting the customary_emergence_reading create structural pressure that can shift resource availability and legitimacy conditions for the binding_universalism_reading (successful customary claims can be cited as stepping stones toward stronger universalist claims) and can erode the practical force of the aspirational_sovereignty_reading (each successful customary-law finding narrows the space in which pure non-binding aspiration can be asserted for a given provision).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
