% ============================================================================
% CONSTRAINT STORY: udhr_authority__binding_universalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__binding_universalism_reading, []).

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
 *   constraint_id: udhr_authority__binding_universalism_reading
 *   human_readable: UDHR as Justiciable Universal Rights Regime Binding States Without Consent
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   Under the binding_universalism reading, the 1948 Universal Declaration is
 *   treated by international tribunals, treaty bodies, and advocacy networks
 *   as establishing rights that individuals may invoke against any state,
 *   independent of whether that state ratified a specific binding instrument
 *   or consented to a specific tribunal's jurisdiction. This reading grounds
 *   erga omnes and jus cogens doctrines used in contemporary human rights
 *   litigation. It is claimed here as a tangled_rope: it performs genuine
 *   coordination (a shared normative floor enabling cross-system claims)
 *   while also functioning as an enforcement structure that extracts
 *   sovereign discretion from non-consenting and structurally weaker states,
 *   asymmetrically applied against states lacking great-power leverage.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, 0.68).
domain_priors:suppression_score(udhr_authority__binding_universalism_reading, 0.55).
domain_priors:theater_ratio(udhr_authority__binding_universalism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__binding_universalism_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__binding_universalism_reading, "UDHR as Justiciable Universal Rights Regime Binding States Without Consent").
narrative_ontology:topic_domain(udhr_authority__binding_universalism_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(udhr_authority__binding_universalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__binding_universalism_reading, '2bdac012-2fa8-4542-ac8f-f02ba9756c0d').
narrative_ontology:cs_kernel_codification('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', fixed_text).
narrative_ontology:cs_authority_grounding('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', extraction).
narrative_ontology:cs_interpretation_layer_present('2bdac012-2fa8-4542-ac8f-f02ba9756c0d').
narrative_ontology:cs_reading_relation('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', udhr_authority__aspirational_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', foundational, individual_rights_bind_states_independent_of_consent).
narrative_ontology:cs_axiom_status(individual_rights_bind_states_independent_of_consent, holdable).
narrative_ontology:cs_axiom_grounding('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', individual_rights_bind_states_independent_of_consent, deontological).
narrative_ontology:cs_axiom('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', secondary, peremptory_norms_override_sovereign_non_ratification).
narrative_ontology:cs_axiom_status(peremptory_norms_override_sovereign_non_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', peremptory_norms_override_sovereign_non_ratification, conventional).
narrative_ontology:cs_reference_frame('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', postwar_non_binding_declaration_consensus).
narrative_ontology:cs_drift_state('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', contemporary_tribunal_jurisprudence_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('2bdac012-2fa8-4542-ac8f-f02ba9756c0d', '').
narrative_ontology:cs_kernel_id(udhr_authority__binding_universalism_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, individual_rights_claimants).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, international_human_rights_tribunals).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, human_rights_advocacy_organizations).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, non_consenting_member_states).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, states_with_divergent_constitutional_orders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, great_powers_with_selective_compliance).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, smaller_states_without_veto_leverage).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, great_powers_with_selective_compliance).
narrative_ontology:constraint_vindicates(udhr_authority__binding_universalism_reading, universal_moral_personhood_doctrine).
narrative_ontology:constraint_vindicates(udhr_authority__binding_universalism_reading, jus_cogens_supremacy_over_sovereign_consent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals harmed by state action (detention, expression suppression, discrimination) who, under this reading, can invoke UDHR-derived norms before international tribunals or treaty bodies regardless of whether their state consented to be bound by a specific instrument. They benefit directly when a forum accepts jurisdiction and issues a finding against their state.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, individual_rights_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Bodies (regional human rights courts, UN treaty committees, ad hoc tribunals) that adjudicate claims by reading UDHR provisions as reflecting binding customary or peremptory norms. They set the interpretive agenda for what counts as a justiciable right, extend jurisdiction over non-consenting states through doctrines like erga omnes obligations, and enforce compliance through naming, sanctions recommendations, or coordination with other bodies.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, international_human_rights_tribunals, agenda_setter,
    institutional, generational, arbitrage, global).

% NGOs and transnational advocacy networks that litigate strategically, build the jurisprudential record supporting binding-universalism readings, and gain institutional standing, funding, and moral authority from the constraint's existence. They can shift venues and forums; they are not bound by the constraint themselves.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, human_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__binding_universalism_reading, human_rights_advocacy_organizations, agenda_setter).

% States that signed the UDHR as a non-binding declaration in 1948 (or acceded to the UN without treaty-specific commitments) but now face tribunals asserting jurisdiction over their domestic conduct on the theory that UDHR norms bind independently of subsequent consent. Withdrawal from the UN or from regional bodies carries severe diplomatic and economic costs, so exit is nominally available but practically foreclosed.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, non_consenting_member_states, payer,
    institutional, generational, constrained, national).

% States whose constitutional traditions (religious law, single-party governance, distinct conceptions of collective versus individual rights) conflict with specific UDHR provisions as interpreted by tribunals. They bear reputational costs, aid conditionality, and diplomatic pressure when tribunals or treaty bodies find violations, even absent ratification of the specific enforcement mechanism.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, states_with_divergent_constitutional_orders, payer,
    powerful, generational, constrained, national).

% States lacking the geopolitical weight of great powers face binding-universalism enforcement (sanctions, aid conditionality, tribunal findings) with far less capacity to resist or negotiate exceptions than powerful states, which can often absorb findings without consequence. The same doctrine applies asymmetrically in practice.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, smaller_states_without_veto_leverage, payer,
    powerless, biographical, trapped, national).

% Major powers invoke binding-universalism to justify intervention or sanction against rivals while resisting the same doctrine's application to their own conduct, using veto power, non-ratification of optional protocols, or selective withdrawal from jurisdiction to blunt enforcement against themselves. They benefit from the doctrine's coercive reach when wielded against others.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, great_powers_with_selective_compliance, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__binding_universalism_reading, great_powers_with_selective_compliance, beneficiary).

% Scholars who study whether the 1948 General Assembly resolution's drafting history, voting record, and subsequent state practice actually support a claim of automatic binding force absent consent, or whether tribunals have constructed this authority beyond what the text or its adoption process warrants.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, comparative_international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_authority__binding_universalism_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_authority__binding_universalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, agreed-upon baseline vocabulary and normative floor for human treatment that lets disparate legal systems, tribunals, and advocacy movements coordinate claims and responses without renegotiating first principles in every case.
% TRANSFER_FUNCTION: Moves adjudicative authority and reputational/diplomatic leverage from sovereign states (particularly those with divergent domestic orders or weak geopolitical standing) to international tribunals, treaty bodies, and the advocacy networks that litigate before them, on the strength of a 1948 declaration many signatories understood as non-binding.
% ABSENT_VOICES: The 1948 drafting states that explicitly rejected binding legal force for the Declaration (as distinct from the later covenants) are not present to contest the subsequent reinterpretation; contemporary non-Western legal traditions object that the 'universal' framing encodes a particular liberal-individualist philosophy but are underrepresented in the tribunals that adjudicate compliance.
% DISAPPEARANCE_RATIONALE: Beneficiary seats (tribunals, advocacy organizations, claimants) hold that if binding force vanished, decades of jurisprudence protecting individuals would collapse and states would revert to unchecked domestic abuse — the world rearranges toward impunity. Payer seats (non-consenting states) hold that without the binding-universalism doctrine, states would simply return to the negotiated-treaty basis of obligation they always understood themselves to have accepted, and the practical human rights protections delivered through actual ratified treaties would persist unchanged — the world stays much the same. The disagreement is the kernel contest itself.
% FOUNDING_PROBLEM: In the aftermath of the Second World War, the founding problem was the absence of any international normative baseline condemning atrocities committed by a state against its own population, and the drafters' judgment that no enforceable treaty could be negotiated quickly enough or with sufficient state buy-in.
% FOUNDING_PROBLEM_CORROBORATION: The UDHR's own drafters (per Eleanor Roosevelt's contemporaneous statements and the travaux préparatoires) attest the instrument was deliberately non-binding, a corroboration from within the founding process itself rather than from a beneficiary of later reinterpretation. Independent international law historians outside the tribunal system and outside advocacy networks corroborate that the drafting record shows explicit rejection of binding force, while tribunal jurisprudence and advocacy scholarship — themselves beneficiaries of the binding reading — assert the norm has since become independently binding through custom; this status question is precisely the subject of the kernel's sibling readings.
narrative_ontology:disappearance_verdict(udhr_authority__binding_universalism_reading, contested).
narrative_ontology:founding_problem_status(udhr_authority__binding_universalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__binding_universalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_authority__binding_universalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__binding_universalism_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__binding_universalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_authority__binding_universalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.15 to 0.68) tracking the doctrinal growth of erga omnes/jus cogens jurisprudence, the proliferation of treaty-monitoring bodies, and increased willingness of regional courts to assert binding force independent of ratification. Suppression is moderate-high (0.55) because exit for smaller states is nominally available (leave the UN, refuse tribunal jurisdiction) but practically foreclosed by diplomatic and economic consequences; suppression is not scaled by scope or power in this authoring — it is the raw structural coercive capacity of the enforcement apparatus. Theater ratio (0.4) reflects that a substantial share of tribunal and advocacy activity is genuine adjudication of real grievances, but a growing share is performative — findings issued against states with no realistic compliance mechanism, serving reputational rather than remedial function.
 *
 * PERSPECTIVAL GAP:
 *   From the tribunal and advocacy seats, this is a rope: a hard-won universal floor protecting the powerless from state abuse. From the non-consenting state seat, this is a tangled_rope shading toward snare: consent-based international law has been reinterpreted after the fact to bind parties who explicitly negotiated non-binding language. The engine should compute divergent per-seat types from these structural positions; the claimed_type here (tangled_rope) states what I believe is structurally true given both a real coordination function and demonstrable asymmetric extraction on weaker/dissenting states — it does not resolve the kernel contest, which belongs to the sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual claimants and advocacy organizations sit near the beneficiary end: they gain standing, remedies, and institutional leverage from a doctrine that did not exist in this form in 1948. Tribunals are structural agenda-setters with arbitrage-grade exit (they are not bound by the doctrine they administer). Non-consenting and constitutionally divergent states are targets: high d, bearing sovereignty costs and reputational sanction for conduct they never agreed a specific body could adjudicate. Great powers occupy a dual position — they invoke the doctrine coercively against rivals (beneficiary-like) while insulating themselves from its application via veto power and selective ratification (target-adjacent but practically exempt), which is why their exit option is coded arbitrage rather than constrained despite formally being a payer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no normative floor after WWII atrocities) is genuinely dead in the sense that a normative floor now exists and is widely cited; whether the SPECIFIC mechanism of non-consensual bindingness remains necessary to preserve that floor, or whether it has become a vehicle for tribunal and advocacy-network authority beyond what the founding instrument's own drafters intended, is exactly the contested founding_problem_status recorded above. This is not mandatrophy in the classic form (function fully dead, form persists) — it is closer to functional overreach, where a real function persists but has been amplified into a coercive authority structure not present at founding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_versus_custom_bindingness,
    'Does the UDHR bind states independent of consent because it always did (natural/inherent rights reading) or because sufficient state practice and opinio juris subsequently crystallized it into binding custom — and if the latter, is ''binding_universalism'' actually a claim that collapses into ''customary_emergence'' once its historical basis is examined?',
    'Systematic review of ICJ jurisprudence, state objections and reservations since 1948, and the density/consistency of state practice invoking UDHR provisions as binding versus aspirational, would distinguish an inherent-bindingness claim from a customary-crystallization claim.',
    'If the doctrine''s real basis is customary crystallization rather than inherent bindingness, this story''s premise (bindingness regardless of consent, ab initio) is not structurally distinct from the customary_emergence_reading and the two constraints should be merged or the difference sharpened to timing-of-bindingness only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_versus_custom_bindingness, conceptual, 'Whether binding_universalism is a genuinely distinct premise from customary_emergence or a variant of it.').

omega_variable(
    tribunal_authority_beyond_drafting_intent,
    'Did the 1948 General Assembly resolution''s drafters and voting states intend or foresee the degree of coercive tribunal authority this reading now attributes to the instrument?',
    'Historical analysis of the travaux préparatoires, contemporaneous statements by delegations (particularly those from the Soviet bloc, Saudi Arabia, and South Africa who abstained or objected), and comparison to the explicit non-binding language used in the resolution''s adoption.',
    'If drafting intent clearly rejected binding force, this reading''s authority claim rests entirely on subsequent institutional construction by tribunals and advocacy networks rather than on the kernel text itself — strengthening the case that this reading is a constructed extraction dressed as inherent right, which would push classification toward snare rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tribunal_authority_beyond_drafting_intent, empirical, 'Divergence between 1948 drafting intent and the coercive authority this reading claims.').

omega_variable(
    asymmetric_enforcement_across_power,
    'Is the binding-universalism doctrine applied consistently across states regardless of power, or does its coercive force fall disproportionately on states lacking veto power, alliance protection, or economic leverage?',
    'Comparative dataset of tribunal findings, sanctions, and compliance outcomes cross-referenced against state GDP, UN Security Council membership, and alliance structures.',
    'Confirmed asymmetry would support the tangled_rope classification''s victim declarations (non_consenting_member_states, smaller states) as structurally accurate rather than incidental, and would sharpen the distinction between the doctrine''s stated universal scope and its actual selective application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_enforcement_across_power, empirical, 'Whether enforcement asymmetry by state power undermines the doctrine''s universalist self-description.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__binding_universalism_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_authority__binding_universalism_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(udhr_tr_t1966, udhr_authority__binding_universalism_reading, theater_ratio, 1966, 0.18).
narrative_ontology:measurement(udhr_tr_t1980, udhr_authority__binding_universalism_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(udhr_tr_t1998, udhr_authority__binding_universalism_reading, theater_ratio, 1998, 0.32).
narrative_ontology:measurement(udhr_tr_t2010, udhr_authority__binding_universalism_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(udhr_tr_t2024, udhr_authority__binding_universalism_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_authority__binding_universalism_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(udhr_be_t1966, udhr_authority__binding_universalism_reading, base_extractiveness, 1966, 0.3).
narrative_ontology:measurement(udhr_be_t1980, udhr_authority__binding_universalism_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(udhr_be_t1998, udhr_authority__binding_universalism_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(udhr_be_t2010, udhr_authority__binding_universalism_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(udhr_be_t2024, udhr_authority__binding_universalism_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_authority__binding_universalism_reading, suppression_requirement, 1948, 0.1).
narrative_ontology:measurement(udhr_su_t1966, udhr_authority__binding_universalism_reading, suppression_requirement, 1966, 0.2).
narrative_ontology:measurement(udhr_su_t1980, udhr_authority__binding_universalism_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(udhr_su_t1998, udhr_authority__binding_universalism_reading, suppression_requirement, 1998, 0.4).
narrative_ontology:measurement(udhr_su_t2010, udhr_authority__binding_universalism_reading, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(udhr_su_t2024, udhr_authority__binding_universalism_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__binding_universalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, aspirational_sovereignty_reading).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, customary_emergence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the udhr_authority kernel, decomposed per the ε-invariance principle: aspirational_sovereignty_reading (rope-leaning, low extraction, consent-preserving), binding_universalism_reading (this story, tangled_rope, substantial extraction from non-consenting states via tribunal enforcement), and customary_emergence_reading (a gradualist middle account). Each carries its own ε and classification; they are linked here rather than merged because measuring 'the UDHR's authority' by different observables (drafting text vs. tribunal practice vs. accreted custom) yields genuinely different extraction profiles — exactly the case the ε-invariance test flags for decomposition rather than averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
