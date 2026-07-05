% ============================================================================
% CONSTRAINT STORY: udhr_authority__binding_universalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: udhr_authority__binding_universalism_reading
 *   human_readable: UDHR as Justiciable Individual Rights Regime Binding States Absent Consent
 *   domain: international_law/political_philosophy/human_rights_doctrine
 *
 * SUMMARY:
 *   This story generates ONE reading of the udhr_authority kernel: the
 *   binding_universalism_reading, under which the UDHR (and the
 *   covenant/tribunal architecture built on it) establishes justiciable
 *   individual rights enforceable against states regardless of that state's
 *   consent. This is a substantively different constraint from the
 *   aspirational_sovereignty_reading (moral guidance requiring consent) and
 *   the customary_emergence_reading (gradual crystallization into custom via
 *   state practice) — under binding_universalism the tribunal's coercive
 *   authority is treated as already established at the level of the
 *   individual right, not contingent on either express consent or accumulated
 *   practice. The extractiveness trajectory (0.15 in 1948 rising to 0.68 by
 *   2024) reflects the accumulation of enforcement infrastructure — regional
 *   human rights courts, UN treaty body jurisprudence, universal jurisdiction
 *   doctrines — that operationalized the binding claim well beyond what the
 *   1948 declaration's own drafters intended it to carry.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, 0.68).
domain_priors:suppression_score(udhr_authority__binding_universalism_reading, 0.58).
domain_priors:theater_ratio(udhr_authority__binding_universalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__binding_universalism_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__binding_universalism_reading, "UDHR as Justiciable Individual Rights Regime Binding States Absent Consent").
narrative_ontology:topic_domain(udhr_authority__binding_universalism_reading, "international_law/political_philosophy/human_rights_doctrine").

domain_priors:requires_active_enforcement(udhr_authority__binding_universalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__binding_universalism_reading, 'f2f5be41-8299-4715-a6f3-2cd866ecd7ee').
narrative_ontology:cs_kernel_codification('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', fixed_text).
narrative_ontology:cs_authority_grounding('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', extraction).
narrative_ontology:cs_interpretation_layer_present('f2f5be41-8299-4715-a6f3-2cd866ecd7ee').
narrative_ontology:cs_reading_relation('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', udhr_authority__aspirational_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', foundational, individual_rights_bind_states_independent_of_consent).
narrative_ontology:cs_axiom_status(individual_rights_bind_states_independent_of_consent, holdable).
narrative_ontology:cs_axiom_grounding('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', individual_rights_bind_states_independent_of_consent, deontological).
narrative_ontology:cs_axiom('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', secondary, tribunal_jurisdiction_derives_directly_from_rights_content).
narrative_ontology:cs_axiom_status(tribunal_jurisdiction_derives_directly_from_rights_content, holdable).
narrative_ontology:cs_axiom_grounding('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', tribunal_jurisdiction_derives_directly_from_rights_content, conventional).
narrative_ontology:cs_reference_frame('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', binding_law_from_adoption).
narrative_ontology:cs_drift_state('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', post_cold_war_tribunal_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f2f5be41-8299-4715-a6f3-2cd866ecd7ee', '').
narrative_ontology:cs_kernel_id(udhr_authority__binding_universalism_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, individual_rights_claimants).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, international_human_rights_tribunals).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, transnational_advocacy_networks).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, non_ratifying_states).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, states_with_divergent_constitutional_orders).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, domestic_democratic_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals harmed by state action who invoke UDHR-derived norms before tribunals or treaty bodies to obtain a remedy their domestic legal order denies them. Under this reading their claim does not depend on their state's consent to be bound; the tribunal's authority to hear and rule against the state is treated as already established.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, individual_rights_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Bodies (regional courts, UN treaty committees, ad hoc tribunals) that adjudicate claims against states by treating UDHR-derived rights as binding law rather than aspirational text. They issue findings and, in some regimes, judgments states are formally obligated to comply with, and they administer the doctrine that grounds their own jurisdiction over non-consenting states.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, international_human_rights_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% NGOs, litigation networks, and epistemic communities that bring cases, shape tribunal jurisprudence, and gain standing and resources from a regime in which UDHR rights are litigable against states. They benefit from the binding-universalism reading being institutionally entrenched because it is the reading that gives their casework coercive teeth.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, transnational_advocacy_networks, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__binding_universalism_reading, transnational_advocacy_networks, agenda_setter).

% States that never ratified the specific covenants operationalizing UDHR rights, or that ratified with reservations, but find tribunals and diplomatic pressure treating the underlying norms as binding on them regardless. Their formal non-consent does not function as an exit; withdrawal from the normative regime carries reputational and material costs disproportionate to the consent they withheld.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, non_ratifying_states, payer,
    institutional, generational, trapped, national).

% States whose constitutional traditions balance the enumerated rights differently (e.g., different weightings of speech, religion, property, collective versus individual rights) are told their domestic balance is subordinate to the tribunal's reading of universal rights. They can resist through diplomatic friction or selective non-compliance but bear costs — sanctions exposure, aid conditionality, reputational sanction — for maintaining their own constitutional balance.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, states_with_divergent_constitutional_orders, payer,
    institutional, generational, constrained, national).

% Voting publics whose enacted domestic policy (on migration, punishment, family law, property, or speech regulation) is overridden or delegitimized by tribunal rulings grounded in UDHR rights their state never separately consented to as binding. They experience the loss of a policy outcome they voted for, with limited recourse other than exiting international institutions altogether.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, domestic_democratic_majorities, payer,
    moderate, biographical, constrained, national).

% Scholars and diplomats who hold that international obligation requires state consent and that the UDHR was drafted and voted precisely to avoid binding legal force. Their view structures the aspirational_sovereignty_reading (a sibling constraint) but has limited voice within tribunal proceedings that already presuppose the binding-universalism premise as their jurisdictional foundation.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, consent_based_sovereignty_theorists, excluded,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable normative reference point so that individuals harmed by state conduct have a forum and a standard of judgment that does not depend on the harming state's own legal system or willingness to be judged — coordinating cross-border expectations about minimum treatment.
% TRANSFER_FUNCTION: Moves adjudicatory authority over a state's treatment of its own population from that state's domestic institutions to international tribunals and treaty bodies, and moves practical leverage from domestic democratic majorities and non-consenting states to rights claimants and the advocacy/tribunal apparatus that represents them.
% ABSENT_VOICES: Consent-based sovereignty theorists and diplomats who negotiated the UDHR explicitly as non-binding are structurally absent from tribunal proceedings, whose jurisdiction already presupposes the binding premise; their objection is heard in scholarly and diplomatic fora but does not reach the adjudicative record as a live jurisdictional challenge.
% DISAPPEARANCE_RATIONALE: If the binding-universalism reading were abandoned overnight, existing tribunal judgments against states would lose their claimed jurisdictional basis, ongoing litigation invoking UDHR rights directly (rather than through separately ratified and consented treaties) would collapse, and advocacy networks would have to re-ground claims entirely in customary law or specific consented instruments — a substantial reorganization of the international human rights litigation architecture.
% FOUNDING_PROBLEM: The 1948 drafters faced states unwilling to accept binding legal obligations on how they treat their own populations, in the immediate aftermath of atrocities that domestic legal orders had failed to prevent or had actively authorized.
% FOUNDING_PROBLEM_CORROBORATION: The travaux préparatoires and the drafting committee's own contemporaneous statements (notably Eleanor Roosevelt's remarks to the UN General Assembly) attest that the UDHR was adopted as a declaration, explicitly not a treaty, precisely because binding legal force was rejected by the negotiating states — this is corroboration from the founding parties themselves that the founding problem was NOT solved via binding law but was left as aspiration. Contemporary tribunals and advocacy networks (the beneficiaries of this reading) assert the problem has since become live as binding obligation through subsequent practice; that assertion is self-interested and is precisely what the customary_emergence_reading and this reading contest.
narrative_ontology:disappearance_verdict(udhr_authority__binding_universalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_authority__binding_universalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__binding_universalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.68) because the reading's structural claim — that tribunals may bind states without their consent — directly transfers adjudicatory sovereignty from states (and their domestic democratic processes) to international bodies and the claimants/advocacy networks who use them. Suppression (0.58) is substantial but not maximal: states retain exit options (withdrawal from optional protocols, non-compliance with non-binding findings, denunciation of specific treaties) even though those exits carry high reputational cost. Theater ratio starts high (0.6 in 1948, when the declaration had essentially no enforcement machinery behind its rhetorical claims) and falls as genuine adjudicative capacity was built, then ticks back up slightly (0.42 by 2024) as compliance mechanisms outpace actual enforcement capacity in many venues. Accessibility collapse is moderate (0.4) — states and theorists retain live doctrinal alternatives (the sibling readings) that have not been extinguished, so collapse is far from mountain-level.
 *
 * PERSPECTIVAL GAP:
 *   From the tribunal and claimant seats, this reading is coordination: a stable, universal floor of treatment that does not depend on any single state's willingness to be bound, closing exactly the gap that let atrocities go unremedied by domestic law. From the non-ratifying and constitutionally-divergent state seats, the identical structure is extraction: a claim of jurisdiction that was never actually consented to, backed by the same enforcement and reputational machinery, functioning as an assertion of authority independent of the normal international-law requirement of state consent. The engine should register these as different computed types from the same structural data — that divergence is the object of study, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual claimants and the tribunal/advocacy apparatus sit at the beneficiary end: the binding reading is precisely what gives their claims and their institutional mandate coercive force. States — whether formally non-ratifying or ratifying-but-constitutionally-divergent — sit at the target end: the reading extracts adjudicatory authority from them regardless of what they consented to, and their nominal 'trapped' or 'constrained' exit options reflect that formal withdrawal does not neutralize the normative and reputational pressure the regime exerts. Domestic democratic majorities are targets at one remove: their enacted preferences can be overridden by a tribunal finding grounded in a premise (binding without consent) their state never separately endorsed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (atrocity-enabling domestic legal orders in 1948) is contested as live: proponents of this reading argue new and ongoing state atrocities show the problem persists and the binding mechanism is still necessary; critics note that the specific mechanism chosen in 1948 — a non-binding declaration — was deliberately NOT the binding-tribunal mechanism this reading now claims. This is exactly the kind of divergence the R5 genealogy interview is built to surface: the founding parties' own contemporaneous corroboration (Roosevelt's UNGA remarks, the travaux préparatoires) supports the aspirational reading, not this one, which weakens (but does not resolve) the binding-universalism reading's genealogical claim to be executing rather than exceeding the original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_independence_of_tribunal_jurisdiction,
    'Does a tribunal''s authority to bind a state to UDHR-derived norms genuinely not depend on that state''s consent, or is the consent simply relocated (to treaty ratification, to UN Charter membership, to acquiescence over time) rather than eliminated?',
    'Doctrinal analysis of specific tribunal jurisdictional holdings: do they cite consent-substitutes (UN membership, jus cogens status, erga omnes obligations) or do they claim jurisdiction directly from the UDHR text without any consent-proxy? Comparative analysis across regional human rights court jurisprudence would resolve whether the ''regardless of consent'' claim is doctrinally load-bearing or rhetorical.',
    'If tribunals in practice always locate some consent-proxy, this reading collapses toward customary_emergence_reading; if genuine non-consent-based jurisdiction is asserted and exercised, this reading is structurally distinct and the extractiveness measure is warranted at its authored level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_independence_of_tribunal_jurisdiction, conceptual, 'Whether binding jurisdiction is truly consent-independent or consent-relocated.').

omega_variable(
    kernel_reading_selection_evidence,
    'What in the historical and doctrinal record would distinguish which of the three kernel readings (aspirational, binding-universalist, customary-emergent) is the operative one at any given moment, given that all three are asserted by different actors about the same text?',
    'Track which reading actually governs outcomes in specific compliance disputes over time — does state non-compliance with a tribunal finding face consequences consistent with the binding reading (legal sanction) or consequences consistent with the aspirational reading (reputational cost only)?',
    'If enforcement outcomes consistently pattern as reputational-only even where tribunals assert binding jurisdiction, the binding_universalism_reading''s high extractiveness claim is descriptively overstated relative to actual practice, and the customary_emergence_reading would better fit the observed pattern.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'How to empirically distinguish which kernel reading actually governs practice.').

omega_variable(
    false_universalism_beneficiary_capture,
    'Is the binding-universalism reading a genuine philosophical/legal position, or is it substantially shaped by the institutional interests of the tribunals and advocacy networks that gain jurisdiction and resources under it?',
    'Compare the professional and institutional composition of scholars/jurists advancing each reading against their institutional affiliations and funding sources; assess whether the binding reading gained doctrinal traction in step with tribunal caseload growth or independent of it.',
    'If the reading''s dominance correlates strongly with the institutional growth of its beneficiaries rather than independent doctrinal development, this weakens the reading''s claim to be a neutral interpretation and supports treating it as partly self-interested institutional construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_universalism_beneficiary_capture, empirical, 'Whether beneficiary institutions have shaped the reading''s doctrinal ascendance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__binding_universalism_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_authority__binding_universalism_reading, theater_ratio, 1948, 0.6).
narrative_ontology:measurement(udhr_tr_t1966, udhr_authority__binding_universalism_reading, theater_ratio, 1966, 0.5).
narrative_ontology:measurement(udhr_tr_t1984, udhr_authority__binding_universalism_reading, theater_ratio, 1984, 0.45).
narrative_ontology:measurement(udhr_tr_t1998, udhr_authority__binding_universalism_reading, theater_ratio, 1998, 0.4).
narrative_ontology:measurement(udhr_tr_t2010, udhr_authority__binding_universalism_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(udhr_tr_t2024, udhr_authority__binding_universalism_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_authority__binding_universalism_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(udhr_be_t1966, udhr_authority__binding_universalism_reading, base_extractiveness, 1966, 0.28).
narrative_ontology:measurement(udhr_be_t1984, udhr_authority__binding_universalism_reading, base_extractiveness, 1984, 0.42).
narrative_ontology:measurement(udhr_be_t1998, udhr_authority__binding_universalism_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(udhr_be_t2010, udhr_authority__binding_universalism_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(udhr_be_t2024, udhr_authority__binding_universalism_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_authority__binding_universalism_reading, suppression_requirement, 1948, 0.1).
narrative_ontology:measurement(udhr_su_t1966, udhr_authority__binding_universalism_reading, suppression_requirement, 1966, 0.22).
narrative_ontology:measurement(udhr_su_t1984, udhr_authority__binding_universalism_reading, suppression_requirement, 1984, 0.35).
narrative_ontology:measurement(udhr_su_t1998, udhr_authority__binding_universalism_reading, suppression_requirement, 1998, 0.47).
narrative_ontology:measurement(udhr_su_t2010, udhr_authority__binding_universalism_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(udhr_su_t2024, udhr_authority__binding_universalism_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, aspirational_sovereignty_reading).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, customary_emergence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the udhr_authority kernel, decomposed per the ε-invariance principle: aspirational_sovereignty_reading (low extraction, consent-required, near-rope), binding_universalism_reading (this story — high extraction, consent-independent tribunal authority, tangled_rope), and customary_emergence_reading (moderate, time-dependent extraction rising as practice accumulates, likely scaffold-to-tangled-rope drift). Each carries its own ε and stakeholder structure; they are linked here rather than merged because measuring the UDHR's authority under different jurisdictional theories yields materially different extraction values — exactly the case the ε-invariance test is designed to catch.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
