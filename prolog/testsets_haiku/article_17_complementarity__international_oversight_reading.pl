% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: Article 17 Complementarity (International Oversight Reading)
 *   domain: international_law/criminal_justice
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes complementarity: the ICC can
 *   only intervene when a case is inadmissible before national courts. The
 *   kernel 'Article 17 Complementarity' is contested: one reading
 *   (international_oversight_reading, THIS CONSTRAINT) interprets 'unwilling
 *   or unable' broadly to capture independence failures, sham prosecution,
 *   and elite immunity, making admissibility thresholds LOW and ICC override
 *   frequent. The sibling reading (national_primacy_reading, separate
 *   constraint file) interprets complementarity as presuming national
 *   adequacy unless proven demonstrably sham, keeping ICC jurisdiction narrow
 *   and sovereignty protected. These are not two perspectives on the same
 *   constraint—they instantiate structurally different constraints with
 *   different ε values, different beneficiary/victim structures, and
 *   different extracted transfer flows. This file generates ONLY the
 *   international_oversight_reading.
 *
 * KEY AGENTS:
 *   - International Criminal Court: institutional agenda-setter, determines complementarity thresholds, monopolizes admissibility interpretation
 *   - Victims in complicit/failed states: powerless beneficiaries, dependent on ICC override for any accountability path
 *   - Transitional justice advocates: organized beneficiaries, vindicate universal accountability doctrine
 *   - National sovereigns subject to scrutiny: powerful payers, bear cost of external judicial review and cooperation demands
 *   - Elites in failed states: powerful payers, face exposure to prosecution in uncontrollable forums
 *   - National courts in complicit states: identity-locked payers, subjected to external competence judgment without voice
 *   - Liberal international order architects: institutional beneficiaries, vindicate proposition that accountability transcends sovereignty
 *   - State coalitions avoiding ICC accountability: organized payers, constrained to defection or compliance
 *   - Victor states framing selective prosecution: excluded, maintain diplomatic insulation while escaping scrutiny
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.58).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.62).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity (International Oversight Reading)").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '574ecfc5-bcd6-467b-8926-bb9bb53e01f1').
narrative_ontology:cs_kernel_codification('574ecfc5-bcd6-467b-8926-bb9bb53e01f1', fixed_text).
narrative_ontology:cs_authority_grounding('574ecfc5-bcd6-467b-8926-bb9bb53e01f1', lineage).
narrative_ontology:cs_interpretation_layer_present('574ecfc5-bcd6-467b-8926-bb9bb53e01f1').
narrative_ontology:cs_reading_relation('574ecfc5-bcd6-467b-8926-bb9bb53e01f1', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('574ecfc5-bcd6-467b-8926-bb9bb53e01f1', foundational, complementarity_as_accountability_trigger).
narrative_ontology:cs_axiom_status(complementarity_as_accountability_trigger, holdable).
narrative_ontology:cs_axiom_grounding('574ecfc5-bcd6-467b-8926-bb9bb53e01f1', complementarity_as_accountability_trigger, deontological).
narrative_ontology:cs_axiom('574ecfc5-bcd6-467b-8926-bb9bb53e01f1', foundational, genuine_intent_requires_substantive_accountability).
narrative_ontology:cs_axiom_status(genuine_intent_requires_substantive_accountability, holdable).
narrative_ontology:cs_axiom_grounding('574ecfc5-bcd6-467b-8926-bb9bb53e01f1', genuine_intent_requires_substantive_accountability, empirically_contingent).
narrative_ontology:cs_reference_frame('574ecfc5-bcd6-467b-8926-bb9bb53e01f1', complementarity_as_accountability_backstop).
narrative_ontology:cs_drift_state('574ecfc5-bcd6-467b-8926-bb9bb53e01f1', contemporary_practice_divergence, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('574ecfc5-bcd6-467b-8926-bb9bb53e01f1', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_accountability_framework).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, national_sovereigns_subject_to_scrutiny).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, elites_in_failed_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the constraint transfers sovereignty and judicial discretion from national to international forums; it is not pure snare (coordination function is real—accountability backstop) but the transfer is asymmetric and benefits the institutional agenda-setter (ICC) and abstract frame (universal justice) more than weak-state victims it nominally protects. Suppression is moderately high (0.62) because broad 'unwilling or unable' reading requires states to meet demanding standards or accept override; resistance is highest (0.71) because sovereigns contest thresholds and Global South states increasingly challenge ICC selectivity. Theater ratio is moderate (0.41): independence review is real, but institutional performance of neutrality obscures victor-state exemptions and Global North overrepresentation in beneficiary/victim asymmetry. Accessibility collapse is low (0.48): alternatives exist (domestic justice, transitional commissions, amnesty) but complementarity removes them by introducing external veto. Measurement series tracks the reading's institutional evolution: extractiveness rises slightly as case law precedents lower admissibility thresholds; suppression requirement rises as states face increasing cooperation demands (witness production, travel warrants); theater ratio rises as the court emphasizes procedure-as-legitimacy in response to selectivity accusations.
 *
 * PERSPECTIVAL GAP:
 *   The payer (national sovereign) seat and the agenda-setter (ICC) seat should compute as radically divergent. From the sovereign's position, broad complementarity is extractive override of judicial discretion; from the ICC's position, it is necessary enforcement of universal justice. The engine computes this divergence from the structural data (sovereign = payer, powerful but constrained exit; ICC = agenda-setter, institutional power, analytical exit). The authored claim is tangled_rope (genuine coordination function + asymmetric extraction + active enforcement), which the engine will either confirm or challenge based on the metrics and beneficiary/victim structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (victims, transitional justice advocates, liberal order architects) have low directionality (d ~0.1–0.2): they lack enforcement power but gain access to justice and symbolic accountability. They depend on ICC cooperation, so exit options trend toward 'constrained' for victims and 'mobile' for advocates. Payers (national sovereigns, elites, national courts) have high directionality (d ~0.75–0.85): they bear the cost of sovereignty transfer and judicial override. Exit options vary: sovereigns can withdraw from Rome Statute (costly but possible—'constrained'), elites have limited exit (flight or political reconstruction—'constrained'), national court judges cannot exit their institutional role ('identity_locked'). The ICC itself is positioned as analytical (institutional power, gatekeeper authority, but no direct collection—though it extracts institutional prestige and budget growth). No directionality overrides are needed: the structural derivation from beneficiary/victim + exit produces the intended per-seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (atrocity impunity in failed/complicit states) is CONTESTED in status: beneficiaries attest it is live; sovereigns attest it is over-interpreted; independent scholars note that ICC selectivity (overwhelming Africa focus) suggests the constraint serves victor's justice rather than universal accountability. The disappearance verdict is world_rearranges (sovereigns recover discretion, elites recover immunity, accountability architecture collapses). The mismatch (live founding problem but contested resolution) flags the constraint as a candidate for mandatrophy: the institutional means (ICC override) has decoupled from the intended end (universal accountability free from victor's bias). The theater ratio rising from 0.25 to 0.41 supports this: increasing share of ICC activity is procedural legitimacy-building rather than substantive prosecution of perpetrators, especially as selectivity accusations accumulate. A piton-transition signal: the constraint persists because beneficiaries vindicate the doctrine (accountability-transcends-sovereignty) even as the real function (holding elites accountable universally) atrophies in practice (selectivity by power and region).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unwilling_vs_unable_boundary,
    'Can ''unwilling'' be reliably distinguished from ''unable'' in state proceedings, or does the boundary collapse under scrutiny, allowing ICC determinations to become exercises of power politics?',
    'Comparative analysis of ICC admissibility decisions: if determinations track state power/alliance politics rather than judicial independence indicators, the boundary is unreliable.',
    'If reliable: broad complementarity reading is justified. If boundary collapses: the constraint becomes a snare (pure extraction of sovereignty transfer to ICC)—classification shifts from tangled_rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwilling_vs_unable_boundary, empirical, 'Whether ''unwilling or unable'' can be objectively determined or is proxy for power politics.').

omega_variable(
    victor_justice_ambiguity,
    'Does broad ''unwilling or unable'' interpretation enable or impede victors from framing selective prosecution as genuine accountability?',
    'Examine ICC prosecutions of victor states and their allies: are they held to the same complementarity standards as defeated/weak states? Does geographic and power-distribution asymmetry track random variation or systematic bias?',
    'If victors escape scrutiny while defeated states face intensive review: complementarity reading serves victor''s justice, not universal accountability—the constraint is extractive asymmetry cloaked in legitimacy doctrine. Classification remains tangled_rope, but mandatrophy signals sharpen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victor_justice_ambiguity, empirical, 'Whether broad complementarity serves universal accountability or victor-state advantage.').

omega_variable(
    sovereignty_presumption_inversion,
    'Does the international_oversight_reading invert the Rome Statute''s original presumption (sovereignty-first, ICC-as-backstop) into presumption-of-inadequacy (ICC-first, national-courts-on-trial)?',
    'Negotiating record of Rome Statute (preparatory documents, state intent declarations) versus current ICC case law practice on admissibility thresholds.',
    'If inversion is real: the reading is a departing interpretation that extrapolates beyond the kernel''s original commitment. This would support the kernel-context uncertainty: the international_oversight_reading and national_primacy_reading are irreconcilable commitments, and one must be rejected or the Rome Statute dissolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_presumption_inversion, conceptual, 'Whether broad complementarity inverts or fulfills the Rome Statute''s foundational bargain.').

omega_variable(
    extraction_captive_or_functional,
    'Is the measured extractiveness (0.58) a side effect of functional accountability architecture (coordination cost is legitimately high), or is it rent-seeking leverage that could be reduced without compromising victims'' access?',
    'Counterfactual design: could a narrower complementarity threshold (presuming national adequacy, ICC override rare) deliver the same accountability outcomes while reducing sovereignty transfer?',
    'If high extraction is functional cost: classification as tangled_rope is correct. If extraction is reducible without outcome loss: the constraint is snare (pure extraction defended by legitimacy doctrine)—classification shifts down.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_captive_or_functional, preference, 'Whether extraction is inherent to coordination or design choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__international_oversight_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__international_oversight_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__international_oversight_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__international_oversight_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__international_oversight_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(arti_tr_t25, article_17_complementarity__international_oversight_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__international_oversight_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__international_oversight_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__international_oversight_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__international_oversight_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__international_oversight_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(arti_be_t25, article_17_complementarity__international_oversight_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__international_oversight_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__international_oversight_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__international_oversight_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__international_oversight_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__international_oversight_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(arti_su_t25, article_17_complementarity__international_oversight_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__international_oversight_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).

% DUAL FORMULATION NOTE:
% Article 17 Complementarity decomposes into two structurally distinct constraints: (1) international_oversight_reading—broad 'unwilling or unable', low admissibility threshold, high ICC override frequency, vindicates universal accountability doctrine, extraction ε~0.58; (2) national_primacy_reading—narrow 'unwilling or unable', presumed national adequacy, ICC rare override, vindicates sovereign judicial discretion, extraction ε~0.25. Sibling readings with different ε values and different beneficiary/victim structures. Both are live readings held by different institutional coalitions; neither forecloses the other within a single framework (coexists_with relation), but this reading (international_oversight) creates structural pressure on the national_primacy reading by lowering admissibility thresholds and increasing ICC intervention frequency, making national sovereignty presumptions harder to sustain (influences relation). The Rome Statute's kernel is the complementarity principle itself; the contest is over what counts as 'genuine intent' and 'independence'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
