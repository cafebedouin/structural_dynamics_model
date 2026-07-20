% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: Article II Non-Appropriation Deferral to International Regime (International Regime Reading)
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint is the international_regime reading of the
 *   ost_article_ii_non_appropriation kernel. Article II of the Outer Space
 *   Treaty states that outer space is not subject to national appropriation
 *   by claim of sovereignty, by means of use or occupation, or by any other
 *   means. The sibling extraction_permissive reading holds that this bars
 *   sovereign territorial claims but not private ownership of extracted
 *   resources; the sibling commons_conservation reading holds that 'use or
 *   occupation' prohibits de facto appropriation via resource extraction by
 *   states or private actors. This readingâthe international_regime
 *   readingâholds that Article II defers the appropriation question to a
 *   future multilateral framework (an Article XI analogue), meaning that
 *   neither extraction nor prohibition has treaty authority absent such a
 *   regime. The result is a legal grey zone in which first-mover firms
 *   operate, regime negotiation is stalled by zero-sum distributional
 *   conflict, and the treaty provision functions as a scaffold rather than a
 *   resolved rule.
 *
 * KEY AGENTS:
 *   - spacefaring_states: Agenda-setter (institutional/global) â maintains treaty ambiguity to preserve strategic flexibility and avoid binding benefit-sharing obligations
 *   - first_mover_corporations: Beneficiary (powerful/global) â exploits regulatory grey zone for resource extraction ventures under permissive national laws
 *   - late_entrant_states: Payer (organized/global) â excluded from benefit-sharing and resource access by stalled regime negotiations
 *   - commons_advocates: Excluded (moderate/global) â structurally absent from dominant interpretive fora, would argue for conservation reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.42).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.38).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "Article II Non-Appropriation Deferral to International Regime (International Regime Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_space_law/treaty_interpretation/commons_governance").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, '89711583-b2e9-4937-a2bd-9873e9ff5642').
narrative_ontology:cs_kernel_codification('89711583-b2e9-4937-a2bd-9873e9ff5642', fixed_text).
narrative_ontology:cs_authority_grounding('89711583-b2e9-4937-a2bd-9873e9ff5642', distributed).
narrative_ontology:cs_reading_relation('89711583-b2e9-4937-a2bd-9873e9ff5642', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('89711583-b2e9-4937-a2bd-9873e9ff5642', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_axiom('89711583-b2e9-4937-a2bd-9873e9ff5642', foundational, appropriation_deferred_to_future_regime).
narrative_ontology:cs_axiom_status(appropriation_deferred_to_future_regime, holdable).
narrative_ontology:cs_axiom_grounding('89711583-b2e9-4937-a2bd-9873e9ff5642', appropriation_deferred_to_future_regime, conventional).
narrative_ontology:cs_axiom('89711583-b2e9-4937-a2bd-9873e9ff5642', foundational, multilateral_framework_required_for_authority).
narrative_ontology:cs_axiom_status(multilateral_framework_required_for_authority, holdable).
narrative_ontology:cs_axiom_grounding('89711583-b2e9-4937-a2bd-9873e9ff5642', multilateral_framework_required_for_authority, conventional).
narrative_ontology:cs_reference_frame('89711583-b2e9-4937-a2bd-9873e9ff5642', article_ii_deferral_regime).
narrative_ontology:cs_drift_state('89711583-b2e9-4937-a2bd-9873e9ff5642', post_national_legislation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89711583-b2e9-4937-a2bd-9873e9ff5642', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_corporations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, late_entrant_states).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, article_xi_analogue_deferral).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, non_appropriation_transitional).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated and maintain the Outer Space Treaty framework. They benefit from the deferral reading by retaining policy flexibility for their national space industries while avoiding sovereignty conflicts with other major powers. They resist binding multilateral regime formation that would constrain their first-mover corporate actors or mandate benefit-sharing.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, spacefaring_states, agenda_setter,
    institutional, generational, mobile, global).

% Plan and execute space resource extraction missions under national legal frameworks (e.g., US Commercial Space Launch Competitiveness Act, Luxembourg Space Resources Act). They benefit from the absence of an authoritative conservation reading that would prohibit unilateral extraction, operating in the regulatory grey zone sustained by the deferral.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_corporations, beneficiary,
    powerful, biographical, constrained, global).

% Lack the technological capacity to engage in space resource extraction. They bear the cost of the legal grey zone: no guaranteed benefit-sharing mechanism, no veto over first-mover extraction, and stalled regime negotiations that freeze the distributional status quo in favor of technologically advanced states.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, late_entrant_states, payer,
    organized, generational, constrained, global).

% Represent cosmological, environmental, and heritage perspectives that are structurally excluded from COPUOS and bilateral space-law forums. They would argue for a conservation reading or for the rights of non-extractive users but lack standing in the dominant interpretive fora.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, commons_advocates, excluded,
    moderate, civilizational, constrained, global).

% Analyze and debate the competing readings of Article II. They document the divergence between the treatyâs original coordination function and its current operation as a holding pattern, and advocate for either regime closure or conservation interpretations.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_law_scholars, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__international_regime, first_mover_corporations).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__international_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents immediate territorial sovereignty conflicts over celestial bodies by deferring the appropriation question to a future international regime, maintaining stable great-power relations in space pending multilateral agreement.
% TRANSFER_FUNCTION: Transfers temporal advantage and legal grey-zone operational freedom from the international community as a whole to spacefaring states and their corporate first-movers, while transferring the cost of legal uncertainty and deferred benefit-sharing to late-entrant states.
% ABSENT_VOICES: Non-spacefaring nations and commons advocates with cosmological or heritage interests are structurally underrepresented in regime negotiations; conservation-oriented legal scholars who read Article II as prohibiting de facto appropriation are sidelined by the deferral reading.
% DISAPPEARANCE_RATIONALE: If this deferral reading were authoritatively resolvedâeither by a binding conservation interpretation or an explicit extraction-permissive frameworkâfirst-mover corporate strategies, national space legislation, and investment patterns would reorganize around the new authoritative rule; the current grey zone that sustains regulatory arbitrage would collapse.
% FOUNDING_PROBLEM: The 1967 Outer Space Treaty was negotiated under Cold War conditions where immediate territorial claims risked great-power conflict, but states could not agree on a comprehensive resource regime; Article II needed to forestall sovereignty grabs without resolving the politically intractable question of resource property rights.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the 1966â1967 COPUOS negotiations corroborate that the immediate problem was preventing sovereign territorial claims during the Cold War space race. However, no contemporary non-beneficiary party attests that the current deferral still serves that original coordination function; late-entrant states and the Group of 77 explicitly argue the opposite, that the deferral now serves extraction interests.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate because the constraint itself does not directly extract rents, but the legal grey zone it sustains enables unilateral resource capture by first-movers at the expense of orderly commons governance. The rising measurement series (0.15â0.42) captures the historical drift from Cold War coordination to post-2015 commercial extraction. Suppression (0.38) is moderate-low: the constraint suppresses neither the extraction-permissive nor conservation camp authoritatively, but it does suppress the emergence of a binding regime that would resolve the ambiguity. Theater_ratio (0.48) reflects the growing performative aspect of COPUOS negotiations, where annual debate sustains the appearance of progress toward a regime while distributional deadlock prevents closure. Accessibility_collapse (0.35) is low because the alternative (a multilateral regime) is institutionally accessible but politically blocked. Resistance (0.55) is moderate because late-entrant states and commons advocates actively contest the unilateral extraction narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the spacefaring-state seat, the constraint is a successful diplomatic scaffold that prevented immediate great-power conflict over celestial sovereignty. From the late-entrant-state seat, it is a procedural trap that freezes distributional injustice and denies them a voice in resource allocation. From the first-mover corporate seat, it is a permission structure that licenses extraction pending an indefinitely deferred regime. The engine computes these divergences from the structural data rather than adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   Spacefaring_states and first_mover_corporations are declared beneficiaries: they gain policy flexibility and operational grey-zone advantage from the deferral, placing them at low directionality (d near 0.0â0.2). Late_entrant_states are declared victims: they bear the cost of exclusion from resource benefits and lack of regime protection, placing them at high directionality (d near 0.8â1.0). The engine will scale effective extraction accordingly: high Ï for late entrants, damped or negative Ï for spacefaring states and corporations. Spacefaring states have mobile exit (can shape or exit treaty frameworks), while late entrants are constrained (dependent on multilateral processes they do not control), amplifying the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a scaffold rather than a rope prevents misreading the original 1967 coordination function as still fully operative; the founding problem (preventing sovereignty conflict) has been solved, but the transitional mechanism has persisted without its anticipated closure. Classifying it as a scaffold rather than a snare prevents conflating the genuine coordination origin with the current extraction-friendly grey zone. The scaffold type captures the exact structural position: a temporary support whose justification is the transition to a regime, not the steady state of unilateral extraction. The absence of a formal sunset clause in the treaty text itself is offset by the reading's own anticipatory structure (Article XI analogue), but the decades-long stall risks mandatrophy if the regime never materializes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_emergence_likelihood,
    'Will the anticipated multilateral international regime under an Article XI analogue actually emerge to resolve the appropriation question, or will the legal grey zone persist indefinitely?',
    'Observation of COPUOS and UNGA negotiations: successful adoption of a binding space resources convention or continued deadlock over benefit-sharing and scope.',
    'If the regime never emerges, the scaffold loses its transitional justification and decays toward a piton or a de facto rope for spacefaring states; if it emerges, the scaffold completes its intended function and dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_emergence_likelihood, empirical, 'Whether the anticipated future regime will materialize to close the deferral.').

omega_variable(
    scaffold_to_extraction_drift,
    'Has the deferral reading shifted from a genuine transitional coordination mechanism to a cover story for unilateral extraction-by-default?',
    'Comparative analysis of state practice: if national space resource legislation and bilateral agreements proliferate without multilateral coordination, the drift toward extraction-permissive practice is confirmed.',
    'If the reading has drifted, the constraint''s effective extractiveness is higher than its scaffold claim suggests, and it should be reclassified as a tangled rope or snare; if not, it remains a stalled scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_to_extraction_drift, conceptual, 'Whether the scaffold has drifted into serving extraction interests.').

omega_variable(
    committer_reading_stability,
    'Is the international_regime reading structurally stable against its siblings, or will empirical state practice collapse the interpretive ambiguity into one of the other readings?',
    'Tracking ICJ or international tribunal jurisprudence, or a decisive multilateral convention that adopts one reading.',
    'If practice collapses the ambiguity, this constraint dissolves as a distinct reading and merges into either the extraction_permissive or commons_conservation constraint family member.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_stability, conceptual, 'Stability of this reading against sibling readings given evolving state practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__international_regime, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ost__tr_t15, ost_article_ii_non_appropriation__international_regime, theater_ratio, 15, 0.28).
narrative_ontology:measurement(ost__tr_t30, ost_article_ii_non_appropriation__international_regime, theater_ratio, 30, 0.35).
narrative_ontology:measurement(ost__tr_t45, ost_article_ii_non_appropriation__international_regime, theater_ratio, 45, 0.42).
narrative_ontology:measurement(ost__tr_t60, ost_article_ii_non_appropriation__international_regime, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ost__be_t15, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 15, 0.2).
narrative_ontology:measurement(ost__be_t30, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 30, 0.26).
narrative_ontology:measurement(ost__be_t45, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 45, 0.34).
narrative_ontology:measurement(ost__be_t60, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 60, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ost_article_ii_non_appropriation__international_regime, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Article II non-appropriation kernel. The international_regime reading treats the text as deferring the appropriation question to a future multilateral framework, while the extraction_permissive reading treats it as permitting private extraction and the commons_conservation reading treats it as prohibiting de facto appropriation. These are not the same constraint viewed from different angles; they have different epsilon values, different beneficiary/victim structures, and different empirical/legal status. They form a constraint family linked by shared kernel origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
