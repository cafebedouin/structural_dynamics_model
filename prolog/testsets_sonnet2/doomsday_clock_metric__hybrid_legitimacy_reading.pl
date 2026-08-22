% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock as Hybrid Scientific-Normative Judgment (Hybrid Legitimacy Reading)
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This story instantiates the hybrid legitimacy reading of the Doomsday
 *   Clock kernel: the claim that the clock's authority derives PRECISELY from
 *   its refusal to separate empirical risk indicators from normative judgment
 *   about what matters and how urgently. Unlike the objective_index_reading
 *   (which would treat the clock as a measurement instrument whose legitimacy
 *   depends on tracking real indicators) or the performative_tool_reading
 *   (which would treat the entanglement as strategic rhetoric optimized for
 *   mobilization), this reading holds that the entanglement is neither error
 *   nor strategy but an honest structural feature of any existential-risk
 *   metric — fact and value cannot be cleanly separated in this domain, and
 *   pretending otherwise (either through false objectivity or cynical
 *   instrumentalism) would be the actual distortion. Because this reading
 *   treats the hybrid as legitimate rather than as a cover story, it produces
 *   low extraction, low suppression, and no identifiable victim structure —
 *   the coordination benefit (a legible shared reference point) is real and
 *   the accountability gap (no auditable weighting) is a genuine cost but not
 *   one that is extracted BY anyone FROM anyone; it is diffuse and structural
 *   rather than captured.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.28).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.15).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock as Hybrid Scientific-Normative Judgment (Hybrid Legitimacy Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, 'e0ec7710-becd-4e1d-9c75-aadd81d8c292').
narrative_ontology:cs_kernel_codification('e0ec7710-becd-4e1d-9c75-aadd81d8c292', distributed).
narrative_ontology:cs_authority_grounding('e0ec7710-becd-4e1d-9c75-aadd81d8c292', expertise).
narrative_ontology:cs_interpretation_layer_present('e0ec7710-becd-4e1d-9c75-aadd81d8c292').
narrative_ontology:cs_reading_relation('e0ec7710-becd-4e1d-9c75-aadd81d8c292', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0ec7710-becd-4e1d-9c75-aadd81d8c292', doomsday_clock_metric__performative_tool_reading, influences).
narrative_ontology:cs_axiom('e0ec7710-becd-4e1d-9c75-aadd81d8c292', foundational, fact_value_entanglement_is_epistemically_necessary).
narrative_ontology:cs_axiom_status(fact_value_entanglement_is_epistemically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e0ec7710-becd-4e1d-9c75-aadd81d8c292', fact_value_entanglement_is_epistemically_necessary, conventional).
narrative_ontology:cs_axiom('e0ec7710-becd-4e1d-9c75-aadd81d8c292', foundational, expert_discretion_over_weighting_constitutes_legitimate_authority).
narrative_ontology:cs_axiom_status(expert_discretion_over_weighting_constitutes_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('e0ec7710-becd-4e1d-9c75-aadd81d8c292', expert_discretion_over_weighting_constitutes_legitimate_authority, instrumental).
narrative_ontology:cs_reference_frame('e0ec7710-becd-4e1d-9c75-aadd81d8c292', founding_synthesis_legitimacy).
narrative_ontology:cs_drift_state('e0ec7710-becd-4e1d-9c75-aadd81d8c292', contemporary_multi_risk_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e0ec7710-becd-4e1d-9c75-aadd81d8c292', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_field).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, policymakers_and_public).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, fact_value_entanglement_in_risk_assessment).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, expert_normative_judgment_as_legitimate_synthesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes annually to set the clock's minute value, explicitly combining empirical indicators (warhead counts, emissions trajectories, biosecurity incidents) with contested judgments about political will, institutional trust, and which risks deserve foregrounding. Defends the entanglement itself as the source of the clock's authority — a purely technical index would miss what matters, a purely rhetorical one would forfeit credibility. Retains full discretion over the weighting and framing each year, and the credibility this discretion produces accrues to the Board's own standing as arbiters of existential risk.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board, beneficiary).

% Uses the clock as a widely legible reference point and funding/attention anchor for research on nuclear, climate, and biological risk. Benefits from the clock's hybrid legitimacy because it licenses their own field's characteristic move — combining technical modeling with normative urgency claims — without requiring either side to be separately defensible.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_field, beneficiary,
    organized, generational, mobile, global).

% Receive the clock's annual announcement as a compressed signal about civilizational risk trajectory. Cannot independently audit the weighting between empirical and normative components, and have no formal channel to contest a given year's setting, but are not identifiably harmed by the ambiguity — they gain a usable heuristic they could not construct themselves.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policymakers_and_public, observer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, policymakers_and_public, beneficiary).

% Developers of alternative, more narrowly empirical risk indices (e.g. purely quantitative existential-risk dashboards) compete for the same attention space but lack the clock's seven-decade brand recognition. They are not suppressed by any enforcement mechanism — they simply cannot displace an incumbent whose legitimacy rests on a synthesis they consider methodologically illegitimate to attempt themselves.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, rival_risk_metrics_developers, excluded,
    moderate, biographical, mobile, global).

% Study the clock as a case study in whether fact and value can be cleanly separated in risk communication. From this seat, the entanglement is neither a flaw to be fixed nor a strategy to be exposed, but the honest structural condition of any existential-risk metric — the analytical claim this reading is built on.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, philosophers_of_science_communication, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__hybrid_legitimacy_reading, diffuse).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__hybrid_legitimacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, widely recognized reference point that lets disparate audiences (scientists, policymakers, the public) discuss trajectory of existential risk without each needing to independently synthesize nuclear, climate, and biosecurity indicators plus political-will assessments themselves.
% TRANSFER_FUNCTION: Moves interpretive authority over 'how worried should the world be' from any single discipline or actor to the Science and Security Board's collective judgment; moves attention and legitimacy toward the Bulletin as an institution and toward the existential-risk field's characteristic fact-value synthesis methodology.
% ABSENT_VOICES: Rival metric developers who believe risk should be tracked through disaggregated, auditable indicators would object that the entanglement is unfalsifiable by design; they are not excluded by rule but are structurally unable to compete with an incumbent whose authority rests on exactly the synthesis they reject. Publics affected by the risks being assessed (e.g. populations near nuclear facilities, biosecurity-vulnerable regions) have no seat in the annual determination at all.
% DISAPPEARANCE_RATIONALE: The Board and the existential-risk field would say the world rearranges substantially — a widely legible risk-attention anchor would vanish and require reconstruction by some other means. Rival metric developers and accountability-focused critics would say the world stays roughly the same or improves, since a more auditable index could occupy the same attention space without the unfalsifiable hybrid structure. The verdict genuinely depends on which sibling reading of the kernel one holds.
% FOUNDING_PROBLEM: In 1947, physicists involved in the Manhattan Project needed a way to communicate the urgency of nuclear risk to a public and policy establishment that lacked the technical background to evaluate warhead counts, doctrine, or escalation dynamics directly — a compressed, memorable signal was needed that could carry both technical content and normative urgency simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin itself attests the founding problem is still live, citing expanded scope to climate and biosecurity as evidence of continued relevance. Independent science-communication researchers outside the Bulletin (e.g. STS scholars studying risk perception) corroborate that a legibility gap between technical risk assessment and public/policy understanding persists, but some of the same researchers dispute that the ORIGINAL hybrid mechanism — rather than a reformed, more transparent one — is still the right instrument for it.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, contested).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).
:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 at present) because no party is identifiably paying so that another party may benefit — the Board's discretion produces institutional standing but not rents extracted from a victim class. Theater ratio is moderate and rising (0.20 to 0.42 over the interval) because as the clock's media performance has professionalized (annual press events, minute-hand ceremony) a growing share of the activity is communicative ritual layered atop the underlying judgment process; this is authored honestly as drift toward theater without claiming it dominates. Suppression is low because no alternative metrics are coercively excluded — rival indices simply cannot match the incumbent's legitimacy. Accessibility collapse is moderate (0.35): a purely empirical alternative remains conceivable and exists in practice (dashboards, indices) but has not displaced the clock's cultural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The Board and the existential-risk field sit toward the beneficiary end: they gain institutional standing and a validated methodology respectively, without a corresponding victim bearing an extracted cost. Policymakers and the public sit near symmetric-to-mildly-beneficiary: they receive a usable heuristic they could not construct themselves, at the cost of interpretive dependency they cannot audit — a cost, but not a captured rent. Rival metric developers are excluded from the conversation but not coerced out of it; their disadvantage is competitive, not extractive, which is why no victims are declared under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communicating technical risk to a non-technical public/policy audience) remains genuinely live per this reading, which is why founding_problem_status is authored as contested rather than dead — the hybrid reading holds the mechanism has not outlived its function, while critics (visible in the rival readings) hold it has ossified into brand maintenance. Classifying this as rope rather than tangled_rope or piton reflects the reading's own commitment: it denies there is a captured beneficiary extracting from an identifiable victim, and denies the coordination function has died, distinguishing it sharply from what the performative_tool_reading or a piton reading of the same kernel would conclude.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_legitimacy_vs_unfalsifiability,
    'Is the deliberate fact-value entanglement a genuine epistemic necessity of existential-risk assessment, or is it functionally indistinguishable from an unfalsifiable metric that cannot be wrong by design?',
    'Compare the clock''s minute-setting trajectory against a reconstructed purely-empirical index over the same period; persistent large divergences unexplained by disclosed normative weighting would support the unfalsifiability reading, while close tracking with disclosed value-adjustments would support the hybrid-legitimacy reading.',
    'If unfalsifiability dominates, this reading''s rope classification would be undermined and the constraint would structurally resemble the performative_tool_reading''s tangled_rope framing instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_legitimacy_vs_unfalsifiability, conceptual, 'Whether hybrid legitimacy is a real epistemic feature or an unfalsifiability artifact.').

omega_variable(
    kernel_framing_under_determination,
    'Is the choice to treat ''the Doomsday Clock'' as a single kernel with three readings (hybrid, objective, performative) itself under-determined — could a fourth reading (e.g. a pure-theater/piton reading treating the entanglement as vestigial brand maintenance with no live coordination function) be equally defensible from the same source material?',
    'Survey Bulletin archival records and Board member statements across decades for evidence of whether the entanglement was ever treated as instrumentally justified (piton-adjacent) versus principled (hybrid-legitimacy-adjacent) at the time of each setting.',
    'A fourth defensible reading would mean the kernel decomposition used here is incomplete; the hybrid_legitimacy_reading''s claim to represent ''the'' non-objective, non-performative alternative would need revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the three-reading kernel decomposition is exhaustive or under-determined.').

omega_variable(
    coordination_benefit_vs_accountability_void_tradeoff,
    'Does the coordination benefit (a legible shared risk-attention anchor) structurally require the accountability void (no auditable weighting formula), or could the same coordination function be achieved with disclosed, contestable weighting?',
    'Examine whether comparable hybrid metrics in other domains (e.g. credit ratings with disclosed methodology, IPCC confidence-level language) achieve similar coordination function with greater methodological transparency, without losing legibility.',
    'If disclosed weighting preserves coordination function, the accountability void is not necessary and this reading''s implicit defense of opacity-as-legitimacy weakens considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_vs_accountability_void_tradeoff, empirical, 'Whether opacity is necessary to the coordination function or a separable, contingent feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1947, 0.2).
narrative_ontology:measurement(doom_tr_t1970, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(doom_tr_t1990, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(doom_tr_t2005, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(doom_tr_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1947, 0.1).
narrative_ontology:measurement(doom_be_t1970, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(doom_be_t1990, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(doom_be_t2005, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(doom_be_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2015, 0.26).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2024, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(doomsday_clock_metric__hybrid_legitimacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__hybrid_legitimacy_reading, 0.05).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__performative_tool_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the doomsday_clock_metric kernel. objective_index_reading treats the clock as an empirical measurement instrument (its ε and classification would reflect fidelity-to-indicators concerns, likely producing a mountain-or-rope claim contested by measurement-validity critiques). performative_tool_reading treats the entanglement as strategically chosen for mobilization impact (its ε and classification would likely reflect tangled_rope or snare dynamics, with the public as an under-informed target of persuasion). This hybrid_legitimacy_reading occupies neither pole: it authors low extraction and a rope claim because it holds the entanglement itself to be legitimate rather than either a measurement failure or a manipulation. All three share the same underlying event (the annual clock announcement) but are structurally distinct constraints under the ε-invariance principle, since each reading assigns a different ε to what is nominally 'the same' arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
