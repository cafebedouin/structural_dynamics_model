% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Hybrid (Legitimacy-Conditioned) Reading of the Montevideo Statehood Criteria
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the Montevideo statehood
 *   kernel: statehood requires not only the classical objective criteria
 *   (population, territory, government, capacity for foreign relations) but
 *   also normative legitimacy — democratic governance, human rights
 *   compliance, and non-aggression. This is a distinct constraint from the
 *   declaratory reading (objective criteria alone establish statehood) and
 *   the constitutive reading (recognition by existing states is
 *   constitutive). Under the hybrid reading, entities that satisfy the
 *   classical criteria but fail the normative overlay are denied full
 *   standing, while the states positioned to judge normative compliance are
 *   disproportionately the powerful liberal democracies that built the
 *   standard. This creates a new victim class relative to the declaratory
 *   reading: non-liberal secessionists and authoritarian-but-effective states
 *   who would qualify under pure effectiveness but do not under the hybrid
 *   test — and a new justificatory resource for the beneficiary bloc:
 *   humanitarian intervention and regime-change operations gain legal cover
 *   from the claim that a government's own illegitimacy voids its sovereign
 *   protections.
 *
 * KEY AGENTS:
 *   - liberal_democratic_states: agenda_setter/beneficiary (institutional/arbitrage) — set and apply the normative overlay
 *   - non_liberal_secessionist_movements: payer (powerless/trapped) — meet objective criteria but fail normative test
 *   - authoritarian_but_functionally_effective_states: payer (moderate/constrained) — effective control denied full legitimacy
 *   - international_human_rights_advocates: beneficiary (organized/mobile) — gain a legal lever, bear no cost of denial
 *   - excluded_populations_under_unrecognized_authorities: excluded (powerless/trapped) — bear consequences, no voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.62).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.58).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Hybrid (Legitimacy-Conditioned) Reading of the Montevideo Statehood Criteria").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '7f8fc732-e5bb-4c7e-809a-54e09730f4d1').
narrative_ontology:cs_kernel_codification('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', distributed).
narrative_ontology:cs_authority_grounding('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', distributed).
narrative_ontology:cs_reading_relation('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', montevideo_statehood_criteria__declaratory_reading, influences).
narrative_ontology:cs_reading_relation('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', foundational, statehood_conditioned_on_governance_legitimacy).
narrative_ontology:cs_axiom_status(statehood_conditioned_on_governance_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', statehood_conditioned_on_governance_legitimacy, deontological).
narrative_ontology:cs_axiom('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', secondary, effectiveness_alone_insufficient_for_full_sovereign_standing).
narrative_ontology:cs_axiom_status(effectiveness_alone_insufficient_for_full_sovereign_standing, holdable).
narrative_ontology:cs_axiom_grounding('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', effectiveness_alone_insufficient_for_full_sovereign_standing, conventional).
narrative_ontology:cs_reference_frame('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', post_cold_war_liberal_internationalist_consensus).
narrative_ontology:cs_drift_state('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', post_2014_multipolar_contestation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7f8fc732-e5bb-4c7e-809a-54e09730f4d1', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, western_led_recognition_blocs).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, international_human_rights_advocates).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_but_functionally_effective_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, post_colonial_states_with_contested_governance_records).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, democratic_governance_as_legal_precondition).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, human_rights_compliance_as_sovereignty_condition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the dominant recognition apparatus (UN membership votes, EU accession frameworks, bilateral recognition practice) and have layered democratic governance, human rights compliance, and non-aggression onto the classical four-criteria test. This lets them withhold or grant recognition based on a regime's internal character, not merely its territorial control and government effectiveness. They face no comparable scrutiny of their own historical recognition inconsistencies.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, beneficiary).

% Achieve effective territorial control, a permanent population, and a functioning government — satisfying the classical objective criteria — but are denied recognition because their internal governance is not liberal-democratic or their rights record is contested. They have no forum to contest the added normative layer and cannot exit the requirement; failing it means permanent non-statehood regardless of on-the-ground effectiveness.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer,
    powerless, biographical, trapped, regional).

% Exercise full effective control over territory and population and meet every classical Montevideo criterion, but face contested legitimacy, sanctions regimes, or non-recognition campaigns keyed to governance character. They can attempt reform to satisfy the normative layer, but reform is costly, slow, and evaluated by the same states that benefit from withholding recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, authoritarian_but_functionally_effective_states, payer,
    moderate, biographical, constrained, national).

% Already recognized under the classical criteria decades ago, but find their continued full standing periodically re-litigated (aid conditionality, suspension from regional bodies, selective non-recognition of governments) whenever elections or rights records are judged deficient by the same normative layer now used prospectively against new claimants.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, post_colonial_states_with_contested_governance_records, payer,
    moderate, generational, constrained, national).

% Gain a legal lever: the hybrid reading lets them argue that gross rights violations or non-democratic seizure of power should defeat or suspend statehood claims, reinforcing their advocacy goals. They do not bear the costs of denied recognition themselves and can shift focus case to case.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Coordinate recognition policy through alliances and multilateral bodies, using the normative layer to justify selective recognition, sanctions, or intervention against regimes framed as illegitimate, while extending swift recognition to governance changes (including externally-assisted ones) that align with liberal-democratic preferences.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, western_led_recognition_blocs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, western_led_recognition_blocs, beneficiary).

% Adjudicate recognition and membership disputes, applying an increasingly normatively-inflected standard inconsistently across cases depending on which states have interests at stake. Can in principle revise practice but is itself composed of the same states whose interests the hybrid standard serves.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, un_general_assembly_and_regional_bodies, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, un_general_assembly_and_regional_bodies, agenda_setter).

% Live under authorities denied recognition on normative grounds, bearing the practical consequences (no treaty access, no international financial institution membership, no diplomatic protection) without any voice in the recognition debate that determines their status.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, excluded_populations_under_unrecognized_authorities, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the international system with a shared standard for distinguishing entities entitled to full sovereign treatment from those that are not, reducing the friction and unpredictability of ad hoc, case-by-case recognition disputes, and in principle aligns statehood with baseline protections against tyranny and aggression.
% TRANSFER_FUNCTION: Moves legal standing, treaty access, international financial institution membership, and diplomatic protection toward entities whose internal governance matches liberal-democratic and human-rights benchmarks, and away from entities that meet the classical objective criteria but fail the normative overlay — regardless of the overlay's own contestedness or the overlay-setters' consistency in applying it.
% ABSENT_VOICES: Non-liberal secessionist movements and populations under unrecognized authorities would object that the normative layer is applied selectively and self-servingly by the same powerful states that benefit from controlling the recognition gate; they have no forum within the UN system to contest the standard itself, only individual applications of it.
% DISAPPEARANCE_RATIONALE: If the hybrid reading disappeared and recognition reverted to the pure declaratory standard, several currently-unrecognized effective governments would gain a stronger legal claim to statehood, and humanitarian-intervention justifications tied to 'illegitimate' governance would lose a layer of legal cover — a real rearrangement for those actors. But for most already-recognized states, day-to-day international relations would look similar, since the normative layer is invoked selectively rather than continuously; hence the verdict is contested rather than a clean rearranges/unchanged call.
% FOUNDING_PROBLEM: The classical four-criteria test (permanent population, defined territory, government, capacity for foreign relations) could validate territorial control achieved by force, ethnic cleansing, or authoritarian seizure as equally entitled to statehood as governments arising through self-determination and consent — the hybrid reading was built to prevent naked effectiveness from being legally laundered into full sovereign standing.
% FOUNDING_PROBLEM_CORROBORATION: Human rights scholars and international courts (citing, e.g., non-recognition of apartheid-era Rhodesia and of entities created through unlawful use of force) attest the normative layer addresses a genuine gap in the declaratory test. Non-Western international law scholars and representatives of unrecognized effective governments attest, from outside the beneficiary bloc, that the same normative criteria are invoked asymmetrically — enforced against weak or geopolitically inconvenient claimants while excused for powerful states' own governance failures — making the 'genuine gap-filling' account only partly corroborated.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, contested).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high: the hybrid standard does perform a real coordination function (denying legal laundering of force-based territorial control) but the enforcement of the normative overlay is asymmetric, concentrating costs on weaker or geopolitically inconvenient claimants while sparing powerful incumbents with comparable governance deficits. Suppression (0.58) reflects that non-liberal secessionists and contested-legitimacy states have essentially no forum to contest the standard's application, only individual case outcomes. Theater ratio (0.40) captures that a meaningful share of normative-legitimacy invocations function as post-hoc justification for recognition decisions already made on geopolitical grounds, rather than genuine antecedent screening — this share has risen over the interval as the overlay has been invoked more frequently in contested cases (Kosovo, South Ossetia, Abkhazia, Crimea, various African secession movements) without corresponding rise in consistent application.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic states and the recognition blocs they lead sit at the beneficiary end: they set the normative criteria, apply them, and gain both the coordination benefit (a principled-sounding basis for recognition policy) and the strategic benefit (discretion to deny recognition to inconvenient effective governments while extending it to convenient ones). Non-liberal secessionists and authoritarian-but-effective states sit at the target end: they satisfy the older objective test but are denied the legal upgrade because of internal governance judged by external, unaccountable evaluators. Human rights advocates are beneficiaries in a narrower sense — normatively aligned with the standard's stated purpose, but structurally insulated from the costs its selective application imposes on the excluded populations they nominally advocate for.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing naked effectiveness (e.g., force, ethnic cleansing, unconstitutional seizure) from being legally laundered into full statehood — remains partly live: some rejections under the hybrid standard track genuine, widely-shared concerns (non-recognition of apartheid Rhodesia, non-recognition of Turkish Republic of Northern Cyprus). But the founding-problem status is marked contested because the same standard is applied with visibly different rigor depending on the geopolitical alignment of the claimant, which is the signature of a standard whose stated coordination function has been partially captured by its administering bloc's strategic interests — exactly the asymmetry the tangled_rope classification is built to register.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_norm_or_selective_pretext,
    'Is the normative-legitimacy overlay a genuine, principled constraint on statehood recognition, or a selectively-invoked pretext that tracks the geopolitical interests of the states empowered to apply it?',
    'Comparative case analysis: code all contested recognition/non-recognition decisions since 1990 against (a) the claimant''s governance/rights record and (b) the claimant''s alignment with the interests of the dominant recognition bloc; test whether governance record predicts outcome independent of alignment.',
    'If governance record predicts outcomes independent of geopolitical alignment, the hybrid reading is closer to a genuine rope-like normative advance; if alignment predicts outcomes independent of governance record, it is closer to a snare using normative language as cover for power-political recognition decisions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_norm_or_selective_pretext, empirical, 'Whether the normative overlay tracks genuine legitimacy or geopolitical alignment.').

omega_variable(
    kernel_reading_choice_is_contestable,
    'Is the hybrid reading itself a defensible evolution of customary international law, or an interpretive move by powerful states to entrench discretionary control over recognition that the declaratory reading was designed to foreclose?',
    'Track opinio juris and state practice: does a genuine cross-regional consensus support normative conditions on statehood, or is the hybrid standard advocated primarily by one geopolitical bloc and resisted by others (as reflected in UN voting patterns, African Union and non-aligned movement statements)?',
    'If cross-regional consensus is thin, the hybrid reading''s claim to be customary law is weaker than its proponents assert, and its practical effect is closer to unilateral recognition policy dressed in legal-doctrinal language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_is_contestable, conceptual, 'Whether the hybrid reading reflects genuine customary law evolution or bloc-specific doctrinal entrenchment — this is the committer-axis ambiguity distinguishing this reading from its siblings.').

omega_variable(
    asymmetric_enforcement_against_incumbents,
    'Why are already-recognized states with comparable or worse governance/rights records not subject to statehood-questioning under the same normative overlay applied prospectively to new claimants?',
    'Compare treatment of new claimant governance failures against treatment of incumbent-state governance failures of similar severity (coups, election fraud, rights abuses) — does either category face loss of recognized status, suspension, or only the new claimant?',
    'A finding that incumbents are systematically exempted would support classifying the normative overlay as primarily a gatekeeping tool against new entrants rather than a genuinely bidirectional legitimacy standard, sharpening the tangled_rope classification toward extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_enforcement_against_incumbents, empirical, 'Whether the normative standard is applied symmetrically to incumbents and new claimants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(mont_tr_t1997, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1997, 0.29).
narrative_ontology:measurement(mont_tr_t2004, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2004, 0.33).
narrative_ontology:measurement(mont_tr_t2011, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2011, 0.36).
narrative_ontology:measurement(mont_tr_t2018, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(mont_be_t1997, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1997, 0.48).
narrative_ontology:measurement(mont_be_t2004, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2004, 0.53).
narrative_ontology:measurement(mont_be_t2011, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2011, 0.57).
narrative_ontology:measurement(mont_be_t2018, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(mont_su_t1997, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1997, 0.45).
narrative_ontology:measurement(mont_su_t2004, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2004, 0.49).
narrative_ontology:measurement(mont_su_t2011, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2011, 0.53).
narrative_ontology:measurement(mont_su_t2018, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2018, 0.56).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, constitutive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the montevideo_statehood_criteria kernel. declaratory_reading holds that the classical four objective criteria alone establish statehood as legal fact, with no normative overlay and no beneficiary bloc controlling a legitimacy gate. constitutive_reading holds that recognition by existing states is itself constitutive, dispensing with an independent objective floor. This hybrid_reading occupies the middle position: it retains declaratory_reading's objective floor but adds a normative-legitimacy filter that produces a distinct victim set (non-liberal secessionists, authoritarian-but-effective states) not present in declaratory_reading, and a distinct legitimating resource (humanitarian intervention/regime-change cover) not present in constitutive_reading's pure power-recognition account. All three are ε-invariant, separately classified constraints; do not average or reconcile their extraction values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
