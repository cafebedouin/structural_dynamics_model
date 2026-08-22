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
 *   human_readable: Doomsday Clock as Irreducibly Hybrid Scientific-Normative Instrument
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This story instantiates the hybrid_legitimacy_reading of the
 *   doomsday_clock_metric kernel: the claim that the clock's minute-hand
 *   setting is not reducible to either a pure empirical index or a pure
 *   advocacy tool, but that its legitimacy rests precisely on the irreducible
 *   entanglement of scientific judgment and normative stakes characteristic
 *   of existential-risk assessment. Under this reading the clock is not lying
 *   by being ambiguous — the ambiguity is doing real epistemic work, because
 *   existential risk assessment genuinely cannot be cleanly separated into
 *   fact-finding and value-weighing components at the scale and uncertainty
 *   involved. This reading differs sharply in beneficiary/victim structure
 *   from its siblings: the objective_index_reading would treat any normative
 *   admixture as measurement error and would need to name victims of that
 *   contamination (readers misled about objectivity); the
 *   performative_tool_reading would treat the ambiguity as strategic and
 *   would need to name whoever's interests the strategic choices serve. This
 *   reading declares no clear victims because the entanglement it describes
 *   is, on its own terms, not an extraction mechanism — it is a structural
 *   feature of the domain. The absence of victims and the modest
 *   extractiveness score are the reading's central empirical commitment, not
 *   an oversight.
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
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock as Irreducibly Hybrid Scientific-Normative Instrument").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '926ec13d-c877-4962-8ac9-b11e067bc62b').
narrative_ontology:cs_kernel_codification('926ec13d-c877-4962-8ac9-b11e067bc62b', distributed).
narrative_ontology:cs_authority_grounding('926ec13d-c877-4962-8ac9-b11e067bc62b', expertise).
narrative_ontology:cs_interpretation_layer_present('926ec13d-c877-4962-8ac9-b11e067bc62b').
narrative_ontology:cs_reading_relation('926ec13d-c877-4962-8ac9-b11e067bc62b', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('926ec13d-c877-4962-8ac9-b11e067bc62b', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('926ec13d-c877-4962-8ac9-b11e067bc62b', foundational, fact_value_entanglement_is_structurally_irreducible).
narrative_ontology:cs_axiom_status(fact_value_entanglement_is_structurally_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('926ec13d-c877-4962-8ac9-b11e067bc62b', fact_value_entanglement_is_structurally_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('926ec13d-c877-4962-8ac9-b11e067bc62b', secondary, deliberate_ambiguity_can_be_epistemically_warranted_legitimacy_source).
narrative_ontology:cs_axiom_status(deliberate_ambiguity_can_be_epistemically_warranted_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('926ec13d-c877-4962-8ac9-b11e067bc62b', deliberate_ambiguity_can_be_epistemically_warranted_legitimacy_source, conventional).
narrative_ontology:cs_reference_frame('926ec13d-c877-4962-8ac9-b11e067bc62b', manhattan_project_expert_stewardship).
narrative_ontology:cs_drift_state('926ec13d-c877-4962-8ac9-b11e067bc62b', multi_domain_risk_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('926ec13d-c877-4962-8ac9-b11e067bc62b', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_field).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, policymakers_and_publics).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, policymakers_and_publics).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, fact_value_entanglement_in_risk_assessment).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_judgment_requires_normative_commitment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the clock's minute-hand position annually through closed deliberation combining physicists', biosecurity experts', and policy scholars' judgments. Defends the ambiguity between empirical tracking and normative signaling as the source of the clock's authority — a purely mechanical index would be more falsifiable but less able to speak to stakes that are not reducible to measured indicators. Gains reputational standing and continued institutional relevance from the clock's persistence as the field's most recognized artifact.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board, beneficiary).

% Draws public attention, funding conversations, and disciplinary legitimacy partly through the clock's cultural salience. Benefits from the hybrid framing because it licenses expert judgment under irreducible uncertainty — a stance the field needs for its own methodological survival, since most existential-risk estimates cannot be validated against outcomes.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_field, beneficiary,
    organized, generational, constrained, global).

% Receive a condensed, memorable signal of expert concern that can inform attention allocation and political will, without needing to parse the underlying multi-domain assessment. Pay an indirect cost when the same number is treated as if it were a validated risk estimate in debates where its normative content is doing unacknowledged work — the signal can be over-read as more objective than the board itself claims it is.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policymakers_and_publics, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, policymakers_and_publics, payer).

% Researchers proposing more decomposed, falsifiable risk indices (separate probability estimates per risk category, calibrated forecasting tournaments) argue the hybrid framing is unfalsifiable by design and crowds out competing measurement approaches by monopolizing public attention on existential risk. They are not part of the board's deliberation and have no formal channel to contest the clock's methodology.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, rival_metric_designers, excluded,
    moderate, biographical, constrained, global).

% Study the clock as a case of deliberately maintained interpretive ambiguity in science communication — neither claiming pure objectivity nor pure advocacy allows the instrument to carry legitimacy across both registers simultaneously. They document how this dual register functions rhetorically without adjudicating whether it is warranted.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, science_communication_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, low-bandwidth signal that lets a fragmented expert community and a public with no capacity to evaluate multi-domain existential risk assessments converge on a shared sense of urgency, without requiring the underlying judgments to be independently reproducible.
% TRANSFER_FUNCTION: Moves attention and legitimacy from the diffuse work of individual researchers and disciplines toward the board's annual pronouncement; moves interpretive authority from the falsifiability standards of empirical science toward the board's closed normative-cum-empirical synthesis.
% ABSENT_VOICES: Designers of decomposed, falsifiable risk indices and calibrated-forecasting researchers are not represented in the board's deliberation and have no institutional standing to contest the clock's methodology; publics who treat the number as a validated probability rather than a normative judgment are also not consulted about that conflation.
% DISAPPEARANCE_RATIONALE: The board and much of the existential-risk field would say the clock's disappearance would remove a valuable, if imperfect, coordination device and existential risk discourse would lose a shared reference point. Critics who favor decomposed, falsifiable metrics would say little of empirical substance would be lost and the field might gain rigor. Both positions are genuinely held by informed parties, which is why this sits at contested rather than either pole.
% FOUNDING_PROBLEM: In 1947, physicists involved in the Manhattan Project needed a way to communicate urgency about nuclear risk to a public and policy establishment that could not evaluate classified technical assessments directly — a symbolic instrument that could carry both expert judgment and moral urgency in a single legible gesture.
% FOUNDING_PROBLEM_CORROBORATION: The board itself attests the founding problem (communicating irreducibly entangled scientific-and-moral urgency) remains live, citing the expansion to biosecurity, climate, and AI risk. Science communication scholars outside the board, studying the instrument's rhetorical function rather than defending it, corroborate that the clock still performs this hybrid signaling role; they do not adjudicate whether that role is still the best available one, only that it persists and is deliberately maintained as such.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, contested).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.28) and rising only slowly, reflecting the reading's own claim that the hybrid function is a genuine feature of the domain rather than a captured mechanism — the modest rise over decades tracks the field's professionalization and increasing institutional stakes in the clock's continued salience, not an extraction ratchet. Theater ratio is authored moderate and rising (0.20 to 0.42) because part of what distinguishes this reading from a naive rope reading is that some of the annual ritual of clock-setting is genuinely performative even under this reading's own lights — the entanglement thesis does not deny that ceremony has grown alongside substance, it denies that the ceremony is a cover for something else. Suppression is low (0.15) because no one is coerced into accepting the clock's framing; resistance is moderate (0.4) because rival metric designers and falsifiability-minded critics actively contest the approach, which this reading must acknowledge even while defending the hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The board and the existential-risk research field are named beneficiaries because the hybrid legitimacy structure is precisely what licenses their continued authority to speak with a mixture of expert and moral voice — this is a structural benefit, not an accusation of bad faith. Policymakers and publics are both beneficiaries (receive a usable signal) and, secondarily, bear a cost when the signal's normative content is mistaken for pure measurement. No group is named a pure victim, consistent with the reading's expected structural delta: this reading maintains that the coordination benefit (a shared attention signal) is not clearly outweighed by an accountability void, in contrast to how the performative_tool_reading or a critical reading of the objective_index_reading might characterize the same artifact.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — communicating irreducibly hybrid scientific-and-moral urgency to publics who cannot evaluate the underlying technical judgments directly — is authored as contested-but-live rather than dead, which blocks a mandatrophy verdict under this reading. A hybrid_legitimacy reading in which the founding problem had gone dead (say, if calibrated forecasting had fully displaced narrative risk communication) would need to reconsider whether the clock persists on inertia; this reading's corroboration from science communication scholars outside the board supports treating the function as still active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entanglement_versus_strategic_ambiguity,
    'Is the fact-value entanglement in clock-setting an irreducible structural feature of existential risk judgment, or a strategically maintained ambiguity that serves the board''s institutional interests (the performative_tool_reading''s claim)?',
    'Compare board deliberation records across decades for evidence of explicit strategic calibration of clock movements against media cycles or funding events versus evidence of good-faith struggle with genuinely inseparable normative and empirical considerations. No fully dispositive resolution mechanism exists because the two hypotheses predict overlapping observable behavior.',
    'If strategic ambiguity dominates, this reading collapses toward the performative_tool_reading and the beneficiary structure would need revision to include an accountability-void victim class (publics misled about the signal''s nature). If irreducible entanglement dominates, this reading''s low-extraction, no-victim structure is sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(entanglement_versus_strategic_ambiguity, conceptual, 'Whether the hybrid framing reflects genuine epistemic necessity or strategic institutional choice — the core fork between this reading and its performative sibling.').

omega_variable(
    objectivity_claim_residue,
    'Does the board''s public communication about the clock retain enough objective-index framing (precise minute-hand distances, year-over-year comparisons) to make the objective_index_reading a live competing self-description, even if the hybrid_legitimacy_reading is structurally more accurate?',
    'Discourse analysis of the board''s own press releases and statements: do they emphasize the judgment-laden, normative character of the assessment, or do they lean on numerical precision language that implies measurement?',
    'If the board''s own communication leans toward objective-index framing while the underlying process is genuinely hybrid, this reading would need to additionally account for a form of institutional self-misrepresentation not currently captured in the beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objectivity_claim_residue, empirical, 'Whether the board''s public self-presentation matches or diverges from the hybrid_legitimacy reading''s own account of what the clock is doing.').

omega_variable(
    no_victim_structure_stability,
    'Is the absence of a clear victim group under this reading a stable structural fact, or an artifact of this reading''s own framing that a more critical hybrid reading would revise?',
    'Solicit accounts from rival metric designers and calibration-forecasting researchers about whether they experience concrete costs (funding displacement, attention capture) from the clock''s continued cultural dominance, treated as evidence bearing on victim-structure even under a hybrid-legitimacy framing.',
    'If rival methodologists can document concrete displaced funding or attention, the no-victim structure would need revision even while retaining the hybrid_legitimacy claim about entanglement — the two questions (is the entanglement real, and who pays for the clock''s dominance) are logically separable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(no_victim_structure_stability, empirical, 'Whether the reading''s declared absence of victims survives scrutiny of opportunity costs borne by competing risk-communication approaches.').


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
narrative_ontology:measurement(doom_tr_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1947, 0.15).
narrative_ontology:measurement(doom_be_t1970, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1970, 0.18).
narrative_ontology:measurement(doom_be_t1990, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(doom_be_t2005, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(doom_be_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2015, 0.25).
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
% This constraint is one of three sibling readings of the doomsday_clock_metric kernel. objective_index_reading treats normative content as measurement contamination (would authorize a victim class: publics misled about objectivity). performative_tool_reading treats the ambiguity as strategically instrumental (would authorize a beneficiary class capturing mobilization value and a victim class bearing accountability costs). This reading (hybrid_legitimacy_reading) treats the fact-value entanglement as structurally irreducible to the domain and authors no clear victim structure. All three share the same underlying artifact (the clock) but instantiate structurally distinct constraints with distinct ε values and distinct beneficiary/victim declarations, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
