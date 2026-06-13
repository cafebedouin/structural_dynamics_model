% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_democratic_participation, []).

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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Political Speech Protection (Democratic Participation Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel: the
 *   speech protection doctrine grounded in democratic theory. Under this
 *   reading, political expression — speech about governance, public policy,
 *   official conduct, and constitutional meaning — receives maximal or
 *   categorical protection as a prerequisite for democratic self-governance.
 *   Non-political speech (commercial, artistic, intimate) receives graduated
 *   protection. This reading faces structural challenge from the dignity
 *   reading (which would constrain speech that functions as systematic
 *   subordination), the harm threshold reading (which would allow restriction
 *   of demonstrably harmful speech), and the marketplace reading (which
 *   treats speech protection as serving truth-discovery through competitive
 *   exchange, not as a prerequisite to voting). The constraint's
 *   beneficiaries are political speakers and the voting public; the victims
 *   are targets of political speech and marginalized groups whose exclusion
 *   from political standing is reinforced by political speech this reading
 *   protects. The constraint CLAIMS to be rope (genuine coordination on a
 *   necessary condition for democracy); the authored metrics show moderate
 *   extraction rising over time, suggesting that institutional power to
 *   define the political/non-political boundary creates extractive leverage.
 *
 * KEY AGENTS:
 *   - political_speakers: Agents exercising speech about governance, receiving categorical protection — constitute the beneficiary constituency
 *   - voting_public: Citizens requiring access to political speech for informed participation — also beneficiary
 *   - democratic_institutions (courts, legislatures): Agenda-setters interpreting and policing the political/non-political boundary — define the constraint's operation
 *   - targets_of_political_speech: Government officials and public figures exposed to criticism — bear the cost of maximum speaker protection
 *   - non_political_speakers: Speakers in commercial, artistic, intimate domains — receive graduated protection, constrained exit
 *   - subordinated_groups: Historically excluded from political standing; experience political speech as weaponized exclusion — structural victims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.38).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.29).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Political Speech Protection (Democratic Participation Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, 'abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c').
narrative_ontology:cs_kernel_codification('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', fixed_text).
narrative_ontology:cs_authority_grounding('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', lineage).
narrative_ontology:cs_interpretation_layer_present('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c').
narrative_ontology:cs_reading_relation('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_axiom('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', foundational, political_speech_prerequisite_to_self_governance).
narrative_ontology:cs_axiom_status(political_speech_prerequisite_to_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', political_speech_prerequisite_to_self_governance, deontological).
narrative_ontology:cs_axiom('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', foundational, internal_hierarchy_political_over_non_political).
narrative_ontology:cs_axiom_status(internal_hierarchy_political_over_non_political, holdable).
narrative_ontology:cs_axiom_grounding('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', internal_hierarchy_political_over_non_political, deontological).
narrative_ontology:cs_reference_frame('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', political_speech_maximal_protection_democracy_foundational).
narrative_ontology:cs_drift_state('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abdc8321-ec6d-4cbd-ac0d-e3e64e85a45c', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, voting_public).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, democratic_institutions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).
:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the asymmetry in protection: political speech gains categorical or near-categorical immunity; non-political speech and targets of political speech lose protection. The reading's extractiveness is not as high as a snare (0.74) because the coordination function is real — democratic self-governance does require protection for political expression — but it rises over the interval (0.22 to 0.38) as institutional power to define the boundary accumulates. Suppression measures the institutional effort required to police the political/non-political boundary and exclude challenge to that boundary itself. Theater ratio measures the performative element: courtroom discussion of 'political importance' as the criterion for protection can substitute for actual analysis of whether the particular speech functions in governance. The measurement series track a story of initial tight constraint (early 20th-century clarity about what counts as political) degrading into increasingly contested boundary-setting, with institutional authority filling the gap — the extractiveness rise from 0.22 to 0.38 and theater rise from 0.08 to 0.18 describe this degradation. At time=80, the metrics stabilize slightly as litigation settles some boundaries.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (courts/institutions) and the beneficiary (political speakers) should see this as coordination; the payers (targets of speech, non-political speakers, subordinated groups) should see it as enforced asymmetry. Institutional seat: 'We maintain the political/non-political boundary to protect democracy itself.' Target seat: 'You use the boundary to shield government from criticism and shield yourselves (a powerful institutional cohort) from defamation claims.' Non-political speaker seat: 'Our expression is restricted while theirs is protected based on a classification you control.' Subordinated-group seat: 'The reading protects speech that excludes us from political standing and weaponizes our exclusion — calling it political expression insulates it from restriction.' The engine computes per-seat types from the power/exit/directionality data; these seat gaps are the structural data the computation reads.
 *
 * DIRECTIONALITY LOGIC:
 *   Political speakers (moderate power, mobile exit) benefit from the constraint and get low d → low/negative χ. Voting public (organized, mobile) also benefit → low d. Democratic institutions (institutional power, analytical exit) set the rule and collect the institutional authority it grants → near-zero to beneficiary-end d. Targets of political speech (powerful but constrained exit, bear the cost) get high d → high χ. Non-political speakers (moderate, constrained exit by the boundary classification) get moderate-to-high d. Subordinated groups (powerless, trapped exit, structurally victimized) get highest d → highest effective extraction. No directionality override is needed; the derivation chain from beneficiary/victim declarations plus exit options should produce the right directionality for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (democracies need political speech protection against government suppression) remains live. This reading's mandate (political speech receives highest protection) has not outlived its function; courts continue to resolve hard cases by invoking it. However, there is a subordinate mandatrophy risk: the distinction between political and non-political speech, once relatively clear (political = speech about government / candidates / policy), has become increasingly ambiguous and subject to institutional discretion. Speech about race, gender, immigration, and economics can be framed as political (governance, constitutional meaning) or non-political (private grievance, artistic expression, commercial interest). The boundary-policing function has expanded and the institutional leverage it grants has grown. A mandatrophy signal would be if courts began invoking the reading's institutional authority to restrict speech without reference to the founding problem (democracy needs this protection). Currently, mandatrophy is not complete — the founding problem is still articulated — but the constraint is showing early signs: suppression_requirement and theater_ratio both rise over the interval, suggesting institutional effort to maintain the boundary is increasing and the performance of democracy-serving justification is becoming more frequent than actual analysis of whether removal of the constraint would harm self-governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_non_political_boundary,
    'Is the boundary between political and non-political speech stable, clear, and principled, or does it depend on institutional discretion and contestation?',
    'Comparative analysis of how courts and institutions draw the boundary across cases: do they apply consistent criteria or do they resolve ambiguous cases based on outcome preferences? Do dissidents appeal the boundary classification itself?',
    'If the boundary is stable and principled, the reading''s internal hierarchy is coherent and the extraction is coordinate cost. If the boundary is unstable and discretionary, institutional power to classify speech is the true constraint, and the extraction is substantially higher than metrics suggest — classifying speech as non-political becomes a tool to suppress dissent while maintaining the appearance of democratic protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_non_political_boundary, empirical, 'Whether the political/non-political boundary is a stable classification or a site of institutional discretion.').

omega_variable(
    democratic_self_governance_necessity,
    'Is maximum protection for political speech empirically necessary for democratic self-governance, or is it one defensible approach among alternatives?',
    'Comparative study of democracies with different speech protection regimes: do democracies with graduated protection for political speech (where political speech is protected but can be restricted on other grounds like dignity or order) suffer greater democratic collapse or dissent suppression than those with categorical protection? Do majorities in democratic polities actually support categorical political speech protection or do they prefer graduated protection?',
    'If categorical protection is empirically necessary, the constraint serves an irreducible democratic function and the extraction is a necessary coordination cost. If graduated protection is compatible with functional democracy, the reading''s claim to necessity is disputed and the constraint appears more extractive than coordinate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_self_governance_necessity, empirical, 'Whether categorical political speech protection is empirically necessary for democratic self-governance.').

omega_variable(
    dignity_versus_political_priority,
    'When political speech functions as systematic subordination of a group (e.g., denial of membership, dehumanization), should dignity protection override or modulate the priority given to political speech protection?',
    'Normative contestation between democratic theorists (self-governance reading vs. dignity reading) and empirical study of whether subordinated groups'' exclusion from political standing is reinforced by unconstrained political speech in this reading''s regime.',
    'If dignity should override, the reading''s hierarchy is unjust and should be revised — political speech would be constrained when it systemically demeans a group''s standing. If political speech protection should remain paramount, the reading is defensible but acknowledges a cost: subordinated groups experience the reading''s protection of political speech as weaponized against their political inclusion. This is an irreducible normative divergence between readings, not an empirical question that resolution would settle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_versus_political_priority, conceptual, 'Whether dignity interests should modulate political speech protection.').

omega_variable(
    institutional_boundary_drift,
    'Over time, does the institutional power to classify speech as political or non-political accumulate, and does this accumulation change the constraint''s function from coordination to institutional rent-seeking?',
    'Historical analysis of boundary classifications: have courts expanded or contracted what counts as political over the interval? Have institutional classifications become more frequent and discretionary? Does the theater_ratio rise suggest performative justification replacing substantive analysis?',
    'Rising institutional accumulation would support a mandatrophy signal: the constraint begins as coordinate (protecting speech necessary for democracy) and gradually becomes extractive (institutional power to classify speech). This would explain why extractiveness and theater_ratio both rise in the measurements while suppression_requirement rises: the constraint is being maintained theatrically while real institutional leverage over speech classification grows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_boundary_drift, empirical, 'Whether institutional power to classify speech is accumulating over time.').

omega_variable(
    reading_identity_fusion,
    'Do institutional actors (judges, legislators) fuse their own identity with the reading such that questioning the reading becomes unthinkable, even when evidence suggests the boundary classification is unstable?',
    'Observation of whether courts acknowledge boundary ambiguity or dismiss challenges as settled; whether alternative readings are engaged substantively or dismissed as incompatible with democracy; whether judges outside the U.S./Western framework can hold the reading without functional loss.',
    'Identity fusion (this reading IS democracy, questioning it IS anti-democratic) would explain why institutional suppression_requirement rises: defending the reading requires not just enforcing the boundary but preventing the boundary itself from being questioned. If this is true, the constraint has become partly identity-locked for institutional actors and extraction is higher than metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_fusion, conceptual, 'Whether institutional actors are identity-locked to this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__democratic_participation_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__democratic_participation_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__democratic_participation_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__democratic_participation_reading, theater_ratio, 60, 0.19).
narrative_ontology:measurement(spee_tr_t80, speech_protection_kernel__democratic_participation_reading, theater_ratio, 80, 0.18).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 60, 0.37).
narrative_ontology:measurement(spee_be_t80, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 20, 0.24).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(spee_su_t80, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 80, 0.29).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__democratic_participation_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of speech_protection_kernel. The kernel is the doctrine that speech receives legal protection; different readings instantiate different internal hierarchies and restrictions. The democratic_participation_reading gives highest protection to political expression necessary for self-governance. The absolutist_reading extends near-categorical protection to all speech. The dignity_reading makes protection conditional on not functioning as systematic subordination. The harm_threshold_reading makes protection conditional on absence of demonstrable harm. The marketplace_reading treats protection as serving truth-discovery. Each reading has different beneficiaries, victims, and extraction profiles. They coexist across jurisdictions and judicial coalitions; none logically forecloses all others, but they create structural pressure on each other. The democratic_participation reading influences the others by privileging one axis (self-governance / political necessity) and pressuring other readings to either adopt that axis or defend their alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
