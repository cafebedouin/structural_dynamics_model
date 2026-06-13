% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Risk Dominance in Energy Policy (Irreversibility Framing)
 *   domain: risk_governance/energy_policy/public_safety
 *
 * SUMMARY:
 *   Energy policy in jurisdictions facing simultaneous climate urgency and
 *   nuclear waste stewardship must choose a risk metric: expected-value
 *   optimization (probability × consequence), comparative risk (coal deaths
 *   vs. nuclear accidents vs. climate catastrophe), or catastrophic-tail
 *   dominance (irreversibility and intergenerational burden override
 *   probabilistic discounting). This story instantiates the
 *   catastrophic-tail-dominant reading of the contested kernel
 *   'acceptable_risk_for_energy.' Under this reading, nuclear waste becomes a
 *   multi-generational injustice regardless of accident probability, and
 *   fossil alternatives become less justifiable because carbon emissions are
 *   also irreversible on geological timescales—yet the tail-risk frame
 *   suppresses the claim that comparable irreversibility should apply to
 *   both. The constraint coordinates climate advocacy and
 *   intergenerational-ethics language while extracting burden (siting risk,
 *   waste stewardship, occupational exposure) from host communities, future
 *   generations, and workers who bear the named tail risks without
 *   participating in the framing choice.
 *
 * KEY AGENTS:
 *   - zero_carbon_transition_advocates: Beneficiary and agenda-setter. Sets the tail-risk criterion; benefits from legitimacy and policy momentum. Uses catastrophic framing to justify nuclear expansion while constraining siting.
 *   - climate_risk_reduction_constituency: Beneficiary. Gains rhetorical parallel between climate and nuclear irreversibility; supports the tail-risk frame because it makes climate urgency sound as permanent as radionuclide hazard.
 *   - nuclear_host_communities: Payer and structurally excluded. Bear siting risk and reputational stigma. The tail-risk frame validates their concerns in rhetoric but not in decision-making—they remain trapped and voiceless.
 *   - future_generations_waste_custodians: Payer, identity-locked by temporal asymmetry. Named as primary victims of irreversibility; cannot participate in the risk trade-off. Inherit 24,000-year stewardship burden.
 *   - waste_management_workers: Payer, constrained exit. Operate repositories and reprocessing plants. Bear occupational exposure and institutional accountability for tail-risk prevention without additional authority or resources.
 *   - energy_policy_authorities: Agenda-setter. Enforce the tail-risk frame through licensing standards, siting procedures, multi-generational impact assessments. Suppress alternative frames (expected-value, comparative-risk) through procedural gatekeeping.
 *   - expected_value_risk_analysts: Excluded and systematically subordinated. Their voice is admitted but their framing is treated as incomplete; tail-risk analysis is required alongside probabilistic assessment and takes decision-precedence.
 *   - comparative_risk_advocates: Excluded and actively suppressed. Arguments that nuclear risk is acceptable relative to coal and climate are labeled reductive or insufficiently precautionary. Their framing is barred from decision-making forums.
 *   - repository_site_neighbors: Payer. Carry localized tail-risk (groundwater contamination, thermal effects, human intrusion). Beneficiaries of precautionary rhetoric; bear all concentrated costs without resources to monitor or relocate.
 *   - climate_scientists: Beneficiary and analytical observer. Use catastrophic-tail language for climate communication. Benefit from the rhetorical parallel without taking direct positions on nuclear-risk assessment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.79).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Risk Dominance in Energy Policy (Irreversibility Framing)").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_governance/energy_policy/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, 'b7c81dfe-7b7e-4c3a-b917-7b8283e32865').
narrative_ontology:cs_kernel_codification('b7c81dfe-7b7e-4c3a-b917-7b8283e32865', distributed).
narrative_ontology:cs_authority_grounding('b7c81dfe-7b7e-4c3a-b917-7b8283e32865', distributed).
narrative_ontology:cs_reading_relation('b7c81dfe-7b7e-4c3a-b917-7b8283e32865', acceptable_risk_for_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('b7c81dfe-7b7e-4c3a-b917-7b8283e32865', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('b7c81dfe-7b7e-4c3a-b917-7b8283e32865', foundational, irreversibility_axiomatically_overrides_probability).
narrative_ontology:cs_axiom_status(irreversibility_axiomatically_overrides_probability, holdable).
narrative_ontology:cs_axiom_grounding('b7c81dfe-7b7e-4c3a-b917-7b8283e32865', irreversibility_axiomatically_overrides_probability, deontological).
narrative_ontology:cs_axiom('b7c81dfe-7b7e-4c3a-b917-7b8283e32865', foundational, intergenerational_burden_principle).
narrative_ontology:cs_axiom_status(intergenerational_burden_principle, holdable).
narrative_ontology:cs_axiom_grounding('b7c81dfe-7b7e-4c3a-b917-7b8283e32865', intergenerational_burden_principle, deontological).
narrative_ontology:cs_created_at('b7c81dfe-7b7e-4c3a-b917-7b8283e32865', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, zero_carbon_transition_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_risk_reduction_constituency).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations_waste_custodians).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, waste_management_workers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers decision authority from probabilistic analysts to tail-risk framers, and concentrates burden on host communities and future generations who cannot negotiate the terms. The constraint is not pure extraction—it does coordinate a genuine problem (how to weigh irreversible harms)—but the coordination is asymmetric: beneficiaries include advocates who use the frame instrumentally while victims include future people who have no voice. Suppression is high (0.79) because the constraint's persistence requires active procedural gatekeeping to exclude expected-value and comparative-risk framings from decision forums. Theater is moderate (0.42) and rising: the tail-risk analysis is functionally real (licensing does take multi-generational impacts seriously), but a rising share of suppression effort goes to rhetorical management—framing debates about whether irreversibility is the right metric rather than merely applying it. Accessibility collapse is high (0.71): once host communities understand the tail-risk frame, the framing becomes nearly unavoidable in policy discourse, even when they disagree with it. The measurement series shows extractiveness rising steeply from 0-20 years (as the climate urgency and intergenerational-ethics language solidifies), then plateauing from 20-40 years (the frame is now institutionalized; suppression cost stabilizes). Theater ratio rises throughout, indicating increasing energy devoted to defending the frame against alternative metrics rather than merely applying it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (energy policy authorities, zero-carbon advocates) and the payer seats (host communities, future generations) should compute dramatically different types from the same structural data. From the beneficiary side, the constraint solves the genuine problem of comparing irreversible harms and appropriately weights intergenerational burden. From the payer side, the constraint is extractive suppression masquerading as ethical precaution. From the excluded side (expected-value analysts, comparative-risk advocates), the constraint is a category error—treating one metric as axiomatically superior without justifying why other metrics should be abandoned. The engine computes these divergences from the structural asymmetries: beneficiaries have high d→low χ (they shape the frame and reap legitimacy); victims have high d→high χ (they bear concentrated burden); excluded have moderate d reflecting their admission but subordination. The authored claim (tangled_rope) reflects the constraint's genuine coordination function (weighing incomparable harms) while the metrics show its extractive operation (suppression of alternative frames, concentration of burden on voiceless parties).
 *
 * DIRECTIONALITY LOGIC:
 *   Zero-carbon advocates and climate researchers benefit from the tail-risk frame: it gives them an irreversibility parallel to climate catastrophe and it constrains nuclear expansion in ways that force energy-system decarbonization through renewables and efficiency rather than nuclear expansion alone. Their directionality is low (beneficiaries, d ~0.2). Nuclear host communities, future-generation custodians, and waste workers are the constraint's named victims: they carry the tail risks without choosing the metric by which those risks became central. Their directionality is high (targets, d ~0.8–0.9). Energy policy authorities sit near the beneficiary end despite officially being neutral: they administer the frame and their institutional authority is reinforced by the complexity of multi-generational impact assessment. Expected-value and comparative-risk analysts are moderately high-d (0.55–0.65) because their framing is systematically subordinated in decision forums—they are not victims in the sense of bearing catastrophic tail risk, but their intellectual standing is extracted by requiring their work while suppressing its conclusions. Repository-site neighbors and waste workers have identity-locked exit (no relocation option, occupational identity fused with nuclear enterprise), which amplifies their effective d toward 0.9.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: how to weigh low-probability catastrophe against climate catastrophe when both carry irreversible harms on different timescales. The catastrophic-tail-dominant constraint solves this by making irreversibility the primary decision criterion, which initially aligned coordination (everyone agrees nuclear waste is a multi-generational burden) with extraction (the framing enabled nuclear expansion while suppressing siting opposition). Over the 40-year interval, the founding problem has shifted status from 'live' to 'partially dead, partially zombie.' The founding problem of comparing incommensurable catastrophes remains live in climate science and intergenerational ethics. But the operating problem—whether tail-risk framing should dominate policy metrics—has become institutionalized in a way that resembles mandatrophy: the original decision rationale (irreversibility is important) has decoupled from the current operating function (suppressing alternative risk frames to enable nuclear expansion). Host communities that initially invoked the tail-risk frame to oppose siting now find the frame being used to force their acceptance of sites despite their objections. The constraint persists partly because dismantling it would require energy policy authorities to adopt explicit criteria for choosing between risk metrics—a politically difficult conversation that neither beneficiaries nor authorities want to have. The theater ratio's rise reflects this: more effort goes to defending the tail-risk frame as axiomatically ethical, less effort goes to applying it consistently across nuclear and climate decisions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_risk_vs_expected_value_boundary,
    'Is low-probability high-consequence risk structurally incommensurable with expected-value optimization, or is it a special case of probabilistic weighting where the weight is non-linear in tail severity?',
    'Formal decision theory analysis: show whether tail-risk dominance can be derived from a utility function without explicit irreversibility axioms (nonlinear utility), or whether it requires adding irreversibility as an independent criterion outside probabilistic frameworks.',
    'If tail-risk is mathematically derivable from utility theory, the constraint''s suppression of expected-value framing is Goodhart drift (pursuing one metric while disguising it as a different one). If truly independent, the constraint is a genuine conflict between incommensurable frameworks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tail_risk_vs_expected_value_boundary, conceptual, 'Whether catastrophic-tail dominance is a distinct decision criterion or a repackaging of nonlinear utility.').

omega_variable(
    reading_specific_kernel_ambiguity,
    'Does the ''acceptable_risk_for_energy'' kernel admit three genuinely distinct readings (catastrophic_tail_dominant, expected_value_dominant, comparative_risk_dominant), or are these readings of overlapping but distinct kernels (acceptable_risk? risk_comparison_metric? temporal_discounting_of_harm?)?',
    'Examine whether all three readings share the SAME core commitment (what counts as acceptable energy-system risk) and differ only in the metric for acceptability. If they differ in the commitment itself—what acceptability means—they are readings of different kernels.',
    'If truly sibling readings of one kernel, the constraint enforces one reading''s metric against others that are reading the same kernel differently. If they are readings of different kernels, the suppression is across-kernel contamination and the whole network structure is different.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specific_kernel_ambiguity, conceptual, 'Whether catastrophic_tail_dominant is a reading of a single kernel or a distinct kernel.').

omega_variable(
    intergenerational_voice_identity_lock,
    'Is the identity-locking of future-generations as custodians a form of structural suppression (they cannot exit), or an inescapable feature of temporal asymmetry (future people do not exist yet to negotiate terms)?',
    'Examine proxy-representation mechanisms in energy policy: do future-generation advocates have decision-making authority equivalent to present parties, or are they perpetual passive victims of present choices? If authority is absent despite proxy advocacy, suppression is real. If authority is structurally impossible, the lock is ontological, not extractive.',
    'If suppression is real, the constraint extracts burden from parties who cannot defend themselves. If ontological, the lock is not itself an extraction mechanism, though it enables extraction by making future interests non-negotiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_voice_identity_lock, empirical, 'Whether future-generation identity-lock is suppressive or ontological.').

omega_variable(
    irreversibility_vs_reversibility_metrics,
    'What counts as ''irreversible'' under this constraint? Is geological timescale (plutonium half-life 24,000 years) categorically irreversible, while climate-carbon-cycle timescales (centuries) reversible? Or are both irreversible from the perspective of human institution-building (centuries >> institutional stability)?',
    'Examine how the constraint treats climate impacts (rising sea levels, permafrost thaw, ecosystem collapse): are these treated as reversible-in-principle (if we stop emissions), or irreversible-in-practice (committed warming, lag effects)? Consistency check reveals whether irreversibility is a threshold (yes/no) or a spectrum (timescale of reversal relative to human timescales).',
    'If irreversibility is a spectrum, both nuclear and climate carry irreversible components on human timescales, which undermines the categorical distinction the constraint depends on.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irreversibility_vs_reversibility_metrics, empirical, 'Whether irreversibility is categorical or spectral.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of expected-value and comparative-risk framing structural (procedural rules, evidentiary standards, gatekeeping) or internalized (advocates of other frames internalize the tail-risk frame as axiomatically more ethical, so suppression persists post-decision)?',
    'Test: would expected-value analysts continue to use tail-risk framing if procedural suppression were lifted? Post-suppression behavior: do communities that oppose siting based on tail-risk concerns maintain that framing if the tail-risk constraint is removed?',
    'If structural, removing the procedural enforcement would allow alternative frames to re-emerge. If internalized, the constraint''s extractive power persists through its victims'' own judgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Suppression mechanism structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.2).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 5, 0.25).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.3).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 15, 0.35).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.38).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 25, 0.41).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 30, 0.42).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, global_infrastructure).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_repository_siting).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_catastrophe_risk_weighting).

% DUAL FORMULATION NOTE:
% This constraint (catastrophic_tail_dominant) is one reading of the kernel 'acceptable_risk_for_energy' alongside expected_value_dominant and comparative_risk_dominant. The three readings differ in what criterion determines acceptability: irreversibility and intergenerational burden (this reading), probabilistic expected value (sibling 1), or comparative risk relative to alternatives (sibling 2). All three readings address the same institutional problem (how to choose energy technologies in the face of multiple irreducible uncertainties) but arrive at incommensurable conclusions about what trade-offs are permissible. The catastrophic_tail_dominant reading structurally influences the other two by changing what counts as relevant evidence and by shifting procedural burden (now multi-generational impact assessment is mandatory). See acceptable_risk_for_energy__expected_value_dominant and acceptable_risk_for_energy__comparative_risk_dominant for the sibling readings and their respective directionality and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, powerless, 0.88).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
