% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Ritual â Hybrid Atrophy Reading
 *   domain: religious studies/collective memory/ritual practice
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_atrophy_reading of the
 *   catastrophe_memory_preservation kernel. The kernel addresses whether
 *   ritual preserves operational survival competence, pure symbolic
 *   continuity, or a hybrid that has atrophied from the former to the latter.
 *   This reading holds that ritual once encoded genuine survival competence
 *   but has decayed into mourning practice under modernity: the present
 *   generation inherits costly obligations without receiving adaptive payoff.
 *   The constraint persists as institutional inertia and identity performance
 *   rather than functional coordination. It is structurally distinct from the
 *   survival_competence_reading (which claims ongoing operational function)
 *   and the mourning_practice_reading (which sees only symbolic continuity
 *   without historical atrophy).
 *
 * KEY AGENTS:
 *   - Community elders: agenda_setter (organized/identity_locked) â administer the atrophied ritual and interpret tradition
 *   - Present-generation practitioners: payer (moderate/identity_locked) â bear inherited costs without survival-relevant return
 *   - Anthropological observers: observer (analytical) â document the gap between historical function and current performance
 *   - Secularized youth: excluded (moderate/mobile) â have exited and argue obsolescence but are absent from legitimacy discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.25).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Ritual â Hybrid Atrophy Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious studies/collective memory/ritual practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, '0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc').
narrative_ontology:cs_kernel_codification('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', distributed).
narrative_ontology:cs_authority_grounding('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', practice).
narrative_ontology:cs_interpretation_layer_present('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc').
narrative_ontology:cs_reading_relation('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', foundational, ritual_competence_has_atrophied_under_modernity).
narrative_ontology:cs_axiom_status(ritual_competence_has_atrophied_under_modernity, holdable).
narrative_ontology:cs_axiom_grounding('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', ritual_competence_has_atrophied_under_modernity, empirically_contingent).
narrative_ontology:cs_axiom('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', foundational, present_generation_inherits_obligation_without_adaptive_payoff).
narrative_ontology:cs_axiom_status(present_generation_inherits_obligation_without_adaptive_payoff, holdable).
narrative_ontology:cs_axiom_grounding('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', present_generation_inherits_obligation_without_adaptive_payoff, empirically_contingent).
narrative_ontology:cs_reference_frame('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', pre_modern_ritual_efficacy).
narrative_ontology:cs_drift_state('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', contemporary_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d560bc4-c5b7-49e5-9e7f-8e7a19dbbecc', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer catastrophe-related rituals and interpret their rules. Their authority rests on continuity with ancestral tradition. They are locked into their role by identity fusion with the community's historical narrative; abandoning the ritual would mean relinquishing their defined social purpose and status.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, community_elders, agenda_setter,
    organized, generational, identity_locked, regional).

% Inherit participation obligations in costly rituals â fasting, pilgrimage, observance â that ancestors performed for survival-relevant reasons. Under modern conditions these practices provide no adaptive threat-mitigation payoff, yet they continue due to family expectations and identity fusion. Geographic exit is possible but psychologically and socially costly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% Document the ritual's historical survival function and its current atrophied state. They observe the structural gap between encoded operational competence and present symbolic performance without being bound by community identity or obligation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, anthropological_observers, observer,
    analytical, civilizational, analytical, global).

% Have exited or reduced participation in the ritual and would argue for its obsolescence, but their voices are marginalized within the community's interpretive authority. They are not present when ritual legitimacy is publicly affirmed.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, secularized_youth, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically coordinated the intergenerational transmission of survival-relevant knowledge and threat-response capacity in catastrophe-prone environments without written records.
% TRANSFER_FUNCTION: Historically transferred survival competence from experienced generation to novice generation; now transfers social obligation and identity confirmation from elders to present generation without operational return.
% ABSENT_VOICES: Secularized youth who have exited the community and disaster-preparedness experts who would argue for functional competence over symbolic repetition are excluded from the ritual's interpretive authority.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, community identity structures would lose a key boundary marker and the social obligations binding generations would weaken; however, physical survival outcomes would be largely unchanged, revealing the constraint's atrophied state.
% FOUNDING_PROBLEM: Catastrophe preparedness and survival competence had to be preserved across generations without written records or stable institutions, in environments of irregular but lethal threat.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and historians corroborate that pre-modern catastrophe rituals encoded survival-relevant competence. Secularized community members and disaster-preparedness sociologists attest from outside the ritual-administrator circle that modern institutional systems have superseded this function. The ritual community itself claims continuity, which is precisely what makes the status contested.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is high (0.72) because the ritual now persists primarily as identity performance and symbolic repetition rather than functional competence transmission. Extractiveness is moderate (0.42) and declining over the interval because the social costs remain real but the constraint's enforcement teeth have softened into inertia and expectation. Suppression is low-moderate (0.22 at interval end) because active coercion has decayed; persistence is driven by identity lock and social habit rather than enforced compliance. Resistance is low (0.30) because opposition is diffuse and exits individually rather than organizing collectively. Accessibility_collapse is moderate (0.40) because secular alternatives exist but carry heavy social and familial switching costs.
 *
 * PERSPECTIVAL GAP:
 *   From the elder seat, the ritual is necessary continuity and community boundary maintenance; they experience it as generational duty rather than extraction. From the practitioner seat, it is inherited obligation with no commensurate return; they experience diffuse extraction through time and opportunity cost. The engine computes this divergence from the structural asymmetry: both seats are identity_locked, but the elder seat holds agenda-setting power without material capture, while the practitioner seat bears the operational costs.
 *
 * DIRECTIONALITY LOGIC:
 *   No concentrated beneficiary captures the extraction; in-group identity benefits are diffuse across the community and do not accrue to any specific seat. The present_generation_practitioners are declared as victims in base_properties, giving them high directionality (target). The community_elders have no beneficiary declaration and fall back to their power atom's canonical directionality (near-symmetric), reflecting their trapped administrative position without material profit. No directionality overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preserving survival competence across pre-literate generations facing irregular catastrophe â is dead. Modernity provides institutional disaster preparedness, literacy, and early warning systems. The ritual persists as a zombie institution (piton) because the social and identity costs of explicit termination exceed the diffuse costs of continuation. Declaring this as piton rather than snare is warranted because no party captures concentrated extraction; the agenda_setter could theoretically change the ritual but faces prohibitive social and identity costs relative to personal benefit, while the payers bear diffuse costs insufficient to motivate collective resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the catastrophe ritual a preserved survival competence mechanism, an atrophied hybrid, or a pure mourning practice?',
    'Comparative ethnographic and historical analysis of ritual form against actual threat-mitigation outcomes; comparison of pre-modern and modern ritual performance.',
    'Resolution determines whether the constraint classifies as rope or tangled_rope (survival_competence_reading), piton (hybrid_atrophy_reading), or symbolic coordination (mourning_practice_reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Structural ambiguity between three readings of the catastrophe memory preservation kernel.').

omega_variable(
    persistence_mechanism_ambiguity,
    'Is the ritual''s persistence driven by internalized identity-lock or by residual social coercion?',
    'Longitudinal study of practitioners who gain geographic or social mobility: if they maintain the ritual in isolation, persistence is identity-locked; if they drop it when community surveillance ends, it was coercive.',
    'Identity-locked persistence raises effective extraction above structural measures because practitioners internalize the constraint; coercive persistence is bounded by community boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_mechanism_ambiguity, empirical, 'Internalized versus structural suppression in atrophied ritual practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 80, 0.65).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.72).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 60, 0.27).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 80, 0.24).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 100, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, mourning_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_preservation kernel. The kernel decomposes into three structurally distinct claims: survival_competence_reading (ongoing operational function), hybrid_atrophy_reading (historical function decayed to mourning), and mourning_practice_reading (pure symbolic continuity). Each has a different epsilon, beneficiary structure, and type classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
