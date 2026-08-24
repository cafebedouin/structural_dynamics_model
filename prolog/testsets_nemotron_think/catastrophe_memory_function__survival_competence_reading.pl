% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Catastrophe Memory as Survival Competence Transmission (D5)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint models the survival_competence_reading of the
 *   catastrophe_memory_function kernel — the claim that commemorative ritual
 *   (exemplified by Passover as D5) primarily functions to transmit adaptive
 *   capacity for surviving catastrophe through embodied rehearsal and
 *   knowledge transmission, enabling institutional transformation and
 *   decentralized continuity. The reading treats the ritual as a coordination
 *   mechanism that solves the problem of preserving survival knowledge across
 *   generations without writing or stable institutions. Extraction is low but
 *   non-zero: institutional authorities capture legitimacy rents, and
 *   marginalized practitioners bear compliance costs. The measurement series
 *   shows gradual extraction accumulation and theater creep as institutions
 *   co-opt the ritual's coordination function over the interval.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.35).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.25).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Catastrophe Memory as Survival Competence Transmission (D5)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '8b83cef8-d890-49d5-a8e0-33add2ab19ef').
narrative_ontology:cs_kernel_codification('8b83cef8-d890-49d5-a8e0-33add2ab19ef', distributed).
narrative_ontology:cs_authority_grounding('8b83cef8-d890-49d5-a8e0-33add2ab19ef', lineage).
narrative_ontology:cs_interpretation_layer_present('8b83cef8-d890-49d5-a8e0-33add2ab19ef').
narrative_ontology:cs_reading_relation('8b83cef8-d890-49d5-a8e0-33add2ab19ef', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b83cef8-d890-49d5-a8e0-33add2ab19ef', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('8b83cef8-d890-49d5-a8e0-33add2ab19ef', foundational, ritual_transmits_survival_competence).
narrative_ontology:cs_axiom_status(ritual_transmits_survival_competence, holdable).
narrative_ontology:cs_axiom_grounding('8b83cef8-d890-49d5-a8e0-33add2ab19ef', ritual_transmits_survival_competence, empirically_contingent).
narrative_ontology:cs_axiom('8b83cef8-d890-49d5-a8e0-33add2ab19ef', secondary, decentralized_continuity_requires_embodied_rehearsal).
narrative_ontology:cs_axiom_status(decentralized_continuity_requires_embodied_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('8b83cef8-d890-49d5-a8e0-33add2ab19ef', decentralized_continuity_requires_embodied_rehearsal, empirically_contingent).
narrative_ontology:cs_reference_frame('8b83cef8-d890-49d5-a8e0-33add2ab19ef', diasporic_survival_transmission).
narrative_ontology:cs_drift_state('8b83cef8-d890-49d5-a8e0-33add2ab19ef', contemporary_institutional_stability, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8b83cef8-d890-49d5-a8e0-33add2ab19ef', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, practitioner_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, descendant_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, marginalized_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, marginalized_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, adaptive_capacity_transmission_through_ritual).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, decentralized_continuity_via_embodied_rehearsal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that perform the commemorative ritual gain embodied survival knowledge — how to navigate displacement, scarcity, and institutional collapse. They invest time and interpretive labor in the ritual but receive adaptive capacity that has historically enabled diaspora survival. Exit means losing the primary intergenerational transmission vehicle for this competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, practitioner_communities, beneficiary,
    organized, generational, constrained, regional).

% Religious and communal authorities curate the ritual canon, authorize interpreters, and regulate performance standards. They benefit from the legitimacy and cohesion the ritual generates, which sustains their institutional position. They can modify the ritual within tradition-bound parameters and have exit options through institutional reform or schism.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, institutional_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).

% Members within practitioner communities who bear disproportionate compliance costs — gendered ritual obligations, economic costs of observance, or interpretive conformity demands — while still receiving the adaptive capacity the ritual transmits. Their exit is constrained by communal belonging and identity fusion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, marginalized_practitioners, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, marginalized_practitioners, beneficiary).

% Anthropologists, historians, and theorists who study the ritual as a cultural transmission system. They analyze its adaptive function without being subject to its normative demands. Their analytical seat sees the full structural pattern across communities and epochs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% Individuals who reject the ritual's normative framework — secular descendants, trauma survivors for whom re-enactment compounds harm, or theological dissenters — but remain socially embedded in communities where the ritual structures belonging. They would object to the ritual's claim to transmit universal survival competence but have no voice in its governance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, excluded_dissidents, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits adaptive survival competence through embodied rehearsal, enabling communities to navigate catastrophe and maintain decentralized continuity across generations without centralized institutions or stable material infrastructure.
% TRANSFER_FUNCTION: Moves embodied knowledge, procedural memory, and normative frameworks from elders to initiates through ritual performance, at the cost of compliance time, interpretive conformity, and the exclusion of dissenting voices from the transmission chain.
% ABSENT_VOICES: Dissident practitioners who reject the ritual's normative framework but remain subject to its social enforcement; trauma survivors for whom the ritual re-enacts rather than resolves catastrophe; secular descendants who inherit the ritual's social expectations without its meaning.
% DISAPPEARANCE_RATIONALE: If the survival-competence transmission function vanished, communities would lose a primary vehicle for intergenerational catastrophe preparedness, forcing improvisation of new transmission mechanisms or suffering degraded adaptive response when catastrophe recurs. The decentralized continuity it enables would fracture.
% FOUNDING_PROBLEM: How to preserve and transmit catastrophe-survival knowledge across generations without centralized institutions, writing, or stable material infrastructure — a problem faced by diasporic, displaced, and stateless peoples.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of oral traditions (Goody, Ong), historical analyses of Jewish diaspora survival (Yerushalmi), and disaster studies of community resilience (Aldrich) corroborate from outside the beneficiary set that ritual embodies adaptive knowledge. The beneficiary communities themselves attest the problem remains live; external scholarship confirms the structural function.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.35) reflects that while the ritual's core function is coordination (transmitting survival competence), institutional authorities extract legitimacy and control rents, and compliance costs fall unevenly. Suppression (0.25) is normative — social enforcement of participation — not coercive; alternatives (secular preparedness, written records) exist but lack the ritual's embodied transmission fidelity. Theater ratio (0.3) captures the growing performative layer: as the ritual's practical survival relevance diminishes in stable periods, its symbolic and identity-marking performances expand. Accessibility collapse (0.45) is moderate: the ritual is the primary but not exclusive transmission vehicle; written texts and oral teachings provide partial alternatives. Resistance (0.3) reflects ongoing interpretive contests within communities, not rejection of the ritual's function.
 *
 * PERSPECTIVAL GAP:
 *   The practitioner community seat experiences this as a rope — genuine coordination they voluntarily maintain. The marginalized practitioner seat experiences a tangled_rope — coordination mixed with asymmetric extraction (gendered obligations, economic costs). The excluded dissident seat experiences a snare — the ritual's normative force extracts conformity without delivering adaptive benefit to them. The institutional authority seat experiences a scaffold — they administer a transitional mechanism (until writing/institutions stabilize) but have incentives to prolong it. The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioner communities are structural beneficiaries (d ~ 0.2): they receive adaptive capacity that has historically enabled survival, though they invest compliance labor. Institutional authorities are agenda_setters with arbitrage-grade exit (d ~ 0.1): they capture legitimacy rents and can reform or exit the tradition. Marginalized practitioners are payers with constrained exit (d ~ 0.7): they bear disproportionate compliance costs while still receiving the adaptive benefit. Excluded dissidents are trapped (d ~ 0.9): they bear social costs of non-participation without the adaptive benefit. Ritual scholars are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting survival knowledge without stable infrastructure) remains live for diasporic and displaced communities, but for settled communities with writing and institutions, the ritual's original mandate has atrophied. The persistence of the ritual in settled contexts shows mandatrophy: the coordination function has been partially displaced by texts and institutions, but the ritual persists through identity-coordination and institutional self-preservation. The classification as rope (not piton) holds because the coordination function remains genuinely active for the core beneficiary communities, and the extraction has not yet overwhelmed the coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the catastrophe_memory_function kernel a single persisting commitment with multiple readings, or are these structurally distinct constraints that share only a colloquial label?',
    'Apply the ε-invariance test: if measuring the constraint via mourning-practice observables yields a different ε than survival-competence observables, they are distinct constraints. The BGS worked example (spectral universality vs eigenvector thermalization) is the model.',
    'If distinct constraints, each reading gets its own ε, stakeholders, and classification linked by network.affects_constraints. If single kernel, the readings are perspectival framings of one constraint and the engine''s per-seat computation handles the divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel decomposition follows ε-invariance or represents perspectival framing of one constraint.').

omega_variable(
    coordination_extraction_boundary,
    'Does the ritual''s institutional capture (legitimacy rents, compliance enforcement) constitute asymmetric extraction that makes this a tangled_rope rather than a rope?',
    'Measure whether the coordination function would persist without the extraction layer — i.e., if institutional authorities lost their legitimacy rents but the ritual continued unchanged, would practitioner communities still maintain it for its adaptive function?',
    'If extraction is separable and the coordination function is self-sustaining, rope classification holds. If extraction is structurally necessary for the coordination to persist (e.g., institutions fund the transmission), tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the measured extraction is overhead on a self-sustaining coordination or structurally necessary for the coordination''s persistence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (normative enforcement of participation) structural (communal sanctions, material dependency) or internalized (identity fusion, belief that non-participation betrays ancestors)?',
    'Post-exit suppression trajectory: track dissidents who leave the community — if normative pressure persists after structural barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, which amplifies χ for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ritual participation norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmf_scr_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(cmf_scr_tr_t0, observed).
narrative_ontology:measurement(cmf_scr_tr_t25, catastrophe_memory_function__survival_competence_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(cmf_scr_tr_t25, observed).
narrative_ontology:measurement(cmf_scr_tr_t50, catastrophe_memory_function__survival_competence_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement_basis(cmf_scr_tr_t50, observed).
narrative_ontology:measurement(cmf_scr_tr_t75, catastrophe_memory_function__survival_competence_reading, theater_ratio, 75, 0.28).
narrative_ontology:measurement_basis(cmf_scr_tr_t75, observed).
narrative_ontology:measurement(cmf_scr_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(cmf_scr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cmf_scr_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(cmf_scr_be_t0, observed).
narrative_ontology:measurement(cmf_scr_be_t25, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement_basis(cmf_scr_be_t25, observed).
narrative_ontology:measurement(cmf_scr_be_t50, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement_basis(cmf_scr_be_t50, observed).
narrative_ontology:measurement(cmf_scr_be_t75, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 75, 0.33).
narrative_ontology:measurement_basis(cmf_scr_be_t75, observed).
narrative_ontology:measurement(cmf_scr_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement_basis(cmf_scr_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(cmf_scr_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(cmf_scr_su_t0, observed).
narrative_ontology:measurement(cmf_scr_su_t25, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(cmf_scr_su_t25, observed).
narrative_ontology:measurement(cmf_scr_su_t50, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement_basis(cmf_scr_su_t50, observed).
narrative_ontology:measurement(cmf_scr_su_t75, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 75, 0.23).
narrative_ontology:measurement_basis(cmf_scr_su_t75, observed).
narrative_ontology:measurement(cmf_scr_su_t100, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 100, 0.25).
narrative_ontology:measurement_basis(cmf_scr_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__survival_competence_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the survival_competence_reading (D5) of the catastrophe_memory_function kernel. The mourning_practice_reading (D1/D4) and hybrid_transformation_reading (D1/D4+D5) are sibling constraints. All three share the kernel's referent (commemorative ritual's function) but author different ε values and beneficiary/victim structures. This reading's ε (0.35) is lower than the hybrid's expected ε (which includes both functions) and differs in victim structure from the mourning reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__survival_competence_reading, organized, 0.2).
constraint_indexing:directionality_override(catastrophe_memory_function__survival_competence_reading, moderate, 0.7).
constraint_indexing:directionality_override(catastrophe_memory_function__survival_competence_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
