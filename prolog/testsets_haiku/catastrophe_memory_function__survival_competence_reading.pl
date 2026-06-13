% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Ritual Transmission of Survival-Competence for Institutional Transformation
 *   domain: religious/cultural/institutional
 *
 * SUMMARY:
 *   A constraint story analyzing ritual as transmission of institutional
 *   survival-competence — the reading that emphasizes Passover and diaspora
 *   commemoration as embodied teaching of how to maintain decentralized
 *   continuity, adaptive decision-making, and institutional resilience when
 *   centralized authority collapses. This is ONE reading of the contested
 *   kernel 'catastrophe_memory_function'; other readings
 *   (mourning_practice_reading, hybrid_transformation_reading) assign
 *   different structural weights to mourning, identity-maintenance, and
 *   survival-competence. This story isolates the survival-competence reading
 *   as its own constraint with its own ε, beneficiary structure, and
 *   stakeholder geometry. The reading is structural: it is not merely a
 *   different interpretation of the same phenomenon but asserts that the
 *   primary adaptive function the ritual solves is transmission of
 *   decentralized continuity mechanisms.
 *
 * KEY AGENTS:
 *   - tradition_bearers: identity-locked participants transmitting ritual and embedded survival-competence across generations
 *   - institutional_continuity_agents: organizations that depend on the ritual's distributed adaptive capacity for organizational resilience
 *   - younger_generation_participants: beneficiaries acquiring embodied knowledge of catastrophe-survival through ritual participation
 *   - secular_institutional_authorities: payers bearing the cost of coexisting with parallel (non-state-controlled) institutional competence centers
 *   - assimilationist_forces: excluded voices that undermine transmission by delegitimizing ritual as archaic
 *   - anthropological_observer: analytical seat assessing whether survival-competence transmission is the primary function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.15).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Ritual Transmission of Survival-Competence for Institutional Transformation").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious/cultural/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '892f8556-325a-4833-99c2-89624f811d08').
narrative_ontology:cs_kernel_codification('892f8556-325a-4833-99c2-89624f811d08', distributed).
narrative_ontology:cs_authority_grounding('892f8556-325a-4833-99c2-89624f811d08', practice).
narrative_ontology:cs_interpretation_layer_present('892f8556-325a-4833-99c2-89624f811d08').
narrative_ontology:cs_reading_relation('892f8556-325a-4833-99c2-89624f811d08', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('892f8556-325a-4833-99c2-89624f811d08', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('892f8556-325a-4833-99c2-89624f811d08', foundational, survival_competence_is_primary_function).
narrative_ontology:cs_axiom_status(survival_competence_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('892f8556-325a-4833-99c2-89624f811d08', survival_competence_is_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('892f8556-325a-4833-99c2-89624f811d08', foundational, decentralized_continuity_transmissible_through_ritual).
narrative_ontology:cs_axiom_status(decentralized_continuity_transmissible_through_ritual, holdable).
narrative_ontology:cs_axiom_grounding('892f8556-325a-4833-99c2-89624f811d08', decentralized_continuity_transmissible_through_ritual, instrumental).
narrative_ontology:cs_reference_frame('892f8556-325a-4833-99c2-89624f811d08', ritual_as_institutional_knowledge_infrastructure).
narrative_ontology:cs_drift_state('892f8556-325a-4833-99c2-89624f811d08', contemporary_assimilationist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('892f8556-325a-4833-99c2-89624f811d08', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, tradition_bearers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, institutional_continuity_agents).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).

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
 *   Extractiveness is LOW (0.22 by interval end) because the ritual is participatory, non-exclusive, and produces genuine collective competence — no party collects concentrated rents from its operation. The modest rise over the interval (0.18→0.22) reflects increasing pressure from secular modernization and institutional standardization that makes ritual transmission appear costly relative to other institutional forms, adding a small extraction layer. Suppression is very low (0.15) because the ritual relies on voluntary participation and identity-lock rather than coercive enforcement — the only suppression present is the indirect pressure from assimilationist forces delegitimizing transmission. Theater ratio is minimal (0.08) because the ritual's survival-competence function is genuinely operative (taught, learned, deployed in practice) rather than merely performed. Accessibility collapse is high (0.78) because once a community internalizes that survival depends on embodied knowledge of decentralization, alternatives (formal institutions, written manuals, state-provided continuity) appear inadequate as substitutes. Resistance is very low (0.12) because participation is identity-affirming, not coerced.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (secular institutional authorities) and the beneficiary seats (tradition-bearers) experience this constraint entirely differently. From the authorities' position, the ritual is a residual practice that competes for loyalty and transmits knowledge outside state control — an inefficiency or threat. From the beneficiary seats, the ritual is adaptive infrastructure whose loss would degrade organizational resilience. From the observer seat, the classification depends on whether the survival-competence function is empirically real (deployed successfully under catastrophe) or ceremonial. The engine computes per-seat classification; the authored metrics describe an operational transmission system, not a purely symbolic one.
 *
 * DIRECTIONALITY LOGIC:
 *   Tradition-bearers and institutional-continuity agents are structural beneficiaries — they acquire and maintain adaptive capacity without paying concentrated costs. Younger participants are also beneficiaries (they receive survival-competence knowledge) and are identity-locked (exiting means severing this transmission to their cohort). Secular institutional authorities are payers — they bear the cost of coexisting with parallel institutional competence centers they do not control. The asymmetry is not extraction-of-value but asymmetry in institutional control: the ritual operates outside state or formal-organizational hierarchies, which is a cost to centralized authorities and a benefit to distributed communities. Directionality per stakeholder: tradition-bearers d≈0.15 (strong beneficiary, low target pressure), institutional-continuity agents d≈0.25 (moderate beneficiary, institutional stake), younger participants d≈0.2 (beneficiary with identity-lock, low exit), secular authorities d≈0.75 (moderate target — they coexist with parallel competence they do not command).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophe destroys centralized authority; communities need decentralized survival-competence) remains live — diaspora, displacement, and institutional crisis remain live realities. The ritual persists because the problem persists and the ritual demonstrably solves it. No mandatrophy. However, assimilationist pressure is rising (theater_ratio creeps up in the measurement series), suggesting that over time the ritual may come to be performed for cultural-identity reasons even as the survival-competence transmission weakens. A future reading with rising theater and stable extractiveness would indicate piton drift — the ritual becoming vestigial. This reading's near-term trajectory shows it remaining operational rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_deployment_of_survival_competence,
    'Does the survival-competence transmitted by ritual actually function as adaptive institutional capacity when deployed under catastrophic pressure, or is the transmission primarily symbolic (identity-maintenance) with survival-competence as a rationalization?',
    'Comparative institutional history: do communities with intact ritual transmission demonstrate measurably faster institutional reconstruction, more resilient distributed decision-making, and better-preserved knowledge networks after catastrophe (diaspora, war, collapse) than communities without it? Oral history and testimony from survivors deployed under catastrophic pressure.',
    'If empirically deployed: the constraint is Rope (genuine coordination, low extractiveness, high accessibility-collapse of alternatives). If primarily symbolic: reclassify to mourning_practice_reading (D1/D4 emphasis) or piton (theater-ratio would rise, ε would fall or stabilize).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_deployment_of_survival_competence, empirical, 'Whether survival-competence is functionally operative or symbolically performed.').

omega_variable(
    kernel_reading_decomposition,
    'Are mourning-practice and survival-competence transmission separable functions, or does the ritual encode them as inseparable — such that the mourning-practice reading and the survival-competence reading measure the same constraint from different interpretive angles (sibling readings of one constraint) versus genuinely distinct constraints (different ε-invariance properties)?',
    'Ethnographic and textual analysis: can the ritual''s mourning-practice transmission operate independently (without survival-competence content)? Can survival-competence transmission occur without mourning-practice framing? Do institutional outcomes differ when one function is emphasized over the other?',
    'If separable: each reading is a distinct constraint with distinct ε (ε-invariance principle applies; write separate stories). If inseparable: readings are framings of one constraint; the hybrid_transformation_reading becomes the structural truth and the specialization readings are observer perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether mourning and survival-competence are structurally separable functions or interpretive frames on one constraint.').

omega_variable(
    institutional_control_coercion_boundary,
    'Does the constraint operate as Rope (genuine coordination with voluntary participation) or as Tangled Rope (coordination for some stakeholders, extraction for institutional authorities who coexist with distributed competence outside their control)?',
    'Examine the secular_institutional_authorities seat: is their ''payer'' status because they bear real costs (parallel competence threatens their authority monopoly) or because the researcher''s framing privileges their perspective? Institutional history of how states respond to distributed transmission networks — do they suppress, regulate, tolerate, or integrate?',
    'If pure Rope: extractiveness from the authorities'' perspective is zero (they are defending against diffusion of authority, not collecting extraction). If Tangled Rope from the authorities'' seat: the constraint operates as multi-type per-seat, and the survival-competence reading''s claimed Rope classification is incomplete (the same constraint is Tangled Rope for authorities). The engine computes per-seat; divergence is expected and diagnostic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_control_coercion_boundary, empirical, 'Whether the constraint is Rope across all seats or multi-type per institutional position.').

omega_variable(
    assimilationist_suppression_internalization,
    'As assimilationist pressure rises (measured indirectly through theater_ratio increase), does the suppression of ritual transmission remain structural (external delegitimization pressure) or migrate toward internalized suppression (younger generations internalizing that ritual is irrational)?',
    'Generational analysis: tracking whether exit-option classification shifts from identity_locked (participation because identity depends on it) to identity_locked_with_cognitive_reframing (participation despite internalized belief it is archaic, OR exit desire coupled with identity cost that makes actual exit impossible).',
    'If internalization occurs, the constraint drifts toward snare mechanics (extractiveness stays low but suppression rises, theater rises, resistance falls). If suppression remains structural, the constraint remains operational Rope under increasing pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assimilationist_suppression_internalization, empirical, 'Structural versus internalized suppression under assimilationist pressure.').

omega_variable(
    ritual_reading_committer_boundary,
    'This reading frames the kernel through a ''survival-competence'' lens that emphasizes institutional resilience and decentralized continuity. Is this reading itself contingent on a particular committer perspective (academic, diaspora-studies, post-colonial institutional theory) such that a different committer framing (psychological, mourning-focused, identity-centered) would generate a different survival_competence reading, or is the empirical content of what the ritual transmits observer-independent?',
    'Philosophical: does survival-competence transmission exist independent of the interpretive frame through which it is recognized, or is it a property that emerges only under certain observer framings? Check whether different committer traditions (academic anthropology, community oral history, survivor testimony) agree on whether ritual transmits survival-competence.',
    'If observer-dependent: the reading is committer-specific; sibling readings are not merely different measurements but different constitutive framings, which affects how divergence between readings is interpreted (perspectival vs. empirical). If observer-independent: readings are different measurements of observer-independent phenomena; divergence indicates real structural complexity in what the ritual encodes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_reading_committer_boundary, conceptual, 'Observer-independence of the survival-competence framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_function__survival_competence_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_function__survival_competence_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__survival_competence_reading, theater_ratio, 50, 0.09).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_function__survival_competence_reading, theater_ratio, 75, 0.11).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 15, 0.2).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 75, 0.24).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 15, 0.13).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 30, 0.14).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 50, 0.16).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 75, 0.18).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'catastrophe_memory_function'. The kernel decomposes into three structurally distinct constraints based on different ε-invariance measurements: (1) mourning_practice_reading measures primary function as identity-maintenance and boundary-norm preservation (likely lower ε, different beneficiary geometry); (2) hybrid_transformation_reading assigns equal weight to mourning and survival-competence transmission (likely moderate ε, broader beneficiary set); (3) survival_competence_reading (this constraint) isolates adaptive institutional resilience as primary function (ε 0.22, emphasis on institutional-continuity agents and distributed networks). Each reading is a separate story with its own stakeholder geometry and metrics. The constraint family is linked by affects_constraints relationships; divergence between readings is diagnostic of the kernel's interpretive contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__survival_competence_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
