% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Catastrophe Memory Function — Mourning-Practice Reading (Commemorative Ritual Maintains Group Identity)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   Tisha B'Av and commemorative mourning rituals in Jewish tradition serve
 *   as the kernel case. This constraint story instantiates ONE of three
 *   possible readings: the mourning-practice reading holds that the ritual's
 *   primary function is to preserve the community's capacity to grieve
 *   collectively, maintain boundaries between the community and the outside
 *   world, and transmit the obligation to remember catastrophic loss. The
 *   kernel is contested because two other readings also claim structural
 *   legitimacy: the hybrid_transformation_reading argues that ritual
 *   transmits BOTH mourning-practice AND survival-competence (adaptive
 *   mechanisms for institutional continuity under persecution); the
 *   survival_competence_reading argues that ritual PRIMARILY transmits
 *   survival-capacity and institutional flexibility, with mourning as the
 *   framing. This story generates the mourning-practice reading only, with
 *   clean ε-invariance (extractiveness 0.31, low suppression, high
 *   theater_ratio as consensus-maintenance performance). The three readings
 *   are not equivalent interpretations of the same constraint — they have
 *   different ε values and different structural effects. The kernel contest
 *   is resolved by empirical analysis of the ritual's actual transmission
 *   content and community function, not by interpretive preference.
 *
 * KEY AGENTS:
 *   - ritual_practitioners: Participants in Tisha B'Av observance, fasting, liturgical recitation, collective mourning — sustain the constraint through participation
 *   - religious_authority_institutions: Rabbinical academies, denominational bodies, published liturgical standards — set and maintain canonical mourning-practice readings
 *   - community_continuity_mechanism: The collective identity that persists through ritual participation and memory transmission — the 'beneficiary' is not a human actor but a system property
 *   - younger_generation: Receive the mourning-obligation and identity-boundary from older practitioners — their participation renews the constraint's authority
 *   - secular_or_non-observant_descendants: Would benefit from the constraint if they participated; exit via non-participation or reframing
 *   - rival_interpretations (survival_competence and hybrid_transformation readings): Constitute the reading contest; coexist with this reading in some communities, foreclose it in others
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.31).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.18).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Catastrophe Memory Function — Mourning-Practice Reading (Commemorative Ritual Maintains Group Identity)").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, '750dfedd-05f4-4944-803e-df7e9f09abfe').
narrative_ontology:cs_kernel_codification('750dfedd-05f4-4944-803e-df7e9f09abfe', fixed_text).
narrative_ontology:cs_authority_grounding('750dfedd-05f4-4944-803e-df7e9f09abfe', lineage).
narrative_ontology:cs_interpretation_layer_present('750dfedd-05f4-4944-803e-df7e9f09abfe').
narrative_ontology:cs_reading_relation('750dfedd-05f4-4944-803e-df7e9f09abfe', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('750dfedd-05f4-4944-803e-df7e9f09abfe', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('750dfedd-05f4-4944-803e-df7e9f09abfe', foundational, mourning_as_primary_function).
narrative_ontology:cs_axiom_status(mourning_as_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('750dfedd-05f4-4944-803e-df7e9f09abfe', mourning_as_primary_function, deontological).
narrative_ontology:cs_axiom('750dfedd-05f4-4944-803e-df7e9f09abfe', foundational, identity_preservation_through_commemoration).
narrative_ontology:cs_axiom_status(identity_preservation_through_commemoration, holdable).
narrative_ontology:cs_axiom_grounding('750dfedd-05f4-4944-803e-df7e9f09abfe', identity_preservation_through_commemoration, conventional).
narrative_ontology:cs_reference_frame('750dfedd-05f4-4944-803e-df7e9f09abfe', collective_mourning_authority).
narrative_ontology:cs_drift_state('750dfedd-05f4-4944-803e-df7e9f09abfe', contemporary_secularization_and_holocaust_centrality, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('750dfedd-05f4-4944-803e-df7e9f09abfe', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, community_continuity_through_memory).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, ritual_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.31, rising to 0.31 over the interval) is LOW because the constraint's operation is primarily a coordination function — collective mourning is a genuine shared good that requires participation from multiple agents; no single agent extracts a disproportionate benefit. The theater_ratio (0.72, rising over the interval) is HIGH because the ritual's primary mechanism is performative: the point IS to ritualize, to make mourning visible and shared, to enact collective identity through formalized action. This is not theatrical in the sense of being false — mourning is genuinely performed and genuinely felt — but rather in the sense that the ritual's FORM and PERFORMANCE are the mechanism itself, not a side effect. Suppression (0.18) is LOW because participation is maintained primarily through identity-fusion and community consensus, not through coercion or legal enforcement. Accessibility_collapse (0.64) is MODERATE-HIGH: once you understand the ritual's boundary-maintenance function, alternatives (secular memorialization, private grief, non-participation) are visible but carry social/identity costs; you cannot be a practicing member of the tradition without the ritual, but you can exit by leaving the tradition. Resistance (0.42) is MODERATE: some participants experience the constraint as burdensome, some resist the obligation, some reframe or selectively participate — the ritual is not universally accepted without friction, but the friction is managed through debate about how to mourn, not through organized opposition to mourning itself.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a ritual practitioner: the constraint is Rope, a genuine coordination mechanism that solves a real problem (how to grieve collectively, how to maintain community identity through memory). From the seat of a secular descendant: the constraint is closer to Tangled Rope or even Snare — the tradition extracts participation (identity-lock exit) but the coordination benefit is unclear to the outsider. From the seat of a religious authority: the constraint is essential to institutional continuity. From the seat of a rival reading (survival_competence or hybrid): this reading is incomplete because it treats the ritual as pure mourning-practice when it also (or primarily) transmits survival-competence. The engine computes these per-seat differences; the authoring commentary explains why they arise from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The named beneficiary is not a person but a system property — 'community_continuity_through_memory' and 'ritual_practitioners' collect the constraint's benefit. From the perspective of a practicing community member, directionality is near symmetric (d ~0.5): they participate in mourning, they receive the identity and the shared grief, they contribute to the collective memory, they bear the time-cost of ritual. For a descendant or new member, directionality is slightly weighted toward benefit (d ~0.4): the ritual is presented as a gift, an inheritance, an obligation that confers membership. For a secular descendant or non-believer, directionality approaches target (d ~0.7): the ritual is an identity-trap, a behavioral expectation, a cost without perceived coordination benefit. The engine derives these per-seat directionalities from the beneficiary/victim structure and exit-option analysis; this commentary explains why the same constraint produces different d values across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was the need to process and memorialize catastrophic loss (destruction of the Temple, persecution across diaspora) in a way that sustained group identity across generations without geographic continuity or political sovereignty. The founding problem is LIVE (catastrophe memory remains necessary for tradition-continuity) but CONTESTED (some argue the founding problem is obsolete because the immediate context has changed, or because survival-competence rather than mourning-practice is the primary need). No mandatrophy is present under this reading — the ritual's function remains coherent with its founding purpose, though what counts as 'proper mourning' is debated. A mandatrophy would emerge if the ritual persisted primarily for theatrical/identity reasons after the founding function (processing catastrophic loss) became irrelevant — but that is not the case here because the losses commemorated (historical and ongoing) remain part of the tradition's self-understanding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mourning_vs_pragmatism_reading_boundary,
    'Does ritual function PRIMARILY as mourning-practice and identity-maintenance (this reading), or does it ALSO transmit survival-competence and adaptive mechanisms (hybrid_transformation_reading)? Are these functions separable or inseparable?',
    'Ethnographic and historical analysis: interview practitioners about the explicit purpose they assign to ritual; trace whether adaptive mechanisms (decentralized leadership, resource-sharing norms, institutional flexibility) are taught through ritual or transmitted separately; examine whether communities that maintain mourning-ritual without survival-knowledge transmission show differential institutional durability.',
    'If mourning is primary and survival-competence is incidental or absent, the reading''s ε remains low (~0.31) and the constraint is pure Rope. If survival-competence is systematic and foundational, ε should be reclassified upward and the hybrid_transformation_reading''s framing is more accurate. The Boltzmann floor and complexity offset shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mourning_vs_pragmatism_reading_boundary, empirical, 'Whether ritual transmits survival-competence as a core or incidental function.').

omega_variable(
    identity_fusion_mechanism,
    'Does the mourning-ritual''s enforcement mechanism rely on internalized identity-fusion (the practitioner''s self-concept is constituted through ritual participation and group membership) or on structural/social pressure (community enforcement, institutional authority)?',
    'Post-exit ethnography: interview former practitioners about what persists when they leave the community; measure identity-lock directionality via exit-option analysis; compare identity-fusion rhetoric in ritual prescriptions with actual material consequences of non-participation.',
    'If identity-fusion is primary, exit_options for some seats shift from constrained to identity_locked; directionality changes; the constraint''s suppression should be re-measured to clarify internalized vs. structural components. This feeds into whether ritual operates via consensus (low suppression, high theater_ratio as consensus maintenance) or coercion (higher suppression). The reading''s theater_ratio of 0.72 suggests consensus-maintenance dominance; high identity-fusion would support that framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_fusion_mechanism, empirical, 'Internalized identity-fusion vs. structural enforcement in ritual participation.').

omega_variable(
    functional_obsolescence_kernel_reading_divergence,
    'This reading treats the kernel (Tisha B''Av and Holocaust memorialization in Jewish tradition) as instantiating PURE D1/D4 mourning-practice. But is the kernel actually instantiating a HYBRID of mourning-practice AND survival-competence transmission (hybrid_transformation_reading)? Which reading captures the actual structure of the ritual system?',
    'Textual analysis of Tisha B''Av liturgy and commemorative practice across multiple Jewish communities: catalogue which texts explicitly teach survival-relevant adaptations (decentralized governance, statelessness coping, diaspora resilience, institutional continuity under persecution) vs. which teach mourning and memorial obligation only. Distinguish between (a) explicitly taught survival-competence, (b) implicitly modeled adaptive mechanisms, (c) purely memorial/boundary-maintenance function. Compare the proportion of liturgical content and ritual time devoted to each domain.',
    'If the kernel instantiates substantial survival-competence teaching alongside mourning, this reading (mourning_practice_reading) is incomplete, and the hybrid_transformation_reading''s ε and classification are more accurate. If mourning is genuinely primary and survival-competence is not systematic, this reading''s framing is confirmed. The three readings are not equivalent framings of the same constraint — they instantiate different ε values and different constraint types (this reading: Rope; survival_competence_reading: Scaffold or Tangled Rope; hybrid_transformation_reading: Rope with higher ε). The kernel contest is resolved by empirical analysis of the ritual''s actual content and transmission structure, not by interpretive preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_obsolescence_kernel_reading_divergence, empirical, 'Which of the three readings (mourning-only vs. hybrid vs. survival-competence-only) captures the actual kernel structure and transmission function.').

omega_variable(
    committer_reading_authority_grounding,
    'What authority grounds THIS reading''s legitimacy claim? Is it (a) lineage (chain of authorized interpretive transmission), (b) practice (what contemporary communities actually do and teach), (c) distributed (no single authority — multiple communities hold different readings), or (d) extraction (institutions benefit from maintaining the mourning-reading as authoritative)?',
    'Institutional analysis: identify which institutions or authorities claim to speak authoritatively for the kernel (Tisha B''Av); examine whether they privilege mourning-practice reading over the other two; trace whether their institutional authority is grounded in religious lineage, enforcement power, or consensus practice; assess whether they benefit structurally from the reading they promote.',
    'The authority_grounding value in cs_structure is determined by this analysis. If lineage, the reading inherits interpretive authority from tradition; if extraction, institutions benefit from maintaining the reading despite rival interpretations; if distributed, the kernel admits multiple readings with no canonical adjudicator. The reading''s legitimacy structure affects whether it forecloses siblings or coexists with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_authority_grounding, conceptual, 'Authority grounding for the mourning-practice reading''s claim to canonical status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement(cata_tr_t7, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 7, 0.62).
narrative_ontology:measurement(cata_tr_t14, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 14, 0.67).
narrative_ontology:measurement(cata_tr_t21, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 21, 0.7).
narrative_ontology:measurement(cata_tr_t28, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 28, 0.72).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t7, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 7, 0.22).
narrative_ontology:measurement(cata_be_t14, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 14, 0.26).
narrative_ontology:measurement(cata_be_t21, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 21, 0.29).
narrative_ontology:measurement(cata_be_t28, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 28, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__mourning_practice_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel catastrophe_memory_function. The kernel describes the functional role of Tisha B'Av and commemorative rituals. Three readings decompose the kernel by ε-invariance: mourning_practice_reading (this story, ε~0.31, Rope), survival_competence_reading (ε~0.48, Rope/Tangled Rope, transmits adaptive mechanisms), hybrid_transformation_reading (ε~0.42, Rope, transmits both mourning and survival functions). Each reading has distinct beneficiaries, distinct ε, distinct structural analysis. The three are not equivalent framings of one constraint — they are three different constraints instantiated from one contested kernel. The kernel contest is empirical: which functional content does the ritual actually transmit? The three readings together form a constraint family linked by affects_constraints; they share no shared measurement grid (each has independent time points) and are consumed in parallel by the genealogy/obsolescence resolver.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
