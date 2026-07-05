% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Commemoration Ritual as Symbolic Continuity Practice
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the mourning-practice reading of the
 *   catastrophe_memory_preservation kernel: ritual observance is analyzed
 *   strictly as a symbolic continuity mechanism, distinct from any claim that
 *   it preserves operational threat-recognition capacity (the
 *   survival_competence_reading) or that it began as the latter and degraded
 *   into the former (the hybrid_atrophy_reading). Under this reading, the
 *   ritual's entire justification is identity transmission and communal
 *   cohesion; it makes no claim to functional survival utility, so its
 *   extractiveness profile is low and its suppression near-absent because
 *   participation is genuinely voluntary and exit is unobstructed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Commemoration Ritual as Symbolic Continuity Practice").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, '601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2').
narrative_ontology:cs_kernel_codification('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', distributed).
narrative_ontology:cs_authority_grounding('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', practice).
narrative_ontology:cs_interpretation_layer_present('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2').
narrative_ontology:cs_reading_relation('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', foundational, ritual_function_is_purely_symbolic).
narrative_ontology:cs_axiom_status(ritual_function_is_purely_symbolic, holdable).
narrative_ontology:cs_axiom_grounding('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', ritual_function_is_purely_symbolic, conventional).
narrative_ontology:cs_axiom('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', secondary, identity_continuity_requires_no_operational_content).
narrative_ontology:cs_axiom_status(identity_continuity_requires_no_operational_content, holdable).
narrative_ontology:cs_axiom_grounding('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', identity_continuity_requires_no_operational_content, conventional).
narrative_ontology:cs_reference_frame('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', founding_catastrophe_commemorative_intent).
narrative_ontology:cs_drift_state('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', contemporary_diaspora_generation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('601d87b6-bfbf-4c3c-8712-aaa40e4f7ce2', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, in_group_descendant_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, communal_identity_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, younger_generation_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, collective_identity_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates annually in commemorative rites marking an ancestral catastrophe. Gains a shared calendar, shared vocabulary, and reaffirmed sense of belonging. Can decline to attend any given observance without formal penalty, though social warmth toward non-participants may cool somewhat; emigration, intermarriage, or secularization all function as real exits without legal or economic barrier.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, in_group_descendant_community, beneficiary,
    moderate, generational, mobile, national).

% Synagogues, churches, cultural associations, and memorial foundations organize the liturgy, the memorial calendar, and the commemorative texts. They administer the ritual and select what is emphasized, but no toll or transactional fee is extracted from attendees, and their continued operation depends on voluntary participation and donation rather than compulsion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, communal_identity_institutions, agenda_setter,
    organized, generational, constrained, national).

% Inherit the ritual's forms without having lived the founding catastrophe. Some find the symbolic vocabulary meaningful for identity anchoring; others find it emotionally opaque or feel obligated attendance crowds out other commitments. Their preferences about updating or retiring specific ritual elements are often not solicited by the institutions that administer the calendar.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, younger_generation_members, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, younger_generation_members, excluded).

% Document the historical catastrophe independently of the ritual calendar. They can assess whether ritual observance transmits any operationally useful threat-recognition content or functions purely as identity commemoration, providing an outside check on what the ritual actually preserves.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, historians_and_survivor_testimony_archives, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, repeatable occasion through which a dispersed descendant community reaffirms common identity, marks continuity with ancestors who experienced the catastrophe, and transmits a symbolic (not operational) vocabulary of belonging across generations.
% TRANSFER_FUNCTION: Moves attention, emotional labor, and modest voluntary donations from participants toward the maintenance of commemorative institutions and toward the shared symbolic repertoire itself; no resource is extracted from non-participants and no coercive cost attaches to non-attendance.
% ABSENT_VOICES: Members who have exited the community entirely (through secularization, conversion, or diaspora dissolution) are not present to comment on whether the ritual's symbolic function justifies its emotional or temporal demands; their absence is voluntary and largely self-selected rather than structurally imposed.
% DISAPPEARANCE_RATIONALE: Some community members would say the world barely rearranges — identity persists through family narrative, textual study, and informal memory even without the formal ritual calendar. Others, particularly the organizing institutions, would say the loss would be significant: the ritual is the primary recurring occasion binding a geographically dispersed population, and its disappearance would accelerate identity diffusion. The two camps genuinely disagree, which is why this sits at 'contested' rather than a clean verdict.
% FOUNDING_PROBLEM: A community that experienced a historical catastrophe needed a durable, repeatable mechanism to prevent the event and the identity forged by it from being forgotten as direct survivors aged and died.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians and archival researchers, outside the commemorative institutions themselves, corroborate that the ritual calendar continues to function as one of the primary vectors of transgenerational identity transmission in dispersed communities, based on ethnographic and survey research rather than institutional self-report.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because the ritual imposes only modest voluntary costs (time, donation, emotional labor) with no coercive backing. Suppression is low (0.12) because non-participation carries social rather than material consequence and multiple real exits exist (secularization, geographic dispersal, intermarriage). Theater ratio is moderate and rising over the interval (0.22 to 0.40) reflecting that as direct survivor testimony recedes, an increasing share of the ritual's content becomes performative reenactment of a memory rather than a lived transmission — this is expected and does not by itself indicate extraction, since under this reading the performative function IS the preserved function, not a substitute for a lost operational one.
 *
 * PERSPECTIVAL GAP:
 *   The organizing institutions and the youngest generation of inheritors compute this constraint differently: institutions experience the ritual as a coordination success actively worth sustaining, while some younger members experience inherited obligation without having consented to the symbolic content's specific emphases. This gap is a normal feature of intergenerational transmission under this reading, not evidence of extraction — the engine's computed seat divergence should reflect generational distance from the founding event, not hidden coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   In-group descendant communities and communal identity institutions are declared beneficiaries: cohesion, belonging, and institutional continuity flow to them from the ritual's operation. No victim group is declared, consistent with the expected structural delta for this reading — participation is opt-in and non-participants bear no material penalty, so there is no directionality target analogous to a payer under extraction. Communal identity institutions carry constrained exit options because their organizational survival depends on continued ritual observance even though no individual is coerced into attending.
 *
 * MANDATROPHY ANALYSIS:
 *   Because this reading explicitly denies any claim to operational survival function, the founding_problem_status of 'live' does not imply that catastrophe-preparedness content is being transmitted — it implies that the identity-transmission problem (preventing forgetting) remains live. This blocks the mandatrophy misreading in the other direction: a critic cannot correctly charge this reading with pretending to teach threat-recognition it no longer teaches, because under mourning_practice_reading it never claimed to.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the mourning-practice reading the correct structural account of this ritual, or does it in fact retain vestigial operational content that would place it under the survival_competence_reading or hybrid_atrophy_reading instead?',
    'Ethnographic and textual analysis of the ritual''s specific content: does it encode actionable warnings, resource-behavior heuristics, or vigilance instructions with measurable transmission fidelity, or is its content exclusively narrative/liturgical with no operational payload? Comparative study across communities practicing variants of the same commemorative tradition would help triangulate.',
    'If operational content is found, this story''s classification as a low-extraction rope would need to be reconsidered under the survival_competence_reading (which the source material predicts to carry a different, likely higher-coordination-value profile) or the hybrid_atrophy_reading (which would predict a decaying operational core beneath the symbolic surface).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading correctly characterizes the ritual as purely symbolic rather than partly operational.').

omega_variable(
    voluntary_participation_erosion_over_generations,
    'Does the low suppression measured for this reading remain low as generational distance from the founding catastrophe increases, or does declining organic motivation eventually require institutions to introduce social or normative pressure to sustain attendance?',
    'Longitudinal survey of attendance rates, stated motivations, and any institutional messaging shift (from invitation-based to obligation-based framing) across successive generational cohorts.',
    'If institutions begin applying normative pressure to compensate for declining organic participation, the suppression metric would need to rise for later time points, and the constraint could drift toward tangled_rope as institutional self-preservation needs begin to diverge from pure cohesion benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_erosion_over_generations, empirical, 'Whether declining organic participation over generations induces rising institutional suppression.').

omega_variable(
    rising_theater_ratio_interpretation,
    'Does the rising theater_ratio (0.22 to 0.40) over the interval indicate healthy adaptation of a symbolic practice to a generation further from the founding event, or early-stage hollowing-out that a future reading might reclassify as piton?',
    'Compare theater_ratio trajectory against measures of subjective meaningfulness reported by participants across cohorts; a rising performative share accompanied by stable or rising reported meaningfulness supports healthy symbolic adaptation, while rising performative share with declining reported meaningfulness would support hollowing-out.',
    'Under the healthy-adaptation interpretation the rope classification holds; under the hollowing-out interpretation a future extension of this story''s interval might show the constraint crossing into piton territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rising_theater_ratio_interpretation, empirical, 'Whether rising theatricality reflects adaptation or degradation of the ritual''s coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 60, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the catastrophe_memory_preservation kernel. survival_competence_reading claims the ritual preserves operational threat-recognition capacity; hybrid_atrophy_reading claims the ritual began with that operational function and has degraded to this reading's purely symbolic content. All three readings share a text/practice kernel (the ritual observance itself) but diverge on what the ritual actually transmits, producing different epsilon values, different beneficiary/victim structures, and different constraint types. They are linked here rather than merged per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
