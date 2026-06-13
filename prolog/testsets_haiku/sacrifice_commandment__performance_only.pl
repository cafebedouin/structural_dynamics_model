% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment Performance-Only Reading
 *   domain: religious/legal/commitment_system
 *
 * SUMMARY:
 *   The performance-only reading of the sacrifice commandment kernel holds
 *   that the commandment requires physical Temple execution and cannot be
 *   fulfilled through study, teaching, or eschatological preparation. This
 *   reading has been the dominant institutional interpretation for 1,900
 *   years, generating an enormous scholarly apparatus around unperformable
 *   laws—a system that extracts observant individuals' time and attention
 *   while concentrating interpretive authority in the talmudic scholarly
 *   institution. The constraint is a tangled rope: it performs genuine
 *   coordination (knowledge preservation, textual transmission) while
 *   simultaneously extracting from observant individuals who bear the
 *   obligation to study law they cannot perform. The reading forecloses the
 *   study-as-performance alternative (direct contradiction of core axioms)
 *   and coexists with archive-maintenance reading (a different justification
 *   for the same suspended-study phenomenon). The claim and metrics are
 *   intentionally independent: the reading is claimed as coordination
 *   (knowledge preservation) while the measurement profile describes
 *   substantial extraction (1,900 years of scholarly labor directed at
 *   unperformable acts, theater rising to 0.68 by endpoint, suppression
 *   holding steady at 0.72).
 *
 * KEY AGENTS:
 *   - Talmudic scholarly authority: maintains institutional jurisdiction over sacrifice law by declaring it suspended; benefits from the scholarly apparatus that suspension creates
 *   - Observant Jewish individuals: bear the obligation to study unperformable commandments; trapped by identity-lock (exiting obligation requires exiting Jewish identity)
 *   - Study-as-performance advocates: excluded from adjudicating this reading; their position logically forecloses the performance-only axiom
 *   - Archive-maintenance advocates: excluded from adjudicating this reading; their eschatological framing contradicts pure suspension
 *   - Post-Enlightenment textual critics: observe the constraint from outside halakhic authority; document how suspension doctrine sustains itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.81).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.72).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.81).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/legal/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, 'f843f4d1-41d2-4a8e-8b10-f54996de5765').
narrative_ontology:cs_kernel_codification('f843f4d1-41d2-4a8e-8b10-f54996de5765', fixed_text).
narrative_ontology:cs_authority_grounding('f843f4d1-41d2-4a8e-8b10-f54996de5765', lineage).
narrative_ontology:cs_interpretation_layer_present('f843f4d1-41d2-4a8e-8b10-f54996de5765').
narrative_ontology:cs_reading_relation('f843f4d1-41d2-4a8e-8b10-f54996de5765', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('f843f4d1-41d2-4a8e-8b10-f54996de5765', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('f843f4d1-41d2-4a8e-8b10-f54996de5765', foundational, physical_execution_required).
narrative_ontology:cs_axiom_status(physical_execution_required, holdable).
narrative_ontology:cs_axiom_grounding('f843f4d1-41d2-4a8e-8b10-f54996de5765', physical_execution_required, deontological).
narrative_ontology:cs_axiom('f843f4d1-41d2-4a8e-8b10-f54996de5765', foundational, study_cannot_substitute).
narrative_ontology:cs_axiom_status(study_cannot_substitute, holdable).
narrative_ontology:cs_axiom_grounding('f843f4d1-41d2-4a8e-8b10-f54996de5765', study_cannot_substitute, deontological).
narrative_ontology:cs_reference_frame('f843f4d1-41d2-4a8e-8b10-f54996de5765', suspension_indefinite).
narrative_ontology:cs_drift_state('f843f4d1-41d2-4a8e-8b10-f54996de5765', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f843f4d1-41d2-4a8e-8b10-f54996de5765', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, talmudic_scholarly_authority).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, observant_jewish_individuals).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, living_commandment_fulfillment).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because the constraint diverts observant individuals' effort from living law toward mastery of technical details of unperformable rituals—a cost borne entirely by those bound by Jewish identity, with no corresponding performance benefit. The scholarly institution benefits: it accumulates interpretive authority and the organizational structure to transmit and adjudicate sacrifice law. Suppression is high (0.72) because the constraint's persistence depends on maintaining authority to declare suspension binding; alternative readings (study-as-performance, archive-maintenance) that would redistribute authority or reframe the meaning of study must be suppressed. Theater is high and rising (0.68 at endpoint): as historical distance from Temple destruction grows, the rationale shifts. Early suspension doctrine might have served genuine coordination (preserving knowledge for hoped-for restoration); by the modern era, the scholarly apparatus operates primarily to maintain itself—the textual focus is performative, defending institutional authority rather than serving a live eschatological function. The measurement series captures this drift: extractiveness rises moderately (constraint's institutional capture intensifies), theater rises substantially (functional rationale erodes, institutional theater increases), suppression holds steady (the authority structure required to enforce the reading remains constant). The rising theater_ratio without corresponding suppression_requirement increase indicates the constraint persists through institutional inertia and identity-lock rather than active coercion—a piton-adjacent trajectory that the theater dynamics flag.
 *
 * PERSPECTIVAL GAP:
 *   From the talmudic scholarly authority's seat: this reading is genuine coordination—knowledge preservation, textual mastery, transmission of complex ritual expertise. The scholar-institution sees itself as custodian of the law and protector of Jewish continuity. From the observant individual's seat: this is enforced study of unperformable law, an obligation they cannot fulfill, sustained by institutional authority they cannot challenge without exiting their identity. From the study-as-performance advocate's seat: the reading forecloses the only coherent way to make the commandment live and fulfillable, reducing it to eternal suspension. From the archive-maintenance advocate's seat: the reading ignores the eschatological purpose that justifies the scholarly apparatus. The engine computes each seat's type from the structural data: the beneficiary (talmudic authority) likely experiences this as rope (genuine coordination with asymmetric benefit), while the victim (observant individual) experiences it as tangled_rope or snare (asymmetric extraction dressed as coordination, maintained by institutional authority). The gap reveals the constraint's extractive architecture.
 *
 * DIRECTIONALITY LOGIC:
 *   Talmudic scholarly authority is the structural beneficiary: it collects institutional authority, maintains jurisdictional control over sacrifice law, and benefits from the scholarly apparatus that suspension doctrine creates. Directionality for this seat is near 0.0 (beneficiary end)—the constraint subsidizes their authority. Observant Jewish individuals are the structural targets: they bear the obligation to study, cannot fulfill it, and are locked in by identity. Their directionality is near 1.0 (full target end)—the constraint extracts their time and intellectual attention without fulfillment. The abstract good 'living commandment fulfillment' also bears cost—the constraint diverts from performable commandments and living law toward theoretical mastery of the unperformable. The non-agent entries (messianic restoration, living fulfillment) represent the beneficiary framing of the constraint (eschatological preparation) and the cost it imposes (suspension of living obligation), but they do not carry directionality themselves—they reify the commitments that inform the directionality of agent seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction, status of commandments requiring Temple) was live and pressing in 70 CE—it demanded an answer to preserve Jewish law and community continuity. The performance-only reading offered one answer: the commandment is suspended, not fulfilled via study, but the obligation to study it remains, preserving technical knowledge for possible future restoration. For 500-1000 years, this reading likely served genuine coordination—knowledge preservation in diaspora conditions where Temple restoration seemed plausible within living memory. However, by 1200 CE and certainly by 1800 CE, the founding problem was substantially dead: 1,100+ years had passed without restoration, and the eschatological rationale had become distant. Yet the scholarly apparatus persisted and elaborated. This is a mandatrophy signature: the constraint that solved a real problem now persists primarily to maintain the institutional apparatus that the problem created. The theater_ratio rising to 0.68 confirms this—much of the scholarly activity is performative, defending institutional authority rather than solving the original problem. The constraint is a candidate for Piton classification at the institutional level (though tangled_rope at the individual level, because active suppression still maintains the reading against alternatives). Mandatrophy is partially resolved: the founding problem's death is documented (1,900 years without Temple restoration; contemporary Jewish scholarship contests whether suspension doctrine remains binding), but the constraint persists because the talmudic authority structure benefits from it and alternative readings are suppressed. To fully resolve mandatrophy would require institutional reform—treating study-as-performance or archive-maintenance as legitimate alternatives rather than heresy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_substitution_ambiguity,
    'Can study of a commandment requiring physical execution logically constitute performance of that commandment, or is substitution ruled out by the nature of the obligation itself?',
    'Halakhic analysis of the foundational texts (Talmud, Maimonides, later codifiers) and the logical structure of commandment classes. Empirical test: examine whether contemporary Jewish movements accept study-as-performance for other Temple-dependent commandments or only for sacrifice.',
    'If study CAN logically substitute, the performance-only axiom is not foundational to Jewish law—the reading is contingent on institutional maintenance. If substitution is impossible, the performance-only reading is structurally necessary. This determines whether the constraint is genuinely natural-law-like or institutionally constructed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_substitution_ambiguity, conceptual, 'Whether performance-substitution is logically possible or ruled out by commandment nature.').

omega_variable(
    eschatological_restoration_assumption,
    'Is the performance-only reading''s implicit assumption—that Temple restoration will eventually occur—still operative, or has it become a formal theological posture without real institutional weight?',
    'Examine how often and in what contexts contemporary talmudic authorities invoke restoration as a live possibility vs. a formal affirmation. Compare frequency of restoration language in medieval vs. modern halakhic literature. Analyze contemporary Jewish institutional priorities: how much institutional energy is directed toward actual Temple restoration preparation?',
    'If restoration assumption is live, the scholarly apparatus serves a real eschatological function and the constraint is partially justified by its coordination role. If assumption is dead, the constraint is pure institutional theater defending authority without serving the original purpose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eschatological_restoration_assumption, empirical, 'Whether the eschatological justification for suspension doctrine remains operationally operative.').

omega_variable(
    suppression_mechanism_source,
    'Is the suppression of study-as-performance and archive-maintenance readings structural (inherent to the logical positions) or institutional (enforced by talmudic authority gatekeeping)?',
    'Historical analysis: when did alternative readings emerge and why were they marginalized? Did the talmudic establishment suppress them actively, or did they naturally lose institutional standing? Can study-as-performance advocates articulate coherent halakhic positions, or are their positions genuinely incoherent within the halakhic framework?',
    'If suppression is structural, the constraint''s persistence is self-maintaining—the reading is intrinsically stable. If suppression is institutional, the constraint depends on the authority structure, and institutional reform could unseat it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, conceptual, 'Whether suppression of alternatives is logically inherent or institutionally maintained.').

omega_variable(
    identity_lock_dissolution_path,
    'For observant Jewish individuals bearing the obligation to study sacrifice law, what would it take to exit without dissolving Jewish identity? Can the obligation be reframed or reinterpreted within Jewish frameworks?',
    'Document existing alternative Jewish readings (Conservative, Reform, Reconstructionist, academic approaches). Interview observant individuals about conditions under which they would question or exit the constraint. Examine whether Jewish communities are producing institutional structures (study-as-performance communities, archive-as-function communities) that offer exit while maintaining identity.',
    'If exit requires identity dissolution, the identity-lock is complete and suppression is nearly impossible to overcome. If exit routes exist within Jewish tradition, the constraint is more vulnerable to reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_dissolution_path, empirical, 'Whether identity-lock is absolute or whether exit is possible while maintaining Jewish identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_commandment__performance_only, theater_ratio, 70, 0.15).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__performance_only, theater_ratio, 500, 0.35).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_commandment__performance_only, theater_ratio, 1200, 0.52).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_commandment__performance_only, theater_ratio, 1800, 0.64).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_commandment__performance_only, theater_ratio, 1950, 0.67).
narrative_ontology:measurement(sacr_tr_t2026, sacrifice_commandment__performance_only, theater_ratio, 2026, 0.68).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_commandment__performance_only, base_extractiveness, 70, 0.45).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__performance_only, base_extractiveness, 500, 0.62).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_commandment__performance_only, base_extractiveness, 1200, 0.74).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_commandment__performance_only, base_extractiveness, 1800, 0.79).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_commandment__performance_only, base_extractiveness, 1950, 0.8).
narrative_ontology:measurement(sacr_be_t2026, sacrifice_commandment__performance_only, base_extractiveness, 2026, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_commandment__performance_only, suppression_requirement, 70, 0.4).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__performance_only, suppression_requirement, 500, 0.55).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_commandment__performance_only, suppression_requirement, 1200, 0.68).
narrative_ontology:measurement(sacr_su_t1800, sacrifice_commandment__performance_only, suppression_requirement, 1800, 0.71).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_commandment__performance_only, suppression_requirement, 1950, 0.72).
narrative_ontology:measurement(sacr_su_t2026, sacrifice_commandment__performance_only, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, attachment_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__performance_only, 0.12).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three structurally distinct constraints because the three readings instantiate radically different ε values and beneficiary/victim structures from the same source text (the halakhic obligation to bring sacrifices). The performance_only reading (this file) extracts substantially (ε=0.81) from observant individuals while benefiting talmudic authority; study_as_performance reading extracts minimally (ε ≈ 0.25) by reframing study as fulfillment; archive_maintenance reading is intermediate (ε ≈ 0.45) by justifying study as eschatological preparation rather than indefinite suspension. All three readings agree the same observable fact (intense sacrifice study without physical performance) but attribute different meanings and different institutional arrangements to it. Each reading is a valid constraint story because each produces a different ε-invariant classification. The three stories are linked by network.affects_constraints: performance_only forecloses study_as_performance (logically rules it out from within a single halakhic framework) and coexists with archive_maintenance (both justify study without performance, using different rationales). All three readings draw from the same kernel (sacrifice commandment status post-Temple destruction) and the same textual tradition, but instantiate three different constraints in the Deferential Realism classification system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__performance_only, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
