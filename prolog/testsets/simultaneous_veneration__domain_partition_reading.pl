% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Simultaneous Veneration as Domain-Partitioned Coordination (Domain Partition Reading)
 *   domain: religious_studies/comparative_religion/japanese_religious_history
 *
 * SUMMARY:
 *   In Japanese religious history, simultaneous veneration of kami (Shinto
 *   deities) and buddhas (Buddhist figures) persisted as standard practice
 *   from approximately the 9th century through the Meiji Restoration (1868),
 *   despite apparent theological incoherence. This constraint story
 *   instantiates ONE reading of the contested kernel 'simultaneous
 *   veneration': the domain partition reading holds that kami and buddhas are
 *   functionally distinct entities governing separate domains, making
 *   simultaneous veneration a coherent coordination mechanism rather than a
 *   contradiction requiring resolution. Under this reading, kami govern
 *   this-worldly prosperity (agricultural success, business fortune, health,
 *   protection from harm), while buddhas govern afterlife salvation (escape
 *   from samsara, enlightenment, spiritual liberation). Practitioners
 *   maintained parallel devotional streams without requiring a unified
 *   ontology — households made offerings at both kami shrines and buddha
 *   statues, and institutions (temples, shrines, hybrid establishments)
 *   coordinated ritual specialization. The constraint is a pure coordination
 *   rope with minimal extractiveness: practitioners benefit from addressing
 *   multiple soteriological domains simultaneously, institutional actors
 *   benefit from specialization, and no agent extracts asymmetric advantage.
 *   This reading competes with two sibling readings: the
 *   ontological_fusion_reading (honji-suijaku theory claims kami and buddhas
 *   are ontologically identical), and the pragmatic_incoherence_reading
 *   (simultaneous veneration was never coherent, sustained only by lack of
 *   enforcement pressure until Meiji state separation). All three readings
 *   share the kernel claim (simultaneous veneration persisted), but differ on
 *   whether it was coherent (this reading), unified ontologically (fusion
 *   reading), or contradictory but unenforced (incoherence reading).
 *
 * KEY AGENTS:
 *   - Household practitioners (moderate/mobile): Coordinate separate ritual practices for worldly and soteriological goals without extraction or coercion.
 *   - Temple-shrine institutions (powerful/mobile): Specialize in either kami or buddha domains; benefit from clear functional boundaries that enable institutional focus and prevent resource competition.
 *   - Ritual specialists (powerful/mobile): Priests and monks develop expertise in their respective domains; domain partition reduces role confusion and enables mastery.
 *   - Doctrinal authorities (institutional/arbitrage): Buddhist and Shinto clerical establishments articulate the theoretical justifications (or in this reading, accept domain partition without requiring unified theory).
 *   - Analytical observer (analytical/analytical): Sees simultaneous veneration as coherent specialization without requiring honji-suijaku fusion theory.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.05).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.08).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Simultaneous Veneration as Domain-Partitioned Coordination (Domain Partition Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious_studies/comparative_religion/japanese_religious_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, 'b915d3d4-ea7e-465a-8472-2a55118c13d5').
narrative_ontology:cs_kernel_codification('b915d3d4-ea7e-465a-8472-2a55118c13d5', distributed).
narrative_ontology:cs_authority_grounding('b915d3d4-ea7e-465a-8472-2a55118c13d5', practice).
narrative_ontology:cs_interpretation_layer_present('b915d3d4-ea7e-465a-8472-2a55118c13d5').
narrative_ontology:cs_reading_relation('b915d3d4-ea7e-465a-8472-2a55118c13d5', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('b915d3d4-ea7e-465a-8472-2a55118c13d5', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('b915d3d4-ea7e-465a-8472-2a55118c13d5', foundational, functional_domain_partition_sufficient_for_coherence).
narrative_ontology:cs_axiom_status(functional_domain_partition_sufficient_for_coherence, holdable).
narrative_ontology:cs_axiom_grounding('b915d3d4-ea7e-465a-8472-2a55118c13d5', functional_domain_partition_sufficient_for_coherence, conventional).
narrative_ontology:cs_axiom('b915d3d4-ea7e-465a-8472-2a55118c13d5', foundational, no_unified_ontology_required_for_coordination).
narrative_ontology:cs_axiom_status(no_unified_ontology_required_for_coordination, holdable).
narrative_ontology:cs_axiom_grounding('b915d3d4-ea7e-465a-8472-2a55118c13d5', no_unified_ontology_required_for_coordination, conventional).
narrative_ontology:cs_reference_frame('b915d3d4-ea7e-465a-8472-2a55118c13d5', domain_specialized_practice).
narrative_ontology:cs_drift_state('b915d3d4-ea7e-465a-8472-2a55118c13d5', meiji_restoration_and_after, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b915d3d4-ea7e-465a-8472-2a55118c13d5', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, practitioners_seeking_worldly_prosperity).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, practitioners_seeking_soteriological_salvation).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, institutional_temples_and_shrines).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSEHOLD PRACTITIONER (ROPE) — Coordinate separate devotions as a practical specialization. Make offerings to kami for this-life prosperity (health, harvest, business success) and to buddhas for afterlife salvation. No perceived extraction — both domains address legitimate needs without conflict. The constraint solves a coordination problem: how to address multiple soteriological goals simultaneously without theological confusion.
constraint_indexing:constraint_classification(simultaneous_veneration__domain_partition_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: TEMPLE/SHRINE ADMINISTRATOR (ROPE) — Manage separate ritual spaces and specialist priesthoods for kami veneration and buddha worship. The domain partition enables institutional coordination: each institution can specialize in its functional domain without doctrinal contradiction. Extraction is minimal — the constraint benefits all parties by reducing ritual friction and enabling complementary specialization.
constraint_indexing:constraint_classification(simultaneous_veneration__domain_partition_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / DOMAIN PARTITION (ROPE) — From a comparative religion perspective, simultaneous veneration of functionally distinct entities in separate domains is a coherent coordination mechanism. Kami address this-worldly prosperity (rain, harvest, protection, business success); buddhas address soteriological salvation (nirvana, afterlife liberation). The system requires no supernatural fusion theory or theological contradiction — domain specialization is sufficient. Extractiveness and suppression are minimal because the mechanism solves a genuine coordination problem without coercion or asymmetric benefit.
constraint_indexing:constraint_classification(simultaneous_veneration__domain_partition_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Minimal. Under the domain partition reading, simultaneous veneration solves a genuine coordination problem (how to address multiple soteriological domains) without asymmetric extraction. All agents benefit: practitioners access both worldly and transcendent goods; institutions specialize and avoid role conflict; no agent captures disproportionate resources. The low extractiveness reflects that coordination is symmetric — no party is coercing another into participation or capturing hidden gains. Suppression (0.08): Minimal. The domain partition is not enforced coercively. Practitioners voluntarily maintain separate devotional streams because each serves a genuine need. Alternative approaches (rejecting one domain, seeking fusion theory, specialized monasticism) are available and pursued by some groups without penalty. The low suppression reflects that alternatives are genuinely open and the domain partition is pragmatically chosen, not imposed. Theater ratio (0.25): Low. Ritual performance serves genuine functional purposes: kami veneration coordinates agricultural calendars and community protection; buddha worship coordinates soteriological aspiration. The performative component is subordinate to the functional one. This reading does not classify the rituals as primarily theater — they accomplish real coordination work.
 *
 * PERSPECTIVAL GAP:
 *   The three sibling readings produce different perspectival gaps from the base properties. Under the domain_partition_reading (this constraint), all perspectives classify as Rope with minimal extraction — the constraint is a pure coordination mechanism. Under the ontological_fusion_reading (sibling constraint), perspectives would differ based on whether the agent accepts honji-suijaku theory, and fusion-skeptics might perceive Snare (coercion toward false metaphysical doctrine). Under the pragmatic_incoherence_reading (sibling constraint), perspectives would cluster toward Piton or Snare, reflecting that contradiction is sustained by institutional inertia or enforcement. The gap between this reading and its siblings is not in observed behavior (simultaneous veneration happens in all three readings) but in theoretical structure: what makes the practice coherent (or not) and what mechanism sustains it.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the domain partition reading, directionality is symmetric across all agent perspectives. Beneficiaries (practitioners, institutions) all experience low or slightly positive extraction (they gain access to multiple domains without cost). There are no victims because the constraint does not extract from anyone. The constraint is Rope for all perspectives because it solves a coordination problem that all parties benefit from solving. No directionality override is needed because the structural data — symmetric beneficiaries, no victims, minimal suppression, minimal extraction — directly yields low d values and produces Rope classification across all contexts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_sufficiency_for_coherence,
    'Does functional domain partition alone provide sufficient theoretical justification for simultaneous veneration, or does the absence of ontological connection create an unexplained gap that practitioners felt compelled to fill with fusion theories?',
    'Historical analysis of practitioner articulations: examination of household shrine arrangements, ritual manuals, and oral traditions to assess whether practitioners actually articulated the domain partition logic or simply performed both rites without explicit justification. Comparison with cultures that practice multiple religious traditions to assess whether explicit domain framing is necessary or domain-crossing practice is self-justifying.',
    'If domain partition suffices: this reading explains simultaneous veneration as coherent coordination without requiring honji-suijaku theory. The ontological_fusion_reading becomes secondary. If explicit fusion logic was required: the domain partition reading describes only a functional outcome, not the lived theological structure — the ontological_fusion_reading captures actual practitioner rationalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_sufficiency_for_coherence, empirical, 'Whether domain partition logic alone provides sufficient theoretical coherence').

omega_variable(
    institutional_specialization_vs_doctrine,
    'Did the domain partition arise from institutional logic (temples and shrines naturally specialized to preserve ritual and economic niches) or from deliberate doctrinal development (priests articulated domain partition as a coherent theology)?',
    'Historical trace: comparison of institutional chronology (when separate temple and shrine organizations emerged) with textual evidence (when domain partition concepts first appear in written sources). Assessment of which preceded the other and whether doctrinal rationalization followed from institutional necessity.',
    'If institutional logic primary: this reading correctly frames simultaneous veneration as coordination emerging from specialization, with domain partition as post-hoc coherence-making. If doctrine primary: the reading underestimates the theoretical sophistication required to justify simultaneous practice — coherence was asserted, not naturally obvious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_specialization_vs_doctrine, empirical, 'Whether domain partition arose from institutional specialization or doctrinal development').

omega_variable(
    reading_boundary_ambiguity,
    'Does the domain partition reading correctly identify the kernel boundary? Is the contested claim ''kami and buddhas are functionally distinct'' or ''simultaneous veneration is theologically coherent''? These are subtly different claims with different constraint structures.',
    'Conceptual analysis of the three sibling readings: assess which claim each reading actually contests. If ontological_fusion_reading contests functional distinctness while pragmatic_incoherence_reading contests coherence, then the kernel has two competing axes (ontology vs coherence). This reading may collapse two distinct constraint problems into one.',
    'If kernel is two-axis: this story should decompose into two separate constraints (life-domain kami coordination + death-domain buddha coordination) with their own ε values and perspectives, linked via network.affects_constraints. If kernel is genuinely one claim: current reading is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Ambiguity in kernel boundary definition across three readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, resource_allocation).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The simultaneous_veneration kernel decomposes into three structurally distinct constraints, one for each reading. The domain_partition_reading asserts that two parallel coordination constraints (life-domain kami coordination and death-domain buddha coordination) solve the simultaneous veneration problem without theological fusion. The ontological_fusion_reading would claim a single unified constraint grounded in honji-suijaku metaphysics. The pragmatic_incoherence_reading would frame simultaneous veneration as a degraded piton or snare constraint sustained by institutional inertia. Each reading is a distinct constraint with its own ε value, beneficiary/victim structure, and type classification. They are linked by the kernel they interpret, not by direct structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
