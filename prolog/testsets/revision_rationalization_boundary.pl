% ============================================================================
% CONSTRAINT STORY: revision_rationalization_boundary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_revision_rationalization_boundary, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: revision_rationalization_boundary
 *   human_readable: Revision-Rationalization Boundary in Moral Self-Assessment
 *   domain: moral_philosophy/psychology_of_agency/social_epistemology
 *
 * SUMMARY:
 *   The revision-rationalization boundary constraint captures the epistemic
 *   problem autonomous agents face when attempting to distinguish principled
 *   moral revision from motivated reasoning in their own case. This
 *   constraint is downstream of both the recognition dependency mechanism
 *   (the structural requirement for external validation of internal states)
 *   and the heteronomy adoption pathway (the mechanism by which agents
 *   internalize external standards). The boundary problem emerges because the
 *   meta-standards an agent uses to evaluate whether a principle-revision is
 *   principled or rationalized are themselves subject to motivated
 *   distortion. An agent revising their moral principles can always generate
 *   a justification that appears principled from within their current frame,
 *   but this appearance is precisely what rationalization produces. The
 *   constraint exhibits high extractiveness (0.68) because it enables
 *   systematic self-deception while maintaining the appearance of moral
 *   agency, and high suppression (0.72) because both social costs (admitting
 *   rationalization threatens status) and psychological costs (recognizing
 *   rationalization threatens identity coherence) create barriers to exit.
 *   The theater ratio (0.65) reflects that traditional introspective
 *   self-audit practices provide psychological comfort and social signaling
 *   value but minimal functional verification of revision quality.
 *
 * KEY AGENTS:
 *   - Autonomous Agent (Identity-Locked): Primary victim (powerless/identity_locked) — cannot distinguish revision from rationalization because meta-standards are subject to same motivated distortion; identity constituted through self-perception as principled reasoner
 *   - Epistemic Commons: Primary victim (powerless/trapped) — moral discourse reliability as abstract collective good; bears full cost of undetected rationalization contaminating moral claims
 *   - Reflective Practitioner: Secondary victim (moderate/constrained) — has access to external feedback mechanisms but faces high social and psychological costs to use them; benefits from genuine revision capacity while bearing extraction from undetected rationalization
 *   - Self-Concept Maintenance System: Primary beneficiary (institutional/arbitrage) — psychological subsystem that benefits from rationalization capacity; experiences boundary ambiguity as coordination enabling adaptive self-concept updates
 *   - Accountability Infrastructure Coalition: Organized agents (organized/mobile) — building external verification mechanisms (pre-commitment devices, public justification, adversarial collaboration) with sunset logic
 *   - Introspective Self-Audit Ritual: Institutional actor (institutional/arbitrage) — traditional moral self-examination practices persist through cultural inertia despite limited effectiveness; sees own process as degraded
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing boundary ambiguity as inherent to self-reflective cognition rather than contingent feature of isolated individual cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(revision_rationalization_boundary, 0.68).
domain_priors:suppression_score(revision_rationalization_boundary, 0.72).
domain_priors:theater_ratio(revision_rationalization_boundary, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(revision_rationalization_boundary, extractiveness, 0.68).
narrative_ontology:constraint_metric(revision_rationalization_boundary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(revision_rationalization_boundary, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(revision_rationalization_boundary, snare).
narrative_ontology:human_readable(revision_rationalization_boundary, "Revision-Rationalization Boundary in Moral Self-Assessment").
narrative_ontology:topic_domain(revision_rationalization_boundary, "moral_philosophy/psychology_of_agency/social_epistemology").

domain_priors:requires_active_enforcement(revision_rationalization_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(revision_rationalization_boundary, self_concept_maintenance_system).
narrative_ontology:constraint_beneficiary(revision_rationalization_boundary, social_status_preservation).
narrative_ontology:constraint_victim(revision_rationalization_boundary, autonomous_agents).
narrative_ontology:constraint_victim(revision_rationalization_boundary, epistemic_reliability_of_moral_discourse).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AUTONOMOUS AGENT (SNARE) — Identity-locked by self-concept coherence requirements. Cannot distinguish principled revision from motivated reasoning because the meta-standards for evaluation are themselves subject to the same motivated distortion. The agent's identity is constituted through their self-perception as a principled reasoner, making recognition of rationalization literally unthinkable from within the frame. Structural mobility exists (could adopt external accountability mechanisms) but identity fusion prevents exit.
constraint_indexing:constraint_classification(revision_rationalization_boundary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: EPISTEMIC COMMONS (SNARE) — Moral discourse reliability as abstract collective good. Cannot exit the contamination from undetected rationalization. Bears full cost of false moral claims presented as principled revision. No advocate, no exit option, maximum extraction.
constraint_indexing:constraint_classification(revision_rationalization_boundary, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: REFLECTIVE PRACTITIONER (TANGLED ROPE) — Agent with access to external feedback mechanisms (therapy, peer review, accountability partners) but facing high costs to use them. Benefits from the self-revision capacity (genuine moral growth is possible) while bearing extraction from undetected rationalization episodes. Constrained by social cost of admitting motivated reasoning and psychological cost of identity threat.
constraint_indexing:constraint_classification(revision_rationalization_boundary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: SELF-CONCEPT MAINTENANCE SYSTEM (ROPE) — Psychological subsystem that benefits from rationalization capacity. Experiences the boundary ambiguity as coordination: enables adaptive self-concept updates without identity crisis. Net beneficiary — extraction runs toward this system (preserves coherence) not away from it.
constraint_indexing:constraint_classification(revision_rationalization_boundary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ACCOUNTABILITY INFRASTRUCTURE (SCAFFOLD) — Organized agents building external verification mechanisms: pre-commitment devices, public justification requirements, adversarial collaboration norms, registered moral predictions. See the boundary problem as temporary coordination failure with sunset: as accountability infrastructure matures, the gap between revision and rationalization becomes externally verifiable rather than purely introspective. Estimated sunset: 20-40 years for norms to mature in moral philosophy and adjacent fields.
constraint_indexing:constraint_classification(revision_rationalization_boundary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTROSPECTIVE SELF-AUDIT (PITON) — Traditional moral self-examination practices (journaling, meditation, confessional practices) persist through cultural inertia despite limited effectiveness at detecting motivated reasoning. The ritual provides psychological comfort and social signaling value but minimal functional verification. Theater ratio high because the practice cannot access its own blind spots.
constraint_indexing:constraint_classification(revision_rationalization_boundary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational/universal perspective, some boundary ambiguity between revision and rationalization may appear inherent to self-reflective cognition: any meta-standard for evaluating revisions is itself revisable, creating infinite regress. This perspective risks naturalizing what is actually a contingent feature of isolated individual cognition — the boundary becomes clearer with external accountability structures.
constraint_indexing:constraint_classification(revision_rationalization_boundary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(revision_rationalization_boundary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(revision_rationalization_boundary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(revision_rationalization_boundary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(revision_rationalization_boundary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(revision_rationalization_boundary, TR),
    TR >= 0.70.

:- end_tests(revision_rationalization_boundary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint enables systematic self-deception while preserving the appearance of moral agency. Agents can revise principles in response to self-interest while genuinely believing the revision is principled, extracting the benefits of flexibility without the costs of acknowledged inconsistency. The extraction is severe because it operates at the meta-level — not just deceiving others but deceiving oneself about one's own epistemic processes. Suppression (0.72): High. Multiple barriers prevent exit: (1) Social cost — admitting rationalization threatens moral status and credibility. (2) Psychological cost — recognizing rationalization threatens identity coherence for agents whose self-concept depends on being principled reasoners. (3) Cognitive architecture — the same motivated reasoning processes that generate rationalization also generate justifications for why the revision is not rationalization. (4) Lack of external verification — in isolated individual cognition, no independent check exists. Theater ratio (0.65): Moderate-high. Traditional introspective practices (journaling, meditation, moral self-examination) provide psychological comfort and signal moral seriousness but have limited capacity to detect motivated reasoning. The practice cannot access its own blind spots — an agent rationalizing will produce introspective outputs that appear principled to themselves. The theater has increased over the interval as moral discourse has become more sophisticated in generating post-hoc justifications.
 *
 * PERSPECTIVAL GAP:
 *   The autonomous agent sees a snare — cannot distinguish revision from rationalization, identity-locked by self-concept coherence requirements. The epistemic commons sees a snare — bears full contamination cost with no exit. The reflective practitioner sees tangled rope — benefits from revision capacity while bearing extraction from undetected rationalization, constrained by costs of external accountability. The self-concept maintenance system sees rope — benefits from rationalization capacity, experiences boundary ambiguity as coordination. The accountability infrastructure coalition sees scaffold — building external verification mechanisms with sunset logic. The introspective self-audit ritual sees piton — traditional practices persist through inertia despite limited effectiveness. The analytical observer risks seeing mountain — boundary ambiguity as inherent to self-reflective cognition — but structural data reveals this as naturalization of contingent feature of isolated cognition.
 *
 * DIRECTIONALITY LOGIC:
 *   The autonomous agent is identity-locked rather than trapped because the binding mechanism is cognitive rather than structural. The agent has structural mobility — could adopt external accountability mechanisms, could submit revisions to adversarial review, could use pre-commitment devices — but cannot exercise this mobility because their identity is constituted through their self-perception as a principled autonomous reasoner. Exit would require not just paying a cost but becoming a different kind of agent — one who acknowledges dependence on external validation for moral self-knowledge. This is the identity lock: the agent's self-concept depends on the very capacity (autonomous moral reasoning) that the constraint undermines. The self-concept maintenance system is the primary beneficiary with arbitrage exit — it benefits from the rationalization capacity and can shift between frames as needed. The epistemic commons is trapped with no exit — abstract collective good that bears contamination cost. The reflective practitioner is constrained — has access to exit mechanisms but faces high costs. The accountability infrastructure coalition is organized with mobile exit — building alternative verification pathways. The analytical observer risks naturalizing the boundary ambiguity as inherent to cognition, but the structural data reveals this as contingent on isolated individual cognition — the boundary becomes clearer with external accountability.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival decomposition. The boundary problem is not 'inherently unsolvable' (mountain) nor 'purely extractive' (snare from all perspectives) but exhibits different structural features from different positions. The identity-locked agent genuinely cannot see the boundary from within their frame — this is not a failure of effort but a structural feature of identity-constituted cognition. The self-concept maintenance system genuinely benefits from the ambiguity — this is coordination from its perspective, not extraction. The accountability infrastructure genuinely provides a sunset — external verification mechanisms make the boundary externally verifiable even when internally opaque. The mandatrophy is resolved by recognizing that 'the revision-rationalization boundary' is not a single constraint but a presheaf over observation sites: snare from the identity-locked agent's position, rope from the self-concept system's position, scaffold from the organized accountability coalition's position. The analytical mountain is a false summit — naturalizes what external accountability structures reveal as contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meta_standard_stability_threshold,
    'What temporal stability of meta-standards distinguishes principled revision from motivated reasoning?',
    'Longitudinal tracking of agents'' stated criteria for principle-revision across multiple revision episodes; correlation between meta-standard stability and external validation of revision quality',
    'If threshold requires high stability: many genuine moral learning episodes misclassified as rationalization. If threshold allows high variability: rationalization episodes pass as principled revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meta_standard_stability_threshold, empirical, 'Temporal stability threshold for meta-standards distinguishing revision from rationalization').

omega_variable(
    external_accountability_sufficiency,
    'Do external accountability mechanisms (pre-commitment, public justification, adversarial review) actually reduce rationalization rates or merely shift rationalization to meta-level?',
    'Comparison of rationalization detection rates: agents with vs without accountability structures; analysis of whether accountability produces genuine constraint or higher-order rationalization',
    'If effective: scaffold perspective confirmed — accountability infrastructure provides real sunset. If ineffective: rationalization is fractal and accountability merely adds theatrical layer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_accountability_sufficiency, empirical, 'Whether external accountability mechanisms reduce rationalization or displace it').

omega_variable(
    identity_lock_reversibility,
    'Can identity-locked agents recognize their own rationalization without external intervention, or is the lock structurally irreversible from within?',
    'Case studies of spontaneous rationalization recognition; identification of internal cognitive triggers vs external prompts; analysis of whether meta-cognitive awareness breaks identity lock',
    'If reversible: identity_locked classification overstates extraction — agents have latent exit capacity. If irreversible: snare classification confirmed — exit requires external shock to identity frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, conceptual, 'Whether identity-locked rationalization is internally reversible').

omega_variable(
    rationalization_base_rate,
    'What proportion of self-assessed principled revisions are actually motivated reasoning?',
    'Comparison of self-assessed revision justifications against external evaluator consensus; longitudinal tracking of revision stability and downstream consistency',
    'If base rate < 30%: extractiveness overstated — most revisions are genuine. If base rate > 70%: extractiveness understated — rationalization is dominant mode.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rationalization_base_rate, empirical, 'Base rate of rationalization among self-assessed principled revisions').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (social cost of admitting rationalization) or internalized (psychological inability to recognize rationalization)?',
    'Post-intervention suppression trajectory: if suppression persists after social costs are removed (anonymous settings, therapeutic contexts), reclassify as internalized cognitive pattern',
    'If structural: suppression is environmentally contingent and reducible through norm change. If internalized: suppression is cognitive architecture and persists across contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized cognitive pattern').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(revision_rationalization_boundary, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(revrat_tr_t0, revision_rationalization_boundary, theater_ratio, 0, 0.45).
narrative_ontology:measurement(revrat_tr_t3, revision_rationalization_boundary, theater_ratio, 3, 0.52).
narrative_ontology:measurement(revrat_tr_t6, revision_rationalization_boundary, theater_ratio, 6, 0.58).
narrative_ontology:measurement(revrat_tr_t9, revision_rationalization_boundary, theater_ratio, 9, 0.62).
narrative_ontology:measurement(revrat_tr_t12, revision_rationalization_boundary, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(revrat_be_t0, revision_rationalization_boundary, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(revrat_be_t3, revision_rationalization_boundary, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(revrat_be_t6, revision_rationalization_boundary, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(revrat_be_t9, revision_rationalization_boundary, base_extractiveness, 9, 0.66).
narrative_ontology:measurement(revrat_be_t12, revision_rationalization_boundary, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(revision_rationalization_boundary, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of recognition_dependency_mechanism (the structural requirement for external validation of internal states) and heteronomy_adoption_pathway (the mechanism by which agents internalize external standards). The boundary problem emerges because the meta-standards for evaluating revisions are themselves subject to motivated distortion, creating a second-order version of the recognition dependency problem. The constraint family structure: recognition_dependency_mechanism (mountain, ε=0.08) → heteronomy_adoption_pathway (tangled_rope, ε=0.42) → revision_rationalization_boundary (snare, ε=0.68). Each downstream constraint has higher extractiveness because it operates at a higher meta-level where verification becomes progressively more difficult.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
