% ============================================================================
% CONSTRAINT STORY: logical_coherence_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_logical_coherence_paradox, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: logical_coherence_paradox
 *   human_readable: Logical Coherence Paradox in Dirty Hands Ethics
 *   domain: political_philosophy/normative_ethics/applied_ethics
 *
 * SUMMARY:
 *   The logical coherence paradox in dirty hands ethics claims that political
 *   actors can face situations where an act is simultaneously categorically
 *   wrong (violates a moral absolute) and yet right to perform (required by
 *   political necessity or consequentialist calculation). This constraint has
 *   structured political philosophy since Machiavelli but crystallized as an
 *   explicit research program with Walzer's 'Political Action: The Problem of
 *   Dirty Hands' (1973). The paradox generates a persistent literature
 *   attempting to resolve or domesticate the contradiction through various
 *   strategies: internal/external conflict (the actor experiences guilt even
 *   while acting rightly), role-relative obligations (political roles have
 *   different moral requirements), tragic dilemmas (some situations have no
 *   right answer), or meta-ethical pluralism (different moral frameworks
 *   apply to different domains). The constraint exhibits tangled rope
 *   structure: it coordinates a research program and professional discourse
 *   while extracting from those who maintain logical consistency standards.
 *   The theater ratio (0.38) reflects that much engagement with the paradox
 *   is performative — papers rehearse the same moves without advancing toward
 *   resolution — but the theater is lower than in purely extractive academic
 *   constraints because genuine philosophical work occurs within the
 *   tradition. The suppression trajectory shows enforcement intensification:
 *   early resistance to the paradox (1950s-1960s) gave way to professional
 *   normalization (1970s-1990s) where treating the paradox as legitimate
 *   became a requirement for participation in political philosophy.
 *
 * KEY AGENTS:
 *   - Dirty Hands Theorists: Primary beneficiary (institutional/arbitrage) — the paradox generates a research program, secures positions, produces citations, establishes a tradition
 *   - Logical Consistency Standards: Primary victim (powerless/trapped) — abstract epistemic norm that cannot organize or exit; bears full cost of the incoherence
 *   - Graduate Students in Ethics: Secondary victim (powerless/identity_locked) — professionally bound to master the literature; career depends on treating paradox as legitimate rather than as category error
 *   - Deontological Critics: Mixed position (moderate/constrained) — constrained by professional norms requiring engagement but also benefit from the paradox as a foil for demonstrating consistency
 *   - Formal Ethics Coalition: Organized agents (organized/mobile) — deontic logicians, decision theorists building alternative frameworks that dissolve the paradox through formalization; see a sunset
 *   - Journal Editors: Institutional actors (institutional/constrained) — benefit from the paradox as a source of submissions but also constrained by field norms perpetuating the literature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(logical_coherence_paradox, 0.48).
domain_priors:suppression_score(logical_coherence_paradox, 0.62).
domain_priors:theater_ratio(logical_coherence_paradox, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(logical_coherence_paradox, extractiveness, 0.48).
narrative_ontology:constraint_metric(logical_coherence_paradox, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(logical_coherence_paradox, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(logical_coherence_paradox, tangled_rope).
narrative_ontology:human_readable(logical_coherence_paradox, "Logical Coherence Paradox in Dirty Hands Ethics").
narrative_ontology:topic_domain(logical_coherence_paradox, "political_philosophy/normative_ethics/applied_ethics").

domain_priors:requires_active_enforcement(logical_coherence_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(logical_coherence_paradox, dirty_hands_theorists).
narrative_ontology:constraint_beneficiary(logical_coherence_paradox, political_realism_tradition).
narrative_ontology:constraint_victim(logical_coherence_paradox, logical_consistency_standards).
narrative_ontology:constraint_victim(logical_coherence_paradox, deontological_frameworks).
narrative_ontology:constraint_victim(logical_coherence_paradox, graduate_students_in_ethics).
narrative_ontology:constraint_vindicates(logical_coherence_paradox, moral_complexity_irreducibility).
narrative_ontology:constraint_vindicates(logical_coherence_paradox, political_exceptionalism_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRADUATE STUDENT (SNARE) — Identity-locked within the professional training pipeline that requires mastery of dirty hands literature. Cannot exit without abandoning career trajectory. Bears full cost of the logical incoherence: must learn to navigate contradictory frameworks, write dissertations defending positions that violate basic logical principles, and internalize the professional norm that accepting paradox is sophistication rather than failure. The identity lock is professional: becoming an ethicist in the political philosophy tradition requires fluency in dirty hands discourse, and the student's career depends on treating the paradox as a legitimate research program rather than a category error.
constraint_indexing:constraint_classification(logical_coherence_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: DEONTOLOGICAL CRITIC (TANGLED ROPE) — Constrained by professional norms requiring engagement with dirty hands literature but also benefits from the paradox as a foil for demonstrating deontological consistency. Experiences both coordination (the paradox provides a clear target for critique, generates publication opportunities, structures debate) and extraction (must treat incoherent positions as worthy of serious engagement, expend intellectual labor refuting claims that violate basic logic). The constraint coordinates the field's discourse while extracting from those who maintain logical standards.
constraint_indexing:constraint_classification(logical_coherence_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DIRTY HANDS THEORIST (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: the paradox generates a research program, secures academic positions, produces citation networks, and establishes a tradition. The logical incoherence is not a cost but a feature — it ensures the problem remains perpetually open, immune to resolution, and thus a reliable source of professional capital. Arbitrage exit options: can move between political philosophy, applied ethics, and political theory without losing the paradox's professional value.
constraint_indexing:constraint_classification(logical_coherence_paradox, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LOGICAL CONSISTENCY STANDARD (MOUNTAIN) — From the analytical perspective, the constraint appears as an immutable feature of moral reasoning: genuine moral dilemmas involve incommensurable values, and any attempt to resolve them within a single logical framework will produce apparent contradictions. The paradox is not a failure of theory but a reflection of moral reality's irreducible complexity. This is a false summit candidate: the 'irreducible complexity' framing naturalizes what is actually a choice to privilege political realism over logical coherence, and the beneficiary structure (dirty hands theorists) reveals the naturalization's function.
constraint_indexing:constraint_classification(logical_coherence_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: FORMAL ETHICS COALITION (SCAFFOLD) — Organized agents (formal ethicists, decision theorists, deontic logicians) see the paradox as a temporary coordination failure with a sunset: formal frameworks (deontic logic, multi-valued logics, preference orderings over moral theories) are building alternative pathways that dissolve the apparent contradiction by making explicit the hidden parameters (scope restrictions, role-relative obligations, meta-ethical commitments). The coalition has mobile exit options and sees the dirty hands literature as a transitional phase before formal methods mature. Estimated sunset: 20-30 years as formal ethics becomes standard training.
constraint_indexing:constraint_classification(logical_coherence_paradox, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: JOURNAL EDITOR (TANGLED ROPE) — Constrained by the field's norms and citation networks but also benefits from the paradox as a reliable source of submissions and citations. Experiences coordination (the paradox structures a recognizable research program, enables editorial decisions based on engagement with canonical texts) and extraction (must publish papers that treat logical incoherence as sophistication, perpetuate a literature that resists resolution). The editor cannot simply reject dirty hands papers without professional cost, but also gains from the paradox's generativity.
constraint_indexing:constraint_classification(logical_coherence_paradox, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(logical_coherence_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(logical_coherence_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(logical_coherence_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(logical_coherence_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(logical_coherence_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The paradox extracts from logical consistency standards and from those professionally required to engage with incoherent positions. The extraction is substantial because the constraint perpetuates a literature that resists resolution — the logical incoherence is not a bug but a feature ensuring the problem remains open. However, extraction is not maximal because genuine philosophical work occurs within the tradition, and some theorists use the paradox productively to explore moral complexity. Suppression (0.62): Moderate-high. Significant barriers to rejecting the paradox include professional norms (treating dirty hands as a legitimate research program is required for participation in political philosophy), citation networks (canonical texts must be engaged), graduate training (students must master the literature), and editorial gatekeeping (papers dismissing the paradox as incoherent face rejection). The suppression intensified over the interval as the paradox became normalized. Theater ratio (0.38): Moderate. Much engagement with the paradox is performative — papers rehearse internal/external conflict, role-relative obligations, or tragic dilemmas without advancing toward resolution. However, theater is lower than in purely extractive constraints because genuine conceptual work occurs: some theorists use the paradox to explore moral phenomenology, political realism, or meta-ethical pluralism in ways that generate insight even if they do not resolve the contradiction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same logical structure appears differently depending on the observer's position. Dirty hands theorists see coordination (Rope) — the paradox structures a productive research program. Graduate students see extraction (Snare) — they are identity-locked within a training pipeline that requires mastery of incoherent positions. Deontological critics see mixed coordination and extraction (Tangled Rope) — the paradox both structures debate and extracts from those maintaining logical standards. The formal ethics coalition sees a temporary problem with a sunset (Scaffold) — formalization will dissolve the paradox. The analytical observer risks seeing an immutable feature of moral reality (Mountain) — but the beneficiary structure reveals this as a false summit naturalizing political realism. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' The presheaf over the observation site captures all six perspectives as legitimate readings of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Dirty hands theorists are primary beneficiaries with arbitrage exit options — they experience low effective extraction because the paradox generates professional capital and they can move between subfields without losing its value. Their directionality is near 0.0 (full beneficiary). Graduate students are victims with identity_locked exit — they are structurally mobile (could leave academia) but identity-fused with the professional training pipeline. Their directionality is high (near 0.85) because they bear the cost of internalizing incoherence as a career requirement. Deontological critics are mixed: they are victims (must engage with incoherent positions) but also benefit (the paradox provides a foil for demonstrating consistency). Their directionality is moderate (around 0.45). Logical consistency standards are the abstract victim with no exit and no advocate — directionality is maximal (1.0). The formal ethics coalition has mobile exit options and sees a sunset — their directionality is low (around 0.25) because they have agency and an alternative framework. Journal editors are constrained beneficiaries — they benefit from the paradox as a source of content but are also bound by field norms; directionality is moderate (around 0.40).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the paradox's persistence is not a failure of philosophical progress but a structural feature of its professional function. The paradox generates a research program precisely because it resists resolution — if it were dissolved, the program would end. The mandate (explore moral complexity in political action) has not outlived its function from the beneficiaries' perspective, but has outlived its function from the victims' perspective (logical consistency standards, graduate students forced to internalize incoherence). The mandatrophy is resolved by recognizing that 'function' is observer-relative: what appears as productive complexity from one position appears as extractive incoherence from another. The formal ethics coalition's scaffold perspective suggests a genuine sunset: as formal methods mature, the paradox will be revealed as a pseudo-problem arising from under-specified parameters. But the sunset is generational, and in the meantime the constraint continues to coordinate and extract.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internal_external_distinction,
    'Does the internal/external conflict resolution (Walzer, Coady) genuinely dissolve the logical contradiction, or does it merely relocate the incoherence to a different level of analysis?',
    'Formal logical analysis of internal/external conflict formulations; identification of whether the ''two perspectives'' are genuinely independent or whether the external judgment presupposes the internal judgment''s validity',
    'If genuinely dissolved: dirty hands is a coordination problem (Rope from more perspectives). If relocated: the paradox is an extraction mechanism that survives by shifting the contradiction rather than resolving it (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internal_external_distinction, conceptual, 'Whether internal/external conflict resolution dissolves or relocates the contradiction').

omega_variable(
    political_exceptionalism_warrant,
    'Is there a principled reason why political action should be exempt from logical consistency requirements that apply to other domains of practical reasoning?',
    'Cross-domain comparison: do other high-stakes practical domains (medicine, engineering, military strategy) accept logical incoherence as legitimate, or is political philosophy uniquely permissive?',
    'If warranted: the mountain perspective is correct — political reality is genuinely exceptional. If unwarranted: the paradox is a false summit naturalizing a professional norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_exceptionalism_warrant, conceptual, 'Whether political action warrants exemption from logical consistency').

omega_variable(
    formalization_resistance,
    'Is the dirty hands literature''s resistance to formal methods a principled commitment to moral phenomenology, or a defensive strategy to preserve a research program that cannot survive formalization?',
    'Historical analysis of responses to formal ethics proposals; identification of whether resistance is grounded in substantive arguments about moral experience or in professional boundary maintenance',
    'If principled: the scaffold perspective is premature — formal methods cannot capture the phenomenon. If defensive: the scaffold sunset is real, and resistance is extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalization_resistance, empirical, 'Whether resistance to formalization is principled or defensive').

omega_variable(
    false_summit_naturalness,
    'Is the logical coherence paradox a genuine feature of moral reality (Mountain), or a constructed constraint that benefits dirty hands theorists by naturalizing political realism (False Summit)?',
    'Cross-cultural and cross-temporal analysis: do non-Western ethical traditions and pre-modern political philosophy exhibit the same paradox, or is it specific to post-Machiavellian political realism? Formal logical analysis: can the paradox be dissolved by making implicit parameters explicit?',
    'If genuine Mountain: the analytical perspective is correct and the paradox is irreducible. If False Summit: the beneficiary structure (dirty hands theorists, political realism tradition) reveals the naturalization''s function, and the constraint should reclassify to Tangled Rope or Snare from the analytical perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalness, conceptual, 'Whether the paradox is a natural feature of moral reality or a constructed constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(logical_coherence_paradox, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(logic_coh_theater_1950, logical_coherence_paradox, theater_ratio, 0, 0.25).
narrative_ontology:measurement(logic_coh_theater_1970, logical_coherence_paradox, theater_ratio, 20, 0.32).
narrative_ontology:measurement(logic_coh_theater_1990, logical_coherence_paradox, theater_ratio, 40, 0.38).
narrative_ontology:measurement(logic_coh_theater_2010, logical_coherence_paradox, theater_ratio, 60, 0.38).

% Extraction over time
narrative_ontology:measurement(logic_coh_extract_1950, logical_coherence_paradox, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(logic_coh_extract_1970, logical_coherence_paradox, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(logic_coh_extract_1990, logical_coherence_paradox, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(logic_coh_extract_2010, logical_coherence_paradox, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(logic_coh_suppress_1950, logical_coherence_paradox, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(logic_coh_suppress_1970, logical_coherence_paradox, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(logic_coh_suppress_1990, logical_coherence_paradox, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(logic_coh_suppress_2010, logical_coherence_paradox, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(logical_coherence_paradox, identity_coordination).
narrative_ontology:affects_constraint(logical_coherence_paradox, trolley_problem_industry).
narrative_ontology:affects_constraint(logical_coherence_paradox, moral_luck_literature).
narrative_ontology:affects_constraint(logical_coherence_paradox, political_realism_tradition).

% DUAL FORMULATION NOTE:
% The logical coherence paradox is one component of a larger constraint family in applied ethics. The trolley problem industry shares the same structural pattern (a philosophical puzzle that generates a research program by resisting resolution), but with different beneficiaries and a different domain. Moral luck literature is downstream (dirty hands cases are often framed as moral luck cases). Political realism tradition is upstream (the paradox vindicates the claim that politics operates under different moral rules than private life).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(logical_coherence_paradox, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
