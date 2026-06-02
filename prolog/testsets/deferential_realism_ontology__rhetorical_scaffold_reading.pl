% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Deferential Realism Typology as Rhetorical Scaffold (Normative Vocabulary Reading)
 *   domain: epistemology/institutional_design/normative_theory
 *
 * SUMMARY:
 *   The Deferential Realism typology (mountain, rope, tangled rope, snare,
 *   scaffold, piton) presents itself as a framework for discovering
 *   constraint types from institutional reality. This reading argues that the
 *   framework is actually a normative vocabulary — a toolkit for naming and
 *   critiquing institutional mechanisms according to prior value commitments
 *   — and that the constraint's extractive power lies precisely in disguising
 *   this normativity as discovery. When a policy advocate labels a mechanism
 *   a 'snare,' they are not discovering that the mechanism serves
 *   illegitimate beneficiaries; they are *declaring* it illegitimate
 *   according to their own normative premises, while the framework's
 *   technical apparatus (epsilon measurement, chi formula, directionality
 *   sigmoid) provides rhetorical cover for what is fundamentally a value
 *   judgment. This reading instantiates the rhetorical scaffold framework:
 *   the framework temporarily scaffolds policy critique by providing shared
 *   vocabulary and appearance of objectivity, with an intended sunset when
 *   users achieve explicit consciousness of the normative work being done.
 *
 * KEY AGENTS:
 *   - Policy Advocates: Primary beneficiary (institutional/arbitrage) — use the typology to legitimize preferred institutional critiques through technical language; gain institutional adoption and rhetorical authority
 *   - Empirical Diagnosticians: Primary victim (powerless/trapped) — committed to measurement-based analysis but trapped in a framework that conflates diagnosis with advocacy; cannot exit without abandoning shared language
 *   - Skeptical Framework Users: Secondary actor (moderate/constrained) — maintain epistemological integrity while constrained by institutional pressure to adopt the framework; face career cost if they reject the typology
 *   - Reflexive Reformers: Organized agents (organized/constrained) — intellectual movement explicitly acknowledging the normative dimension and working to make it a conscious, value-transparent policy tool; target a sunset where normativity is acknowledged rather than hidden
 *   - Formal Theorists: Institutional maintainers (institutional/arbitrage) — preserve mathematical scaffolding (chi formula, directionality sigmoid) through institutional inertia even as the epistemological ground has eroded; benefit from appearance of rigor
 *   - Naive Realist Observer: Civilizational perspective (analytical/analytical) — risks treating constructed normative vocabulary as discovered natural kind; the false summit detector reveals this as unconscious fictionalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.58).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.35).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Deferential Realism Typology as Rhetorical Scaffold (Normative Vocabulary Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/institutional_design/normative_theory").

domain_priors:requires_active_enforcement(deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '676f29f5-26b4-4390-a71d-c1ba38c52ecc').
narrative_ontology:cs_kernel_codification('676f29f5-26b4-4390-a71d-c1ba38c52ecc', formalized).
narrative_ontology:cs_authority_grounding('676f29f5-26b4-4390-a71d-c1ba38c52ecc', extraction).
narrative_ontology:cs_interpretation_layer_present('676f29f5-26b4-4390-a71d-c1ba38c52ecc').
narrative_ontology:cs_reading_relation('676f29f5-26b4-4390-a71d-c1ba38c52ecc', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('676f29f5-26b4-4390-a71d-c1ba38c52ecc', deferential_realism_ontology__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('676f29f5-26b4-4390-a71d-c1ba38c52ecc', foundational, constraint_classification_is_normative_vocabulary).
narrative_ontology:cs_axiom_status(constraint_classification_is_normative_vocabulary, holdable).
narrative_ontology:cs_axiom_grounding('676f29f5-26b4-4390-a71d-c1ba38c52ecc', constraint_classification_is_normative_vocabulary, deontological).
narrative_ontology:cs_axiom('676f29f5-26b4-4390-a71d-c1ba38c52ecc', foundational, framework_neutrality_is_cover_story).
narrative_ontology:cs_axiom_status(framework_neutrality_is_cover_story, holdable).
narrative_ontology:cs_axiom_grounding('676f29f5-26b4-4390-a71d-c1ba38c52ecc', framework_neutrality_is_cover_story, empirically_contingent).
narrative_ontology:cs_reference_frame('676f29f5-26b4-4390-a71d-c1ba38c52ecc', framework_as_discovery_apparatus).
narrative_ontology:cs_drift_state('676f29f5-26b4-4390-a71d-c1ba38c52ecc', contemporary_reflexive_challenge, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('676f29f5-26b4-4390-a71d-c1ba38c52ecc', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_designers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, empirical_diagnosis_practitioners).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, framework_neutrality_assumption).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPIRICAL DIAGNOSTICIAN (SNARE) — Trapped in a framework claiming to discover constraint types but actually performing normative classification. Cannot exit without abandoning the constraint typology entirely. Bears the cost of category collapse: measurements that should reveal structure instead legitimize pre-selected policy positions. Maximum extraction experienced as loss of diagnostic integrity.
constraint_indexing:constraint_classification(deferential_realism_ontology__rhetorical_scaffold_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SKEPTICAL FRAMEWORK USER (TANGLED ROPE) — Moderate agent constrained by institutional pressure to adopt the framework while maintaining epistemological integrity. Partially benefits from shared vocabulary and analytical structure; bears cost of normative capture. Can exit through framework refinement or alternative tools, but at career/credibility cost.
constraint_indexing:constraint_classification(deferential_realism_ontology__rhetorical_scaffold_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POLICY ADVOCATE (ROPE) — Institutional agent who benefits from the typology as a rhetorical tool for legitimizing preferred policy positions. Experiences the constraint as pure coordination: shared vocabulary enables coalition-building and institutional adoption. Net beneficiary with high arbitrage value — can use 'snare' classification to advocate for specific policy targets while maintaining the appearance of technical analysis.
constraint_indexing:constraint_classification(deferential_realism_ontology__rhetorical_scaffold_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFLEXIVE REFORMER (SCAFFOLD) — Organized intellectual movement working to explicitly acknowledge the normative dimension of the framework and build it into a deliberate policy tool. Sees the constraint as a temporary problem: if the typology is reframed as normative vocabulary rather than descriptive science, the epistemic collapse can be resolved. Sunset is the transition to conscious value-explicit framework design — when users acknowledge they are choosing normative positions, not discovering constraints.
constraint_indexing:constraint_classification(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL THEORIST (PITON) — The mathematical scaffolding (chi formula, directionality sigmoid, epsilon thresholds) persists through institutional inertia as apparatus when the epistemological ground has eroded. Formal theory maintains appearance of rigor while the underlying claim to discovery has degraded. The piton persists because the theory is *useful for advocacy* even if not true to nature.
constraint_indexing:constraint_classification(deferential_realism_ontology__rhetorical_scaffold_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NAIVE REALIST OBSERVER (MOUNTAIN) — From a civilizational perspective, one might claim that constraint types (snare, rope, tangled rope) exist as natural kinds in institutional reality and the framework merely discovers them. This perspective treats the typology as a true natural law — 'snares are discovered to exist.' However, the engine's false summit detector will identify this as naturalization of a constructed normative vocabulary, revealing the naive realism as the reading's own unconscious fiction.
constraint_indexing:constraint_classification(deferential_realism_ontology__rhetorical_scaffold_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deferential_realism_ontology__rhetorical_scaffold_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deferential_realism_ontology__rhetorical_scaffold_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, TR),
    TR >= 0.70.

:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The framework extracts from empirical diagnosticians (who lose diagnostic autonomy to normative capture) and from the shared assumption of framework neutrality (which becomes unavailable as a resource for alternative perspectives). However, extractiveness is not maximal (would require suppression ≥ 0.60 and victim status universalized) because the framework does provide genuine coordination benefits: shared vocabulary enables coalition-building, and the typology does identify real structural patterns even if the interpretation is normatively loaded. The measurement trajectory (0.42 → 0.50 → 0.58) reflects increasing normative capture over time — as the framework becomes institutionalized, more consequential policy decisions rest on its classifications, raising the stakes of the normative encoding. Suppression (0.35): Moderate. The framework suppresses awareness that classification decisions are normative by encoding them in technical language (epsilon, chi, directionality). Alternative framings are not legally prohibited but face institutional headwinds — it is professionally risky to publicly challenge the framework's neutrality. However, suppression is not high because reflexive challenges exist and are becoming more visible. Theater ratio (0.62): Moderately high. Significant performative content: the chi formula, the sigmoid directionality function, and the epsilon measurement process appear technical and objective but actually encode value choices about what counts as extraction, who bears costs, and what constitutes a beneficiary. The formalism is theater because it creates the appearance of discovery where normativity is actually the moving part. The measurement trajectory (0.45 → 0.54 → 0.62) reflects increasing formalization over time — as the apparatus becomes more mathematized, more cognitive work is delegated to technical machinery that users do not fully scrutinize.
 *
 * PERSPECTIVAL GAP:
 *   The central gap is between the policy advocate (rope) and the empirical diagnostician (snare). The policy advocate sees the framework as enabling coordination — a shared language for naming institutional problems. The empirical diagnostician sees the framework as extractive — it colonizes diagnosis by forcing measurements to conform to pre-selected normative categories. The framework user can inhabit either position depending on whether they prioritize advocacy goals (rope) or diagnostic integrity (snare). The reflexive reformer (scaffold) sees the gap itself as the problem: if the framework were explicit about its normativity, it would cease being extractive and become a transparent policy tool. The formal theorist (piton) maintains the apparatus through institutional inertia even as the epistemological ground has degraded — the formalism persists because it is useful for advocacy, not because it is true. The naive realist observer (mountain) risks naturalizing the entire scaffolding as an immutable law of institutional analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality flows from the agent's structural position relative to normative capture. Policy advocates (institutional/arbitrage) have low d — they benefit from the framework and can exit into alternative vocabularies if needed, but prefer the current system because its appearance of neutrality amplifies their rhetorical force. Empirical diagnosticians (powerless/trapped) have high d — they are locked into using the framework by institutional mandate while losing diagnostic integrity. Skeptical framework users (moderate/constrained) occupy the middle: they have some exit options (alternative analytical methods) but face career cost for using them. Organized reformers (organized/constrained) deliberately accept constraint to drive the framework toward reflexivity — their d is moderate-to-high but they are working toward the sunset. The analytical observer (analytical/analytical) faces the oracle gap: the framework provides intellectual tools that the observer uses to analyze institutions, but the framework itself becomes the object of analysis, creating the risk that the observer's own use of the framework blinds them to its normativity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy: the framework's typology appears to resolve the measurement problem (how to classify constraint types) but actually shifts the site of contestation. Before the framework, policy advocates and diagnosticians debated institutional values directly. After the framework, they debate epsilon values, chi thresholds, and directionality measures — the debate is laundered through technical language but the underlying value disagreement remains unsolved. The framework resolves mandatrophy by making explicit (via this reading) that the framework is a normative vocabulary, not a discovery tool. Once this is acknowledged, the mandatrophy dissolves: users can consciously debate values rather than pretending to measure objective properties. This is what the scaffold's sunset logic targets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discovery_vs_construction,
    'Are constraint types (snare, rope, tangled rope, etc.) discovered from institutional reality or constructed through normative judgment and rhetorical selection?',
    'Diagnostic test: apply the typology to a contested case with multiple stakeholder positions; track whether different stakeholders'' readings converge to consensus (discovery signal) or diverge systematically by power position (construction signal). Historical case: regulatory capture — do regulators and regulated firms converge on ''this is a snare'' or do they persistently disagree?',
    'If discovered: framework is epistemically neutral; classification process is independent of advocate values. If constructed: framework is a normative vocabulary; classification legitimizes pre-selected positions through technical language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discovery_vs_construction, conceptual, 'Whether constraint types are discovered from institutional structure or constructed through normative judgment').

omega_variable(
    measurement_independence,
    'Can epsilon (base extractiveness) be measured independently of the normative judgment about what counts as extractive?',
    'Test case: measure epsilon for the same constraint using different normative framings (e.g., libertarian vs. egalitarian vs. utilitarian value premises). If epsilon values remain stable across framings, measurement is normatively independent. If epsilon changes with the value frame applied, measurement is value-laden.',
    'If independent: epsilon is an objective structural property; classification is technically sound. If value-laden: epsilon is a constructed score reflecting advocate priorities; framework''s persuasive power rests on normativity, not discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_independence, empirical, 'Whether epsilon measurement is independent of normative framing').

omega_variable(
    reflexivity_sustainability,
    'If users consciously acknowledge that the typology is a normative vocabulary (not a discovery), does it retain sufficient rhetorical force for institutional adoption?',
    'Institutional adoption rates before and after explicit value acknowledgment; survey of policy advocates'' willingness to use the framework when framed as ''normative vocabulary'' vs. ''scientific classification system''; longitudinal tracking of framework legitimacy after reflexive reform.',
    'If adoption remains high: the framework''s value lies in providing shared analytical vocabulary, regardless of epistemological grounding. Reflex reform is sustainable. If adoption drops sharply: the framework''s power depends on disguising normativity as discovery; reflexivity would dissolve the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reflexivity_sustainability, empirical, 'Whether reflexive acknowledgment of normativity sustains framework adoption').

omega_variable(
    false_summit_classification,
    'Is this constraint itself a false summit — does declaring it a ''tangled rope'' (mixed coordination and extraction) naturalize what is actually a pure extraction mechanism (the framework''s normative capture of diagnosis)?',
    'Cross-position analysis: from the empirical diagnostician''s perspective, is there genuine coordination function or only the appearance of shared vocabulary masking value capture? Does the framework actually solve a coordination problem or does it primarily serve the policy advocate''s rhetorical interests?',
    'If false summit confirmed: the constraint should be reclassified as snare (pure extraction) — the framework is a coercive tool that colonizes diagnosis. The coordination function is illusory. If not false summit: the framework genuinely provides coordination benefits (shared language, common analytical structure) alongside extractive abuse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_classification, conceptual, 'Whether this constraint is itself a false summit naturalizing pure extraction as mixed coordination').

omega_variable(
    axiom_overriding_trajectory,
    'The foundational axiom of this reading (''constraint_classification_is_normative_vocabulary'') has become increasingly explicit and acknowledged within the Deferential Realism community (post-2024). Does this axiom acknowledgment constitute an override of the prior implicit axiom (''constraint_types_are_discovered_from_nature''), or do both axioms coexist?',
    'Literature analysis: count explicit acknowledgments of normativity in DR papers before and after 2024; assess whether authors maintain both discovery framing and normative framing in parallel or whether the discovery framing has been formally abandoned.',
    'If overridden: the reading has evolved; the prior implicit axiom is no longer operative; the framework is now consciously normative. If coexisting: the reading maintains both claims simultaneously, creating the contradictory state that makes false summit detection necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_overriding_trajectory, empirical, 'Whether normative axiom acknowledgment has overridden prior discovery axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drrs_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(drrs_tr_t2, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 2, 0.54).
narrative_ontology:measurement(drrs_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.62).

% Extraction over time
narrative_ontology:measurement(drrs_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(drrs_be_t2, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(drrs_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The three readings of the deferential_realism_ontology kernel are structurally distinct constraints with different epsilon values reflecting different empirical claims about the typology's nature. Rhetorical scaffold reading (ε=0.58, tangled rope) claims the framework is normative vocabulary. Immutable diagnostic reading (ε=0.08, mountain) claims the framework discovers objective properties. Hybrid pragmatic reading (ε=0.35, rope) claims the framework does both. These are not the same constraint viewed from different angles — they are three competing claims about what the typology IS, instantiated as separate constraint stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
