% ============================================================================
% CONSTRAINT STORY: absolutism_attachment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_absolutism_attachment, []).

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
 *   constraint_id: absolutism_attachment
 *   human_readable: Absolutism Attachment in Dirty Hands Theory
 *   domain: political_philosophy/normative_ethics/applied_ethics
 *
 * SUMMARY:
 *   The absolutism attachment in dirty hands theory creates a structural
 *   tension between the framework's explicit rejection of moral absolutism
 *   and its functional dependence on absolutist residue to ground moral
 *   seriousness. Sandsmark's diagnosis identifies this as incoherence:
 *   theorists simultaneously deny that any acts are absolutely prohibited
 *   while insisting that certain violations (torture, civilian targeting,
 *   promise-breaking) carry a moral remainder that persists even when the act
 *   is justified. This ambiguous attitude coordinates a genuine problem —
 *   preserving the phenomenology of moral tragedy in post-absolutist ethics —
 *   while extracting from theoretical consistency and suppressing alternative
 *   frameworks (consequentialist, virtue-theoretic, care-ethical) that could
 *   address political wrongdoing without the absolutism contradiction. The
 *   constraint exhibits rising theater ratio (0.42 → 0.58) as the absolutist
 *   vocabulary persists despite explicit disavowal of its metaphysical
 *   commitments, and rising suppression (0.45 → 0.62) as dirty hands
 *   dominance in political ethics curricula and publication venues
 *   marginalizes non-absolutist approaches. The framework is not a false
 *   summit — the coordination function is real — but the extraction is also
 *   real, making this a paradigmatic tangled rope at the analytical level.
 *
 * KEY AGENTS:
 *   - Dirty Hands Framework: Primary beneficiary (institutional/arbitrage) — the ambiguous attitude preserves centrality by maintaining compatibility with contemporary moral philosophy while retaining tragic dimension
 *   - Theoretical Consistency Standard: Primary victim (powerless/identity_locked) — abstract standard that cannot exit its identity frame; bears full cost of accommodating contradiction
 *   - Consequentialist Theorists: Secondary victim (moderate/constrained) — face career cost of rejecting framework but gain coordination through shared problem space
 *   - Pluralist Theorists: Mixed position (moderate/constrained) — constrained by framework dominance but benefit from validation of value incommensurability
 *   - Experimental Ethics Coalition: Organized agents (organized/mobile) — building alternative empirical grounding for moral seriousness with sunset logic
 *   - Absolutism Vocabulary: Institutional artifact (institutional/arbitrage) — degraded lexicon maintained theatrically despite functional atrophy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination and genuine extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(absolutism_attachment, 0.48).
domain_priors:suppression_score(absolutism_attachment, 0.62).
domain_priors:theater_ratio(absolutism_attachment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(absolutism_attachment, extractiveness, 0.48).
narrative_ontology:constraint_metric(absolutism_attachment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(absolutism_attachment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(absolutism_attachment, tangled_rope).
narrative_ontology:human_readable(absolutism_attachment, "Absolutism Attachment in Dirty Hands Theory").
narrative_ontology:topic_domain(absolutism_attachment, "political_philosophy/normative_ethics/applied_ethics").

domain_priors:requires_active_enforcement(absolutism_attachment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(absolutism_attachment, dirty_hands_framework).
narrative_ontology:constraint_beneficiary(absolutism_attachment, moral_seriousness_discourse).
narrative_ontology:constraint_victim(absolutism_attachment, theoretical_consistency).
narrative_ontology:constraint_victim(absolutism_attachment, consequentialist_theorists).
narrative_ontology:constraint_victim(absolutism_attachment, pluralist_theorists).
narrative_ontology:constraint_vindicates(absolutism_attachment, moral_tragedy_irreducibility).
narrative_ontology:constraint_vindicates(absolutism_attachment, deontological_residue_necessity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THEORETICAL CONSISTENCY (SNARE) — Identity-locked rather than trapped: the standard could in principle be relaxed (biographical rope from identity_locked), but doing so would require abandoning the professional identity constituted through philosophical rigor. Bears full cost of the ambiguous attitude — forced to accommodate simultaneous rejection and retention of absolutism without resolution mechanism. Maximum extraction from an abstract standard that cannot organize or exit its identity frame.
constraint_indexing:constraint_classification(absolutism_attachment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSEQUENTIALIST THEORIST (TANGLED ROPE) — Constrained by disciplinary norms requiring engagement with dirty hands literature to address political ethics, but also benefits from the framework's provision of a shared problem space. Faces career cost of rejecting the absolutism attachment (marginalization from core debates) but gains coordination through common vocabulary. Mixed experience: genuine coordination function alongside asymmetric extraction.
constraint_indexing:constraint_classification(absolutism_attachment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DIRTY HANDS FRAMEWORK (ROPE) — Primary beneficiary. The ambiguous attitude preserves the framework's centrality: rejecting absolutism maintains compatibility with contemporary moral philosophy while retaining absolutism's emotional force preserves the tragic dimension that distinguishes dirty hands from mere cost-benefit analysis. Experiences the constraint as pure coordination — the ambiguity solves the problem of maintaining relevance across incompatible metaethical commitments.
constraint_indexing:constraint_classification(absolutism_attachment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PLURALIST THEORIST (TANGLED ROPE) — Constrained by the framework's dominance in political ethics curricula and publication venues, but benefits from the absolutism attachment's validation of value incommensurability claims. The ambiguous attitude provides conceptual resources (moral remainder, tragic conflict) while imposing costs (must navigate the absolutism contradiction). Moderate extraction — some agency, some benefit, significant constraint.
constraint_indexing:constraint_classification(absolutism_attachment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: EXPERIMENTAL ETHICS COALITION (SCAFFOLD) — Organized agents (empirical moral psychology, behavioral ethics, naturalized epistemology) see the absolutism attachment as a transitional artifact of armchair philosophy's dominance. As empirical methods mature, the ambiguous attitude's function (preserving intuition-driven theory) becomes obsolete — experimental data on moral judgment, emotional response, and decision-making under uncertainty provide alternative grounding for moral seriousness without requiring absolutist residue. Estimated sunset: 15-25 years as empirical ethics gains institutional foothold.
constraint_indexing:constraint_classification(absolutism_attachment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ABSOLUTISM VOCABULARY (PITON) — The language of absolute prohibitions persists in dirty hands discourse despite explicit rejection of absolutism's metaphysical commitments. Terms like 'violation,' 'transgression,' 'moral remainder' carry absolutist connotations that theorists simultaneously disavow and depend upon. The vocabulary is maintained theatrically — its function (marking moral seriousness) has atrophied into performance, but no alternative lexicon has replaced it. Theater ratio reflects this degraded state.
constraint_indexing:constraint_classification(absolutism_attachment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the ambiguous attitude coordinates a genuine problem (preserving moral seriousness in post-absolutist ethics) while extracting from theoretical consistency. The constraint is not a false summit — the coordination function is real (dirty hands theory does solve the problem of articulating political tragedy). But the extraction is also real — the framework suppresses alternative approaches (consequentialist, virtue-theoretic, care-ethical) that could address political wrongdoing without the absolutism contradiction. Tangled rope at the analytical level: both coordination and extraction are structurally present.
constraint_indexing:constraint_classification(absolutism_attachment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(absolutism_attachment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(absolutism_attachment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(absolutism_attachment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(absolutism_attachment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(absolutism_attachment, TR),
    TR >= 0.70.

:- end_tests(absolutism_attachment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The dirty hands framework captures theoretical centrality and institutional dominance during the post-absolutist transition, but the extraction is not maximal — the framework does solve a genuine coordination problem (articulating political tragedy without metaphysical absolutism). The value reflects that the career and publication asymmetry, while real, coexists with legitimate theoretical function. Suppression (0.62): Moderate-high. Significant barriers to alternative frameworks include dirty hands dominance in political ethics curricula, publication bias toward tragic-conflict framing, and career risk of challenging the framework's core commitments. But suppression is not total — consequentialist, virtue-theoretic, and care-ethical approaches do exist and publish, and experimental ethics is building institutional foothold. Theater ratio (0.58): Moderate-high. The absolutist vocabulary (violation, transgression, moral remainder) persists despite explicit rejection of absolutism's metaphysical commitments. Theorists use terms that carry absolutist connotations while disavowing the underlying metaphysics. The theater has increased over the interval as the gap between explicit metaethics and implicit vocabulary has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates tangled rope classification from multiple perspectives, with the critical gap between the framework's self-perception (rope — pure coordination of moral seriousness) and the victim perspectives (snare or tangled rope — extraction from consistency and suppression of alternatives). The dirty hands framework sees the ambiguous attitude as solving a legitimate problem: how to preserve moral tragedy without metaphysical absolutism. Consequentialist and pluralist theorists see mixed coordination and extraction: the framework provides shared vocabulary but suppresses their approaches. Theoretical consistency sees pure extraction: forced to accommodate contradiction with no resolution mechanism. The experimental ethics coalition sees a temporary problem with a sunset: empirical methods will displace intuition-driven theory. The absolutism vocabulary sees its own degradation: maintained theatrically despite functional atrophy. The analytical observer sees genuine tangled rope: both coordination (the framework does articulate political tragedy) and extraction (the framework does suppress alternatives and impose contradiction) are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   The dirty hands framework is the primary beneficiary — it experiences low effective extraction because the ambiguous attitude solves its coordination problem (maintaining relevance across incompatible metaethical commitments). Theoretical consistency is the primary victim with identity_locked exit — it is structurally mobile (the standard could be relaxed) but functionally trapped because relaxing it would require abandoning the professional identity constituted through philosophical rigor. This produces high directionality and high effective extraction. Consequentialist and pluralist theorists are secondary victims with constrained exit — they face career costs for rejecting the framework but have agency and gain some coordination benefit. The experimental ethics coalition has mobile exit — they are building alternative institutional pathways and see a sunset. The absolutism vocabulary has arbitrage exit — it persists through inertia but could be replaced if an alternative lexicon emerged. The analytical observer has analytical exit and sees both coordination and extraction as structurally real.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled rope is the structurally accurate classification when both coordination and extraction are irreducibly present. The dirty hands framework is not a false summit (mountain naturalization) — it genuinely coordinates the problem of post-absolutist moral seriousness. It is not pure rope — the suppression of alternatives and extraction from consistency are real. It is not pure snare — the coordination function is not mere cover. It is not scaffold — the framework shows no sunset logic from its own perspective (though the experimental ethics coalition sees one). It is not piton — the framework retains substantial function despite rising theater ratio. The tangled rope classification captures the structural reality: a constraint that both coordinates and extracts, where the coordination is genuine and the extraction is also genuine, and where the two functions are inseparable because the ambiguous attitude is the mechanism that enables both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutism_necessity,
    'Does the dirty hands framework genuinely require absolutist residue to capture moral tragedy, or is the attachment a contingent artifact of the framework''s Weberian origins?',
    'Comparative analysis of non-absolutist accounts of political wrongdoing (consequentialist regret, virtue-theoretic failure, care-ethical betrayal) — do they preserve the phenomenology of moral seriousness without absolutist commitments?',
    'If absolutism is necessary: the ambiguous attitude is unavoidable coordination (more rope-like from more perspectives). If contingent: the attachment is extractive suppression of alternatives (more snare-like from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutism_necessity, conceptual, 'Whether dirty hands requires absolutist residue or merely inherited it').

omega_variable(
    sandsmark_diagnosis_validity,
    'Is Sandsmark correct that the ambiguous attitude is incoherent, or does the ambiguity reflect a genuine structural feature of post-absolutist moral thought?',
    'Philosophical analysis of whether simultaneous rejection and retention of absolutism is a contradiction (incoherent) or a dialectical tension (coherent but unstable). Empirical check: do theorists who explicitly reject absolutism nonetheless exhibit absolutist commitments in their judgments about cases?',
    'If incoherent: the constraint is extractive suppression of clarity (snare from more perspectives). If coherent tension: the constraint is legitimate coordination of incompatible commitments (rope from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sandsmark_diagnosis_validity, conceptual, 'Whether the ambiguous attitude is contradiction or dialectical tension').

omega_variable(
    empirical_ethics_displacement,
    'Will empirical moral psychology displace intuition-driven dirty hands theory, or will the two approaches coexist as complementary methods?',
    'Longitudinal tracking of publication patterns, citation networks, and curriculum adoption in political philosophy and applied ethics programs. Does experimental ethics gain market share at dirty hands theory''s expense, or do both grow?',
    'If displacement: scaffold perspective confirmed — the absolutism attachment has a real sunset. If coexistence: scaffold perspective is aspirational — the constraint persists as a stable feature of one methodological tradition among others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_ethics_displacement, empirical, 'Whether empirical ethics will displace or coexist with dirty hands theory').

omega_variable(
    alternative_frameworks_suppression,
    'Are consequentialist, virtue-theoretic, and care-ethical approaches to political wrongdoing genuinely suppressed by dirty hands dominance, or do they fail to gain traction for independent reasons (lack of compelling accounts, insufficient institutional support)?',
    'Comparative institutional analysis: publication rates, conference representation, curriculum inclusion for dirty hands vs alternative frameworks. Control for quality by expert assessment of theoretical sophistication. If alternatives are high-quality but low-representation, suppression is real. If low-quality, suppression claim is weak.',
    'If suppression is real: higher extractiveness, more snare-like from victim perspectives. If alternatives fail independently: lower extractiveness, more rope-like from beneficiary perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_frameworks_suppression, empirical, 'Whether dirty hands framework actively suppresses alternatives or they fail independently').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(absolutism_attachment, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(absatt_theater_origin, absolutism_attachment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(absatt_theater_mid, absolutism_attachment, theater_ratio, 15, 0.5).
narrative_ontology:measurement(absatt_theater_current, absolutism_attachment, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(absatt_extract_origin, absolutism_attachment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(absatt_extract_mid, absolutism_attachment, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(absatt_extract_current, absolutism_attachment, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(absatt_suppress_origin, absolutism_attachment, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(absatt_suppress_mid, absolutism_attachment, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(absatt_suppress_current, absolutism_attachment, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(absolutism_attachment, identity_coordination).

% DUAL FORMULATION NOTE:
% The absolutism attachment is downstream of logical_coherence_paradox (the structural impossibility of coherently rejecting and retaining absolutism) and moral_remainder_requirement (the framework's dependence on remainder to mark moral seriousness). These upstream constraints have their own extractiveness values reflecting their specific structural tensions; the absolutism attachment has its own extractiveness reflecting the career and institutional asymmetry created by the ambiguous attitude's dominance in political ethics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
