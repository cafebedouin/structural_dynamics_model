% ============================================================================
% CONSTRAINT STORY: evolutionary_mechanism_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evolutionary_mechanism_sufficiency, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: evolutionary_mechanism_sufficiency
 *   human_readable: Evolutionary Mechanism Sufficiency
 *   domain: evolutionary_biology/theoretical_framework
 *
 * SUMMARY:
 *   The claim that mutation, natural selection, and genetic drift suffice to
 *   explain all major evolutionary patterns has structured evolutionary
 *   biology for seven decades. This constraint exhibits a classic pattern:
 *   genuine coordination function (the neo-Darwinian framework does enable
 *   research coordination and cumulative science) combined with systematic
 *   extraction (suppression of alternative mechanisms and empirical
 *   anomalies). The constraint's theater_ratio (0.68) reflects that
 *   institutional consensus is maintained through narrative assertion and
 *   pedagogical repetition rather than rigorous validation: undergraduate
 *   education emphasizes adaptationist narrative, professional reviews
 *   dismiss alternatives via terminology ('just another form of selection')
 *   rather than principled analysis, and textbooks present the sufficiency
 *   claim as established fact despite 70+ years of documented empirical
 *   challenges. The theater has increased from 0.45 at the early Modern
 *   Synthesis period (when mechanisms were genuinely novel) to 0.72 in
 *   contemporary pedagogy (when the claim is institutionally enforced rather
 *   than experimentally defended). The extractiveness trajectory shows
 *   moderate increase (0.28 → 0.38) as the constraint's entrenchment grows
 *   despite persistent empirical anomalies. This is a tangled rope in the
 *   analytical definition: genuine coordination function (Rope perspective)
 *   combined with asymmetric extraction of alternative mechanism research
 *   (Snare perspective) and mixed treatment of moderate researchers (Tangled
 *   Rope perspective), all enforced through institutional power rather than
 *   logical necessity.
 *
 * KEY AGENTS:
 *   - Neo-Darwinian Research Establishment: Primary beneficiary (institutional/arbitrage) — maintains paradigm monopoly, controls funding allocation, gatekeeps publication. Experiences constraint as coordination enabling research continuity.
 *   - Empirical Anomaly: Primary victim (powerless/trapped) — observations inconsistent with neo-Darwinian predictions are systematically reinterpreted as noise or local exception. No institutional pathway for anomaly resolution outside the establishment paradigm.
 *   - Alternative Mechanism Researcher: Secondary victim (moderate/constrained) — faces funding barriers, journal review bias, and reputational cost of challenging orthodoxy. Also benefits from shared research infrastructure and terminology inherited from neo-Darwinian framework.
 *   - Extended Synthesis Coalition: Organized actors (organized/constrained) — building integrative frameworks accommodating neo-Darwinian mechanisms while explaining anomalies through additional principles (developmental constraints, epigenetic inheritance, niche construction). Sees constraint as temporary sunset.
 *   - Theoretical Coherence: Secondary victim (powerless/trapped) — frameworks that would resolve anomalies more elegantly are suppressed in favor of increasingly elaborate neo-Darwinian accommodations.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional-political constraints as necessary features of the scientific process itself.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolutionary_mechanism_sufficiency, 0.38).
domain_priors:suppression_score(evolutionary_mechanism_sufficiency, 0.52).
domain_priors:theater_ratio(evolutionary_mechanism_sufficiency, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolutionary_mechanism_sufficiency, extractiveness, 0.38).
narrative_ontology:constraint_metric(evolutionary_mechanism_sufficiency, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(evolutionary_mechanism_sufficiency, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolutionary_mechanism_sufficiency, tangled_rope).
narrative_ontology:human_readable(evolutionary_mechanism_sufficiency, "Evolutionary Mechanism Sufficiency").
narrative_ontology:topic_domain(evolutionary_mechanism_sufficiency, "evolutionary_biology/theoretical_framework").

domain_priors:requires_active_enforcement(evolutionary_mechanism_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolutionary_mechanism_sufficiency, neo_darwinian_research_establishment).
narrative_ontology:constraint_beneficiary(evolutionary_mechanism_sufficiency, mechanistic_explanation_paradigm).
narrative_ontology:constraint_victim(evolutionary_mechanism_sufficiency, alternative_mechanism_research).
narrative_ontology:constraint_victim(evolutionary_mechanism_sufficiency, empirical_anomaly_resolution).
narrative_ontology:constraint_victim(evolutionary_mechanism_sufficiency, theoretical_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPIRICAL ANOMALY (SNARE) — Observations inconsistent with neo-Darwinian predictions cannot exit the interpretive framework; they are systematically reframed as noise, measurement error, or local exceptions. The anomaly bears full extraction cost without recourse. No alternative explanation framework is permitted institutional platform. Maximum experienced extraction.
constraint_indexing:constraint_classification(evolutionary_mechanism_sufficiency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE MECHANISM RESEARCHER (TANGLED ROPE) — Constrained by funding gatekeeping, journal review bias, and reputational risk from challenging orthodoxy. Yet some benefit exists from the constraint: the neo-Darwinian framework provides experimental paradigm, terminology, and research coordinates even for work proposing alternatives. Mixed extraction and coordination — significant cost to exit, some benefit from the coordinated field.
constraint_indexing:constraint_classification(evolutionary_mechanism_sufficiency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NEO-DARWINIAN ESTABLISHMENT (ROPE) — Experiences the constraint as coordination: the sufficiency of mutation-selection mechanisms enables publication, funding allocation, and research planning without constant epistemic renegotiation. Net beneficiary. Effective extraction is negative — the framework subsidizes institutional continuity.
constraint_indexing:constraint_classification(evolutionary_mechanism_sufficiency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXTENDED SYNTHESIS COALITION (SCAFFOLD) — Organized research programs (evolutionary developmental biology, ecological evolution, evolutionary systems theory) are building integrative frameworks that accommodate neo-Darwinian mechanisms while explaining anomalies through additional principles. This coalition sees the constraint as temporary — a sunset is emerging as empirical work accumulates and new paradigm gradually displaces the orthodoxy through data accumulation and institutional diffusion.
constraint_indexing:constraint_classification(evolutionary_mechanism_sufficiency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NEO-DARWINIAN CONSENSUS RITUAL (PITON) — The sufficiency claim persists as institutional dogma despite 70+ years of empirical challenges. Textbooks, curricula, and grant rubrics mandate neo-Darwinian framing. The theater is high (0.68): undergraduate education emphasizes adaptationist narrative; professional consensus is asserted without rigorous defense; alternative mechanisms are dismissed via terminology ('just another form of selection') rather than principled analysis. The ritual persists through inertia, not active enforcement. Piton classification confirmed by high theater and institutional maintenance despite functional degradation.
constraint_indexing:constraint_classification(evolutionary_mechanism_sufficiency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the sufficiency of any mechanism to explain all biological complexity is impossible — the universe is computationally bounded and contingent patterns always exceed deterministic explanation. This perspective sees mechanistic sufficiency claims as inherently transgressive limits. However, structural data contradicts this classification — the constraint is enforced via social coordination (institutional power, funding control, publication bias), not via immutable logical necessity. The engine's false summit detector identifies this as naturalization of a sociological phenomenon.
constraint_indexing:constraint_classification(evolutionary_mechanism_sufficiency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolutionary_mechanism_sufficiency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evolutionary_mechanism_sufficiency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolutionary_mechanism_sufficiency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(evolutionary_mechanism_sufficiency, TR),
    TR >= 0.70.

:- end_tests(evolutionary_mechanism_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The neo-Darwinian framework genuinely enables research coordination and has produced substantial empirical advances. However, the claim of sufficiency exceeds demonstrated explanatory scope — developmental mechanisms, epigenetic inheritance, niche construction, and rapid morphological change all require mechanisms beyond mutation-selection-drift to explain. The extractiveness reflects institutional monopoly on mechanism space rather than inherent logical necessity. Suppression (0.52): Moderate-high. Barriers to alternative mechanism research include funding gatekeeping (NSF/NIH grant criteria heavily weighted toward neo-Darwinian framings), journal review bias (editors trained in orthodox paradigm screen out alternatives), reputational risk (young researchers face career damage from heterodox work), and terminological absorption (novel mechanisms repackaged as 'just selection' to avoid paradigm threat). However, suppression is not total — evo-devo, ecological evolution, and systems biology have found institutional niches. Theater ratio (0.68): High and increasing. The sufficiency claim is presented as established fact in undergraduate education, professional consensus statements, and research funding rubrics despite ongoing empirical challenge. Textbooks assert neo-Darwinian mechanism sufficiency without rigorous proof; alternative mechanisms are dismissed via terminology ('adaptive explanation') rather than principled analysis; peer review asserts consensus without defending its basis. The theater has grown over the measurement interval as the claim has become more entrenched institutionally while becoming less defensible empirically.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion characteristic of institutional paradigm dominance. The beneficiary (establishment) experiences coordination; the victims (anomalies, alternatives) experience snares. The analytical observer risks false naturalization — seeing mechanistic sufficiency as inherent to biology rather than as enforced institutional coordination. The scaffold perspective is genuine — extended synthesis and evo-devo genuinely represent alternative mechanisms gaining institutional traction — but the sunset is contested: whether the new frameworks represent genuine theoretical alternatives or merely neo-Darwinian reformulations remains the central omega question.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness derives from structural position: power level, exit options, and relationship to the sufficiency claim. The neo-Darwinian establishment is the beneficiary (d ≈ 0.10, institutional/arbitrage: can freely exercise alternatives if paradigm shifts but has no incentive to). Alternative mechanism researchers are victims constrained by career risk (d ≈ 0.72, moderate/constrained: face significant cost to exit but have some agency). Anomalous observations are trapped victims (d ≈ 0.95, powerless/trapped: no exit pathway, no institutional voice). The extended synthesis coalition occupies a middle position (d ≈ 0.55, organized/constrained: organized enough to build alternative spaces but constrained by institutional pressure). The directing flow is from anomaly toward establishment: empirical evidence flows in but is reframed; alternative mechanisms arise but are absorbed; dissent is suppressed through gatekeeping rather than refutation. This is the classic extraction pattern: structural asymmetry maintained through institutional power and terminology control.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves via perspectival reading: the constraint is simultaneously Rope (coordination enabling research), Tangled Rope (coordination + extraction), Snare (institutional suppression of alternatives), Scaffold (temporary institutional dominance dissolving through data accumulation), and Piton (degraded consensus ritual). The natural law reading (Mountain) is a false summit — sufficiency claims are enforced through institutional mechanisms (funding control, peer review gatekeeping, terminology absorption), not logical necessity. The empirical test: if the neo-Darwinian establishment were merely coordinating (Rope), it would eagerly adopt superior alternatives; the fact that alternative mechanisms face systematic institutional suppression reveals extraction. The constraint prevents not biological research (which proceeds despite institutional suppression) but institutional recognition of mechanistic pluralism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_sufficiency_definition,
    'What constitutes ''sufficiency'' — explanatory power for all observed outcomes, or only the modal evolutionary pathways?',
    'Formal definition of sufficiency scope: comparison of neo-Darwinian predictions against all biological phenomena vs only directional evolutionary trends. Specification of what counts as ''explained'' vs ''accommodated''.',
    'If sufficiency means explaining all outcomes: the constraint is false and should dissolve (Rope perspective collapses). If sufficiency means explaining central tendency: the constraint is true and alternative mechanisms are unnecessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mechanism_sufficiency_definition, conceptual, 'Definition of mechanistic sufficiency scope').

omega_variable(
    anomaly_classification_ambiguity,
    'Are empirical anomalies (rapid morphological change, developmental constraint influence, epigenetic inheritance, niche construction) genuinely inexplicable under neo-Darwinian mechanisms, or have they been successfully incorporated through expanded interpretation?',
    'Systematic review of anomaly resolution literature; comparison of original anomaly formulation with current neo-Darwinian response; assessment of whether responses invoke new causal mechanisms or reframe existing ones.',
    'If anomalies remain unresolved: constraint is snare-like (empirical failure masked by institutional power). If incorporated: constraint is rope-like (coordination with sufficient explanatory scope). If partially resolved: constraint is tangled rope (mixed coordination and extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anomaly_classification_ambiguity, empirical, 'Whether empirical anomalies remain genuinely anomalous or have been accommodated').

omega_variable(
    evo_devo_integration_threshold,
    'At what degree of developmental mechanism incorporation into evolutionary theory does neo-Darwinism lose its core identity as a theory of differential reproduction, and does institutional resistance increase as integration threatens the original paradigm?',
    'Historical analysis of evo-devo textbook integration over 25-year period; correlation between mechanism inclusion and institutional resistance; tracking of terminology shifts (''extended synthesis'' as rebranding vs genuine theoretical expansion).',
    'If institutional resistance increases with integration: constraint operates via defense of paradigm boundaries (tangled rope extraction). If integration proceeds smoothly: constraint is relaxing (scaffold sunset scenario valid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evo_devo_integration_threshold, empirical, 'Institutional dynamics as developmental mechanisms are incorporated').

omega_variable(
    alternative_framework_viability,
    'Do alternative evolutionary frameworks (systems evolutionary theory, evolutionary systems biology, multi-level selection) provide genuine explanatory advances or merely reformulate neo-Darwinian logic?',
    'Comparative analysis of novel predictions and empirical confirmations; assessment of whether alternatives invoke genuinely distinct causal mechanisms or repackage mutation-selection-drift.',
    'If true alternatives exist: the constraint is enforced suppression (snare). If alternatives are reformulations: the constraint is genuinely coordinating (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_viability, empirical, 'Whether alternative frameworks provide genuine theoretical alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolutionary_mechanism_sufficiency, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evo_tr_t0, evolutionary_mechanism_sufficiency, theater_ratio, 0, 0.45).
narrative_ontology:measurement(evo_tr_t20, evolutionary_mechanism_sufficiency, theater_ratio, 20, 0.58).
narrative_ontology:measurement(evo_tr_t40, evolutionary_mechanism_sufficiency, theater_ratio, 40, 0.68).
narrative_ontology:measurement(evo_tr_t60, evolutionary_mechanism_sufficiency, theater_ratio, 60, 0.72).

% Extraction over time
narrative_ontology:measurement(evo_be_t0, evolutionary_mechanism_sufficiency, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(evo_be_t20, evolutionary_mechanism_sufficiency, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(evo_be_t40, evolutionary_mechanism_sufficiency, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(evo_be_t60, evolutionary_mechanism_sufficiency, base_extractiveness, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolutionary_mechanism_sufficiency, information_standard).
narrative_ontology:affects_constraint(evolutionary_mechanism_sufficiency, development_mechanism_explanatory_gap).
narrative_ontology:affects_constraint(evolutionary_mechanism_sufficiency, epigenetic_inheritance_institutional_status).
narrative_ontology:affects_constraint(evolutionary_mechanism_sufficiency, niche_construction_mechanism_recognition).

% DUAL FORMULATION NOTE:
% Evolutionary mechanism sufficiency is decomposed into three constraint stories reflecting distinct mechanistic claims: (1) mutation-selection-drift as complete explanation for morphological change (highest empirical contestation, ε ≈ 0.38); (2) adequacy of neo-Darwinian framework for ontogenetic development (increasingly recognized as inadequate, ε ≈ 0.52); (3) organizational principles beyond differential reproduction (most suppressed, ε ≈ 0.65). Each story links to the parent via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evolutionary_mechanism_sufficiency, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
