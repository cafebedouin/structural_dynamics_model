% ============================================================================
% CONSTRAINT STORY: stoic_logos_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stoic_logos_governance, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: stoic_logos_governance
 *   human_readable: The Stoic Logos as an Imperial Governance Framework
 *   domain: philosophical/political
 *
 * SUMMARY:
 *   The Stoic Logos as employed by Marcus Aurelius and the later 2nd-century
 *   Roman emperors represents a sophisticated governance constraint that
 *   fuses coordination and extraction. The framework establishes that the
 *   emperor submits his personal will to universal reason (the Logos) — a
 *   claim that simultaneously legitimizes imperial authority and constrains
 *   it. For subjects, this presents an intriguing paradox: the emperor is
 *   bound by the same rational natural law that binds them, yet the emperor
 *   alone interprets that law in practice. The constraint exhibits all six
 *   classification types depending on perspective: snare for powerless
 *   subjects (bound by interpretation they cannot challenge), tangled rope
 *   for provincial administrators (both constrained and enabled), rope for
 *   the imperial bureaucracy (genuine coordination mechanism), piton for
 *   competing schools (their suppression appears natural rather than
 *   enforced), scaffold for the empire as a transitional structure (the Stoic
 *   framework becomes obsolete as Christianity offers an alternative
 *   universalism), and false mountain for observers who naturalize this
 *   institutional arrangement as inherent to rational rule. The theater ratio
 *   increases over the 250-year interval as the Stoic framework becomes more
 *   performative — the actual content of imperial decisions diverges further
 *   from the stated philosophical principles, yet the philosophical
 *   vocabulary persists.
 *
 * KEY AGENTS:
 *   - Marcus Aurelius and Stoic Emperors: Primary beneficiaries (institutional/arbitrage) — gains philosophical legitimacy and coordination mechanism for multi-ethnic empire
 *   - Imperial Subjects: Primary victims (powerless/trapped) — must conform to ruler's interpretation of Logos with no exit or meaningful challenge mechanism
 *   - Provincial Administrators: Secondary actors (moderate/constrained) — experience mixed extraction (political liability) and coordination (predictable governance through shared philosophy)
 *   - Competing Philosophical Schools (Cynics, Epicureans, Skeptics): Suppressed agents (organized/constrained) — lose institutional patronage and legitimacy through what appears as natural decline rather than coercion
 *   - Imperial Bureaucracy: Beneficiary institution (institutional/arbitrage) — Stoic rationalization enables hierarchical organization and multi-ethnic administration
 *   - Intellectual Elites: Secondary beneficiaries (powerful/mobile) — Stoicism offers career and patronage opportunities through imperial favor
 *   - Christian Theology (3rd-4th century): Successor framework — eventually displaces Stoicism as legitimacy mechanism, driving the scaffold's sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stoic_logos_governance, 0.38).
domain_priors:suppression_score(stoic_logos_governance, 0.52).
domain_priors:theater_ratio(stoic_logos_governance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stoic_logos_governance, extractiveness, 0.38).
narrative_ontology:constraint_metric(stoic_logos_governance, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(stoic_logos_governance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stoic_logos_governance, tangled_rope).
narrative_ontology:human_readable(stoic_logos_governance, "The Stoic Logos as an Imperial Governance Framework").
narrative_ontology:topic_domain(stoic_logos_governance, "philosophical/political").

domain_priors:requires_active_enforcement(stoic_logos_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stoic_logos_governance, imperial_stability).
narrative_ontology:constraint_beneficiary(stoic_logos_governance, philosophical_elite).
narrative_ontology:constraint_beneficiary(stoic_logos_governance, state_continuity).
narrative_ontology:constraint_victim(stoic_logos_governance, imperial_subjects).
narrative_ontology:constraint_victim(stoic_logos_governance, provincial_populations).
narrative_ontology:constraint_victim(stoic_logos_governance, competing_philosophical_schools).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPERIAL SUBJECT (SNARE) — Subject must conform to the ruler's interpretation of the Logos with no exit option. The constraint extracts obedience justified by universal reason, but the subject cannot escape the frame. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.52.
constraint_indexing:constraint_classification(stoic_logos_governance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PROVINCIAL ADMINISTRATOR (TANGLED ROPE) — Constrained by imperial hierarchy but benefits from coordination through Stoic principles (predictable, rational governance). Experiences both extraction (political liability) and coordination (philosophical legitimacy). d≈0.62, f(d)≈0.80, σ=1.1 → χ≈0.33.
constraint_indexing:constraint_classification(stoic_logos_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: IMPERIAL BUREAUCRACY (ROPE) — Benefits from Stoic rationalization of administrative hierarchy. The framework solves the coordination problem of governing a multi-ethnic empire through shared philosophical vocabulary. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary through institutional legitimacy.
constraint_indexing:constraint_classification(stoic_logos_governance, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETING PHILOSOPHICAL SCHOOLS (PITON) — Cynic, Epicurean, and other schools are suppressed not through direct coercion but through loss of imperial patronage and institutional legitimacy. Theater_ratio=0.68 reflects performative aspects: Stoicism appears as natural philosophy while others appear as deviant. The suppression mechanism is institutional inertia — formerly competitive schools persist in reduced form. d≈0.85, f(d)≈1.18, σ=1.0 → χ≈0.45.
constraint_indexing:constraint_classification(stoic_logos_governance, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: TRANSITIONAL EMPIRE WITH SUNSET (SCAFFOLD) — The Stoic framework serves a temporary coordination function during a period of imperial consolidation and multi-ethnic integration. As the empire stabilizes and Christianity offers an alternative universal framework (3rd-4th century), the Stoic legitimacy mechanism decays. The constraint has built-in obsolescence: once the empire no longer needs philosophical universalism to justify rule, the Stoic framework becomes dispensable. d≈0.45, f(d)≈0.48, σ=1.1 → χ≈0.20. Estimated sunset: 150-250 years as Christian theology replaces Stoic logos.
constraint_indexing:constraint_classification(stoic_logos_governance, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — FALSE SUMMIT ALERT. Some analysts treat the Stoic constraint as a mountain: 'the ruler submitting to universal reason is inherent to rational governance.' But the structural data (ε=0.38, suppression=0.52, theater=0.68) reveals this as a contingent institutional arrangement, not a law of nature. The appearance of inevitability is manufactured through philosophy — naturalization is the mechanism of extraction.
constraint_indexing:constraint_classification(stoic_logos_governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stoic_logos_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stoic_logos_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stoic_logos_governance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(stoic_logos_governance, TR),
    TR >= 0.70.

:- end_tests(stoic_logos_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts obedience and conformity justified by reference to universal reason, but the emperor is officially bound by the same Logos, reducing the appearance of naked extraction. In practice, the emperor's interpretation of the Logos is final, making extraction substantial. The value reflects the gap between stated constraint (emperor bound) and actual mechanism (emperor interprets binding). Suppression (0.52): Moderate-high. Subjects cannot challenge the emperor's interpretation of the Logos without rejecting the framework itself. Competing philosophical schools are not violently suppressed but lose institutional support and legitimacy, creating a gentler but effective suppression mechanism. Theater ratio (0.68): High. The Stoic framework is increasingly performative over the interval. By the 2nd-3rd century, imperial actions frequently contradict stated Stoic principles (Commodus's cruelty, Caracalla's fratricide), yet the philosophical vocabulary persists. The theater rises as the gap between principle and practice widens but institutional commitment to the philosophical narrative remains.
 *
 * PERSPECTIVAL GAP:
 *   The subject and beneficiary perspectives diverge sharply. The emperor (beneficiary/institutional/arbitrage) experiences Stoicism as genuine coordination: authentic submission to universal reason that constrains his own will for the common good. This perspective is sincere — Marcus Aurelius's Meditations testify to real philosophical commitment. The subject (victim/powerless/trapped) experiences Stoicism as extraction: the emperor invokes universal reason to justify whatever the emperor decides, with no mechanism for subjects to challenge the interpretation. The provincial administrator occupies the middle: they experience genuine coordination (predictable, rational governance) alongside extraction (political liability for implementing unpopular policies justified philosophically). The competing schools experience gradual suppression through institutional inertia rather than force — they disappear not through coercion but through loss of patronage and legitimacy, making the process appear natural. The analytical observer risks seeing the constraint as a mountain ('rational governance requires submission to universal principles') but the structural data reveals this as naturalization: the Logos is one choice among multiple possible governance frameworks.
 *
 * DIRECTIONALITY LOGIC:
 *   Emperor/Imperial bureaucracy: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary position; philosophical framework legitimizes authority. Subject: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction target; cannot exit frame or challenge interpretation. Provincial administrator: Victim + constrained → d≈0.62, f(d)≈0.80. Significant extraction (political liability) but also benefits from coordination mechanism. Competing schools: Victim + constrained → d≈0.85, f(d)≈1.18. High extraction through institutional suppression disguised as natural decline. Intellectual elite: Beneficiary + mobile → d≈0.18, f(d)≈0.02. Can exit (patronage is valuable but alternatives exist); net beneficiary. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risk of false summit if they naturalize the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The Stoic constraint resolves the mandatrophy by revealing how coordination and extraction can be genuinely fused. The emperor is not lying when he claims submission to the Logos — the philosophical commitment appears authentic. Yet the constraint simultaneously extracts obedience because the emperor alone interprets the Logos. This is not a case where one perspective sees coordination and another sees extraction of the same mechanism. Rather, the constraint operates on two different levels: (1) Genuine coordination at the philosophical/intellectual level — shared vocabulary and rational justification for governance. (2) Extraction at the structural/enforcement level — the emperor's interpretation is final and subjects have no meaningful exit. The tangled rope classification is correct: both elements are essential, neither is purely instrumental. The emperor's genuine philosophical commitment (real coordination) is what enables the extraction mechanism (subjects accept constraints justified philosophically). A purely coercive constraint would be a snare; a purely coordinative one would be rope. The Stoic framework is tangled rope precisely because its legitimacy depends on the sincere fusion of commitment and authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marcus_aurelius_authentic_commitment,
    'Did Marcus Aurelius genuinely believe his own Stoic philosophy, or did he maintain it as a performance of authority?',
    'Textual analysis of Meditations vs public pronouncements; correlation with documented imperial actions (clemency vs execution patterns); historical accounts of contemporaries',
    'If authentic: constraint is primarily coordination (Rope from ruler perspective) — true belief reduces extraction coefficient. If performance: constraint is primarily extraction (Snare from subject perspective) — philosophical cover legitimizes power asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marcus_aurelius_authentic_commitment, conceptual, 'Whether Marcus Aurelius authentically believed his Stoicism').

omega_variable(
    subject_perception_of_logos,
    'Did imperial subjects experience the Stoic Logos as divine reason or as a rationalization for arbitrary imperial will?',
    'Historical sources from non-elite writers; provincial inscriptions; evidence of resistance or reinterpretation; comparative analysis with how subjects understood earlier emperor justifications',
    'If perceived as divine reason: extraction mechanism is weaker (subjects see legitimacy) — χ decreases. If perceived as rationalization: extraction mechanism is stronger (cynicism increases compliance costs) — χ increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subject_perception_of_logos, empirical, 'Subject-level perception of Logos legitimacy').

omega_variable(
    alternative_coordination_feasibility,
    'Could the Roman Empire have achieved equivalent stability and multi-ethnic governance through non-Stoic coordination mechanisms (military force alone, dynastic legitimacy, Christian theology, etc.)?',
    'Comparative institutional analysis with pre-Stoic and post-Stoic imperial periods; counterfactual analysis of what happened when Stoicism declined; study of non-Stoic empires',
    'If alternative mechanisms available: Stoicism is one choice among many (extraction component increases). If Stoicism was uniquely suited: constraint is more coordinative (extraction component decreases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, conceptual, 'Whether alternative governance frameworks could substitute for Stoic coordination').

omega_variable(
    sunset_mechanism_reliability,
    'What explains the actual historical replacement of Stoic governance by Christian theology? Was the transition inevitable, or contingent on specific political choices?',
    'Historical analysis of 3rd-4th century transition; documented shifts in imperial rhetoric; analysis of competing legitimacy frameworks during this period',
    'If inevitable: scaffold sunset is reliable. If contingent: sunset logic was aspirational rather than structural; constraint could have persisted indefinitely if empire had sustained Stoic intellectual infrastructure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_mechanism_reliability, empirical, 'Whether the Stoic framework''s historical decline was inevitable or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stoic_logos_governance, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stoic_tr_t0, stoic_logos_governance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(stoic_tr_t50, stoic_logos_governance, theater_ratio, 50, 0.58).
narrative_ontology:measurement(stoic_tr_t100, stoic_logos_governance, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(stoic_be_t0, stoic_logos_governance, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(stoic_be_t50, stoic_logos_governance, base_extractiveness, 50, 0.34).
narrative_ontology:measurement(stoic_be_t100, stoic_logos_governance, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stoic_logos_governance, enforcement_mechanism).
narrative_ontology:affects_constraint(stoic_logos_governance, roman_imperial_legitimacy).
narrative_ontology:affects_constraint(stoic_logos_governance, multi_ethnic_empire_governance).
narrative_ontology:affects_constraint(stoic_logos_governance, christian_theology_succession).

% DUAL FORMULATION NOTE:
% The Stoic Logos represents a distinct constraint from specific imperial policies or theological doctrines. It is a meta-constraint: a framework that legitimizes how other constraints are justified and enforced. The downstream constraints (imperial legitimacy, empire governance) depend on the Stoic frame; the Christian theology constraint represents the successor framework that eventually displaces Stoicism, driving the sunset mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
