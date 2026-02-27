% ============================================================================
% CONSTRAINT STORY: deferential_realism_core
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_core, []).

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
 *   constraint_id: deferential_realism_core
 *   human_readable: The Deferential Realism Classification System
 *   domain: epistemological/political
 *
 * SUMMARY:
 *   The Deferential Realism framework itself acts as a constraint on how
 *   information about constraints is processed, validated, and legitimated.
 *   This self-referential structure creates a critical analytical puzzle: the
 *   framework claims to be descriptive and neutral, mapping pre-existing
 *   constraint structures, yet its adoption necessarily shapes which
 *   constraints are recognized as real and how their properties are measured.
 *   The framework exhibits a tangled hybrid structure: it provides genuine
 *   coordination benefits (cross-domain constraint mapping, systematic
 *   comparison methodology) while simultaneously extracting authority from
 *   alternative epistemologies by forcing their concepts into the (P,T,E,S)
 *   indexical space or rejecting them as incoherent. The constraint's
 *   theater_ratio (0.65) reflects the gap between the framework's claimed
 *   neutrality and its actual role in determining what counts as a valid
 *   constraint claim. The extractiveness (0.52) reflects that the framework's
 *   beneficiaries (developers, institutional validators) capture authority
 *   and legitimacy through the adoption cascade, while costs (translation
 *   overhead, loss of domain-specific nuance, suppression of alternative
 *   frameworks) are borne by alternative epistemologies and domain
 *   communities adapting to the new primitives.
 *
 * KEY AGENTS:
 *   - Framework Developers: Primary beneficiary (institutional/arbitrage) — control schema definition, authority to validate constraint classifications, network effects from adoption
 *   - Alternative Epistemologies: Primary victim (powerless/trapped) — holistic, non-indexed, context-embedded knowledge systems cannot exit framework validation regime or be recognized as valid without translation
 *   - Domain Expert Communities: Secondary victim (moderate/constrained) — must translate domain knowledge into (P,T,E,S) tuple space; face translation overhead and loss of nuance
 *   - Constraint Mapping Coalition: Organized agents (organized/constrained) — research networks and policy bodies adopting DR as scaffolding for cross-domain analysis; see framework as temporary tool
 *   - Legacy Classification Regime: Institutional actor (institutional/arbitrage) — previous constraint taxonomies persist through inertia; maintain parallel validation systems
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the contingent design choice as logically necessary structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_core, 0.52).
domain_priors:suppression_score(deferential_realism_core, 0.48).
domain_priors:theater_ratio(deferential_realism_core, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_core, extractiveness, 0.52).
narrative_ontology:constraint_metric(deferential_realism_core, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(deferential_realism_core, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_core, tangled_rope).
narrative_ontology:human_readable(deferential_realism_core, "The Deferential Realism Classification System").
narrative_ontology:topic_domain(deferential_realism_core, "epistemological/political").

domain_priors:requires_active_enforcement(deferential_realism_core).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_core, framework_developers).
narrative_ontology:constraint_beneficiary(deferential_realism_core, institutional_validators).
narrative_ontology:constraint_victim(deferential_realism_core, alternative_epistemologies).
narrative_ontology:constraint_victim(deferential_realism_core, informal_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE EPISTEMOLOGY (SNARE) — Cannot exit the framework's validation regime. Claims that don't fit the (P,T,E,S) tuple or the χ formula are classified as incoherent or unobservable. The constraint forces alternative knowledge systems (holistic, non-indexed, context-embedded) into the framework's measurement space or rejects them entirely. Maximum extraction: legitimacy flows only through DR categories.
constraint_indexing:constraint_classification(deferential_realism_core, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMAIN EXPERT COMMUNITY (TANGLED ROPE) — Constrained by the requirement to map their domain knowledge into DR primitives. But also benefits: the framework provides a common language for cross-domain analysis, enables comparative constraint mapping, and reduces parochial disagreement. Real costs (translation overhead, loss of nuance) and real benefits (interoperability, systematic classification). Significant but not maximal extraction.
constraint_indexing:constraint_classification(deferential_realism_core, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRAMEWORK DEVELOPERS (ROPE) — Coordinating how constraints are formally analyzed and compared. The framework solves the genuine problem of making constraint classifications systematic and verifiable across domains. Beneficiaries through control of the schema, authority to define validity, and institutional legitimacy. Extraction runs toward them through network effects and adoption.
constraint_indexing:constraint_classification(deferential_realism_core, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTRAINT MAPPING COALITION (SCAFFOLD) — Organized institutions (research networks, policy bodies, standards groups) are adopting DR to improve cross-domain constraint analysis. They experience the framework as temporary scaffolding: as alternative formal systems mature, or as domain-specific extensions supersede the base typology, the original DR framework becomes a legacy compatibility layer. Low experienced extraction because the coalition has agency and sees the framework as one tool among many.
constraint_indexing:constraint_classification(deferential_realism_core, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CLASSIFICATION REGIME (PITON) — Previous approaches to constraint analysis (rhetorical taxonomy, power-theoretic models, institutional analysis) persist through inertia even as DR provides alternatives. The legacy regime's primary function (explaining constraint behavior) has been partially superseded, but institutional commitment and training investment keep it in operation. Theater ratio high because competing frameworks perform similar functions with different primitives.
constraint_indexing:constraint_classification(deferential_realism_core, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL NECESSITY (MOUNTAIN) — From a universal perspective, some framework constraint on information processing is inherent: any systematic analysis requires primitives, any communication requires shared semantics, any comparison requires common metrics. The DR framework is one instantiation of what is logically necessary. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'logically necessary' framing naturalizes what is actually a contingent design choice.
constraint_indexing:constraint_classification(deferential_realism_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_core_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deferential_realism_core, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deferential_realism_core, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_core, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deferential_realism_core, TR),
    TR >= 0.70.

:- end_tests(deferential_realism_core_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The framework concentrates authority over constraint validation through its adoption cascade. Institutional validators gain legitimacy by endorsing DR; alternative frameworks lose funding and publication venues. This is not theoretical extraction—it is measurable through resource allocation and career incentives. However, the framework's coordination benefits (systematic cross-domain comparison, reduced parochial disagreement) are real, not merely performative. The extractiveness value reflects that the coordination benefit is genuine but asymmetrically captured by beneficiaries. Suppression (0.48): Moderate. Barriers to alternative frameworks include momentum effects (training investment in DR), network effects (more data = better analysis), and institutional commitment (funding bodies standardize on DR). But suppression is not total—alternative frameworks can coexist, and new domains can propose extensions. Theater ratio (0.65): Moderate-high. The framework claims perfect neutrality and descriptive purity (we are merely mapping constraints), yet its adoption shapes what counts as a constraint and how constraints are measured. The theater has increased as the framework matures and institutional adoption grows—early versions presented as exploratory now function as definitional standards.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range. Framework developers see pure coordination (Rope) — solving the real problem of cross-domain constraint mapping. The coalition sees temporary scaffolding (Scaffold) — the framework is one tool among many, useful but not permanent. Legacy regimes see a degraded competing system (Piton) — performing similar functions with different primitives and persisting through inertia. Domain experts see mixed coordination and extraction (Tangled Rope) — real benefits from interoperability but real costs from translation and standardization pressure. Alternative epistemologies see pure extraction (Snare) — their claims are delegitimated by the framework's closure and primitives. The civilizational analytical observer risks seeing logical necessity (Mountain) — any systematic analysis requires primitives and common metrics — but the structural data reveals this as a false summit: the closure to six types, the specific (P,T,E,S) design, and the measurement regime are contingent choices, not logical necessities.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the framework's authority and its measurement regime. Framework developers occupy low directionality (d ≈ 0.10) — they benefit through control of the schema and institutional legitimacy. Alternative epistemologies occupy high directionality (d ≈ 0.92) — they cannot exit the validation regime and bear maximum cost of forced translation or rejection. Domain expert communities occupy moderate directionality (d ≈ 0.55) — they face translation costs and benefits from interoperability. The coalition's constrained exit options (d ≈ 0.58) reflect that they can develop extensions and alternatives but face network effects pushing standardization on the base framework. The analytical observer's directionality at the civilizational scale (d ≈ 0.73) reflects the risk of false summit: naturalizing a contingent design choice as logically necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The deferential realism framework illustrates mandatrophy at the level of self-reference. The framework claims to be a neutral descriptive tool (rope or mountain perspective), yet its adoption mechanism exhibits high extraction (snare perspective). The resolution requires recognizing that the framework is genuinely useful for coordination (its rope properties are real) AND genuinely extractive of authority (its snare properties are real). The mandatrophy is resolved not by choosing one type but by recognizing that the framework's legitimacy depends on maintaining the coordination function while minimizing the extractive overhead. If the framework becomes purely extractive (high theater, declining coordination benefit), it degrades to piton. If it becomes purely coordinative (low theater, genuine interoperability), it approaches rope. The current state (tangled_rope, 0.52 extractiveness) reflects genuine coordination function plus genuine authority extraction. The trajectory (extractiveness rising from 0.30 to 0.52, theater ratio rising from 0.35 to 0.65) indicates increasing theater and increasing extraction—the framework is at risk of becoming more snare-like if institutional adoption outpaces its actual analytic utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    closure_vs_openness,
    'Does the framework''s closure (fixed set of six types, four axes) enable systematic analysis or preclude discovery of emergent constraint types?',
    'Longitudinal corpus analysis: do new constraint stories consistently map cleanly to six types, or do emerging patterns require additional categories?',
    'If closure enables: framework demonstrates sufficient expressiveness and is a true constraint on analysis. If closure limits: framework is an extractive gate preventing alternative constraint taxonomies from being recognized as valid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(closure_vs_openness, empirical, 'Whether the six-type closure enables or restricts constraint discovery').

omega_variable(
    primitive_sufficiency,
    'Are (P,T,E,S) sufficient to capture all structurally distinct constraints, or do some real constraints require additional axes (e.g., directionality context, measurability, transparency)?',
    'Test cases where experts from different domains claim DR cannot capture their constraint structure. Attempt to force-fit each case. If force-fitting produces artificial or misleading classifications, DR primitives are insufficient.',
    'If sufficient: framework is necessary, not merely imposed. If insufficient: framework is an extractive constraint gate, precluding alternative primitive sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primitive_sufficiency, conceptual, 'Whether (P,T,E,S) primitives are sufficient for all constraints').

omega_variable(
    measurement_independence,
    'Can constraints be measured and classified independently of the framework''s measurement methodology, or does adopting DR necessarily bias how constraints are observed and valued?',
    'Pre-adoption vs post-adoption constraint analysis: do the same domains describe constraints differently before and after DR adoption? Is the difference in precision or in redefinition?',
    'If independent: DR is a neutral analytical tool. If measurement-dependent: DR is extractive because its adoption changes which constraints are recognized as real and valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_independence, preference, 'Whether DR measurement methodology is domain-independent').

omega_variable(
    alternative_framework_suppression,
    'Does DR institutional adoption suppress development of competing formal constraint taxonomies, or does it create space for multiple systems to coexist?',
    'Survey of funding, publication, and training resources allocated to alternative constraint frameworks before and after DR adoption in major institutions.',
    'If suppression: DR is a snare on alternative epistemologies (confirmed). If coexistence: DR is closer to rope or scaffold and less extractive than hypothesis suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_suppression, empirical, 'Whether DR adoption suppresses alternative constraint frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_core, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defreal_tr_t0, deferential_realism_core, theater_ratio, 0, 0.35).
narrative_ontology:measurement(defreal_tr_t3, deferential_realism_core, theater_ratio, 3, 0.5).
narrative_ontology:measurement(defreal_tr_t6, deferential_realism_core, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(defreal_be_t0, deferential_realism_core, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(defreal_be_t3, deferential_realism_core, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(defreal_be_t6, deferential_realism_core, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_core, information_standard).
narrative_ontology:affects_constraint(deferential_realism_core, institutional_knowledge_validation).
narrative_ontology:affects_constraint(deferential_realism_core, epistemological_pluralism_suppression).
narrative_ontology:affects_constraint(deferential_realism_core, index_closure_gate).

% DUAL FORMULATION NOTE:
% The deferential realism framework constrains how other constraints are understood and validated. Three distinct structurally downstream constraints are identified: (1) institutional_knowledge_validation — how institutions decide which knowledge systems are legitimate; (2) epistemological_pluralism_suppression — how adoption of unified frameworks suppresses alternative epistemologies; (3) index_closure_gate — how the closure to six types and four axes precludes discovery of constraints requiring different primitives. These are separate constraint stories, each with their own extractiveness and perspectives, but all are structurally downstream of the framework's adoption and authority regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_core, analytical, 0.73).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
