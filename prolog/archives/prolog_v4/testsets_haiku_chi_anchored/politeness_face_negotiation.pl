% ============================================================================
% CONSTRAINT STORY: politeness_face_negotiation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_politeness_face_negotiation, []).

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
 *   constraint_id: politeness_face_negotiation
 *   human_readable: The Face Negotiation Constraint (Politeness Theory)
 *   domain: social/communication
 *
 * SUMMARY:
 *   Politeness theory (Brown & Levinson, 1987) identifies a universal
 *   constraint where individuals manage their 'face'—a social self-image
 *   composed of positive face (desire for acceptance and approval) and
 *   negative face (desire for autonomy and freedom from imposition). This
 *   constraint operates across all human societies and communication
 *   contexts, making it a candidate for either a natural law (Mountain) or a
 *   universal coordination mechanism. However, the indexical analysis reveals
 *   a more complex structure: politeness functions as coordination for
 *   dominant groups (Rope perspective) but as pure extraction for low-status
 *   individuals (Snare perspective). The constraint exhibits Tangled Rope
 *   characteristics because it simultaneously enables workplace stability
 *   (genuine coordination function) and hides status hierarchies (asymmetric
 *   extraction). Over recent decades, theater_ratio has increased from 0.35
 *   to 0.62 as increasingly explicit performative face-work (corporate
 *   politeness training, HR-mandated civility, emoji-based tone softening in
 *   digital communication) has substituted for functional coordination. The
 *   rise of direct communication norms and radical candor movements reveals
 *   that politeness's coordination function may be replaced, suggesting a
 *   Scaffold structure with a realizable sunset clause. Yet the Mountain
 *   perspective naturalizes face concerns as immutable features of human
 *   psychology, obscuring the extent to which current politeness norms reify
 *   power asymmetries rather than solving genuine coordination problems.
 *
 * KEY AGENTS:
 *   - Low-status individuals: Primary victims (powerless/trapped) — must manage both positive face (maintain acceptance despite subordinate status) and negative face (honor autonomy restrictions imposed by hierarchy)
 *   - Dominant groups and institutional power holders: Primary beneficiaries (institutional/arbitrage) — their face is protected automatically; they can arbitrage politeness norms selectively
 *   - Professionals in hierarchical organizations: Secondary victims (moderate/constrained) — experience mixed coordination benefit (workplace stability) and extraction cost (asymmetric face management burden)
 *   - Social justice advocates: Organized agents (organized/mobile) — work to replace politeness with direct communication norms; see constraint as replaceable within generational timescale
 *   - Cultural institutions: Norm maintainers (institutional/arbitrage) — enforce politeness through school curricula, HR policy, etiquette literature; benefit from maintaining status quo
 *   - Analytical observer: Sees risk of naturalizing contingent arrangement as human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(politeness_face_negotiation, 0.38).
domain_priors:suppression_score(politeness_face_negotiation, 0.48).
domain_priors:theater_ratio(politeness_face_negotiation, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(politeness_face_negotiation, extractiveness, 0.38).
narrative_ontology:constraint_metric(politeness_face_negotiation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(politeness_face_negotiation, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(politeness_face_negotiation, tangled_rope).
narrative_ontology:human_readable(politeness_face_negotiation, "The Face Negotiation Constraint (Politeness Theory)").
narrative_ontology:topic_domain(politeness_face_negotiation, "social/communication").

domain_priors:requires_active_enforcement(politeness_face_negotiation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(politeness_face_negotiation, dominant_social_group).
narrative_ontology:constraint_beneficiary(politeness_face_negotiation, institutional_power_holders).
narrative_ontology:constraint_victim(politeness_face_negotiation, low_status_individuals).
narrative_ontology:constraint_victim(politeness_face_negotiation, marginalized_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-STATUS INDIVIDUAL (SNARE) — Cannot exit politeness requirements; must constantly manage negative face (autonomy threat) and positive face (acceptance threat) in presence of higher-status actors. Trapped in asymmetric face-work where their concerns are suppressed. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(politeness_face_negotiation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PROFESSIONAL IN HIERARCHY (TANGLED ROPE) — Benefits from politeness as a coordination mechanism for workplace stability and predictability. Simultaneously exploited by asymmetric face management: must protect superior's face while own face is vulnerable. Constrained by career consequences of breaking norms. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.38.
constraint_indexing:constraint_classification(politeness_face_negotiation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOMINANT GROUP (ROPE) — Experiences politeness primarily as coordination mechanism. Higher-status positions grant greater latitude in face-work; their face receives automatic protection. Can arbitrage politeness norms (breaking them selectively carries less cost). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary of coordination.
constraint_indexing:constraint_classification(politeness_face_negotiation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL NORM SYSTEM (PITON) — Politeness norms are maintained through cultural momentum and theatrical compliance. Much performative face-work serves no coordination function—it is ritual maintained because 'this is how things are done.' Theater ratio 0.62 reflects that significant politeness activity is performative rather than functionally necessary. Institutions persist despite reduced functional utility. d≈0.12, f(d)≈0.05, σ=1.0 → χ≈0.02.
constraint_indexing:constraint_classification(politeness_face_negotiation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: SOCIAL JUSTICE MOVEMENT (SCAFFOLD) — Recognizes politeness as a temporary coordination mechanism with an identifiable sunset. Organized advocates work to replace hierarchical face-work with more egalitarian communication norms (direct feedback, transparent criticism, radical candor). Sees politeness constraint as interim—replaceable by better coordination technologies. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.11. Lower extraction because movement has agency and visible exit pathway.
constraint_indexing:constraint_classification(politeness_face_negotiation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURALIST PERSPECTIVE (MOUNTAIN) — From the analytical/civilizational view, face management is an immutable feature of human social cognition. Every human has concerns about acceptance (positive face) and autonomy (negative face); therefore politeness is a natural law of social interaction. Accessible from first principles; no society can eliminate face concern. However, base properties (ε=0.38, suppression=0.48) contradict mountain classification—this is a false summit. The engine will detect that naturalizing a contingent institutional hierarchy as human nature is analytical error.
constraint_indexing:constraint_classification(politeness_face_negotiation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(politeness_face_negotiation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(politeness_face_negotiation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(politeness_face_negotiation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(politeness_face_negotiation, TR),
    TR >= 0.70.

:- end_tests(politeness_face_negotiation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Politeness extracts time and emotional labor from low-status individuals who must perform deference work. However, the extraction is not maximal because some coordination value is real—predictable, low-conflict interaction has genuine utility. The extraction magnitude is partially obscured by theater (people internalize politeness as intrinsically valuable rather than seeing it as status maintenance). Suppression (0.48): Moderate. Status-based asymmetries in politeness norms are suppressed through naturalization ('this is just how humans are') and institutional enforcement ('professional behavior requires politeness'). But suppression is not total—explicit critiques of politeness exist, and alternative norms are spreading. Theater ratio (0.62): Moderate-high. Significant politeness activity is performative: hedging language that conveys no real information, smile-work divorced from genuine feeling, politeness formulae maintained despite ineffectiveness. The rise of corporate politeness training indicates that theater has increased—politeness has become explicitly theatrical as coordination functions have weakened.
 *
 * PERSPECTIVAL GAP:
 *   The dominant group and low-status individuals experience this constraint in fundamentally incompatible ways. For the dominant group, politeness appears to be pure coordination (Rope)—a mechanism that solves the problem of coordinating disparate interests. For low-status individuals, the same constraint appears to be pure extraction (Snare)—a mechanism that forces them to constantly manage their face while their autonomy is restricted. The professional in a hierarchy sees a Tangled Rope: politeness enables workplace coordination but disproportionately burdens them. The social justice movement sees a Scaffold: politeness is a temporary coordination mechanism being replaced by direct communication norms. The institution maintaining the norms sees a Piton: politeness persists through inertia and theatrical compliance despite weakened functional utility. The naturalist perspective sees a Mountain: face management is a universal feature of human psychology. The engine's classification will expose the Mountain as a false summit—naturalizing power asymmetry as human nature rather than recognizing it as a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-status individuals: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction victim status. No exit options. Dominant groups: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary status; can selectively break norms. Professionals: Victims + constrained → d≈0.68, f(d)≈1.05. Significant extraction (face-work burden) but constrained rather than trapped (career mobility possible). Social justice movement: Organized + mobile → d≈0.35, f(d)≈0.30. Low extraction due to agency and exit option (new norms replacing old). Institutions: Beneficiaries + arbitrage → d≈0.12, f(d)≈0.05. Maintain status quo; benefit from politeness maintenance. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risks naturalizing constructed hierarchy as immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying the distinction between genuine coordination functions and power-concealing theater. The naturalist perspective claims Mountain—face management is a universal human need, therefore politeness constraints are immutable laws of social nature. But the base properties contradict this: ε=0.38 and suppression=0.48 are not the values for a natural law. A true Mountain would have ε≤0.25 and suppression≤0.05. The puzzle is resolved by recognizing that face concerns ARE universal (positive face and negative face as human psychological needs are likely universal), but the POLITENESS CONSTRAINT's extractive structure is contingent. Humans universally care about acceptance and autonomy. But whether politeness norms symmetrically protect both, or asymmetrically protect high-status people's faces while burdening low-status people with deference work—that is a contingent institutional choice, not a law of nature. The Tangled Rope classification captures the true structure: genuine coordination function (politeness does enable interaction) + asymmetric extraction (politeness norms disproportionately burden low-status individuals). The Scaffold perspective shows the constraint is decomposable—alternative coordination mechanisms (direct feedback, radical candor, transparent disagreement) are emerging and could replace politeness norms within a generation or two. The Snare and Piton perspectives reveal the hidden costs: pure extraction for those without power, pure theater for institutions maintaining obsolete norms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    face_universality_vs_cultural_construction,
    'Is face (positive and negative) a universal human need or a culturally constructed category that reifies power differences?',
    'Cross-cultural linguistic analysis of politeness systems; comparison of societies with genuinely flat hierarchies vs. those enforcing politeness asymmetrically; anthropological documentation of whether face management exists prior to status differentiation',
    'If universal need: Mountain classification gains force—politeness is inherent to social cognition. If cultural construction: Snare classification dominates—face is reified power hierarchy. Directs whether constraint is reform-resistant or decomposable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(face_universality_vs_cultural_construction, conceptual, 'Whether face is universal human need or constructed social hierarchy').

omega_variable(
    extraction_magnitude_measurement,
    'How much actual status inequality does politeness norm-following conceal or legitimize? Can it be quantified via linguistic analysis or social network position?',
    'Comparative analysis of actual resource distribution vs. face-work equality claims; measurement of who speaks, who is interrupted, whose ideas are credited in ''polite'' vs. direct communication settings',
    'If actual extraction is high (hidden by politeness theater): χ rises, Snare classification strengthens. If extraction is low (politeness is genuinely coordinating): Rope classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_magnitude_measurement, empirical, 'Quantification of status inequality hidden by politeness norms').

omega_variable(
    radical_candor_sustainability,
    'Can replacement coordination mechanisms (radical candor, non-hierarchical feedback, transparency-first communication) actually function at scale without face-work scaffolding?',
    'Longitudinal study of organizations implementing direct-feedback-only norms; measurement of cohesion, turnover, conflict resolution quality; assessment of whether new face management forms emerge (status anxiety expressed differently)',
    'If sustainable: Scaffold sunset is real—politeness is replaceable. If unstable: Politeness is more fundamental than believed—constraint cannot be fully decomposed, shifts from Scaffold to Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(radical_candor_sustainability, empirical, 'Whether radical candor can replace politeness-based coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(politeness_face_negotiation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(poli_tr_t0, politeness_face_negotiation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(poli_tr_t50, politeness_face_negotiation, theater_ratio, 50, 0.48).
narrative_ontology:measurement(poli_tr_t100, politeness_face_negotiation, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(poli_be_t0, politeness_face_negotiation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(poli_be_t50, politeness_face_negotiation, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(poli_be_t100, politeness_face_negotiation, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(politeness_face_negotiation, enforcement_mechanism).
narrative_ontology:affects_constraint(politeness_face_negotiation, status_hierarchy_legitimation).
narrative_ontology:affects_constraint(politeness_face_negotiation, emotional_labor_extraction).

% DUAL FORMULATION NOTE:
% Face negotiation functions as both a coordination mechanism (universal need to manage social self-image) and an extraction mechanism (asymmetric enforcement of deference norms). The constraint family includes status_hierarchy_legitimation (how politeness conceals inequality) and emotional_labor_extraction (how politeness demands uncompensated emotional work from low-status groups). Each family member has a distinct ε reflecting different aspects of the same institutional phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
