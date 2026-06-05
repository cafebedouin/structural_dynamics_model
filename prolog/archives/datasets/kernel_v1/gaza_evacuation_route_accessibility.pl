% ============================================================================
% CONSTRAINT STORY: gaza_evacuation_route_accessibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gaza_evacuation_route_accessibility, []).

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
 *   constraint_id: gaza_evacuation_route_accessibility
 *   human_readable: Gaza Evacuation Route Accessibility Constraint
 *   domain: conflict/humanitarian/logistics
 *
 * SUMMARY:
 *   The Gaza evacuation route constraint structures civilian freedom of
 *   movement under conflict conditions through a system of checkpoints that
 *   nominally enable evacuation but functionally control access, timing, and
 *   eligibility. The constraint exhibits the structural signature of a Snare
 *   from the perspective of the displaced population — it appears to offer an
 *   exit from danger but in practice extracts maximum compliance,
 *   documentation, and vulnerability disclosure while maintaining authority
 *   discretion over access. From the checkpoint authority perspective, the
 *   same constraint appears as coordination (Rope) — solving the legitimate
 *   problem of managing population flows and preventing weapons smuggling.
 *   From the humanitarian organization perspective, it appears as mixed
 *   coordination-extraction (Tangled Rope) — enabling some protection
 *   activities while imposing operational constraints. From the international
 *   law perspective, it appears as theatrical compliance (Piton) — formal
 *   adherence to humanitarian protocols masking coercive access control. The
 *   constraint's increasing theater_ratio (0.50 → 0.65 over the measured
 *   interval) reflects intensifying performative elements: formal
 *   humanitarian checkpoints, official eligibility criteria, and documented
 *   procedures that provide the appearance of systematic access while
 *   maintaining extraction mechanism integrity. The rising extractiveness and
 *   suppression reflect both increased coercion and feedback loops where
 *   failed evacuation attempts increase vulnerability disclosure and
 *   compliance pressure. The civilian population faces absolute barriers to
 *   exit both the conflict zone (military/physical barriers) and the
 *   evacuation route system (checkpoint authority dependency). This creates
 *   the Snare signature: nominally voluntary movement that is practically
 *   coercive; the constraint appears to offer safety but functions as
 *   apparatus for extraction of compliance and vulnerability disclosure.
 *
 * KEY AGENTS:
 *   - Displaced Civilian Population: Primary victims (powerless/trapped) — no alternative to evacuation; submission to checkpoint authority decisions about eligibility, timing, capacity; maximum extraction and suppression
 *   - Vulnerable Subpopulations: Primary victims (powerless/trapped) — elderly, disabled, medical-dependent, unaccompanied minors face compounded extraction through exclusionary checkpoint criteria; separation from protection
 *   - Checkpoint Authorities: Primary beneficiaries (institutional/arbitrage) — extract compliance, documentation, timing control; experience constraint as coordination mechanism for security and population management
 *   - Military Command Structure: Secondary beneficiary (institutional/arbitrage) — benefits from population control, movement intelligence, access to civilian information
 *   - Humanitarian Organizations: Secondary victim (organized/constrained) — gain access for protection activities but bear extraction costs through delays, denials, and operational constraints; risk cooptation into system legitimation
 *   - International Law Framework: Institutional observer (institutional/arbitrage) — persists performatively; formal protocols provide legitimation appearance while extraction mechanism operates unchanged
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices about access as inherent conflict logistics necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaza_evacuation_route_accessibility, 0.68).
domain_priors:suppression_score(gaza_evacuation_route_accessibility, 0.82).
domain_priors:theater_ratio(gaza_evacuation_route_accessibility, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaza_evacuation_route_accessibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(gaza_evacuation_route_accessibility, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(gaza_evacuation_route_accessibility, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gaza_evacuation_route_accessibility, snare).
narrative_ontology:human_readable(gaza_evacuation_route_accessibility, "Gaza Evacuation Route Accessibility Constraint").
narrative_ontology:topic_domain(gaza_evacuation_route_accessibility, "conflict/humanitarian/logistics").

domain_priors:requires_active_enforcement(gaza_evacuation_route_accessibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gaza_evacuation_route_accessibility, checkpoint_authorities).
narrative_ontology:constraint_beneficiary(gaza_evacuation_route_accessibility, military_command_structure).
narrative_ontology:constraint_victim(gaza_evacuation_route_accessibility, civilian_population).
narrative_ontology:constraint_victim(gaza_evacuation_route_accessibility, displaced_persons).
narrative_ontology:constraint_victim(gaza_evacuation_route_accessibility, vulnerable_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED CIVILIAN (SNARE) — Faces absolute barriers to exit the route system. Evacuation is nominally voluntary but practically coercive: remaining means exposure to active conflict; using the route means submission to checkpoint authority decisions about eligibility, timing, and capacity. Zero degrees of freedom; maximum extraction and suppression.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: VULNERABLE SUBPOPULATION (SNARE) — Elderly, disabled, medical-dependent, and unaccompanied minors face compounded extraction. Checkpoint criteria often exclude or deprioritize those requiring specialized assistance. Evacuation route becomes apparatus for separating vulnerable persons from protection. No exit mechanism; pure extraction.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHECKPOINT AUTHORITY (ROPE) — Experiences the constraint as a coordination mechanism for controlling population movement, preventing weapons smuggling, and maintaining security perimeter integrity. Benefits from authority to gate access and extract compliance (documentation, verification, delays). Low perceived extraction because the constraint solves their coordination problem directly.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: HUMANITARIAN ORGANIZATION (TANGLED ROPE) — Must coordinate evacuation logistics, negotiate checkpoint access, and deliver assistance. Both coordinates with authorities (gains access for medical convoys, aid distribution) and bears extraction costs (delays, arbitrary denial, documentation barriers). Mixed experience: genuine coordination function with embedded asymmetric extraction.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL LAW FRAMEWORK (PITON) — Humanitarian law nominally guarantees safe passage for displaced persons and wounded. Evacuation routes are framed as legal compliance mechanisms. In practice, the framework persists performatively while the extraction mechanism operates unchanged. Theater_ratio high: formal adherence to humanitarian protocols (medical checkpoints, designated routes, humanitarian organization access) masks coercive access control.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, evacuation routes under conflict are presented as physically necessary: wars require population movement control; security perimeters require checkpoints; resource scarcity requires triage. This perspective sees route accessibility constraints as inherent to conflict logistics. However, this naturalizes contingent institutional choices about who can pass, when, and under what conditions — the engine's false summit detector identifies this as constructed constraint masquerading as natural law.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gaza_evacuation_route_accessibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gaza_evacuation_route_accessibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gaza_evacuation_route_accessibility, TR),
    TR >= 0.70.

:- end_tests(gaza_evacuation_route_accessibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts maximum compliance, vulnerability disclosure (family composition, health status, destination information), and psychological submission to authority discretion. The extraction is compounded over time as populations attempt multiple evacuation efforts, each requiring renewed compliance and documentation. The extractiveness trajectory (0.52 → 0.68) reflects both intensifying coercion and feedback loops where failed attempts increase desperation and compliance pressure. Suppression (0.82): Very high. Absolute barriers to alternative exits: military control of perimeter, no safe passage guarantees outside checkpoint system, resources insufficient for autonomous civilian movement, social structures fragmented by conflict. Suppression is structural and intensifying (0.75 → 0.82), reflecting both official checkpoint tightening and desperation-driven population vulnerability. Theater ratio (0.65): Moderate-high. The constraint includes performative elements — formal humanitarian checkpoints, documented eligibility criteria, official procedures — that provide appearance of systematic access while maintaining extraction mechanism. Theater_ratio trajectory (0.50 → 0.65) reflects increasing performative overlay: formal humanitarian protocols, international observer presence, documented procedures that create legitimation appearance without substantive access expansion. Claimed type (Snare): Justified by extractiveness ≥ 0.46, suppression ≥ 0.60, and victim-centered classification. The constraint has minimal genuine coordination function from the victim's perspective — evacuation is presented as enabling escape from danger but functions as apparatus for extraction of compliance and vulnerability.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The checkpoint authority perceives coordination (Rope) — solving legitimate security and population-flow problems — while the displaced civilian perceives pure extraction (Snare) — coercive compliance mechanism masked as humanitarian assistance. The humanitarian organization perceives mixed coordination-extraction (Tangled Rope) — enabling some protection activities while bearing extraction costs. The international law framework perceives legal compliance (Piton) — formal protocols satisfied while extraction operates unchanged. The analytical observer at civilizational scope risks perceiving natural necessity (Mountain) — checkpoint constraints as inherent to conflict logistics — but the structural data reveals this as false summit: the contingent institutional choices about access control are presented as necessary constraints. The most revealing gap is between the beneficiary's Rope and the victim's Snare: from the same checkpoint system, one experiences pure coordination and the other experiences pure extraction. This gap is diagnostic of snare structure: the constraint simultaneously benefits the authority through compliance extraction and harms the victim through access denial.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by structural position relative to the extraction flow. Victims with no exit options (trapped) experience maximum extraction — d ≈ 0.95 → f(d) ≈ 1.42. Beneficiaries with arbitrage options experience minimum extraction — d ≈ 0.05 → f(d) ≈ -0.12. Organized secondary victims (humanitarian organizations) with constrained exit options experience moderate extraction — d ≈ 0.65 → f(d) ≈ 1.00. The institutional beneficiary at immediate time horizon with arbitrage exit derives d ≈ 0.05, producing negative effective extractiveness (χ = 0.68 × (-0.12) × 1.0 ≈ -0.08). The powerless victim at immediate time horizon with trapped exit derives d ≈ 0.95, producing high effective extractiveness (χ = 0.68 × 1.42 × 0.8 ≈ 0.77, adjusted for local scope). The perspectival gap between beneficiary rope (negative χ) and victim snare (positive χ > 0.66) is the diagnostic signature of extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLVED: The constraint meets all snare thresholds (ε ≥ 0.46, suppression ≥ 0.60, χ ≥ 0.66 from victim perspective). The mandatrophy is resolved by recognizing that the constraint has no genuine coordination function from the victim's perspective — evacuation is presented as escape route but functions as apparatus for extraction of compliance and vulnerability. The checkpoint authority's perception of coordination (Rope) does not constitute genuine coordination with the victim because the victim faces binary choice between remaining in danger zone or submitting to extraction. The false coordination appears only from the beneficiary's perspective; the analytical observer at civilizational scope risks naturalizing this as inherent to conflict (Mountain) but the omega variables reveal contingent institutional choices. The piton classification from the international law perspective reflects that formal humanitarian protocols mask unchanged extraction mechanism — theater provides legitimation without functional protection expansion. The constraint resolves mandatrophy by demonstrating that coordinate classification appears across multiple perspectives (Snare dominant, with theater-mediated Piton, coordination-masked false summit, and beneficiary-side Rope) and that the perspectival diversity is itself the diagnostic signature of extraction mechanism operating through institutional legitimation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_rationale_ambiguity,
    'Are checkpoint access restrictions primarily justified by genuine security requirements (weapons interdiction, infiltration prevention) or primarily by population control and resource extraction?',
    'Comparative analysis of checkpoint interdiction rates vs. denial rates; correlation between stated security threats and actual documented incidents; timing of restrictions relative to security events vs. administrative cycles',
    'If security-driven: constraint may be Tangled Rope (mixed coordination-extraction). If primarily extraction-driven: classification remains Snare with theater masking pure coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_rationale_ambiguity, empirical, 'Whether checkpoint restrictions are security-justified or extraction-driven').

omega_variable(
    alternative_route_feasibility,
    'Do realistic alternative evacuation pathways exist that would reduce dependency on controlled checkpoints?',
    'Geographic and logistical feasibility analysis of sea routes, humanitarian corridors, safe passage guarantees outside checkpoint system; cost-benefit assessment relative to checkpoint access delays',
    'If alternatives feasible: constraint is extractive choice, not necessity (strengthens Snare classification). If alternatives blocked or infeasible: suppression magnitude may reflect structural constraint rather than deliberate coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_route_feasibility, empirical, 'Whether feasible alternative evacuation routes exist').

omega_variable(
    checkpoint_criteria_consistency,
    'Are checkpoint eligibility criteria applied consistently across time, population groups, and different checkpoints, or do they show patterns of arbitrary enforcement favoring particular groups or outcomes?',
    'Longitudinal analysis of access approval rates by category (age, health status, family composition, geographic origin); comparison across checkpoints and time periods; documentation of exceptions and criteria changes',
    'If consistent: constraint has predictability allowing adaptation (reduces suppression perception). If arbitrary: maximizes both extraction and suppression; strengthens Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(checkpoint_criteria_consistency, empirical, 'Consistency of checkpoint eligibility criteria application').

omega_variable(
    humanitarian_organization_cooptation,
    'Does humanitarian organization participation in the evacuation route system constitute genuine coordination for civilian protection or functional legitimation of an extraction mechanism?',
    'Analysis of humanitarian organization resource allocation, decision-making authority at checkpoints, advocacy positions on route accessibility, and divergence between public statements and operational constraints faced',
    'If genuine protection role: maintains Tangled Rope classification for humanitarian organizations. If cooptation: humanitarian presence becomes theater masking Snare, elevating theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_organization_cooptation, conceptual, 'Whether humanitarian participation constitutes genuine coordination or system legitimation').

omega_variable(
    natural_law_false_summit_candidate,
    'Is the checkpoint constraint a natural law inherent to conflict logistics, or a contingent institutional choice presented as inevitable?',
    'Comparative historical analysis of evacuation systems in other conflicts; identification of how different authorities structure evacuation access; distinguishing technical requirements from policy choices',
    'If natural law: Mountain classification from analytical perspective is correct. If false summit: reveals constructed extraction mechanism disguised as necessity; shifts analytical perspective toward Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_false_summit_candidate, conceptual, 'Whether checkpoint constraints are inherent to conflict or contingent policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gaza_evacuation_route_accessibility, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaza_evac_tr_t0, gaza_evacuation_route_accessibility, theater_ratio, 0, 0.5).
narrative_ontology:measurement(gaza_evac_tr_t3, gaza_evacuation_route_accessibility, theater_ratio, 3, 0.58).
narrative_ontology:measurement(gaza_evac_tr_t6, gaza_evacuation_route_accessibility, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(gaza_evac_be_t0, gaza_evacuation_route_accessibility, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gaza_evac_be_t3, gaza_evacuation_route_accessibility, base_extractiveness, 3, 0.61).
narrative_ontology:measurement(gaza_evac_be_t6, gaza_evacuation_route_accessibility, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gaza_evac_su_t0, gaza_evacuation_route_accessibility, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(gaza_evac_su_t3, gaza_evacuation_route_accessibility, suppression_requirement, 3, 0.79).
narrative_ontology:measurement(gaza_evac_su_t6, gaza_evacuation_route_accessibility, suppression_requirement, 6, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gaza_evacuation_route_accessibility, enforcement_mechanism).
narrative_ontology:affects_constraint(gaza_evacuation_route_accessibility, humanitarian_aid_distribution_bottleneck).
narrative_ontology:affects_constraint(gaza_evacuation_route_accessibility, medical_access_under_siege).
narrative_ontology:affects_constraint(gaza_evacuation_route_accessibility, family_separation_mechanism).

% DUAL FORMULATION NOTE:
% The evacuation route constraint is upstream of specific humanitarian crises (medical access, aid distribution, family separation) by controlling population mobility. Each downstream constraint has its own extractiveness reflecting specific structural mechanisms; the evacuation route provides the structural dependency that amplifies extraction across the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gaza_evacuation_route_accessibility, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
