% ============================================================================
% CONSTRAINT STORY: structural_position_constraint_divergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_position_constraint_divergence, []).

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
 *   constraint_id: structural_position_constraint_divergence
 *   human_readable: Structural Position Constraint Divergence
 *   domain: social_systems/institutional_dynamics/stratification_mechanics
 *
 * SUMMARY:
 *   Structural Position Constraint Divergence is the meta-constraint that
 *   describes the Deferential Realism framework's own central observation:
 *   identical institutional mechanisms produce radically different
 *   classifications depending on the observer's structural position. A hiring
 *   rule, promotion criterion, resource allocation formula, or disciplinary
 *   standard is experienced by privileged actors as pure coordination (rope)
 *   and by marginalized actors as pure extraction (snare). This is not a
 *   failure of the institution to be fair — it is a mathematical property of
 *   how power asymmetry interacts with indexical classification. The
 *   constraint divergence itself emerges from the interaction of power
 *   position (agent_power), exit options, and extraction flow. When power and
 *   extraction flow align (privileged actor benefits), experienced extraction
 *   is low and classification appears cooperative. When power and extraction
 *   flow oppose (marginalized actor bears costs), experienced extraction is
 *   high and classification appears coercive. The framework does not create
 *   this divergence; it makes it visible. This constraint story models the
 *   divergence as itself a structural mechanism that institutions maintain,
 *   often unconsciously, because beneficiaries rationalize the extraction as
 *   coordination while marginalized actors bear the costs of both the actual
 *   coordination and the hidden extraction.
 *
 * KEY AGENTS:
 *   - Privileged Position Holders: Primary beneficiary (institutional/arbitrage) — experience the constraint's mechanisms as coordination; their power position aligns with extraction flow; capacity to define the constraint as 'just how things work'
 *   - Marginalized Position Holders: Primary victim (powerless/trapped) — experience the same mechanisms as extraction; no meaningful exit; bear suppression costs of both coordination and enforcement overhead
 *   - Institutional Authority Structures: Secondary beneficiary (institutional/arbitrage) — maintain the constraint to preserve existing power distributions; benefit from ability to rationalize as natural/inevitable
 *   - Collective Epistemic Capacity: Abstract victim — the institution's capacity to learn and adapt is degraded by epistemically isolated groups (privileged groups see coordination, marginalized groups see extraction, no shared diagnostic framework)
 *   - Boundary Holders: Liminal actors (moderate/constrained) — experience genuine hybridity; critical for detecting when the constraint becomes piton (when even boundary holders stop crediting the coordination rationale)
 *   - Institutional Carriers: Bureaucratic agents (institutional/arbitrage) — transmit the constraint across time; increasingly recognize it as theatrical/piton; tension between loyalty to institution and recognition of inertia
 *   - Structural Reformers: Powerful with exit (powerful/mobile) — rare position; can see both coordination and extraction; face opposition from privileged beneficiaries defending status quo
 *   - Coalition for Structural Transparency: Organized agents (organized/constrained) — build alternative legitimacy structures; push toward sunset through indexing, transparency, participatory design; see the constraint as solvable, not natural
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_position_constraint_divergence, 0.52).
domain_priors:suppression_score(structural_position_constraint_divergence, 0.48).
domain_priors:theater_ratio(structural_position_constraint_divergence, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_position_constraint_divergence, extractiveness, 0.52).
narrative_ontology:constraint_metric(structural_position_constraint_divergence, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(structural_position_constraint_divergence, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_position_constraint_divergence, tangled_rope).
narrative_ontology:human_readable(structural_position_constraint_divergence, "Structural Position Constraint Divergence").
narrative_ontology:topic_domain(structural_position_constraint_divergence, "social_systems/institutional_dynamics/stratification_mechanics").

domain_priors:requires_active_enforcement(structural_position_constraint_divergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_position_constraint_divergence, privileged_position_holders).
narrative_ontology:constraint_beneficiary(structural_position_constraint_divergence, institutional_authority_structures).
narrative_ontology:constraint_victim(structural_position_constraint_divergence, marginalized_structural_position_holders).
narrative_ontology:constraint_victim(structural_position_constraint_divergence, collective_epistemic_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRIVILEGED ACTOR (ROPE) — From a position of institutional power with arbitrage optionality, the constraint appears as pure coordination. The same mechanism that extracts from others is experienced as beneficial coordination: resource allocation rules, hiring practices, promotion criteria, institutional norms. Low experienced extraction because power position aligns with extraction flow. The privileged actor can credibly claim 'this is just how coordination works' because their structural position makes the statement true for them.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED ACTOR (SNARE) — From a position of marginalization with no meaningful exit, the identical institutional mechanism appears as pure extraction. Barrier creation, opportunity foreclosure, epistemic suppression, resource denial. The extraction is experienced as maximally coercive because exit options are foreclosed and power position is opposite the extraction flow. The marginalized actor cannot exit the constraint without abandoning structural participation in the institution.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: BOUNDARY HOLDER (TANGLED ROPE) — From a transitional or boundary position (quasi-insider with high costs to full participation, quasi-outsider with some access), the constraint exhibits both coordination and extraction. Experiences the same mechanism as both enabling and constraining: some benefits of participation, some costs of exclusion. This is the perspectival position of the aspiring member, the internal critic, the sector-switcher. Genuine hybridity: not rationalization, but structural ambiguity of a liminal position.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: STRUCTURAL REFORMER (TANGLED ROPE) — From a position of actual power but with genuine mobility (can exit or restructure), the constraint is experienced as manageable hybridity. The reformer sees the coordination function (legitimate institutional needs) AND the extractive layer (unnecessary enforcement overhead). Unlike the privileged actor who rationalizes the extraction as coordination, the reformer can see both and has structural capacity to separate them. Classification remains tangled rope because even reformers face real enforcement constraints from beneficiaries defending the status quo.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL CARRIER (PITON) — From the perspective of institutional agents (bureaucracies, professional associations, hierarchies) responsible for transmitting the constraint across time, the mechanism appears as increasingly theatrical. The founding coordination rationale (resource allocation, skill-matching, risk distribution) has atrophied while the enforcement apparatus persists. Theater ratio high: rules maintained because 'that is how we do things,' not because the functional need justifies the suppression. Institutional actors gradually recognize their own constraint as degraded piton rather than functional rope.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL RISK — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical vantage, the constraint risks being naturalized: 'some agents are always more positioned than others,' 'hierarchy is inherent to institutions,' 'unequal experience is inevitable.' This perspective mistakes a mathematical property of indexical classification for a natural law of social structure. The false summit: treating the perspectival divergence itself as an immutable feature rather than a structural property of power-asymmetric institutions that could be designed differently.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: COALITION FOR STRUCTURAL TRANSPARENCY (SCAFFOLD) — From the position of organized actors (coalition of marginalized groups, internal reformers, external auditors) building alternative legitimacy structures, the constraint is experienced as temporary and solvable. Indexed classification systems (like Deferential Realism itself), transparency requirements, participatory design, and distributed authority can reduce the perspectival gap. This perspective sees the constraint as having a sunset: once structural divergence is explicit and measured, institutions face pressure to design mechanisms that produce comparable classifications across positions. Sunset logic: as transparency mechanisms mature, the ability to maintain pure extraction behind rationalized coordination decreases.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_position_constraint_divergence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_position_constraint_divergence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_position_constraint_divergence, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_position_constraint_divergence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(structural_position_constraint_divergence, TR),
    TR >= 0.70.

:- end_tests(structural_position_constraint_divergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from marginalized positions through both direct mechanisms (resource denial, opportunity foreclosure) and indirect mechanisms (suppression of alternative framings, enforcement of coordination rationale). But the extraction is not total (snare-level ε ≥ 0.66) because the constraint provides genuine coordination benefits to privileged actors and some coordination value to marginalized actors (they do gain access to institutional coordination, even if asymmetrically). The 0.52 value reflects that the constraint genuinely coordinates while also genuinely extracting — it is neither pure rope nor pure snare. Suppression (0.48): Moderate. The constraint's suppression operates on multiple levels: (1) Structural suppression — legal/economic barriers to exit for marginalized actors; (2) Epistemic suppression — difficulty for marginal ized actors to articulate extraction because privileged actors credibly claim it is coordination; (3) Organizational suppression — penalties for those who publicly challenge the coordination narrative. Theater ratio (0.55): Moderate-high, trending higher. Early in the interval (t=0), theater is low — the constraint genuinely coordinates and genuinely extracts with relatively little performative overhead. Over the interval, theater increases as institutional actors gradually recognize the extractive layer (piton perspective emerging) while maintaining the coordination ritual. By t=10, the theater has increased because the coordin ation rationale is held more defensively (less credible to boundary holders and reformers) while enforcement continues. The trajectory suggests institutional awareness creeping into piton territory. The measurements show all three metrics rising, indicating accumulation: increasing extraction, increasing enforcement requirement, increasing theater — the constraint is consolidating rather than stabilizing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence across the first three core perspectives. The privileged actor (institutional/arbitrage) classifies as rope (coordination), the marginalized actor (powerless/trapped) classifies as snare (pure extraction), and the boundary holder (moderate/constrained) classifies as tangled rope (hybrid). These are not different evaluations of the same type — they are fundamentally different experiences of the same mechanism. The rope perspective is not rationalization; the privileged actor genuinely experiences coordination benefits. The snare perspective is not hypersensitivity; the marginalized actor genuinely bears asymmetric costs. The gap is not bridged by better communication or perspective-taking — it is structural, determined by position. The reformer perspective reveals that the gap is not immutable: a powerful actor with exit can see both rope (coordination function) and snare (extraction enforcement) simultaneously in tangled rope form. The piton and mountain perspectives reveal the downstream risk: institutional actors gradually see piton (meaningless ritual), and analytical observers risk seeing mountain (naturalized inevitable). The coalition perspective reveals the counter-structure: organized actors see this as a solvable tangled rope with sunset clause, not an immutable divergence. The perspectival gap is real, structural, and measurable.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain is central to this constraint. The chi formula χ = ε × f(d) × σ(S) distributes extractiveness differently across positions based on d (directionality), which is derived from beneficiary/victim status + exit options + power position. For the privileged actor with arbitrage exit: d ≈ 0.05 (full beneficiary), f(d) ≈ -0.12 (negative, suppressing effective extraction), χ_low → rope classification. For the marginalized actor with trapped exit: d ≈ 0.95 (full target), f(d) ≈ 1.42 (maximum, amplifying effective extraction), χ_high → snare classification. The identical base extractiveness (ε ≈ 0.52) produces opposite experienced extractiveness values through the deriv ation chain. This is not subjective interpretation — it is mathematical consequence of the indexical tuple. The constraint works by maintaining this divergence: privileged actors experience low χ and credibly describe coordination, marginalized actors experience high χ and experience extraction, no shared diagnostic frame emerges. Overrides are not needed here — the canonical derivation is accurate. The directionality divergence is the constraint's core mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divergence_observability,
    'Is the perspectival divergence a real structural property or an artifact of how the DR classification system frames observations?',
    'Test across multiple classification frameworks (critical theory, institutional economics, behavioral game theory, organizational sociology). Do all frameworks detect similar divergence patterns between privileged and marginalized positions?',
    'If real: the constraint is a deep structural feature of power-asymmetric institutions. The DR framework is detecting something fundamental. If artifact: the divergence is an property of how the DR framework''s indexical method works, not of social structure per se. Methodological implication, not empirical falsification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divergence_observability, conceptual, 'Whether perspectival divergence is structural or methodological').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function be technically separated from the extractive enforcement layer, or are they intrinsically coupled in most institutions?',
    'Case analysis of institutional redesigns: experiments with distributed authority, participatory governance, transparent criteria, decoupled compensation. Track whether reducing extraction also reduces coordination capacity or whether the coupling is contingent on specific institutional designs.',
    'If separable: marginalized actors have a real path to experiencing rope instead of snare through institutional redesign. If coupled: the extraction and coordination are locked together by deep structural necessity, and change requires replacing the institution, not reforming it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Technical separability of coordination function from extraction enforcement').

omega_variable(
    power_position_indexicality,
    'Is power position the only driver of perspectival divergence, or do other context dimensions (historical experience, group identity, epistemic location) independently shift classification?',
    'Multivariate analysis of perspectives from agents with same power position but different historical/epistemic context; comparison of uniform-power-position perspectives across different institutions.',
    'If power position dominates: reducing power asymmetry directly narrows the perspectival gap. If other dimensions are significant: institutional change requires addressing epistemic capture, historical trauma, identity lock, and other mechanisms beyond power equalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_position_indexicality, empirical, 'Relative causal weight of power position vs. other context dimensions').

omega_variable(
    false_summit_trigger_threshold,
    'What level of perspectival divergence triggers the false summit detection signature in analytical observers? Is there a threshold magnitude of gap, or do all power-asymmetric constraints risk mountain misclassification?',
    'Corpus analysis: examine the gap magnitude (privileged→rope, marginalized→snare) across constraint types. Correlation with false_summit_mountain signature firing rate. Determine if high-divergence institutions (e.g., caste systems, colonialism) produce uniform false-summit patterns.',
    'If threshold exists: only high-divergence constraints risk mountain misclassification; low-divergence institutions may genuinely be rope or scaffold. If universal: all power-asymmetric institutions risk analytical false summits, and the framework''s reliability on civilization-scale analysis is compromised without explicit index checking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_trigger_threshold, empirical, 'Threshold for false summit signature in power-asymmetric institutions').

omega_variable(
    marginalized_actor_coalition_power,
    'Can marginalized actors at the snare perspective coordinate into the organized constraint perspective, and does coalition power actually shift their experienced classification?',
    'Historical analysis of successful organizing campaigns, mutual aid networks, and counter-institutional movements. Track whether organized marginalized actors report different constraint experience (lower suppression, higher exit options, classification shift toward tangled_rope or scaffold). Compare with unorganized marginalized cohorts.',
    'If yes: the snare classification is not immutable; collective action is a real exit mechanism that the DR framework should reflect. Perspective should shift from (powerless/trapped) to (organized/constrained) or (organized/mobile). If no: ''organization'' is a label applied to marginalized groups that doesn''t change their structural position. Snare persists despite nominal coalition; requires deeper structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_actor_coalition_power, empirical, 'Coalition power of marginalized actors and classification shift').

omega_variable(
    institutional_transparency_mechanism_effectiveness,
    'Do transparency mechanisms and indexed classification systems actually reduce the perspectival divergence, or do they become incorporated into the constraint itself (theater)?',
    'Empirical tracking of institutions that adopt transparency/indexing mechanisms (diversity metrics, equity audits, participatory governance frameworks, DR-like classification systems). Measure: (1) Does perspectival gap decrease? (2) Does marginalized actor experienced extraction decrease? (3) Or do metrics become performative theater while structural extraction persists?',
    'If effective: the scaffold perspective is structural reality, not aspiration. Institutional design changes can reduce the constraint''s extractiveness. Sunset logic is valid. If theater: transparency becomes part of the piton — maintained apparatus without functional change. The constraint persists at same extractiveness, now with awareness-washing. Requires deeper structural reform, not just transparency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_transparency_mechanism_effectiveness, empirical, 'Effectiveness of transparency mechanisms in reducing perspectival divergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_position_constraint_divergence, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spcd_tr_t0, structural_position_constraint_divergence, theater_ratio, 0, 0.38).
narrative_ontology:measurement(spcd_tr_t5, structural_position_constraint_divergence, theater_ratio, 5, 0.48).
narrative_ontology:measurement(spcd_tr_t10, structural_position_constraint_divergence, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(spcd_be_t0, structural_position_constraint_divergence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spcd_be_t5, structural_position_constraint_divergence, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(spcd_be_t10, structural_position_constraint_divergence, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(spcd_su_t0, structural_position_constraint_divergence, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(spcd_su_t5, structural_position_constraint_divergence, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(spcd_su_t10, structural_position_constraint_divergence, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_position_constraint_divergence, resource_allocation).
narrative_ontology:affects_constraint(structural_position_constraint_divergence, institutional_rationalization).
narrative_ontology:affects_constraint(structural_position_constraint_divergence, epistemic_isolation_by_position).
narrative_ontology:affects_constraint(structural_position_constraint_divergence, exit_option_collapse).
narrative_ontology:affects_constraint(structural_position_constraint_divergence, privilege_naturalization).

% DUAL FORMULATION NOTE:
% Structural Position Constraint Divergence is the meta-level constraint that describes the framework's operation itself. Downstream constraints like institutional_rationalization, epistemic_isolation, exit_option_collapse, and privilege_naturalization are specific mechanisms through which the divergence is maintained in particular domains. This story models the generic divergence property; sibling stories model domain-specific instantiations. The network relationship is causal: the generic divergence drives the domain-specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
