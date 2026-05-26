% ============================================================================
% CONSTRAINT STORY: manufacturer_standardization_incentive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manufacturer_standardization_incentive, []).

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
 *   constraint_id: manufacturer_standardization_incentive
 *   human_readable: Manufacturer Standardization Incentive in Keyboard Layout Lock-In
 *   domain: technology_history/economic_sociology/path_dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persistence is a canonical exemplar in
 *   technology history and path-dependence studies. David's (1985)
 *   influential work argued that QWERTY represents a suboptimal standard
 *   locked in by network effects and early manufacturer choices, with
 *   superior alternatives (Dvorak) unable to gain adoption despite their
 *   ergonomic advantages. This interpretation has been contested by Liebowitz
 *   and Margolis (1990), who argued that historical evidence for QWERTY
 *   inferiority is weak and that beneficiary-hunting analysis may
 *   artifactually construct lock-in narratives. The constraint tests the
 *   analytical framework itself: does the Deferential Realism classification
 *   system discover actual beneficiaries and structural extraction, or does
 *   focus on finding winners in path-dependent systems construct beneficiary
 *   narratives that dissolve under epistemological scrutiny? The constraint
 *   exhibits all six classification types across perspectives, making it a
 *   critical diagnostic case for evaluating whether beneficiary declarations
 *   correspond to structural reality or to analytical frame imposition.
 *
 * KEY AGENTS:
 *   - Typewriter Manufacturers: Primary beneficiary candidate (institutional/arbitrage) — coordination benefits from standardization, suppression of competing layouts through installed base effects
 *   - Alternative Layout Designers (Dvorak, Colemak researchers): Primary victim (moderate/constrained) — bore R&D costs, unable to commercialize superior designs due to network effects and established infrastructure
 *   - Typing Users: Secondary victim (powerless/trapped) — locked into QWERTY through training path-dependence and lack of viable alternatives; bear ergonomic costs if alternatives are actually superior
 *   - Digital Device Manufacturers: Institutional actor (institutional/arbitrage) — maintained QWERTY as default for compatibility but possessed technical capacity to support alternatives
 *   - Ergonomic Researchers and Open-Source Communities: Organized coalition (organized/mobile) — digitization enabled near-zero switching costs, reducing lock-in mechanism to institutional ritual
 *   - Standards Bodies and Technology Incumbents: Institutional perpetuators (institutional/arbitrage) — maintain QWERTY as default through consensus and convention, benefiting from reduced specification burden
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent technological choice as inevitable standardization dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manufacturer_standardization_incentive, 0.38).
domain_priors:suppression_score(manufacturer_standardization_incentive, 0.42).
domain_priors:theater_ratio(manufacturer_standardization_incentive, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manufacturer_standardization_incentive, extractiveness, 0.38).
narrative_ontology:constraint_metric(manufacturer_standardization_incentive, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(manufacturer_standardization_incentive, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manufacturer_standardization_incentive, tangled_rope).
narrative_ontology:human_readable(manufacturer_standardization_incentive, "Manufacturer Standardization Incentive in Keyboard Layout Lock-In").
narrative_ontology:topic_domain(manufacturer_standardization_incentive, "technology_history/economic_sociology/path_dependence").

domain_priors:requires_active_enforcement(manufacturer_standardization_incentive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manufacturer_standardization_incentive, typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(manufacturer_standardization_incentive, incumbent_technology_producers).
narrative_ontology:constraint_victim(manufacturer_standardization_incentive, alternative_layout_innovators).
narrative_ontology:constraint_victim(manufacturer_standardization_incentive, user_ergonomic_optimization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ERGONOMIC OPTIMIZATION (SNARE) — Users and alternative layout designers are structurally trapped by network effects. Switching to layouts with superior ergonomic properties (Dvorak, Colemak) carries prohibitive costs: relearning time, incompatibility with existing equipment and social infrastructure, loss of transferable skills. The victim bears full extraction cost with zero exit option. Individual users cannot coordinate escape; the constraint persists through suppression of viable alternatives.
constraint_indexing:constraint_classification(manufacturer_standardization_incentive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE LAYOUT PIONEER (TANGLED ROPE) — Dvorak and other innovators faced mixed incentives. They contributed to coordination (publishing layout research, enabling ergonomic innovation discussion) while simultaneously experiencing extraction: their superior designs could not gain adoption due to installed base lock-in, making their intellectual contribution uncompensable. They bore R&D costs with minimal market reward. Constrained exit: could patent and publish but could not force adoption against network effects.
constraint_indexing:constraint_classification(manufacturer_standardization_incentive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TYPEWRITER MANUFACTURER (ROPE) — Standardization on QWERTY solved a genuine coordination problem: manufacturers could produce equipment compatible with typist training, stenographers could move between employers, business infrastructure stabilized around uniform keyboard layouts. Manufacturers benefited from reduced design variation costs and market expansion. From this perspective, the constraint functions as pure coordination — the manufacturer experiences the standard as enabling rather than constraining.
constraint_indexing:constraint_classification(manufacturer_standardization_incentive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL TRANSITION COALITION (SCAFFOLD) — Organized actors (computer manufacturers, standards bodies, ergonomic researchers) viewed the keyboard layout lock-in as a temporary constraint solvable through digital transition. Unlike typewriter-era path dependence, software can remap keys costlessly, and digital devices could support multiple layouts. This perspective sees the constraint as degrading over time (theater rises as lock-in becomes purely performative in digital contexts). Sunset clause: hardware independence and software configurability enable escape without retraining infrastructure.
constraint_indexing:constraint_classification(manufacturer_standardization_incentive, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY COMPLIANCE SYSTEM (PITON) — In the digital era, QWERTY keyboard mapping persists despite near-zero technical necessity. Software can implement any layout; hardware supports arbitrary key assignment; switching costs are now behavioral, not technical. QWERTY persists through institutional inertia: manufacturers maintain it because users expect it, users maintain it because equipment provides it, standards bodies preserve it because institutional consensus treats it as the baseline. Theater ratio is high: the constraint's functional coordination role (typewriter-era interoperability) has atrophied, but the layout persists as ritual default.
constraint_indexing:constraint_classification(manufacturer_standardization_incentive, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some degree of keyboard standardization appears inevitable: users must coordinate on a common interface, manufacturing complexity requires design constraints, and the 'first major standardization' will have arbitrary elements that persist through path dependence. This perspective risks naturalizing QWERTY as an instance of inevitable standardization-path-dependence dynamics. However, the structural data contradicts this: identifiable beneficiaries (manufacturers gaining coordination benefits) and victims (alternative designers losing market access) exist; the constraint exhibits active enforcement (institutional perpetuation despite technical malleability). This is a false summit — what appears as natural standardization law is actually a contingent institutional choice maintained through beneficiary power.
constraint_indexing:constraint_classification(manufacturer_standardization_incentive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manufacturer_standardization_incentive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manufacturer_standardization_incentive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manufacturer_standardization_incentive, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(manufacturer_standardization_incentive, TR),
    TR >= 0.70.

:- end_tests(manufacturer_standardization_incentive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits measurable asymmetries but ambiguous beneficiary intent. Typewriter manufacturers gained coordination benefits (genuine, not purely extractive), but the network effects that exclude alternatives do constitute extraction against alternative designers. The base extractiveness reflects this mixture: coordination on QWERTY solved real typewriter-era problems, but the lock-in persists beyond its justifying conditions. Suppression (0.42): Moderate. Barriers to alternative adoption are real (retraining costs, equipment incompatibility, network effects, institutional defaults) but not absolute. Alternative layouts exist, some communities use them, and digital devices technically support them. Suppression is structural (network effects are real), not coercive (no explicit prohibition). Theater ratio (0.55): Moderate-high and increasing. In the typewriter era, QWERTY's function was genuine coordination — manufacturers needed a standard to train compatible typists, and standardization reduced design variation costs. Theater was low (the layout solved the real problem of incompatibility). In the digital era, QWERTY's functional role has atrophied: software supports arbitrary layouts costlessly, devices need not enforce mechanical constraints, and the standard persists primarily through institutional convention and user familiarity. Theater increases as the constraint's functional justification decays but institutional enforcement persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification while exposing beneficiary-hunting epistemology. The manufacturer sees genuine coordination (Rope) — standardization enabled market expansion and operational efficiency. The alternative designer sees extraction (Tangled Rope) — their superior innovations could not achieve adoption because of existing lock-in, making their effort uncompensated. The trapped user sees snare — unable to access ergonomically superior alternatives due to network effects. The digital transition coalition sees sunset (Scaffold) — technical constraints that enabled lock-in are dissolving, and the institutional structure maintaining it is degrading. The legacy system sees inertial persistence (Piton) — QWERTY functions as ritual default despite evaporated technical necessity. The civilizational analytical observer risks seeing natural law (Mountain) — that some standardization is inevitable and its arbitrary elements persist through path dependence. The gap reveals that each classification is structurally accurate from its perspective, but the question underlying mandatrophy is empirical: are manufacturers actual beneficiaries (with measurable, sustained advantage from lock-in), or are they incidental to a system that persists through institutional inertia rather than active beneficiary interest?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from agent power, exit options, and beneficiary/victim declarations. Typewriter manufacturers (institutional/arbitrage) experienced low d: they benefit from standardization and can shift to alternative standards costlessly if market demand shifted. Users (powerless/trapped) experience high d: they cannot exit without retraining. Alternative designers (moderate/constrained) experience intermediate d: they could theoretically develop their layouts further but cannot achieve market adoption. The analytical observer (analytical/analytical) occupies the highest d position from which to perceive the full structure: they see that beneficiary identity is ambiguous (manufacturers benefited, but did they deliberately engineer lock-in or did lock-in emerge from uncoordinated choices?). This ambiguity triggers the beneficiary-artifactual omega: analytical focus on finding beneficiaries may construct them rather than discover them. The framework's capacity to expose this ambiguity is its diagnostic strength.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that all six types are epistemically defensible and that the choice among them depends on empirical facts (Did manufacturers deliberately engineer lock-in? Are alternatives actually superior?) and conceptual commitments (Does the analytical frame discover beneficiaries or construct them?). The typewriter manufacturer perspective (Rope) is genuine if standardization solved real coordination problems. The user perspective (Snare) is genuine if alternatives are objectively superior and network effects are the sole barrier. The scaffold perspective is genuine if digital technology has actually undermined the lock-in mechanism. The piton perspective is genuine if institutional inertia rather than beneficiary interest maintains the standard. No single type is 'correct' — the presheaf of perspectives and the unresolved omegas together constitute the constraint's logical structure. The critical unresolved question is whether beneficiary-hunting analytical frameworks (of which Deferential Realism's beneficiary/victim declarations are an instance) discover actual structural beneficiaries or construct them as analytical artifacts. This constraint is a test case for the framework itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qwerty_optimality_empirical_status,
    'Is QWERTY actually inferior to alternatives (Dvorak, Colemak) on ergonomic and productivity metrics, or does the ''superiority'' of alternatives depend on contested measurement frameworks and implementation assumptions?',
    'Meta-analysis of comparative typing studies controlling for: practice duration, finger strength distribution variance, task type (prose vs code vs numbers), device ergonomics independent of layout, and measurement bias in studies funded by alternative-layout advocates',
    'If QWERTY is objectively inferior: constraint is pure extraction (victims bear real costs for beneficiary coordination). If alternatives are context-dependent or measurement-dependent: constraint involves genuine coordination tradeoffs (not pure snare from user perspective). Affects whether ''ergonomic optimization'' victim framing is structural or constructed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qwerty_optimality_empirical_status, empirical, 'Empirical status of QWERTY vs alternative layout productivity claims').

omega_variable(
    beneficiary_intentionality_ambiguity,
    'Did typewriter manufacturers deliberately choose QWERTY to lock in users and prevent disruptive innovation, or did QWERTY emerge through uncoordinated competitive design choices that happened to converge, with lock-in arising as an unintended consequence?',
    'Historical analysis of manufacturer correspondence, patent filings, and design rationales; identification of explicit strategy vs emergent path-dependence from competitive pressures. Documentation of whether standardization was chosen to exclude alternatives or to solve coordination problems.',
    'If deliberate lock-in strategy: beneficiaries knowingly engineered extraction (snare confirmed from user perspective; tangled rope confirms beneficiary status). If emergent path-dependence: beneficiaries exist but did not intentionally create extraction (constraint may be rope with unintended asymmetries, or piton maintained through inertia rather than active strategy). Affects mandatrophy resolution and whether constraint should be classified as extraction or coordination with asymmetric residuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intentionality_ambiguity, empirical, 'Whether QWERTY lock-in was deliberate manufacturer strategy or emergent path-dependence').

omega_variable(
    alternative_layout_adoption_barriers_decomposition,
    'Are switching costs that prevent Dvorak adoption primarily network-effect suppression (structural), behavioral/habitual (internalized), or institutional/standards-based (regulatory enforcement)?',
    'Comparative analysis of adoption barriers: network effects (market size of alternative-layout users), training costs (relearning curves), institutional resistance (standards bodies, employer policies), and behavioral entrenchment (habit vs rational switching calculation). Testing whether removing network effects (software switching, employer support programs) changes adoption rates as predicted by network effect theory.',
    'If primarily network-effect suppression: constraint is structural snare (high f(d) for trapped agent). If primarily behavioral entrenchment: constraint involves identity-locked agent (user identity fused with QWERTY competency), reclassifying to rope from identity-locked perspective. If institutional: constraint is active enforcement maintaining lock-in. Decomposition clarifies which victim framing is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_layout_adoption_barriers_decomposition, empirical, 'Decomposition of alternative layout adoption barriers into network effects, behavioral habit, and institutional enforcement').

omega_variable(
    beneficiary_artifactual_construction,
    'Does analytical focus on ''beneficiaries'' (manufacturers, standards bodies, incumbent producers) artifactually construct a beneficiary class by hunting for winners in a path-dependent system, or do identifiable agents exhibit genuine structural benefit from the lock-in?',
    'Counterfactual analysis: would manufacturers have lower coordination costs under a fragmented keyboard landscape? Would they have higher profit margins under lock-in vs open competition? Comparative historical analysis of markets where standards were MORE vs LESS entrenched to examine beneficiary emergence. Epistemic audit: whether beneficiary frame is an analytical lens imposed on the data or whether beneficiaries actually exist with predictable, measurable, reproducible advantage.',
    'If beneficiaries are analytical artifacts: constraint is piton maintained by institutional momentum, not tangled rope maintained by beneficiary power. Classification shifts from active extraction (beneficiaries keeping it locked) to degraded coordination (nobody gains enough to actively maintain, but inertia persists). If beneficiaries are structural: constraint is tangled rope maintained by identifiable agent interests. Diagnostic question for beneficiary-hunting epistemology itself: does the framework discover beneficiaries or construct them?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_artifactual_construction, conceptual, 'Whether beneficiary-hunting analysis discovers actual structural beneficiaries or constructs them as analytical artifacts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manufacturer_standardization_incentive, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mfg_std_theater_typewriter_era, manufacturer_standardization_incentive, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mfg_std_theater_early_computer, manufacturer_standardization_incentive, theater_ratio, 50, 0.42).
narrative_ontology:measurement(mfg_std_theater_contemporary, manufacturer_standardization_incentive, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(mfg_std_extract_typewriter_era, manufacturer_standardization_incentive, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mfg_std_extract_early_computer, manufacturer_standardization_incentive, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(mfg_std_extract_contemporary, manufacturer_standardization_incentive, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manufacturer_standardization_incentive, information_standard).
narrative_ontology:affects_constraint(manufacturer_standardization_incentive, standardization_path_dependence).
narrative_ontology:affects_constraint(manufacturer_standardization_incentive, network_effects_lock_in).

% DUAL FORMULATION NOTE:
% The manufacturer standardization incentive is part of a larger constraint family examining path-dependence dynamics. The upstream constraint is 'standardization path dependence' (the general phenomenon that early choices persist due to network effects), which has lower extractiveness and appears as rope from most perspectives. This story focuses on the manufacturer incentive structure and beneficiary role, treating the path-dependence as the mechanism rather than the constraint itself. These are distinct: path-dependence is the structural mechanism; manufacturer incentives are the agency question about who benefits and whether they deliberately engineer the lock-in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manufacturer_standardization_incentive, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
