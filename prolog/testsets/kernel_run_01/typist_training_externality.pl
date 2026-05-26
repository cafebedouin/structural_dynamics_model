% ============================================================================
% CONSTRAINT STORY: typist_training_externality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_typist_training_externality, []).

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
 *   constraint_id: typist_training_externality
 *   human_readable: QWERTY Lock-in and Typist Training Externality
 *   domain: technology_history/economic_sociology/path_dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persistence exemplifies a canonical
 *   path-dependence puzzle: did lock-in mechanisms deliberately extract value
 *   from alternative-keyboard adopters, or did QWERTY dominance emerge from
 *   historical contingency without clear beneficiaries strategically
 *   enforcing it? This constraint tests whether beneficiary-hunting
 *   frameworks discover actual extraction structures or construct them
 *   artifactually. The empirical history shows: (1) QWERTY's origins lay in
 *   mechanical lever design (Sholes typewriter, 1870s) rather than ergonomic
 *   optimization or strategic lock-in planning; (2) alternatives (Dvorak,
 *   developed 1932) demonstrated superior ergonomic properties but failed to
 *   displace QWERTY despite massive training investments; (3) the training
 *   infrastructure (typing schools, labor certification, classroom curricula)
 *   converged on QWERTY through institutional coordination rather than
 *   through explicit exclusion of alternatives. The constraint exhibits
 *   structural lock-in (switching costs accumulate with scale, making
 *   alternatives economically unviable) without clear evidence of intentional
 *   enforcement. This makes it a diagnostic case for whether lock-in and
 *   extraction are synonymous or whether path-dependent constraints can
 *   emerge without extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Typist Learners (Young cohorts entering training): Powerless/trapped (biographical horizon) — human capital investment in QWERTY makes alternative layouts structurally unavailable; switching post-training incurs severe retraining cost with no job-market offsetting benefit
 *   - Keyboard Manufacturers (Remington, Olympia, IBM, etc.): Institutional/arbitrage (immediate horizon) — benefit from QWERTY standardization through simplified manufacturing and supply-chain coordination; niche production of alternative-layout keyboards available but carries minimal market demand
 *   - Typing Pedagogy Institutions (Schools, commercial typing courses, vocational training): Institutional/arbitrage (immediate horizon) — streamlined curriculum through standardized QWERTY instruction; would theoretically benefit equally from any universal standard, making their beneficiary status ambiguous
 *   - Alternative Keyboard Developers (Dvorak Institute, Colemak community, ergonomic researchers): Moderate/constrained (generational horizon) — solve genuine ergonomic problems but face suppressed market demand due to QWERTY training network effects; niche sustainable communities exist but unable to reach critical mass
 *   - Software/Hardware Accessibility Ecosystem: Organized/constrained (generational horizon) — voice input, predictive text, mobile swipe keyboards, and gesture input create parallel pathways that are gradually reducing QWERTY lock-in dependency
 *   - Analytical Observer: Analytical/analytical (civilizational horizon) — risks naturalizing path-dependent contingency as immutable law of technology infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(typist_training_externality, 0.38).
domain_priors:suppression_score(typist_training_externality, 0.42).
domain_priors:theater_ratio(typist_training_externality, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(typist_training_externality, extractiveness, 0.38).
narrative_ontology:constraint_metric(typist_training_externality, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(typist_training_externality, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(typist_training_externality, tangled_rope).
narrative_ontology:human_readable(typist_training_externality, "QWERTY Lock-in and Typist Training Externality").
narrative_ontology:topic_domain(typist_training_externality, "technology_history/economic_sociology/path_dependence").

domain_priors:requires_active_enforcement(typist_training_externality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(typist_training_externality, qwerty_keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(typist_training_externality, typing_pedagogy_institutions).
narrative_ontology:constraint_victim(typist_training_externality, alternative_keyboard_adopters).
narrative_ontology:constraint_victim(typist_training_externality, future_typists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED TYPIST (SNARE) — Individual learner enters a training ecosystem where QWERTY dominance is near-universal. Learning alternative layouts requires forgoing access to standard typing instruction, computer labs, and workplace keyboard conventions. The typist's human capital investment in QWERTY creates irreversible path-dependence. No realistic exit option — switching layouts post-training incurs severe retraining cost with no offsetting benefit in job market or hardware availability. Suppression is structural: the training infrastructure itself prevents exit.
constraint_indexing:constraint_classification(typist_training_externality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE KEYBOARD DEVELOPER (TANGLED ROPE) — Dvorak and Colemak developers benefit from genuine coordination: they solve real ergonomic problems and create intellectual property with niche audiences. But they also bear extraction: the QWERTY training infrastructure actively suppresses demand for alternatives by making adoption costly. The developer has exit options (niche markets, specialized communities) but faces high costs. Both genuine coordination function (ergonomic improvement) and asymmetric extraction (suppressed market demand) coexist.
constraint_indexing:constraint_classification(typist_training_externality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KEYBOARD MANUFACTURER (ROPE) — QWERTY dominance solves a genuine coordination problem: producing one standard layout rather than managing multiple variants reduces manufacturing complexity and enables interchangeable components. The manufacturer benefits from the QWERTY lock-in through simplified supply chains and economies of scale. From this perspective, the constraint is pure coordination — the manufacturer's interest is perfectly aligned with keeping the system standard. Arbitrage options (producing Dvorak-layout keyboards for niche markets) are available but carry minimal extraction cost.
constraint_indexing:constraint_classification(typist_training_externality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TYPING PEDAGOGY PROFESSION (PITON) — Typing instruction has become substantially ritualized. The pedagogical rationale for QWERTY (finger dexterity, muscle memory, home-row optimization) was historically reasonable but is now questionable given modern ergonomic research showing superior alternatives. Typing instruction persists in curricula through institutional inertia — 'this is how typing is taught' — rather than because QWERTY is pedagogically optimal. The profession sees its own practice as somewhat degraded (theater_ratio = 0.50) but continues because alternatives would require curriculum redesign. Low effective extraction from this perspective because the institution maintains the constraint through tradition rather than active strategic enforcement.
constraint_indexing:constraint_classification(typist_training_externality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SOFTWARE ECOSYSTEM (SCAFFOLD) — Digital transition (touchscreens, voice input, predictive text, phone keyboards) is creating alternative input pathways that do NOT require QWERTY mastery. Voice-to-text, mobile swipe keyboards, and gesture inputs are parallel solutions with sunset logic: as voice recognition and alternative input mature, the requirement to master QWERTY touch-typing declines. Current constraint: software still defaults to QWERTY-compatible input methods. But organized technology actors (Apple, Google, accessibility researchers) are actively building non-QWERTY pathways. Suppression is declining over time as alternatives gain functionality.
constraint_indexing:constraint_classification(typist_training_externality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, once a training infrastructure forms around a technical standard, path-dependence becomes nearly immutable: switching costs accumulate exponentially with scale, making alternatives structurally unreachable. This perspective treats QWERTY lock-in as an inevitable consequence of network effects, not a contingent institutional outcome. Switching would require coordinating billions of typists — a coordination problem so severe it appears as a law of technology. However, the perspective risks naturalizing what is actually a set of contingent institutional choices (pedagogy design, manufacturing standards, labor certification) as immutable technical necessity.
constraint_indexing:constraint_classification(typist_training_externality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: PROGRAMMER / ACCESSIBILITY RESEARCHER (ROPE) — Professional actors with mobility (ability to learn alternative layouts, access to specialized keyboards, authority to customize their environment) experience the constraint as weak coordination: QWERTY is a helpful standard but not binding. These actors can and do switch layouts, remap keyboards, and contribute to alternative input method development. The constraint dissolves at their scale because they have resources and expertise to exit. Classification is Rope because the constraint provides genuine coordination value (universal keyboard availability) without extracting from them — they can arbitrage the benefits.
constraint_indexing:constraint_classification(typist_training_externality, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(typist_training_externality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(typist_training_externality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(typist_training_externality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(typist_training_externality, TR),
    TR >= 0.70.

:- end_tests(typist_training_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits real lock-in costs for alternative-keyboard adopters and imposes training overhead on new typists, but the measured extraction is lower than a pure snare would suggest because: (1) QWERTY appears to have emerged without deliberate strategic enforcement — manufacturers and educators converged on it through coordination rather than intentional lock-in strategy; (2) alternative-keyboard communities sustain niche markets and practice, suggesting suppression is strong but not absolute; (3) digital transition is reducing QWERTY-specific human capital importance (voice input, predictive text, mobile keyboards). Suppression (0.42): Moderate-high. The training infrastructure creates substantial barriers to alternative adoption: typing pedagogy concentrates on QWERTY (< 5% of instructional content addresses alternatives), job-market certification requires QWERTY typing speed, computer labs default to QWERTY hardware, and social network effects mean alternative-layout adopters cannot easily collaborate with QWERTY-trained peers. But suppression is not maximum because (1) niche pedagogy for alternatives exists, (2) software-based keyboard remapping is available, (3) accessibility communities actively develop non-standard input methods. Theater ratio (0.35): Low-moderate. Typing instruction is functionally oriented (teaching transferable motor skill) rather than performative. The pedagogy measures actual learning outcomes (typing speed, accuracy) with modest procedural overhead. Theater has increased somewhat over the interval (from 0.20 to 0.35) as formal credentialing and certification processes emerged, adding ritual layers to what was originally skill-focused training. Claimed type (Tangled Rope): Requires proof of both genuine coordination function and asymmetric extraction. Coordination is genuine: QWERTY standardization reduces training complexity for instructors and ensures compatibility across diverse workplaces and machines. Extraction is real but conditional on beneficiary identity: if manufacturers deliberately engineered lock-in (unproven), extraction is intentional; if QWERTY dominance emerged accidentally, extraction is a side effect of path-dependence rather than a designed feature. The 'requires_active_enforcement = true' declaration reflects that the typing infrastructure actively enforces QWERTY standardization through pedagogy and certification, regardless of whether the original QWERTY choice was strategic.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range from Snare to Mountain reveals the diagnostic ambiguity central to this constraint. Trapped typists (Snare) experience real extraction — they are locked into QWERTY skills with no viable exit. But the beneficiary's perspective (Rope/institutional) shows pure coordination value — manufacturing standardization and curriculum streamlining genuinely solve logistics problems. The piton perspective observes that typing pedagogy persists through institutional inertia despite questionable QWERTY optimality. The mountain perspective risks naturalizing contingent historical choices as inevitable technology laws. The critical gap is between the Snare (extraction is real for typists) and the Rope (coordination solves actual manufacturing/curriculum problems). This gap does NOT resolve to 'Snare is correct' — both are structurally valid. The gap reveals that lock-in and coordination can coexist in a single constraint: the same standardization that enables efficient manufacturing simultaneously prevents typist exit. The perspectival divergence points to the omega ambiguity: is this active extraction (beneficiaries deliberately enforce lock-in) or passive path-dependence (beneficiaries benefit from lock-in without designing it)?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from power/exit/beneficiary-victim structure: Trapped typists (powerless + trapped) experience maximum d ≈ 0.95, producing high f(d) ≈ 1.42. Institutional manufacturers (institutional + arbitrage + beneficiary) experience d ≈ 0.05, producing f(d) ≈ -0.12. Alternative developers (moderate + constrained + victim) experience d ≈ 0.65, producing f(d) ≈ 1.00. Pedagogical institutions pose a directionality puzzle: they are declared beneficiaries but may not strategically enforce QWERTY lock-in — they may be passive coordinators rather than active extractors. No directionality override is declared because the beneficiary status itself is in question (omega_id: beneficiary_identity_ambiguity). If the omega resolves to 'passive coordinators,' the beneficiary declaration should be removed in a revised story.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies the mandatrophy problem in pure form: Does lock-in = extraction? The constraint's extractiveness (0.38) is moderate, placing it in the ambiguous Tangled Rope region rather than clearly above the Snare threshold (ε ≥ 0.46). The mandatrophy manifests as: (1) Snare vs. Rope debate — typists experience extraction (Snare), manufacturers experience coordination (Rope), same constraint; (2) Strategic vs. accidental lock-in — if QWERTY lock-in was designed, the constraint is Tangled Rope with active enforcement; if it emerged accidentally, it may be a path-dependent Rope without strategic extraction; (3) Beneficiary identity question — are manufacturers and pedagogy institutions true beneficiaries deriving sustained advantage from lock-in, or neutral vectors that happen to benefit from any standard? The mandatrophy resolves through omega variables: beneficiary_identity_ambiguity (Are beneficiaries structural or artifactual?), manufacturer_strategic_intent (Was QWERTY chosen strategically or mechanically?), and pedagogy_institutional_incentives (Do institutions specifically benefit from QWERTY or neutral on standard choice?). The perspective taxonomy itself reveals the problem: every perspective from Snare to Mountain is theoretically defensible from its structural position. No single perspective is 'wrong' — the divergence reveals that the constraint's core mechanism is ambiguous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_identity_ambiguity,
    'Are the declared beneficiaries (keyboard manufacturers, pedagogy institutions) genuinely structured to benefit from QWERTY lock-in, or does the analysis artificially construct beneficiaries post-hoc from the observed outcome?',
    'Historical archive analysis: Did manufacturers actively lobby for QWERTY standardization, or did QWERTY dominance emerge accidentally? Did typing pedagogy institutions strategically choose QWERTY for lock-in, or did they simply adopt the dominant layout? Compare explicit strategic documents vs. post-hoc rationalization in institutional histories.',
    'If beneficiaries are genuine: constraint is Tangled Rope with intentional extraction (requires_active_enforcement). If beneficiaries are artifactual: constraint may be pure path-dependence (Rope or Piton) without a clear enforcement structure — the extraction emerges from lock-in rather than from someone choosing to enforce it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_ambiguity, empirical, 'Whether declared beneficiaries actively engineered lock-in or emerged post-hoc from path-dependence').

omega_variable(
    switching_cost_threshold,
    'Below what typist population size would alternative keyboard adoption become structurally feasible? What is the critical mass for non-QWERTY ecosystem emergence?',
    'Empirical observation of actual non-QWERTY communities (Dvorak, Colemak, programming-optimized layouts): track adoption rates, availability of pedagogy, hardware compatibility, social network effects. Test whether niche populations can sustain alternatives without mainstream support.',
    'If threshold is low (< 1% population): constraint is weaker than modeled; alternatives remain viable in niche contexts. If threshold is high (> 10%): QWERTY lock-in is robust across all foreseeable scenarios. Affects classification of future_typists victim group.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_threshold, empirical, 'Population size threshold for non-QWERTY ecosystem viability').

omega_variable(
    path_dependence_vs_optimality,
    'Is QWERTY a local optimum that lock-in has protected, or is it a global minimum that would persist even without lock-in mechanisms?',
    'Ergonomic comparative studies: measure typing speed, accuracy, injury rates (RSI), learning time for QWERTY vs. alternatives in controlled settings. If alternatives show superior ergonomic outcomes, path-dependence is masking a suboptimal equilibrium.',
    'If QWERTY is globally optimal: extraction is minimal — the lock-in protects a genuinely good outcome. If alternatives are superior: extraction is substantial — lock-in forces adoption of a suboptimal standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependence_vs_optimality, empirical, 'Whether QWERTY represents local or global optimum in ergonomic outcomes').

omega_variable(
    manufacturer_strategic_intent,
    'Did early keyboard manufacturers (Sholes, Remington) strategically design QWERTY for lock-in, or was the layout chosen for mechanical reasons (lever interference, typebar design)?',
    'Historical document analysis: Sholes'' patent applications, Remington design specifications, manufacturing correspondence. Compare design rationales against lock-in hypothesis.',
    'If strategic intent: requires_active_enforcement = true (consistent with Tangled Rope, Snare). If mechanical necessity: requires_active_enforcement = false (consistent with path-dependent Rope or Piton). Affects whether enforcement is active or passive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manufacturer_strategic_intent, empirical, 'Whether QWERTY design was intentional lock-in strategy or mechanical artifact').

omega_variable(
    pedagogy_institutional_incentives,
    'Do typing pedagogy institutions (schools, commercial typing courses) benefit from QWERTY standardization through reduced curriculum complexity, or would they benefit equally from any universal standard?',
    'Institutional records: examine typing curriculum design, training costs, career certification pathways. Test whether standardization on an alternative layout (Dvorak) would significantly change pedagogical efficiency.',
    'If institutions actively benefit from QWERTY specifically: they are true beneficiaries (Tangled Rope confirmed). If any standard would serve equally: institutions are neutral vectors, not beneficiaries (constraint becomes path-dependent Rope without clear beneficiary structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogy_institutional_incentives, empirical, 'Whether pedagogy institutions specifically benefit from QWERTY or neutral on layout choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(typist_training_externality, 1870, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(typext_theater_1870, typist_training_externality, theater_ratio, 0, 0.2).
narrative_ontology:measurement(typext_theater_1900, typist_training_externality, theater_ratio, 30, 0.28).
narrative_ontology:measurement(typext_theater_1930, typist_training_externality, theater_ratio, 60, 0.38).
narrative_ontology:measurement(typext_theater_1960, typist_training_externality, theater_ratio, 90, 0.35).

% Extraction over time
narrative_ontology:measurement(typext_extractiveness_1870, typist_training_externality, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(typext_extractiveness_1900, typist_training_externality, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(typext_extractiveness_1930, typist_training_externality, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(typext_extractiveness_1960, typist_training_externality, base_extractiveness, 90, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(typist_training_externality, information_standard).
narrative_ontology:affects_constraint(typist_training_externality, dvorak_adoption_barrier).
narrative_ontology:affects_constraint(typist_training_externality, typing_skill_certification_monopoly).

% DUAL FORMULATION NOTE:
% QWERTY lock-in decomposes into: (1) typist_training_externality (this story) — the training infrastructure that locks in QWERTY skills, with ambiguous beneficiary structure; (2) dvorak_adoption_barrier — the specific market failure preventing alternative adoption despite ergonomic superiority; (3) typing_skill_certification_monopoly — the labor credentialing system that requires QWERTY speed testing. Each has different ε and different beneficiary profiles. The typist_training_externality is upstream: it creates the network effects that power the barrier and certification lock.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
