% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__copenhagen_reading, []).

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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Measurement as Irreducible Epistemic Boundary (Copenhagen Reading)
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The Copenhagen interpretation of quantum mechanics instantiates a
 *   contested reading of the quantum formalism kernel. This reading treats
 *   measurement as a primitive irreducible feature of physical reality that
 *   marks an absolute epistemic boundary: quantum systems exist in
 *   superposition until measured, measurement collapses the wavefunction to
 *   an eigenstate with irreducible indeterminism, and the observer role is
 *   non-eliminable from the description of reality. This constraint is ONE
 *   READING among at least three competing readings (many-worlds, pilot-wave,
 *   objective collapse) of the same underlying formalism. The Copenhagen
 *   reading benefits identifiable institutional actors (pedagogical
 *   establishments, operational formalism advocates, measurement apparatus
 *   authorities) while imposing costs on others (deterministic ontology
 *   traditions, observer-independent realism advocates, researchers seeking
 *   unified quantum mechanics without privileged measurement boundary). The
 *   extraction mechanism is not malicious — it emerges from the institutional
 *   success of Copenhagen pedagogy and its calculational efficiency — but it
 *   operates through suppression of ontological alternatives and enforcement
 *   of measurement as fundamental. The theater ratio (0.62) reflects that
 *   much of Copenhagen's rhetoric (collapse is physical, observers are
 *   irreducible) is decoupled from the formalism's actual predictive
 *   machinery, which would function identically under alternative
 *   interpretations.
 *
 * KEY AGENTS:
 *   - Deterministic Ontology Tradition: Primary victim (powerless/identity_locked) — committed to observer-independent, deterministic reality; must either abandon this identity or resist Copenhagen; Copenhagen denies fundamental reality of this commitment
 *   - Pedagogical Establishment: Primary beneficiary (institutional/arbitrage) — Copenhagen provides canonical, standardized, teachable framework; controls textbook production and curriculum; no meaningful exit cost
 *   - Operational Formalism Advocates: Secondary beneficiary (institutional/arbitrage) — Copenhagen privileges operationalism (measurement outcomes as fundamental) over realism; supports this epistemological tradition
 *   - Measurement Apparatus Authority: Secondary beneficiary (powerful/mobile) — Copenhagen privileges classical measurement apparatus as boundary between quantum and classical; supports this institutional role
 *   - Working Physicists: Secondary victim (moderate/constrained) — benefit from Copenhagen's calculational utility and predictive success; bear cost of suppressed ontological questions; exits constrained by career incentives
 *   - Alternative Interpretation Coalition: Organized agents (organized/constrained) — many-worlds, pilot-wave, objective-collapse researchers; see Copenhagen dominance as temporary coordination lock with sunset as experiments become more sensitive
 *   - Observer-Independent Realism Tradition: Tertiary victim (powerful/identity_locked) — committed to reality independent of observation; must resist Copenhagen's observer non-eliminability claim or abandon identity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.58).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.48).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Measurement as Irreducible Epistemic Boundary (Copenhagen Reading)").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '67c079ff-0662-4dbb-b30a-9643048e694b').
narrative_ontology:cs_kernel_codification('67c079ff-0662-4dbb-b30a-9643048e694b', fixed_text).
narrative_ontology:cs_authority_grounding('67c079ff-0662-4dbb-b30a-9643048e694b', lineage).
narrative_ontology:cs_interpretation_layer_present('67c079ff-0662-4dbb-b30a-9643048e694b').
narrative_ontology:cs_reading_relation('67c079ff-0662-4dbb-b30a-9643048e694b', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('67c079ff-0662-4dbb-b30a-9643048e694b', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('67c079ff-0662-4dbb-b30a-9643048e694b', foundational, measurement_induced_collapse_is_physical).
narrative_ontology:cs_axiom_status(measurement_induced_collapse_is_physical, holdable).
narrative_ontology:cs_axiom_grounding('67c079ff-0662-4dbb-b30a-9643048e694b', measurement_induced_collapse_is_physical, empirically_contingent).
narrative_ontology:cs_axiom('67c079ff-0662-4dbb-b30a-9643048e694b', foundational, observer_role_is_non_eliminable).
narrative_ontology:cs_axiom_status(observer_role_is_non_eliminable, holdable).
narrative_ontology:cs_axiom_grounding('67c079ff-0662-4dbb-b30a-9643048e694b', observer_role_is_non_eliminable, deontological).
narrative_ontology:cs_reference_frame('67c079ff-0662-4dbb-b30a-9643048e694b', operational_measurement_primacy).
narrative_ontology:cs_drift_state('67c079ff-0662-4dbb-b30a-9643048e694b', contemporary_quantum_information_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('67c079ff-0662-4dbb-b30a-9643048e694b', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, pragmatist_epistemology).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, operational_formalism).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, measurement_apparatus_authority).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, deterministic_ontology_tradition).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, observer_independent_realism).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, unified_quantum_mechanics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ONTOLOGICAL REALIST (SNARE) — Agent committed to deterministic, observer-independent reality. The Copenhagen reading makes this commitment identity-locked: to exit would require abandoning the foundational premise that reality exists independent of measurement. The agent is structurally mobile (can adopt alternative readings intellectually) but identity-fused with deterministic ontology. Experiences maximum extraction: the framework denies what the agent's identity takes as fundamental truth. No exit path appears available from within the deterministic worldview.
constraint_indexing:constraint_classification(quantum_formalism__copenhagen_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(universal))).

% PERSPECTIVE 2: WORKING PHYSICIST (TANGLED ROPE) — Derives genuine coordination benefit from Copenhagen formalism (calculational efficiency, empirical prediction accuracy) while bearing extraction cost: must suppress ontological questions, accept measurement as primitive, abandon determinism at measurement events. The constraint enforces operational success at the price of semantic agnosticism. Exits are constrained by career incentives (publication, peer acceptance) that reward Copenhagen pragmatism over ontological questioning.
constraint_indexing:constraint_classification(quantum_formalism__copenhagen_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PEDAGOGICAL ESTABLISHMENT (ROPE) — Universities teaching quantum mechanics via Copenhagen interpretation benefit from a canonical, standardized framework that students learn, replicate, and transmit. The establishment faces no meaningful exit cost — Copenhagen dominance in textbooks is self-reinforcing. Net beneficiary through coordination: the shared formalism enables reliable training and institutional continuity. Effective extraction runs toward this institutional power.
constraint_indexing:constraint_classification(quantum_formalism__copenhagen_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE INTERPRETATION COALITION (SCAFFOLD) — Organized researchers developing many-worlds, pilot-wave, and objective-collapse alternatives. See Copenhagen dominance as a temporary coordination lock with a sunset: as experimental capabilities mature (Bell tests, quantum erasure experiments, quantum computing), the empirical equivalence breaks down and the coalition gains leverage. The constraint is experienced as coordination failure with an addressable exit path, not as extraction. Theater is moderate because the coalition can point to specific experimental predictions that differentiate interpretations.
constraint_indexing:constraint_classification(quantum_formalism__copenhagen_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERPRETIVE ORTHODOXY INSTITUTION (PITON) — Copenhagen interpretation maintains institutional authority through inertia and textbook canonicity rather than through superior predictive or explanatory power. The framework's primary function (coordinating pedagogy and calculation) has been achieved; what persists is the theater (the claim that measurement is fundamental, observer role irreducible, collapse is physical) maintained for institutional continuity. Theater ratio is high because the ontological claims are largely decoupled from the formalism's actual utility.
constraint_indexing:constraint_classification(quantum_formalism__copenhagen_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EXPERIMENTAL PHYSICS COMMUNITY (TANGLED ROPE) — Benefits from Copenhagen's predictive machinery and operationalist framework (measurement outcomes are fundamental to experimental design). Also bears extraction cost: forced to treat measurement apparatus as a privileged classical system, cannot ask deep questions about what happens during the interaction, must suppress the hard problem of why collapse occurs at one scale and not another. Experiences mixed coordination and extraction; has mobile exit options (can adopt alternative interpretations) but organizational norms create barriers.
constraint_indexing:constraint_classification(quantum_formalism__copenhagen_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT CANDIDATE (MOUNTAIN) — From a civilizational perspective, measurement-induced indeterminism appears as an immutable feature of quantum reality: the Schrödinger equation is time-reversible but measurement introduces irreversibility; this asymmetry seems baked into the physics itself. However, the presence of identified beneficiaries (pedagogical establishment, operational formalism, measurement apparatus authority) and victims (deterministic ontology, observer-independent realism) reveals this as a false summit: the 'necessity' of measurement as fundamental and irreducible is not a law of nature but a reading choice that stabilizes particular institutional interests.
constraint_indexing:constraint_classification(quantum_formalism__copenhagen_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quantum_formalism__copenhagen_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_formalism__copenhagen_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, TR),
    TR >= 0.70.

:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Copenhagen benefits pedagogical and institutional actors through standardization and operationalist epistemology, while imposing costs on deterministic and realist traditions through suppression of alternatives. The extraction is not maximal because working physicists genuinely benefit from the formalism's predictive utility (tangled rope structure), not just bearing costs. Suppression (0.48): Moderate. The primary suppression mechanism is institutional: Copenhagen dominance in textbooks, peer review, and pedagogy creates barriers to alternative interpretations, but these barriers are not absolute — alternative research programs persist and grow. Physicists can and do work on alternatives, though at career cost. Theater ratio (0.62): Moderate-high. Copenhagen's ontological claims (collapse is physical, observers irreducible, determinism abandoned) are substantially rhetorical — they do not affect the formalism's calculational content. Many-worlds, pilot-wave, and objective-collapse alternatives make identical empirical predictions in most regimes. The theater has increased over time as experimental precision has grown but experiments have continued to show equivalence across interpretations.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The ontological realist sees a snare that denies fundamental reality. The working physicist sees genuine mixed benefit. The establishment sees pure benefit with coordination. The alternative coalition sees a temporary institutional lock. The institution itself sees degraded theater. The analytical observer risks naturalizing what is an institutional choice. No single perspective is wrong — each accurately describes what Copenhagen looks like from that structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from agent structural position. Deterministic ontologists are victims of Copenhagen's measurement boundary claim (high d toward 1.0) and trapped by identity fusion with determinism (cannot exit cognitively even if they exit institutionally). Working physicists benefit from predictive utility but bear suppression costs (moderate d around 0.65). Pedagogical establishment are beneficiaries with arbitrage exit (low d toward 0.1). Alternative interpretation coalition are victims of Copenhagen dominance but have organized exit paths (moderate-high d around 0.55). The false summit perspective is deliberately included to show how naturalization occurs: the analytical observer risks d around 0.72 (treating measurement as a discovered natural law) when structural analysis reveals beneficiaries and victims, suggesting d should be higher (measurement boundary benefits some agents, harms others).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is NOT RESOLVED here (base_properties.mandatrophy_resolved: false) because the constraint's classification itself depends on reading choice. The Copenhagen reading is internally consistent (ε=0.58 yields tangled rope with measurements showing slight increase over time), but the competing readings (many-worlds, pilot-wave) would yield different ε values and different classifications. The mandatrophy is irresolvable at the single-reading level because the question 'what is the constraint?' is exactly what the kernel dispute concerns. Resolution requires comparing ε values across all three readings and determining empirically which reading's structural assumptions hold in nature. Until that empirical work is complete, mandatrophy remains open. The false summit detection (mountain classification at analytical perspective with identified beneficiaries) is itself a form of mandatrophy awareness — the constraint flags its own vulnerability to naturalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_primitiveness_vs_emergence,
    'Is measurement-induced collapse a primitive irreducible feature of quantum reality, or an emergent phenomenon reducible to decoherence and entanglement in the universal wavefunction?',
    'Empirical: quantum computing experiments testing whether apparent collapse requires special physics or emerges from standard Schrödinger dynamics under decoherence. Theoretical: construction of collapse-free models (many-worlds, pilot-wave, consistent histories) that match all Copenhagen predictions without treating measurement as primitive.',
    'If collapse is primitive: Copenhagen mountain classification confirmed; measurement boundary is real. If emergent: Copenhagen collapses to piton (theater), and many-worlds/pilot-wave perspectives gain structural legitimacy. This resolution determines whether the constraint is foundational or institutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_primitiveness_vs_emergence, empirical, 'Whether measurement collapse is primitive or emergent from universal dynamics').

omega_variable(
    observer_independence_definition,
    'What exactly does ''observer-independent'' mean? Does Copenhagen''s claim that observers are non-eliminable apply to conscious observers specifically, or any measuring apparatus, or any degree of freedom that decoheres the system?',
    'Conceptual clarification via analysis of Copenhagen texts (Heisenberg, Born, Bohr) vs contemporary interpretations; empirical: tests of whether apparent collapse occurs for apparatus absent conscious agents (automated measurements, pre-recorded outcomes)',
    'If observer = conscious: Copenhagen is metaphysically extreme and empirically falsified. If observer = apparatus: Copenhagen collapses to operationalism (consistent with tangled rope). If observer = decoherent degree of freedom: Copenhagen becomes indistinguishable from many-worlds (reading boundary dissolves).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_independence_definition, conceptual, 'Whether ''observer'' means conscious agent, measuring apparatus, or decohering degree of freedom').

omega_variable(
    determinism_abandonment_scope,
    'Does abandoning determinism at measurement events apply universally, or only at certain scales or under certain experimental conditions? Is there a regime where the Schrödinger equation predicts collapse without additional mechanism?',
    'Empirical: automated measurements with no classical apparatus feedback (quantum eraser experiments, entanglement swapping with delayed choice); tests of whether collapse timing correlates with conscious observation vs apparatus interaction timing.',
    'If determinism abandoned universally: Copenhagen is a radical metaphysical claim with specific empirical content. If determinism failure only when observers present: Copenhagen is anthropocentric (reclassifies as snare from realist perspective). If collapse-free regimes exist: Copenhagen is context-dependent coordinate choice, not ontological truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinism_abandonment_scope, empirical, 'Scope of determinism abandonment and whether universal wavefunction dynamics can reproduce all phenomena').

omega_variable(
    kernel_reading_distinctness,
    'Does the Copenhagen reading instantiate a genuinely distinct commitment structure, or is it operationally equivalent to many-worlds or pilot-wave readings despite different rhetoric?',
    'Comparative analysis of predictions, equivalence proofs, and empirical distinguishability. Examination of whether reading choice affects which phenomena are treated as fundamental vs derived.',
    'If genuinely distinct: Copenhagen represents a real alternative with identifiable structural commitments. If operationally equivalent: Copenhagen is piton — rhetorical theater without structural difference. This determines whether the constraint is a substantive claim or institutional narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Whether Copenhagen reading is structurally distinct from alternative interpretations or operationally equivalent').

omega_variable(
    false_summit_natural_law_vs_institutional,
    'Is the appearance of measurement as inevitable and fundamental a genuine feature of physical reality, or a naturalization of institutional choices that stabilize the Copenhagen reading''s power (pedagogical authority, operational success, measurement apparatus privilege)?',
    'Historical: examination of whether determinism abandonment was scientifically necessary or interpretive choice made by specific figures. Comparative: whether non-Copenhagen frameworks explain quantum phenomena with equal empirical success while preserving determinism and observer independence. Network: whether Copenhagen''s institutional dominance is best explained by its superior explanatory power or by path-dependency and social factors.',
    'If natural law: Mountain classification holds; Copenhagen boundary is fundamental. If institutional: False summit detected; constraint reclassifies to tangled_rope (coordination + beneficiaries) or piton (theater), revealing Copenhagen as a reading that benefits pedagogical establishments and operationalist epistemology at the cost of deterministic ontology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_institutional, conceptual, 'Whether measurement boundary is natural law or institutionalized reading choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copen_tr_t0, quantum_formalism__copenhagen_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(copen_tr_t20, quantum_formalism__copenhagen_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(copen_tr_t40, quantum_formalism__copenhagen_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(copen_be_t0, quantum_formalism__copenhagen_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(copen_be_t20, quantum_formalism__copenhagen_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(copen_be_t40, quantum_formalism__copenhagen_reading, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, information_standard).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_measurement_problem).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, interpretive_foundations_epistemology).

% DUAL FORMULATION NOTE:
% The quantum formalism kernel decomposes into multiple readings with distinct structural properties. The Copenhagen reading (ε=0.58, tangled rope) benefits pedagogical institutions and operationalism while imposing costs on deterministic ontology. The many-worlds reading (separate story, expected ε < 0.35, rope or scaffold) preserves determinism at cost of branching ontology, benefits realism tradition. The pilot-wave reading (separate story, expected ε < 0.40, tangled rope or rope) preserves determinism via hidden variables, has higher theater. Each reading gets its own constraint story with distinct beneficiaries/victims. The three stories are linked via network effects: Copenhagen dominance constrains research resources available to alternative readings (affects_constraints edges). Empirical developments (Bell test precision, quantum erasure experiments) affect all three stories' ε and suppression values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__copenhagen_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
