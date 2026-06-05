% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__many_worlds_reading, []).

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
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Reading: Decoherence-Induced Branching and Observer Elimination
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The many-worlds reading of quantum formalism represents a fundamental
 *   interpretive commitment: the universal wavefunction evolves
 *   deterministically under Schrödinger's equation in all cases, measurement
 *   is not a special process but rather decoherence-induced apparent
 *   branching into orthogonal components, and all measurement outcomes are
 *   equally real in separate, non-communicating worlds. This reading
 *   constrains and benefits different epistemic communities in structurally
 *   distinct ways. It preserves global determinism and eliminates the
 *   observer-dependent measurement axiom of Copenhagen, providing formal
 *   elegance and removing the need for collapse mechanisms. Simultaneously,
 *   it imposes an ontologically extravagant structure (infinite branching)
 *   that contradicts the intuitive experience of single outcomes and creates
 *   new conceptual problems (how to define probability, how to recover
 *   observer-centric prediction, how to justify privileging 'our branch').
 *   The reading exhibits significant suppression (0.62) because access to
 *   alternative branches is constitutively impossible; observers are trapped
 *   in their branch and cannot verify claims about co-equal parallel
 *   outcomes. Theater has risen over the measurement interval (0.42 → 0.68)
 *   as textbook treatment has become more ritualistic and less pragmatically
 *   grounded — the reading is maintained as 'philosophically serious' even as
 *   working physicists operate under Copenhagen or ignore interpretation
 *   entirely.
 *
 * KEY AGENTS:
 *   - Empirical Agent (Single-World Observer): Primary victim (powerless/trapped) — direct sensory/epistemic access devalued, cannot verify extravagant ontology, structurally unable to access other branches
 *   - Experimental Physicist: Secondary victim and beneficiary (moderate/constrained) — benefits from determinism and no-collapse formalism, constrained by ontological awkwardness and inability to publish about multiplicity
 *   - Mathematical Formalism Conservation (Institutional): Primary beneficiary (institutional/arbitrage) — Schrödinger equation preserved globally, no new machinery, no modification to computational tools
 *   - Eliminativist Epistemology Community: Organized beneficiary (organized/constrained) — advances observer-elimination agenda, but constrained by legitimacy challenges and need for active defense
 *   - Textbook Exposition Ritual: Institutional actor (institutional/arbitrage) — sustains reading as curricular content through academic ritual despite weakened pragmatic grounding
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false summit: treating interpretive choice (realism + determinism + formalism conservation) as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.38).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.62).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Reading: Decoherence-Induced Branching and Observer Elimination").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '4ca3074e-75c8-4a35-beaf-cad28379dd8a').
narrative_ontology:cs_kernel_codification('4ca3074e-75c8-4a35-beaf-cad28379dd8a', fixed_text).
narrative_ontology:cs_authority_grounding('4ca3074e-75c8-4a35-beaf-cad28379dd8a', expertise).
narrative_ontology:cs_interpretation_layer_present('4ca3074e-75c8-4a35-beaf-cad28379dd8a').
narrative_ontology:cs_reading_relation('4ca3074e-75c8-4a35-beaf-cad28379dd8a', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('4ca3074e-75c8-4a35-beaf-cad28379dd8a', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('4ca3074e-75c8-4a35-beaf-cad28379dd8a', foundational, universal_schrodinger_evolution_deterministic).
narrative_ontology:cs_axiom_status(universal_schrodinger_evolution_deterministic, holdable).
narrative_ontology:cs_axiom_grounding('4ca3074e-75c8-4a35-beaf-cad28379dd8a', universal_schrodinger_evolution_deterministic, empirically_contingent).
narrative_ontology:cs_axiom('4ca3074e-75c8-4a35-beaf-cad28379dd8a', foundational, branching_ontology_all_outcomes_equally_real).
narrative_ontology:cs_axiom_status(branching_ontology_all_outcomes_equally_real, holdable).
narrative_ontology:cs_axiom_grounding('4ca3074e-75c8-4a35-beaf-cad28379dd8a', branching_ontology_all_outcomes_equally_real, deontological).
narrative_ontology:cs_reference_frame('4ca3074e-75c8-4a35-beaf-cad28379dd8a', deterministic_wavefunction_universalism).
narrative_ontology:cs_drift_state('4ca3074e-75c8-4a35-beaf-cad28379dd8a', contemporary_quantum_foundations, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4ca3074e-75c8-4a35-beaf-cad28379dd8a', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, determinism_preserving_ontology).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, mathematical_formalism_conservation).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, eliminativist_epistemology).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, intuitive_single_world_experience).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, parsimony_principle).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, observer_centric_epistemology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPIRICAL AGENT / SINGLE-WORLD OBSERVER (SNARE) — The observer trapped in lived experience of branching-into-one. Constrained to experience one outcome per measurement, but the reading insists this subjective experience is merely apparent — the 'real' ontology is all branches equally realized. Extraction: the agent's direct epistemic access is devalued relative to the mathematical formalism. No exit from the epistemic trap: cannot access other branches, cannot verify the claim that all outcomes are realized, can only assent to a framework that contradicts their observational evidence.
constraint_indexing:constraint_classification(quantum_formalism__many_worlds_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: EXPERIMENTAL PHYSICIST (TANGLED ROPE) — Benefits from the reading's preservation of determinism (enables computational prediction and retroactive consistency) and its elimination of observer-dependent measurement axioms (simplifies apparatus design philosophy). But constrained by the reading's extravagant ontology: experiments produce single outcomes, yet the framework claims all outcomes exist. The physicist must design experiments under the assumption of branching, publish results as if single outcomes matter, secure funding based on claimed discoveries — all while the reading insists their experimental outcome is merely apparent and co-equal with all alternatives. Mixed extraction: genuine coordination benefit (deterministic evolution, no collapse axiom) alongside genuine cost (ontological awkwardness, unpublishable multiplicity).
constraint_indexing:constraint_classification(quantum_formalism__many_worlds_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MATHEMATICAL FORMALISM CONSERVATION / INSTITUTIONAL (ROPE) — The mathematical structure benefits from the reading: Schrödinger evolution is global, deterministic, linear, no collapse axiom required. The formalism is preserved entire. Institutional authority (the mathematical framework) experiences this reading as pure coordination — no modification to the equations, no new axioms, no additional mathematical machinery. Arbitrage exit: can switch to alternative interpretations while keeping all equations and computational tools. Net beneficiary.
constraint_indexing:constraint_classification(quantum_formalism__many_worlds_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ELIMINATIVIST EPISTEMOLOGY COMMUNITY (TANGLED ROPE) — Organized agents (philosophers of science, quantum foundations researchers, some theoretical physicists) experience this reading as both enabling and constraining. Enabling: it eliminates observer-centrism from foundational physics, advancing a long-standing philosophical agenda. Constraining: the reading's ontological extravagance (infinite branching, equal reality of all outcomes) creates legitimacy challenges. The community must defend the reading against intuition-based criticism while maintaining institutional credibility. Active enforcement required: must construct arguments that decoherence is genuine branching, not merely appearance; must establish consensus that unintuitive ontology is acceptable cost for mathematical simplicity.
constraint_indexing:constraint_classification(quantum_formalism__many_worlds_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TEXTBOOK EXPOSITION RITUAL (PITON) — Advanced quantum mechanics textbooks present the many-worlds reading with decreasing conviction over successive editions. The reading persists in the curriculum as a 'serious interpretation worthy of discussion' despite being taught as optional, counterintuitive, and rarely the working interpretation of practitioners. Theater ratio elevated (0.68) by the ritual of presenting the reading as a live option when actual research practice operates under Copenhagen or pragmatist assumptions. The institutional function (teaching interpretations) has atrophied relative to the rhetorical performance (maintaining 'interpretive pluralism'). Piton classification: the reading is sustained by theoretical completeness expectations and academic ritual, not by its empirical or pragmatic superiority.
constraint_indexing:constraint_classification(quantum_formalism__many_worlds_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL NECESSITY VIEW (MOUNTAIN) — From the civilizational, universal perspective: if Schrödinger's equation is universally true and describes all systems including observers, then measurement cannot terminate the evolution (no collapse axiom). Logically, the only consistent interpretation is that all measurement outcomes are equally real, branching into separate worlds. This appears as a logical necessity — the reading is forced by the requirement of mathematical consistency. However, this perspective is a FALSE SUMMIT: the appearance of logical necessity naturalizes a choice between interpretations. Copenhagen and pilot-wave readings also preserve mathematical consistency while rejecting the many-worlds ontology. The 'necessity' is conditioned on accepting specific metaphysical assumptions (realism about the wavefunction, determinism, universe-level unitarity).
constraint_indexing:constraint_classification(quantum_formalism__many_worlds_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quantum_formalism__many_worlds_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_formalism__many_worlds_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, TR),
    TR >= 0.70.

:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading extracts from empirical agents (who must accept unintuitive ontology without direct verification) and from pragmatic physicists (who must defend or explain away multiplicity). But the extraction is tempered by genuine coordination benefits — determinism is preserved, formalism is simplified, observer-dependence is eliminated. Not a pure snare. Suppression (0.62): High. Fundamental suppression mechanisms: (1) Epistemic closure — observers cannot access other branches, cannot verify the claim, cannot exit the framework once adopted; (2) Ontological extravagance — the reading imposes unintuitive structure that creates cognitive friction; (3) Mathematical authority — the mathematical formalism's prestige shields the reading from criticism ('if math demands it, intuition must yield'). Theater ratio (0.68): Elevated and rising. The reading is taught in textbooks as a 'serious interpretation' but invoked rarely in working physics. Textbook exposition has ritualized; each generation inherits the reading as established option without renewed justification. Over the 70-year measurement interval (roughly 1950s Everett to 2020s), theater has risen from ~0.42 (when the reading was novel and actively defended in journals) to ~0.68 (when it is preserved by curricular inertia and the mystique of 'interpretive pluralism'). This rising theater is characteristic of piton dynamics: the reading's functional role in physics has atrophied while its rhetorical role in 'completing the interpretation menu' has become more performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a structural rift between mathematical and experiential authority. The mathematical/institutional perspective (Rope) sees the reading as pure coordination — the formalism is preserved, equations unchanged, elegance achieved. The empirical/observer perspective (Snare) sees extraction — lived reality is devalued relative to mathematical abstraction, direct sensory evidence is subordinated to a formalism that insists on multiplicity they cannot access. The experimental physicist (Tangled Rope) is caught between these: they use the formalism for genuine predictive work (coordination benefit) while maintaining the reading's ontology creates awkwardness that cannot be published or defended to funders (extraction cost). The eliminativist community (Tangled Rope, organized) experiences enforcement strain — must actively defend the reading against intuition-based criticism while building conceptual architecture (branch counting, probability recovery, environmental decoherence) that smuggles observer-centric structure back in through the back door. The analytical observer (Mountain, false summit) risks naturalizing the interpretive choice as logical necessity when it is actually one option among structurally distinct alternatives (Copenhagen, pilot-wave) that preserve mathematical consistency without the ontological extravagance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for this reading is derived from the structural relationship of each agent to the extraction flow. The empirical observer is a full victim (d ≈ 0.95): structurally trapped, no exit options, maximum epistemic devaluation. The experimental physicist is mixed (d ≈ 0.65): benefits from determinism and formalism preservation, constrained by multiplicity problem. Mathematical formalism is a beneficiary (d ≈ 0.05): receives protection and elevation from the reading. The eliminativist community is partially beneficiary (d ≈ 0.35): advances their agenda but constrained by legitimacy deficits. The analytical observer neutrally positioned (d ≈ 0.73): sees the reading from civilizational perspective, risks false-summit error. The suppression metric (0.62) is not scaled by context — it represents the structural barriers to exit (epistemic closure, ontological extravagance, mathematical authority) that bind all agents independent of power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The many-worlds reading resolves the mandatrophy by clarifying WHICH coordination benefit justifies the extraction cost. The coordination benefit is not experimental prediction (all interpretations preserve this) or pragmatic utility (Copenhagen works fine for this). The coordination benefit is mathematical elegance: global determinism, no collapse axiom, universal wavefunction evolution. The extraction cost is the empirical agent's epistemic devaluation: their direct evidence is dismissed as apparent. The question 'Is this a fair trade?' is a preference question, not a factual one. From the mathematical perspective (institutional beneficiary), the answer is yes — formalism preservation is a genuine coordination good worth the ontological cost. From the empirical perspective (powerless victim), the answer is no — ontological extravagance is not worth the epistemic subordination. The constraint exhibits Tangled Rope structure precisely because both answers are defensible: there IS a coordination function (formalism preservation) and there IS asymmetric extraction (epistemic devaluation of observers). The mandatrophy is resolved by accepting that the reading legitimately serves mathematical elegance while legitimately imposing costs on intuitive empiricism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_emergence_vs_appearance,
    'Is measurement-as-decoherence-induced-branching a genuine ontological claim or merely a reformulation that preserves Copenhagen''s predictive content while adopting an extravagant interpretive structure?',
    'Compare empirical predictions of many-worlds interpretation with Copenhagen and pilot-wave under identical experimental setups. Examine whether any experimental test could distinguish branching ontology from apparent branching. If no test distinguishes, the difference is interpretive not empirical.',
    'If genuine ontological claim: extractiveness justified by conceptual cost of unintuitive ontology. If merely reformulation: extractiveness becomes pure institutional performance (elevated theater_ratio, reduced epistemic justification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_emergence_vs_appearance, conceptual, 'Whether many-worlds branching is ontological or merely interpretive reformulation').

omega_variable(
    wavefunction_realism_grounding,
    'What justifies treating the wavefunction as a real, evolving entity rather than as an epistemological bookkeeping device? Does the reading depend on prior commitment to wavefunction-realism or does it justify it?',
    'Historical analysis of conceptual development: did physicists adopt many-worlds because they already believed in wavefunction realism, or did many-worlds convince them of realism? Logical reconstruction: can the reading be maintained without wavefunction realism?',
    'If prior commitment: the reading is downstream of an earlier interpretive choice (not fundamental). If justifying: the reading attempts to provide foundational grounding (claims foundational status). If decoupled: the reading is neutral on realism (weakens both its justification and its constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wavefunction_realism_grounding, conceptual, 'Relationship between wavefunction realism and many-worlds reading').

omega_variable(
    branching_epistemic_access,
    'If all measurement outcomes are equally real in separate branches, what justification exists for the observer to privilege their own experienced branch? Why is prediction of ''outcomes in my branch'' a coherent epistemic goal under many-worlds?',
    'Examine modern formulations (DeWitt environment branch structure, derivative theories of probability, functional-branch-relative predictions). Test whether the reading can maintain empirical adequacy without secretly re-introducing observer-centrism through the backdoor of ''our branch''.',
    'If coherent without observer-smuggling: observer elimination is genuine. If observer-centrism unavoidably returns: the reading is performatively rejecting observer-centrism while practically maintaining it (suppression increases, theater increases).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(branching_epistemic_access, empirical, 'Whether many-worlds epistemic adequacy requires hidden observer-centrism').

omega_variable(
    ontological_extravagance_cost,
    'Is the infinite branching ontology a necessary cost of mathematical formalism conservation, or an arbitrary metaphysical choice that adds complexity without empirical justification?',
    'Reconstruction of alternatives: can the Schrödinger evolution be preserved globally without positing branching (as in pilot-wave interpretations)? Cost-benefit analysis: what empirical or conceptual gain justifies the ontological extravagance?',
    'If necessary cost: extractiveness is legitimized by the need for mathematical preservation. If arbitrary choice: extractiveness increases (the reading imposes costs without gains).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_extravagance_cost, preference, 'Whether ontological extravagance is necessary or arbitrary').

omega_variable(
    kernel_reading_identity_ambiguity,
    'Is the many-worlds reading a genuine alternative interpretation of the quantum formalism kernel, or a restatement of the mathematical formalism that adopts an interpretive layer incompatible with the kernel''s original use-cases?',
    'Historical analysis: how did Everett, DeWitt, and subsequent defenders justify the reading as an ''interpretation'' rather than a reinterpretation? Functional analysis: does the reading enable or disable the original problem-solving role of quantum mechanics in laboratory physics?',
    'If genuine alternative: kernel remains open to multiple readings (coexists_with other interpretations). If restatement with incompatible layer: the reading may foreclose alternatives by naturalizing its interpretive layer as necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_ambiguity, conceptual, 'Status of many-worlds as a kernel reading vs. reinterpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mw_quant_theater_t0, quantum_formalism__many_worlds_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mw_quant_theater_t35, quantum_formalism__many_worlds_reading, theater_ratio, 35, 0.58).
narrative_ontology:measurement(mw_quant_theater_t70, quantum_formalism__many_worlds_reading, theater_ratio, 70, 0.68).

% Extraction over time
narrative_ontology:measurement(mw_quant_extract_t0, quantum_formalism__many_worlds_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mw_quant_extract_t35, quantum_formalism__many_worlds_reading, base_extractiveness, 35, 0.33).
narrative_ontology:measurement(mw_quant_extract_t70, quantum_formalism__many_worlds_reading, base_extractiveness, 70, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, information_standard).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, quantum_formalism__pilot_wave_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, observer_elimination_epistemology).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, wavefunction_realism_foundational_claim).

% DUAL FORMULATION NOTE:
% The many-worlds reading is one of three structurally distinct interpretations of the quantum formalism kernel. Each reading (many-worlds, Copenhagen, pilot-wave) is a separate constraint story with its own ε value, beneficiary/victim structure, and classification profile. They are linked via network.affects_constraints because adoption of one reading constrains the legitimacy and adoption of others. Many-worlds constrains Copenhagen by offering an alternative to collapse; constrains pilot-wave by offering determinism without hidden variables. This is not a single constraint viewed three ways — it is three distinct constraints that share a common mathematical formalism but instantiate different interpretive ontologies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
