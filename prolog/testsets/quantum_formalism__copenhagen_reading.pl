% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Wavefunction Collapse as Physical Process (Copenhagen Reading)
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The Copenhagen interpretation of quantum mechanics treats wavefunction
 *   collapse as a physically real process triggered by measurement, marking
 *   an absolute epistemic boundary between the prepared quantum system and
 *   the measured outcome. Measurement outcomes are irreducibly indeterminate
 *   until collapse occurs; determinism is abandoned at measurement events.
 *   This reading is a KERNEL READING instantiating one contested
 *   interpretation of the quantum formalism. Alternative readings
 *   (many-worlds, pilot-wave, superdeterministic) restore determinism or
 *   hidden variables by reinterpreting what measurement is and what the
 *   wavefunction represents. The Copenhagen reading has institutional
 *   dominance in pedagogy and operational practice, but it faces sustained
 *   challenge from alternative interpretations backed by serious physicists
 *   and philosophers. The claim (mountain: emerges naturally) and the
 *   authored metrics (extractiveness 0.68, suppression 0.72, theater 0.44)
 *   are DELIBERATELY INDEPENDENT. The story asserts this is a natural feature
 *   of quantum mechanics; the metrics describe how the reading functions as
 *   an institutional/interpretive constraint that suppresses alternatives and
 *   extracts explanatory authority. The engine computes whether a false
 *   summit (natural law that benefits identifiable parties) is in operation.
 *
 * KEY AGENTS:
 *   - copenhagen_interpretive_community: organized advocates who benefit from the reading as institutional orthodoxy
 *   - deterministic_interpretation_advocates: physicists and philosophers bearing the cost of defending alternatives
 *   - measurement_problem_researchers: structurally excluded by the reading's treatment of measurement as primitive
 *   - realist_physicists: identity-locked out by the reading's abandonment of observer-independent description
 *   - pedagogy_curriculum_setters: institutional agenda-setters encoding Copenhagen as the default frame
 *   - quantum_philosophy_scholars: analytical observers tracking the interpretive landscape
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.68).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.72).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, mountain).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Wavefunction Collapse as Physical Process (Copenhagen Reading)").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '7ecdcde5-8259-43da-bb43-d25e760ca12a').
narrative_ontology:cs_kernel_codification('7ecdcde5-8259-43da-bb43-d25e760ca12a', formalized).
narrative_ontology:cs_authority_grounding('7ecdcde5-8259-43da-bb43-d25e760ca12a', extraction).
narrative_ontology:cs_interpretation_layer_present('7ecdcde5-8259-43da-bb43-d25e760ca12a').
narrative_ontology:cs_reading_relation('7ecdcde5-8259-43da-bb43-d25e760ca12a', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ecdcde5-8259-43da-bb43-d25e760ca12a', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('7ecdcde5-8259-43da-bb43-d25e760ca12a', foundational, measurement_is_primitive_ontological).
narrative_ontology:cs_axiom_status(measurement_is_primitive_ontological, holdable).
narrative_ontology:cs_axiom_grounding('7ecdcde5-8259-43da-bb43-d25e760ca12a', measurement_is_primitive_ontological, conventional).
narrative_ontology:cs_axiom('7ecdcde5-8259-43da-bb43-d25e760ca12a', foundational, indeterminism_irreducible_at_measurement).
narrative_ontology:cs_axiom_status(indeterminism_irreducible_at_measurement, holdable).
narrative_ontology:cs_axiom_grounding('7ecdcde5-8259-43da-bb43-d25e760ca12a', indeterminism_irreducible_at_measurement, empirically_contingent).
narrative_ontology:cs_axiom('7ecdcde5-8259-43da-bb43-d25e760ca12a', secondary, observer_role_non_eliminable).
narrative_ontology:cs_axiom_status(observer_role_non_eliminable, holdable).
narrative_ontology:cs_axiom_grounding('7ecdcde5-8259-43da-bb43-d25e760ca12a', observer_role_non_eliminable, deontological).
narrative_ontology:cs_reference_frame('7ecdcde5-8259-43da-bb43-d25e760ca12a', quantum_mechanics_epistemological_framework_early_twentieth_century).
narrative_ontology:cs_drift_state('7ecdcde5-8259-43da-bb43-d25e760ca12a', contemporary_quantum_information_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7ecdcde5-8259-43da-bb43-d25e760ca12a', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_interpretive_community).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, operational_quantum_mechanics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, quantum_computing_engineers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, deterministic_interpretation_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Physicists and philosophers who adopt Copenhagen as their working interpretation. They benefit from institutional standing as holders of the 'standard' view, from reduced need to justify their interpretive choice (it is the default), and from having measurement and indeterminism encoded as primitive unquestionable features. Their membership in the 'orthodox' camp confers legitimacy in mainstream publications and conferences. Exit is constrained because switching interpretations requires relearning conceptual frameworks and accepting minority status in the field.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_interpretive_community, beneficiary,
    organized, generational, constrained, global).

% The institutional practice of using quantum formalism as a calculational tool without commitment to any interpretation. Benefits from Copenhagen's provision of a lingua franca: 'measurement happens, outcomes are indeterminate, use the formalism'—a position that allows practitioners to work without settling deeper questions. Could in principle switch interpretive frames (the formalism is interpretation-neutral), but the cost of adopting a new default frame is high, so exit is constrained by institutional inertia despite theoretical mobility.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, operational_quantum_mechanics, beneficiary,
    institutional, generational, mobile, global).

% Physicists and philosophers defending Bohmian mechanics, pilot-wave theory, superdeterministic models, or other deterministic frameworks. They bear the professional cost of working on 'alternative' interpretations: their papers are published in specialized venues, their funding proposals face skepticism ('why spend resources on non-standard approaches?'), and they are introduced to students as offering 'interpretations' rather than 'real physics.' Exit is constrained because they cannot abandon quantum mechanics (the empirical foundation is solid) but working within it means accepting minority status.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, deterministic_interpretation_advocates, payer,
    powerful, generational, constrained, global).

% Researchers whose central interest is explaining the measurement problem—understanding how and why measurement produces definite outcomes from quantum superposition. Copenhagen's treatment of measurement as primitive excludes this question from the framework: measurement does not require explanation, it IS the explanatory primitive. To research the measurement problem, one must explicitly adopt a non-Copenhagen interpretation (where measurement is a process that can be explained) or move to philosophy of physics (where the question is treated as conceptual rather than empirical). Their exclusion is structural, not accidental.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, measurement_problem_researchers, excluded,
    moderate, biographical, constrained, global).

% Practitioners designing quantum computers and quantum algorithms. Copenhagen provides the interpretive gloss for quantum advantage: superposition and entanglement exist in an indeterminate state until measurement collapses them to a definite outcome. This reading justifies why quantum computers can explore many computational paths simultaneously (superposition) and then harvest the answer via measurement. The reading does not explain how this works mechanically, but it provides permission to use the formalism without deeper justification. Exit is mobile because quantum engineering ultimately rests on empirical properties of quantum systems, not interpretive commitment, but the default framing is Copenhagen.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_computing_engineers, beneficiary,
    powerful, biographical, mobile, global).

% Physicists committed to scientific realism—the view that physics describes how the world actually is, independent of human observation or measurement. Copenhagen's core move (treating measurement as primitive and abandoning observer-independent description) is incompatible with realism: if the wavefunction does not describe reality, only our knowledge of it, then physics does not tell us how the world is. Realists cannot adopt Copenhagen without abandoning realism. Their exit is identity-locked because realism is not a negotiable stance for them; it is fundamental to their philosophical commitment to science.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, realist_physicists, excluded,
    moderate, generational, identity_locked, global).

% Textbook authors, university curriculum designers, and physics education leaders who decide what interpretation to teach first as 'the standard view.' By choosing Copenhagen as the default, they shape the conceptual frameworks of students entering the field, establish Copenhagen as the reference point against which alternatives are measured, and make Copenhagen the lingua franca of the discipline. Their power is substantial: they set what 'normal' physics looks like. Exit is constrained because changing the curriculum across many institutions requires coordination and consensus.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, pedagogy_curriculum_setters, agenda_setter,
    institutional, generational, constrained, global).

% Philosophers and historians of physics analyzing the foundations of quantum mechanics, the structure of interpretive debates, and how different readings shape the landscape of available questions. They take no stake in defending Copenhagen or any alternative interpretation; they observe the constraint's operation: who benefits, who pays, what gets excluded, how institutional authority shapes the interpretive landscape. Their position is analytical—they can exit the constraint by leaving the field without loss of core identity.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_philosophy_scholars, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified interpretive framework for physicists with different metaphysical commitments to use the same quantum formalism and communicate using shared concepts. Solves the coordination problem of allowing measurement-related indeterminacy to be treated as an unquestionable feature rather than a problem requiring explanation, thus enabling practitioners to advance quantum mechanics without resolving foundational debates.
% TRANSFER_FUNCTION: Transfers interpretive authority from physicists and philosophers to the formalism itself: by treating measurement as primitive and indeterminism as irreducible, the reading transfers the burden of justification onto those who wish to restore determinism or ask what measurement 'really is.' This movement of authority enables operational use of the formalism without deeper commitment, but it extracts interpretive costs from those who want to understand measurement itself.
% ABSENT_VOICES: Measurement-problem researchers are structurally excluded: their central research question (how does measurement produce outcomes?) is treated as pseudo-problem under Copenhagen. Realist physicists are identity-locked out: they cannot adopt Copenhagen without abandoning their core commitment that physics describes reality. Deterministic-interpretation advocates are marginalized as 'alternative' rather than legitimate competitors. These parties would argue that measurement requires explanation, that physics should describe the world as it is independent of observation, and that determinism can be restored via hidden variables or branches—but these arguments are excluded from the default interpretive framework.
% DISAPPEARANCE_RATIONALE: Defenders of Copenhagen argue that if the reading disappeared, quantum mechanics would lose its conceptual anchoring—physicists would need to adopt an alternative interpretation (many-worlds, Bohmian, etc.), fundamentally changing what 'quantum mechanics' means. Critics argue that quantum mechanics (the predictive apparatus) would persist unchanged; only the interpretive gloss would shift, and the persistence of the formalism would show Copenhagen was interpretive choice, not necessary feature. The verdict hinges on whether the formalism's success depends on Copenhagen's specific interpretive claims, or whether it is interpretation-neutral.
% FOUNDING_PROBLEM: Early quantum theory (1920s-1930s) faced an epistemological crisis: the mathematical formalism was empirically successful but its structure (superposition, entanglement, non-commutativity, the role of the observer) defied classical intuition and raised foundational questions about the nature of reality and observation. The Copenhagen interpretation resolved this impasse by enshrining measurement as a primitive ontological category and accepting indeterminism as irreducible. This move allowed physicists to proceed with research and practical applications without resolving deeper questions about what quantum mechanics 'really' describes.
% FOUNDING_PROBLEM_CORROBORATION: Bohr, Heisenberg, and the Vienna Circle attested that the founding problem was solved by accepting measurement as primitive and abandoning the demand for a description of unobserved reality. Contemporary critics—including David Bohm (pilot-wave theory), Hugh Everett (many-worlds), and current leaders in quantum information and quantum foundations—attest that the founding problem persists: the measurement problem (explaining how and why measurement produces definite outcomes) remains unresolved, and Copenhagen merely names the difficulty rather than resolving it. Historical analysis (Paul Teller, David Albert, and others) documents that Copenhagen's ascendance was driven by pedagogical convenience, authority assertion by its founding figures, and Cold War institutional structures, not by empirical evidence favoring it over alternatives. Independent philosophical analysis of the measurement problem shows it remains live: decoherence-based approaches, many-worlds interpretations, and Bohmian mechanics all claim to address what Copenhagen defers.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, ExtMetricName, E),
    domain_priors:suppression_score(quantum_formalism__copenhagen_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTION (0.68): The Copenhagen reading extracts interpretive authority by treating measurement as primitive and indeterminism as irreducible features of nature rather than features of our knowledge. This move closes off the measurement problem (explaining why measurement produces definite outcomes) and reframes it as a pseudo-problem. Physicists who want to use the formalism without adopting Copenhagen must shoulder the burden of defending an 'alternative interpretation,' linguistically marking them as non-standard. The reading thus extracts the right to define what counts as a proper question in quantum foundations. SUPPRESSION (0.72): The reading suppresses alternative interpretations through institutional mechanisms: pedagogical default-setting (Copenhagen taught first as 'the interpretation'), gatekeeping in publication (alternatives labeled 'interpretation' rather than science), and resource allocation (funding flows to standard approaches). For realist or deterministic interpreters, alternatives are available but constrained—they require professional costs (publishing in specialized venues, defending against skepticism from the Copenhagen-trained mainstream). THEATER (0.44): The reading includes substantial functional content (the formalism works, predictions are accurate), but a growing share of activity defends the interpretive move rather than advancing physics. Debates about 'what measurement really is' and 'whether outcomes are really indeterminate' occupy considerable effort in philosophy of physics but do not change empirical predictions. The theater ratio reflects this mixed character: real coordination function, rising share of defense activity. ACCESSIBILITY_COLLAPSE (0.89): Once the reading's core claim is understood (measurement is primitive, indeterminism is real, wavefunction is predictive tool), alternative interpretations seem available in principle but are presented as requiring additional ontological machinery or sacrificing clarity. The reading's framing makes alternatives appear less natural, raising their apparent cost. RESISTANCE (0.58): The reading meets substantial resistance from realists, determinists, and measurement-problem researchers, but this resistance is professionally marginalized rather than numerically overwhelming. The 0.58 reflects that alternatives have serious defenders (Bohmian mechanics, many-worlds, superdeterminism) but lack the institutional standing of Copenhagen.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setters (pedagogy_curriculum_setters, copenhagen_interpretive_community) and beneficiaries experience this reading as natural and necessary—the only way to make sense of quantum indeterminacy. From their seat, the reading emerges as a physical fact: measurement does produce indeterminism, and denying this is denying empirical reality. The payers (deterministic_interpretation_advocates, measurement_problem_researchers) and excluded (realist_physicists) experience it as an interpretive CHOICE that suppresses their questions and constrains their professional options. They see the reading as beneficial primarily to those who benefit from not having to answer the measurement problem. The engine computes this divergence: the same constraint structure generates different per-seat classifications. From the beneficiary seat, the reading should compute as mountain (natural boundary). From the payer and excluded seats, it computes as tangled_rope or snare (choice backed by institutional enforcement, benefiting the interpreters). This divergence is exactly the point: a false-summit reading that appears natural to its beneficiaries but operates as extraction from the perspective of those excluded.
 *
 * DIRECTIONALITY LOGIC:
 *   BENEFICIARIES: copenhagen_interpretive_community (d ≈ 0.1–0.2, near beneficiary end—they collect institutional standing and avoid the burden of defending a non-standard position) and operational_quantum_mechanics (d ≈ 0.15–0.25, near beneficiary end—the institutional practice benefits from having a default interpretive gloss). PAYERS: deterministic_interpretation_advocates (d ≈ 0.75–0.85, near target end—they bear the professional cost of defending alternatives, constrained exit, organized power so they can articulate their position but not change the default), measurement_problem_researchers (d ≈ 0.7–0.8, near target end—their research focus is explicitly excluded by treating measurement as primitive). EXCLUDED: realist_physicists (d ≈ 0.8–0.9, near target end—identity-locked by realist commitment, forced to either abandon realism or adopt a non-standard interpretation). The directionality derivation follows from beneficiary/victim status + exit options: beneficiaries have mobile or arbitrage exit (can work in any framework that uses the formalism), payers have constrained exit (must work within quantum mechanics but face professional costs for non-standard readings), excluded have identity-locked exit (cannot embrace Copenhagen without betraying realist commitments). No overrides are needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early quantum theory's epistemological crisis) is contested in status: defenders claim it is solved by accepting measurement as primitive; critics claim it is merely renamed. The constraint's persistence is vulnerable to mandatrophy diagnosis: if the founding problem is dead (the crisis has been resolved by other means—decoherence-based accounts, quantum information theory, or alternative interpretations that explain measurement), then Copenhagen's persistence despite its costs suggests the mandate has outlived its function. The theater_ratio trajectory (rising slowly from 0.38 to 0.44) supports this reading: the formalism works, but increasingly the work of defending Copenhagen against alternatives is defense rather than productive physics. The measurement problem (which Copenhagen treats as solved by fiat) remains contested in foundational physics, suggesting the founding problem is not conclusively closed. The mandatrophy analysis hinges on whether the measurement problem is 'really' solved by treating measurement as primitive (Copenhagen's claim) or merely deferred (critics' claim). This is routed to the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_choice,
    'Is the Copenhagen reading a discovered feature of nature—an unavoidable conclusion about how quantum systems behave—or an institutionally-enforced interpretive choice among available frameworks?',
    'Meta-analysis of epistemic status across alternative interpretations: do all empirically equivalent interpretations satisfy the same empirical evidence equally well, with differences only in what they claim ''really'' is happening? If so, the reading is a choice. Do experiments or evidence systematically favor Copenhagen over alternatives? If so, the reading approximates a natural boundary.',
    'If the reading is a natural law, it should compute as mountain (emerges_naturally: true is correct). If it is an institutional choice, FSM triggers: the declared beneficiaries (copenhagen_interpretive_community) combined with this metric profile suggest a false summit—a constructed constraint that benefits identifiable parties by presenting itself as natural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_choice, empirical, 'Whether Copenhagen is discovered or chosen').

omega_variable(
    measurement_problem_resolution,
    'Does treating measurement as a primitive ontological category SOLVE the measurement problem (explaining why measurement produces definite outcomes from quantum superposition), or does it merely DEFER the problem by refusing to explain measurement itself?',
    'Detailed analysis of what ''collapse'' is claimed to be: is it a physical process (requiring explanation), or a formal rule (needing no explanation)? Comparison with alternative interpretations (many-worlds, Bohmian): do they explain measurement in a way Copenhagen does not, or do they relocate the difficulty?',
    'If Copenhagen solves the measurement problem, the founding problem is alive and the constraint''s mandate is live. If Copenhagen defers the problem, the founding_problem_status should be ''dead'' or ''zombie''—the founding problem is declared solved by fiat, not by actual explanation, suggesting mandatrophy. This affects whether the theater_ratio trajectory should be interpreted as rising defense activity (deferred problem) or stable productive physics (solved problem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_problem_resolution, conceptual, 'Whether collapse solves or defers the measurement problem').

omega_variable(
    interpretation_empirical_equivalence,
    'Are Copenhagen, many-worlds, and pilot-wave interpretations EMPIRICALLY EQUIVALENT—do they make identical predictions for all possible experiments—or do experiments favor one interpretation over the others?',
    'Systematic review of proposed experiments that could differentiate interpretations (collapse models with empirical signatures, quantum-gravity experiments, high-precision tests of quantum superposition). If no such experiments exist or all proposed ones have null results, empirical equivalence holds. If experiments show systematic deviations supporting one interpretation, equivalence fails.',
    'Empirical equivalence would establish the interpretive choice as conventional (chosen for pedagogical or institutional convenience rather than empirical grounds), supporting the false-summit reading. Empirical differentiation would establish one interpretation as more justified by evidence, potentially supporting Copenhagen as an approximate natural law (if evidence favors it) or revealing it as false if evidence favors an alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_empirical_equivalence, empirical, 'Whether the interpretation landscape is empirically differentiated or equivalent').

omega_variable(
    institutional_dominance_mechanism,
    'Is Copenhagen''s institutional dominance in pedagogy and research culture driven by its superior explanatory or empirical power, or by historical authority, pedagogical convenience, and active institutional gatekeeping?',
    'Historical analysis of how Copenhagen became canonical (role of Bohr/Heisenberg authority, Cold War institutional structures, textbook adoption patterns). Comparison of pedagogical clarity: is Copenhagen easier to teach than alternatives, or is it taught first because it is taught first? Measurement of resource allocation: do funding bodies, journal editors, and hiring committees systematically favor Copenhagen, and if so, is this because of empirical or institutional factors?',
    'If dominance is driven by empirical power or superior explanation, the institutional enforcement (suppression = 0.72) is justified protective gatekeeping. If dominance is driven by authority and convenience, the suppression is extractive, supporting a snare or false-summit reading. This directly affects whether the constraint should compute as mountain (if dominance reflects empirical reality) or as tangled_rope/snare (if dominance is institutional enforcement of a choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_dominance_mechanism, empirical, 'Whether Copenhagen''s dominance reflects empirical superiority or institutional power').

omega_variable(
    realism_constraint,
    'Does abandoning observer-independent description (as Copenhagen does) constitute an acceptable price for solving the epistemological crisis of early quantum mechanics, or does it undermine the scientific realist commitment that grounds the legitimacy of physics as a description of the world?',
    'Philosophical analysis of whether Copenhagen''s anti-realism is a feature (accepting that physics describes only observations) or a bug (accepting that physics does not describe reality). Examination of whether realism can be preserved under an alternative interpretation (Bohmian mechanics explicitly preserves realism via hidden variables; many-worlds preserves realism via universal determinism). Empirical survey: do working physicists view anti-realism as a necessary price or as a regrettable cost they would abandon if alternatives worked as well?',
    'If realism is foundational to physics''s legitimacy and Copenhagen abandons it, realist physicists are identity-locked OUT of Copenhagen—they must adopt an alternative or abandon realism. This establishes realism as a contested value, not a shared commitment. The identity-lock (exit_options: identity_locked for realist_physicists) is correct if realism is non-negotiable for them. The suppression (0.72) is justified enforcement of a legitimate interpretive stance if realism is optional; it is unjustified exclusion if realism is a core commitment of physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(realism_constraint, preference, 'Whether realism is a core or optional commitment in quantum foundations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__copenhagen_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t5, quantum_formalism__copenhagen_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement_basis(quan_tr_t5, observed).
narrative_ontology:measurement(quan_tr_t10, quantum_formalism__copenhagen_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(quan_tr_t10, observed).
narrative_ontology:measurement(quan_tr_t15, quantum_formalism__copenhagen_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(quan_tr_t15, observed).
narrative_ontology:measurement(quan_tr_t20, quantum_formalism__copenhagen_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement_basis(quan_tr_t20, observed).
narrative_ontology:measurement(quan_tr_t25, quantum_formalism__copenhagen_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(quan_tr_t25, observed).
narrative_ontology:measurement(quan_tr_t30, quantum_formalism__copenhagen_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(quan_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__copenhagen_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t5, quantum_formalism__copenhagen_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(quan_be_t5, observed).
narrative_ontology:measurement(quan_be_t10, quantum_formalism__copenhagen_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(quan_be_t10, observed).
narrative_ontology:measurement(quan_be_t15, quantum_formalism__copenhagen_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(quan_be_t15, observed).
narrative_ontology:measurement(quan_be_t20, quantum_formalism__copenhagen_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(quan_be_t20, observed).
narrative_ontology:measurement(quan_be_t25, quantum_formalism__copenhagen_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(quan_be_t25, observed).
narrative_ontology:measurement(quan_be_t30, quantum_formalism__copenhagen_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(quan_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__copenhagen_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(quan_su_t0, observed).
narrative_ontology:measurement(quan_su_t5, quantum_formalism__copenhagen_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement_basis(quan_su_t5, observed).
narrative_ontology:measurement(quan_su_t10, quantum_formalism__copenhagen_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(quan_su_t10, observed).
narrative_ontology:measurement(quan_su_t15, quantum_formalism__copenhagen_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(quan_su_t15, observed).
narrative_ontology:measurement(quan_su_t20, quantum_formalism__copenhagen_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(quan_su_t20, observed).
narrative_ontology:measurement(quan_su_t25, quantum_formalism__copenhagen_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(quan_su_t25, observed).
narrative_ontology:measurement(quan_su_t30, quantum_formalism__copenhagen_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(quan_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, information_standard).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, measurement_problem).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_indeterminacy_boundary).

% DUAL FORMULATION NOTE:
% The quantum_formalism kernel is instantiated by three distinct constraint stories: copenhagen_reading, many_worlds_reading, pilot_wave_reading. Each reading has its own ε, its own beneficiary/victim structure, its own claimed type. Copenhagen claims mountain (natural boundary); many-worlds claims mountain (universal deterministic evolution); pilot-wave claims mountain (deterministic hidden variables). The stories are linked because they are competing interpretations of the same physics formalism—changing from one reading to another would alter the structure of the constraint landscape even though the empirical predictions remain identical. Measurement_problem is downstream of all three readings (each addresses the measurement problem differently); quantum_indeterminacy_boundary is affected by all three (each redefines where indeterminacy lies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__copenhagen_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
