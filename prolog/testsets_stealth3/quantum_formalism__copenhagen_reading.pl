% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Reading: Measurement as Primitive Ontological Cut
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The Copenhagen reading of the quantum formalism — collapse as a physical
 *   process marking an absolute epistemic boundary, measurement as a
 *   primitive ontological category, the observer non-eliminable, determinism
 *   abandoned at measurement events — operated for most of a century not
 *   merely as an interpretation but as an enforceable settlement governing
 *   what questions physicists could legitimately ask. Consolidated at Como
 *   and Solvay (1927), it gave a fractured post-classical discipline a single
 *   operational recipe; from the 1940s through the 1970s it was actively
 *   enforced through journal gatekeeping, hiring, funding exclusion, and the
 *   wielding of von Neumann's flawed no-hidden-variables theorem; Bell's
 *   theorem (1964), Aspect's experiments (1982), and the loophole-free tests
 *   (2015) progressively dissolved both the settlement's prohibitions and the
 *   need to enforce them; the 2022 Nobel for entanglement experiments
 *   consecrated the formerly suppressed line. This file instantiates ONLY the
 *   copenhagen_reading per the committer frame: the many-worlds and
 *   pilot-wave readings are separate constraints (separate files, linked via
 *   network.affects_constraints), and no averaging or hedging across readings
 *   occurs here. The claim/metric gap is deliberate: claimed_type is
 *   tangled_rope (genuine coordination function plus asymmetric extraction
 *   plus active enforcement over the arc), while the authored metrics
 *   describe the attenuated end-state — the engine measures the divergence;
 *   the claim is not reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - - copenhagen_orthodoxy_establishment: Agenda-setter (institutional/arbitrage) — owns the textbook canon, journal editorships, and the Solvay-lineage authority; sets what counts as a legitimate question and collects the resulting authority rents
 *   - - instrumentalist_mainstream_physicists: Primary beneficiary (organized/mobile) — receive calculational confidence without metaphysical labor; benefit from the settlement without running its enforcement
 *   - - foundational_researchers: Primary target (moderate/constrained) — pursued what measurement actually is; paid in exile, marginalization, and grant-review friction; exit means abandoning their central question
 *   - - alternative_interpretation_advocates: Excluded party (moderate/trapped) — Bohmians, Everetters, and hidden-variable researchers barred from mainstream venues; could neither enter the conversation nor leave physics without abandoning the question
 *   - - physics_students: Secondary target and incidental beneficiary (powerless/mobile) — taught the settlement as settled fact; discouraged questions carry grade and social costs; exit is leaving physics entirely
 *   - - experimental_quantum_foundations_groups: Late beneficiary, early payer (organized/constrained) — Bell-test experimentalists absorbed decades of credibility costs before collecting recognition, funding, and the 2022 Nobel as the settlement cracked
 *   - - philosophy_of_physics_community: Analytical observer (moderate/analytical) — documents the settlement's sociology from departmental positions insulated from physics gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.28).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.18).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Reading: Measurement as Primitive Ontological Cut").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '0f95a228-77b3-4d6e-a470-0bf4e2cb8704').
narrative_ontology:cs_kernel_codification('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', formalized).
narrative_ontology:cs_authority_grounding('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', lineage).
narrative_ontology:cs_interpretation_layer_present('0f95a228-77b3-4d6e-a470-0bf4e2cb8704').
narrative_ontology:cs_reading_relation('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_reading_relation('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', foundational, collapse_is_physical_irreducible_process).
narrative_ontology:cs_axiom_status(collapse_is_physical_irreducible_process, holdable).
narrative_ontology:cs_axiom_grounding('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', collapse_is_physical_irreducible_process, empirically_contingent).
narrative_ontology:cs_axiom('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', foundational, measurement_outcomes_irreducibly_indeterminate).
narrative_ontology:cs_axiom_status(measurement_outcomes_irreducibly_indeterminate, holdable).
narrative_ontology:cs_axiom_grounding('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', measurement_outcomes_irreducibly_indeterminate, empirically_contingent).
narrative_ontology:cs_axiom('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', secondary, observer_role_non_eliminable).
narrative_ontology:cs_axiom_status(observer_role_non_eliminable, holdable).
narrative_ontology:cs_axiom_grounding('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', observer_role_non_eliminable, conventional).
narrative_ontology:cs_reference_frame('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', complementarity_operational_settlement).
narrative_ontology:cs_drift_state('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', post_bell_loophole_free_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0f95a228-77b3-4d6e-a470-0bf4e2cb8704', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_orthodoxy_establishment).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, instrumentalist_mainstream_physicists).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, foundational_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, alternative_interpretation_advocates).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, physics_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, physics_students).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, experimental_quantum_foundations_groups).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, experimental_quantum_foundations_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns and transmits the settlement: the textbook chains descended from the founding generation, journal editorships, seminar hierarchies, and the rhetorical authority of the Solvay lineage. Decides which questions appear in curricula, which papers clear review, which job candidates count as serious. Collects authority and gatekeeping rents directly. Because it defines the terms of the arrangement, it loses nothing under any outcome — if the settlement falls, it leads whatever succeeds it.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_orthodoxy_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Apply the formalism daily in condensed matter, optics, atomic physics, and quantum technology. The settlement hands them a finished recipe: compute amplitudes, square them, report statistics — no obligation to say what a wavefunction is. They fund no enforcement and staff no gatekeeping, but their collective practice is what made the settlement's authority credible. Exit is easy: they can change subfields, ignore foundations entirely, or adopt whichever interpretation eventually wins without career consequence.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, instrumentalist_mainstream_physicists, beneficiary,
    organized, biographical, mobile, global).

% Pursue the question the settlement ruled out of order: what happens at measurement, whether the wavefunction is complete, whether definite values exist pre-observation. Historically they paid in exile and marginalization — Bohm left Princeton under political pressure compounded by orthodoxy hostility; Everett's dissertation was shunted out of physics. Today they pay smaller sums: grant-review friction, the 'philosophy' label, extra burden of proof for interpretively loaded results. Leaving the discipline would mean abandoning the question their entire expertise is built around, so exit is available only at the price of professional identity.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, foundational_researchers, payer,
    moderate, generational, constrained, global).

% During the orthodoxy era, Bohmians, Everetters, and hidden-variable researchers were barred from mainstream journals, seminar invitations, and funding lines. They had something substantive to say about the discipline's central question but no seat in the conversation that adjudicated it. They could not enter the discussion the establishment controlled, and they could not leave physics without abandoning the question — a doubly closed position. Partial admission arrived only after Bell's theorem and experimental tests made their subject matter unavoidable.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, alternative_interpretation_advocates, excluded,
    moderate, generational, trapped, global).

% Receive the settlement as settled fact in coursework: collapse is presented as what happens, foundational worries as resolved or childish. They get a usable recipe that lets them pass exams and start research quickly. They pay in inherited closure — the belief that the measurement problem was solved, installed before they possess the tools to evaluate the claim — and in the social cost of asking questions marked as inappropriate. Their exit is leaving physics altogether, which most who feel the cost do.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, physics_students, payer,
    powerless, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, physics_students, beneficiary).

% The Bell-test lineage — Clauser, Aspect, Zeilinger and their students — spent decades absorbing credibility costs for working on questions the settlement deprecated: reviewers asked why anyone would test something settled, funding panels ranked the work as speculative philosophy. After Aspect (1982) and decisively after the loophole-free tests (2015), the position inverted: the same line of work drew major funding, the field of quantum information grew partly on its results, and the 2022 Nobel Prize consecrated it. Their net position across the interval is beneficiary, purchased through a long paying phase.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, experimental_quantum_foundations_groups, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, experimental_quantum_foundations_groups, payer).

% Analyzes the foundations and the settlement's history from philosophy departments structurally insulated from physics gatekeeping. Produces the documentation the discipline's internal record lacks: Jammer's and Beller's histories of the settlement's consolidation, analyses of the von Neumann theorem's flaw, reconstructions of what Bohr's remarks could and could not have licensed. Nothing material flows to or from them under the arrangement; they see the full structure precisely because they hold no seat in it.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophy_of_physics_community, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__copenhagen_reading, copenhagen_orthodoxy_establishment).
narrative_ontology:fixing_cost_class(quantum_formalism__copenhagen_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: a discipline confronting a formalism that appeared to defy classical ontology needed a single shared account of how to apply it to experiments. The settlement supplied one — draw the classical/quantum cut at measurement, treat outcomes as irreducible statistics, proceed — ending the interpretive paralysis that threatened the formalism's uptake and giving a global community one operational language.
% TRANSFER_FUNCTION: Moves epistemic authority and career security from foundational questioners to the orthodox mainstream: publication access, jobs, funding, and the right to define legitimacy flowed toward those who accepted the settlement, away from those who pressed the measurement question. It also transfers metaphysical labor — the burden of saying what measurement IS — into a deferred category ('interpretation'), converting an unsolved physics problem into a private cost borne by whoever insisted on it.
% ABSENT_VOICES: The dissident interpreters — Bohm, Everett, and the hidden-variable tradition — would have objected that the settlement mistook a truce for a solution, and they were kept out of the journals, seminars, and funding streams where the objection could register; their objections were reclassified as philosophy rather than engaged as physics. Students were absent in a second sense: the settlement was transmitted to them as accomplished fact, with no seat at any stage of its formation or review.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the discipline would rearrange: textbooks would lose their organizing chapter structure, curricula would fragment across rival accounts of measurement, the historical allocation of authority and careers would stand exposed as contingent rather than merited, and the interpretive landscape — currently arranged as dissent FROM an orthodoxy — would reorganize as a plurality without a center. Practice-level calculation would continue (the formalism's operational core is shared by all readings), but the pedagogical, institutional, and career architecture built on the settlement would not survive intact.
% FOUNDING_PROBLEM: In the mid-1920s, quantum mechanics produced correct predictions while appearing to demolish classical ontology: wave-particle duality, measurement disturbance, apparent acausality, and no agreed account of what the wavefunction described. The settlement was built to let physics proceed without resolving the ontology — to replace the unanswered question 'what happens at measurement?' with a workable prohibition on asking it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of physics (Jammer's conceptual histories; Beller's 'Quantum Dialogue') document the settlement's consolidation as a sociological achievement distinct from empirical necessity, including the enforcement episodes; John Bell's published critiques attest from within physics that the prohibitions outran the evidence; the Nobel Committee's 2022 citation for Bell-test experiments implicitly attests that the settlement's core prohibition — against taking entanglement and measurement seriously as physical questions — was lifted prematurely. No corroborating source outside the beneficiary set attests that the founding problem remains solved; the sibling readings' continued vitality is itself testimony that the parties dispute the settlement's adequacy.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).
:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.28 at end-state) is attenuated but real: the mid-century peak (0.55, 1970) reflected career destruction of dissenters; today it survives as pedagogical gatekeeping and grant-panel conservatism. Suppression (0.18) is authored as a temporal series because enforcement-capacity change IS the dynamic this story traces — the suppression_requirement series rises to a 1955 peak (0.60: Bohm's exile, loyalty-politics complicity, funding exclusion) and decays as Bell/Aspect/loophole-free results made enforcement of the prohibitions untenable. Theater_ratio rises monotonically (0.12 to 0.55): the settlement began doing real coordinative work, but by century's end few practitioners could state a coherent collapse dynamics, decoherence performs the practical work collapse-talk once pretended to, and 'the' interpretation framing persists as ritual — classic Goodhart drift of deference replacing function. Accessibility_collapse is LOW (0.30): unlike a natural law, the settlement never closed its alternatives — Bohmian mechanics, Everettian quantum mechanics, QBism, and GRW all survived and eventually thrived, which is itself evidence against mountain certification. Resistance is HIGH (0.62): a sustained century of dissent from Bohm through Bell to the modern pluralist mainstream. All three series run on one shared eight-point grid (1927–2022) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical structural data. From the establishment seat, the settlement is a coordination achievement it built and stewarded — the thing that made quantum mechanics teachable and usable. From the foundational-researcher seat, the same structure operated as enforced extraction: a career tax on asking the discipline's deepest question, collected through gatekeeping the establishment controlled. From the student seat, it is inherited closure — a false sense that the measurement problem was solved, transmitted before the student possessed the tools to notice it wasn't. From the observer seat, both descriptions are accurate of the same object at different times. The engine computes this per-seat divergence from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The establishment sits nearest the beneficiary pole (d near 0.05): it wrote the rules, bears none of their costs, and collects authority rents directly — arbitrage-grade exit means it loses nothing under any outcome. Instrumentalist mainstream physicists derive low d from declared beneficiary status, further damped by mobile exit (they can ignore foundations entirely, move subfields, or defect to whichever interpretation wins without career cost). Foundational researchers sit near the full-target end (d near 0.9): declared victims with constrained exit — their expertise and identity are bound to the very question the settlement prohibited, so they could not leave without abandoning their life's work, and staying meant paying. Alternative-interpretation advocates occupy the extreme target position (d near 1.0): trapped exit during the orthodoxy era — excluded from the conversation AND unable to exit the discipline. Physics students carry moderate-high d tempered by mobility (leaving physics is costly but possible). The 2022-vintage experimental groups illustrate temporal role inversion: payers for forty years, beneficiaries after vindication — the static role declaration records their net position with the inversion documented in their situation text.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetric errors. Reading the settlement as pure extraction ignores its genuine coordination achievement: without a shared operational settlement in 1927, quantum mechanics might have fragmented into schools the way early-20th-century foundation disputes fragmented other fields, and the calculational culture that produced the transistor, the laser, and quantum information science plausibly depends on it. Reading it as pure coordination ignores the documented extraction: Bohm's political-era exile from Princeton, Everett's marginalization out of academic physics, four decades in which citing hidden variables carried professional risk, all enforced by machinery the beneficiary camp controlled. On the genealogy interview, the founding problem — how to apply a formalism that seemed to defy classical ontology — is operationally dead: solved so thoroughly that practice no longer needs the settlement's prohibitions. Yet the arrangement persists in textbooks and curricula, and the parties dispute whether the deeper problem (what measurement IS) is live. The status-contested x world-rearranges combination flags capture/zombie dynamics for cross-checking against the computed theater path; the rising theater series past 0.5 and the decayed suppression series jointly indicate end-state drift toward inertial-theatrical persistence, which the engine evaluates from the measurement record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_settlement,
    'Does the reading''s measurement-postulate component track a real feature of nature (genuine physical collapse, irreducible indeterminism), or is the entire edifice a sociological settlement destined to dissolve as a wrong-turn like the luminiferous ether?',
    'Decisive experimental discrimination between objective collapse models and no-collapse models: collapse-model parameter searches, progressively larger macroscopic superposition experiments, gravitationally induced decoherence tests.',
    'If nature-side, the reading''s empirical core hardens toward mountain-like fixity independent of any community; if constructed, the reading continues drifting toward vestigial-theatrical maintenance and eventual abandonment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_settlement, empirical, 'Whether irreducible indeterminism is a fact about nature or an artifact of a 1920s interpretive bargain.').

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the quantum_formalism kernel; its epsilon is authored for the Copenhagen settlement as an operative constraint on practice, not for the formalism itself. Would sibling readings (many-worlds, pilot-wave) author materially different epsilon over their own settlements, and is any observed epsilon being misattributed to the shared formalism?',
    'Cross-reading comparison corpus: author the sibling stories and compare epsilon, beneficiary/victim structure, and enforcement profiles across the family; locate disagreement at the ontological status of measurement (primitive process vs. emergent decoherence vs. effective coarse-graining).',
    'High misattribution risk if epsilon is read as a property of quantum mechanics rather than of this reading''s social operation; correct attribution requires the full family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: epsilon belongs to the reading, not the kernel.').

omega_variable(
    suppression_mechanism_split,
    'How much of the historical suppression of foundational work was structural (funding lines, hiring, journal gatekeeping) versus internalized (training-induced self-censorship, the learned reflex that interpretation questions are ''not physics'')?',
    'Post-liberalization trajectory analysis: surveys and publication/citation pattern studies after 2000, when structural barriers largely fell; persistence of dismissive attitudes despite open venues indicates internalized residue.',
    'If largely internalized, the constraint outlives its enforcement machinery and deepens toward vestigial-theatrical maintenance; if structural, removing gatekeeper seats suffices for dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized components of the settlement''s suppressive force.').

omega_variable(
    empirical_core_vs_interpretive_overlay,
    'Which portion of the reading''s extractive history attaches to its interpretive overlay (collapse-talk, observer-primitivism, question-prohibition) versus its operational core (Born-rule application, complementarity heuristics) that all sibling readings share?',
    'Comparative counterfactual analysis: examine whether practice under sibling readings reproduces the same operational successes without the overlay''s enforcement history; decompose into separate family stories if the layers separate cleanly.',
    'If separable, the operational core is a low-extraction coordination layer serving all readings while the overlay carries the extraction; the family should be split accordingly with distinct epsilon per layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_core_vs_interpretive_overlay, conceptual, 'Decomposability of the settlement''s shared operational function from its reading-specific enforcement.').

omega_variable(
    endstate_function_atrophy,
    'Has the reading''s coordination function fully atrophied into pedagogical ritual, or does it retain live function for newcomers acquiring the discipline?',
    'Longitudinal study of how practicing physicists acquire, use, and discard the settlement across careers; measure whether graduate training outcomes depend on the settlement''s content or only on its existence as a shared entry rite.',
    'Full atrophy supports end-state reclassification away from the hybrid coordination/extraction profile toward inertial-theatrical persistence; retained function supports the hybrid reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endstate_function_atrophy, empirical, 'Whether the settlement still coordinates anything or only performs coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 1927, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qfr_copenhagen_tr_t1927, quantum_formalism__copenhagen_reading, theater_ratio, 1927, 0.12).
narrative_ontology:measurement(qfr_copenhagen_tr_t1940, quantum_formalism__copenhagen_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(qfr_copenhagen_tr_t1955, quantum_formalism__copenhagen_reading, theater_ratio, 1955, 0.3).
narrative_ontology:measurement(qfr_copenhagen_tr_t1970, quantum_formalism__copenhagen_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement(qfr_copenhagen_tr_t1985, quantum_formalism__copenhagen_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(qfr_copenhagen_tr_t2000, quantum_formalism__copenhagen_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement(qfr_copenhagen_tr_t2015, quantum_formalism__copenhagen_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(qfr_copenhagen_tr_t2022, quantum_formalism__copenhagen_reading, theater_ratio, 2022, 0.55).

% Extraction over time
narrative_ontology:measurement(qfr_copenhagen_be_t1927, quantum_formalism__copenhagen_reading, base_extractiveness, 1927, 0.3).
narrative_ontology:measurement(qfr_copenhagen_be_t1940, quantum_formalism__copenhagen_reading, base_extractiveness, 1940, 0.4).
narrative_ontology:measurement(qfr_copenhagen_be_t1955, quantum_formalism__copenhagen_reading, base_extractiveness, 1955, 0.52).
narrative_ontology:measurement(qfr_copenhagen_be_t1970, quantum_formalism__copenhagen_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(qfr_copenhagen_be_t1985, quantum_formalism__copenhagen_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(qfr_copenhagen_be_t2000, quantum_formalism__copenhagen_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(qfr_copenhagen_be_t2015, quantum_formalism__copenhagen_reading, base_extractiveness, 2015, 0.33).
narrative_ontology:measurement(qfr_copenhagen_be_t2022, quantum_formalism__copenhagen_reading, base_extractiveness, 2022, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(qfr_copenhagen_su_t1927, quantum_formalism__copenhagen_reading, suppression_requirement, 1927, 0.25).
narrative_ontology:measurement(qfr_copenhagen_su_t1940, quantum_formalism__copenhagen_reading, suppression_requirement, 1940, 0.42).
narrative_ontology:measurement(qfr_copenhagen_su_t1955, quantum_formalism__copenhagen_reading, suppression_requirement, 1955, 0.6).
narrative_ontology:measurement(qfr_copenhagen_su_t1970, quantum_formalism__copenhagen_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(qfr_copenhagen_su_t1985, quantum_formalism__copenhagen_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(qfr_copenhagen_su_t2000, quantum_formalism__copenhagen_reading, suppression_requirement, 2000, 0.34).
narrative_ontology:measurement(qfr_copenhagen_su_t2015, quantum_formalism__copenhagen_reading, suppression_requirement, 2015, 0.24).
narrative_ontology:measurement(qfr_copenhagen_su_t2022, quantum_formalism__copenhagen_reading, suppression_requirement, 2022, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, identity_coordination).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the interpretation of quantum mechanics' decomposes, per the epsilon-invariance principle, into at least three structurally distinct constraints sharing the quantum_formalism kernel. Each member has its own epsilon, beneficiary/victim structure, and enforcement profile; measuring one with another's observables changes epsilon because it changes the constraint. Copenhagen is the historically upstream member: its enforcement machinery shaped the resource and legitimacy environment into which the sibling readings emerged (Everett's and Bohm's receptions were events IN this constraint's operation), so this story links to both siblings as downstream dependents. The sibling stories should link back and document their own deltas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
