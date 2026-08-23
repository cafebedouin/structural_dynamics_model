% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Reading of the Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   Within the interpretive economy of quantum foundations, the many-worlds
 *   reading operates as a standing arrangement: a coordinated research
 *   program (decoherence-based branching, decision-theoretic probability,
 *   Everettian cosmology) organized around the commitment that the universal
 *   wavefunction evolves deterministically and that measurement is nothing
 *   over and above decoherence-induced branching. The arrangement coordinates
 *   genuine work — it gave the quantum-to-classical transition a research
 *   agenda and supplied an ontology for quantum computation and cosmology —
 *   while concentrating interpretive authority and career goods in a compact
 *   leadership group and exporting the framework's central unpaid liability
 *   (the probability problem) to every adopter. This file instantiates ONE
 *   reading of the kernel quantum_formalism; the copenhagen_reading and
 *   pilot_wave_reading are separate constraints with their own epsilon
 *   values, beneficiary structures, and classifications, linked through
 *   network.affects_constraints. Per the epsilon-invariance principle, no
 *   averaging across readings is attempted: this story's metrics describe the
 *   many-worlds arrangement as it operates. KEY AGENTS (by structural
 *   relationship): - senior_everettian_theorists: agenda-setting beneficiary
 *   (institutional / identity_locked) — administers the reading's canon,
 *   collects its largest career goods - decoherence_program_researchers:
 *   secondary beneficiary (organized / mobile) — converts technical results
 *   into interpretive payoff - quantum_information_foundations_community:
 *   incidental beneficiary (organized / arbitrage) — draws rhetorical
 *   capital, indifferent to outcome - science_communicators: amplifier
 *   beneficiary (organized / arbitrage) — mass-audience narration -
 *   early_career_foundations_researchers: primary payer (moderate / immediate
 *   horizon / constrained) — inherits the probability debt as training burden
 *   - rival_interpretation_researchers: payer (organized / generational /
 *   constrained) — bears marginalization and opportunity costs -
 *   philosophy_of_physics_analysts: analytical observer (moderate /
 *   analytical) — external audit seat
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.42).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.3).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Reading of the Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '23d9f678-1714-40e7-8a6d-e176127e8579').
narrative_ontology:cs_kernel_codification('23d9f678-1714-40e7-8a6d-e176127e8579', formalized).
narrative_ontology:cs_authority_grounding('23d9f678-1714-40e7-8a6d-e176127e8579', expertise).
narrative_ontology:cs_interpretation_layer_present('23d9f678-1714-40e7-8a6d-e176127e8579').
narrative_ontology:cs_reading_relation('23d9f678-1714-40e7-8a6d-e176127e8579', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('23d9f678-1714-40e7-8a6d-e176127e8579', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('23d9f678-1714-40e7-8a6d-e176127e8579', foundational, universal_unitary_evolution_no_collapse).
narrative_ontology:cs_axiom_status(universal_unitary_evolution_no_collapse, holdable).
narrative_ontology:cs_axiom_grounding('23d9f678-1714-40e7-8a6d-e176127e8579', universal_unitary_evolution_no_collapse, instrumental).
narrative_ontology:cs_axiom('23d9f678-1714-40e7-8a6d-e176127e8579', foundational, wavefunction_completeness_no_hidden_variables).
narrative_ontology:cs_axiom_status(wavefunction_completeness_no_hidden_variables, holdable).
narrative_ontology:cs_axiom_grounding('23d9f678-1714-40e7-8a6d-e176127e8579', wavefunction_completeness_no_hidden_variables, empirically_contingent).
narrative_ontology:cs_reference_frame('23d9f678-1714-40e7-8a6d-e176127e8579', universal_deterministic_wavefunction).
narrative_ontology:cs_drift_state('23d9f678-1714-40e7-8a6d-e176127e8579', contemporary_post_decoherence_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('23d9f678-1714-40e7-8a6d-e176127e8579', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, senior_everettian_theorists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, decoherence_program_researchers).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_information_foundations_community).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, science_communicators).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, early_career_foundations_researchers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, rival_interpretation_researchers).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, universal_schrodinger_evolution).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, decoherence_branching_emergence).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, wavefunction_realism).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, decision_theoretic_born_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior theorists at the major foundations centers who wrote the reading's canonical defenses (decoherence-based branching, decision-theoretic probability), hold editorial and seminar gatekeeping positions, train graduate students inside the framework, and collect the largest share of the citations, prizes, and invitations the reading generates. Leaving would mean disavowing decades-defining work; their professional identities are constituted by the program.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, senior_everettian_theorists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, senior_everettian_theorists, beneficiary).

% Physicists working on environmental decoherence and the quantum-to-classical transition. The reading supplies the interpretive payoff for their technical results: their calculations become accounts of how worlds branch rather than mere loss of coherence. Their technical skills transfer readily into quantum information science, so departing the interpretive program does not strand them.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, decoherence_program_researchers, beneficiary,
    organized, biographical, mobile, global).

% Researchers in quantum computation and information who draw on a branching-universe ontology for conceptual claims about parallel computation and for public justification of the field. They harvest rhetorical capital from the reading's visibility, but their day-to-day results succeed or fail independently of it.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_information_foundations_community, beneficiary,
    organized, biographical, arbitrage, global).

% Authors, broadcasters, and documentary producers for whom the branching-worlds picture is uniquely narratable. They amplify the reading to audiences orders of magnitude larger than the specialist literature reaches, shaping what the public takes quantum mechanics to say, and they switch narratives freely when a story ages.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, science_communicators, beneficiary,
    organized, immediate, arbitrage, global).

% Graduate students and postdocs who enter foundations through this reading's literature because it is the best-developed current program. They inherit its open problems — above all the probability problem — as their dissertation burden, while hiring committees discount interpretive work as low-yield. Switching subfields mid-training costs years they do not have.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, early_career_foundations_researchers, payer,
    moderate, immediate, constrained, global).

% Researchers advancing Bohmian mechanics, objective collapse models, QBism, and kindred programs. They bear the opportunity cost of a field whose attention and funding concentrate on the branching program, and their work is routinely framed as unnecessary given the reading's claimed sufficiency. A subset has spent entire careers inside a single rival framework and cannot leave without self-disavowal.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, rival_interpretation_researchers, payer,
    organized, generational, constrained, global).

% Philosophers of physics who audit the reading's claims — stress-testing the probability derivations, the ontology of branches, the status of decoherence. They neither collect from nor pay into the arrangement; they produce the external scrutiny the program's self-assessments must survive.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, philosophy_of_physics_analysts, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, senior_everettian_theorists).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Applies quantum theory to closed systems, including observers and the cosmos, without added dynamics: one deterministic law (universal unitary evolution) governs everything; decoherence explains the emergence of stable, effectively classical, non-interfering records; the framework gives quantum computation and quantum cosmology an ontology that needs no external classical domain.
% TRANSFER_FUNCTION: Moves interpretive authority, citations, prizes, and research territory toward the Everettian leadership and its satellite programs; moves the cost of the framework's open probability problem from its architects to each new adopter; and moves public attention toward branching-worlds narratives, which the reading's visibility converts back into institutional capital.
% ABSENT_VOICES: Instrumentalist and pragmatist physicists who regard all interpretive ontology as idle — the silent majority of working physicists — are absent from the foundations conversation their indifference subsidizes; so are QBist and participatory-realist voices outside the main centers. They would object that the contest's stakes are inflated and its authority concentrations undeserved; they are absent because interpretive work carries no career reward in mainstream departments.
% DISAPPEARANCE_RATIONALE: Laboratory physics would proceed identically — the reading issues no novel predictions — but the interpretive ecosystem built on it would rearrange within a decade: the Everettian program's literatures, curricula, and career lattices dissolve; decoherence survives as physics but loses its branching gloss; quantum-computation popularization loses its multiverse register; philosophy of physics reorganizes around the remaining readings. The rearrangement concentrates exactly where the constraint's parties live.
% FOUNDING_PROBLEM: Orthodox quantum mechanics required a collapse postulate and placed the observer outside the quantum description, leaving the theory unable to describe the universe as a whole and ambiguous about what counts as a measurement. Everett's move was to delete collapse and take the resulting universal dynamics literally.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem is real is corroborated far outside the benefiting parties: critics of this reading — philosophers of physics of analytic, Bohmian, and pragmatist allegiance alike — affirm the measurement problem while denying this arrangement solves it, and decoherence's physical content is experimentally corroborated independently of any interpretation. That THIS arrangement resolves the problem is attested almost exclusively by the program's own members; no corroborating source outside the beneficiary set endorses the resolution, and that absence is itself signal.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).
:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42: the transfers are real but academic-scaled — status, citations, and research territory flow to a compact leadership while the framework's principal liability (deriving probability when every outcome occurs) is carried by adopters rather than resolved. Suppression is authored at 0.30 as a raw structural property, unscaled by power or scope: no coercion binds anyone to the reading; what enforcement exists is interpretive gatekeeping — referee standards, seminar canon, the framing of rivals as redundant — plus an internalized component in which early-career researchers treat rival programs as career-risky. Theater ratio 0.20: decoherence physics is genuine and productive, but a growing share of program rhetoric declares the measurement problem solved faster than the probability problem is actually closed. Accessibility collapse 0.45: once the decoherence picture is absorbed, collapse postulates appear ad hoc and observer-centered accounts parochial, so some alternatives lose grip — yet Bohmian, QBist, and collapse-model programs remain fully workable, so collapse is far from complete. Resistance 0.55: the reading attracts persistent technical criticism (probability, branch ontology, the measure problem in cosmology) while meeting little institutional resistance inside its strongholds. All three tracked series share one seven-point grid (t=0..66, roughly decadal steps from Everett 1957); trajectories are monotonic — neglect, then traction, then consolidation — not cyclical. Claim and metrics are independent authored facts: claimed_type tangled_rope reflects the judgment that genuine coordination and asymmetric extraction coexist in one enforced structure; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the leadership seat the arrangement presents as parsimony itself — nothing added, nothing hidden, the formalism simply read literally — so extraction computes near zero and the structure looks rope-like. From the early-career and rival seats the same structure operates as an authority-and-debt system: entry requires accepting an unpaid conceptual liability as the price of admission, and dissent is priced as career risk. The quantum-information seat barely registers the constraint either way (arbitrage-grade exit), which places its derived directionality nearer the beneficiary end despite its paying none of the organizing costs. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership combines agenda-setting with capture (d near 0.05): the reading subsidizes them directly. Decoherence researchers (d≈0.2) and the quantum-information community (d≈0.15) collect payoff without running enforcement. Communicators (d≈0.2) harvest narrative value. Early-career researchers (d≈0.75) and rival-program researchers (d≈0.85) sit near the target end: the former pay in inherited open problems and constrained exit, the latter in marginalization, with an identity-locked subset for whom exit equals self-disavowal. Analysts sit near symmetric (d≈0.5). These mappings follow from the beneficiary/victim declarations plus exit atoms; no directionality overrides were needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — quantum theory without an observer outside it, applicable to the cosmos — remains contested rather than dead: insiders attest it solved, external philosophers of physics attest it was traded for harder problems. Because the founding problem is not dead, the arrangement has not outlived its mandate and no zombie flag is warranted; mandatrophy is unresolved in both directions. The drift risk to watch is rhetorical: if 'the measurement problem is solved' hardens into recitation while the probability problem stays open, theater_ratio climbs and the program's interpretive layer decouples from its technical layer — a piton-shaped failure inside a tangled-rope structure. The temporal series tracks exactly this gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of the kernel quantum_formalism (many_worlds_reading). How would the structural profile change under a sibling reading — copenhagen_reading or pilot_wave_reading — and where exactly is the disagreement located?',
    'Generate the sibling files and compare computed per-seat classifications: the disagreement is located in the fundamental status of measurement (physical collapse versus decoherent branching versus wave-guidance) and in wavefunction completeness; each reading yields its own epsilon, victim set, and enforcement profile.',
    'If a sibling reading displaced this one institutionally, the victim set shifts (Copenhagen''s arrangement burdens users of the measurement terminology; pilot-wave burdens the hidden-variable skeptic), the coordination type changes, and this file''s extraction profile no longer describes the operative arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas and disagreement location.').

omega_variable(
    born_rule_probability_debt,
    'Is probability derivable inside the reading (Deutsch-Wallace decision theory, envariance, self-locating uncertainty), or does the framework carry a permanent unpaid liability — what fixes rational credence when every outcome occurs?',
    'Technical assessment of the derivation programs: whether the axioms smuggle in what they claim to derive, and whether a consensus proof emerges that critics outside the program accept.',
    'If derivable, measured extraction drops toward rope levels and the reading stabilizes; if permanently indebted, extraction stays elevated and the export-to-adopters transfer continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(born_rule_probability_debt, conceptual, 'Status of the probability problem inside the Everettian framework.').

omega_variable(
    naturality_rhetoric_vs_structure,
    'Proponents present the reading as ''just the formalism'' — no added postulates, hence natural. Is that naturality a structural fact of the arrangement, or positioning that concentrates interpretive authority in whoever defines what ''literal'' means?',
    'Compare the reading''s auxiliary apparatus (decision-theoretic axioms, branch-counting measures, self-location postulates) against the bare formalism: if the apparatus keeps growing, the naturality claim functions rhetorically.',
    'If the reading were claimed as a mountain on naturality grounds while these beneficiaries exist, false-summit detection would fire; under the current tangled-rope claim the omega calibrates how much of the arrangement''s authority rests on the naturality rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_rhetoric_vs_structure, conceptual, 'Whether the reading''s naturality claim is structural or strategic.').

omega_variable(
    empirical_underdetermination_horizon,
    'No feasible experiment currently discriminates the readings of the quantum formalism; will one ever exist, or is the interpretive marketplace structurally permanent?',
    'Watch for proposals connecting interpretation to novel physics (quantum-gravity phenomenology, collapse-model tests via macroscopic superposition limits); a decisive discriminator would collapse the marketplace.',
    'If never discriminated, the arrangement''s competition-for-authority dynamic persists indefinitely, sustaining its extraction profile; a discriminator would force rapid reallocation and likely dissolve the enforcement layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_underdetermination_horizon, empirical, 'Whether empirical arbitration of the kernel''s readings is possible in practice or principle.').

omega_variable(
    decoherence_sufficiency_for_outcomes,
    'Does decoherence by itself ground definite, single-record outcomes (branching with classical appearance), or does it merely suppress interference while leaving the definiteness of outcomes unexplained?',
    'Technical analysis of decoherence''s capacity to select robust records and of whether the appearance of outcomes needs an additional interpretive posit; track the preferred-basis and record-selection literature.',
    'If insufficient, a larger share of the program''s ''solved'' rhetoric is performative, raising the honest theater_ratio and pushing the arrangement toward inertial maintenance; if sufficient, the technical layer fully backs the interpretive claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoherence_sufficiency_for_outcomes, conceptual, 'Whether decoherence delivers what the reading''s rhetoric credits it with.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qfmwi_tr_t0, quantum_formalism__many_worlds_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(qfmwi_tr_t0, observed).
narrative_ontology:measurement(qfmwi_tr_t11, quantum_formalism__many_worlds_reading, theater_ratio, 11, 0.06).
narrative_ontology:measurement_basis(qfmwi_tr_t11, observed).
narrative_ontology:measurement(qfmwi_tr_t22, quantum_formalism__many_worlds_reading, theater_ratio, 22, 0.09).
narrative_ontology:measurement_basis(qfmwi_tr_t22, observed).
narrative_ontology:measurement(qfmwi_tr_t33, quantum_formalism__many_worlds_reading, theater_ratio, 33, 0.12).
narrative_ontology:measurement_basis(qfmwi_tr_t33, observed).
narrative_ontology:measurement(qfmwi_tr_t44, quantum_formalism__many_worlds_reading, theater_ratio, 44, 0.15).
narrative_ontology:measurement_basis(qfmwi_tr_t44, observed).
narrative_ontology:measurement(qfmwi_tr_t55, quantum_formalism__many_worlds_reading, theater_ratio, 55, 0.18).
narrative_ontology:measurement_basis(qfmwi_tr_t55, observed).
narrative_ontology:measurement(qfmwi_tr_t66, quantum_formalism__many_worlds_reading, theater_ratio, 66, 0.2).
narrative_ontology:measurement_basis(qfmwi_tr_t66, observed).

% Extraction over time
narrative_ontology:measurement(qfmwi_be_t0, quantum_formalism__many_worlds_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(qfmwi_be_t0, observed).
narrative_ontology:measurement(qfmwi_be_t11, quantum_formalism__many_worlds_reading, base_extractiveness, 11, 0.18).
narrative_ontology:measurement_basis(qfmwi_be_t11, observed).
narrative_ontology:measurement(qfmwi_be_t22, quantum_formalism__many_worlds_reading, base_extractiveness, 22, 0.26).
narrative_ontology:measurement_basis(qfmwi_be_t22, observed).
narrative_ontology:measurement(qfmwi_be_t33, quantum_formalism__many_worlds_reading, base_extractiveness, 33, 0.31).
narrative_ontology:measurement_basis(qfmwi_be_t33, observed).
narrative_ontology:measurement(qfmwi_be_t44, quantum_formalism__many_worlds_reading, base_extractiveness, 44, 0.37).
narrative_ontology:measurement_basis(qfmwi_be_t44, observed).
narrative_ontology:measurement(qfmwi_be_t55, quantum_formalism__many_worlds_reading, base_extractiveness, 55, 0.41).
narrative_ontology:measurement_basis(qfmwi_be_t55, observed).
narrative_ontology:measurement(qfmwi_be_t66, quantum_formalism__many_worlds_reading, base_extractiveness, 66, 0.42).
narrative_ontology:measurement_basis(qfmwi_be_t66, observed).

% Suppression requirement over time
narrative_ontology:measurement(qfmwi_su_t0, quantum_formalism__many_worlds_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(qfmwi_su_t0, observed).
narrative_ontology:measurement(qfmwi_su_t11, quantum_formalism__many_worlds_reading, suppression_requirement, 11, 0.08).
narrative_ontology:measurement_basis(qfmwi_su_t11, observed).
narrative_ontology:measurement(qfmwi_su_t22, quantum_formalism__many_worlds_reading, suppression_requirement, 22, 0.15).
narrative_ontology:measurement_basis(qfmwi_su_t22, observed).
narrative_ontology:measurement(qfmwi_su_t33, quantum_formalism__many_worlds_reading, suppression_requirement, 33, 0.22).
narrative_ontology:measurement_basis(qfmwi_su_t33, observed).
narrative_ontology:measurement(qfmwi_su_t44, quantum_formalism__many_worlds_reading, suppression_requirement, 44, 0.27).
narrative_ontology:measurement_basis(qfmwi_su_t44, observed).
narrative_ontology:measurement(qfmwi_su_t55, quantum_formalism__many_worlds_reading, suppression_requirement, 55, 0.29).
narrative_ontology:measurement_basis(qfmwi_su_t55, observed).
narrative_ontology:measurement(qfmwi_su_t66, quantum_formalism__many_worlds_reading, suppression_requirement, 66, 0.3).
narrative_ontology:measurement_basis(qfmwi_su_t66, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, identity_coordination).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the kernel 'quantum formalism': the colloquial label covers structurally distinct commitments, so each reading is a separate story with its own epsilon, beneficiaries, and type (epsilon-invariance principle). This file (many_worlds_reading) links to copenhagen_reading and pilot_wave_reading. Historical influence runs upstream: Copenhagen's textbook orthodoxy defined the measurement problem this reading answered, and the pilot-wave no-go-theorem landscape (Bell, Kochen-Specker) shapes this reading's completeness axiom; edges are declared from this reading to both siblings accordingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
