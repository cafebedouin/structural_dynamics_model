% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Many-Worlds Reading of the Quantum Formalism (Universal Branching Regime)
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The many-worlds reading holds that the universal wavefunction evolves
 *   deterministically under the Schrödinger equation, that measurement is
 *   nothing over and above decoherence-induced apparent branching, and that
 *   every outcome is realized in its own branch. As a governing arrangement
 *   it does real work: it delivers a collapse-free, observer-free framework
 *   that lets quantum theory be applied uniformly to microscopic,
 *   macroscopic, and cosmological systems, and it anchors a functioning
 *   realist research community. It also imposes asymmetric costs: every
 *   adopter carries the branching ontology, critics spend standing against a
 *   center that absorbs objections, students inherit a contested probability
 *   derivation as background, and rival-reading researchers pay a framing tax
 *   under which their added structure reads as superfluous. This file is ONE
 *   READING of the quantum_formalism kernel (Rule 1): the Copenhagen and
 *   pilot-wave readings are separate constraints with their own epsilon
 *   values and are linked, not folded in here. The claim and the metrics are
 *   independent authored facts: the claimed type records my structural
 *   judgment (coordination plus asymmetric extraction under active defense);
 *   the metrics record the arrangement's observed operation from this
 *   reading's own seat.
 *
 * KEY AGENTS:
 *   - everettian_leading_theorists: agenda-setting beneficiary (institutional power / identity-locked exit) — develops and defends the reading; collects its legitimacy
 *   - decoherence_program_researchers: secondary beneficiary (organized / mobile) — supplies the branching mechanism; portable across readings
 *   - quantum_computing_foundations_researchers: beneficiary (moderate / constrained) — draws explanatory leverage from the branching picture
 *   - hidden_variable_interpretation_researchers: payer (moderate / constrained) — pays standing costs under the completeness framing
 *   - instrumentalist_physicists: diffuse payer (institutional / arbitrage) — bears framing costs it can walk away from
 *   - physics_graduate_students: payer and absent voice (powerless / trapped) — inherits the controversy as training background
 *   - philosophy_of_physics_critics: payer (organized / constrained) — mounts the standing objections; absorbed rather than silenced
 *   - journal_editors_and_grant_panels: resource agenda-setter (institutional / constrained) — gates publication and funding
 *   - foundations_historians: analytical observer (analytical / analytical) — maps the dispute from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.45).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.3).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Reading of the Quantum Formalism (Universal Branching Regime)").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, 'b3ae8033-f136-4d45-a538-cc78b920b260').
narrative_ontology:cs_kernel_codification('b3ae8033-f136-4d45-a538-cc78b920b260', formalized).
narrative_ontology:cs_authority_grounding('b3ae8033-f136-4d45-a538-cc78b920b260', expertise).
narrative_ontology:cs_interpretation_layer_present('b3ae8033-f136-4d45-a538-cc78b920b260').
narrative_ontology:cs_reading_relation('b3ae8033-f136-4d45-a538-cc78b920b260', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3ae8033-f136-4d45-a538-cc78b920b260', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('b3ae8033-f136-4d45-a538-cc78b920b260', foundational, wavefunction_completeness_axiom).
narrative_ontology:cs_axiom_status(wavefunction_completeness_axiom, holdable).
narrative_ontology:cs_axiom_grounding('b3ae8033-f136-4d45-a538-cc78b920b260', wavefunction_completeness_axiom, conventional).
narrative_ontology:cs_axiom('b3ae8033-f136-4d45-a538-cc78b920b260', foundational, deterministic_universal_evolution_axiom).
narrative_ontology:cs_axiom_status(deterministic_universal_evolution_axiom, holdable).
narrative_ontology:cs_axiom_grounding('b3ae8033-f136-4d45-a538-cc78b920b260', deterministic_universal_evolution_axiom, empirically_contingent).
narrative_ontology:cs_reference_frame('b3ae8033-f136-4d45-a538-cc78b920b260', universal_unitary_deterministic_dynamics).
narrative_ontology:cs_drift_state('b3ae8033-f136-4d45-a538-cc78b920b260', contemporary_post_decoherence_consolidation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b3ae8033-f136-4d45-a538-cc78b920b260', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, everettian_leading_theorists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, decoherence_program_researchers).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_computing_foundations_researchers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, hidden_variable_interpretation_researchers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, instrumentalist_physicists).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, physics_graduate_students).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, philosophy_of_physics_critics).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, universal_unitary_dynamics).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, decoherence_branching_emergence).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, wavefunction_completeness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop, defend, and extend the reading through monographs, journal articles, and conference addresses; train students into it; answer objections in print. Their scholarly identities and bodies of work are bound to the reading's fortunes, and they collect citations, chairs, and standing as it consolidates. Departure would mean publicly repudiating their life's central commitment, so leaving is effectively unavailable to them.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, everettian_leading_theorists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, everettian_leading_theorists, beneficiary).

% Work out how interaction with environments suppresses interference between macroscopically distinct components of a quantum state. The reading gives their results a direct ontological payoff — the branching mechanism — but the same mathematics serves every rival reading, so they can carry their research program to any interpretive home without technical loss.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, decoherence_program_researchers, beneficiary,
    organized, biographical, mobile, global).

% Describe computation as interference among components of a global state and draw explanatory leverage from that picture. The reading supplies the realist backdrop their explanatory style presumes; their niche is narrower than the decoherence community's and ties them to the reading more tightly, though day-to-day hardware work proceeds without any interpretation at all.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_computing_foundations_researchers, beneficiary,
    moderate, biographical, constrained, global).

% Build completions on which particles carry definite positions guided by the wavefunction. The reading's framing of the wavefunction as complete casts their added structure as superfluous, costing them standing in venues that prize the bare formalism's economy. Their community is small, positions are scarce, and moving into mainstream foundations work means abandoning their research program.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, hidden_variable_interpretation_researchers, payer,
    moderate, biographical, constrained, global).

% Compute scattering amplitudes, design experiments, and teach the formalism's rules without committing to what the wavefunction is. They absorb a diffuse reputational cost when the reading's center characterizes refusal to commit as evasion, but they hold near-complete exit: declining to engage with foundations costs them nothing in laboratory practice, funding, or publication in their own venues.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, instrumentalist_physicists, payer,
    institutional, biographical, arbitrage, global).

% Meet the interpretation dispute in coursework, qualifying exams, and seminar culture, inheriting it as settled background in some departments and as open scandal in others. Those entering foundations find advisor availability and job prospects shaped by which reading their department favors. They lack the standing to contest how the dispute is framed, yet bear its training costs and carry its controversies into their early careers.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, physics_graduate_students, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, physics_graduate_students, excluded).

% Maintain the standing objections: how definite probabilities emerge from branching, what fixes the basis of everyday experience, whether unobservable worlds earn their keep. They are published and answered rather than silenced, but their professional niche is the critique itself, and each exchange spends standing against a center that recasts objections as misunderstandings to be absorbed.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, philosophy_of_physics_critics, payer,
    organized, biographical, constrained, global).

% Allocate publication space, special issues, invited symposia, and funding lines across the interpretation landscape. Their allocations tilt with the reading's fashionability, and reversing an accumulated editorial orientation carries coordination costs across venues and agencies, so they tend to follow consolidations rather than anticipate them.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, journal_editors_and_grant_panels, agenda_setter,
    institutional, generational, constrained, continental).

% Reconstruct how the reading was proposed, abandoned for roughly a decade, revived through decoherence and quantum computation, and consolidated into a school. Positioned outside the contest, they document enforcement patterns, patronage, and the shifting fortunes of the rival readings without collecting from any of them.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, foundations_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, everettian_leading_theorists).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a collapse-free, observer-free framework under which quantum theory applies uniformly to microscopic, macroscopic, and cosmological systems, and coordinates a realist research community around a shared ontological reading of the formalism — solving, for its members, the problem of how to apply quantum mechanics to closed systems (including the universe) without invoking external observers or an unexplained collapse process.
% TRANSFER_FUNCTION: Moves interpretive authority and research legitimacy toward formalism-first realism, concentrating cosmological and decoherence application benefits in the Everettian programs; moves the cost of the branching ontology onto every adopter, argumentative standing from critics to the center that absorbs their objections, and an inherited controversy (with its hardest problem pre-labeled as handled) onto incoming students.
% ABSENT_VOICES: Working experimental physicists who neither measure nor care about interpretation would object that the dispute changes no prediction and consumes attention better spent elsewhere; they sit outside the foundations subfield, in laboratories and industry, and enter the conversation only when polled. Historically, the reading's originator was himself absent for a decade — sidelined after 1957 until the DeWitt revival — a silence the community's own histories document.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, quantum cosmology would lose its collapse-free framework and revert to instrumentalist fence-sitting or hidden-variable reconstruction; the Everettian community would disperse into Copenhagen practice or pilot-wave realism; decoherence results would retain their mathematics but lose their realist significance as a branching mechanism; and the measurement problem would remain open with one fewer live resolution on the table.
% FOUNDING_PROBLEM: The measurement problem: reconciling the deterministic, unitary evolution of the wavefunction with the definite single outcomes observed in measurement, without positing an unexplained collapse process or placing observers outside the physics.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: mainstream textbooks and disciplinary surveys continue to list the measurement problem among physics' open problems, and the standing critiques of non-Everettian philosophers of physics affirm both the problem's liveness and the incompleteness of the branching solution, particularly on probability. No corroborator outside the beneficiary set attests that the problem is solved; the 'handled' framing circulates chiefly within the Everettian community itself.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate (0.45 at interval end): the coordination deliverable is real and used, but the arrangement layers consolidation rents onto it — the branching ontology carried by every adopter, the probability derivation presented to newcomers as settled while contested in print, and the standing costs paid by critics and rival-reading researchers. Suppression is moderate-low (0.30): nothing coerces; the Copenhagen and pilot-wave readings remain fully publishable, and the enforcement that exists is soft — framing rhetoric, venue tilts, textbook emphasis. Theater is low-moderate (0.28): decoherence research is functional across all readings, but a growing share of activity is rhetorical consolidation — repeated re-derivations of the Born rule, restatements that the reading is 'just the formalism' — which performs settlement rather than producing it. Accessibility collapse is low (0.20): understanding the reading collapses no alternatives; both rivals remain fully available, which is itself evidence against the naturality self-presentation. Resistance is substantial (0.60): the probability problem keeps resistance alive inside the community, and instrumentalist, hidden-variable, and objective-collapse constituencies sustain it outside. Temporal shape, one shared eight-point grid with all three metrics authored at every point: extraction accumulates monotonically as the community consolidates (neglect-era 0.12 to 0.45); theater rises in step with rhetorical consolidation; suppression_requirement traces an enforcement hump — near-zero while the reading lay abandoned (1957-1967), building through the DeWitt popularization and the decoherence revival, peaking around the Wallace-era consolidation, then easing as the reading became an accepted option requiring less active defense. The hump is the honest signal: enforcement capacity was built up and then partially stood down, and the endpoint scalar reflects the stood-down state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the leading-theorist seat the arrangement is the formalism speaking for itself — near-zero imposition, pure service to physics. From the critic seat the same arrangement is an absorption machine: objections are heard, answered, and recast as misunderstandings, at standing cost to the objector. From the student seat it is an inherited background whose hardest open problem arrives pre-labeled as handled. From the instrumentalist seat it barely registers at all — a dispute one can decline to enter at no professional cost. The engine derives these divergent per-seat classifications from the structural data (power, exit, role); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the leading theorists collect legitimacy and standing; decoherence researchers collect ontological significance for their results; quantum-computing foundations researchers collect an explanatory backdrop. The leaders' identity lock deepens their stake in the arrangement without raising their directionality — they are subsidized by it, not targeted by it, and identity lock amplifies extraction only for targets. Among payers, exit structure dominates: instrumentalist physicists hold arbitrage-grade exit (decline to engage, lose nothing), which damps their effective burden despite nominal victim status; hidden-variable researchers and philosophy critics hold constrained niches and sit near the target end; graduate students are trapped by career-stage dependence and sit nearest full-target. Global spatial scope for most seats modestly amplifies effective extraction by making verification of the 'settled' framing harder. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already produce the correct relationships, including the arbitrage damping for the instrumentalist seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling deterministic unitary evolution with definite observed outcomes — is live, so no mandate has outlived its function and mandatrophy_resolved is not declared. The classification guards against two opposite mislabels. Reading the arrangement as pure coordination ignores the concentrated-benefit/diffuse-cost asymmetry: the cosmological and decoherence programs collect the application benefits while the ontological and reputational costs spread across adopters, critics, students, and rivals. Reading it as pure extraction ignores that exits are open, rivals thrive unmolested, and the coordination deliverable — a collapse-free framework that makes quantum cosmology tractable — is real, used, and unavailable from the rivals at equal formalist economy. The tangled-rope reading holds both facts: genuine coordination function, asymmetric extraction through the same structure, held together by continuous argumentative and institutional defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the quantum_formalism kernel; how would the sibling readings (copenhagen_reading, pilot_wave_reading) change the structural data?',
    'Author the sibling stories separately and compare per-seat classifications. The disagreement is located in the measurement postulate: physical collapse marking an epistemic boundary (Copenhagen), decoherence-induced apparent branching with all outcomes realized (this reading), or hidden-variable completion with definite particle positions (pilot wave).',
    'Each reading instantiates a different constraint with its own epsilon, beneficiaries, and victims; averaging across readings would fabricate a composite that no party actually holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: this story is the many_worlds_reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    born_rule_resolution_status,
    'Is the probability problem solved within the reading (the Deutsch-Wallace decision-theoretic derivation) or carried as an open difficulty presented to newcomers as settled?',
    'Track the community''s own acknowledgments alongside cross-tradition reception: the derivation''s architect concedes its contestability, and the Greaves, Albert, and Maudlin critiques remain unanswered to critics'' satisfaction. Resolved when a derivation wins assent outside the Everettian set, or when the community openly reclassifies the problem as open.',
    'If carried-as-settled, extraction is higher than authored, since adopters inherit a contested result as background and critics spend standing relitigating it; if genuinely solved, epsilon falls toward pure-coordination territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(born_rule_resolution_status, conceptual, 'Whether the reading''s principal internal debt is discharged or merely managed.').

omega_variable(
    ontology_cost_or_free_lunch,
    'Does accepting the branching multiverse impose a real cost on adopters (ontological extravagance), or is it a free consequence of taking the formalism literally?',
    'Conceptual analysis of what counts as theoretical cost: weigh the reading''s own admissions (the branching ontology described as the price of the theory) against its no-extra-structure rhetoric; survey how practicing adopters actually weigh the ontology in theory choice.',
    'If the ontology is a genuine imposed cost borne by all adopters while application benefits concentrate in cosmology and decoherence programs, the arrangement is coordination-plus-extraction; if costless, it approaches pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontology_cost_or_free_lunch, conceptual, 'Whether the infinite-worlds ontology is an imposed cost or a free corollary.').

omega_variable(
    enforcement_mode_sociology,
    'Is the reading''s enforcement coercive-institutional (editorial, funding, and hiring gatekeeping) or purely argumentative (persuasion among equals)?',
    'Sociological study of publication outcomes, grant decisions, and faculty hiring in foundations of physics: measure whether anti-Everettian work faces systematic disadvantage after controlling for quality indicators.',
    'Purely argumentative enforcement lowers suppression and weakens the active-enforcement requirement, trending the structure toward looser coordination; documented gatekeeping raises suppression and confirms enforced hybridity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mode_sociology, empirical, 'Sociological mode of the reading''s boundary maintenance.').

omega_variable(
    naturality_rhetoric_vs_structure,
    'The reading presents itself as ''no interpretation, just the formalism taken literally'' — is that naturality claim accurate, or does it mark a constructed arrangement maintained by identifiable programs?',
    'Test the claim''s content: if the formalism alone determined this reading, rival readings of the same formalism could not persist; the documented persistence of the Copenhagen and pilot-wave readings evidences construction. Confirm via the sibling stories'' independent viability.',
    'If the naturality claim fails, the reading''s self-presentation functions as a false-summit marker and its beneficiaries'' framing labor is part of the enforcement load; if it holds, part of the measured extraction is misattributed and belongs to the underlying measurement problem instead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturality_rhetoric_vs_structure, conceptual, 'Naturality self-presentation versus constructed, defended arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 0, 68).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(many_worlds_reading_tr_t0, quantum_formalism__many_worlds_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(many_worlds_reading_tr_t0, observed).
narrative_ontology:measurement(many_worlds_reading_tr_t10, quantum_formalism__many_worlds_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement_basis(many_worlds_reading_tr_t10, observed).
narrative_ontology:measurement(many_worlds_reading_tr_t20, quantum_formalism__many_worlds_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(many_worlds_reading_tr_t20, observed).
narrative_ontology:measurement(many_worlds_reading_tr_t30, quantum_formalism__many_worlds_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(many_worlds_reading_tr_t30, observed).
narrative_ontology:measurement(many_worlds_reading_tr_t40, quantum_formalism__many_worlds_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement_basis(many_worlds_reading_tr_t40, observed).
narrative_ontology:measurement(many_worlds_reading_tr_t50, quantum_formalism__many_worlds_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement_basis(many_worlds_reading_tr_t50, observed).
narrative_ontology:measurement(many_worlds_reading_tr_t58, quantum_formalism__many_worlds_reading, theater_ratio, 58, 0.27).
narrative_ontology:measurement_basis(many_worlds_reading_tr_t58, observed).
narrative_ontology:measurement(many_worlds_reading_tr_t68, quantum_formalism__many_worlds_reading, theater_ratio, 68, 0.28).
narrative_ontology:measurement_basis(many_worlds_reading_tr_t68, observed).

% Extraction over time
narrative_ontology:measurement(many_worlds_reading_be_t0, quantum_formalism__many_worlds_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(many_worlds_reading_be_t0, observed).
narrative_ontology:measurement(many_worlds_reading_be_t10, quantum_formalism__many_worlds_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(many_worlds_reading_be_t10, observed).
narrative_ontology:measurement(many_worlds_reading_be_t20, quantum_formalism__many_worlds_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement_basis(many_worlds_reading_be_t20, observed).
narrative_ontology:measurement(many_worlds_reading_be_t30, quantum_formalism__many_worlds_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement_basis(many_worlds_reading_be_t30, observed).
narrative_ontology:measurement(many_worlds_reading_be_t40, quantum_formalism__many_worlds_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(many_worlds_reading_be_t40, observed).
narrative_ontology:measurement(many_worlds_reading_be_t50, quantum_formalism__many_worlds_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement_basis(many_worlds_reading_be_t50, observed).
narrative_ontology:measurement(many_worlds_reading_be_t58, quantum_formalism__many_worlds_reading, base_extractiveness, 58, 0.44).
narrative_ontology:measurement_basis(many_worlds_reading_be_t58, observed).
narrative_ontology:measurement(many_worlds_reading_be_t68, quantum_formalism__many_worlds_reading, base_extractiveness, 68, 0.45).
narrative_ontology:measurement_basis(many_worlds_reading_be_t68, observed).

% Suppression requirement over time
narrative_ontology:measurement(many_worlds_reading_su_t0, quantum_formalism__many_worlds_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(many_worlds_reading_su_t0, observed).
narrative_ontology:measurement(many_worlds_reading_su_t10, quantum_formalism__many_worlds_reading, suppression_requirement, 10, 0.08).
narrative_ontology:measurement_basis(many_worlds_reading_su_t10, observed).
narrative_ontology:measurement(many_worlds_reading_su_t20, quantum_formalism__many_worlds_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(many_worlds_reading_su_t20, observed).
narrative_ontology:measurement(many_worlds_reading_su_t30, quantum_formalism__many_worlds_reading, suppression_requirement, 30, 0.26).
narrative_ontology:measurement_basis(many_worlds_reading_su_t30, observed).
narrative_ontology:measurement(many_worlds_reading_su_t40, quantum_formalism__many_worlds_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement_basis(many_worlds_reading_su_t40, observed).
narrative_ontology:measurement(many_worlds_reading_su_t50, quantum_formalism__many_worlds_reading, suppression_requirement, 50, 0.36).
narrative_ontology:measurement_basis(many_worlds_reading_su_t50, observed).
narrative_ontology:measurement(many_worlds_reading_su_t58, quantum_formalism__many_worlds_reading, suppression_requirement, 58, 0.34).
narrative_ontology:measurement_basis(many_worlds_reading_su_t58, observed).
narrative_ontology:measurement(many_worlds_reading_su_t68, quantum_formalism__many_worlds_reading, suppression_requirement, 68, 0.3).
narrative_ontology:measurement_basis(many_worlds_reading_su_t68, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, identity_coordination).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the interpretation of quantum mechanics' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one kernel (quantum_formalism). This story is the many_worlds_reading member; copenhagen_reading and pilot_wave_reading are siblings with their own epsilon values, victim sets, and classifications. The members are linked pairwise through affects_constraints. Upstream/downstream structure: the shared formalism's empirical success legitimizes all three; each reading then cites the formalism's economy against the others, so no member is cleanly upstream — the coupling is lateral competition within a family, and contamination propagates through shared objections (the probability problem, the preferred basis) rather than through dependency chains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
