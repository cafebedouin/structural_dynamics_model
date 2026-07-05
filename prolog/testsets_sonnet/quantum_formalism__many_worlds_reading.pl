% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Many-Worlds (Everettian) Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The Everett/many-worlds reading holds that the universal wavefunction
 *   evolves unitarily and deterministically at all times, without ever
 *   collapsing; apparent single outcomes on measurement are explained by
 *   decoherence-induced branching in which all quantum-mechanically permitted
 *   outcomes are realized, each in a separate, mutually non-interacting
 *   'world.' The reading is mathematically minimal (no extra postulates
 *   beyond the Schrodinger equation and decoherence) but ontologically
 *   maximal (an unobservable proliferation of worlds) and carries an
 *   unresolved technical problem: deriving the Born rule's probabilistic
 *   weights from a framework that, on its face, has no genuine probability,
 *   only branching multiplicity. This story treats the many-worlds reading as
 *   ONE of three structurally distinct readings of a single contested formal
 *   kernel (the quantum formalism); the Copenhagen and pilot-wave readings
 *   are separate constraint stories with their own ε values, per the
 *   ε-invariance principle — they are not alternative measurements of this
 *   constraint but different constraints.
 *
 * KEY AGENTS:
 *   - everettian_theorists: institutional/identity_locked — administer and benefit from the reading's academic and popular standing
 *   - decoherence_program_researchers: organized/mobile — benefit from elevated salience of adjacent empirical research
 *   - quantum_computing_narrative_builders: powerful/arbitrage — opportunistically deploy the framing for funding and marketing
 *   - graduate_students_seeking_ontological_clarity: powerless/trapped — bear career-capital risk under interpretive uncertainty
 *   - rival_interpretation_researchers: moderate/constrained — compete for attention and resources against an institutionally prestigious rival reading
 *   - public_science_communication_audiences: powerless/trapped — receive a confidence-inflated account of settled science
 *   - analytical_philosophers_of_physics: analytical/analytical — assess the formal and philosophical merits without institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.42).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.28).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds (Everettian) Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '7ea1af9b-b20c-4259-af1d-f40fd5839256').
narrative_ontology:cs_kernel_codification('7ea1af9b-b20c-4259-af1d-f40fd5839256', formalized).
narrative_ontology:cs_authority_grounding('7ea1af9b-b20c-4259-af1d-f40fd5839256', expertise).
narrative_ontology:cs_interpretation_layer_present('7ea1af9b-b20c-4259-af1d-f40fd5839256').
narrative_ontology:cs_reading_relation('7ea1af9b-b20c-4259-af1d-f40fd5839256', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('7ea1af9b-b20c-4259-af1d-f40fd5839256', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('7ea1af9b-b20c-4259-af1d-f40fd5839256', foundational, measurement_is_derivative_not_fundamental).
narrative_ontology:cs_axiom_status(measurement_is_derivative_not_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('7ea1af9b-b20c-4259-af1d-f40fd5839256', measurement_is_derivative_not_fundamental, conventional).
narrative_ontology:cs_axiom('7ea1af9b-b20c-4259-af1d-f40fd5839256', foundational, wavefunction_realism_completeness).
narrative_ontology:cs_axiom_status(wavefunction_realism_completeness, holdable).
narrative_ontology:cs_axiom_grounding('7ea1af9b-b20c-4259-af1d-f40fd5839256', wavefunction_realism_completeness, conventional).
narrative_ontology:cs_axiom('7ea1af9b-b20c-4259-af1d-f40fd5839256', secondary, all_branches_equally_real).
narrative_ontology:cs_axiom_status(all_branches_equally_real, holdable).
narrative_ontology:cs_axiom_grounding('7ea1af9b-b20c-4259-af1d-f40fd5839256', all_branches_equally_real, instrumental).
narrative_ontology:cs_reference_frame('7ea1af9b-b20c-4259-af1d-f40fd5839256', unitary_evolution_without_collapse).
narrative_ontology:cs_drift_state('7ea1af9b-b20c-4259-af1d-f40fd5839256', post_decoherence_program_maturation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7ea1af9b-b20c-4259-af1d-f40fd5839256', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, everettian_theorists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, decoherence_program_researchers).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_computing_narrative_builders).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, graduate_students_seeking_ontological_clarity).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, rival_interpretation_researchers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, public_science_communication_audiences).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, unitary_quantum_mechanics_completeness).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, wavefunction_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build careers, journals, conference tracks, and popular-science franchises on the claim that unitarity is complete and no collapse occurs. They administer the interpretation's plausibility structure — decision-theoretic derivations of the Born rule, decoherence formalism, philosophy-of-physics seminar syllabi — and benefit from its acceptance as the 'minimal' or 'default' reading among physicalist philosophers.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, everettian_theorists, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, everettian_theorists, beneficiary).

% Their empirical decoherence work (environment-induced superselection, einselection) is real physics that predates and is independent of the many-worlds ontological commitment, but the reading's popularity elevates the perceived importance and funding salience of their research program by attaching it to a totalizing metaphysical narrative.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, decoherence_program_researchers, beneficiary,
    organized, generational, mobile, global).

% Companies and popular-science communicators invoke 'parallel universes doing the computation' as a marketing and funding narrative for quantum computing, even though the computational speedup is interpretation-neutral. They can adopt or drop the many-worlds framing opportunistically depending on audience.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_computing_narrative_builders, beneficiary,
    powerful, biographical, arbitrage, global).

% Encounter the many-worlds reading presented in coursework or advising relationships as the physically serious, philosophically rigorous default, and must navigate committing scarce career capital to a research program whose central ontological commitment (branch counting, probability meaning under determinism) remains genuinely unresolved. Dissent from an advisor's preferred interpretation carries real career cost.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, graduate_students_seeking_ontological_clarity, payer,
    powerless, biographical, trapped, national).

% Copenhagen-adjacent and pilot-wave researchers compete for the same limited grant lines, journal space, and hiring committee attention; the many-worlds reading's institutional prestige in some Anglophone philosophy-of-physics departments crowds out consideration of rival ontologies as equally serious research programs.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, rival_interpretation_researchers, payer,
    moderate, biographical, constrained, global).

% Receive the many-worlds reading through popular books and documentaries as settled or near-settled physics ('parallel universes are real'), with the underlying interpretive contest and the probability-derivation controversy almost never disclosed. Their understanding of what physics has actually established is shaped by a curated, confidence-inflated version of a live dispute.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, public_science_communication_audiences, payer,
    powerless, biographical, trapped, global).

% Study the formal derivability of the Born rule from decision theory (Deutsch-Wallace program), the coherence of 'branch counting' as an ontological notion, and the comparative theoretical virtues of the three readings without a stake in any single research program's institutional success.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, analytical_philosophers_of_physics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, diffuse).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, internally consistent, mathematically economical account of quantum mechanics that requires no extra postulates (no collapse dynamics, no hidden variables) — coordinating theoretical work around unitary evolution alone and enabling decoherence theory to do double duty as both physics and metaphysics.
% TRANSFER_FUNCTION: Moves institutional attention, funding salience, graduate student career investment, and public epistemic trust toward the Everettian research program and its practitioners, and away from rival interpretive programs and away from honest disclosure of the field's genuine unresolved status to lay audiences.
% ABSENT_VOICES: Graduate students who privately doubt the interpretation but cannot say so to advisors; working physicists who consider interpretation debates irrelevant to their instrumental use of the formalism and are not represented in either camp's rhetoric; historians of science who would note that 'minimal' framing is itself a rhetorical achievement, not a physical fact.
% DISAPPEARANCE_RATIONALE: If the many-worlds reading vanished as a live research program overnight, the underlying mathematics of quantum mechanics (unitary evolution, decoherence, the Born rule as empirically confirmed) would be entirely unchanged — no experiment depends on it. But institutional structures (specific faculty lines, conference tracks, popular books, some funding narratives around quantum foundations) would need to reorganize. Everettian theorists would say the world of physics understanding rearranges; instrumentalist physicists would say nothing observable changes at all.
% FOUNDING_PROBLEM: The measurement problem: standard quantum mechanics as taught (Copenhagen-style) posited an unexplained, seemingly ad hoc collapse process triggered by 'measurement,' with no principled account of what counts as a measurement or why collapse should be a distinct physical process from ordinary unitary evolution.
% FOUNDING_PROBLEM_CORROBORATION: The measurement problem itself is corroborated as live by all three interpretive camps and by textbook treatments across the field — this is one of few points of consensus in quantum foundations. However, whether the MANY-WORLDS SOLUTION to that problem is itself settled, as opposed to merely one live contender, is attested as still contested by philosophers of physics outside the Everettian program (e.g., critiques of the probability/branch-counting derivation from decision-theoretic and frequentist perspectives) and by working physicists who report indifference to interpretation entirely.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at interval end) rather than low or extreme: the reading's mathematical core (unitary QM, decoherence theory) is genuine, rigorous physics with essentially zero extraction on its own — this is why it is NOT authored as a snare. But the totalizing ontological claim layered atop that mathematics ('all outcomes are equally real, in literally existing worlds') functions as a career-shaping and public-narrative-shaping social technology whose confidence exceeds its evidential warrant, particularly regarding the still-unresolved Born-rule-from-branching derivation. Suppression is low-moderate (0.28): dissent is possible and voiced within philosophy of physics, but real career costs attach to publicly rejecting an advisor's or department's preferred interpretation, and popular communication rarely discloses the controversy at all. Theater ratio (0.30) captures the gap between claimed settled-ness ('the simplest, most minimal interpretation') and the field's actual unresolved state on probability and branch-counting. All three tracked metrics rise together over the interval as the reading's popular and institutional prestige (post-Everett-50th-anniversary revival, quantum computing hype cycles) outpaced resolution of its foundational puzzles.
 *
 * DIRECTIONALITY LOGIC:
 *   Everettian theorists sit at the beneficiary end: institutional prestige, publication and citation gravity, popular-science visibility, and coherence with their own prior theoretical commitments all accrue to them, and their identity as researchers is substantially fused with the reading's institutional success (identity_locked). Graduate students and public audiences sit at the target end: they bear the cost of the reading's confident public presentation without commensurate institutional power to contest it — students risk career capital dissenting from advisors, audiences receive a curated epistemic picture. Rival interpretation researchers are targets of a subtler kind: they pay in attention-and-resource competition, not direct coercion. Decoherence researchers and quantum-computing narrative-builders are secondary beneficiaries who ride the reading's popularity without being its primary defenders — their mobile/arbitrage exit options reflect that they can decouple from the ontological claim if it becomes reputationally costly, unlike the fully identity-fused theorists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the measurement problem — why does 'measurement' seem to trigger a distinct physical process?) remains genuinely live and is corroborated as live by all camps, which is why this is NOT classified as a piton or dead-mandate structure. What is contested is not whether the problem exists but whether THIS reading has solved it, or has merely re-described it (as 'apparent branching') while introducing a new unresolved problem (deriving probability from a deterministic multiverse) equally serious as the one it dissolved. Tangled rope, not snare, is the claimed type because the coordination function is real and valuable (a rigorous, minimal-postulate account of unitary QM, real decoherence physics with independent empirical support) even as the totalizing ontological packaging around it extracts institutional and epistemic value disproportionate to its resolved status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    born_rule_derivability,
    'Can the Born rule''s probabilistic weights be rigorously derived from the many-worlds framework''s decision-theoretic axioms (Deutsch-Wallace program), or does the derivation smuggle in probability-like assumptions that a purely deterministic branching ontology cannot license?',
    'Continued formal philosophical and mathematical scrutiny of the decision-theoretic derivation; identification of whether any hidden probabilistic postulate is required, which would undercut the ''no extra postulates'' minimality claim central to the reading''s appeal.',
    'If the derivation genuinely succeeds without hidden postulates, the reading''s claim to superior theoretical economy over Copenhagen and pilot-wave is substantially strengthened, reducing the extractiveness assessment. If it fails, the reading''s minimality claim is a rhetorical overstatement and the extraction assessment is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(born_rule_derivability, empirical, 'Whether the Born rule is genuinely derivable within Everettian quantum mechanics without additional postulates.').

omega_variable(
    branch_ontology_naturalness,
    'Is ''branching into really-existing worlds'' a natural and minimal ontological consequence of unitary quantum mechanics, or a metaphysically extravagant interpretive choice among several equally consistent options?',
    'No empirical resolution is possible in principle — branches are by construction non-interacting and non-observable from within any branch. Resolution, if any, would come from philosophical argument about theoretical virtue (parsimony of postulates vs. parsimony of entities) rather than data.',
    'If treated as natural/minimal, the reading''s institutional prestige as the ''default'' interpretation is warranted and the diffuse gain_flow assessment stands as genuine, low-suppression coordination. If treated as one extravagant option among three live and comparably defensible readings, the reading''s outsized institutional and popular-narrative dominance reflects social/rhetorical capture rather than physical necessity — raising the effective extraction and suppression the story assigns.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(branch_ontology_naturalness, conceptual, 'Whether many-worlds ontology is a natural reading of the formalism or one contestable metaphysical choice among peers.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where, precisely, do the three quantum_formalism readings (Copenhagen, many-worlds, pilot-wave) locate their disagreement — is it about what exists (ontology), what happens during measurement (dynamics), or what can be known (epistemology)?',
    'Systematic comparison of the three readings'' treatment of the shared empirical core (identical predictions for all current experiments) against their divergent metaphysical commitments; this is the committer-structure question that distinguishes readings within one kernel from genuinely different physical theories.',
    'Confirms that all three readings are, in the relevant DR sense, structurally distinct constraints riding the SAME empirically confirmed formalism — none is falsified by current experiment, so the disagreement is located entirely in ontological/interpretive commitment, not in predictive content. This licenses generating them as three separate ε-invariant constraint stories rather than one story with an observable parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Documents that the three sibling readings diverge on ontology/measurement-status/determinism while sharing identical empirical predictions — the structural basis for treating them as a constraint family rather than one constraint measured three ways.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__many_worlds_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t12, quantum_formalism__many_worlds_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement_basis(quan_tr_t12, observed).
narrative_ontology:measurement(quan_tr_t24, quantum_formalism__many_worlds_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement_basis(quan_tr_t24, observed).
narrative_ontology:measurement(quan_tr_t36, quantum_formalism__many_worlds_reading, theater_ratio, 36, 0.24).
narrative_ontology:measurement_basis(quan_tr_t36, observed).
narrative_ontology:measurement(quan_tr_t48, quantum_formalism__many_worlds_reading, theater_ratio, 48, 0.27).
narrative_ontology:measurement_basis(quan_tr_t48, observed).
narrative_ontology:measurement(quan_tr_t60, quantum_formalism__many_worlds_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(quan_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__many_worlds_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t12, quantum_formalism__many_worlds_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement_basis(quan_be_t12, observed).
narrative_ontology:measurement(quan_be_t24, quantum_formalism__many_worlds_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(quan_be_t24, observed).
narrative_ontology:measurement(quan_be_t36, quantum_formalism__many_worlds_reading, base_extractiveness, 36, 0.36).
narrative_ontology:measurement_basis(quan_be_t36, observed).
narrative_ontology:measurement(quan_be_t48, quantum_formalism__many_worlds_reading, base_extractiveness, 48, 0.4).
narrative_ontology:measurement_basis(quan_be_t48, observed).
narrative_ontology:measurement(quan_be_t60, quantum_formalism__many_worlds_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(quan_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__many_worlds_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(quan_su_t0, observed).
narrative_ontology:measurement(quan_su_t12, quantum_formalism__many_worlds_reading, suppression_requirement, 12, 0.18).
narrative_ontology:measurement_basis(quan_su_t12, observed).
narrative_ontology:measurement(quan_su_t24, quantum_formalism__many_worlds_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement_basis(quan_su_t24, observed).
narrative_ontology:measurement(quan_su_t36, quantum_formalism__many_worlds_reading, suppression_requirement, 36, 0.24).
narrative_ontology:measurement_basis(quan_su_t36, observed).
narrative_ontology:measurement(quan_su_t48, quantum_formalism__many_worlds_reading, suppression_requirement, 48, 0.26).
narrative_ontology:measurement_basis(quan_su_t48, observed).
narrative_ontology:measurement(quan_su_t60, quantum_formalism__many_worlds_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement_basis(quan_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, information_standard).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'interpretation of quantum mechanics' (or colloquially 'the measurement problem') into structurally distinct readings of a shared formal kernel, per the ε-invariance principle. copenhagen_reading claims physical collapse and irreducible indeterminism (measurement is fundamental, not derivative); pilot_wave_reading claims deterministic hidden-variable trajectories with a physically real guiding wavefunction (restoring classical-style definite particle positions); this story (many_worlds_reading) claims global unitary determinism with no collapse and branching into equally real worlds. All three share identical empirical predictions for every experiment performed to date and therefore share the same underlying mathematical formalism (the kernel), but diverge sharply on ontology, on whether 'measurement' is a fundamental or derivative category, and on determinism. Each carries its own extractiveness, suppression, and institutional-economy profile because each attaches to different research communities, pedagogical traditions, and public-narrative economies. They are linked bidirectionally via affects_constraints because prestige, funding, and attention shifts toward one reading structurally affect the resource and legitimacy environment of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
