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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   The many-worlds (Everettian) reading treats the universal wavefunction as
 *   the whole of physical reality, evolving deterministically under the
 *   Schrodinger equation with no special collapse process. 'Measurement' is
 *   not a distinct physical event but decoherence-induced branching:
 *   superpositions become dynamically uncorrelated (functionally separate
 *   'worlds') as systems entangle with environments, and all outcomes are
 *   realized, each in its own branch. This is one of three structurally
 *   distinct readings of the shared quantum formalism kernel —
 *   copenhagen_reading (collapse is real, physical, and marks an epistemic
 *   boundary) and pilot_wave_reading (hidden variables restore classical
 *   determinism via a guiding field) are the other two, generated as separate
 *   constraint stories. The underlying mathematics (unitary evolution,
 *   decoherence theory) is shared infrastructure across all three and is not
 *   itself the site of contest; the contest is over what that mathematics
 *   means and what additionally exists.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.42).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.28).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds (Everettian) Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, 'b57979a1-09e4-401a-ad0e-412b639d58af').
narrative_ontology:cs_kernel_codification('b57979a1-09e4-401a-ad0e-412b639d58af', formalized).
narrative_ontology:cs_authority_grounding('b57979a1-09e4-401a-ad0e-412b639d58af', expertise).
narrative_ontology:cs_interpretation_layer_present('b57979a1-09e4-401a-ad0e-412b639d58af').
narrative_ontology:cs_reading_relation('b57979a1-09e4-401a-ad0e-412b639d58af', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('b57979a1-09e4-401a-ad0e-412b639d58af', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('b57979a1-09e4-401a-ad0e-412b639d58af', foundational, unitary_evolution_is_complete_and_universal).
narrative_ontology:cs_axiom_status(unitary_evolution_is_complete_and_universal, holdable).
narrative_ontology:cs_axiom_grounding('b57979a1-09e4-401a-ad0e-412b639d58af', unitary_evolution_is_complete_and_universal, empirically_contingent).
narrative_ontology:cs_axiom('b57979a1-09e4-401a-ad0e-412b639d58af', foundational, observer_is_eliminable_from_fundamental_description).
narrative_ontology:cs_axiom_status(observer_is_eliminable_from_fundamental_description, holdable).
narrative_ontology:cs_axiom_grounding('b57979a1-09e4-401a-ad0e-412b639d58af', observer_is_eliminable_from_fundamental_description, conventional).
narrative_ontology:cs_axiom('b57979a1-09e4-401a-ad0e-412b639d58af', secondary, ontological_cost_of_branches_is_acceptable_price_of_parsimony).
narrative_ontology:cs_axiom_status(ontological_cost_of_branches_is_acceptable_price_of_parsimony, holdable).
narrative_ontology:cs_axiom_grounding('b57979a1-09e4-401a-ad0e-412b639d58af', ontological_cost_of_branches_is_acceptable_price_of_parsimony, instrumental).
narrative_ontology:cs_reference_frame('b57979a1-09e4-401a-ad0e-412b639d58af', everett_1957_relative_state_formulation).
narrative_ontology:cs_drift_state('b57979a1-09e4-401a-ad0e-412b639d58af', post_decoherence_program_maturity, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b57979a1-09e4-401a-ad0e-412b639d58af', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, everettian_theoretical_physicists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, decoherence_program_researchers).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_computing_no_collapse_framers).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, graduate_students_committed_early).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, experimentalists_seeking_falsification_funding).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, rival_interpretation_researchers).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, unitary_evolution_is_complete).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, born_rule_as_derived_not_axiomatic).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, observer_independence_of_physical_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and defend the decoherence-branching program, publish derivations attempting to recover the Born rule from decision theory or self-locating uncertainty, and set the research agenda for what counts as progress on the measurement problem within this camp. Their professional standing and citation networks are built on the interpretation's continued plausibility; they can pivot topics (decoherence theory itself is uncontroversially useful) more easily than junior researchers can.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, everettian_theoretical_physicists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, everettian_theoretical_physicists, beneficiary).

% Work on the mathematically rigorous, empirically well-supported decoherence formalism that many-worlds leans on for its 'no collapse needed' claim. This part of their work is portable to any interpretation and even to purely instrumental readings, so they benefit from the many-worlds narrative's prestige without being trapped if it falls from favor.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, decoherence_program_researchers, beneficiary,
    organized, biographical, mobile, global).

% Use the rhetorical convenience of 'no collapse, just branching' to explain quantum parallelism to funders and the public. The interpretive commitment is largely decorative for their actual engineering work, which does not depend on which interpretation is true.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_computing_no_collapse_framers, beneficiary,
    organized, biographical, mobile, global).

% Choose a foundations-of-physics specialization and align with a many-worlds-friendly advisor early, before the field's contested status is fully apparent. Switching interpretive camps mid-PhD costs years and advisor relationships; switching out of foundations entirely means abandoning sunk specialization. Their career outcomes ride on a philosophical question with no decisive experimental resolution in sight.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, graduate_students_committed_early, payer,
    powerless, biographical, trapped, national).

% Attempt to design experiments that would discriminate between interpretations (decoherence timescales, macroscopic superposition tests) but face funding panels skeptical that interpretation is empirically resolvable at all, partly because many-worlds proponents argue no experiment could ever distinguish the reading from its rivals by construction. This starves a research direction that would otherwise test the very claim.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, experimentalists_seeking_falsification_funding, payer,
    moderate, biographical, constrained, national).

% Copenhagen and pilot-wave researchers compete for the same limited foundations-of-physics positions, journal space, and conference slots. They argue many-worlds' ontological extravagance is being normalized as default 'serious physicist' opinion through institutional weight rather than decisive argument, crowding out alternative research programs.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, rival_interpretation_researchers, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, rival_interpretation_researchers, excluded).

% Are taught quantum mechanics with whichever interpretive gloss their instructor favors, usually without exposure to the full interpretive contest, and have no voice in which reading becomes the default framing they carry forward into research or teaching careers of their own.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, physics_undergraduate_students, excluded,
    powerless, biographical, trapped, national).

% Study the interpretive contest itself, including how underdetermination by evidence interacts with sociological factors (advisor lineages, funding structures, publication venues) in sustaining or eroding any one reading's dominance.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, philosophy_of_science_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__many_worlds_reading, everettian_theoretical_physicists).
narrative_ontology:fixing_cost_class(quantum_formalism__many_worlds_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mathematically minimal, deterministic, universally unitary formalism that avoids postulating a special physical collapse process or an observer-dependent boundary, letting researchers derive classical appearances and probabilistic outcomes from decoherence and branching alone without adding new physics.
% TRANSFER_FUNCTION: Moves prestige, funding attention, and career viability toward researchers and programs aligned with the no-collapse, branching ontology, and away from rival-interpretation researchers and from experimentalists whose falsification-oriented proposals are discounted by an interpretation that treats empirical underdetermination as a feature rather than a problem to solve.
% ABSENT_VOICES: Physics undergraduates receive the interpretation (or a rival) as settled background without ever hearing the contest laid out; rival-interpretation researchers are in the room but structurally disadvantaged in venues and hiring committees shaped by decades of many-worlds-friendly institutional weight at certain departments.
% DISAPPEARANCE_RATIONALE: If the many-worlds reading vanished from the field overnight, the underlying decoherence mathematics and the unitary formalism would persist untouched — they are shared infrastructure across all three readings. What would rearrange is the interpretive narrative layered on top: career trajectories built on the branching ontology, popular-science framing of quantum computing, and the specific philosophical commitments (observer-eliminability, global determinism) that many researchers have staked significant work on defending. Whether that counts as 'the world rearranges' or 'the world stays the same' is itself part of the contest — everettians would say little of substance changes (the physics is unaffected); critics would say a great deal of institutional weight and career investment would suddenly need new justification.
% FOUNDING_PROBLEM: Standard (Copenhagen-style) quantum mechanics postulates an unexplained, physically ad hoc collapse process and treats 'measurement' and 'observer' as primitive, undefined terms doing enormous theoretical work without a mechanism — a problem Everett's 1957 relative-state formulation was built to dissolve by taking the Schrodinger equation to apply universally, without exception, to observers included.
% FOUNDING_PROBLEM_CORROBORATION: Decoherence theorists (largely interpretation-neutral) corroborate that the mathematical problem Everett identified — the ad hoc, unexplained nature of postulated collapse — is real and unsolved by Copenhagen as originally stated. However, philosophers of physics outside the everettian camp (including some pilot-wave and objective-collapse theorists, who are not beneficiaries of the many-worlds reading) contest whether branching-into-separate-worlds is a solution to that problem or a restatement of it with added ontology; no experimental result corroborates the branching claim itself, since by the reading's own account no experiment could.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
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
 *   Extraction is authored moderate (0.42) and rising over the interval: the constraint's cost is not financial extraction in the ordinary sense but career and epistemic-attention capture — early-committed researchers who orient a career around the branching ontology bear a cost when the field's center of gravity shifts, and experimentalists proposing discriminating tests face funding disadvantage partly because the reading's own logic (no experiment could ever distinguish it) discourages the panels that would fund them. Suppression (0.28) is lower than extraction because exit is genuinely available for most actors (decoherence work is portable, quantum computing framing is decorative) except for the trapped early-career seat. Theater ratio (0.22) is modest but rising, reflecting an increasing gap between the popular 'many worlds' framing used in outreach/funding pitches and the actual technical content (decoherence, not literal world-splitting) driving real research.
 *
 * DIRECTIONALITY LOGIC:
 *   Everettian theorists and the decoherence/QC-adjacent researchers sit near the beneficiary end: they collect prestige, funding narrative, and institutional standing from the reading's currency, with mobile or arbitrage-grade exit because their technical work transfers across interpretations. Graduate students committed early sit at the trapped-target end: sunk specialization with no comparable exit, bearing the interpretive contest's career risk without the seniority to hedge it. Rival-interpretation researchers are targets of a subtler kind — not extracted from directly, but structurally disadvantaged in a shared, scarce resource pool (positions, venues) by an interpretation with greater institutional weight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (collapse is ad hoc and unexplained) remains genuinely live at the mathematical level — this is why the constraint is not classified as pure extraction (snare) despite the career costs it imposes. The coordination function (a minimal, unitary, observer-eliminable formalism) is real and shared with decoherence theory generally. What has drifted is the ratio of technical content to institutional/rhetorical currency: the reading increasingly does interpretive-identity work (this is who I am as a physicist) alongside its explanatory work, which is the tangled-rope signature — genuine coordination (solving Everett's 1957 problem) coexisting with asymmetric cost-bearing (trapped junior researchers, disadvantaged rivals) sustained by active institutional enforcement (hiring committees, department cultures, funding panel composition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    born_rule_derivation_circularity,
    'Can the Born rule''s probabilities be genuinely derived from decision theory or self-locating uncertainty within a purely deterministic branching structure, or does every proposed derivation covertly smuggle in a probability postulate?',
    'Formal review by researchers outside the everettian program of whether the decision-theoretic derivations (Deutsch-Wallace and successors) are non-circular; convergence or persistent disagreement in the philosophy-of-physics literature over multiple decades is itself evidence.',
    'If the derivation is genuinely circular, many-worlds'' central selling point over its rivals (deriving rather than postulating probability) fails, which would substantially lower its claim to formal minimalism and increase the case that its persistence is sociological rather than explanatory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(born_rule_derivation_circularity, conceptual, 'Whether the Born rule is genuinely derived or covertly assumed in the many-worlds framework.').

omega_variable(
    ontological_cost_accounting,
    'Should the reading be assessed by parsimony of postulates (many-worlds wins: no collapse postulate needed) or parsimony of entities (many-worlds loses badly: infinite unobservable worlds)?',
    'This is a question about which parsimony criterion physics should use, not a fact resolvable by data — it depends on prior methodological commitments in philosophy of science.',
    'Adopting postulate-parsimony as the standard favors many-worlds and pilot-wave over Copenhagen; adopting entity-parsimony favors Copenhagen and disfavors many-worlds sharply. The classification of many-worlds as elegant vs. extravagant flips entirely depending on which criterion is applied.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_cost_accounting, preference, 'Which parsimony criterion should govern interpretive theory-choice in quantum foundations.').

omega_variable(
    empirical_underdetermination_permanence,
    'Is the underdetermination between many-worlds, Copenhagen, and pilot-wave permanent in principle, or could future physics (quantum gravity, decoherence-rate measurements at macroscopic scale) break the tie?',
    'Track proposed experimental discriminators (e.g., macroscopic superposition longevity tests, potential quantum gravity signatures) for whether any gain community-wide acceptance as genuinely discriminating rather than being reinterpreted as consistent with all three readings after the fact.',
    'If permanently underdetermined, the persistence of institutional advantage for any one reading is harder to justify on truth-tracking grounds and more plausibly explained by sociological/career factors — strengthening the tangled_rope classification. If breakable, the current cost to disadvantaged researchers may be temporary rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_underdetermination_permanence, empirical, 'Whether the interpretive underdetermination is permanent in principle or contingently unresolved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 1957, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1957, quantum_formalism__many_worlds_reading, theater_ratio, 1957, 0.05).
narrative_ontology:measurement_basis(quan_tr_t1957, observed).
narrative_ontology:measurement(quan_tr_t1975, quantum_formalism__many_worlds_reading, theater_ratio, 1975, 0.08).
narrative_ontology:measurement_basis(quan_tr_t1975, observed).
narrative_ontology:measurement(quan_tr_t1990, quantum_formalism__many_worlds_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement_basis(quan_tr_t1990, observed).
narrative_ontology:measurement(quan_tr_t2005, quantum_formalism__many_worlds_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement_basis(quan_tr_t2005, observed).
narrative_ontology:measurement(quan_tr_t2015, quantum_formalism__many_worlds_reading, theater_ratio, 2015, 0.19).
narrative_ontology:measurement_basis(quan_tr_t2015, observed).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__many_worlds_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(quan_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t1957, quantum_formalism__many_worlds_reading, base_extractiveness, 1957, 0.15).
narrative_ontology:measurement_basis(quan_be_t1957, observed).
narrative_ontology:measurement(quan_be_t1975, quantum_formalism__many_worlds_reading, base_extractiveness, 1975, 0.2).
narrative_ontology:measurement_basis(quan_be_t1975, observed).
narrative_ontology:measurement(quan_be_t1990, quantum_formalism__many_worlds_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement_basis(quan_be_t1990, observed).
narrative_ontology:measurement(quan_be_t2005, quantum_formalism__many_worlds_reading, base_extractiveness, 2005, 0.34).
narrative_ontology:measurement_basis(quan_be_t2005, observed).
narrative_ontology:measurement(quan_be_t2015, quantum_formalism__many_worlds_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement_basis(quan_be_t2015, observed).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__many_worlds_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(quan_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quantum_formalism__many_worlds_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quantum_formalism kernel (BGS-style decomposition, per the ε-invariance principle). Each reading shares the underlying unitary formalism and decoherence mathematics but diverges on the ontological status of measurement, the reality of collapse, and the completeness of the wavefunction description. copenhagen_reading treats collapse as physically real and foundational (ε profile centered on epistemic-boundary enforcement in pedagogy and canonical textbook framing); pilot_wave_reading restores hidden-variable determinism at the cost of nonlocal guidance dynamics (ε profile centered on the minority-program funding and legitimacy cost pilot-wave researchers bear). many_worlds_reading (this story) accepts unobservable ontological proliferation to avoid both collapse and hidden variables, with ε concentrated in career/institutional capture rather than empirical extraction, since the reading's own logic denies empirical discriminability is expected. The three stories should be read together to see the full contested kernel; none alone represents 'the' interpretation of quantum mechanics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
