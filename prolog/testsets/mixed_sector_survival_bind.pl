% ============================================================================
% CONSTRAINT STORY: mixed_sector_survival_bind
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mixed_sector_survival_bind, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mixed_sector_survival_bind
 *   human_readable: Mixed-Sector Contact-Coupling Survival Bind (S_X(A,Z) Self-Limitation Requirement)
 *   domain: theoretical_physics/cosmology/speculative_astrophysics
 *
 * SUMMARY:
 *   This story addresses a distinct quantitative bind embedded within the
 *   mixed-sector (large alpha_m) hypothesis: the same contact-coupling
 *   strength needed to generate testable astrophysical signatures (Psyche
 *   mascons, solar core anomalies, catalytic fission) is independently
 *   required to be self-limiting enough that it does not destabilize ordinary
 *   matter over cosmological timescales. This is NOT the existence question
 *   (addressed by sibling readings inconsistency_reading,
 *   nonperturbative_matter_sector_reading, phenomenological_program_reading,
 *   mirror_sector_alternative_reading under the
 *   alpha_m_supercriticality_kernel) — it is a separate, sharper demand on
 *   the sign and magnitude of S_X(A,Z) and the droplet equation of state,
 *   conditioned on the sector existing at all. Under this reading, the
 *   hypothesis's own observational ambitions manufacture a survival
 *   requirement that the theory has not been shown to meet, and treating the
 *   two demands (testability, safety) as jointly satisfiable without
 *   demonstration is the extractive move: credibility is drawn from the
 *   observational program's promise while the harder quantitative accounting
 *   is deferred.
 *
 * KEY AGENTS:
 *   - hypothesis_credibility_and_ordinary_matter_survival: joint victim (analytical/trapped) — bears the cost of whichever quantitative outcome obtains
 *   - nonperturbative_matter_sector_research_program: beneficiary (organized/mobile) — retreats to nonminimal variants under any specific falsification
 *   - observational_falsification_program_advocates: beneficiary (institutional/arbitrage) — output value survives even if hypothesis dies
 *   - planetary_and_solar_stability_record: excluded non-agent — the brute empirical check, consulted post-hoc only
 *   - field_theory_consistency_critics: excluded (organized/mobile) — hold that the entire survival-bind question may be moot
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mixed_sector_survival_bind, 0.68).
domain_priors:suppression_score(mixed_sector_survival_bind, 0.42).
domain_priors:theater_ratio(mixed_sector_survival_bind, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mixed_sector_survival_bind, extractiveness, 0.68).
narrative_ontology:constraint_metric(mixed_sector_survival_bind, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(mixed_sector_survival_bind, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mixed_sector_survival_bind, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(mixed_sector_survival_bind, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mixed_sector_survival_bind, snare).
narrative_ontology:human_readable(mixed_sector_survival_bind, "Mixed-Sector Contact-Coupling Survival Bind (S_X(A,Z) Self-Limitation Requirement)").
narrative_ontology:topic_domain(mixed_sector_survival_bind, "theoretical_physics/cosmology/speculative_astrophysics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mixed_sector_survival_bind, nonperturbative_matter_sector_research_program).
narrative_ontology:constraint_beneficiary(mixed_sector_survival_bind, observational_falsification_program_advocates).
narrative_ontology:constraint_victim(mixed_sector_survival_bind, hypothesis_credibility_and_ordinary_matter_survival).
narrative_ontology:constraint_vindicates(mixed_sector_survival_bind, dirac_quantization_condition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As a joint entity, the hypothesis's scientific credibility and the empirical fact of long-term planetary/solar stability are bound together and both put at risk by the same demand: the coupling strong enough to make Psyche mascons, solar core anomalies, and catalytic fission testable is exactly the coupling regime where uncontrolled contact interactions could destabilize ordinary nuclear matter over Gyr timescales. There is no independent lever to relax one requirement without weakening the other; if S_X(A,Z) does not turn out self-limiting in just the right quantitative window, either the observable signatures vanish (hypothesis becomes untestable) or Earth/Moon/Sun's observed stability becomes unexplained (hypothesis becomes falsified by geology and helioseismology). This stakeholder cannot exit the bind because it is not an agent choosing among options — it is the joint casualty of whichever quantitative outcome obtains.
narrative_ontology:constraint_stakeholder(mixed_sector_survival_bind, hypothesis_credibility_and_ordinary_matter_survival, payer,
    analytical, civilizational, trapped, universal).

% Theorists building the composite/droplet machinery (liquid-drop EOS, hypernuclear-analogy binding, macro-dark-matter candidates) benefit from the large-alpha_m result being treated as a feature requiring an entire ontologically expansive research apparatus rather than a fatal inconsistency. They can retreat to nonperturbative or nonminimal model variants if any specific quantitative bind (e.g. a particular S_X(A,Z) sign) is falsified, preserving the program's viability across many parameter choices while individual predictions absorb the risk.
narrative_ontology:constraint_stakeholder(mixed_sector_survival_bind, nonperturbative_matter_sector_research_program, beneficiary,
    organized, generational, mobile, global).

% Researchers proposing the Psyche mascon survey, helioseismic anomaly searches, and LRD demographic studies benefit from the mixed-sector hypothesis's continued live status regardless of whether the underlying field theory is ever shown consistent — grant proposals, telescope time, and publication programs are justified by 'falsifiable predictions exist' rather than by prior resolution of the bound-state or survival problem. Their exit option is arbitrage: if the hypothesis dies, the observational techniques and null-result papers still count as productive output.
narrative_ontology:constraint_stakeholder(mixed_sector_survival_bind, observational_falsification_program_advocates, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(mixed_sector_survival_bind, observational_falsification_program_advocates, observer).

% The multi-Gyr observed stability of Earth, Moon, and Sun is the brute empirical fact against which the self-limitation requirement must be checked, but it has no voice in the theoretical debate — it simply sits as a constraint that any viable S_X(A,Z) sign/magnitude must satisfy. It is not consulted or represented in the model-building process; it is only checked against after the fact, and often only approximately.
narrative_ontology:constraint_stakeholder(mixed_sector_survival_bind, planetary_and_solar_stability_record, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(mixed_sector_survival_bind, planetary_and_solar_stability_record).

% Theorists holding the inconsistency reading (that alpha_m~34 with shared photon/color content signals a no-go theorem, not a matter sector) would object that the entire survival-bind question is moot if the minimal particle content cannot be completed into a consistent QFT at all. Their objection is structurally excluded from the phenomenological program's framing, which proceeds as though the existence question is separable from and secondary to the observational program.
narrative_ontology:constraint_stakeholder(mixed_sector_survival_bind, field_theory_consistency_critics, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mixed_sector_survival_bind, diffuse).
narrative_ontology:fixing_cost_class(mixed_sector_survival_bind, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the genuine sense — there is no collective-action problem being solved. The nearest analogue is a shared quantitative discipline: any theorist wanting to claim testable mixed-sector signatures must simultaneously show the same coupling does not destroy the very matter used to detect it, which in principle disciplines model-building against ad hoc parameter tuning.
% TRANSFER_FUNCTION: Moves scientific attention, grant justification, and publication credibility from a rigorous prior resolution of the survival bind toward continued pursuit of testable signatures under an unresolved quantitative tension. What is transferred is epistemic patience: the community extends credibility to the observational program before the self-limitation requirement on S_X(A,Z) and the droplet EOS has actually been shown satisfiable.
% ABSENT_VOICES: Field-theory consistency critics (inconsistency_reading) are largely absent from the phenomenological framing, which treats the existence question as bracketed rather than settled against the hypothesis. The brute empirical record of planetary/solar stability is also structurally voiceless — it functions as a post-hoc check rather than a design constraint baked into model construction from the start.
% DISAPPEARANCE_RATIONALE: If the survival-bind requirement disappeared overnight (i.e., if nobody demanded that the coupling be simultaneously large enough to test and self-limiting enough to be safe), the nonperturbative program and observational program would both lose their most serious internal check — model-builders could tune S_X(A,Z) freely without accountability to Earth/Moon/Sun's continued existence. Proponents would say nothing observable changes since the bind is already satisfied in viable models; critics would say the absence of the bind is precisely what let unfalsifiable parameter tuning proceed unchecked. The parties dispute whether the bind is currently doing real disciplinary work or is honored mostly in principle.
% FOUNDING_PROBLEM: The problem the survival-bind requirement was built to address: without it, any large contact-coupling value derived from Dirac quantization (alpha_m~34) could be paired with arbitrarily tuned nuclear-level parameters to produce whatever observational signature is convenient, with no check against the elementary fact that ordinary matter has survived billions of years unmolested by any such sector.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the basic geological and helioseismological record itself (planetary and solar stability over Gyr timescales is an independent, non-theory-laden observation) and by critics holding the inconsistency reading, who argue the bind is not merely live but likely unsatisfiable for the minimal shared-photon/shared-color content. No party benefiting from the observational program's continuation has offered a demonstration that the bind is resolved rather than merely assumed satisfiable in viable corners of parameter space.
narrative_ontology:disappearance_verdict(mixed_sector_survival_bind, contested).
narrative_ontology:founding_problem_status(mixed_sector_survival_bind, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mixed_sector_survival_bind, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-11',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(mixed_sector_survival_bind, 'none', 1).
narrative_ontology:epsilon_provenance(mixed_sector_survival_bind, 0.68, 'claude-sonnet-5', 'dirac_magnetic_matter_2026_20260811_143746', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mixed_sector_survival_bind_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mixed_sector_survival_bind, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mixed_sector_survival_bind_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects that scientific credibility and observational-program resources are drawn from a hypothesis whose central quantitative safety condition (self-limiting S_X(A,Z)) has not been independently demonstrated satisfiable jointly with the testability condition — the two demands are asserted compatible rather than shown compatible. Suppression is moderate (0.42): there is no coercive apparatus, but null results and inconvenient EOS calculations face soft resistance from a program with strong incentive to keep both demands framed as jointly tractable. Theater ratio is high and rising (0.40 to 0.58) because an increasing share of activity is preparatory framing and grant-justifying observational design rather than resolution of the underlying nuclear-physics accounting; the discourse increasingly performs 'testable and safe' as an assumed joint property rather than establishing it. Accessibility collapse is low (0.35) because genuine alternatives exist and are actively pursued — the inconsistency reading, the mirror-sector reading, and outright abandonment of the composite program are all live options, unlike a true mountain where alternatives have vanished. Resistance is moderate-high (0.55) reflecting active pushback from consistency critics.
 *
 * PERSPECTIVAL GAP:
 *   From the research-program seat, the survival bind is treated as an engineering detail to be worked out within viable parameter space — a solvable technical problem, not a structural threat. From the excluded consistency-critic seat, the bind is a symptom that the underlying sector may not exist in consistent form at all, making the entire testability program premature. The engine should compute these as structurally different classifications from the same base data: the beneficiaries' seat reads as ordinary (perhaps rope-like) ongoing research; the joint-victim seat reads as bearing snare-like extraction of credibility against an undemonstrated safety property.
 *
 * DIRECTIONALITY LOGIC:
 *   The joint victim (hypothesis credibility + ordinary-matter survival) is not a conventional agent but the structural casualty of the bind — it bears the cost regardless of which quantitative outcome (safe-and-testable vs. unsafe vs. untestable) obtains, so it is placed at trapped/analytical exit with civilizational time horizon: there is no escape from the joint fate once the coupling value is fixed by Dirac quantization. The two beneficiary groups are differentiated by exit quality: the theoretical research program has mobile exit (can retreat to nonminimal model variants), while the observational program has arbitrage-grade exit (null results and technique development remain valuable outputs regardless of the hypothesis's fate) — this is why the observational advocates are the less exposed beneficiary despite drawing on the same unresolved bind.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (disciplining coupling-value tuning against the brute fact of planetary/solar survival) remains live rather than resolved-and-vestigial — this is not mandatrophy in the classic sense of an arrangement outliving its function. Rather, the bind persists as unresolved tension: it has never been definitively shown either satisfiable or unsatisfiable, so the arrangement (continuing to pursue signatures while deferring the safety demonstration) survives by virtue of the question staying open, not by virtue of having been answered. The distinction from mandatrophy is important: mandatrophy names a function that died while the form persisted; here the function (real quantitative accountability) has never yet been performed, which is a different and arguably more serious failure mode — a credibility snare rather than an inertial piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    s_x_sign_determinability,
    'Can the sign and magnitude of the mixed-sector separation energy S_X(A,Z) be computed from first principles (or reliably bounded) in a way that jointly satisfies both the signature-generation requirement and the passivation/fission-limiting requirement, or is this a case where no consistent parameter choice satisfies both simultaneously?',
    'Detailed nonperturbative calculation of the droplet equation of state across the relevant coupling range, cross-checked against Psyche mascon predictions and solar core stability bounds over Gyr integration times; absence of any viable window after thorough search would itself be strong evidence against the hypothesis.',
    'If no consistent window exists, the entire mixed-sector observational program collapses regardless of the existence question — this would convert the current ''live and contested'' status into a decisive negative result independent of the QFT-consistency debate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(s_x_sign_determinability, empirical, 'Whether testability and self-limitation can be jointly satisfied by any S_X(A,Z) value.').

omega_variable(
    post_hoc_versus_designed_constraint,
    'Is the survival-bind requirement functioning as a genuine design constraint actively shaping model parameter choices, or is it invoked only as a post-hoc consistency check applied after signatures are proposed?',
    'Review of the model-building literature''s chronology: were self-limitation calculations performed before or after specific observational predictions (Psyche mascons, CNO anomalies) were proposed?',
    'If post-hoc, the extractiveness reading strengthens (credibility drawn from testability claims made prior to safety demonstration); if designed-in from the start, the constraint functions closer to genuine coordination discipline and the claimed snare type would be weaker than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_hoc_versus_designed_constraint, empirical, 'Whether the bind disciplines model-building in practice or is applied retroactively.').

omega_variable(
    conditional_dependence_on_existence_kernel,
    'Given that this story''s victim and beneficiary structure is entirely conditioned on some matter-sector reading (nonperturbative_matter_sector_reading or phenomenological_program_reading) being granted, how should the classification here be weighted if the sibling inconsistency_reading turns out to be correct?',
    'Track resolution of the alpha_m_supercriticality_kernel''s contested readings; if inconsistency_reading is formally vindicated (a no-go theorem is established for the minimal shared-photon/shared-color content), this constraint''s entire subject matter becomes counterfactual.',
    'A vindicated inconsistency_reading would not change this story''s authored ε/classification retroactively (per ε-invariance, this story is about the standing arrangement under contest by its own reading''s lights) but would make the practical stakes of resolving s_x_sign_determinability moot.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_dependence_on_existence_kernel, conceptual, 'This constraint''s relevance is conditional on the outcome of a separate, linked existence dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mixed_sector_survival_bind, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mixe_tr_t0, mixed_sector_survival_bind, theater_ratio, 0, 0.4).
narrative_ontology:measurement_basis(mixe_tr_t0, observed).
narrative_ontology:measurement(mixe_tr_t2, mixed_sector_survival_bind, theater_ratio, 2, 0.46).
narrative_ontology:measurement_basis(mixe_tr_t2, observed).
narrative_ontology:measurement(mixe_tr_t4, mixed_sector_survival_bind, theater_ratio, 4, 0.5).
narrative_ontology:measurement_basis(mixe_tr_t4, observed).
narrative_ontology:measurement(mixe_tr_t6, mixed_sector_survival_bind, theater_ratio, 6, 0.53).
narrative_ontology:measurement_basis(mixe_tr_t6, projected).
narrative_ontology:measurement(mixe_tr_t8, mixed_sector_survival_bind, theater_ratio, 8, 0.56).
narrative_ontology:measurement_basis(mixe_tr_t8, projected).
narrative_ontology:measurement(mixe_tr_t10, mixed_sector_survival_bind, theater_ratio, 10, 0.58).
narrative_ontology:measurement_basis(mixe_tr_t10, projected).

% Extraction over time
narrative_ontology:measurement(mixe_be_t0, mixed_sector_survival_bind, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(mixe_be_t0, observed).
narrative_ontology:measurement(mixe_be_t2, mixed_sector_survival_bind, base_extractiveness, 2, 0.59).
narrative_ontology:measurement_basis(mixe_be_t2, observed).
narrative_ontology:measurement(mixe_be_t4, mixed_sector_survival_bind, base_extractiveness, 4, 0.62).
narrative_ontology:measurement_basis(mixe_be_t4, observed).
narrative_ontology:measurement(mixe_be_t6, mixed_sector_survival_bind, base_extractiveness, 6, 0.65).
narrative_ontology:measurement_basis(mixe_be_t6, projected).
narrative_ontology:measurement(mixe_be_t8, mixed_sector_survival_bind, base_extractiveness, 8, 0.67).
narrative_ontology:measurement_basis(mixe_be_t8, projected).
narrative_ontology:measurement(mixe_be_t10, mixed_sector_survival_bind, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(mixe_be_t10, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(mixed_sector_survival_bind, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mixed_sector_survival_bind, resource_allocation).
narrative_ontology:boltzmann_floor_override(mixed_sector_survival_bind, 0.1).
narrative_ontology:affects_constraint(mixed_sector_survival_bind, alpha_m_supercriticality_kernel_nonperturbative_matter_sector_reading).
narrative_ontology:affects_constraint(mixed_sector_survival_bind, alpha_m_supercriticality_kernel_phenomenological_program_reading).

% DUAL FORMULATION NOTE:
% This constraint is structurally downstream of and conditioned on the alpha_m_supercriticality_kernel's existence readings, but is NOT itself a reading of that kernel — it authors a separate quantitative bind (on S_X(A,Z) sign/magnitude and droplet EOS) that arises only once some matter-sector reading is granted. Decomposed per the epsilon-invariance principle: the existence question and the survival-bind question have different epsilon referents (theoretical consistency vs. empirical safety-and-testability co-satisfiability) and should not be collapsed into one story's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
