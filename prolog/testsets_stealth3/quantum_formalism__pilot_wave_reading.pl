% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__pilot_wave_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: quantum_formalism__pilot_wave_reading
 *   human_readable: Pilot-Wave Reading of the Quantum Formalism (de Broglie-Bohm Guided-Particle Ontology)
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the quantum-formalism kernel: the
 *   pilot-wave (de Broglie-Bohm) reading, on which particles always possess
 *   definite positions guided by an ontologically real wavefunction field,
 *   evolution is deterministic, measurement reveals pre-existing values, and
 *   the observer is eliminable. The epsilon referent is the standing
 *   arrangement under contest — the pilot-wave reading as a maintained
 *   interpretive commitment governing the Bohmian research community and its
 *   interface with the foundations field — assessed by the reading's own
 *   lights (realism, determinism, and explanatory closure as goods;
 *   positivist scruples and nonlocality as prices). It is NOT the Copenhagen
 *   settlement the reading opposes, and NOT the fully vindicated ontology the
 *   reading anticipates. Claim and metrics are independent authored facts:
 *   the claimed type (tangled_rope) states what I believe structurally true —
 *   a genuine coordination function joined to asymmetric cost-bearing under
 *   active enforcement — while the metrics describe the arrangement's actual
 *   operation; the engine computes per-seat classifications from the
 *   structural data, and divergence between claim and computed type is
 *   signal, not error. Per the epsilon-invariance principle, 'interpretation
 *   of quantum mechanics' decomposes into three structurally distinct stories
 *   (this file, copenhagen_reading, many_worlds_reading), linked via
 *   network.affects_constraints; each carries its own epsilon, beneficiaries,
 *   and victims. Receipt surface: the extraction the arrangement generates
 *   (defensive labor, career risk, deferred empirical payoff) demonstrably
 *   accrues to the senior theorists' seat as lineage authority, editorial
 *   control, and citation centrality, so gain_flow names that seat;
 *   fixing_cost is prohibitive because the actors positioned to liberalize
 *   the canon would pay for the fix with the identity and distinctiveness
 *   that constitute their standing.
 *
 * KEY AGENTS:
 *   - bohmian_senior_theorists: Agenda setter + primary beneficiary (organized/identity_locked) — administers the canon, runs the venues, collects the status returns
 *   - junior_foundation_researchers: Primary target (moderate/constrained) — bears defensive labor and career risk
 *   - quantum_nonequilibrium_experimentalists: Secondary target (moderate/constrained) — bears the perpetually deferred empirical payoff
 *   - copenhagen_operationalists: Excluded mainstream (institutional/arbitrage) — neither pays nor collects; declines engagement from institutional height
 *   - realist_philosophers_of_physics: Secondary beneficiary (organized/mobile) — collects argumentative resources without administering anything
 *   - ontology_seeking_graduate_students: Incidental beneficiary (powerless/mobile) — collects conceptual relief transiently
 *   - science_studies_analysts: Analytical observer (analytical/analytical) — sees the full structure from no partisan seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.42).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.34).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot-Wave Reading of the Quantum Formalism (de Broglie-Bohm Guided-Particle Ontology)").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '8f0811b3-6aca-434c-bff2-2a1445d4c680').
narrative_ontology:cs_kernel_codification('8f0811b3-6aca-434c-bff2-2a1445d4c680', formalized).
narrative_ontology:cs_authority_grounding('8f0811b3-6aca-434c-bff2-2a1445d4c680', expertise).
narrative_ontology:cs_interpretation_layer_present('8f0811b3-6aca-434c-bff2-2a1445d4c680').
narrative_ontology:cs_reading_relation('8f0811b3-6aca-434c-bff2-2a1445d4c680', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('8f0811b3-6aca-434c-bff2-2a1445d4c680', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_axiom('8f0811b3-6aca-434c-bff2-2a1445d4c680', foundational, definite_particle_positions_at_all_times).
narrative_ontology:cs_axiom_status(definite_particle_positions_at_all_times, holdable).
narrative_ontology:cs_axiom_grounding('8f0811b3-6aca-434c-bff2-2a1445d4c680', definite_particle_positions_at_all_times, instrumental).
narrative_ontology:cs_axiom('8f0811b3-6aca-434c-bff2-2a1445d4c680', foundational, wavefunction_physical_field_in_configuration_space).
narrative_ontology:cs_axiom_status(wavefunction_physical_field_in_configuration_space, holdable).
narrative_ontology:cs_axiom_grounding('8f0811b3-6aca-434c-bff2-2a1445d4c680', wavefunction_physical_field_in_configuration_space, empirically_contingent).
narrative_ontology:cs_axiom('8f0811b3-6aca-434c-bff2-2a1445d4c680', secondary, local_hidden_variables_suffice).
narrative_ontology:cs_axiom_status(local_hidden_variables_suffice, overridden).
narrative_ontology:cs_axiom_grounding('8f0811b3-6aca-434c-bff2-2a1445d4c680', local_hidden_variables_suffice, empirically_contingent).
narrative_ontology:cs_reference_frame('8f0811b3-6aca-434c-bff2-2a1445d4c680', deterministic_guided_particle_ontology).
narrative_ontology:cs_drift_state('8f0811b3-6aca-434c-bff2-2a1445d4c680', contemporary_post_bell_consolidation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8f0811b3-6aca-434c-bff2-2a1445d4c680', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_senior_theorists).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, realist_philosophers_of_physics).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, ontology_seeking_graduate_students).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, junior_foundation_researchers).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, quantum_nonequilibrium_experimentalists).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, von_neumann_impossibility_refutation).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, kochen_specker_contextuality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the program's canonical line — which formulations count as Bohmian mechanics, which derivations of the Born rule are acceptable, which objections require answer. They run the specialized journals, summer schools, and conference tracks where the reading is maintained. Their careers, citation networks, and intellectual identities are fused with the program; leaving would mean abandoning the body of work that constitutes their standing. They collect the program's status returns: lineage authority, editorial control, and the standing of having kept a realist alternative alive.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_senior_theorists, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, bohmian_senior_theorists, beneficiary).

% Enter the program as graduate students and postdocs drawn by its clarity. They bear its defensive labor — answering no-go objections, writing the rebuttal literature — and carry its career risk: hiring committees in mainstream departments discount Bohmian specialization, and the program's permanent positions concentrate at a handful of centers. Exit to mainstream topics is possible but costs retraining, lost citations, and severed mentorship ties.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, junior_foundation_researchers, payer,
    moderate, biographical, constrained, global).

% Design and pursue tests of quantum nonequilibrium — relic cosmological particles, astrophysical photon statistics — that would decisively distinguish the reading from standard quantum mechanics. The reading's equilibrium structure places the decisive regime perpetually out of reach, so grant cycles and career timelines are spent on proposals whose confirmation recedes. Returning to conventional atomic or condensed-matter experimentation is possible but writes off accumulated specialized expertise.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, quantum_nonequilibrium_experimentalists, payer,
    moderate, biographical, constrained, national).

% Control the mainstream venues — textbooks, funding panels, general journals — and treat the interpretive dispute as settled or meaningless, dismissing the reading without sustained engagement. They pay nothing to the reading's maintenance and collect nothing from it; their operational practice is untouched by it. From their seat the constraint barely registers; they would object to its ontological claims if compelled to engage, but engagement is precisely what their institutional position lets them decline.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, copenhagen_operationalists, excluded,
    institutional, generational, arbitrage, global).

% Use the reading as a working existence proof that quantum phenomena admit a realist, deterministic, observer-independent account. They collect argumentative resources — a counterexample to folklore impossibility proofs, a case study in theory underdetermination — without administering the program and without bearing its internal defensive labor. Their disciplinary home in philosophy gives them exit mobility the physics-side members lack.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, realist_philosophers_of_physics, beneficiary,
    organized, generational, mobile, global).

% Adopt the reading because it restores an intelligible, observer-independent world picture. They collect conceptual relief and a usable framework for thinking about measurement, without administering anything; most never specialize in foundations, so their exposure is transient and their exit trivial.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, ontology_seeking_graduate_students, beneficiary,
    powerless, immediate, mobile, global).

% Track the reading's sociological trajectory — presentation at Solvay, dormancy, revival, consolidation, marginalization — from outside any partisan seat. They neither maintain nor oppose the program, and their analyses feed no enforcement mechanism on either side.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, science_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a realist research community around a shared ontology (definite particles plus a guiding field), a common mathematical core (guidance equation plus Schrödinger evolution), and a shared problem agenda — typicality derivations of the Born rule, relativistic extensions, nonequilibrium phenomenology — solving the collective-action problem of sustaining a coherent realist alternative without fragmenting into idiosyncratic variants.
% TRANSFER_FUNCTION: Moves epistemic labor and career risk from junior researchers and allied experimentalists toward maintenance of the canonical line; moves status, lineage authority, and citation centrality to the senior theorists who administer it; moves attention from the broader foundations community, which must periodically adjudicate the reading's no-go challenges.
% ABSENT_VOICES: The operationalist mainstream is structurally absent from the program's internal discourse — it dismisses rather than engages, so its objections enter only as ambient pressure, never as answered argument. Experimentalists capable of designing decisive tests are largely absent from the theory-internal agenda-setting that decides which problems count. Both would object to the canon's priorities if seated; their absence is what lets the defensive agenda dominate the research agenda.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the realism-determinism pole of the foundations debate loses its anchor existence proof; Bell-test pedagogy loses its clearest foil for teaching what nonlocality does and does not imply; a small but productive research community disperses and its open problems (relativistic extension, nonequilibrium phenomenology) lose their custodians; and the mainstream's 'hidden variables are impossible' folklore would stand unchallenged. Arrangements across several communities depend on it.
% FOUNDING_PROBLEM: The measurement problem and the apparent collapse of classical ontology: the quantum formalism seemed to deny definite particle trajectories between measurements and to make observation primitive. De Broglie (1927) and Bohm (1952) built this reading to restore definite positions, determinism, and an observer-independent world without changing the predictions.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: mainstream graduate texts and Copenhagen-sympathetic reviewers concede the measurement problem is unresolved, and the problem's reality was attested by Bell in work predating his program advocacy. What the mainstream disputes is whether the pilot-wave solution's costs — nonlocality, surplus ontology, empirical equivalence — are worth paying, not whether the founding problem exists. No corroborating source claims the problem is solved by the standard treatment.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__pilot_wave_reading_tests).
:- end_tests(quantum_formalism__pilot_wave_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42): the arrangement delivers real goods — a coherent realist ontology, a determinate measurement account, a live problem agenda (typicality derivations, relativistic extensions, nonequilibrium phenomenology) — while imposing real costs: defensive labor against no-go objections, career risk from mainstream discounting, and the locked-in nonlocality concession. Suppression is moderate-low (0.34): within the community the canonical line is actively policed (collapse talk and observer-centrism are out of bounds, heterodox variants are marginalized), but the arrangement cannot suppress alternatives field-wide and exit, though costly, exists. Theater is moderate-low (0.28): anniversary events, lineage veneration, and restatement literature are present, but the core theoretical production is functional — this is not a piton profile. Accessibility collapse is mid-range (0.52): accepting the reading's premises forecloses observer-centered accounts from within, yet the rival readings remain conceptually and socially available. Resistance is high (0.68): the mainstream's institutional weight bears on the program continuously. The temporal series run on ONE shared nine-point grid (every tracked metric authored at every point). The series show the arrangement's real history: near-dormancy around T=13 (de Broglie's abandonment), revival discontinuity at T=25 (Bohm 1952, suppression requirement spiking as defense machinery is built under fire), post-Bell legitimation, school consolidation peaking enforcement around T=61, and gradual normalization thereafter. Suppression_requirement is tracked because enforcement capacity genuinely changed across the interval — collapse, rebuild, ratchet, partial relaxation — not merely shifted extraction; the end-state value matches the base_properties scalar.
 *
 * PERSPECTIVAL GAP:
 *   Four seats compute differently from the same structural data. From the senior theorists' seat (agenda_setter, identity_locked) the arrangement is home: the canon they administer is the price of the coherence they enjoy, and exit is unthinkable because their standing IS the program. From the junior researchers' seat (payer, constrained) the same structure operates as levied labor: they produce the rebuttal literature and carry the hiring-market penalty while the status returns concentrate above them. From the experimentalists' seat (payer, constrained) the structure appears as perpetual deferral — the equilibrium condition that protects the theory also places decisive tests permanently out of reach. From the copenhagen_operationalists' seat (excluded, arbitrage) the constraint barely registers at all: their operational practice is untouched, their venues are elsewhere, and engagement is a cost they can simply decline. The engine computes this divergence from role, power, and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for bohmian_senior_theorists, realist_philosophers_of_physics, and ontology_seeking_graduate_students; victim declarations drive high directionality for junior_foundation_researchers and quantum_nonequilibrium_experimentalists, amplified for the juniors by their constrained exit and for the experimentalists by their sunk specialized investment. The copenhagen_operationalists seat requires an override: they carry the institutional power atom but declare no beneficiary or victim relation, and the canonical fallback for that atom would impute stakes they do not have — they are neither subsidized nor taxed by this arrangement, so d is overridden to 0.5 (symmetric indifference). Identity-lock modulation applies to the senior theorists: their fused identity pushes them toward the beneficiary end regardless of the defensive costs they personally bear, which is why the extraction they administer lands on seats below them rather than on themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an observer-independent ontology consistent with the quantum predictions — remains live by broad cross-school attestation, so the mandate has not outlived its function and no mandatrophy resolution is declared. The tangled_rope classification prevents two opposite mislabelings. Calling the arrangement a snare would erase its genuine coordination function: a real research agenda, real explanatory goods, and voluntary membership with costly-but-real exit. Calling it a rope would erase the asymmetric extraction: the same structure that coordinates the community levies its defensive and deferred-payoff costs on the seats least able to decline, while status returns concentrate at the administering seat. A piton reading fails on the evidence: the function is alive (active theoretical production, theater_ratio 0.28, no cost-asymmetry of the administrator-could-fix-but-won't-bears-nothing shape — the administrators bear real identity costs and deliver real goods). The identity_coordination typing carries a known gaming risk — identity narratives are classic cover stories for extraction — and the guard here is the declared victim set: the coupling concentrates career costs on powerless-to-moderate seats at global scope, which the Boltzmann coupling test should flag for review rather than excuse via the complexity offset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which structural elements of this constraint are fixed by the shared quantum formalism (the kernel) and which are contributed by the pilot-wave reading specifically?',
    'Cross-reading comparison across the three sibling story files: elements invariant across copenhagen_reading, many_worlds_reading, and this reading belong to the kernel; elements that vary belong to the reading.',
    'Misattribution would charge this reading''s extraction and victim structure to the formalism, or vice versa; the sibling readings instantiate different victim sets (Copenhagen: no definite pre-measurement values; Many-Worlds: no single definite outcome), so per-seat classifications differ file by file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-versus-kernel attribution of the constraint''s structural elements.').

omega_variable(
    empirical_equivalence_shield,
    'Is the reading''s practical unfalsifiability in quantum equilibrium a permanent structural property of the arrangement or a temporary technological limit?',
    'Detection of quantum-nonequilibrium signatures (relic cosmological particle statistics, astrophysical interference anomalies) or a proof that quantum equilibrium is exceptionless.',
    'If nonequilibrium proves detectable, the program converts from defended enclave to testable frontier and the extraction profile (junior defensive labor, perpetually deferred experimental payoff) collapses; if equilibrium is exceptionless, the empirical shield hardens and suppression requirements rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_equivalence_shield, empirical, 'Whether the equilibrium equivalence that protects the reading is structural or technological.').

omega_variable(
    discovered_vs_constructed_ontology,
    'Is the guided-particle structure a discovered feature of reality (in which case the arrangement approaches a natural-law limit) or a constructed interpretive commitment maintained by a community?',
    'Decisive nonequilibrium evidence would shift the balance toward discovered; continued empirical equivalence combined with observable sociological maintenance (lineage events, canon enforcement) indicates constructed.',
    'If discovered, the extraction framing partially dissolves — the costs become the price of tracking reality and any future mountain claim gains warrant; if constructed, the tangled_rope analysis stands and any natural-law framing of the reading is a false summit candidate requiring FSM scrutiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovered_vs_constructed_ontology, conceptual, 'Natural-law versus constructed-commitment status of the guided-particle ontology.').

omega_variable(
    suppression_mechanism_split,
    'Is the suppression that keeps the community''s canonical line unified structural (mainstream-controlled career and funding incentives) or internalized (Bohmian identity rendering pragmatic dilution unthinkable)?',
    'Post-exit trajectory study of researchers who leave the program: if heterodox or pragmatic positions recur among leavers, suppression was structural; if leavers retain orthodox commitments after exit, suppression was internalized.',
    'If internalized, effective suppression exceeds the structural measure — leavers carry the constraint with them; if structural, mainstream institutional reform (hiring and funding pluralism) would dissolve most of it without touching the reading''s content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized split of the community-unifying suppression.').

omega_variable(
    identity_lock_inflation,
    'Does the senior theorists'' identity fusion inflate or deflate the net extraction borne by the program''s members?',
    'Comparison with counterfactual program structures lacking concentrated lineage authority (decentralized collaborations): measure how career-cost distribution shifts when identity rewards are unavailable.',
    'If identity lock inflates extraction, breaking the lineage frame (for example, decisive evidence forcing merger with mainstream practice) redistributes costs downward; if it deflates extraction (identity rewards offsetting career costs), the tangled_rope verdict understates net member benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_inflation, empirical, 'Direction of identity-fusion effect on the program''s net extraction profile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 0, 96).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__pilot_wave_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(quan_tr_t0, observed).
narrative_ontology:measurement(quan_tr_t13, quantum_formalism__pilot_wave_reading, theater_ratio, 13, 0.05).
narrative_ontology:measurement_basis(quan_tr_t13, observed).
narrative_ontology:measurement(quan_tr_t25, quantum_formalism__pilot_wave_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement_basis(quan_tr_t25, observed).
narrative_ontology:measurement(quan_tr_t37, quantum_formalism__pilot_wave_reading, theater_ratio, 37, 0.22).
narrative_ontology:measurement_basis(quan_tr_t37, observed).
narrative_ontology:measurement(quan_tr_t49, quantum_formalism__pilot_wave_reading, theater_ratio, 49, 0.25).
narrative_ontology:measurement_basis(quan_tr_t49, observed).
narrative_ontology:measurement(quan_tr_t61, quantum_formalism__pilot_wave_reading, theater_ratio, 61, 0.3).
narrative_ontology:measurement_basis(quan_tr_t61, observed).
narrative_ontology:measurement(quan_tr_t73, quantum_formalism__pilot_wave_reading, theater_ratio, 73, 0.33).
narrative_ontology:measurement_basis(quan_tr_t73, observed).
narrative_ontology:measurement(quan_tr_t85, quantum_formalism__pilot_wave_reading, theater_ratio, 85, 0.31).
narrative_ontology:measurement_basis(quan_tr_t85, observed).
narrative_ontology:measurement(quan_tr_t96, quantum_formalism__pilot_wave_reading, theater_ratio, 96, 0.28).
narrative_ontology:measurement_basis(quan_tr_t96, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__pilot_wave_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(quan_be_t0, observed).
narrative_ontology:measurement(quan_be_t13, quantum_formalism__pilot_wave_reading, base_extractiveness, 13, 0.1).
narrative_ontology:measurement_basis(quan_be_t13, observed).
narrative_ontology:measurement(quan_be_t25, quantum_formalism__pilot_wave_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement_basis(quan_be_t25, observed).
narrative_ontology:measurement(quan_be_t37, quantum_formalism__pilot_wave_reading, base_extractiveness, 37, 0.38).
narrative_ontology:measurement_basis(quan_be_t37, observed).
narrative_ontology:measurement(quan_be_t49, quantum_formalism__pilot_wave_reading, base_extractiveness, 49, 0.4).
narrative_ontology:measurement_basis(quan_be_t49, observed).
narrative_ontology:measurement(quan_be_t61, quantum_formalism__pilot_wave_reading, base_extractiveness, 61, 0.44).
narrative_ontology:measurement_basis(quan_be_t61, observed).
narrative_ontology:measurement(quan_be_t73, quantum_formalism__pilot_wave_reading, base_extractiveness, 73, 0.46).
narrative_ontology:measurement_basis(quan_be_t73, observed).
narrative_ontology:measurement(quan_be_t85, quantum_formalism__pilot_wave_reading, base_extractiveness, 85, 0.44).
narrative_ontology:measurement_basis(quan_be_t85, observed).
narrative_ontology:measurement(quan_be_t96, quantum_formalism__pilot_wave_reading, base_extractiveness, 96, 0.42).
narrative_ontology:measurement_basis(quan_be_t96, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__pilot_wave_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(quan_su_t0, observed).
narrative_ontology:measurement(quan_su_t13, quantum_formalism__pilot_wave_reading, suppression_requirement, 13, 0.05).
narrative_ontology:measurement_basis(quan_su_t13, observed).
narrative_ontology:measurement(quan_su_t25, quantum_formalism__pilot_wave_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(quan_su_t25, observed).
narrative_ontology:measurement(quan_su_t37, quantum_formalism__pilot_wave_reading, suppression_requirement, 37, 0.48).
narrative_ontology:measurement_basis(quan_su_t37, observed).
narrative_ontology:measurement(quan_su_t49, quantum_formalism__pilot_wave_reading, suppression_requirement, 49, 0.52).
narrative_ontology:measurement_basis(quan_su_t49, observed).
narrative_ontology:measurement(quan_su_t61, quantum_formalism__pilot_wave_reading, suppression_requirement, 61, 0.62).
narrative_ontology:measurement_basis(quan_su_t61, observed).
narrative_ontology:measurement(quan_su_t73, quantum_formalism__pilot_wave_reading, suppression_requirement, 73, 0.58).
narrative_ontology:measurement_basis(quan_su_t73, observed).
narrative_ontology:measurement(quan_su_t85, quantum_formalism__pilot_wave_reading, suppression_requirement, 85, 0.55).
narrative_ontology:measurement_basis(quan_su_t85, observed).
narrative_ontology:measurement(quan_su_t96, quantum_formalism__pilot_wave_reading, suppression_requirement, 96, 0.34).
narrative_ontology:measurement_basis(quan_su_t96, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, identity_coordination).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, many_worlds_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'interpretation of quantum mechanics' conflates three structurally distinct constraints and is decomposed per the epsilon-invariance principle into three linked stories sharing the quantum_formalism kernel: copenhagen_reading, many_worlds_reading, and this file (pilot_wave_reading). Each has its own epsilon, beneficiary/victim structure, and enforcement profile; none hedges across the others. The upstream shared element is the formalism's empirical core, cited by each reading as evidence for its own downstream claims; the readings differ on wavefunction ontology and outcome definiteness, which is where their victim sets diverge. This file links both siblings; orphan status would indicate a failed decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
