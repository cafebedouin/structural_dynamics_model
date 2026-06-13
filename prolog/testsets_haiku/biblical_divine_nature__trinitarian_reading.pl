% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__trinitarian_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: biblical_divine_nature__trinitarian_reading
 *   human_readable: Trinitarian Ousia-Hypostasis Doctrine (Nicene Reading)
 *   domain: theological/institutional_authority
 *
 * SUMMARY:
 *   The Trinitarian reading of the biblical divine nature holds that God
 *   consists of three hypostases (persons: Father, Son, Holy Spirit) who
 *   share a single ousia (essence), thereby preserving monotheism through
 *   essence-unity rather than numerical singularity. This reading was
 *   codified by the Council of Nicaea (325 CE) and Constantinople (381 CE) as
 *   the binding orthodoxy of Christendom. It is enforced through anathema
 *   (excommunication), property seizure, exile, and execution of rival
 *   theologians. The constraint is structured as a tangled rope: genuine
 *   coordination function (solving the coherence problem of how to hold
 *   together scriptural claims of singular divinity with threefold action)
 *   coupled with asymmetric extraction (institutional authority and imperial
 *   power flowing to orthodox bishops and theologians; exclusion,
 *   persecution, and intellectual suppression flowing to Arian, Unitarian,
 *   and Modalist communities). The constraint's persistence depends on active
 *   enforcement; its justification is both the coordination function and the
 *   claim (contested) that the founding problem remains live. This is ONE
 *   reading of the contested kernel biblical_divine_nature. Sibling readings
 *   (unitarian_reading, modalist_reading) are separate constraint stories,
 *   not alternatives within this one.
 *
 * KEY AGENTS:
 *   - orthodox_institutional_authority: Sets, codifies, and enforces the Trinitarian doctrine through ecumenical councils and imperial law; benefits from unified orthodoxy and control of doctrinal interpretation
 *   - trinitarian_theologians: Interpret, defend, and refine the ousia-hypostasis distinction; receive patronage, prestige, and intellectual authority from the institutional church
 *   - arian_communities: Hold the Son to be created and subordinate; are declared heretical, suppressed, and progressively eliminated from Christian Christendom
 *   - unitarian_communities: Maintain numerical monotheism (Father alone is God); are excluded, executed, and exiled; identity is locked to rational exegetical practice
 *   - modalist_communities: Interpret the three persons as sequential modes of one divine subject; face councils' suppression and modern evangelical exclusion
 *   - imperial_authority: Convenes councils and enforces doctrine through state power; benefits from unified religious foundation for political legitimacy
 *   - jewish_monotheists: Maintain strict numerical monotheism outside Christendom; serve as the silent referent that makes Trinitarianism distinctive and contestable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.72).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.81).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Trinitarian Ousia-Hypostasis Doctrine (Nicene Reading)").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theological/institutional_authority").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, '83ef3075-db43-4b9b-aacc-6ad880af5e83').
narrative_ontology:cs_kernel_codification('83ef3075-db43-4b9b-aacc-6ad880af5e83', fixed_text).
narrative_ontology:cs_authority_grounding('83ef3075-db43-4b9b-aacc-6ad880af5e83', extraction).
narrative_ontology:cs_interpretation_layer_present('83ef3075-db43-4b9b-aacc-6ad880af5e83').
narrative_ontology:cs_reading_relation('83ef3075-db43-4b9b-aacc-6ad880af5e83', biblical_divine_nature__unitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('83ef3075-db43-4b9b-aacc-6ad880af5e83', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('83ef3075-db43-4b9b-aacc-6ad880af5e83', foundational, three_simultaneous_hypostases).
narrative_ontology:cs_axiom_status(three_simultaneous_hypostases, holdable).
narrative_ontology:cs_axiom_grounding('83ef3075-db43-4b9b-aacc-6ad880af5e83', three_simultaneous_hypostases, deontological).
narrative_ontology:cs_axiom('83ef3075-db43-4b9b-aacc-6ad880af5e83', foundational, single_ousia_essential_unity).
narrative_ontology:cs_axiom_status(single_ousia_essential_unity, holdable).
narrative_ontology:cs_axiom_grounding('83ef3075-db43-4b9b-aacc-6ad880af5e83', single_ousia_essential_unity, deontological).
narrative_ontology:cs_reference_frame('83ef3075-db43-4b9b-aacc-6ad880af5e83', apostolic_trinitarian_revelation).
narrative_ontology:cs_drift_state('83ef3075-db43-4b9b-aacc-6ad880af5e83', contemporary_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('83ef3075-db43-4b9b-aacc-6ad880af5e83', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, orthodox_institutional_authority).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, trinitarian_theologians).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, arian_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, unitarian_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, modalist_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, jewish_monotheists).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__trinitarian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__trinitarian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__trinitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__trinitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 terminal, rising to 0.78 at medieval apex, decaying to 0.52 in modernity) because institutional authority captures significant benefits (power, resources, interpretive monopoly) and visitor sets (Arians, Unitarians, Modalists) bear costs (exclusion, suppression, execution) decoupled from the services the constraint provides. Suppression is higher (0.81 terminal, peaking at 0.88 in medieval period when enforcement is most intensive and coercive) because the constraint's persistence depends on active enforcement of anathema and legal suppression, not participant preference. Theater ratio (0.48 terminal, rising from 0.25 at Nicaea to 0.62 in modern period) shows the pattern of institutional constraint inertia: in the medieval and Reformation periods, the suppressive machinery is running hot against live rival doctrines; by modernity, the constraint's function has atrophied (Unitarians, Evangelicals, and Oneness Pentecostals exist openly without state backing), but the constraint persists through institutional theater and intellectual tradition maintenance. The measurement grid shares a single time axis so every metric is authored at every examined time point. Decay in suppression and extractiveness after 1800 reflects the Enlightenment, secularization, and the rise of denominational pluralism (U.S. Constitution disestablishment, European secularization); decay in theater ratio inverts because once the suppressive machinery weakens, the constraint's remaining function becomes increasingly performative rather than functionally coercive.
 *
 * PERSPECTIVAL GAP:
 *   Perspective divergence is severe and follows power lines. From the orthodox institutional seat (councils, bishops, imperial authority), the constraint is genuine coordination: a coherent solution to the problem of reconciling scriptural monotheism with trinitarian action. Trinitarianism is presented as the only rational reading consistent with scripture and the apostolic tradition. From the victim seats (Arian, Unitarian, Modalist communities), the same constraint is pure coercion: a doctrinal imposition enforced by power, not by argument; the constraint persists because institutional authority has the muscle to suppress alternatives, not because Trinitarianism is coherent. The engine should compute this divergence from the structural data: the institutional seat should compute as rope (genuine coordination, low extraction perceived locally); the victim seats should compute as snare (extraction decoupled from any service, high suppression). This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Orthodox institutional authority and trinitarian theologians are structural beneficiaries: they hold the interpretive monopoly, receive institutional patronage, and define legitimate Christian identity. Their directionality is low (near beneficiary end, d ≈ 0.1–0.25). Arian, Unitarian, and Modalist communities are structural targets: they are suppressed, excluded, and bear the cost of conforming or enduring persecution. Their directionality is high (near target end, d ≈ 0.75–0.95). The empirical constraint on d comes from exit options: trinitarian theologians have arbitrage-grade exit (they can recant and join Unitarian or Modalist movements, though career-ending); Unitarian and Modalist victims are identity-locked (exit means reconstructing one's entire theological self-understanding and social identity). Imperial authority sits ambiguously: they benefit from unified orthodoxy (low d in terms of extraction), but the constraint is instrumentally maintained by their power rather than voluntarily held, suggesting a moderate d near symmetric (d ≈ 0.4–0.6). No directionality overrides are needed; the structural derivation captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolves the key tension: Is this constraint a genuine solution to a theological coordination problem, or a political imposition of doctrinal uniformity? The founding_problem_status is contested. The institutional authority attests the problem is perpetually live — doctrinal disagreement threatens unity. External observers (secular historians, Unitarian theologians, Reformation scholars) attest the founding problem is substantially solved by modernity: denominations now accept plurality without institutional collapse or state threat. The measurement series supports this reading: suppression_requirement decays from 0.88 (medieval period, when suppression is intensive and necessary to maintain orthodoxy) to 0.42 (modern period, when institutional pluralism is accepted and suppression is much lighter). This decay pattern is the signature of a constraint whose coordination function has atrophied while its extraction machinery persists. The theater_ratio rise (0.25 to 0.62) shows the constraint becoming increasingly performative: ecumenical councils continue, creeds are recited, but enforcement is cultural rather than legal. The constraint is NOT resolved as a false mountain — it is a genuine tangled rope with real coordination function and real asymmetric extraction. But the founding problem's attenuation and the rise in theater ratio flag the constraint as a candidate for what the modern era calls mandatrophy: the mandate (unifying Christendom through enforced Trinitarianism) has outlived its necessity (modernity has learned to tolerate doctrinal plurality without collapse), but the constraint persists through institutional inertia and continued low-level suppression of alternative readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_divinity,
    'Is the Trinitarian doctrine a discovery of natural theological truth (the way God actually is), or a human construction imposed by institutional power (a useful conceptual framework chosen for political cohesion)?',
    'This question is irreducible: it asks whether theology is metaphysical or phenomenological. Different traditions answer differently. Resolution would require agreement on whether theological truth is a matter of correspondence to reality (discovered) or coherence and institutional function (constructed). No empirical data settles this.',
    'If natural/discovered: the constraint is a mountain (the Trinity is real), and suppression of alternatives is justified as elimination of falsehood. If constructed: the constraint is a snare (institutional imposition), and suppression is unjustified coercion. The engine should flag this omega as permanently open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_divinity, conceptual, 'Whether Trinitarianism is metaphysical truth or institutional construction.').

omega_variable(
    founding_problem_persistence,
    'Does the problem the Trinitarian constraint was built to solve (doctrinal fragmentation threatening imperial unity) remain live, or has modernity solved it through secularization and denominational pluralism?',
    'Historical-sociological: track whether Christian communities report that Trinitarian unity is necessary for Christian coherence. If Unitarian, Pentecostal, and Evangelical communities thrive while rejecting or reinterpreting Trinitarianism, the founding problem is solved. If institutional pressure to conform Trinitarianism remains high, the problem persists.',
    'Problem live => constraint justified by coordination function. Problem dead => constraint is zombie (mandatrophy flag). The measurement series (suppression decay from 0.88 to 0.42; theater rise from 0.25 to 0.62) suggests the problem is dead or dying, supporting a mandatrophy classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem (doctrinal unity as state necessity) persists or has been solved.').

omega_variable(
    ousia_hypostasis_coherence,
    'Is the ousia-hypostasis distinction itself coherent, or does it merely relocate the problem (three hypostases seem to violate monotheism just as much as three gods, if they are truly three)?',
    'Philosophical analysis of the ousia-hypostasis formulation. Trinitarians claim the distinction is coherent (one ousia, three hypostases = one substance/essence, three persons/manifestations). Critics (Unitarians, Jews, modern philosophers) claim the distinction is equivocation or category confusion. Resolution requires agreement on whether the conceptual framework is logically sound.',
    'If coherent: the constraint''s coordination function is real (it genuinely solves the problem). If incoherent: the constraint''s justification is hollow, and its persistence is pure institutional inertia + theater. This affects whether the constraint computes as rope (coordination real) or snare (coordination fake, extraction disguised).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ousia_hypostasis_coherence, conceptual, 'Whether the ousia-hypostasis distinction coherently preserves monotheism or merely relocates the problem.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of Arian, Unitarian, and Modalist communities primarily structural (external legal and institutional barriers) or internalized (these communities have internalized the orthodox reading as correct and suppress themselves)?',
    'Post-suppression trajectory analysis: if communities experience persistent self-suppression after external enforcement mechanisms are removed (as happened with Arianism after barbarian kingdoms provided refuge), the suppression is partially internalized. If communities immediately resurrect and assert alternative readings once external pressure lifts (as happened with Unitarianism in Enlightenment), suppression was primarily structural.',
    'If internalized: the constraint''s effective suppression is higher than the measured structural suppression (victims carry the suppression with them). If structural: the measured suppression captures the constraint''s real coercive force. Internalizations suggest deep identity fusion with orthodox doctrine; structural suppression suggests external enforcement on bodies maintaining dissent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of rival readings is structural or has been internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__trinitarian_reading, theater_ratio, 325, 0.25).
narrative_ontology:measurement(bibl_tr_t500, biblical_divine_nature__trinitarian_reading, theater_ratio, 500, 0.35).
narrative_ontology:measurement(bibl_tr_t1000, biblical_divine_nature__trinitarian_reading, theater_ratio, 1000, 0.42).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__trinitarian_reading, theater_ratio, 1500, 0.48).
narrative_ontology:measurement(bibl_tr_t1800, biblical_divine_nature__trinitarian_reading, theater_ratio, 1800, 0.55).
narrative_ontology:measurement(bibl_tr_t2025, biblical_divine_nature__trinitarian_reading, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__trinitarian_reading, base_extractiveness, 325, 0.68).
narrative_ontology:measurement(bibl_be_t500, biblical_divine_nature__trinitarian_reading, base_extractiveness, 500, 0.75).
narrative_ontology:measurement(bibl_be_t1000, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1000, 0.78).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1500, 0.76).
narrative_ontology:measurement(bibl_be_t1800, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1800, 0.68).
narrative_ontology:measurement(bibl_be_t2025, biblical_divine_nature__trinitarian_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__trinitarian_reading, suppression_requirement, 325, 0.82).
narrative_ontology:measurement(bibl_su_t500, biblical_divine_nature__trinitarian_reading, suppression_requirement, 500, 0.88).
narrative_ontology:measurement(bibl_su_t1000, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1000, 0.85).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1500, 0.79).
narrative_ontology:measurement(bibl_su_t1800, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1800, 0.68).
narrative_ontology:measurement(bibl_su_t2025, biblical_divine_nature__trinitarian_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__trinitarian_reading, 0.12).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__modalist_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, nicene_creed__enforcement_mechanism).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, ecclesiastical_authority__doctrinal_monopoly).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel biblical_divine_nature. The kernel is the scriptural problem of monotheism-in-trinity. Three sibling readings decompose the kernel: trinitarian_reading (this file, three hypostases, one essence), unitarian_reading (strict numerical singularity), modalist_reading (sequential modes). Each reading has different ε, different beneficiaries/victims, and different enforcement mechanisms. They are linked via network.affects_constraints to show they are readings of the same kernel. The trinitarian reading is the institutionally dominant one; the sibling readings are minoritized and suppressed by this constraint's enforcement machinery.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__trinitarian_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
