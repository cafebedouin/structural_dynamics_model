% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__modalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist (Sabellian) Reading of Divine Nature — Sequential Modes of One Person
 *   domain: theology/religious authority/doctrinal history
 *
 * SUMMARY:
 *   This story instantiates the modalist (Sabellian) reading of the
 *   divine-nature kernel: Father, Son, and Spirit are not simultaneous
 *   persons but sequential modes or roles of a single divine person.
 *   Historically judged heretical by the councils that produced trinitarian
 *   orthodoxy (which reads it as collapsing the real distinctions the New
 *   Testament narrates — e.g. the Son praying to the Father), and judged
 *   theologically unstable by unitarians (who argue that calling Jesus fully
 *   God in any mode still compromises numerical divine simplicity), the
 *   reading nonetheless persists institutionally, chiefly in Oneness
 *   Pentecostalism, because it lets Jesus-centered piety proceed without
 *   technical philosophical vocabulary. Suppression was highest early
 *   (patristic-era excommunications, e.g. Sabellius, Noetus, Praxeas) and has
 *   moderated as legal/social enforcement gave way to denominational
 *   self-selection; extraction has risen modestly as the reading became
 *   institutionally load-bearing for a distinct denominational family with
 *   its own seminaries, publishing, and credentialing.
 *
 * KEY AGENTS:
 *   - modalist_clergy: administers doctrine, baptismal formula, and ordination (institutional/arbitrage) — sets the agenda
 *   - jesus_centered_congregations: receives simplified devotional framework (organized/constrained) — beneficiary bearing constrained exit
 *   - oneness_pentecostal_movement: institutional beneficiary and co-agenda-setter (organized/mobile)
 *   - trinitarian_dissenters_in_oneness_bodies: bears social/familial cost of doubt (powerless/trapped)
 *   - excommunicated_modalist_teachers_historical: historical victims of the doctrinal contest itself (powerless/trapped)
 *   - comparative_theologians: analytical observer across the full kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.42).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.55).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist (Sabellian) Reading of Divine Nature — Sequential Modes of One Person").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theology/religious authority/doctrinal history").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, '05ea616f-0ec7-4c78-abac-3c967e800df9').
narrative_ontology:cs_kernel_codification('05ea616f-0ec7-4c78-abac-3c967e800df9', distributed).
narrative_ontology:cs_authority_grounding('05ea616f-0ec7-4c78-abac-3c967e800df9', lineage).
narrative_ontology:cs_interpretation_layer_present('05ea616f-0ec7-4c78-abac-3c967e800df9').
narrative_ontology:cs_reading_relation('05ea616f-0ec7-4c78-abac-3c967e800df9', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('05ea616f-0ec7-4c78-abac-3c967e800df9', biblical_divine_nature__unitarian_reading, coexists_with).
narrative_ontology:cs_axiom('05ea616f-0ec7-4c78-abac-3c967e800df9', foundational, divine_person_is_numerically_singular).
narrative_ontology:cs_axiom_status(divine_person_is_numerically_singular, holdable).
narrative_ontology:cs_axiom_grounding('05ea616f-0ec7-4c78-abac-3c967e800df9', divine_person_is_numerically_singular, deontological).
narrative_ontology:cs_axiom('05ea616f-0ec7-4c78-abac-3c967e800df9', foundational, father_son_spirit_are_sequential_self_manifestations).
narrative_ontology:cs_axiom_status(father_son_spirit_are_sequential_self_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('05ea616f-0ec7-4c78-abac-3c967e800df9', father_son_spirit_are_sequential_self_manifestations, conventional).
narrative_ontology:cs_reference_frame('05ea616f-0ec7-4c78-abac-3c967e800df9', pre_nicene_monarchian_monotheism).
narrative_ontology:cs_drift_state('05ea616f-0ec7-4c78-abac-3c967e800df9', post_nicene_creedal_consolidation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('05ea616f-0ec7-4c78-abac-3c967e800df9', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, modalist_clergy).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, jesus_centered_congregations).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, oneness_pentecostal_movement).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, trinitarian_dissenters_in_oneness_bodies).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, excommunicated_modalist_teachers_historical).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, converts_seeking_doctrinal_clarity).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, strict_numerical_monotheism).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, full_deity_of_christ_without_subordination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer congregations (historically Sabellian communities in 3rd-century Rome; today chiefly Oneness Pentecostal denominations) that teach God is one person manifesting successively as Father, Son, and Spirit. They set catechesis, baptismal formula (in Jesus' name only), and ordination standards around this reading, and derive institutional authority, membership cohesion, and doctrinal distinctiveness from maintaining it against both trinitarian and unitarian critique.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_clergy, agenda_setter,
    institutional, generational, arbitrage, national).

% Lay members receive a devotional framework that lets them worship Jesus as fully and directly God without needing to parse hypostatic union or essence/person distinctions. This simplifies piety and preaching but ties their religious identity to a reading their denomination enforces through baptismal practice and communion access; leaving the framework often means leaving the community entirely.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, jesus_centered_congregations, beneficiary,
    organized, biographical, constrained, regional).

% As an institutional movement (denominations, seminaries, publishing houses), it benefits from modalism as a distinguishing doctrinal identity that differentiates it from mainstream trinitarian Pentecostalism and unitarian groups, sustaining separate institutions, credentialing, and revenue streams (publishing, conferences, missions).
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, oneness_pentecostal_movement, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__modalist_reading, oneness_pentecostal_movement, agenda_setter).

% Individuals raised in or converted into Oneness communities who come to find trinitarian formulations more coherent bear real costs: family estrangement, loss of church community, and re-baptism requirements if they move to trinitarian fellowship. Their doubts are treated as backsliding rather than legitimate theological disagreement.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_dissenters_in_oneness_bodies, payer,
    powerless, biographical, trapped, local).

% Historically, figures like Sabellius and Noetus were excommunicated by the mainstream church (and this reading's own later adherents split from each other) once their formulation was judged heretical; they bore reputational and institutional costs for holding the position before institutional norms hardened against it.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, excommunicated_modalist_teachers_historical, payer,
    powerless, biographical, trapped, regional).

% New converts told this is simply 'biblical monotheism' are not always informed that this reading is a minority position rejected by roughly 2 billion trinitarian Christians as heretical (Sabellianism) and by unitarians as insufficiently monotheist in practice (since it still calls Jesus fully God); the interpretive contest is often not disclosed.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, converts_seeking_doctrinal_clarity, payer,
    powerless, biographical, constrained, local).

% The historical councils (e.g. against Sabellius, and later creedal formulations) that judged this reading heretical are not present within modalist institutions to contest the reading's self-presentation as simply 'the plain biblical teaching'; their objections are addressed only secondhand, often as caricature.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_councils_historical, excluded,
    institutional, civilizational, analytical, global).

% Historians and theologians of doctrine trace how each reading of the kernel emerged from the same scriptural corpus and assess consistency, historical reception, and philosophical coherence without institutional stake in any single reading's survival.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, comparative_theologians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides congregations a way to affirm both strict monotheism (one God, one person) and the full deity of Jesus Christ without adopting the philosophical apparatus of hypostasis/ousia distinctions — coordinating worship practice, baptismal formula, and catechesis around a single, teachable formula.
% TRANSFER_FUNCTION: Moves doctrinal authority and institutional loyalty toward modalist clergy and denominational structures (Oneness Pentecostal bodies), and moves social/familial costs onto members who later find the position theologically untenable and wish to move toward trinitarian or other readings.
% ABSENT_VOICES: Historical trinitarian councils and creedal authorities that formally judged this reading heretical (Sabellianism) are not represented within modalist institutional teaching; unitarian critics who argue the reading smuggles in effective tritheism through role-shifting are similarly absent from internal catechesis.
% DISAPPEARANCE_RATIONALE: If the modalist reading vanished, Oneness Pentecostal denominations would lose their doctrinal distinctive and institutional rationale for separate existence; congregations would need to adopt either trinitarian or unitarian frameworks, likely causing denominational restructuring, re-baptisms, and shifts in publishing/seminary output.
% FOUNDING_PROBLEM: Early Christian communities faced the problem of affirming Jesus' full deity while maintaining strict Jewish-inherited monotheism (one God) without an available philosophical vocabulary (person/essence distinction) to express plurality-within-unity.
% FOUNDING_PROBLEM_CORROBORATION: Patristic-era opponents (e.g., Tertullian and Hippolytus writing against Sabellius/Praxeas) attest the problem was real but argue the modalist solution was theologically inadequate; contemporary comparative theologians and historians of doctrine outside Oneness institutions corroborate that the underlying monotheism-versus-Christ's-deity tension was genuine even as they judge the sequential-modes resolution unsuccessful.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__modalist_reading_tests).
:- end_tests(biblical_divine_nature__modalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42): the reading genuinely solves a coordination problem (simple Jesus-centered monotheistic piety) but also sustains denominational institutions (seminaries, publishing, credentialing) that extract loyalty and resources from congregants who might otherwise adopt a different reading with equal biblical warrant. Suppression starts high (0.70) reflecting patristic-era excommunication enforcement and falls as the mechanism shifts from coercive orthodoxy-policing to voluntary denominational sorting, then ticks back up slightly as Oneness institutions formalize re-baptism requirements and doctrinal tests for members who drift toward trinitarian formulations. Theater ratio is modest but rising (0.12 to 0.28) as institutional apologetics increasingly perform doctrinal defense against both trinitarian and unitarian critique rather than doing first-order biblical exegesis. Accessibility collapse is moderate (0.5): the reading does not eliminate alternative readings from the broader tradition, but within modalist institutions the alternative readings are rarely presented on their own terms. Resistance is high (0.72) because both major sibling readings actively contest this one and label it heretical or theologically inadequate.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (modalist clergy, Oneness institutions), this is genuine doctrinal coordination — solving the Christ's-deity/monotheism tension with a teachable formula. From the payer seats (dissenters, historical excommunicated teachers, uninformed converts), the same structure operates as enforced doctrinal conformity with real exit costs (re-baptism, family estrangement, reputational loss). The engine should compute these seats to different types given the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Modalist clergy and the Oneness Pentecostal institutional apparatus sit near the beneficiary end: they administer the doctrine, derive institutional identity and resources from it, and have mobile/arbitrage-level exit (they can reframe or adjust practice without personal cost). Congregational members are structurally closer to symmetric-to-target: they receive devotional benefit but are constrained by community and baptismal-identity lock-in. Individuals who come to doubt the reading (trinitarian dissenters, converts denied full disclosure) are full targets — powerless, trapped, bearing the cost of the institutional doctrine's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling strict monotheism with the full deity of Christ absent a person/essence philosophical vocabulary — was live in the 2nd-3rd centuries when this reading emerged. Whether it remains live is contested: trinitarian theology addressed the same problem through the hypostasis/ousia distinction beginning in the 4th century, arguably resolving it more durably; modalist institutions did not abandon the sequential-modes solution once that alternative vocabulary became available, but instead institutionalized it as a permanent denominational marker. This is the mandatrophy signature: a reading built to solve a vocabulary gap that has since been closed by a rival tradition, but which persists because it now anchors distinct institutions, credentialing, and identity rather than because it remains the best available solution to the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modalist_reading_kernel_position,
    'Is the modalist reading a genuine early-church solution to the monotheism/Christ''s-deity tension that was later superseded by a better vocabulary (trinitarian hypostasis/ousia), or is it an independently coherent reading unfairly marginalized by councils dominated by the eventually-victorious trinitarian faction?',
    'Historical-critical analysis of pre-Nicene patristic sources (Tertullian''s Adversus Praxean, Hippolytus''s Contra Noetum) alongside comparative analysis of scriptural texts cited by each reading; assessment of whether the sequential-modes formulation can account for texts depicting simultaneous Father-Son interaction (e.g. Gethsemane, baptism scene) without ad hoc revision.',
    'If modalism is judged an inadequate solution superseded by better conceptual tools, its persistence looks more like institutional inertia/mandatrophy. If judged an independently defensible reading suppressed by a contingent councilar outcome, its persistence looks more like ongoing minority-tradition coordination under external suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modalist_reading_kernel_position, conceptual, 'Whether modalism is a superseded solution or an unfairly marginalized independent reading.').

omega_variable(
    sibling_reading_delta,
    'What specifically changes structurally when moving from this reading to trinitarian_reading (three simultaneous hypostases) or unitarian_reading (Father alone is God)?',
    'Document via the network.affects_constraints links and the cs_structure reading_relations: trinitarian_reading rejects sequential-modes as insufficient to account for simultaneous Father-Son address in scripture (the historic Sabellian-heresy charge); unitarian_reading rejects this reading''s affirmation of Jesus'' full deity in any mode as compromising numerical monotheism (the historic ''insufficient monotheism'' charge from the opposite direction). This reading sits structurally between the two siblings on the plurality axis.',
    'Confirms the ε-invariance decomposition is correct: the three readings are not the same constraint measured differently, but three constraints with different victim sets, different institutional apparatus, and different resistance profiles from the two other readings simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Structural delta between this reading and its two siblings in the kernel contest.').

omega_variable(
    oneness_baptism_practice_ambiguity,
    'Is the Oneness Pentecostal requirement of re-baptism ''in Jesus'' name only'' (rejecting trinitarian baptismal formula as invalid) a genuine doctrinal entailment of the modalist reading, or an institutional boundary-maintenance mechanism that could be relaxed without abandoning the underlying theology?',
    'Comparative examination of Oneness denominations that do versus do not require re-baptism of trinitarian converts; interview data on whether re-baptism requirements correlate with institutional retention metrics rather than theological necessity.',
    'If re-baptism is separable from the core doctrine, the suppression measured here partly reflects institutional self-preservation rather than the theological claim itself, which would lower the constraint''s intrinsic suppression relative to its administered suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oneness_baptism_practice_ambiguity, empirical, 'Whether re-baptism enforcement is doctrinally necessary or institutionally self-serving.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__modalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t300, biblical_divine_nature__modalist_reading, theater_ratio, 300, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t300, observed).
narrative_ontology:measurement(bibl_tr_t600, biblical_divine_nature__modalist_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t600, observed).
narrative_ontology:measurement(bibl_tr_t900, biblical_divine_nature__modalist_reading, theater_ratio, 900, 0.2).
narrative_ontology:measurement_basis(bibl_tr_t900, observed).
narrative_ontology:measurement(bibl_tr_t1200, biblical_divine_nature__modalist_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t1200, observed).
narrative_ontology:measurement(bibl_tr_t1600, biblical_divine_nature__modalist_reading, theater_ratio, 1600, 0.26).
narrative_ontology:measurement_basis(bibl_tr_t1600, observed).
narrative_ontology:measurement(bibl_tr_t1800, biblical_divine_nature__modalist_reading, theater_ratio, 1800, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t1800, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__modalist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t300, biblical_divine_nature__modalist_reading, base_extractiveness, 300, 0.3).
narrative_ontology:measurement_basis(bibl_be_t300, observed).
narrative_ontology:measurement(bibl_be_t600, biblical_divine_nature__modalist_reading, base_extractiveness, 600, 0.32).
narrative_ontology:measurement_basis(bibl_be_t600, observed).
narrative_ontology:measurement(bibl_be_t900, biblical_divine_nature__modalist_reading, base_extractiveness, 900, 0.35).
narrative_ontology:measurement_basis(bibl_be_t900, observed).
narrative_ontology:measurement(bibl_be_t1200, biblical_divine_nature__modalist_reading, base_extractiveness, 1200, 0.38).
narrative_ontology:measurement_basis(bibl_be_t1200, observed).
narrative_ontology:measurement(bibl_be_t1600, biblical_divine_nature__modalist_reading, base_extractiveness, 1600, 0.4).
narrative_ontology:measurement_basis(bibl_be_t1600, observed).
narrative_ontology:measurement(bibl_be_t1800, biblical_divine_nature__modalist_reading, base_extractiveness, 1800, 0.42).
narrative_ontology:measurement_basis(bibl_be_t1800, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__modalist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t300, biblical_divine_nature__modalist_reading, suppression_requirement, 300, 0.65).
narrative_ontology:measurement_basis(bibl_su_t300, observed).
narrative_ontology:measurement(bibl_su_t600, biblical_divine_nature__modalist_reading, suppression_requirement, 600, 0.6).
narrative_ontology:measurement_basis(bibl_su_t600, observed).
narrative_ontology:measurement(bibl_su_t900, biblical_divine_nature__modalist_reading, suppression_requirement, 900, 0.55).
narrative_ontology:measurement_basis(bibl_su_t900, observed).
narrative_ontology:measurement(bibl_su_t1200, biblical_divine_nature__modalist_reading, suppression_requirement, 1200, 0.5).
narrative_ontology:measurement_basis(bibl_su_t1200, observed).
narrative_ontology:measurement(bibl_su_t1600, biblical_divine_nature__modalist_reading, suppression_requirement, 1600, 0.53).
narrative_ontology:measurement_basis(bibl_su_t1600, observed).
narrative_ontology:measurement(bibl_su_t1800, biblical_divine_nature__modalist_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement_basis(bibl_su_t1800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__modalist_reading, 0.1).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__unitarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'the nature of God in Christian scripture' per the ε-invariance principle. trinitarian_reading, unitarian_reading, and this modalist_reading each interpret the same scriptural corpus but produce structurally distinct claims with different ε, different beneficiary/victim structures, and different institutional apparatus. They are linked via affects_constraints rather than merged into one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
