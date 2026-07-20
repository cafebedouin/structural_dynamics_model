% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist Reading of Biblical Divine Nature (Oneness Christology)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   The modalist reading of the biblical divine nature kernel holds that
 *   Father, Son, and Holy Spirit are sequential modes or roles of one divine
 *   person, not simultaneous persons. Institutionalized most visibly in
 *   Oneness Pentecostalism, this reading enforces Jesus-name baptism and
 *   strict monotheistic devotion while actively excluding Trinitarian and
 *   unitarian alternatives. It presents itself as a restoration of apostolic
 *   simplicity and a solution to the problem of maintaining monotheism
 *   without philosophical abstraction.
 *
 * KEY AGENTS:
 *   - Oneness clergy (agenda_setter / institutional / constrained exit) â administer doctrinal boundaries and accrue institutional authority
 *   - Oneness laity (payer / moderate / identity_locked) â bear the costs of ecumenical separation and doctrinal conformity
 *   - Trinitarian Pentecostals (excluded / institutional / analytical) â historic rejectors of modalism, absent from oneness governance
 *   - Ecumenical theologians (observer / institutional / analytical) â analyze the reading from outside as heresy or restoration
 *   - Unitarian critics (excluded / moderate / constrained) â reject modalism from the other flank as insufficiently rigorous
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.58).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.72).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist Reading of Biblical Divine Nature (Oneness Christology)").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, 'a002b670-eec1-4730-9d53-5a704924b966').
narrative_ontology:cs_kernel_codification('a002b670-eec1-4730-9d53-5a704924b966', fixed_text).
narrative_ontology:cs_authority_grounding('a002b670-eec1-4730-9d53-5a704924b966', lineage).
narrative_ontology:cs_interpretation_layer_present('a002b670-eec1-4730-9d53-5a704924b966').
narrative_ontology:cs_reading_relation('a002b670-eec1-4730-9d53-5a704924b966', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('a002b670-eec1-4730-9d53-5a704924b966', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_axiom('a002b670-eec1-4730-9d53-5a704924b966', foundational, one_person_three_manifestations).
narrative_ontology:cs_axiom_status(one_person_three_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('a002b670-eec1-4730-9d53-5a704924b966', one_person_three_manifestations, theological).
narrative_ontology:cs_axiom('a002b670-eec1-4730-9d53-5a704924b966', foundational, divine_fulness_in_each_mode).
narrative_ontology:cs_axiom_status(divine_fulness_in_each_mode, holdable).
narrative_ontology:cs_axiom_grounding('a002b670-eec1-4730-9d53-5a704924b966', divine_fulness_in_each_mode, theological).
narrative_ontology:cs_reference_frame('a002b670-eec1-4730-9d53-5a704924b966', strict_apostolic_monotheism).
narrative_ontology:cs_drift_state('a002b670-eec1-4730-9d53-5a704924b966', post_nicene_ecumenical_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a002b670-eec1-4730-9d53-5a704924b966', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, oneness_clergy).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, oneness_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer baptism exclusively in Jesus' name, teach the oneness of God, and maintain doctrinal boundaries against Trinitarianism. Their authority derives from claiming to restore apostolic Christianity and they set the theological agenda for the movement.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, oneness_clergy, agenda_setter,
    institutional, generational, constrained, global).

% Participate in Jesus-name baptism and oneness worship communities. Bear the social and ecclesial cost of separation from mainstream Christianity, including limited access to ecumenical education, inter-church marriage pools, and employment networks outside the movement.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, oneness_laity, payer,
    moderate, biographical, identity_locked, global).

% Represent the mainstream Pentecostal tradition that expelled modalist teachers in 1916. They would object to modalist baptismal formulae and Christology but are not present in oneness governance structures or doctrinal councils.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_pentecostals, excluded,
    institutional, generational, analytical, global).

% Analyze modalism as a Christological deviation (Sabellianism) or as a legitimate restorationist movement. They operate from trinitarian or critical academic frameworks and do not participate in oneness institutional life.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, ecumenical_theologians, observer,
    institutional, civilizational, analytical, global).

% Reject modalism as insufficiently monotheistic or as philosophically incoherent. They advocate for strict unitarianism and are excluded from both Trinitarian and Oneness orthodoxies.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, unitarian_critics, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__modalist_reading, oneness_clergy).
narrative_ontology:fixing_cost_class(biblical_divine_nature__modalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, non-philosophical framework for Jesus-centered worship and monotheistic devotion; coordinates communal identity around a single divine person revealed in saving acts without requiring training in essence-hypostasis metaphysics.
% TRANSFER_FUNCTION: Moves doctrinal authority from ecumenical conciliar tradition and Trinitarian institutions to restorationist apostolic leaders; moves conformity, tithe flows, and communal loyalty from adherents to the oneness ecclesial structure in exchange for sacramental access and group identity.
% ABSENT_VOICES: Trinitarian theologians who view modalism as heresy, and unitarian critics who view it as philosophically inadequate, are structurally excluded from oneness doctrinal councils and seminary governance. Ecumenical dialogue partners are absent from internal boundary-setting.
% DISAPPEARANCE_RATIONALE: If the modalist reading vanished from oneness communities, baptismal practice would shift to trinitarian formulae or other variants, the community's boundary against mainstream Pentecostalism would collapse, and the institutional identity of the movement would dissolve into broader evangelicalism or trinitarian Pentecostalism.
% FOUNDING_PROBLEM: How to preserve the absolute oneness of God against both polytheistic deviation and philosophical abstraction, while maintaining the full saving significance of Jesus Christ and the Spirit's presence.
% FOUNDING_PROBLEM_CORROBORATION: Oneness historians and restorationist theologians attest the problem as live. External church historians and systematic theologians outside the beneficiary set argue the problem was resolved by conciliar Trinitarianism or that modalism is itself a reduction of apostolic complexity; no neutral corroboration exists.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__modalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__modalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) is moderate-to-substantial: the constraint extracts doctrinal conformity, tithe flows, and identity fusion from laity, while providing real coordination in the form of worship community and theological simplicity. Suppression (0.72) is higher: the constraint must actively suppress trinitarian teaching within its spaces to survive, and boundary maintenance intensified as the movement institutionalized. Theater ratio (0.45) reflects substantial performative maintenance of boundaries (heresy vigilance, exclusionary rhetoric, restorationist historiography) relative to the core worship function. Accessibility collapse (0.70) is high for adherents: once the modalist hermeneutic is accepted, trinitarian proof-texts are reinterpreted as modalist evidence, making external alternatives cognitively inaccessible. Resistance (0.60) reflects sustained external rejection by trinitarian majorities and unitarian critics.
 *
 * PERSPECTIVAL GAP:
 *   From the clergy seat, the constraint is a restoration of apostolic purity and a necessary bulwark against trinitarian philosophical corruption. From the laity seat, it is the price of belongingâgenuine community and soteriological certainty purchased with ecumenical isolation and doctrinal conformity. From the excluded trinitarian seat, the constraint is simply heresy; from the unitarian seat, insufficiently rigorous monotheism. The engine computes these divergences from the same structural data without adjudicating theological truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Oneness clergy are structural beneficiaries (low d): they administer the constraint, set doctrinal boundaries, and accrue institutional authority and tithe-based resourcing. Oneness laity are structural targets (high d): they bear the costs of identity-locked conformity, limited ecumenical access, and cognitive suppression of trinitarian alternatives. Trinitarian Pentecostals and unitarian critics are excluded from the constraint's interior, experiencing it from outside as heresy or inadequacy; their directionality is analytically determined as non-participants. The effective extraction is amplified for the identity-locked laity and damped for the clergy who subsidize their authority through the same structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The modalist constraint prevents mislabeling by distinguishing genuine coordination (shared monotheistic worship, community identity, Jesus-centered piety) from pure extraction (institutional boundary maintenance that secures clergy authority and isolates adherents). The classification as tangled_rope reflects that the founding problemâhow to worship Jesus within strict monotheismâremains live for the beneficiary community, while the victim seat experiences the same structure as doctrinal lock-in and social separation. A snare classification would miss the real coordination function; a rope classification would miss the asymmetric extraction and active enforcement required to maintain the boundary against both Trinitarian and unitarian alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the biblical text ontologically commit to one divine person in three modes, three persons in one essence, or a singular God with subordinate agents?',
    'Historical-critical analysis of early Christian diversity; sociological study of which reading communities can sustain themselves over time.',
    'Determines which constraint family members are live institutional arrangements versus historical artifacts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Irreducible ambiguity in the kernel text between the three major readings.').

omega_variable(
    restoration_historicity,
    'Is modalism a recovery of the original apostolic teaching or a modern reaction to Trinitarian philosophical abstraction?',
    'Historical scholarship on early Christian diversity (Patripassianism, Monarchianism) versus conciliar development; archaeological and textual evidence from the first three centuries.',
    'If restoration, the constraint''s authority_grounding is strengthened as lineage; if innovation, it is better classified as identity_coordination extraction with a cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_historicity, empirical, 'Whether modalism represents genuine apostolic continuity or later innovation.').

omega_variable(
    identity_coordination_extraction,
    'Does the identity coordination provided by oneness community exceed the Boltzmann floor for identity_coordination, or is the relational framing primarily a cover for institutional extraction?',
    'Comparative study of exit costs and psychological outcomes across identity-locked religious communities; measurement of effective extraction versus coordination benefit.',
    'If the excess extraction is below the complexity-adjusted floor, the constraint is rope-leaning; if above, it is tangled_rope or snare-leaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coordination_extraction, empirical, 'Whether the community''s identity coordination is genuine or extractively dominated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__modalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bibl_tr_t20, biblical_divine_nature__modalist_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(bibl_tr_t40, biblical_divine_nature__modalist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(bibl_tr_t60, biblical_divine_nature__modalist_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(bibl_tr_t80, biblical_divine_nature__modalist_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(bibl_tr_t100, biblical_divine_nature__modalist_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__modalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bibl_be_t20, biblical_divine_nature__modalist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(bibl_be_t40, biblical_divine_nature__modalist_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(bibl_be_t60, biblical_divine_nature__modalist_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(bibl_be_t80, biblical_divine_nature__modalist_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(bibl_be_t100, biblical_divine_nature__modalist_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__modalist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bibl_su_t20, biblical_divine_nature__modalist_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(bibl_su_t40, biblical_divine_nature__modalist_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(bibl_su_t60, biblical_divine_nature__modalist_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(bibl_su_t80, biblical_divine_nature__modalist_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(bibl_su_t100, biblical_divine_nature__modalist_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, unitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the biblical_divine_nature kernel, decomposed from the colloquial label 'biblical teaching on God's nature' per the Îµ-invariance principle. The modalist reading, trinitarian reading, and unitarian reading each instantiate structurally distinct constraints with different Îµ values, beneficiary structures, and directionality profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
