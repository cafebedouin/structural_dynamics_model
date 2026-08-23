% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__subordinationist, []).

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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Logos Doctrine — Created-Logos Worship and Authority Boundary (John 1:1)
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   John 1:1's Logos is a contested kernel; this file instantiates the
 *   subordinationist reading as its own ε-invariant constraint: the standing
 *   arrangement in which the Logos is the first and highest created agent,
 *   not co-eternal or consubstantial with the Father, and in which community
 *   worship and teaching are organized around that claim. The arrangement
 *   coordinates a global monotheistic community — it solves, as the reading
 *   frames it, the problem of honoring the Logos's preeminence without a
 *   second uncreated God — while bearing asymmetrically on parties outside
 *   its benefit: the authority claims of high-church traditions rest on the
 *   full-divinity reading and lose their scriptural ground if this one is
 *   right, and members whose devotion runs to the fullest register must
 *   discipline it to veneration. The ε value is reading-indexed: it assesses
 *   the subordinationist arrangement by the subordinationist reading's own
 *   lights, over that arrangement as referent — not the orthodox arrangement
 *   this reading contests, and not any arrangement this reading would
 *   endorse. The sibling readings (orthodox_christological,
 *   non_incarnational_monotheist) are separate constraint files; the
 *   committer structure lives in the omegas and cs_structure, not here.
 *
 * KEY AGENTS:
 *   - subordinationist_teaching_authority: agenda-setter and seat of receipt (institutional/arbitrage) — sets and enforces the boundary, collects support and loyalty
 *   - subordinationist_congregations: primary beneficiary (organized/constrained) — gains monotheistic coherence, hermeneutic identity, felt fidelity to the plain sense
 *   - high_church_traditions: primary target (institutional/identity_locked) — authority claims bear the constraint's structural cost; cannot exit without dissolving
 *   - full_divinity_devotees: secondary target (powerless/constrained) — devotional expression bounded by the worship rule; coalition potential suppressed by shunning
 *   - orthodox_christologians: excluded voice (institutional/mobile) — holds the rival reading and its anathemas, outside the teaching conversation
 *   - patristics_scholars: analytical observer — documents the ante-Nicene consensus and the contested character of the Nicene settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.55).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.6).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.55).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Logos Doctrine — Created-Logos Worship and Authority Boundary (John 1:1)").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, '495092c2-5d6d-48ba-b615-ad500b163dc8').
narrative_ontology:cs_kernel_codification('495092c2-5d6d-48ba-b615-ad500b163dc8', fixed_text).
narrative_ontology:cs_authority_grounding('495092c2-5d6d-48ba-b615-ad500b163dc8', lineage).
narrative_ontology:cs_interpretation_layer_present('495092c2-5d6d-48ba-b615-ad500b163dc8').
narrative_ontology:cs_reading_relation('495092c2-5d6d-48ba-b615-ad500b163dc8', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('495092c2-5d6d-48ba-b615-ad500b163dc8', john_1_1_logos__non_incarnational_monotheist, forecloses).
narrative_ontology:cs_axiom('495092c2-5d6d-48ba-b615-ad500b163dc8', foundational, logos_is_first_created_agent).
narrative_ontology:cs_axiom_status(logos_is_first_created_agent, holdable).
narrative_ontology:cs_axiom_grounding('495092c2-5d6d-48ba-b615-ad500b163dc8', logos_is_first_created_agent, theological).
narrative_ontology:cs_axiom('495092c2-5d6d-48ba-b615-ad500b163dc8', secondary, latria_reserved_for_uncreated_father).
narrative_ontology:cs_axiom_status(latria_reserved_for_uncreated_father, holdable).
narrative_ontology:cs_axiom_grounding('495092c2-5d6d-48ba-b615-ad500b163dc8', latria_reserved_for_uncreated_father, deontological).
narrative_ontology:cs_reference_frame('495092c2-5d6d-48ba-b615-ad500b163dc8', ante_nicene_subordinationist_consensus).
narrative_ontology:cs_drift_state('495092c2-5d6d-48ba-b615-ad500b163dc8', contemporary_restorationist_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('495092c2-5d6d-48ba-b615-ad500b163dc8', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_congregations).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_teaching_authority).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, full_divinity_devotees).
narrative_ontology:constraint_vindicates(john_1_1_logos__subordinationist, strict_monotheism_doctrine).
narrative_ontology:constraint_vindicates(john_1_1_logos__subordinationist, ante_nicene_plain_sense_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes the movement's translation and literature, trains teachers, and defines what the community may say about the Logos and how worship may be directed. It disciplines deviation up to expulsion and shunning, and has revised specific doctrines before while holding the core boundary fixed. Material support, volunteer labor, and loyalty flow to it as custodian of the reading; its standing depends on the boundary staying sharp against both orthodox and liberal rivals.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_teaching_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, subordinationist_teaching_authority, beneficiary).

% Local bodies that worship the Father alone and venerate the Logos as His first agent. The reading gives them doctrinal coherence, a felt fidelity to what they take as the plain sense of John 1:1c and Proverbs 8:22, and a distinct identity against both orthodox and secularizing alternatives. Leaving would mean joining bodies whose core claims they believe the text contradicts, at the cost of their entire religious world.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_congregations, beneficiary,
    organized, generational, constrained, global).

% Catholic, Orthodox, and high-church Protestant bodies whose sacramental theology, episcopal authority, and liturgical practice rest on the Logos's full divinity. If the Logos is created, the authority claims built on full divinity lose their scriptural ground. These bodies cannot abandon the claim without dissolving what they are; their response is anathema and apologetic, not exit.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_traditions, payer,
    institutional, civilizational, identity_locked, global).

% Members inside subordinationist communities whose devotion instinctively runs toward honoring Christ in the fullest register. The boundary requires them to venerate rather than worship, to direct latria to the Father alone, and to accept the created-Logos teaching as a condition of membership. Dissent costs them standing, and in high-control congregations, family and social ties.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, full_divinity_devotees, payer,
    powerless, biographical, constrained, regional).

% Theologians of the orthodox reading — they hold the rival claim and its anathemas but sit outside the subordinationist community's teaching conversation. Their exegetical arguments circulate only as apologetic targets; historically, their predecessors were excluded by imperial exile when the enforcement machinery ran in the opposite direction.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, orthodox_christologians, excluded,
    institutional, generational, mobile, global).

% Academic historians of doctrine who document that subordinationist Logos theology was the elite default of the second and third centuries and that the Nicene settlement was a contested adjudication rather than a discovery. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, patristics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__subordinationist, subordinationist_teaching_authority).
narrative_ontology:fixing_cost_class(john_1_1_logos__subordinationist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a monotheistic community's worship and teaching: it solves, as the reading frames it, the problem of giving the Logos preeminence, creative agency, and devotional honor while preserving the Father's uncreated singularity, and it supplies a shared hermeneutic of John 1:1c, Proverbs 8:22, and Colossians 1:15 that marks membership and bounds permissible worship.
% TRANSFER_FUNCTION: Moves devotional expression and interpretive authority: worship that members might direct to the Logos as fully divine is redirected to the Father alone; interpretive authority over the contested texts moves from high-church magisteria to the subordinationist teaching authority; and material support, labor, and loyalty flow from members to the teaching authority that maintains the boundary.
% ABSENT_VOICES: Proponents of the orthodox reading are structurally outside the subordinationist community's interpretive conversation — their anathemas and exegetical arguments circulate only as apologetic targets. Ordinary members who experience full-devotional pull toward Christ lack standing to contest the boundary. Historically, Nicene bishops under Arian-imperial enforcement were literally excluded from the conversation by exile.
% DISAPPEARANCE_RATIONALE: If the subordinationist boundary vanished overnight, its communities would lose the membership marker that organizes worship, teaching, and identity; members would drift toward either the orthodox or the non-incarnational reading; the high-church traditions would lose a standing scriptural challenge to their authority claims; and the teaching authorities would lose the office whose function is boundary maintenance.
% FOUNDING_PROBLEM: How can a monotheist community confess the Logos's preeminence, agency in creation, and exalted authority without worshiping a second uncreated God — how to honor the Logos while preserving the Father's uncreated singularity.
% FOUNDING_PROBLEM_CORROBORATION: Patristics scholarship — including confessional scholars outside the subordinationist benefiting set — attests that the problem was live and that subordinationist Logos theology was the elite default before Nicaea (Justin, Origen, the Eusebians). Orthodox historical theology corroborates that the problem was real but attests it was adjudicated at Nicaea and Constantinople rather than left open. No party outside the benefiting set attests that the problem remains live today; that attestation comes from the subordinationist communities themselves.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__subordinationist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55 at interval end): the arrangement does genuine hermeneutical and liturgical work for its communities, but the same structure transfers interpretive authority and material support to the teaching custodian and bears on high-church authority claims and on members' devotional range. Suppression (0.6) reflects active enforcement: the rival readings re-emerge constantly from the same texts, so the boundary must be maintained by teaching machinery, literature and translation control, and — in high-control instantiations — expulsion and shunning. Theater (0.32) is real but a minority share: most activity is functional boundary-keeping, with performative surplus in anathema rhetoric and restoration self-description. Accessibility collapse (0.4) is low for a doctrinal constraint: the same canon sustains all three readings, so alternatives never fully collapse. Resistance (0.65): the arrangement has survived seventeen centuries of orthodox opposition, imperial anathema, and modern apologetic pressure. The three temporal series share one grid. Suppression_requirement is tracked because enforcement capacity is this story's dynamic — imperial build-up under the Arian-aligned courts, collapse after the post-Theodosian settlement, and congregational rebuild in the modern high-control movements. The base_properties values describe the current (interval-end) state.
 *
 * PERSPECTIVAL GAP:
 *   The teaching-authority seat computes the arrangement as faithful stewardship of the text's plain sense and the necessary price of monotheism; the high-church seat computes the same structure as an assault on the church's sacramental foundation; the devotee seat experiences it as a standing discipline on devotion; the excluded orthodox voice computes it as a settled heresy. The powerless devotee seat's coalition potential is structurally suppressed by the shunning mechanism — the cost of collective dissent exceeds the cost of individual compliance. These per-seat computations follow from the structural data (institutional vs powerless power; arbitrage vs identity_locked vs constrained exit); the engine computes them and the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The congregations and the teaching authority sit at the beneficiary end (low d): the arrangement subsidizes their coherence and identity, and the gains demonstrably accrue to the teaching custodian. The high-church traditions sit near the full-target end (high d): they bear the structural cost — the scriptural ground of their authority is what the arrangement denies — and identity_locked exit places them near the full-target end rather than the mobile end. The full_divinity_devotees are governed directly in their devotional practice, with constrained exit, placing them high on the target side as well. The derivation from beneficiary/victim declarations plus exit options covers every seat; no directionality overrides were needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to confess the Logos's preeminence without a second uncreated God — is contested-live: subordinationist parties attest it is perennial; orthodox parties attest Nicaea settled it. The tangled_rope classification is what keeps both halves visible. A snare reading would erase the genuine coordination: these communities really do solve the monotheism problem as they frame it, at modest internal coercive overhead. A rope reading would erase the victim structure: high-church authority and devotee devotion bear real, asymmetric costs through the same structure that coordinates the community. If the founding problem were ever dead for all parties, the arrangement would decay toward piton — boundary maintenance without a live question — and theater_ratio would climb as enforcement became pure performance. The contested-live status is what currently keeps enforcement meaningful and the classification stable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint instantiates the subordinationist reading of kernel john_1_1_logos. Which reading does the text itself sustain, and what structurally changes under each sibling? The disagreement is located in the referent and force of the anarthrous theos in John 1:1c read against Proverbs 8:22 and Colossians 1:15: created hypostasis (this file), uncreated second person, or personified wisdom with no hypostasis at all.',
    'Exegetical adjudication by methods all parties accept; the historical attempt (Nicaea) settled jurisdiction by imperial coercion rather than exegesis, which is why the kernel remains contested after seventeen centuries.',
    'Under the orthodox sibling the victim set flips — subordinationist communities become the constrained party and full latria becomes mandatory; under the non-incarnational sibling the worship boundary largely dissolves and this file''s hypostasis claim fails. This file''s ε, victims, and type hold only within the subordinationist instantiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Kernel underdetermination: which reading of John 1:1 the constraint instantiates, and what each sibling would change structurally.').

omega_variable(
    enforcement_capacity_source,
    'Is the modern enforcement machinery (expulsion, shunning, literature and translation control) intrinsic to the created-Logos boundary, or an organizational artifact of specific high-control movements that other subordinationist bodies lack?',
    'Comparative study of enforcement practice across subordinationist bodies — independent biblical unitarian congregations, Christadelphians, Watch Tower organizations — holding the doctrine constant and varying the organization.',
    'If organizational artifact, suppression for the reading as such drops toward 0.3 and the extractive half of the tangled_rope structure holds mainly for high-control instantiations; if intrinsic, the boundary requires the enforcement wherever it governs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_source, empirical, 'Whether the measured suppression is doctrinally intrinsic or organizationally contingent.').

omega_variable(
    victim_cost_materiality,
    'Do the high-church traditions bear material costs from this constraint''s persistence, or are the costs nominal given the reading''s minority status?',
    'Measure flows: conversion traffic between communities, the share of high-church apologetic production aimed at the subordinationist challenge, and authority disputes in which the created-Logos reading is the operative objection.',
    'If nominal, the constraint operates closer to a coordination arrangement within its own communities and the victim declaration does little work; if material, the asymmetric-extraction half of the tangled_rope structure is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_cost_materiality, empirical, 'Materiality of the costs borne by high-church traditions under this reading''s persistence.').

omega_variable(
    restoration_vs_innovation,
    'Do modern subordinationist movements instantiate the ante-Nicene reference frame they cite, or a novel organizational arrangement wearing it?',
    'Compare documented organizational structure, worship practice, and enforcement of second- and third-century subordinationist communities with the modern movements claiming restoration.',
    'If novel, the lineage authority grounding is partly theatrical, theater_ratio rises, and the reference_frame declaration weakens toward a legitimating citation rather than a lived continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_vs_innovation, empirical, 'Whether the modern movements restore or merely cite the ante-Nicene frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 100, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t100, john_1_1_logos__subordinationist, theater_ratio, 100, 0.12).
narrative_ontology:measurement(john_tr_t325, john_1_1_logos__subordinationist, theater_ratio, 325, 0.28).
narrative_ontology:measurement(john_tr_t500, john_1_1_logos__subordinationist, theater_ratio, 500, 0.32).
narrative_ontology:measurement(john_tr_t1550, john_1_1_logos__subordinationist, theater_ratio, 1550, 0.35).
narrative_ontology:measurement(john_tr_t1900, john_1_1_logos__subordinationist, theater_ratio, 1900, 0.28).
narrative_ontology:measurement(john_tr_t2025, john_1_1_logos__subordinationist, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(john_be_t100, john_1_1_logos__subordinationist, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(john_be_t325, john_1_1_logos__subordinationist, base_extractiveness, 325, 0.55).
narrative_ontology:measurement(john_be_t500, john_1_1_logos__subordinationist, base_extractiveness, 500, 0.62).
narrative_ontology:measurement(john_be_t1550, john_1_1_logos__subordinationist, base_extractiveness, 1550, 0.45).
narrative_ontology:measurement(john_be_t1900, john_1_1_logos__subordinationist, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(john_be_t2025, john_1_1_logos__subordinationist, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t100, john_1_1_logos__subordinationist, suppression_requirement, 100, 0.22).
narrative_ontology:measurement(john_su_t325, john_1_1_logos__subordinationist, suppression_requirement, 325, 0.58).
narrative_ontology:measurement(john_su_t500, john_1_1_logos__subordinationist, suppression_requirement, 500, 0.7).
narrative_ontology:measurement(john_su_t1550, john_1_1_logos__subordinationist, suppression_requirement, 1550, 0.42).
narrative_ontology:measurement(john_su_t1900, john_1_1_logos__subordinationist, suppression_requirement, 1900, 0.62).
narrative_ontology:measurement(john_su_t2025, john_1_1_logos__subordinationist, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% The kernel john_1_1_logos decomposes into three readings making mutually exclusive claims about the same text: the Logos as created first agent (this file), as uncreated consubstantial second person (john_1_1_logos__orthodox_christological), and as personified wisdom with no hypostasis (john_1_1_logos__non_incarnational_monotheist). Each carries its own ε, victim set, and worship constraint; the victim sets are near-mirror images — this reading's victims are the high-church traditions, while under the orthodox reading the subordinationist communities become the constrained party. Upstream/downstream structure: the ante-Nicene subordinationist consensus is the historically prior claim; the Nicene settlement was constructed against it; the modern restorationist movements cite the prior claim as evidence against the settlement. Family members are linked pairwise via affects_constraints per the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
