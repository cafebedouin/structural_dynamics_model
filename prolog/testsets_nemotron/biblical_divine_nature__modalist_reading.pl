% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-22
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist Reading of Divine Nature (Sabellianism)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   Modalism (Sabellianism) reads Father, Son, and Spirit as three sequential
 *   modes/roles of a single divine person — the one God operating as Father
 *   in the economy of creation and law, as Son in the economy of redemption,
 *   as Spirit in the economy of sanctification. It emerged c. 190-220 CE
 *   (Noetus, Praxeas, Sabellius) as a 'monarchian' alternative to both
 *   Logos-subordinationism and the emerging three-hypostasis orthodoxy. The
 *   reading was condemned at Rome (c. 215) and finally anathematized at
 *   Constantinople 381. Its structural career: early genuine coordination
 *   offer for non-philosophical monotheism → imperial political tool for
 *   unity → suppressed 'heresy' whose function was absorbed into the orthodox
 *   synthesis (one ousia) while its form was rejected. The constraint is the
 *   modalist reading itself as an enforced/imposed doctrinal boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.35).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.68).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, scaffold).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist Reading of Divine Nature (Sabellianism)").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).
narrative_ontology:has_sunset_clause(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, '349ea3d6-519e-4144-bd3c-1967f4d74ae3').
narrative_ontology:cs_kernel_codification('349ea3d6-519e-4144-bd3c-1967f4d74ae3', fixed_text).
narrative_ontology:cs_authority_grounding('349ea3d6-519e-4144-bd3c-1967f4d74ae3', lineage).
narrative_ontology:cs_interpretation_layer_present('349ea3d6-519e-4144-bd3c-1967f4d74ae3').
narrative_ontology:cs_reading_relation('349ea3d6-519e-4144-bd3c-1967f4d74ae3', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('349ea3d6-519e-4144-bd3c-1967f4d74ae3', biblical_divine_nature__unitarian_reading, influences).
narrative_ontology:cs_axiom('349ea3d6-519e-4144-bd3c-1967f4d74ae3', foundational, divine_person_is_numerically_one).
narrative_ontology:cs_axiom_status(divine_person_is_numerically_one, holdable).
narrative_ontology:cs_axiom_grounding('349ea3d6-519e-4144-bd3c-1967f4d74ae3', divine_person_is_numerically_one, deontological).
narrative_ontology:cs_axiom('349ea3d6-519e-4144-bd3c-1967f4d74ae3', foundational, philosophical_distinctions_obscure_scripture).
narrative_ontology:cs_axiom_status(philosophical_distinctions_obscure_scripture, holdable).
narrative_ontology:cs_axiom_grounding('349ea3d6-519e-4144-bd3c-1967f4d74ae3', philosophical_distinctions_obscure_scripture, deontological).
narrative_ontology:cs_axiom('349ea3d6-519e-4144-bd3c-1967f4d74ae3', secondary, father_son_spirit_are_sequential_economies).
narrative_ontology:cs_axiom_status(father_son_spirit_are_sequential_economies, holdable).
narrative_ontology:cs_axiom_grounding('349ea3d6-519e-4144-bd3c-1967f4d74ae3', father_son_spirit_are_sequential_economies, conventional).
narrative_ontology:cs_reference_frame('349ea3d6-519e-4144-bd3c-1967f4d74ae3', scriptural_monarchia).
narrative_ontology:cs_drift_state('349ea3d6-519e-4144-bd3c-1967f4d74ae3', post_constantinople_381, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('349ea3d6-519e-4144-bd3c-1967f4d74ae3', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, christological_centrists).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, anti_philosophical_pietists).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, imperial_mediators).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, trinitarian_orthodox).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, unitarian_strict_monotheists).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, philosophical_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, modalist_bishops).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, numerical_monotheism).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, christological_simplicity).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, scriptural_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote a single-person God who operates in sequential modes (Father in creation/law, Son in redemption, Spirit in sanctification) to preserve numerical monotheism without philosophical apparatus. Their authority depends on this reading; abandoning it collapses their episcopal legitimacy and the congregations they gathered around Christ-centered simplicity.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_bishops, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__modalist_reading, modalist_bishops, beneficiary).

% Lay believers and lower clergy who want Jesus to be fully God without Greek metaphysics (hypostasis/ousia). They gain a worship object that is identically the Father, avoiding the perceived ditheism of Logos-Christology. Their exit is constrained by community ties and lack of alternative simple Christologies.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, christological_centrists, beneficiary,
    organized, biographical, constrained, regional).

% Monastic and devotional circles who experience the Trinity as a barrier to direct encounter with God. Modalism offers unmediated access: the one they pray to is the one who died for them. They benefit affectively; their exit is constrained by devotional formation and community.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, anti_philosophical_pietists, beneficiary,
    moderate, biographical, constrained, local).

% Roman emperors and court theologians (e.g., Callistus, early Constantius II) who see modalism as a unity formula that bypasses the Homoousian/Homoiousian deadlock. They gain a doctrinal tool for imperial cohesion. Their exit is mobile — they shift to whichever reading serves political unity.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, imperial_mediators, beneficiary,
    institutional, immediate, mobile, continental).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__modalist_reading, imperial_mediators, agenda_setter).

% Defend three hypostases in one ousia as the only coherent account of Scripture's relational language (Father sends Son, Son sends Spirit). They pay the cost of philosophical complexity and perpetual conciliar enforcement. Their identity is locked to the Nicene-Constantinopolitan settlement; exit means schism or apostasy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_orthodox, payer,
    institutional, generational, identity_locked, universal).

% Groups (e.g., Paul of Samosata's followers, later Socinian trajectories) who read modalism as collapsing the Father into the Son, thereby losing the transcendent Father. They pay exclusion from both catholic and modalist communion. Exit is constrained by the binary: tritheism vs. Sabellianism leaves no safe monotheism.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, unitarian_strict_monotheists, payer,
    organized, biographical, constrained, regional).

% Origen, Athanasius, the Cappadocians — their life's work is the metaphysics of distinction-in-unity. Modalism renders their project incoherent. They pay professional and epistemic displacement. Exit is constrained by career investment in the conceptual apparatus.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, philosophical_theologians, payer,
    organized, generational, constrained, universal).

% The conciliar bodies that anathematized modalism (Nicaea 325 implicitly, Constantinople 381 explicitly). They set the boundary of orthodoxy. Their exit is arbitrage-grade: they define the field and can redefine it (as later councils did).
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, council_fathers_325_381, agenda_setter,
    institutional, generational, arbitrage, universal).

% Historians of early Christianity who trace modalism as a competing trajectory suppressed by the imperial-church alliance. They observe the structural dynamics without institutional stake.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modern_biblical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a cognitively simple, scripturally grounded account of how the one God of Israel relates to Jesus and the Spirit — no philosophical distinctions, no hierarchical subordination, just one divine subject appearing in three sequential economies.
% TRANSFER_FUNCTION: Moves epistemic authority from philosophical theologians (who require Greek metaphysics) to bishops and communities who read Scripture plainly; moves devotional focus from a triune relationality to a single Christ-centered object; moves imperial cohesion from conciliar compromise to a unity formula the emperor can enforce.
% ABSENT_VOICES: Jewish monotheists of the period who would object that sequential modalism still divinizes a human (Jesus) — a violation of the Shema that neither Trinitarian nor Unitarian readings fully escape. Also absent: the laity in Antioch and Alexandria who rioted over competing christologies but whose voices survive only in hostile reports.
% DISAPPEARANCE_RATIONALE: If modalism vanished overnight (as it largely did post-381), the christological map contracts to a binary: Nicene orthodoxy vs. various subordinationisms. The 'simple' monotheist option disappears, forcing seekers of non-philosophical Christology into either Arianism or crypto-unitarianism. The imperial church loses a negotiating position it used against both extremes.
% FOUNDING_PROBLEM: How to confess Jesus as God without either (a) introducing a second divine principle (ditheism) or (b) adopting Greek philosophical categories (ousia/hypostasis) that have no biblical warrant and fracture the church.
% FOUNDING_PROBLEM_CORROBORATION: Modalist proponents (Sabellius, Callistus, Praxeas) attest the problem is live: the church still uses Greek metaphysics to explain the God of the Bible. Trinitarian theologians (Athanasius, Gregory of Nazianzus) attest the problem is solved: the philosophical apparatus is the only way to preserve both monotheism and the distinctness of Father/Son/Spirit. Modern historians (Harnack, Williams, Ayres) corroborate from outside the benefiting parties that the founding problem was genuine and the modalist solution was structurally viable until imperially suppressed.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.35) is moderate: the reading extracts epistemic compliance (you must confess the modalist formula to be in communion) and devotional reorientation (pray to the one God who died), but does not extract material rents. Suppression (0.68) is high: persistence required active conciliar anathema, imperial exile of bishops, and burning of texts. Theater (0.25) reflects that the modalist formula performed unity while the real coordination work was done by the imperial-church alliance. Accessibility collapse (0.55): alternatives (Logos-Christology, Nicene orthodoxy) remained intellectually available but were ecclesiastically inaccessible once the imperial boundary hardened. Resistance (0.62): modalism resisted through multiple revivals (Paul of Samosata, Photinus, Servetus, Oneness Pentecostalism) showing the coordinate demand persists.
 *
 * PERSPECTIVAL GAP:
 *   From the modalist bishop's seat: a genuine coordination scaffold (temporary unity formula) that was cut down by philosophical imperialism. From the trinitarian seat: a snare that collapses the relationality of salvation into a unipersonal performance. From the imperial seat: a rope that solved a coordination problem until it became a liability. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Modalist bishops are agenda_setters who also benefit (identity_locked: their office and self-concept are fused to the reading). Christological centrists and pietists are beneficiaries with constrained exit (community-bound). Imperial mediators are mobile beneficiaries who use the reading instrumentally. Trinitarian orthodox, unitarians, and philosophical theologians are payers: they bear the cost of exclusion, epistemic marginalization, and having their frameworks declared heretical. All three payer groups are identity_locked or constrained — their professional and communal identities are constituted by the very distinctions modalism collapses.
 *
 * MANDATROPHY ANALYSIS:
 *   The modalist reading was a scaffold: it carried a sunset clause in the sense that its proponents (Callistus, early imperial mediators) treated it as a transitional unity formula pending a more developed consensus. The mandate (simple monotheistic Christology) outlived its function when the Nicene-Constantinopolitan settlement provided a more robust — though philosophically complex — alternative. The mandatrophy is resolved: the constraint persists only as a heresiological category (Sabellianism) and in modern revivals (Oneness Pentecostalism) that are structurally distinct from the ancient reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modalist_suppression_mechanism,
    'Was the suppression of modalism primarily structural (imperial conciliar enforcement) or internalized (theological conviction that it destroys Christian distinctiveness)?',
    'Trace post-381 survival: if modalist communities persist underground without structural enforcement, internalized suppression is low; if they vanish unless structurally protected, internalized suppression is high. Compare with Donatist persistence.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure — the theological conviction that modalism loses the Gospel carries the suppression forward without enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modalist_suppression_mechanism, empirical, 'Structural vs. internalized suppression in doctrinal enforcement').

omega_variable(
    modalist_coordination_genuineness,
    'Did modalism offer a genuine coordination solution (simple monotheistic Christology for non-philosophical believers) or was it always an imperial unity instrument masquerading as theology?',
    'Compare the pre-imperial modalist communities (Rome c. 190-220, Antioch c. 260) with the imperial-phase modalism (Callistus, Constantius II). If early communities show organic growth without imperial backing, coordination is genuine.',
    'If genuine coordination, the scaffold classification holds; if imperial instrument from the start, it is a snare wearing a scaffold''s sunset clause.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modalist_coordination_genuineness, conceptual, 'Whether the modalist reading''s coordination function was authentic or manufactured').

omega_variable(
    kernel_framing_ambiguity,
    'Is the biblical_divine_nature kernel best framed as (a) a doctrinal proposition about God''s being, (b) a liturgical/practical rule for worship, or (c) a political boundary marker for imperial communion?',
    'Analyze which framing each sibling reading privileges. Trinitarians privilege (a) and (b); modalists privilege (b) and reject (a); unitarians privilege (a) differently; imperial actors privilege (c). The engine''s cs_structure classification depends on this framing choice.',
    'If (c) is primary, all three readings are instruments of a political kernel; if (a) is primary, the readings are genuine theological competitors. Changes the authority_grounding assignment in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Commitment-system framing under-determination for the divine nature kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 190, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t190, biblical_divine_nature__modalist_reading, theater_ratio, 190, 0.05).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t215, biblical_divine_nature__modalist_reading, theater_ratio, 215, 0.1).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t260, biblical_divine_nature__modalist_reading, theater_ratio, 260, 0.18).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t325, biblical_divine_nature__modalist_reading, theater_ratio, 325, 0.22).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t360, biblical_divine_nature__modalist_reading, theater_ratio, 360, 0.25).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_tr_t381, biblical_divine_nature__modalist_reading, theater_ratio, 381, 0.25).

% Extraction over time
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t190, biblical_divine_nature__modalist_reading, base_extractiveness, 190, 0.15).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t215, biblical_divine_nature__modalist_reading, base_extractiveness, 215, 0.22).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t260, biblical_divine_nature__modalist_reading, base_extractiveness, 260, 0.28).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t325, biblical_divine_nature__modalist_reading, base_extractiveness, 325, 0.35).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t360, biblical_divine_nature__modalist_reading, base_extractiveness, 360, 0.32).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_be_t381, biblical_divine_nature__modalist_reading, base_extractiveness, 381, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t190, biblical_divine_nature__modalist_reading, suppression_requirement, 190, 0.3).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t215, biblical_divine_nature__modalist_reading, suppression_requirement, 215, 0.42).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t260, biblical_divine_nature__modalist_reading, suppression_requirement, 260, 0.55).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t325, biblical_divine_nature__modalist_reading, suppression_requirement, 325, 0.68).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t360, biblical_divine_nature__modalist_reading, suppression_requirement, 360, 0.65).
narrative_ontology:measurement(biblical_divine_nature__modalist_reading_su_t381, biblical_divine_nature__modalist_reading, suppression_requirement, 381, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__modalist_reading, 0.1).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, nicene_creed_authority).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, constantinopolitan_creed_authority).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, imperial_church_unity_policy).

% DUAL FORMULATION NOTE:
% Part of the biblical_divine_nature constraint family (kernel_id: biblical_divine_nature). The modalist reading offers a coordinate-free monotheism; the trinitarian reading offers a metaphysically articulated monotheism; the unitarian reading offers a hierarchical monotheism. Their epsilon values differ: modalist (0.35, scaffold), trinitarian (0.25, rope/tangled_rope), unitarian (0.45, tangled_rope). The modalist reading was the primary competitor the trinitarian synthesis had to absorb and exclude.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__modalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(biblical_divine_nature__modalist_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
