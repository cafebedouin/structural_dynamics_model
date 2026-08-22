% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__arian_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Christology: Christ Created and Subordinate (Non-Homoousios)
 *   domain: ecclesiastical/theological/political
 *
 * SUMMARY:
 *   The Arian reading of Christ's nature — that He is created, subordinate,
 *   and not of identical substance (homoousios) with the Father — emerged as
 *   the primary challenge to the Nicene settlement after the First Council of
 *   Nicaea (325). Under emperors Constantine II and Constantius II, the
 *   Arian-leaning anti-Nicene bishops achieved substantial institutional
 *   dominance in the Eastern Mediterranean. They coordinated through
 *   episcopal networks, imperial patronage, and theological teaching. The
 *   constraint operates as a tangled rope: a genuine theological coordination
 *   problem (preserving monotheism while accounting for Christ's role)
 *   coupled with extractive institutional enforcement that subordinated
 *   Nicene bishops and exiled Athanasius multiple times. The constraint's
 *   persistence depended on active imperial enforcement; its classification
 *   shifts as imperial will shifts. By 381 (First Council of Constantinople),
 *   the pro-Nicene reading had regained definitional and institutional
 *   authority, but the Arian reading had extracted decades of institutional
 *   suppression and doctrinal contestation.
 *
 * KEY AGENTS:
 *   - anti_nicene_bishop_coalition: institutional beneficiary and primary theological agenda-setter; coordinated through Eusebius and eastern episcopal networks; extracted institutional authority
 *   - imperial_arianism_faction: institutional beneficiary and enforcement agenda-setter; emperors Constantine II and Constantius II played factions against each other, used theological preference as administrative tool
 *   - pro_nicene_bishops: institutional victims; exiled (Athanasius 5 times), deposed, marginalized during Arian ascendancy; bore the costs of doctrinal resistance
 *   - nicene_council_authority: excluded from enforcement; had no independent machinery to sustain its verdict once imperial preference shifted
 *   - theological_traditionalists: excluded; lacked institutional standing despite spiritual authority; not present at power-setting councils
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.68).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.72).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Christology: Christ Created and Subordinate (Non-Homoousios)").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "ecclesiastical/theological/political").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, '6dcb77b8-22bf-4444-a4a3-0f6c3847ec57').
narrative_ontology:cs_kernel_codification('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', fixed_text).
narrative_ontology:cs_authority_grounding('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', lineage).
narrative_ontology:cs_interpretation_layer_present('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57').
narrative_ontology:cs_reading_relation('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', homoousios_christology__semi_arian_reading, coexists_with).
narrative_ontology:cs_axiom('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', foundational, christ_is_created_creature).
narrative_ontology:cs_axiom_status(christ_is_created_creature, holdable).
narrative_ontology:cs_axiom_grounding('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', christ_is_created_creature, empirically_contingent).
narrative_ontology:cs_axiom('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', foundational, father_alone_unbegotten_divine).
narrative_ontology:cs_axiom_status(father_alone_unbegotten_divine, holdable).
narrative_ontology:cs_axiom_grounding('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', father_alone_unbegotten_divine, deontological).
narrative_ontology:cs_reference_frame('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', apostolic_subordinationist_tradition).
narrative_ontology:cs_drift_state('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', post_constantinople_381_authority_shift, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6dcb77b8-22bf-4444-a4a3-0f6c3847ec57', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, anti_nicene_bishop_coalition).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, imperial_arianism_faction).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, pro_nicene_bishops).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, nicene_theological_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Eastern bishops (Eusebius of Caesarea, Eusebius of Nicomedia, and their networks) coordinating theological resistance to homoousios and defending subordinationist interpretation. They set the agenda through episcopal councils, synods, and theological teaching. They benefited from imperial favor under Constantine II and Constantius II, gaining appointments to major sees and authority to enforce Arian teaching. Their exit would require abandoning their theological reading and submitting to Nicene authority — constrained by conviction and by institutional position.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, anti_nicene_bishop_coalition, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, anti_nicene_bishop_coalition, beneficiary).

% Emperors Constantine II (337–340) and especially Constantius II (341–361) who favored the Arian reading as a framework for theological adjudication independent of the Nicene council. They benefited from the theological dispute itself: by maintaining uncertainty about orthodoxy, they retained leverage to appoint bishops, exile dissenters, and position themselves as mediators. Their exit was mobile because theology was a tool of imperial policy, not a conviction — when imperial policy shifted (under Julian toward paganism, then under Theodosius toward Nicene orthodoxy), the empire changed alignment.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, imperial_arianism_faction, beneficiary,
    institutional, biographical, mobile, continental).

% Bishops and theologians who maintained homoousios (notably Athanasius of Alexandria, exiled five times). They bore the costs of resistance: exile, deposition from sees, confiscation of church property, suppression of their theological teaching. Their exit options were trapped because homoousios was their central doctrinal commitment and institutional identity — abandoning it meant ceasing to be Nicene bishops. They had no way to exit the constraint without surrendering their position entirely.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_bishops, payer,
    institutional, generational, trapped, continental).

% The First Council of Nicaea (325) as an institutional body that had no independent enforcement machinery. Once imperial will shifted toward Arianism, the council's authority evaporated. It is a non-agent entity (a body politic, a council-as-decision-body) but included to capture the structural exclusion of the council from power after Constantine's death.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, nicene_council_authority, excluded,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(homoousios_christology__arian_reading, nicene_council_authority).

% Monastic and ascetic theologians (Desert Fathers, Syrian monastics) who held theological commitments but lacked imperial or episcopal standing. They would have contested both the Nicene innovation of homoousios and the Arian political instrumentalization of theology, but they were structurally absent from imperial councils and episcopal synods. Their exclusion is the absence of their voice from the power-setting apparatus, despite their spiritual authority.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, theological_traditionalists, excluded,
    moderate, generational, constrained, local).

% The Western Roman bishops, especially Rome itself, who maintained alignment with Nicene theology but were temporarily marginalized from eastern imperial politics. They observed the eastern theological dispute and occasionally intervened through synods and letters, but lacked the institutional leverage to enforce Nicene orthodoxy in the east during the height of Constantius II's Arian favor. Their position stabilized when Theodosius took power and made Nicene orthodoxy imperial policy.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, rome_episcopal_authority, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__arian_reading, imperial_arianism_faction).
narrative_ontology:fixing_cost_class(homoousios_christology__arian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of defining Christ's metaphysical relationship to God the Father in a way that preserves God's radical monotheistic unity (the unbegotten Father alone as fully divine) and Christ's genuine createdness as the Logos and first creature. The Arian reading coordinates around the principle that Christ, though exalted and eternal, remains fundamentally subordinate and dependent on the Father's will — a coordination that anchors in a particular interpretation of Proverbs 8:22 (the Sophia passage), John's Prologue, and pre-Nicene subordinationist tradition.
% TRANSFER_FUNCTION: Moves doctrinal authority, episcopal appointments, and institutional standing from the Nicene settlement toward the anti-Nicene episcopal networks. Exiles pro-Nicene bishops (Athanasius multiple times), transfers their sees to Arian-sympathetic appointees, and suppresses the teaching of homoousios through imperial decree and book-burning. The extraction is institutional (control of sees, teaching authority, succession) and is enforced through imperial coercion (exile, deposition, confiscation).
% ABSENT_VOICES: Monastic and ascetic theologians with deep theological convictions but no imperial or episcopal standing (Desert Fathers, Syrian ascetics). Lay congregants in major cities who followed bishops into exile but had no seat in councils. Theological systematizers in the Eastern provinces who might have proposed alternatives to both homoousios and full Arianism (the semi-Arian position emerges partly from this gap). Rome and the Western bishops, who remained committed to Nicene theology but were excluded from the apparatus that set eastern imperial policy under Constantius II.
% DISAPPEARANCE_RATIONALE: If the Arian reading and its institutional enforcement vanished overnight, exiled Nicene bishops (especially Athanasius) would return, eastern sees would reorganize around homoousios teaching, the imperial leverage mechanism (using theological preference as a tool of appointment and exile) would collapse, and the ecclesiastical order would shift back toward Nicene institutional dominance. The constraint's persistence is institutional, dependent on active enforcement; without it, the landscape reorganizes.
% FOUNDING_PROBLEM: How to preserve rigorous monotheism (the unbegotten Father as radically singular in divinity) while accounting for the New Testament's exaltation of Christ as the Logos, the Word through whom all things were made, without implying either polytheism or a compromise in God's absolute uniqueness. The Arian reading solved this by asserting that Christ, though the first and highest creature and the mediator of creation, is not of identical substance with the Father — a created Logos who is eternally dependent on the Father's will.
% FOUNDING_PROBLEM_CORROBORATION: The anti-Nicene bishops and their theological successors attest the founding problem remains live: homoousios risks compromising monotheism by treating Father and Son as co-equal substances. Athanasius and later pro-Nicene theology attest the founding problem is reframed by homoousios, not solved: the real problem is not 'how do we preserve monotheism?' (all readings affirm that) but 'what does Scripture and tradition tell us about Christ's relationship to the Father?' — and independent patristic analysis from sources outside the anti-Nicene faction (Augustine, Jerome, and the ecumenical council at Constantinople in 381) attests that subordinationism as the Arian reading framed it was superseded by pro-Nicene metaphysical precision (the development of hypostasis language and later Trinitarian theology). No voice outside the anti-Nicene faction corroborates that the founding problem is still live in its Arian formulation.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__arian_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__arian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval: the anti-Nicene coalition gains institutional ground (especially under Constantius II, 337–361), concentrating sees and teaching authority; extractiveness plateaus after 370 as the constraint's institutional dominance reaches saturation and begins to face organized resistance. Suppression mirrors extractiveness but slightly amplifies it (0.72 at end): the constraint requires active suppression (exile decrees, book-burning, appointment coercion) to persist — it cannot rely on participant preference. Theater ratio rises from 0.25 to 0.41: increasingly, the constraint's enforcement activity focuses on defending the theological reading against scholarly challenge and Nicene resistance, rather than on solving the original coordination problem (how to theologize monotheism). The measurement series track one shared time grid (7-point backbone shared across all three metrics) so temporal analysis has even sampled evidence.
 *
 * PERSPECTIVAL GAP:
 *   The anti-Nicene episcopal coalition and the emperor experience the constraint as genuine coordination: they coordinated around a reading that preserves monotheism and traditional creationist subordinationism, and they enforced it through legitimate episcopal and imperial authority. The pro-Nicene bishops experience it as extraction: they were exiled, deposed, and suppressed for maintaining an alternative reading; their institutional position was subordinated through no choice of their own. The constraint's classification should diverge by seat. From the agenda-setter seat (anti-Nicene coalition + empire), the constraint computes as more coordination, less extraction. From the payer seat (exiled pro-Nicene bishops), it computes as higher extraction, lower coordination benefit. The engine derives directionality from the beneficiary/victim structure and exit options; the gap reflects structural asymmetry, not disagreement about metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The anti-nicene_bishop_coalition and imperial_arianism_faction are the structural beneficiaries: they established institutional dominance, controlled episcopal sees, and extracted authority from the Nicene apparatus. Their directionality is low (toward the beneficiary end, d ≈ 0.15–0.25): they set the rules, collected the institutional gains, and faced no serious exit cost. The pro_nicene_bishops are the structural victims: they were exiled, deposed, and suppressed; their exit options were constrained (either abandon the homoousios commitment or face institutional death). Their directionality is high (toward the target end, d ≈ 0.75–0.85): they paid heavily through suppression and had limited choice. The nicene_council_authority is a non-agent (a body politic, not an individual actor), included for narrative completeness but not feeding directionality computation. Theological_traditionalists are excluded, so their directionality is not derived from this constraint — they sit outside the power apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to preserve monotheism while accounting for Christ's exaltation) is CONTESTED in status: the anti-Nicene reading attest it is live; pro-Nicene theology and later Catholic tradition attest it is reframed by the homoousios framework. The disappearance verdict is world_rearranges: the constraint's persistence is institutional, not natural or inevitable. The mismatch of contested status + world_rearranges, combined with the rising theater_ratio (0.41) and sustained high suppression (0.72), indicates that the Arian reading's founding problem has become a zombie — the constraint persists through imperial enforcement and institutional inertia, not because the founding problem itself is live. By 381, the constraint is candidate for mandatrophy resolution: its founding problem is superseded (the Constantinople council restored Nicene authority), the arrangement persists (Arianism continues in some eastern sees), and the suppression machinery has been repurposed (no longer suppressing Nicene bishops, now defending against their resurgence). The theater_ratio captures this shift: increasing shares of enforcement energy go to defending the reading against challenges, rather than solving the original coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_dependent_substance_claim,
    'Is the claim that Christ is subordinate and non-homoousios a structural property of reality (a claim about metaphysics that is true or false independent of reading), or is it a reading-dependent theological commitment grounded in an interpretation of Scripture, tradition, and philosophical framework?',
    'This is a conceptual ambiguity between metaphysical realism (the claim is true or false about Christ''s nature) and reading-based constructivism (the claim is true within the framework of anti-Nicene interpretation). No empirical observation resolves this; different theological traditions answer it differently. The engine routes this through axiom_grounding and reference_frame analysis in the cs_structure block.',
    'If the claim is metaphysically realist, the Arian reading is either right or wrong about Christ''s nature. If it is reading-dependent, then ''truth'' is internal to each reading''s epistemic framework, and classification depends on which reading anchors the observer. The constraint type (tangled_rope) is robust to this ambiguity because extraction and enforcement are measured independent of metaphysical status — the extraction is institutional, not metaphysical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_dependent_substance_claim, conceptual, 'The metaphysical vs. reading-dependent status of subordinationist Christology.').

omega_variable(
    imperial_leverage_vs_theological_conviction,
    'To what extent did the Arian reading persist because it was theologically superior (more coherent, more scriptural, more traditional) versus because it had imperial backing and institutional enforcement machinery behind it?',
    'Counterfactual: absent imperial enforcement, would the Arian reading have maintained institutional dominance among bishops? Historical evidence: under Julian (who disfavored both Nicene and Arian orthodoxy equally), Arian bishops reorganized quickly to recover ground, suggesting some independent institutional cohesion. Under the Nicene-favoring Theodosius I (after 378), the same bishops were suppressed rapidly. This suggests imperial backing amplified but did not solely create the reading''s dominance.',
    'If extraction is primarily institutional/political (imperial leverage), the constraint is a clear tangled_rope (genuine theological coordination problem + extractive political enforcement). If theological conviction is the dominant mechanism, the constraint approaches rope (shared commitment, low suppression needed). The measured suppression (0.72) and theater_ratio (0.41) support the first reading (institutional + coercive), but the ambiguity matters for assigning agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_leverage_vs_theological_conviction, empirical, 'The relative weight of theological merit vs. imperial patronage in the Arian reading''s institutional dominance.').

omega_variable(
    subordinationism_as_extraction_cover,
    'Did the anti-Nicene coalition genuinely believe that homoousios endangered monotheism, or did the subordinationist theology serve as a cover story for maintaining distributed episcopal power against the Nicene council''s centralizing authority?',
    'Textual analysis of anti-Nicene theological writings (Eusebius of Caesarea, Eusebius of Nicomedia, Athanasius''s opponents) to assess whether theological arguments track a substantive philosophical problem or rationalize a power-preserving position. Cross-check against the correlation between theological intensity and political stakes: do the most doctrinally rigorous anti-Nicene arguments arise in high-stakes political contexts, or in safe contexts?',
    'If the theology is genuine, the coordination function (preserving a particular understanding of monotheism) is real, and the constraint is a hybrid tangled_rope with both coordination and extraction elements. If subordinationism is primarily cover, the constraint approaches snare (extraction machinery dressed in theological language). The rising theater_ratio (0.41) and the fact that suppression persists even as theological consensus shifts (after 381) suggests some cover-story element, but the measured extractiveness (0.68) is high enough to support both a genuine coordination dispute and extractive institutional competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinationism_as_extraction_cover, empirical, 'Whether subordinationist theology is genuine doctrinal conviction or a cover for institutional power preservation.').

omega_variable(
    kernel_reading_containment,
    'Is the distinction between the Arian, pro-Nicene, and semi-Arian readings a distinction within ONE theological commitment (three interpretations of the homoousios doctrine''s meaning), or are these THREE DIFFERENT theological commitments with incompatible metaphysical and soteriological implications?',
    'Examine whether later Catholic/Orthodox theology can subsume all three readings as legitimate interpretations of a single doctrine (ecumenical reconciliation path), or whether the readings are definitionally incompatible (one must be true and others false). The fact that Constantinople I (381) vindicated pro-Nicene homoousios and repudiated Arian subordinationism suggests incompatibility, but ongoing Eastern Orthodox-Coptic debates suggest some ambiguity in how subordinationist language maps to pro-Nicene metaphysics.',
    'If readings are contained within one kernel and potentially reconcilable, then the constraint system is CLOSED within the commitment apparatus: all readings feed back to a single authority structure (eventually the ecumenical council). If readings are genuinely incompatible, the kernel has FRAGMENTED into incompatible theological traditions, and the constraint system is OPEN (each reading anchors a different authority structure that may never reconcile). This affects the cs_structure.kernel_codification classification and the network.affects_constraints edges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_containment, conceptual, 'Whether the three readings are interpretive variants of one kernel or incompatible theological commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 320, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t320, homoousios_christology__arian_reading, theater_ratio, 320, 0.25).
narrative_ontology:measurement(homo_tr_t330, homoousios_christology__arian_reading, theater_ratio, 330, 0.29).
narrative_ontology:measurement(homo_tr_t340, homoousios_christology__arian_reading, theater_ratio, 340, 0.34).
narrative_ontology:measurement(homo_tr_t350, homoousios_christology__arian_reading, theater_ratio, 350, 0.38).
narrative_ontology:measurement(homo_tr_t360, homoousios_christology__arian_reading, theater_ratio, 360, 0.4).
narrative_ontology:measurement(homo_tr_t370, homoousios_christology__arian_reading, theater_ratio, 370, 0.41).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__arian_reading, theater_ratio, 381, 0.41).

% Extraction over time
narrative_ontology:measurement(homo_be_t320, homoousios_christology__arian_reading, base_extractiveness, 320, 0.45).
narrative_ontology:measurement(homo_be_t330, homoousios_christology__arian_reading, base_extractiveness, 330, 0.52).
narrative_ontology:measurement(homo_be_t340, homoousios_christology__arian_reading, base_extractiveness, 340, 0.58).
narrative_ontology:measurement(homo_be_t350, homoousios_christology__arian_reading, base_extractiveness, 350, 0.64).
narrative_ontology:measurement(homo_be_t360, homoousios_christology__arian_reading, base_extractiveness, 360, 0.66).
narrative_ontology:measurement(homo_be_t370, homoousios_christology__arian_reading, base_extractiveness, 370, 0.68).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__arian_reading, base_extractiveness, 381, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t320, homoousios_christology__arian_reading, suppression_requirement, 320, 0.48).
narrative_ontology:measurement(homo_su_t330, homoousios_christology__arian_reading, suppression_requirement, 330, 0.56).
narrative_ontology:measurement(homo_su_t340, homoousios_christology__arian_reading, suppression_requirement, 340, 0.63).
narrative_ontology:measurement(homo_su_t350, homoousios_christology__arian_reading, suppression_requirement, 350, 0.68).
narrative_ontology:measurement(homo_su_t360, homoousios_christology__arian_reading, suppression_requirement, 360, 0.71).
narrative_ontology:measurement(homo_su_t370, homoousios_christology__arian_reading, suppression_requirement, 370, 0.72).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__arian_reading, suppression_requirement, 381, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__arian_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, homoousios_christology__semi_arian_reading).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel decomposes into three reading-specific constraints: arian_reading (this file), pro_nicene_reading, and semi_arian_reading. Each reading instantiates a distinct constraint with distinct ε, beneficiary/victim structure, and stakeholder composition. The readings share a kernel (the contested metaphysical claim about Christ's substance) but differ in how they interpret Scripture, tradition, and philosophical resources. Network edges link the readings through cs_structure.reading_relations (forecloses/coexists_with/influences) and establish ε-variance across the constraint family (ε is reading-specific, not topic-specific). The three constraints are one family because they all depend on the same textual/theological kernel; they are three constraints because their ε values diverge substantially (arian_reading: 0.68; pro_nicene_reading expected to be lower, ~0.35–0.45 under Nicene authority; semi_arian_reading expected to be intermediate, ~0.50–0.60).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
