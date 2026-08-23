% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Reading of John 1:1 (Logos as Created Subordinate Agent)
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   The subordinationist reading of John 1:1 ('In the beginning was the Word,
 *   and the Word was with God, and the Word was God') interprets 'the Word
 *   was God' (theos en ho logos) as qualitative — the Logos is divine in a
 *   derived, subordinate sense — not ontological identity with the Father.
 *   This reading, associated with Arius (c. 256-336) and condemned at Nicaea
 *   (325), persists in various forms: historical Arianism, Socinianism,
 *   Unitarianism, Jehovah's Witnesses, and some biblical unitarian movements.
 *   It constrains worship (Logos venerated with proskynesis but not latria),
 *   sacramental theology (no real presence grounded in divine nature), and
 *   ecclesial authority (no magisterium grounded in Christ's full divinity).
 *   The constraint is a tangled rope: it coordinates a coherent monotheistic
 *   Christology for its adherents while extracting authority and sacramental
 *   coherence from high-church traditions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.58).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.62).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Reading of John 1:1 (Logos as Created Subordinate Agent)").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, '19ea1e50-96a8-43e0-8896-a581ca3a107c').
narrative_ontology:cs_kernel_codification('19ea1e50-96a8-43e0-8896-a581ca3a107c', fixed_text).
narrative_ontology:cs_authority_grounding('19ea1e50-96a8-43e0-8896-a581ca3a107c', lineage).
narrative_ontology:cs_interpretation_layer_present('19ea1e50-96a8-43e0-8896-a581ca3a107c').
narrative_ontology:cs_reading_relation('19ea1e50-96a8-43e0-8896-a581ca3a107c', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('19ea1e50-96a8-43e0-8896-a581ca3a107c', john_1_1_logos__non_incarnational_monotheist, coexists_with).
narrative_ontology:cs_axiom('19ea1e50-96a8-43e0-8896-a581ca3a107c', foundational, logos_created_not_coeternal).
narrative_ontology:cs_axiom_status(logos_created_not_coeternal, holdable).
narrative_ontology:cs_axiom_grounding('19ea1e50-96a8-43e0-8896-a581ca3a107c', logos_created_not_coeternal, deontological).
narrative_ontology:cs_axiom('19ea1e50-96a8-43e0-8896-a581ca3a107c', foundational, logos_subordinate_to_father).
narrative_ontology:cs_axiom_status(logos_subordinate_to_father, holdable).
narrative_ontology:cs_axiom_grounding('19ea1e50-96a8-43e0-8896-a581ca3a107c', logos_subordinate_to_father, deontological).
narrative_ontology:cs_reference_frame('19ea1e50-96a8-43e0-8896-a581ca3a107c', subordinationist_christology).
narrative_ontology:cs_drift_state('19ea1e50-96a8-43e0-8896-a581ca3a107c', post_nicene_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('19ea1e50-96a8-43e0-8896-a581ca3a107c', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_theologians).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, unitarian_groups).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, anti_nicene_protestant_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, sacramental_theology).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, trinitarian_orthodoxy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, laity_in_subordinationist_groups).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, laity_in_high_church).
narrative_ontology:constraint_vindicates(john_1_1_logos__subordinationist, radical_monotheism).
narrative_ontology:constraint_vindicates(john_1_1_logos__subordinationist, christ_as_supreme_creature).
narrative_ontology:constraint_vindicates(john_1_1_logos__subordinationist, father_alone_unbegotten).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Catholic, Orthodox, and high Anglican/Lutheran traditions whose sacramental authority, liturgical practice, and ecclesial identity rest on the full divinity of the Logos. The subordinationist reading undermines the metaphysical ground of the Eucharist, the Trinity, and the Church's teaching office. Exit means abandoning centuries of doctrinal development, liturgical formation, and institutional continuity.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_traditions, payer,
    institutional, generational, constrained, global).

% Historical Arians, modern unitarian theologians, Jehovah's Witnesses, and some radical Reformation traditions who advance the reading that the Logos is the first and highest creation. They gain doctrinal coherence for radical monotheism, avoid the perceived logical contradictions of homoousios, and claim biblical fidelity. Their exit options include forming distinct communities or influencing existing ones.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_theologians, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, subordinationist_theologians, beneficiary).

% Socinian, Unitarian Universalist, Christadelphian, and similar communities that receive the subordinationist reading as their doctrinal foundation. They benefit from a Christology that preserves strict monotheism and rational coherence. Exit is relatively open — they can join trinitarian bodies or secularize.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, unitarian_groups, beneficiary,
    organized, biographical, mobile, global).

% Certain Anabaptist, Restorationist, and non-creedal Protestant streams that reject the Nicene formulation as extra-biblical philosophical imposition. They gain freedom from creedal subscription and a 'biblical' Christology. Exit is constrained by community identity and anti-creedal commitments.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, anti_nicene_protestant_traditions, beneficiary,
    moderate, biographical, constrained, regional).

% Ordinary believers in Catholic, Orthodox, and high Protestant churches whose spiritual formation, sacramental life, and salvation theology depend on the full divinity of Christ. They bear the cost of doctrinal confusion, liturgical destabilization, and potential loss of sacramental assurance. Exit means leaving their spiritual home and community.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, laity_in_high_church, payer,
    organized, biographical, constrained, global).

% Believers in unitarian, JW, Christadelphian, and similar communities who receive a coherent, non-paradoxical Christology that makes rational sense of Scripture. They benefit from doctrinal clarity and avoidance of 'mystery' language. Exit is socially costly but theologically open.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, laity_in_subordinationist_groups, beneficiary,
    moderate, biographical, mobile, global).

% Academic biblical scholars, patristic historians, and theologians who study the Johannine Logos, the Arian controversy, and the development of Nicene orthodoxy as historical phenomena. They neither collect nor pay; they analyze the constraint's operation, its textual basis, and its historical enforcement.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, historical_critical_scholars, observer,
    analytical, civilizational, analytical, universal).

% The earliest Jewish-Christian communities (Ebionites, Nazarenes) who may have held adoptionist or agent-Christologies but were marginalized by both subordinationist and orthodox trajectories. They would object to both the Nicene formulation and the Arian systematization, but their voice is historically silenced.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, early_jewish_christian_voices, excluded,
    powerless, civilizational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a Christological framework that preserves radical monotheism (the Father alone is unbegotten, uncreated, absolutely one) while granting the Logos/Christ supreme honor as the Father's first and highest creation, the agent of creation and revelation, worthy of veneration but not the latria due to God alone.
% TRANSFER_FUNCTION: Moves doctrinal authority, sacramental exclusivity, and the metaphysical ground of worship from high-church traditions (whose authority rests on the full divinity of the Logos) to subordinationist readings that claim biblical fidelity and rational coherence. The transfer is legitimacy and interpretive control over John 1:1 and the christological trajectory it anchors.
% ABSENT_VOICES: Early Jewish-Christian communities (Ebionites, Nazarenes) with adoptionist/agent-Christologies marginalized by both trajectories; Gnostic readers who read the Logos as an emanation or aeon; Islamic theological tradition (which affirms Jesus as Word/Spirit from God but not divine) — none were present at Nicaea or in the Arian councils as recognized parties.
% DISAPPEARANCE_RATIONALE: If the subordinationist reading vanished overnight, the Arian/unitarian/JW doctrinal edifice would collapse; their communities would either adopt Nicene orthodoxy, secularize, or fragment. Conversely, if Nicene orthodoxy vanished, high-church sacramental theology, Trinitarian liturgy, and the magisterial teaching office would lose their metaphysical foundation. The world rearranges either way — the constraint is structural to both ecclesial worlds.
% FOUNDING_PROBLEM: How to articulate the significance of Jesus Christ as the Logos of John 1:1 — the agent of creation, the revealer of God, the one through whom all things were made — without compromising the absolute unity and unoriginated simplicity of the one God (the Father). The problem is monotheism under pressure from the biblical witness to Christ's unique status.
% FOUNDING_PROBLEM_CORROBORATION: Patristic scholars (e.g., Rowan Williams, Khaled Anatolios, Lewis Ayres) attest that the Arian controversy was genuinely about how to preserve monotheism while honoring the biblical Logos — not merely power politics. The subordinationist reading's own proponents (historical Arians, modern unitarians) attest the problem remains live. High-church theologians attest the problem was solved at Nicaea and the subordinationist answer is a false solution. No neutral arbiter exists; the corroboration is split along the very fault line the constraint creates.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.58) reflects the real cost to high-church traditions: loss of sacramental metaphysics, Trinitarian liturgy, and teaching authority. Suppression (0.62) reflects historical enforcement: imperial anathemas, conciliar definitions, creedal subscription, exclusion from communion. Theater ratio (0.28) is low because the theological dispute is genuine, not performative — both sides believe the gospel is at stake. Accessibility collapse (0.55) is moderate: alternative readings exist but are structurally marginalized by the dominance of Nicene categories. Resistance (0.75) is high: the Arian controversy convulsed the empire for decades; modern unitarians still contest the Nicene consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the high-church payer seat, the constraint is a snare: a heretical reading enforced by imperial power that extracts the Church's sacramental life. From the subordinationist agenda-setter seat, it is a rope: a genuine coordination solving the monotheism-Christology tension with minimal coercion (historically, Arians were often the persecuted party). From the analytical observer seat, it is a tangled rope: both coordination and extraction are structurally real. The engine captures this divergence; the claimed_type states the author's structural judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   High-church traditions are payers (d near 1.0): their entire sacramental-ecclesial edifice is extracted from. Subordinationist theologians and groups are beneficiaries/agenda-setters (d near 0.0): they gain doctrinal coherence and biblical warrant. Laity in high churches are payers with constrained exit (d ~0.8). Laity in subordinationist groups are beneficiaries with mobile exit (d ~0.2). Historical-critical scholars are analytical observers (d=0.5). Early Jewish-Christian voices are excluded and trapped (d=1.0 but no structural voice). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The subordinationist reading was founded to solve the monotheism problem. That problem is contested: high-church traditions say Nicaea solved it; subordinationists say Nicaea imported Greek metaphysics and betrayed biblical monotheism. The mandate has not atrophied — the reading still coordinates communities (JWs, unitarians, biblical unitarians) and still extracts from high-church authority. Mandatrophy is unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the subordinationist reading a genuine theological option generated by the biblical text itself, or a constructed heresy imposed on the text by philosophical presuppositions (Greek subordinationism, modern rationalism)?',
    'Historical-philological analysis of pre-Nicene Logos-Christology (Philo, Justin, Origen) to determine whether ''generated'' or ''created'' language for the Logos was native to the tradition or a later imposition; comparative study of how each reading handles the full Johannine corpus.',
    'If the reading is textually generated, its extraction from high-church traditions is the cost of a legitimate interpretive option; if constructed, the extraction is ideological imposition. Affects whether the tangled_rope classification reflects genuine coordination or cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the subordinationist reading emerges from the text or is imposed on it.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Does the subordinationist constraint extract from high-church traditions as a structural necessity (the logic of the reading requires their marginalization) or as a historical contingency (political enforcement at Nicaea and after)?',
    'Counterfactual historical analysis: in contexts where subordinationism held imperial favor (e.g., under Constantius II, Valens), did it suppress Nicene orthodoxy with the same structural logic? Comparative study of modern contexts where both readings coexist without state enforcement.',
    'If extraction is structurally necessary, the tangled_rope classification is stable; if contingent, the coordination function may be separable from the extraction, suggesting a rope core with historical snare overlay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether the asymmetric extraction is intrinsic to the reading or historically contingent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of subordinationist communities (historically: exile, anathema, book-burning; modern: social marginalization, doctrinal exclusion) primarily structural (institutional power) or internalized (theological conviction that the reading is damnable heresy)?',
    'Post-establishment trajectory analysis: in jurisdictions where subordinationist groups gained legal recognition (post-1689 England, post-1791 USA), did suppression persist via internalized theological conviction? Survey data on whether unitarian/JW members experience suppression as external barrier or internal certainty.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after formal barriers fall. Affects χ computation for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for subordinationist communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 300, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john1_logos_subord_tr_t300, john_1_1_logos__subordinationist, theater_ratio, 300, 0.15).
narrative_ontology:measurement(john1_logos_subord_tr_t325, john_1_1_logos__subordinationist, theater_ratio, 325, 0.22).
narrative_ontology:measurement(john1_logos_subord_tr_t381, john_1_1_logos__subordinationist, theater_ratio, 381, 0.35).
narrative_ontology:measurement(john1_logos_subord_tr_t500, john_1_1_logos__subordinationist, theater_ratio, 500, 0.3).
narrative_ontology:measurement(john1_logos_subord_tr_t1000, john_1_1_logos__subordinationist, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(john1_logos_subord_tr_t1500, john_1_1_logos__subordinationist, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(john1_logos_subord_tr_t1800, john_1_1_logos__subordinationist, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(john1_logos_subord_tr_t2025, john_1_1_logos__subordinationist, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(john1_logos_subord_be_t300, john_1_1_logos__subordinationist, base_extractiveness, 300, 0.35).
narrative_ontology:measurement(john1_logos_subord_be_t325, john_1_1_logos__subordinationist, base_extractiveness, 325, 0.48).
narrative_ontology:measurement(john1_logos_subord_be_t381, john_1_1_logos__subordinationist, base_extractiveness, 381, 0.62).
narrative_ontology:measurement(john1_logos_subord_be_t500, john_1_1_logos__subordinationist, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(john1_logos_subord_be_t1000, john_1_1_logos__subordinationist, base_extractiveness, 1000, 0.42).
narrative_ontology:measurement(john1_logos_subord_be_t1500, john_1_1_logos__subordinationist, base_extractiveness, 1500, 0.38).
narrative_ontology:measurement(john1_logos_subord_be_t1800, john_1_1_logos__subordinationist, base_extractiveness, 1800, 0.52).
narrative_ontology:measurement(john1_logos_subord_be_t2025, john_1_1_logos__subordinationist, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(john1_logos_subord_su_t300, john_1_1_logos__subordinationist, suppression_requirement, 300, 0.25).
narrative_ontology:measurement(john1_logos_subord_su_t325, john_1_1_logos__subordinationist, suppression_requirement, 325, 0.55).
narrative_ontology:measurement(john1_logos_subord_su_t381, john_1_1_logos__subordinationist, suppression_requirement, 381, 0.78).
narrative_ontology:measurement(john1_logos_subord_su_t500, john_1_1_logos__subordinationist, suppression_requirement, 500, 0.7).
narrative_ontology:measurement(john1_logos_subord_su_t1000, john_1_1_logos__subordinationist, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement(john1_logos_subord_su_t1500, john_1_1_logos__subordinationist, suppression_requirement, 1500, 0.45).
narrative_ontology:measurement(john1_logos_subord_su_t1800, john_1_1_logos__subordinationist, suppression_requirement, 1800, 0.5).
narrative_ontology:measurement(john1_logos_subord_su_t2025, john_1_1_logos__subordinationist, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:boltzmann_floor_override(john_1_1_logos__subordinationist, 0.08).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, nicene_creed_authority).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, chalcedonian_definition).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, trinitarian_worship_practice).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, eucharistic_real_presence).

% DUAL FORMULATION NOTE:
% This constraint is one member of the john_1_1_logos constraint family. The orthodox_christological reading (epsilon ~0.15, claimed mountain) and non_incarnational_monotheist reading (epsilon ~0.35, claimed rope) decompose the single natural-language label 'John 1:1 interpretation' into three structurally distinct constraints with different ε, different beneficiary/victim structures, and different classifications. They are linked via affects_constraints because each reading's legitimacy conditions affect the others' operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(john_1_1_logos__subordinationist, institutional, 0.85).
constraint_indexing:directionality_override(john_1_1_logos__subordinationist, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
