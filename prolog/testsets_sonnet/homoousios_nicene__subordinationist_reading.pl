% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Homoousios as Compatible with Subordination (Son Derives Being from Father)
 *   domain: historical theology / ecclesiastical history / philosophy of religion
 *
 * SUMMARY:
 *   In the decades following Nicaea (325), the council's chosen term
 *   homoousios ('same substance') did not immediately settle the
 *   Christological question it was meant to close. A durable interpretive
 *   current — running through Homoian and Semi-Arian bishops, sympathetic
 *   emperors (especially Constantius II), and later Germanic Christian
 *   kingdoms — held that homoousios could be subscribed to in good faith
 *   while retaining a hierarchical Christology: the Son genuinely shares
 *   divine substance but derives that being from the Father and remains
 *   functionally or ontologically subordinate. This reading persisted
 *   institutionally for over half a century, contested by imperial councils,
 *   exile of bishops, and shifting court favor, before being substantially
 *   foreclosed (though never fully erased from regional practice) by
 *   Constantinople 381 and subsequent imperial enforcement under Theodosius.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.58).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.71).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Homoousios as Compatible with Subordination (Son Derives Being from Father)").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "historical theology / ecclesiastical history / philosophy of religion").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, '4c986b03-362f-4451-ae31-9b182d479010').
narrative_ontology:cs_kernel_codification('4c986b03-362f-4451-ae31-9b182d479010', fixed_text).
narrative_ontology:cs_authority_grounding('4c986b03-362f-4451-ae31-9b182d479010', lineage).
narrative_ontology:cs_interpretation_layer_present('4c986b03-362f-4451-ae31-9b182d479010').
narrative_ontology:cs_reading_relation('4c986b03-362f-4451-ae31-9b182d479010', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('4c986b03-362f-4451-ae31-9b182d479010', homoousios_nicene__honorific_similarity_reading, coexists_with).
narrative_ontology:cs_axiom('4c986b03-362f-4451-ae31-9b182d479010', foundational, son_derives_being_from_father).
narrative_ontology:cs_axiom_status(son_derives_being_from_father, holdable).
narrative_ontology:cs_axiom_grounding('4c986b03-362f-4451-ae31-9b182d479010', son_derives_being_from_father, deontological).
narrative_ontology:cs_axiom('4c986b03-362f-4451-ae31-9b182d479010', secondary, scriptural_subordination_texts_govern_creedal_interpretation).
narrative_ontology:cs_axiom_status(scriptural_subordination_texts_govern_creedal_interpretation, overridden).
narrative_ontology:cs_axiom_grounding('4c986b03-362f-4451-ae31-9b182d479010', scriptural_subordination_texts_govern_creedal_interpretation, conventional).
narrative_ontology:cs_reference_frame('4c986b03-362f-4451-ae31-9b182d479010', pre_nicene_father_monarchy_tradition).
narrative_ontology:cs_drift_state('4c986b03-362f-4451-ae31-9b182d479010', post_constantinople_381, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('4c986b03-362f-4451-ae31-9b182d479010', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_clergy_networks).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, homoian_and_semi_arian_remnant_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, regional_churches_resisting_conciliar_centralization).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, nicene_pro_homoousian_bishops).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, trinitarian_doctrinal_uniformity).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, laity_under_creedal_discipline_in_contested_sees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, regional_churches_resisting_conciliar_centralization).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, monarchia_of_the_father_doctrine).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, scriptural_subordination_texts_as_authoritative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and presbyters (Homoian, Semi-Arian, and related factions) who read homoousios as leaving room for the Son deriving being and rank from the Father. They retain sees, imperial patronage under sympathetic emperors (notably under Constantius II and in various Gothic and Vandal kingdoms), and liturgical authority in their regions by holding this reading. They administer synods that ratify subordinationist creeds and press for reinterpretation of Nicaea's language.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_clergy_networks, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, subordinationist_clergy_networks, agenda_setter).

% Congregations and clergy, particularly in the Germanic successor kingdoms and parts of the East, who continue subordinationist Christology after Constantinople 381 nominally settles the question. They persist by geographic distance from conciliar enforcement centers and by royal sponsorship (e.g., Gothic Arianism), but face escalating exclusion from communion and property as Nicene enforcement consolidates.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, homoian_and_semi_arian_remnant_communities, beneficiary,
    moderate, generational, constrained, regional).

% Provincial churches that favor a subordinationist or looser reading partly as resistance to the growing doctrinal and political centralization exercised through Alexandria, Rome, and the imperial court. They benefit from theological latitude but pay in later loss of standing, exile of sympathetic bishops, and forced subscription to Nicene formulas once imperial policy shifts under Theodosius.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, regional_churches_resisting_conciliar_centralization, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, regional_churches_resisting_conciliar_centralization, payer).

% Bishops (Athanasius, the Cappadocians, and their allies) who hold homoousios to mean full ontological equality and experience the subordinationist reading's persistence as a direct assault on the settlement they fought exile and deposition to secure. Each subordinationist synod or imperial edict tolerating the rival reading forces renewed campaigns, further exiles, and doctrinal litigation they did not choose to reopen.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, nicene_pro_homoousian_bishops, payer,
    organized, generational, constrained, continental).

% Named for completeness as the doctrinal good the equality reading's proponents claim to be protecting; it is not an actor but is treated by Nicene partisans as the casualty of any accommodation to subordinationist compatibility, since a homoousios that tolerates subordination cannot also ground a settled uniform confession.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, trinitarian_doctrinal_uniformity, payer,
    institutional, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(homoousios_nicene__subordinationist_reading, trinitarian_doctrinal_uniformity).

% Ordinary believers in contested dioceses whose sacraments, clergy, and communion validity depend on which reading their local bishop or emperor currently enforces. They have no forum to adjudicate the technical Greek terms at stake and simply live with excommunication, exile of pastors, or forced rebaptism as the imperial and conciliar tide shifts between readings.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, laity_under_creedal_discipline_in_contested_sees, payer,
    powerless, biographical, trapped, local).

% Roman emperors from Constantine through Theodosius who alternately favor subordinationist and equality readings depending on political calculation, convening and dissolving councils, exiling and recalling bishops, and using the choice of reading to manage factional balance within the episcopate and among Germanic allies. Their exit is effectively arbitrage: they can switch doctrinal favor at will without bearing the theological cost either faction bears.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, continental).

% Scholars examining the fourth-century controversies retrospectively, tracing how the term homoousios itself remained stable in the text of Nicaea while its interpretive load shifted enormously depending on which faction held imperial and conciliar power in a given decade.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__subordinationist_reading, subordinationist_clergy_networks).
narrative_ontology:fixing_cost_class(homoousios_nicene__subordinationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A subordinationist reading of homoousios lets Father-monarchy theology, common in earlier pre-Nicene tradition and in scriptural texts emphasizing the Son's derivation and obedience, remain compatible with formal subscription to the Nicene term — allowing diverse regional Christologies to coexist under one creedal vocabulary rather than forcing an immediate, empire-wide schism.
% TRANSFER_FUNCTION: Moves theological legitimacy and ecclesiastical authority away from the Athanasian/Cappadocian equality faction and toward subordinationist clergy and their patrons; moves doctrinal certainty and communion security away from ordinary believers in contested sees, who bear the instability of shifting official readings.
% ABSENT_VOICES: Ordinary laity in contested sees have no seat in any council; their views on the technical distinction between homoousios and homoiousios were never solicited, yet their sacramental status depended entirely on which faction's reading prevailed locally in a given year. Non-Chalcedonian and pre-Nicene subordinationist voices from the earliest tradition (Origen's hierarchical Logos theology) are also largely absent from the historical record's own self-presentation, having been retrospectively cast as proto-heresy once the equality reading consolidated.
% DISAPPEARANCE_RATIONALE: If subordinationist-compatible readings of homoousios vanished entirely from the fourth-century landscape, the metaphysical-equality faction would say the world simply arrived faster at settled truth; subordinationist sympathizers and modern scholars of contested orthodoxy would say entire regional churches, Germanic Christian communities, and theological options structurally dependent on retaining flexibility in the term would have been foreclosed decades earlier, reorganizing the entire trajectory of Trinitarian doctrine and imperial religious policy.
% FOUNDING_PROBLEM: Nicaea (325) needed a term that would exclude Arius's claim that the Son is a creature ex nihilo, but the council did not fully specify whether 'same substance' entailed strict numerical/ontological equality or merely shared but ranked divine being — leaving room for those sympathetic to the Father's monarchy and to subordinationist scriptural texts (John 14:28, 1 Cor 15:28) to subscribe to the word while retaining a hierarchical Christology.
% FOUNDING_PROBLEM_CORROBORATION: Modern historical-critical scholarship (R.P.C. Hanson, Lewis Ayres) outside any confessional stake in either faction's victory documents that the fourth-century terminological ambiguity was real and was progressively resolved not by the term's inherent clarity but by decades of imperial politics and conciliar attrition culminating in Constantinople 381; this corroboration comes from historians with no institutional interest in either the subordinationist or equality faction's vindication.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, contested).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises through the mid-fourth century (0.32 to 0.63) as the subordinationist reading moves from a live, contestable theological option to an entrenched factional position requiring imperial machinery — synods, exiles, forced subscriptions — to sustain against the consolidating equality faction. It settles rather than continuing to climb after 381 (0.58) because enforcement shifts decisively against subordinationism at that point; the reading persists in pockets (Gothic Arianism) but loses its capacity to extract further institutional concessions. Suppression tracks a similar but steeper curve (0.35 to 0.75) reflecting the escalating machinery — imperial edicts, conciliar anathemas, exile — needed on both sides to hold their reading against the other. Theater ratio is modest throughout (0.12 to 0.30): most conciliar activity in this period was substantively fought, not performative, though later subscription-under-duress by regional bishops (post-381) does carry a genuine performative component as compliance outpaces belief.
 *
 * PERSPECTIVAL GAP:
 *   From the subordinationist clergy's seat, the arrangement looks like a rope: a genuine, defensible theological position preserving continuity with earlier Father-monarchy tradition and scriptural subordination texts, held in good conscience under the same creedal vocabulary as their opponents. From the Nicene pro-equality bishops' seat, the identical arrangement is a tangled rope shading toward snare: a coordination fiction (shared subscription to 'homoousios') papering over an active extraction of doctrinal ground and ecclesiastical office that requires continuous imperial and conciliar enforcement to either sustain or dislodge. The engine computes both seats from the same structural data; the divergence itself is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist clergy networks and remnant communities are declared beneficiaries because the reading's viability directly preserves their ecclesiastical standing, patronage, and theological latitude — their directionality sits near the beneficiary end. Nicene pro-homoousian bishops are victims: every synod or imperial policy that tolerates subordinationist compatibility with homoousios directly costs them ground they had fought exile to win, pushing their directionality toward the target end despite their considerable organized power. Ordinary laity in contested sees are powerless and trapped, bearing costs (excommunication, pastoral instability) they had no say in creating regardless of which reading currently prevails locally — their directionality is pinned near full target by both powerlessness and lack of exit. The imperial court occupies a unique arbitrage position: emperors can switch favored readings for political reasons without bearing either faction's theological cost, which is why their exit_options is set to arbitrage rather than mobile or constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine ambiguity in what 'same substance' entailed relative to rank and origin at the moment of Nicaea's drafting — is genealogically real and independently corroborated by non-partisan historical scholarship (founding_problem_status: dead, corroboration from outside both factions). Treating the subordinationist reading as simply a persistent heresy (a snare with no coordination function) would mislabel a genuinely defensible fourth-century theological position as pure extraction; treating it as a pure rope ignores that its persistence past 381 required active, coercive machinery (imperial edicts, forced communion tests, exile) that materially harmed a class of victims (Nicene bishops, contested laity). Tangled Rope captures both: real coordination function (preserving continuity with Father-monarchy tradition and scriptural texts under a shared creedal term) combined with asymmetric extraction (subordinationist networks retaining sees and patronage at direct cost to the equality faction and to doctrinal certainty for ordinary believers), sustained only through active enforcement on both sides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy_325,
    'Did the bishops who subscribed to homoousios at Nicaea in 325 share a single determinate metaphysical commitment, or did the term''s adoption paper over a real plurality of Christological positions (subordinationist, equality, and honorific) from the outset?',
    'This is the committer-structure question the kernel itself poses: it cannot be resolved empirically by more textual evidence alone, since the participants may genuinely not have shared a single determinate reading in 325. Comparative analysis of contemporaneous letters, conciliar minutes (where they survive), and the documented range of positions among signatories is the best available evidence, but full resolution may be structurally unavailable.',
    'If the term was genuinely indeterminate at adoption, this reading (subordinationist_reading) and its siblings (metaphysical_equality_reading, honorific_similarity_reading) are equally legitimate retrospective disambiguations of an originally unsettled kernel, and none can claim exclusive fidelity to ''what Nicaea meant.'' If the term had a determinate original sense that was later contested rather than genuinely ambiguous, this reading is better characterized as a later reinterpretation rather than a co-original one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy_325, conceptual, 'Whether the homoousios kernel was ever singular, or was multiply-readable from the moment of its adoption.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does this reading''s eventual defeat at Constantinople 381 and under Theodosian enforcement constitute a logical foreclosure of the metaphysical_equality_reading''s core premise, or do both readings remain live positions coexisting in different theological traditions (e.g. surviving in some non-Chalcedonian and later heterodox communities) to this day?',
    'Track whether self-identified subordinationist theological communities persist as a coherent tradition after 431, distinct from mere historical curiosity — modern Unitarian and some Jehovah''s Witness theology explicitly revives subordinationist readings of scripture, suggesting the position was suppressed institutionally rather than logically foreclosed.',
    'If subordinationism persists as a genuinely coherent alternative theological tradition rather than being logically incoherent, the relationship to the metaphysical_equality_reading is properly coexists_with (declared in cs_structure) rather than forecloses — institutional defeat is not the same as logical elimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether institutional/conciliar defeat of subordinationism amounts to logical foreclosure of the rival reading or merely its political suppression.').

omega_variable(
    beneficiary_sincerity_ambiguity,
    'Were subordinationist clergy networks sincere theological actors preserving a defensible tradition, or were at least some using the reading instrumentally to retain imperial patronage and regional autonomy against centralizing conciliar authority?',
    'Case-by-case examination of individual bishops'' correspondence and behavior under changing imperial favor — bishops who shifted readings opportunistically as imperial patronage shifted (documented in several fourth-century cases) provide evidence of instrumental rather than sincere theological commitment; bishops who held the position consistently under both favorable and unfavorable imperial regimes provide evidence of sincerity.',
    'A higher proportion of instrumental actors would push the constraint''s classification further toward snare (extraction with theological cover); a higher proportion of sincere, consistent actors supports the tangled_rope classification (genuine coordination function coexisting with asymmetric extraction) as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_sincerity_ambiguity, empirical, 'Whether beneficiary sincerity or instrumental patronage-seeking better explains subordinationist persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 325, 431).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__subordinationist_reading, theater_ratio, 325, 0.12).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__subordinationist_reading, theater_ratio, 350, 0.18).
narrative_ontology:measurement(homo_tr_t360, homoousios_nicene__subordinationist_reading, theater_ratio, 360, 0.24).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__subordinationist_reading, theater_ratio, 381, 0.3).
narrative_ontology:measurement(homo_tr_t400, homoousios_nicene__subordinationist_reading, theater_ratio, 400, 0.28).
narrative_ontology:measurement(homo_tr_t431, homoousios_nicene__subordinationist_reading, theater_ratio, 431, 0.28).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__subordinationist_reading, base_extractiveness, 325, 0.32).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__subordinationist_reading, base_extractiveness, 350, 0.41).
narrative_ontology:measurement(homo_be_t360, homoousios_nicene__subordinationist_reading, base_extractiveness, 360, 0.55).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__subordinationist_reading, base_extractiveness, 381, 0.63).
narrative_ontology:measurement(homo_be_t400, homoousios_nicene__subordinationist_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement(homo_be_t431, homoousios_nicene__subordinationist_reading, base_extractiveness, 431, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__subordinationist_reading, suppression_requirement, 325, 0.35).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__subordinationist_reading, suppression_requirement, 350, 0.5).
narrative_ontology:measurement(homo_su_t360, homoousios_nicene__subordinationist_reading, suppression_requirement, 360, 0.68).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__subordinationist_reading, suppression_requirement, 381, 0.75).
narrative_ontology:measurement(homo_su_t400, homoousios_nicene__subordinationist_reading, suppression_requirement, 400, 0.71).
narrative_ontology:measurement(homo_su_t431, homoousios_nicene__subordinationist_reading, suppression_requirement, 431, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the homoousios controversy' per the ε-invariance principle: subordinationist_reading (this file), metaphysical_equality_reading, and honorific_similarity_reading. Each reading has a distinct beneficiary/victim structure and distinct ε — the term itself did not change, but which claim 'homoousios' is taken to make differs sharply across the three, producing structurally different constraints rather than one constraint viewed three ways. All three link to each other via affects_constraints; the metaphysical_equality_reading is the eventual conciliar victor (Constantinople 381) and should be read as exerting downstream pressure on this reading's viability after 381, reflected in this story's measurement series flattening rather than continuing to climb.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
