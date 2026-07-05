% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Reading of John 1:1 Logos (Arian/Semi-Arian Christology)
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   This story instantiates the subordinationist reading of the John 1:1
 *   Logos kernel: the Word is the first and greatest created (or
 *   eternally-generated-but-lesser) divine agent, not co-eternal or
 *   consubstantial with the Father. Historically this reading anchors Arian
 *   and semi-Arian Christologies, and its modern descendants (certain
 *   Unitarian and Restorationist traditions) continue to hold it as doctrinal
 *   identity. The reading is contested most sharply against the
 *   orthodox_christological reading, which holds the Logos ontologically
 *   identical with the second Trinitarian person, and less sharply against
 *   the non_incarnational_monotheist reading, which denies the Logos is a
 *   distinct hypostasis at all. Extraction here is read structurally:
 *   institutions and movements built around this Christology draw legitimacy,
 *   communal identity, and (historically) imperial patronage from maintaining
 *   the reading against conciliar condemnation, while lay believers absorbed
 *   the material cost of the controversy (exile, anathema, forced
 *   reconciliation) largely without agency in the dispute.
 *
 * KEY AGENTS:
 *   - subordinationist_clergy_hierarchies: agenda_setter/beneficiary — administers doctrine, gains legitimacy from the reading
 *   - trinitarian_high_church_authorities: primary payer — sacramental and magisterial authority erodes if the reading spreads
 *   - lay_believers_across_condemned_councils: powerless payer — bore the historical cost of doctrinal conflict
 *   - historical_critical_scholars: analytical observer — treats the grammatical and historical dispute as data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.58).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.71).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Reading of John 1:1 Logos (Arian/Semi-Arian Christology)").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, 'a30748b7-5203-4eb9-8d65-7b3ee62e2e31').
narrative_ontology:cs_kernel_codification('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', fixed_text).
narrative_ontology:cs_authority_grounding('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', practice).
narrative_ontology:cs_interpretation_layer_present('a30748b7-5203-4eb9-8d65-7b3ee62e2e31').
narrative_ontology:cs_reading_relation('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', john_1_1_logos__non_incarnational_monotheist, coexists_with).
narrative_ontology:cs_axiom('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', foundational, logos_is_created_or_derivatively_divine).
narrative_ontology:cs_axiom_status(logos_is_created_or_derivatively_divine, holdable).
narrative_ontology:cs_axiom_grounding('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', logos_is_created_or_derivatively_divine, conventional).
narrative_ontology:cs_axiom('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', foundational, strict_numerical_monotheism_excludes_consubstantiality).
narrative_ontology:cs_axiom_status(strict_numerical_monotheism_excludes_consubstantiality, holdable).
narrative_ontology:cs_axiom_grounding('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', strict_numerical_monotheism_excludes_consubstantiality, deontological).
narrative_ontology:cs_reference_frame('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', pre_nicene_subordinationist_consensus).
narrative_ontology:cs_drift_state('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', post_nicene_conciliar_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a30748b7-5203-4eb9-8d65-7b3ee62e2e31', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_clergy_hierarchies).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, unitarian_and_arian_aligned_movements).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, strict_monotheist_apologetics_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, trinitarian_high_church_authorities).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, sacramental_traditions_dependent_on_full_divinity).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, lay_believers_across_condemned_councils).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer congregations and doctrinal formation on the premise that the Logos is the first and greatest created being, subordinate to the Father in nature as well as function. This reading grounds their authority to interpret scripture in a way that pointedly excludes creedal formulas (homoousios) that would otherwise subordinate their office to councils they were not party to. They benefit from the interpretive space this reading opens: a hierarchy of being that mirrors and legitimates a hierarchy of ecclesial office.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_clergy_hierarchies, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, subordinationist_clergy_hierarchies, beneficiary).

% Draw doctrinal legitimacy and communal identity from the subordinationist reading, using it to distinguish themselves from Trinitarian bodies and to ground a strict monotheism they consider more textually and philosophically defensible. Their institutional survival across centuries of condemnation has depended on maintaining and transmitting this reading intact.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, unitarian_and_arian_aligned_movements, beneficiary,
    organized, generational, constrained, national).

% Use the subordinationist reading as an apologetic bridge in interfaith and philosophical argument, arguing it resolves the tension between strict divine unity and the Logos's preexistence more cleanly than either fully divine or purely functional readings. They are not bound to any single institution and can adopt or drop the reading as argumentative context requires.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, strict_monotheist_apologetics_traditions, beneficiary,
    moderate, biographical, mobile, global).

% Their entire sacramental and magisterial authority structure — apostolic succession, the validity of the Eucharist as encounter with a fully divine Christ, conciliar authority itself (Nicaea, Constantinople) — depends on the Logos being consubstantial with the Father. The subordinationist reading, wherever it gains ground, directly erodes the theological premise their institutional authority is built on; they cannot simply exit the dispute because the dispute is over the ground they stand on.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, trinitarian_high_church_authorities, payer,
    institutional, civilizational, trapped, global).

% Liturgical and sacramental practices (Eucharistic real presence, theotokos veneration, divine liturgy addressed to Christ as God) lose their theological warrant if the Logos is a created being. Practitioners inherit these liturgies without having chosen the underlying Christology and cannot renegotiate their sacramental life without a wholesale doctrinal rupture.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, sacramental_traditions_dependent_on_full_divinity, payer,
    organized, generational, trapped, regional).

% Ordinary congregants who, across the fourth-century controversies and subsequent Arian-adjacent movements, found themselves anathematized, exiled, or forcibly reconciled depending on which reading their bishop held and which emperor backed which council. They bore the practical cost — excommunication, loss of clerical office, forced re-baptism, communal schism — of a dispute conducted mostly above their heads by hierarchs and councils.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, lay_believers_across_condemned_councils, payer,
    powerless, biographical, trapped, regional).

% The Nicene and post-Nicene councils that formally condemned subordinationist Christology are structurally excluded from the subordinationist reading's own self-justification — their conciliar authority is precisely what this reading must relativize or reject to remain coherent. They would object that the reading was already adjudicated and rejected by ecumenical process, but within the subordinationist framework their verdict carries no binding weight.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, council_authorities_nicaea_constantinople, excluded,
    institutional, civilizational, analytical, global).

% Study the Greek text, the Hellenistic Jewish logos tradition (Philo), and the fourth-century controversy as historical and philological data rather than as a live theological commitment. They can trace how grammatical ambiguity in the prologue (the anarthrous theos in 1:1c) became load-bearing for competing institutional claims, without needing to adjudicate which reading is doctrinally correct.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, historical_critical_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent Christology that preserves strict monotheism (only the Father is unoriginate God) while still accounting for the Logos's preexistence and role in creation and revelation — coordinating belief and worship practice around a graded hierarchy of divine being that many adherents find more logically parsimonious than Trinitarian formulations.
% TRANSFER_FUNCTION: Moves doctrinal authority and sacramental legitimacy away from institutions whose office and liturgy depend on full consubstantiality, and toward hierarchies and movements whose leadership and identity are constituted by rejecting that premise; historically also moved political and imperial patronage between rival episcopates depending on which court favored which Christology.
% ABSENT_VOICES: The condemning councils (Nicaea 325, Constantinople 381) are treated within this reading as errant or politically compromised bodies rather than binding authorities, so their voice is present only as a rejected precedent, not as a live interlocutor. Ordinary lay believers whose sacramental lives were upended by the controversy left little independent textual record of their own view of the dispute.
% DISAPPEARANCE_RATIONALE: Trinitarian authorities would say the world barely changes — the subordinationist reading is already a minority position without institutional dominance in most of global Christianity, so its disappearance mainly removes a persistent apologetic irritant. Subordinationist-aligned movements would say their entire communal and doctrinal identity rearranges or dissolves, since the reading is constitutive of their self-understanding rather than incidental to it. The verdict is genuinely disputed between the parties rather than resolvable from outside.
% FOUNDING_PROBLEM: How to reconcile John's assertion that 'the Word was with God, and the Word was God' (1:1) with strict monotheism and the grammatical/philosophical worry that calling a second entity 'God' without qualification risks ditheism — the subordinationist solution reads the anarthrous predicate theos as denoting a lesser, derivative divinity rather than full ontological identity with ho theos.
% FOUNDING_PROBLEM_CORROBORATION: Subordinationist-aligned theologians and historians of early Arianism attest the grammatical problem (definite article usage in 1:1c) remains a live exegetical question independent of conciliar outcome. Trinitarian patristic scholars and the councils themselves attest the problem was formally and permanently resolved at Nicaea and Constantinople via the homoousios formula, and that its persistence in subordinationist communities is theological inheritance, not unresolved textual difficulty. Independent historical-critical scholars (outside both benefiting camps) corroborate that the grammatical ambiguity is real and was contested in good faith in the fourth century, but do not corroborate that it remains textually undecided today — most regard the conciliar resolution as theologically, not linguistically, settled.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, contested).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate-high (0.58 at the interval's modern end) because the reading's persistence in specific communities is bound up with communal identity maintenance and boundary-drawing against Trinitarian bodies, not pure disinterested exegesis. Suppression is high historically (0.85 at Nicene founding, reflecting imperial and conciliar coercion against subordinationist clergy and laity) and drops through the medieval period as active imperial enforcement receded, then rises again modestly in the modern period (0.71) as institutional Trinitarian bodies engage apologetics and countercult literature against Arian-descended movements — a softer, discursive suppression rather than coercive. Theater ratio rises slowly (0.20 to 0.32) as doctrinal performance (creedal recitation, catechesis reinforcing the distinct reading) becomes more routinized relative to live exegetical dispute.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (subordinationist clergy), the reading is coordination: a coherent, textually defensible Christology solving the ditheism worry cleanly. From the payer seat (Trinitarian authorities and their dependent sacramental traditions), the same reading operates as an ongoing extractive threat to conciliar and sacramental legitimacy that must be actively resisted through apologetics, catechesis, and historically through coercive conciliar enforcement. The engine should compute divergent seat-level types from this structural asymmetry rather than from any story-level adjudication of which reading is textually correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist clergy hierarchies and aligned movements sit near the beneficiary end: their institutional identity, office, and doctrinal distinctiveness are constituted by this reading, and it costs them little to maintain relative to what it secures. Trinitarian high-church authorities and their dependent sacramental traditions sit near the target end: their authority structure is directly eroded wherever this reading gains adherents, and their exit option is effectively trapped — the dispute is over the ground their office stands on, not a negotiable external cost. Lay believers across history sit furthest toward the target end with the least agency: powerless, trapped exit, bearing the concrete costs (exile, anathema, forced rebaptism) of a doctrinal contest run by hierarchs above them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling monotheism with the Logos's apparent divinity in 1:1c) is contested rather than dead: subordinationist communities hold it structurally unresolved and their own reading as the live solution, while Trinitarian bodies hold it conciliarly resolved and treat subordinationist persistence as inherited identity rather than unresolved textual difficulty. This divergence is exactly what the founding_problem_status='contested' plus disappearance_verdict='contested' pairing is meant to flag — neither side's self-report should be taken as adjudicating the mismatch; the corroboration field notes that outside historical-critical scholarship regards the grammar as historically contested but not textually undecided today, which cuts somewhat against the subordinationist self-report without fully vindicating the Trinitarian one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anarthrous_theos_grammatical_indeterminacy,
    'Does the absence of the definite article before ''theos'' in John 1:1c (kai theos en ho logos) grammatically support a qualitative/lesser reading (''the Word was divine/a god'') as opposed to the full ontological identity reading (''the Word was God'')?',
    'Comparative analysis of anarthrous predicate nominative constructions elsewhere in Johannine and broader Koine Greek corpus (Colwell''s Rule and its critiques); consensus among historical-critical Greek grammarians independent of doctrinal commitment.',
    'If the grammar genuinely underdetermines between qualitative and full-identity readings, the subordinationist reading retains textual legitimacy independent of conciliar outcome. If grammarians converge that the construction more naturally supports full ontological predication in context, the subordinationist reading''s textual warrant weakens relative to its doctrinal-inheritance warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anarthrous_theos_grammatical_indeterminacy, empirical, 'Whether the Greek grammar of John 1:1c genuinely supports the subordinationist reading or whether the reading survives mainly as inherited doctrinal identity.').

omega_variable(
    constructed_vs_inherited_doctrinal_boundary,
    'Is the subordinationist Christology a live, independently-motivated theological position, or is its persistence in contemporary communities primarily an inherited institutional identity marker maintained for communal boundary-drawing rather than active exegetical conviction?',
    'Ethnographic and doctrinal-history study of contemporary subordinationist-aligned communities: do adherents arrive at the position through independent scriptural engagement, or is it transmitted primarily as catechetical inheritance tied to communal membership?',
    'If primarily inherited-identity, the extraction is closer to identity-coordination (group-boundary maintenance) than to genuine ongoing theological extraction; if independently and repeatedly re-derived, the reading has stronger claim to being live coordination around a real textual problem rather than a constructed boundary marker.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constructed_vs_inherited_doctrinal_boundary, conceptual, 'Whether the reading''s persistence is live theological conviction or inherited communal-boundary maintenance.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (subordinationist, orthodox_christological, non_incarnational_monotheist) locate their disagreement — is it primarily grammatical (how to render 1:1c), primarily philosophical (what monotheism logically permits), or primarily authority-based (whether conciliar pronouncement settles the exegetical question)?',
    'Systematic comparison of the three readings'' own stated grounds for their position, cross-checked against whether each reading changes if the grammatical question were independently resolved.',
    'If the disagreement is primarily authority-based rather than grammatical, the subordinationist reading''s persistence is less about textual conviction and more about rejecting conciliar authority as such — which would reclassify part of the constraint''s function from exegetical coordination toward authority contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating whether the kernel''s sibling readings disagree on grammar, philosophy, or authority — this is the committer structure this story routes here rather than resolving internally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t325, john_1_1_logos__subordinationist, theater_ratio, 325, 0.2).
narrative_ontology:measurement(john_tr_t500, john_1_1_logos__subordinationist, theater_ratio, 500, 0.25).
narrative_ontology:measurement(john_tr_t800, john_1_1_logos__subordinationist, theater_ratio, 800, 0.28).
narrative_ontology:measurement(john_tr_t1500, john_1_1_logos__subordinationist, theater_ratio, 1500, 0.3).
narrative_ontology:measurement(john_tr_t1900, john_1_1_logos__subordinationist, theater_ratio, 1900, 0.31).
narrative_ontology:measurement(john_tr_t2025, john_1_1_logos__subordinationist, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(john_be_t325, john_1_1_logos__subordinationist, base_extractiveness, 325, 0.42).
narrative_ontology:measurement(john_be_t500, john_1_1_logos__subordinationist, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(john_be_t800, john_1_1_logos__subordinationist, base_extractiveness, 800, 0.48).
narrative_ontology:measurement(john_be_t1500, john_1_1_logos__subordinationist, base_extractiveness, 1500, 0.4).
narrative_ontology:measurement(john_be_t1900, john_1_1_logos__subordinationist, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(john_be_t2025, john_1_1_logos__subordinationist, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t325, john_1_1_logos__subordinationist, suppression_requirement, 325, 0.85).
narrative_ontology:measurement(john_su_t500, john_1_1_logos__subordinationist, suppression_requirement, 500, 0.8).
narrative_ontology:measurement(john_su_t800, john_1_1_logos__subordinationist, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(john_su_t1500, john_1_1_logos__subordinationist, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement(john_su_t1900, john_1_1_logos__subordinationist, suppression_requirement, 1900, 0.55).
narrative_ontology:measurement(john_su_t2025, john_1_1_logos__subordinationist, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:boltzmann_floor_override(john_1_1_logos__subordinationist, 0.1).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the meaning of John 1:1's Logos' into structurally distinct constraints per the kernel-reading protocol: subordinationist (this file), orthodox_christological, and non_incarnational_monotheist. Each carries its own ε, beneficiary/victim structure, and classification. The subordinationist reading's ε (0.58) sits between the other two on the extraction spectrum: it declares stronger identifiable beneficiaries and victims than the orthodox reading (which claims near-universal institutional dominance and thus more diffuse extraction) and than the non-incarnational reading (which, by denying the Logos is a distinct hypostasis at all, has minimal institutional stake in either direction). The three do not average into one ε; ε-invariance requires this file to stand alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
