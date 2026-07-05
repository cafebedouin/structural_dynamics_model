% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__orthodox_christological
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__orthodox_christological, []).

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
 *   constraint_id: john_1_1_logos__orthodox_christological
 *   human_readable: Orthodox Christological Reading of the Johannine Logos (Nicene-Chalcedonian)
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Johannine Logos
 *   kernel: the orthodox/conciliar Christological reading, under which the
 *   Logos of John 1:1 is ontologically divine, preexistent, and identical
 *   with the second person of the Trinity, and the incarnation described in
 *   1:14 is God literally becoming flesh. This reading became institutionally
 *   codified through the fourth- and fifth-century councils (Nicaea 325,
 *   Constantinople 381, Chalcedon 451) and now grounds sacramental authority,
 *   ordination, and the boundary of who counts as 'Christian' in mainstream
 *   Trinitarian bodies. It is a distinct constraint from its siblings (the
 *   subordinationist reading and the non-incarnational/functional-monotheist
 *   reading), each of which has its own ε, its own beneficiary/victim
 *   structure, and its own classification — they are not alternative
 *   measurements of this same constraint but separate constraints linked
 *   through the shared kernel.
 *
 * KEY AGENTS:
 *   - trinitarian_clergy_hierarchy: agenda_setter (institutional/arbitrage) — administers and enforces creedal boundary
 *   - conciliar_creedal_institutions: beneficiary/agenda_setter (institutional/arbitrage) — legitimacy bound to reading's permanence
 *   - arian_and_subordinationist_communities: payer (moderate/constrained) — historically anathematized
 *   - unitarian_and_non_trinitarian_groups: payer (powerless/trapped) — excluded from recognition as Christian
 *   - jewish_and_muslim_theological_interlocutors_excluded_from_communion: excluded (moderate/constrained) — never seated in the adjudicating councils
 *   - historical_and_comparative_theologians: observer (analytical) — trace conciliar politics behind the settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__orthodox_christological, 0.62).
domain_priors:suppression_score(john_1_1_logos__orthodox_christological, 0.71).
domain_priors:theater_ratio(john_1_1_logos__orthodox_christological, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, extractiveness, 0.62).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__orthodox_christological, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__orthodox_christological, "Orthodox Christological Reading of the Johannine Logos (Nicene-Chalcedonian)").
narrative_ontology:topic_domain(john_1_1_logos__orthodox_christological, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__orthodox_christological).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__orthodox_christological, 'b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d').
narrative_ontology:cs_kernel_codification('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', fixed_text).
narrative_ontology:cs_authority_grounding('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', lineage).
narrative_ontology:cs_interpretation_layer_present('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d').
narrative_ontology:cs_reading_relation('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', john_1_1_logos__subordinationist, forecloses).
narrative_ontology:cs_reading_relation('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', john_1_1_logos__non_incarnational_monotheist, forecloses).
narrative_ontology:cs_axiom('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', foundational, logos_consubstantial_with_father).
narrative_ontology:cs_axiom_status(logos_consubstantial_with_father, holdable).
narrative_ontology:cs_axiom_grounding('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', logos_consubstantial_with_father, theological).
narrative_ontology:cs_axiom('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', foundational, incarnation_is_literal_ontological_embodiment).
narrative_ontology:cs_axiom_status(incarnation_is_literal_ontological_embodiment, holdable).
narrative_ontology:cs_axiom_grounding('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', incarnation_is_literal_ontological_embodiment, theological).
narrative_ontology:cs_reference_frame('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', nicene_chalcedonian_settlement).
narrative_ontology:cs_drift_state('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', contemporary_ecumenical_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b26ffa07-0fe6-4b21-bfb9-d1d06b61f48d', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__orthodox_christological, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, trinitarian_clergy_hierarchy).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, conciliar_creedal_institutions).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, sacramental_authority_structures).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, arian_and_subordinationist_communities).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, unitarian_and_non_trinitarian_groups).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, jewish_and_muslim_theological_interlocutors_excluded_from_communion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, lay_believers_within_orthodox_communion).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, lay_believers_within_orthodox_communion).
narrative_ontology:constraint_vindicates(john_1_1_logos__orthodox_christological, nicene_consubstantiality_doctrine).
narrative_ontology:constraint_vindicates(john_1_1_logos__orthodox_christological, hypostatic_union_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers creedal orthodoxy, adjudicates who counts as within communion, and derives sacramental and magisterial authority from the incarnational reading of Logos. Sets catechesis, liturgy, and canon law around this reading and enforces boundary conditions through excommunication, anathema, or denial of sacraments.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, trinitarian_clergy_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The councils (Nicaea, Constantinople, Chalcedon) and their creedal formulas gain their continuing authority from this reading being treated as settled apostolic doctrine rather than one contested interpretive tradition among several. Their institutional legitimacy is bound to the incarnational claim remaining unrevisable.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, conciliar_creedal_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__orthodox_christological, conciliar_creedal_institutions, agenda_setter).

% Priesthood, ordination, and the sacraments (especially the Eucharist and baptismal formulas) derive their theological warrant from the incarnation of a divine Logos who unites divine and human nature. If Logos is merely functional speech-act language, sacramental efficacy claims lose their ontological ground.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, sacramental_authority_structures, beneficiary,
    institutional, generational, arbitrage, global).

% Hold that the Logos is the highest created being, not co-eternal with the Father. Under this reading's enforcement (post-Nicaea, post-381) their Christology is formally anathematized, their clergy delegitimized, and their communities historically subject to exile, property confiscation, and loss of imperial recognition. Their exit is constrained by social, familial, and political embeddedness in Christian societies that treat the orthodox reading as the only legitimate one.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, arian_and_subordinationist_communities, payer,
    moderate, generational, constrained, regional).

% Modern and historical non-Trinitarian Christian movements (Unitarians, Jehovah's Witnesses, some Christadelphian and Socinian traditions) reject the ontological identity of Logos with the second person of the Trinity. They are excluded from ecumenical communion, denied recognition as 'Christian' by mainstream bodies in interfaith and legal contexts, and bear reputational and communal costs for the divergence. Exit from the label contest is not meaningfully available since the orthodox reading controls institutional recognition of what counts as Christianity itself.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, unitarian_and_non_trinitarian_groups, payer,
    powerless, generational, trapped, global).

% Both traditions reject any hypostatic plurality within the Godhead as a matter of core doctrine (shituf, shirk). They are not merely victims of exclusion from Christian communion but structurally cannot be brought inside this reading's boundary without abandoning their own founding monotheistic commitments. Historically, the orthodox incarnational reading has supplied theological justification for supersessionist and polemical postures toward both communities; they are not seated in the councils that fixed the reading and have no vote in its maintenance.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, jewish_and_muslim_theological_interlocutors_excluded_from_communion, excluded,
    moderate, civilizational, constrained, global).

% Receive coherent liturgical, sacramental, and pastoral identity structured around the incarnation. Also bear the cost of doctrinal enforcement when their own private doubts or heterodox family members are treated as grounds for pastoral discipline or exclusion from sacraments.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, lay_believers_within_orthodox_communion, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__orthodox_christological, lay_believers_within_orthodox_communion, payer).

% Study the textual, philological, and historical development of Logos Christology, including the conciliar politics (imperial pressure, factional disputes) that shaped which reading became codified as orthodox. Can trace how the Chalcedonian settlement's victors wrote the losing readings' theology into the historical record as heresy.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, historical_and_comparative_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__orthodox_christological, conciliar_creedal_institutions).
narrative_ontology:fixing_cost_class(john_1_1_logos__orthodox_christological, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, transmissible answer to 'who is Jesus Christ ontologically,' allowing sacramental practice, creedal recitation, catechesis, and cross-generational doctrinal transmission to proceed without perpetual re-litigation of first-order metaphysics at every liturgy.
% TRANSFER_FUNCTION: Moves recognition, communion access, clerical legitimacy, and historical the label 'Christian' toward Trinitarian-confessing bodies and away from subordinationist and non-Trinitarian bodies; historically also moved political patronage, property, and imperial favor toward the Nicene party after the fourth-century settlements.
% ABSENT_VOICES: Jewish and Muslim theological interlocutors, whose own monotheistic commitments are structurally incompatible with any hypostatic plurality reading, were never seated in the councils that fixed this doctrine and have no standing to object within the tradition's own adjudicative process; Arian and Socinian voices were present at some councils but were outvoted, exiled, or later suppressed by imperial enforcement rather than persuaded.
% DISAPPEARANCE_RATIONALE: If the orthodox incarnational reading lost its institutional monopoly overnight, sacramental theology, ordination requirements, ecumenical recognition criteria, and denominational boundary lines would all require renegotiation; centuries of anathemas, excommunications, and denominational splits are load-bearing on this specific reading remaining the adjudicating standard for 'Christian' identity.
% FOUNDING_PROBLEM: Fourth-century Christian communities faced genuine disunity over whether the Logos of John 1 was co-eternal with the Father or a subordinate/created mediator, threatening both doctrinal coherence and imperial political unity under Constantine and successors.
% FOUNDING_PROBLEM_CORROBORATION: Conciliar and magisterial sources (internal to the benefiting institutions) attest the problem was resolved correctly and permanently. Historians of early Christianity and religious studies scholars operating outside confessional commitment (e.g., studies of the political dynamics at Nicaea and Constantinople) attest that the 'resolution' was substantially shaped by imperial enforcement and factional politics rather than settled by exegesis alone, and that the founding problem — genuine textual and theological ambiguity in the Johannine prologue — remains live rather than dead, since subordinationist and non-Trinitarian readings continue to find textual support among non-confessional scholars.
narrative_ontology:disappearance_verdict(john_1_1_logos__orthodox_christological, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__orthodox_christological, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__orthodox_christological, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(john_1_1_logos__orthodox_christological, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__orthodox_christological, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__orthodox_christological_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__orthodox_christological, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__orthodox_christological_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the reading's persistence depends on continued institutional gatekeeping of sacramental and communal recognition, not merely on textual argument. Suppression is higher (0.71) because enforcement historically included exile, property loss, and denial of political legitimacy for dissenting readings (Arian controversies, post-Chalcedonian schisms), and continues today as denial of ecumenical recognition. Theater ratio is moderate (0.28): much of the conciliar and catechetical apparatus performs genuine doctrinal transmission function, but a nontrivial share (peaking near 0.35 around 900 CE, the height of medieval doctrinal enforcement machinery) is defensive posture rather than live theological engagement. The temporal series traces a genuine historical arc: extraction and suppression spike sharply after Nicaea/Constantinople (t=100-400) when imperial enforcement machinery was built, ease somewhat in the medieval and early modern periods as the boundary became settled fact rather than contested politics, then rise again toward the present (t=1700, representing the contemporary era) as ecumenical and interfaith contexts renew pressure on exclusivist boundary-drawing.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (conciliar institutions, clergy hierarchy), this reading is experienced as settled apostolic truth defended against error — a mountain, in their own framing. From the payer seats (subordinationist and non-Trinitarian communities), the same structure is experienced as an actively enforced exclusion whose persistence depends on continued denial of recognition rather than on the text's plain sense being uncontested. The engine computes this divergence from the structural power/exit data; the claimed_type of tangled_rope is authored precisely because both a genuine coordination function (doctrinal transmission, communal coherence) and an asymmetric extraction (exclusion, anathema, historical persecution) are simultaneously present and neither cancels the other.
 *
 * DIRECTIONALITY LOGIC:
 *   The clergy hierarchy and conciliar institutions sit at the beneficiary end: their authority, sacramental warrant, and historical legitimacy derive directly from this reading remaining the settled standard. Arian/subordinationist and non-Trinitarian groups sit at the target end: their exclusion from communion, historical anathematization, and denial of the 'Christian' label are the transfer this constraint enacts. Jewish and Muslim interlocutors are excluded rather than coordinated — they were never inside the framework that produced this reading and cannot be brought inside it without abandoning their own foundational commitments, which is structurally different from being an internal dissenting minority.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (a stable, transmissible answer to 'who is Christ') was arguably necessary in the fourth century to prevent doctrinal chaos threatening ecclesial unity; whether that founding problem remains live today, in a pluralistic and post-imperial context where doctrinal unity no longer requires political unity, is exactly the founding_problem_status marked 'contested.' Classifying this as tangled_rope rather than snare preserves the genuine coordination function (it is not PURELY extractive — it does solve a real transmission problem) while classifying it as tangled_rope rather than rope registers that the coordination has historically been maintained through enforcement against identifiable victims, not merely through voluntary adherence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settlement_vs_discovery,
    'Was the Nicene-Chalcedonian reading the correct exegetical discovery of what John''s prologue actually asserts, or a politically contingent settlement among live theological alternatives that could have resolved differently under different imperial patronage?',
    'Independent philological and historical analysis of the fourth- and fifth-century conciliar records, imperial correspondence, and the range of pre-Nicene Logos theologies (Justin Martyr, Origen, Arius, and others) to assess how contested the question actually was prior to imperial intervention, versus how settled the text''s plain sense was on exegetical grounds alone.',
    'If the settlement was substantially politically contingent, the reading''s continued institutional enforcement functions closer to extraction dressed as doctrinal necessity; if the text genuinely underdetermines only the orthodox reading, the coordination function is stronger than the extraction framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_vs_discovery, conceptual, 'Whether the orthodox reading''s authority rests on exegetical discovery or political settlement among live alternatives.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the orthodox reading disagree with its sibling readings — is it primarily about the metaphysical status of ho logos in 1:1c (theos vs. a lesser divine title), or primarily about what sarx egeneto in 1:14 asserts (literal ontological embodiment vs. metaphorical dwelling/manifestation)?',
    'Close comparative reading of how each sibling reading''s proponents historically argued the Greek text, particularly the anarthrous theos construction and the semantic range of ginomai in 1:14, cross-checked against how each tradition''s own theologians locate the disagreement.',
    'If the disagreement is located primarily in 1:1c, the subordinationist reading is the nearer sibling (shares the 1:14 incarnational reading) and orthodox_christological would more plausibly relate to it by influences rather than coexists_with; if located primarily in 1:14, the non_incarnational reading is the more structurally distant sibling regardless of 1:1c agreement. This affects how confidently the reading_relations below should be drawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the kernel''s contested locus is the ontology of the Logos or the nature of the incarnation event, and how that shapes sibling-relation typing.').

omega_variable(
    victim_scope_of_anathema,
    'Does ''victim'' status apply equally to historically anathematized fourth-century subordinationist communities, modern non-Trinitarian denominations denied ecumenical recognition, and non-Christian monotheistic traditions excluded from the framework entirely — or are these three structurally distinct forms of exclusion that should not be collapsed into one victim category?',
    'Disaggregate by examining the actual mechanism of exclusion in each case: imperial/conciliar anathema with civil penalties (historical Arians), denominational non-recognition without civil penalty (modern Unitarians), and structural non-participation in the adjudicating framework at all (Jewish and Muslim interlocutors).',
    'Collapsing these into one victims array may overstate homogeneity of harm; disaggregating would support splitting this story further or adding differentiated severity weighting per victim group.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_scope_of_anathema, conceptual, 'Whether the three named victim groups experience structurally comparable or distinct forms of exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__orthodox_christological, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t0, john_1_1_logos__orthodox_christological, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(john_tr_t0, observed).
narrative_ontology:measurement(john_tr_t100, john_1_1_logos__orthodox_christological, theater_ratio, 100, 0.2).
narrative_ontology:measurement_basis(john_tr_t100, observed).
narrative_ontology:measurement(john_tr_t400, john_1_1_logos__orthodox_christological, theater_ratio, 400, 0.3).
narrative_ontology:measurement_basis(john_tr_t400, observed).
narrative_ontology:measurement(john_tr_t900, john_1_1_logos__orthodox_christological, theater_ratio, 900, 0.35).
narrative_ontology:measurement_basis(john_tr_t900, observed).
narrative_ontology:measurement(john_tr_t1500, john_1_1_logos__orthodox_christological, theater_ratio, 1500, 0.3).
narrative_ontology:measurement_basis(john_tr_t1500, observed).
narrative_ontology:measurement(john_tr_t1700, john_1_1_logos__orthodox_christological, theater_ratio, 1700, 0.28).
narrative_ontology:measurement_basis(john_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(john_be_t0, john_1_1_logos__orthodox_christological, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(john_be_t0, observed).
narrative_ontology:measurement(john_be_t100, john_1_1_logos__orthodox_christological, base_extractiveness, 100, 0.55).
narrative_ontology:measurement_basis(john_be_t100, observed).
narrative_ontology:measurement(john_be_t400, john_1_1_logos__orthodox_christological, base_extractiveness, 400, 0.68).
narrative_ontology:measurement_basis(john_be_t400, observed).
narrative_ontology:measurement(john_be_t900, john_1_1_logos__orthodox_christological, base_extractiveness, 900, 0.6).
narrative_ontology:measurement_basis(john_be_t900, observed).
narrative_ontology:measurement(john_be_t1500, john_1_1_logos__orthodox_christological, base_extractiveness, 1500, 0.55).
narrative_ontology:measurement_basis(john_be_t1500, observed).
narrative_ontology:measurement(john_be_t1700, john_1_1_logos__orthodox_christological, base_extractiveness, 1700, 0.62).
narrative_ontology:measurement_basis(john_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t0, john_1_1_logos__orthodox_christological, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(john_su_t0, observed).
narrative_ontology:measurement(john_su_t100, john_1_1_logos__orthodox_christological, suppression_requirement, 100, 0.5).
narrative_ontology:measurement_basis(john_su_t100, observed).
narrative_ontology:measurement(john_su_t400, john_1_1_logos__orthodox_christological, suppression_requirement, 400, 0.85).
narrative_ontology:measurement_basis(john_su_t400, observed).
narrative_ontology:measurement(john_su_t900, john_1_1_logos__orthodox_christological, suppression_requirement, 900, 0.7).
narrative_ontology:measurement_basis(john_su_t900, observed).
narrative_ontology:measurement(john_su_t1500, john_1_1_logos__orthodox_christological, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement_basis(john_su_t1500, observed).
narrative_ontology:measurement(john_su_t1700, john_1_1_logos__orthodox_christological, suppression_requirement, 1700, 0.71).
narrative_ontology:measurement_basis(john_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__orthodox_christological, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__orthodox_christological, john_1_1_logos__subordinationist).
narrative_ontology:affects_constraint(john_1_1_logos__orthodox_christological, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposed from the natural-language label 'the meaning of the Johannine Logos in John 1:1-14.' Each sibling reading (orthodox_christological, subordinationist, non_incarnational_monotheist) has its own ε, its own beneficiary/victim structure, and its own claimed_type, per the ε-invariance principle. This file (orthodox_christological) shows the highest suppression and the most institutionally concentrated beneficiary set, since it is the reading that achieved conciliar/imperial codification and subsequently enforced its boundary against the other two readings. The subordinationist sibling shows the historical victim's-eye view of the same conciliar events; the non_incarnational_monotheist sibling shows a reading with minimal institutional enforcement apparatus (largely academic/minority-tradition, lower suppression, likely rope or mountain-adjacent depending on its own metrics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
