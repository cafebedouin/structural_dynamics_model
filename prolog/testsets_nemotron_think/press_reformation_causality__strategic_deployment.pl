% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Deployment of the Printing Press by Reformers Against Church Authority
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story models the strategic deployment of the printing
 *   press by Protestant reformers and their printer allies as a deliberate
 *   weapon against the Catholic Church's communication monopoly (1450-1650).
 *   The press itself is a coordination technology (rope), but its strategic
 *   deployment by reformers functions as a snare against Church authority —
 *   extracting interpretive control, material resources, and legitimacy. The
 *   claimed type is tangled_rope because the same press infrastructure
 *   simultaneously coordinated the Reformation movement (genuine coordination
 *   for reformers/printers) and extracted authority from the Church
 *   (asymmetric extraction requiring active enforcement via censorship,
 *   Index, and confessional policing). The kernel is
 *   press_reformation_causality; this reading (strategic_deployment)
 *   instantiates one constraint among three siblings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.68).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.72).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.68).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Deployment of the Printing Press by Reformers Against Church Authority").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, 'a11e0207-1e0b-45a7-bb89-34f86f30e5eb').
narrative_ontology:cs_kernel_codification('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', distributed).
narrative_ontology:cs_authority_grounding('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', extraction).
narrative_ontology:cs_interpretation_layer_present('a11e0207-1e0b-45a7-bb89-34f86f30e5eb').
narrative_ontology:cs_reading_relation('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', foundational, human_agency_primacy_in_technological_change).
narrative_ontology:cs_axiom_status(human_agency_primacy_in_technological_change, holdable).
narrative_ontology:cs_axiom_grounding('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', human_agency_primacy_in_technological_change, deontological).
narrative_ontology:cs_axiom('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', secondary, strategic_intent_legitimates_outcomes).
narrative_ontology:cs_axiom_status(strategic_intent_legitimates_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', strategic_intent_legitimates_outcomes, instrumental).
narrative_ontology:cs_reference_frame('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', pre_print_communication_monopoly).
narrative_ontology:cs_drift_state('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', post_westphalia_confessional_settlement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a11e0207-1e0b-45a7-bb89-34f86f30e5eb', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, commercial_printers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_church_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, lay_believers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, lay_believers).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, vernacular_scripture_access).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, priesthood_of_all_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Martin Luther, John Calvin, and other reform leaders used the press to bypass Church censorship, distribute vernacular translations, and coordinate theological arguments across territories. They gained massive audience reach and doctrinal control but remained dependent on printer networks and vulnerable to imperial edicts.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, protestant_reformers, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, protestant_reformers, beneficiary).

% Printers in Wittenberg, Basel, Strasbourg, and Geneva profited enormously from Reformation pamphlets, Bibles, and polemics. They exercised editorial discretion over what they printed, sometimes shaping texts. Their exit was mobile — they could relocate to sympathetic cities — but their livelihood depended on the Reformation's momentum.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, commercial_printers, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, commercial_printers, agenda_setter).

% The papacy, episcopacy, and Inquisition lost control over religious communication. Their monopoly on Latin liturgy, doctrinal interpretation, and censorship collapsed as vernacular print spread. They responded with the Index Librorum Prohibitorum, the Council of Trent, and jurisdictional censorship — but could not exit the constraint; the press had permanently altered the information environment they governed.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_church_authority, payer,
    institutional, civilizational, trapped, continental).

% Ordinary people gained direct access to scripture and religious argument in their own languages, enabling new forms of piety and dissent. But they also became targets of confessional policing, forced to choose sides in confessional conflicts, and subjected to new forms of disciplinary control by both Protestant and Catholic authorities.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, lay_believers, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, lay_believers, payer).

% Princes and city councils exploited the press to assert independence from Rome, confiscate Church property, and standardize administration. They licensed printers, granted privileges, and suppressed dissent when it threatened their order. Their position was analytical-arbitrage: they could back either side or neither, extracting political gain from the press's disruption.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, secular_authorities, observer,
    powerful, generational, arbitrage, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solved the coordination problem of disseminating theological arguments and vernacular scripture across fragmented German territories and beyond, enabling reformers to synchronize doctrine, recruit followers, and build alternative ecclesiastical structures without physical assembly.
% TRANSFER_FUNCTION: The arrangement moved authority over religious truth from the Latin-literate clerical hierarchy to vernacular-literate lay publics and their chosen teachers, transferring interpretive control, material resources (Church lands, tithes), and legitimacy from the Catholic Church to Protestant movements and their secular protectors.
% ABSENT_VOICES: Peasant rebels (e.g., Thomas Müntzer's followers) and Anabaptist radicals who saw the press as a tool for total social transformation were marginalized by mainstream reformers once the movement consolidated. Jewish communities, subject to new forms of print polemic and censorship, had no seat in the emerging confessional order. Women, though active as printers and readers, were excluded from formal theological authority.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of the press by reformers vanished overnight — meaning the coordinated print campaign, the printer-reformer networks, and the vernacular translation project — the Reformation would have remained a local academic dispute. The Catholic Church's communication monopoly would have held. The confessional map of Europe, the rise of vernacular literacy, and the modern public sphere would not exist in their historical form.
% FOUNDING_PROBLEM: The Catholic Church's control over religious communication — Latin liturgy, clerical monopoly on scripture interpretation, censorship of dissent — prevented theological reform and enabled financial extraction (indulgences, benefices) that reformers and princes sought to dismantle.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Church communication monopoly) is dead: the Catholic Church itself abandoned Latin-only liturgy at Vatican II (1962-65), embraced vernacular scripture, and relinquished censorship powers. This is corroborated by Catholic historians (e.g., Hubert Jedin, John O'Malley) and the Church's own conciliar documents — sources outside the Protestant beneficiary set.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the massive transfer of authority, wealth, and communicative control from Church to reformers/princes. Suppression (0.72) captures the Church's intensive countermeasures (Index, Inquisition, censorship) and the reformers' own enforcement of orthodoxy once established. Theater ratio (0.28) is moderate: early print was genuinely functional for coordination; later confessional publishing became increasingly performative (commemorative folios, polemical excess). Accessibility collapse (0.65) is high but not total: manuscript culture persisted, oral preaching remained crucial, and Catholic regions maintained alternative communication. Resistance (0.81) is very high: the Church fought a century-long battle to contain the press's disruption.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer seat, the press is a liberating coordination tool (rope). From the Church seat, it is an extractive weapon (snare). From the printer seat, it is a profitable coordination platform with growing performative overhead. From the lay believer seat, it is a double-edged sword: empowerment and subjection. The engine computes this divergence; the authored claim (tangled_rope) captures the structural hybridity without resolving it.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are structural beneficiaries (d ≈ 0.15-0.25): they gained audience, revenue, and doctrinal control. The Catholic Church is the primary target (d ≈ 0.9): it lost its communication monopoly and faced existential institutional threat. Lay believers are near-symmetric (d ≈ 0.5): they gained vernacular access but became objects of confessional discipline. Secular authorities are analytical-arbitrage (d ≈ 0.1): they extracted political gain regardless of theological outcome. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Church communication monopoly) is dead — the Church itself abandoned it. Yet the confessional structures, censorship apparatuses, and print polemics persisted long after the original problem vanished. This is classic mandatrophy: the arrangement (confessional print culture, state churches, censorship) outlived its founding justification. The founding_problem_status = dead + disappearance_verdict = world_rearranges mismatch flags this as a capture/zombie dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the strategic_deployment reading foreclose the technological_determinism reading, or do they coexist as competing explanations held by different historiographical camps?',
    'Historiographical analysis: if standard narratives treat them as mutually exclusive (one must be wrong), the relation is forecloses; if they are presented as complementary emphases (technology enabled, agents deployed), the relation is coexists_with.',
    'If forecloses, the kernel has a genuine structural split; if coexists_with, the kernel''s contest is framing-level, not logical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Structural relationship between strategic_deployment and technological_determinism readings of the press_reformation_causality kernel.').

omega_variable(
    press_as_rope_vs_snare_boundary,
    'Where exactly does the press''s coordination function (rope) end and its extraction function against the Church (snare) begin? Are they separable phases or simultaneous aspects of the same deployment?',
    'Micro-historical analysis of printer-reformer contracts, censorship records, and print runs: did printers coordinate with reformers first, then extract from Church; or was extraction the coordination mechanism from the start?',
    'If separable phases, the constraint may be two stories (early rope, late snare). If simultaneous, tangled_rope is the correct single classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(press_as_rope_vs_snare_boundary, empirical, 'Whether the press''s coordination and extraction functions are temporally separable or structurally simultaneous in the strategic deployment.').

omega_variable(
    secular_authority_capture,
    'Did secular authorities capture the press''s extraction for their own ends, making them secondary beneficiaries rather than mere observers?',
    'Comparative analysis of princely printing privileges, confiscation of Church property, and state control of censorship post-1555.',
    'If secular authorities are beneficiaries, the extraction structure is three-way (reformers, printers, princes vs. Church), altering the tangled_rope''s asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_authority_capture, empirical, 'Whether secular authorities should be classified as beneficiaries rather than observers in the strategic deployment constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_reformation_strategic_tr_t1450, press_reformation_causality__strategic_deployment, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(press_reformation_strategic_tr_t1480, press_reformation_causality__strategic_deployment, theater_ratio, 1480, 0.08).
narrative_ontology:measurement(press_reformation_strategic_tr_t1517, press_reformation_causality__strategic_deployment, theater_ratio, 1517, 0.12).
narrative_ontology:measurement(press_reformation_strategic_tr_t1530, press_reformation_causality__strategic_deployment, theater_ratio, 1530, 0.22).
narrative_ontology:measurement(press_reformation_strategic_tr_t1555, press_reformation_causality__strategic_deployment, theater_ratio, 1555, 0.28).
narrative_ontology:measurement(press_reformation_strategic_tr_t1600, press_reformation_causality__strategic_deployment, theater_ratio, 1600, 0.31).
narrative_ontology:measurement(press_reformation_strategic_tr_t1650, press_reformation_causality__strategic_deployment, theater_ratio, 1650, 0.28).

% Extraction over time
narrative_ontology:measurement(press_reformation_strategic_be_t1450, press_reformation_causality__strategic_deployment, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement(press_reformation_strategic_be_t1480, press_reformation_causality__strategic_deployment, base_extractiveness, 1480, 0.22).
narrative_ontology:measurement(press_reformation_strategic_be_t1517, press_reformation_causality__strategic_deployment, base_extractiveness, 1517, 0.45).
narrative_ontology:measurement(press_reformation_strategic_be_t1530, press_reformation_causality__strategic_deployment, base_extractiveness, 1530, 0.62).
narrative_ontology:measurement(press_reformation_strategic_be_t1555, press_reformation_causality__strategic_deployment, base_extractiveness, 1555, 0.68).
narrative_ontology:measurement(press_reformation_strategic_be_t1600, press_reformation_causality__strategic_deployment, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(press_reformation_strategic_be_t1650, press_reformation_causality__strategic_deployment, base_extractiveness, 1650, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(press_reformation_strategic_su_t1450, press_reformation_causality__strategic_deployment, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement(press_reformation_strategic_su_t1480, press_reformation_causality__strategic_deployment, suppression_requirement, 1480, 0.25).
narrative_ontology:measurement(press_reformation_strategic_su_t1517, press_reformation_causality__strategic_deployment, suppression_requirement, 1517, 0.55).
narrative_ontology:measurement(press_reformation_strategic_su_t1530, press_reformation_causality__strategic_deployment, suppression_requirement, 1530, 0.72).
narrative_ontology:measurement(press_reformation_strategic_su_t1555, press_reformation_causality__strategic_deployment, suppression_requirement, 1555, 0.75).
narrative_ontology:measurement(press_reformation_strategic_su_t1600, press_reformation_causality__strategic_deployment, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(press_reformation_strategic_su_t1650, press_reformation_causality__strategic_deployment, suppression_requirement, 1650, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__strategic_deployment, 0.03).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, vernacular_literacy_standardization).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, confessional_censorship_regimes).

% DUAL FORMULATION NOTE:
% This constraint (strategic_deployment) is one of three readings of the press_reformation_causality kernel. The technological_determinism reading claims the press's material properties alone caused the Reformation (Mountain-like low extraction). The co_constitution reading claims feedback loops between print economy and religious controversy (Tangled Rope with different beneficiary structure). This reading claims deliberate strategic weaponization by identifiable agents (Tangled Rope with reformers/printers as beneficiaries, Church as victim). The ε values differ substantially across readings because they describe different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causality__strategic_deployment, institutional, 0.88).
constraint_indexing:directionality_override(press_reformation_causality__strategic_deployment, organized, 0.2).
constraint_indexing:directionality_override(press_reformation_causality__strategic_deployment, moderate, 0.25).
constraint_indexing:directionality_override(press_reformation_causality__strategic_deployment, powerless, 0.5).
constraint_indexing:directionality_override(press_reformation_causality__strategic_deployment, powerful, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
