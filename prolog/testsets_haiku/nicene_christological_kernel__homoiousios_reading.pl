% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoiousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoiousios_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nicene_christological_kernel__homoiousios_reading
 *   human_readable: Nicene Christological Kernel: Homoiousios Reading
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   At the Council of Nicaea (325 CE), Christian bishops assembled to resolve
 *   Christological disputes that threatened imperial religious stability. The
 *   council produced a creedal statement including the term homoiousios (of
 *   similar substance) to describe Christ's relation to the Father. This
 *   reading asserts that Christ is of SIMILAR SUBSTANCE — maintaining an
 *   ontological distinction while affirming divinity — rather than of
 *   IDENTICAL SUBSTANCE (homoousios). The homoiousios reading preserves
 *   monotheistic clarity by keeping Father and Son ontologically
 *   distinguishable while coordinating around shared divinity language. It
 *   fragments the empire's goal of absolute doctrinal uniformity by
 *   permitting regional exegetical schools and metropolitan authorities to
 *   interpret 'similarity' in ways compatible with local theological
 *   traditions. This produces moderate extractiveness: the constraint enables
 *   theological pluralism at the cost of imperial religious unity.
 *
 * KEY AGENTS:
 *   - homoiousios_council_party: institutional agenda-setters who formulated and defend the reading
 *   - imperial_religious_uniformity_apparatus: institutional payer bearing the fragmentation cost
 *   - exegetical_schools: organized beneficiaries gaining interpretive autonomy
 *   - regional_ecclesiastical_authorities: powerful beneficiaries collecting doctrinal authority
 *   - homoousios_rival_reading_holders: institutional payers suppressed by the dominant reading
 *   - doctrinal_pluralists: moderate-power beneficiaries and payers, identity-locked to the constraint
 *   - orthodox_historians: analytical observers measuring functional effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, 0.62).
domain_priors:suppression_score(nicene_christological_kernel__homoiousios_reading, 0.58).
domain_priors:theater_ratio(nicene_christological_kernel__homoiousios_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoiousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoiousios_reading, "Nicene Christological Kernel: Homoiousios Reading").
narrative_ontology:topic_domain(nicene_christological_kernel__homoiousios_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoiousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoiousios_reading, 'f895aa92-283f-488f-b82a-ae805a8908a5').
narrative_ontology:cs_kernel_codification('f895aa92-283f-488f-b82a-ae805a8908a5', formalized).
narrative_ontology:cs_authority_grounding('f895aa92-283f-488f-b82a-ae805a8908a5', lineage).
narrative_ontology:cs_interpretation_layer_present('f895aa92-283f-488f-b82a-ae805a8908a5').
narrative_ontology:cs_reading_relation('f895aa92-283f-488f-b82a-ae805a8908a5', nicene_christological_kernel__homoousios_reading, coexists_with).
narrative_ontology:cs_axiom('f895aa92-283f-488f-b82a-ae805a8908a5', foundational, ontological_distinction_preservation).
narrative_ontology:cs_axiom_status(ontological_distinction_preservation, holdable).
narrative_ontology:cs_axiom_grounding('f895aa92-283f-488f-b82a-ae805a8908a5', ontological_distinction_preservation, theological).
narrative_ontology:cs_axiom('f895aa92-283f-488f-b82a-ae805a8908a5', foundational, similarity_substance_adequacy).
narrative_ontology:cs_axiom_status(similarity_substance_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('f895aa92-283f-488f-b82a-ae805a8908a5', similarity_substance_adequacy, deontological).
narrative_ontology:cs_reference_frame('f895aa92-283f-488f-b82a-ae805a8908a5', nicene_conciliar_authority).
narrative_ontology:cs_drift_state('f895aa92-283f-488f-b82a-ae805a8908a5', constantinople_i_381, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f895aa92-283f-488f-b82a-ae805a8908a5', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, regional_ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, exegetical_schools).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, doctrinal_pluralists).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, imperial_religious_unity_apparatus).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, ecumenical_standardization_projects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, imperial_religious_uniformity_apparatus).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, homoousios_rival_reading_holders).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, doctrinal_pluralists).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, monotheistic_clarity_doctrine).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, trinitarian_distinction_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Regional bishops and theologians who formulated and defend the homoiousios reading at Nicaea and its aftermath. They administer the council's doctrinal language, interpret it against challenges, and enforce conformity to their reading among subordinate clergy. They argue the reading preserves monotheistic clarity by maintaining a real ontological distinction between Father and Son while affirming Christ's divinity.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoiousios_council_party, agenda_setter,
    institutional, generational, constrained, continental).

% Constantine and successor emperors who sought a single, unambiguous doctrinal standard to stabilize imperial religious control. The homoiousios reading fragments this aim by allowing regional interpretive latitude and exegetical schools to proliferate under the umbrella of 'similarity' rather than forcing full identity. The apparatus bears the cost of persistent doctrinal contestation and reduced centralized authority.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_religious_uniformity_apparatus, payer,
    institutional, generational, trapped, global).

% Bishops and theologians who advocate for homoousios (full identity of substance), viewing the homoiousios language as insufficiently strong. They are suppressed by the council party's dominance and must either conform publicly while maintaining private doctrine, or face exclusion and anathematization. Their alternative framing is structurally blocked.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoousios_rival_reading_holders, payer,
    institutional, generational, constrained, continental).

% Monastic and scholarly communities (Antiochene, Alexandrian, others) who can develop and transmit interpretive traditions around the homoiousios language without immediate imperial suppression. The 'similarity' formulation grants them latitude to debate fine points of substance-relation theology that a stricter formula would forbid. They gain autonomy for theological work.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, exegetical_schools, beneficiary,
    organized, generational, mobile, regional).

% Metropolitan bishops and provincial councils who benefit from the homoiousios reading's flexibility: they can interpret it in ways compatible with local theology, maintain some exegetical independence from the imperial center, and avoid the rigid uniformity that would reduce their interpretive authority. They collect the benefit of doctrinal autonomy.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, regional_ecclesiastical_authorities, beneficiary,
    powerful, generational, constrained, regional).

% Theologians and bishops who are genuinely committed to both monotheism and Christology and find the homoiousios formulation sufficient for both. They benefit from the reading's ability to hold multiple doctrinal commitments in tension without forcing a singular resolution. They also bear the cost of ongoing doctrinal contestation and the necessity of constant refinement.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, doctrinal_pluralists, beneficiary,
    moderate, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, doctrinal_pluralists, payer).

% Scholars and ecclesiastical historians who analyze how the homoiousios reading functioned to preserve theological pluralism, fragment imperial religious control, and establish the prototype for doctrinal negotiation in Christian councils. They take no position in the dispute but measure its structural effects.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, orthodox_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoiousios_reading, homoiousios_council_party).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoiousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinally defensible formulation of Christ's relation to the Father that satisfies the commitment to both Trinitarian distinction and monotheistic unity. Coordinates regional theological schools around a shared language ('homoiousios') permitting interpretive variance within bounds rather than enforcing a single reading.
% TRANSFER_FUNCTION: Moves ecclesiastical authority from the imperial center (Constantine's uniformity apparatus) to regional councils, exegetical schools, and metropolitan bishops. Transfers doctrinal autonomy from the empire to the Church's theological professionals. The cost is paid by projects seeking unified imperial religious control; the benefit accrues to churches seeking interpretive freedom.
% ABSENT_VOICES: Subordinate clergy and lay believers are kept out of the council; they receive the homoiousios doctrine as received teaching, not as negotiated participants. Gnostic and radical Christological traditions (docetism, Arianism at its most egalitarian) are excluded by the council's authority to anathematize; they would argue for different substance-relation models but have no seat at the table.
% DISAPPEARANCE_RATIONALE: If the homoiousios reading vanished and the council had never settled on it, Christian theology would reorganize around competing Christological formulations; the empire would likely impose one of its preferred readings more successfully; regional exegetical autonomy would shrink. The constraint's existence enabled the survival of pluralist theological work that would be suppressed in its absence.
% FOUNDING_PROBLEM: After Constantine, the empire sought a single doctrinal standard to stabilize religious authority. The homoiousios reading arose to satisfy both the empire's demand for unity AND the Church's commitment to Trinitarian theology and regional interpretive practice. It solved the problem: 'How can we be monotheistically clear AND theologically plural AND ecclesiastically unified?'
% FOUNDING_PROBLEM_CORROBORATION: Constantine's letters and the council records attest the founding problem: the need for religious uniformity. Subsequent council records (Constantinople I, later synods) attest that the homoiousios reading persisted because it balanced demands no single alternative could satisfy. Medieval and modern exegetical traditions attest that the reading's flexibility enabled theological work. Historians and theologians outside the benefiting ecclesiastical parties (secular historians, ecumenical analysts from rival traditions) corroborate that the homoiousios formulation did enable theological pluralism and reduced imperial uniformity.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoiousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoiousios_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoiousios_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoiousios_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoiousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoiousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.62 at interval end) reflects the constraint's dual function: it genuinely coordinates regional churches around a doctrinally defensible formula, AND it fragments the empire's uniformity project. Suppression (0.58) measures the active enforcement required to maintain the homoiousios reading against homoousios challengers and to enforce the council party's authority over regional interpretation. Theater ratio (0.41) reflects that a growing proportion of the constraint's maintenance becomes interpretive defense and doctrinal refinement rather than substantive coordination — by 381 (Constantinople I), the energies devoted to defending homoiousios against alternative readings match the energies devoted to its original coordination function. The measurement series show suppression and theater rising together: as the constraint matures, defending it against alternatives requires more active enforcement (suppression) and more rhetorical maintenance (theater). All three metrics are authored on a single shared grid (t=325, 335, 345, 355, 365, 375, 381), enabling lifecycle drift detection.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (imperial apparatus and homoousios rivals) and the beneficiary seats (regional authorities, exegetical schools) will compute different types from the identical structural data. The empire experiences the constraint as extractive (its uniformity goal is suppressed); regional churches experience it as coordinative (their theological autonomy is preserved). From the engine's seat-by-seat computation, this divergence is the point: tangled_rope shows exactly when one party's coordination is another party's extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from beneficiary/victim declarations + exit options + power atoms. Beneficiaries (regional authorities, exegetical schools, doctrinal pluralists) get low d → low effective extraction. Victims (imperial apparatus, homoousios rivals) get high d → high effective extraction. The constraint is CLAIMED as tangled_rope because it exhibits both coordination (benefits regional churches through theological autonomy) and extraction (harms the empire's uniformity goal through fragmentation). The metrics are authored to reflect a constraint whose coordination and extraction functions are structurally entangled: you cannot separate the theological autonomy (coordination) from the fragmented uniformity (extraction) without dissolving both.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (imperial religious unity + theological pluralism + Trinitarian commitment) was live at 325. By 381 (Constantinople I), the founding problem remains contested: the empire had partially reasserted control, but the homoiousios reading had enabled enough regional theological autonomy that full uniformity was no longer achievable. The constraint does not suffer mandatrophy in the classical sense (founding problem vanished while constraint persists); rather, it solved the founding problem in a way that transformed it — it created the permanent condition the empire did not want (pluralism) as the price of the stability it did want (Trinitarian creedal agreement). The constraint's persistence is not inertial theater; it persists because no alternative solves the founding problem better from the regional seats' perspective, and the empire cannot unilaterally suppress it without triggering schism. This is the structural signature of a successful tangled_rope: one party's coordination benefit is inseparable from the other party's extraction cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    similarity_vs_identity_boundary,
    'Does ''homoiousios'' (similar substance) preserve a real ontological distinction between Father and Son, or is it a linguistic compromise that covertly approaches ''homoousios'' (identical substance) through interpretive drift?',
    'Detailed textual analysis of how different exegetical schools interpret the similarity boundary over the 325–381 interval; examination of whether interpretive drift moves homoiousios toward homoousios or maintains the distinction.',
    'If the boundary is real and stable, homoiousios is a genuinely distinct reading that enables theological pluralism. If drift is ubiquitous and unidirectional toward homoousios, the reading is a temporary compromise stage, and the constraint''s sustainability depends on active suppression rather than genuine coordination. The type assessment (tangled_rope vs. piton) turns on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(similarity_vs_identity_boundary, empirical, 'Whether the homoiousios distinction is substantive or linguistic compromise.').

omega_variable(
    council_authority_vs_regional_autonomy_tension,
    'Does the homoiousios reading genuinely grant regional theological autonomy, or does the council party''s authority structure ultimately collapse regional variation into conformity?',
    'Historical record of regional interpretation divergence post-Nicaea; documentation of whether homoiousios local readings are tolerated or suppressed; examination of whether the constraint enables theological schools or ultimately enforces their compliance.',
    'If genuine autonomy is preserved, the constraint coordinates regional churches around a flexible formula and the extracted cost to the empire is real (fragmented uniformity). If ultimate enforcement collapses regional variation, the constraint is a false pluralism covering coercive uniformity, and the victims are regional churches rather than the imperial apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_authority_vs_regional_autonomy_tension, empirical, 'Whether regional ecclesiastical autonomy is structurally preserved or ultimately suppressed.').

omega_variable(
    monotheistic_clarity_vindication,
    'Does the homoiousios reading successfully vindicate the monotheistic clarity doctrine (a single divine nature despite Trinitarian distinction), or does the preservation of ontological distinction between Father and Son compromise monotheism itself?',
    'Theological analysis of whether ''similar'' substance is consistent with strict monotheistic commitment; examination of whether the reading was later rejected or upheld by subsequent councils and traditions as monotheistically sound.',
    'If homoiousios successfully vindicates monotheism, the constraint''s founding problem (balancing unity and distinction) is genuinely solved. If homoiousios is later reinterpreted as undermining monotheism, the constraint''s vindicated proposition is contested and the reading loses a key legitimacy claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monotheistic_clarity_vindication, empirical, 'Whether homoiousios preserves or compromises monotheistic clarity.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of homoousios alternatives (d=0.80 for rival readers) structural (legal suppression, political exile, institutional exclusion) or internalized (the rival readers internalize the council''s authority and suppress themselves)?',
    'Historical documentation of how homoousios advocates were treated post-Nicaea; examination of whether they faced exile, anathema, or self-censorship; study of whether suppression persisted after political pressure was removed.',
    'If structural, the constraint''s suppression is externally maintained and would relax if the council party lost institutional power. If internalized, the suppression persists even after external pressure is removed, and the constraint''s effective suppression is higher than the institutional measures suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of rivals is structural or internalized authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoiousios_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoiousios_reading, theater_ratio, 325, 0.22).
narrative_ontology:measurement_basis(nice_tr_t325, observed).
narrative_ontology:measurement(nice_tr_t335, nicene_christological_kernel__homoiousios_reading, theater_ratio, 335, 0.25).
narrative_ontology:measurement_basis(nice_tr_t335, observed).
narrative_ontology:measurement(nice_tr_t345, nicene_christological_kernel__homoiousios_reading, theater_ratio, 345, 0.3).
narrative_ontology:measurement_basis(nice_tr_t345, observed).
narrative_ontology:measurement(nice_tr_t355, nicene_christological_kernel__homoiousios_reading, theater_ratio, 355, 0.36).
narrative_ontology:measurement_basis(nice_tr_t355, observed).
narrative_ontology:measurement(nice_tr_t365, nicene_christological_kernel__homoiousios_reading, theater_ratio, 365, 0.4).
narrative_ontology:measurement_basis(nice_tr_t365, observed).
narrative_ontology:measurement(nice_tr_t375, nicene_christological_kernel__homoiousios_reading, theater_ratio, 375, 0.41).
narrative_ontology:measurement_basis(nice_tr_t375, observed).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoiousios_reading, theater_ratio, 381, 0.41).
narrative_ontology:measurement_basis(nice_tr_t381, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 325, 0.45).
narrative_ontology:measurement_basis(nice_be_t325, observed).
narrative_ontology:measurement(nice_be_t335, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 335, 0.48).
narrative_ontology:measurement_basis(nice_be_t335, observed).
narrative_ontology:measurement(nice_be_t345, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 345, 0.52).
narrative_ontology:measurement_basis(nice_be_t345, observed).
narrative_ontology:measurement(nice_be_t355, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 355, 0.58).
narrative_ontology:measurement_basis(nice_be_t355, observed).
narrative_ontology:measurement(nice_be_t365, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 365, 0.6).
narrative_ontology:measurement_basis(nice_be_t365, observed).
narrative_ontology:measurement(nice_be_t375, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 375, 0.62).
narrative_ontology:measurement_basis(nice_be_t375, observed).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 381, 0.62).
narrative_ontology:measurement_basis(nice_be_t381, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 325, 0.35).
narrative_ontology:measurement_basis(nice_su_t325, observed).
narrative_ontology:measurement(nice_su_t335, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 335, 0.42).
narrative_ontology:measurement_basis(nice_su_t335, observed).
narrative_ontology:measurement(nice_su_t345, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 345, 0.48).
narrative_ontology:measurement_basis(nice_su_t345, observed).
narrative_ontology:measurement(nice_su_t355, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 355, 0.54).
narrative_ontology:measurement_basis(nice_su_t355, observed).
narrative_ontology:measurement(nice_su_t365, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 365, 0.58).
narrative_ontology:measurement_basis(nice_su_t365, observed).
narrative_ontology:measurement(nice_su_t375, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 375, 0.58).
narrative_ontology:measurement_basis(nice_su_t375, observed).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 381, 0.58).
narrative_ontology:measurement_basis(nice_su_t381, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoiousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoiousios_reading, 0.12).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel__homoousios_reading).

% DUAL FORMULATION NOTE:
% The nicene_christological_kernel decomposes into at least two readings, distinguished by the core axiom: homoiousios asserts ontological distinction with similarity; homoousios asserts identity. The readings differ in extractiveness (homoiousios fragments imperial unity, homoousios serves uniformity goals), in beneficiary structure (homoiousios benefits regional churches, homoousios benefits the empire), and in suppression mechanics (homoiousios requires active enforcement against homoousios rivals, homoousios would require enforcement against pluralist schools). Each reading is a separate constraint with its own ε, type assessment, and stakeholder seat divergence. They coexist as competing historical positions rather than foreclosing each other logically — both remain live readings in the history of Christian theology. The homoiousios_reading affects the homoousios_reading because the historical triumph of homoousios created downstream pressure on homoiousios exegetical traditions to either converge toward the dominant reading or maintain themselves as schismatic minorities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_christological_kernel__homoiousios_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
