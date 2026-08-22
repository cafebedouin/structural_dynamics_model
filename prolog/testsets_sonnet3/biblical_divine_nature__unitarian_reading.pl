% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Reading of Divine Nature (Father Alone Is God)
 *   domain: theology/religious_authority
 *
 * SUMMARY:
 *   This constraint models the unitarian reading of the divine-nature kernel:
 *   the claim that God is numerically singular, identical with the Father
 *   alone, with the Son and Spirit holding subordinate, derivative, or
 *   created status. This reading was a live and contested position in the
 *   earliest centuries of Christian doctrine (subordinationist christologies,
 *   dynamic monarchianism) and was progressively excluded by name (as
 *   'Arianism' and its cognates) through the ecumenical councils and their
 *   creedal formulas. The reading persists today in various unitarian,
 *   biblical unitarian, and some restorationist traditions. Extraction here
 *   runs against institutional hierarchy and credal orthodoxy: the unitarian
 *   reading, where it gains adherents, withdraws legitimacy, funding, and
 *   disciplinary authority from ecclesial structures whose historical
 *   function is substantially constituted by enforcing anti-subordinationist
 *   doctrine. The suppression that this reading has historically met
 *   (condemnation, exile, book-burning, exclusion from communion) is authored
 *   on the institutional-hierarchy side of the ledger, while the reading
 *   itself, from its own adherents' vantage, is experienced as liberating
 *   them from imposed philosophical machinery.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.58).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.72).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Divine Nature (Father Alone Is God)").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, 'b197d8ec-40ce-4144-bcfe-c7fbb3784ff4').
narrative_ontology:cs_kernel_codification('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', fixed_text).
narrative_ontology:cs_authority_grounding('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', distributed).
narrative_ontology:cs_reading_relation('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', foundational, numerical_singularity_of_the_father).
narrative_ontology:cs_axiom_status(numerical_singularity_of_the_father, holdable).
narrative_ontology:cs_axiom_grounding('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', numerical_singularity_of_the_father, deontological).
narrative_ontology:cs_axiom('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', foundational, son_and_spirit_ontologically_subordinate).
narrative_ontology:cs_axiom_status(son_and_spirit_ontologically_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', son_and_spirit_ontologically_subordinate, conventional).
narrative_ontology:cs_reference_frame('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', pre_nicene_subordinationist_consensus).
narrative_ontology:cs_drift_state('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', post_nicene_constantinopolitan_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('b197d8ec-40ce-4144-bcfe-c7fbb3784ff4', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_congregations).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, biblicist_lay_readers).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_ecclesial_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, biblicist_lay_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Read scripture as establishing the Father as the sole numerically singular God, with the Son and Spirit in subordinate or derivative status. This reading frees them from creedal formulas they regard as post-biblical philosophical accretion, letting them organize worship and doctrine directly from text without deferring to councils or magisterial rulings.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_congregations, beneficiary,
    moderate, generational, mobile, regional).

% Individual believers who find the unitarian reading textually simpler and more intuitively monotheistic. They gain interpretive authority over their own faith but often pay socially and relationally — excommunication, family rupture, exclusion from trinitarian communities — for holding the position openly.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, biblicist_lay_readers, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, biblicist_lay_readers, payer).

% Councils, magisteria, and ordained hierarchies whose authority is substantially constituted by their historical role as adjudicators and enforcers of trinitarian orthodoxy. The unitarian reading, if it gained traction, would dissolve the doctrinal basis for much of their disciplinary and interpretive power — they cannot simply exit this contest without losing constitutive function.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_ecclesial_hierarchy, payer,
    institutional, civilizational, trapped, global).

% The body of creeds, confessions, and heresiological categories (Arianism, subordinationism) built to name and exclude readings like this one. Every anti-subordinationist clause in the historic creeds exists specifically to foreclose this reading; the apparatus's coherence depends on this reading remaining classified as heresy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy_apparatus, payer,
    institutional, civilizational, trapped, global).

% Ordained ministers whose training, credentials, and pastoral authority are built on trinitarian formulation. They are not part of the unitarian reading's own internal conversation but would strenuously object to it; their objections are treated by unitarian communities as evidence of institutional self-interest rather than as theological input to be weighed.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_clergy, excluded,
    organized, generational, constrained, national).

% Study the textual and historical development of both readings without institutional stake in either outcome. They can trace how subordinationist readings predate and coexist with the Nicene settlement, and how the anti-subordinationist creedal language was itself a contested, gradually consolidated response.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, textually-grounded account of monotheism that avoids what unitarian readers see as the philosophical complexity and potential logical incoherence of three-in-one formulations, allowing communities to coordinate worship and doctrine around a simpler numerical claim.
% TRANSFER_FUNCTION: Moves interpretive and disciplinary authority away from councils, creeds, and ordained hierarchies and toward individual scriptural interpretation and congregational autonomy; correspondingly withdraws legitimacy and resource flows (tithes, credentialing power, doctrinal gatekeeping) from institutions built on trinitarian orthodoxy.
% ABSENT_VOICES: Trinitarian clergy and conciliar historians who would argue the unitarian reading ignores or under-weighs pre-Nicene high-Christology texts and patristic consensus; they are treated within unitarian discourse primarily as interested parties defending inherited power rather than as theological interlocutors.
% DISAPPEARANCE_RATIONALE: Unitarian communities would say the world stays largely unchanged doctrinally — they simply return to what they see as the original biblical position, with congregational life continuing much as before. Institutional hierarchies would say the world rearranges substantially: centuries of creedal settlement, ordination requirements, and disciplinary categories built around anti-subordinationism would need to be renegotiated or discarded, and the mechanism by which those institutions exclude and name heresy would lose its object.
% FOUNDING_PROBLEM: Early Jewish-Christian communities needed to reconcile strict monotheism (the Shema) with the exalted status ascribed to Jesus in the New Testament, without collapsing into either polytheism or a philosophically dense metaphysics of shared essence.
% FOUNDING_PROBLEM_CORROBORATION: Unitarian theologians and some historical-critical biblical scholars attest the founding problem remains live and that the trinitarian settlement was a later philosophical overlay rather than a solution demanded by the earliest texts. Patristic scholars and historians of the ecumenical councils, working from outside both confessional camps, attest that subordinationist christologies were genuinely present and contested in the second through fourth centuries, but also that the anti-subordinationist consensus that emerged was not merely institutional self-interest — it addressed textual and liturgical pressures (worship of Christ, baptismal formulas) that the unitarian reading has to independently account for.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, contested).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high 0.58 because, viewed from the standing arrangement under contest (the historical and contemporary institutional order that has classified this reading as heresy), the unitarian reading functions as a withdrawal of resources, legitimacy, and disciplinary reach from that institutional order wherever it takes hold — a real transfer, not merely an opinion difference. Suppression is authored high (0.72) because for most of the interval the dominant response to this reading was not argument but coercion: condemnation at councils, civil penalties under Christian empire, and exclusion from communion. Theater ratio is moderate (0.28) reflecting that some of the ongoing creedal reaffirmation and heresiological vocabulary function performatively (reasserting boundaries against a threat long since institutionally marginalized) rather than addressing live theological argument. Accessibility collapse is moderate (0.45): trinitarian orthodoxy did not eliminate the unitarian reading's textual basis, and it persists as a minority position, so alternatives did not fully collapse the way they would under a genuine mountain. Resistance is high (0.68) — the reading has been actively and continuously resisted by mainstream ecclesial bodies for over a millennium and a half.
 *
 * PERSPECTIVAL GAP:
 *   From the unitarian congregation's seat, this is coordination: a return to textually simple, non-philosophically-burdened monotheism that lets the community organize worship without deferring to external authority. From the institutional hierarchy's seat, the same reading is a structural threat requiring active doctrinal defense — every anti-subordinationist creedal clause is enforcement machinery built specifically against this reading's persistence. The engine should compute divergent per-seat classifications from these structurally different positions rather than resolving them to one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian congregations and biblicist lay readers are declared beneficiaries: the reading grants them direct interpretive authority and frees them from deference to conciliar and magisterial rulings they regard as extraneous. Institutional ecclesial hierarchy and the credal orthodoxy apparatus are declared victims/payers: their constitutive authority is substantially built on excluding exactly this reading, so its persistence or spread is a structural cost to them, not merely a disagreement. Trinitarian clergy are excluded rather than victimized directly — they are not named as bearing the cost in the same structural sense as the institutional apparatus, but their voice is absent from the unitarian reading's internal deliberation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than snare matters here: the unitarian reading is not pure extraction dressed as coordination — it does solve a genuine coordination problem (reconciling strict monotheism with high christology in a textually simpler way) for the communities that hold it, and those communities are net beneficiaries of holding it, not merely victims of a con. But it also imposes real costs on institutional structures whose authority depends on its exclusion, and its persistence in contested spaces has historically required — and still requires, in communities where it is a minority view — active enforcement of boundaries in both directions. Classifying it purely as extraction would erase the real coordination function it performs for its adherents; classifying it purely as rope would erase the genuine, structurally asymmetric cost it imposes on ecclesial hierarchies whose disciplinary categories exist specifically to name and exclude it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordinationism_original_or_deviant,
    'Was subordinationist christology the earlier, more original position from which trinitarian orthodoxy departed, or was it itself a later rationalizing simplification that departed from an original high-christology consensus?',
    'Detailed philological and historical-critical work on pre-Nicene texts (Ignatius, Justin Martyr, Origen, Arius''s own writings) tracing continuity or discontinuity with apostolic-era liturgical and confessional practice, particularly baptismal formulas and worship-of-Christ evidence.',
    'If subordinationism is the earlier position, the unitarian reading''s self-understanding as a restoration is strengthened and the institutional apparatus''s claim to represent unbroken apostolic teaching weakens correspondingly. If trinitarian high-christology is earlier and subordinationism is a later simplification, the unitarian reading''s extraction from institutional legitimacy is harder to justify on originalist grounds and rests more on textual-simplicity preference than historical priority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinationism_original_or_deviant, empirical, 'Whether subordinationism or proto-trinitarianism is the historically prior Christian position.').

omega_variable(
    kernel_disagreement_location,
    'Where precisely does the disagreement with the sibling readings (trinitarian, modalist) live: is it a dispute about numerical identity claims (is the Father the only entity that is fully God), about the ontological status of the Son and Spirit (subordinate/created vs. co-equal vs. modal), or about the proper hermeneutical weight given to specific textual clusters (Johannine high christology vs. subordinationist sayings in the Synoptics)?',
    'Systematic comparison of each reading''s exegesis of the same contested passages (John 1:1, John 14:28, Philippians 2:6-11, 1 Corinthians 15:28) to isolate whether the divergence is fundamentally textual-hermeneutical or philosophical-metaphysical.',
    'If the disagreement is primarily hermeneutical, the readings are more nearly incommensurable interpretive traditions reading the same texts differently, supporting a coexists_with relation to both siblings. If it is primarily metaphysical (a dispute about whether numerical identity or shared essence is the correct monotheism-preserving move), the unitarian reading''s core premise more directly forecloses the trinitarian reading''s core premise within any single coherent framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Whether the kernel disagreement is located in textual hermeneutics or in metaphysical commitment.').

omega_variable(
    institutional_cost_measurement,
    'How much of the measured extraction against institutional hierarchy and credal orthodoxy is a genuine structural cost (loss of adherents, funding, disciplinary reach) versus a rhetorical framing by the unitarian tradition of ordinary theological disagreement as institutional self-interest?',
    'Comparative study of denominational membership, funding, and disciplinary case data in regions and periods where unitarian movements gained meaningful traction (e.g., early modern Poland-Lithuania, Transylvania, later Unitarian churches in England and America) against control regions without such movements.',
    'If the institutional cost is measurable and substantial, the tangled_rope classification (genuine coordination for adherents plus genuine asymmetric cost to institutions) is well-supported. If the cost is mostly rhetorical framing with little measurable institutional effect, the extraction score may be overstated and the reading closer to a rope with contested legitimacy rather than a tangled_rope with real transfer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_cost_measurement, empirical, 'Whether the institutional cost attributed to this reading is structurally real or rhetorically inflated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__unitarian_reading, theater_ratio, 325, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t325, observed).
narrative_ontology:measurement(bibl_tr_t400, biblical_divine_nature__unitarian_reading, theater_ratio, 400, 0.35).
narrative_ontology:measurement_basis(bibl_tr_t400, observed).
narrative_ontology:measurement(bibl_tr_t800, biblical_divine_nature__unitarian_reading, theater_ratio, 800, 0.25).
narrative_ontology:measurement_basis(bibl_tr_t800, observed).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__unitarian_reading, theater_ratio, 1500, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t1500, observed).
narrative_ontology:measurement(bibl_tr_t1900, biblical_divine_nature__unitarian_reading, theater_ratio, 1900, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__unitarian_reading, base_extractiveness, 325, 0.45).
narrative_ontology:measurement_basis(bibl_be_t325, observed).
narrative_ontology:measurement(bibl_be_t400, biblical_divine_nature__unitarian_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement_basis(bibl_be_t400, observed).
narrative_ontology:measurement(bibl_be_t800, biblical_divine_nature__unitarian_reading, base_extractiveness, 800, 0.5).
narrative_ontology:measurement_basis(bibl_be_t800, observed).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__unitarian_reading, base_extractiveness, 1500, 0.52).
narrative_ontology:measurement_basis(bibl_be_t1500, observed).
narrative_ontology:measurement(bibl_be_t1900, biblical_divine_nature__unitarian_reading, base_extractiveness, 1900, 0.58).
narrative_ontology:measurement_basis(bibl_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__unitarian_reading, suppression_requirement, 325, 0.75).
narrative_ontology:measurement_basis(bibl_su_t325, observed).
narrative_ontology:measurement(bibl_su_t400, biblical_divine_nature__unitarian_reading, suppression_requirement, 400, 0.85).
narrative_ontology:measurement_basis(bibl_su_t400, observed).
narrative_ontology:measurement(bibl_su_t800, biblical_divine_nature__unitarian_reading, suppression_requirement, 800, 0.6).
narrative_ontology:measurement_basis(bibl_su_t800, observed).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__unitarian_reading, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement_basis(bibl_su_t1500, observed).
narrative_ontology:measurement(bibl_su_t1900, biblical_divine_nature__unitarian_reading, suppression_requirement, 1900, 0.72).
narrative_ontology:measurement_basis(bibl_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, modalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the single natural-language label 'the nature of God in Christian doctrine' into structurally distinct readings of the biblical_divine_nature kernel: unitarian_reading (this file — numerical singularity, Father alone fully God), trinitarian_reading (three co-equal hypostases sharing one ousia), and modalist_reading (sequential modes of one person). Each reading authors its own ε, beneficiary/victim structure, and classification independently; they are linked here via affects_constraints rather than merged into one story, per the ε-invariance principle. The unitarian reading is authored with a distinctly different institutional-authority profile (low institutional authority, flat ecclesiology) and a victim set (institutional hierarchy, credal orthodoxy) that does not appear symmetrically in the trinitarian reading's own file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
