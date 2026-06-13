% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Authority (Strict Orthodox Reading)
 *   domain: systematic theology / ecclesiology
 *
 * SUMMARY:
 *   The Nicene Creed (325 CE) establishes binding metaphysical doctrine on
 *   the Trinity and Christ's ontological status. Under the strict orthodox
 *   reading, adherence is non-negotiable: believers must assent to the
 *   homoousios (consubstantiality) formula and the trinitarian ontology it
 *   encodes, or face anathema, excommunication, and (in state-backed periods)
 *   exile or execution. The reading treats the creed as a metaphysical law,
 *   not a liturgical practice or confessional witness. Deviation is heresy—a
 *   structural offense against truth itself—warranting institutional
 *   sanction. This generates a clear extractor (the magisterium that
 *   determines and enforces orthodoxy), clear victims (heterodox communities
 *   and lay interpreters excluded from doctrinal power), and a coordination
 *   function (unified Christian metaphysical vocabulary enabling sacramental
 *   and doctrinal unity). The measurement series show extractiveness rising
 *   from ~0.42 at Nicaea (genuine problem-solving, mixed enforcement
 *   capacity) to ~0.68 by 1500 (extraction intensified, theater ratio high,
 *   showing the founding problem solved but the constraint persisting as
 *   power tool).
 *
 * KEY AGENTS:
 *   - hierarchical_magisterium (institutional beneficiary, sets creedal orthodoxy): patriarch, bishops, ecumenical councils, doctrinal offices
 *   - heterodox_interpreters (powerless victims, suppressed metaphysical alternatives): Arian, Monophysite, Nestorian, Pelagian, Hussites, dissenting mystics
 *   - lay_believers (beneficiaries of doctrinal clarity, victims of cognitive compulsion): bound by identity fusion to creedal adherence; exit means social death
 *   - dissenting_theologians (moderate-power victims): university and monastery-based scholars developing creed-adjacent positions
 *   - imperial_state_apparatus (institutional payer/enforcer): Byzantine, Holy Roman, Christendom states providing enforcement machinery for doctrinal conformity
 *   - ecumenical_councils (institutional legitimacy mechanism): convened bodies that ratify magisterium doctrine with conciliar authority
 *   - suppressed_regional_churches (victim communities): entire theological traditions cast out and marginalized (Nestorian, Coptic, Armenian, etc.)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.68).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.79).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Authority (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic theology / ecclesiology").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '87a937fb-a045-49c3-9bb7-93a5a3fcc02b').
narrative_ontology:cs_kernel_codification('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', fixed_text).
narrative_ontology:cs_authority_grounding('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', extraction).
narrative_ontology:cs_interpretation_layer_present('87a937fb-a045-49c3-9bb7-93a5a3fcc02b').
narrative_ontology:cs_reading_relation('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', nicene_creed_authority__liturgical_habituation_reading, forecloses).
narrative_ontology:cs_axiom('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', foundational, metaphysical_assent_mandatory).
narrative_ontology:cs_axiom_status(metaphysical_assent_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', metaphysical_assent_mandatory, deontological).
narrative_ontology:cs_axiom('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', secondary, doctrinal_uniformity_necessary).
narrative_ontology:cs_axiom_status(doctrinal_uniformity_necessary, holdable).
narrative_ontology:cs_axiom_grounding('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', doctrinal_uniformity_necessary, instrumental).
narrative_ontology:cs_reference_frame('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', apostolic_doctrine_conciliar_precision).
narrative_ontology:cs_drift_state('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', post_reformation_pluralism, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('87a937fb-a045-49c3-9bb7-93a5a3fcc02b', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_magisterium).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_believers).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, dissenting_theologians).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint's measurable function is not problem-solving (that stabilized by 451 CE) but authority consolidation and rent-collection (spiritual power, material tithes, confessional monopoly, doctrinal gatekeeping). Suppression is even higher (0.79) because the constraint's persistence depends entirely on active enforcement—excommunication, heresy trial, property seizure, exile, execution—not on participant preference or coordination benefit. Theater is moderate (0.42): the creed's doctrinal function is real and believed; but the measured intensity of enforcement against minor deviations, the retrospective anathematization of pre-council theological diversity, and the theological redundancy of the constraint after the main disputes were settled (by 451 CE) show a growing share of enforcement activity is theater—defending the rule itself, not defending against the founding problem. The measurement trajectory (extractiveness rising, theater rising, suppression rising over 1,175 years) shows the constraint shifting from problem-solution to power-tool. At t=325 (Nicaea), the constraint is genuinely coordinative (real doctrinal chaos, real need for unified vocabulary). By t=1500, the founding problem is archaeologically settled, but the magisterium enforces the creed with the same intensity, indicating the constraint has become a piton-like structure (maintained not for coordination but for institutional inertia and power preservation).
 *
 * PERSPECTIVAL GAP:
 *   The magisterium's seat (agenda-setter) perceives the constraint as eternal coordination necessity: doctrinal unity is foundational to Christian truth and social order; deviation is genuinely dangerous. The heterodox victim's seat perceives the same constraint as pure extraction: the magisterium manufactured uniformity demands, destroyed legitimate theological diversity, weaponized councils to suppress competition, and then cited the suppression as proof of the founding problem's persistence. A dissenting theologian's seat perceives a constraint on intellectual freedom: they can work within creedal bounds, but significant innovation is prohibited; exit is constrained because theological authority is territorial and jurisdictional (leaving the church means leaving Christendom). A lay believer's seat perceives identity-fusion: they have adopted the creed as part of their Christian self-concept; questioning it feels like self-destruction. The engine should compute each seat's experienced type from this structural data: the magisterium might compute as 'rope' (coordination beneficiary), heterodox interpreters as 'snare' (trapped extraction victims), dissenting theologians as 'tangled_rope' (coordinated by the creedal framework but extracted from by the orthodoxy gate), lay believers as 'scaffold' collapsed into permanent (nominally transitional but now inert—they were supposed to mature into creedal understanding but the identity-lock persists). The authored claim (tangled_rope) reflects the story-level structural truth: there IS coordination (unified metaphysical language, sacramental recognition), there ARE asymmetric extraction mechanisms (the magisterium collects authority, victims lose interpretive power), and there IS active enforcement (heresy trials, excommunication, exile). The metrics reflect the operational reality: extraction is high and rising, theater is rising (showing the founding problem solved but the constraint persisting), suppression is highest of all (showing the constraint is defended by coercion, not by participant preference).
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchical_magisterium is the primary beneficiary: they set creedal interpretation, collect spiritual authority (confessional power, doctrinal gatekeeping), collect material resources (tithes conditional on orthodoxy in state-church partnerships), and face no exit cost (they define what orthodox means). Their directionality is near full beneficiary (d ~ 0.1–0.2). Heterodox interpreters are full targets: they bear the extraction (anathema, excommunication, social death, execution), face trapped exit (recantation means intellectual death; geographic exit is often impossible in Christendom conditions), and have no power to change the constraint (they are subject to it, not designers of it). Their directionality is near full target (d ~ 0.85–1.0). Lay believers sit near symmetric (d ~ 0.5–0.6): they receive genuine coordination benefit (clear doctrinal instruction, sacramental unity, membership boundary), but they also bear a cost (enforced cognitive compliance, identity-lock preventing genuine exit, internalized suppression). Dissenting theologians are targets (d ~ 0.75–0.85): they are constrained by the creedal framework and extracted from by the orthodoxy gate, but retain some intellectual mobility and geographic exit options (unlike the powerless heterodox). The imperial state is a partial payer (d ~ 0.6): they benefit from unified religious ideology (suppresses sectarian challenge to legitimacy) but pay the cost of maintaining enforcement machinery (military, police, inquisitors); they can exit by withdrawing enforcement, so they are more mobile than the heterodox interpreters but more constrained than the magisterium.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy: the founding problem (doctrinal chaos in the 4th century) was substantially solved by the 5th century (Chalcedon, 451 CE). The three ecumenical councils (Nicaea 325, Constantinople 381, Ephesus 431, Chalcedon 451) settled the main metaphysical disputes: homoousios was accepted, the trinitarian person/nature distinction was formalized, Christology was defined. By 1500, a thousand years later, these disputes were archaeologically dead. Yet the constraint persisted with undiminished or intensified enforcement (heresy trials intensified in the medieval period, the Inquisition formalized heresy prosecution, Hussites were burned, mystics were suspected). The theater_ratio rising (0.15 to 0.42) shows the constraint increasingly defending itself rather than defending against the founding problem. The six_questions.founding_problem_status='contested' reflects this: the magisterium claims the problem is eternally live (doctrine always under threat of deviation), while historians and heterodox communities claim the problem was solved and the constraint now serves institutional inertia. A mandatrophy resolution would require the magisterium to either (1) redefine the founding problem to something the constraint actually addresses in 1500 (e.g., 'maintaining institutional authority against lay interpretation'), or (2) acknowledge the founding problem is solved and relax enforcement to coordinate on creedal basics while tolerating theological diversity within the creedal bounds. Neither resolution has occurred in the interval; the constraint persists by institutional inertia and power consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is doctrinal deviation a genuinely persistent problem requiring continuous enforcement, or was it substantially solved by the 5th century (and the constraint now persists by institutional inertia)?',
    'Historical analysis of heresy frequency, enforcement intensity, and doctrinal novelty across the interval. Compare pre-Nicene theological diversity to post-Chalcedon diversity to post-Reformation diversity. Does the constraint solve a recurring problem or does it increasingly defend its own rules?',
    'If the problem was solved, the constraint transitions from tangled_rope (genuine coordination + extraction) to piton (inertial institutional structure). Mandatrophy is confirmed. If the problem is genuinely persistent, the high extraction is the price of solving it and the constraint remains legitimately tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem persists or has been substantially resolved.').

omega_variable(
    metaphysical_assent_requirement,
    'Is metaphysical assent to the creedal ontology structurally necessary for Christian community, or can Christians participate fully in the church while holding metaphysically divergent frameworks (compatible with Scripture and reason)?',
    'Theological analysis: can a Nestorian or Monophysite Christian be a valid Christian? Does the magisterium''s answer rest on doctrinal necessity or on institutional authority? Natural experiments from periods where heterodox and orthodox communities coexist without active state enforcement: do sacramental and doctrinal unity collapse, or do they persist?',
    'If metaphysical assent is genuinely structurally necessary, the extraction is coordination cost. If it is not structurally necessary but is enforced as institutional requirement, the constraint is pure extraction riding on a coordination frame. The measurement series rising theater_ratio suggests the latter; an omega documents this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_assent_requirement, conceptual, 'Whether metaphysical assent is structurally necessary or institutionally imposed.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.79) structural (external barriers: excommunication, exile, execution) or internalized (lay believers and dissenting theologians have absorbed the rule that deviation is dangerous)?',
    'Post-suppression trajectory: when lay believers or theological students are removed from the constraint''s enforcement context (leave the church, study outside church authority), does suppression persist? Do they continue to experience deviation as dangerous, or do they adopt divergent metaphysics freely? Autobiographical and confessional evidence from reformed heretics and lapsed believers.',
    'If suppression is primarily structural, removal of enforcement machinery would enable rapid divergence. If suppression is internalized, the constraint''s persistence would outlive enforcement-institution collapse (as with post-Reformation Protestant internalization of creedal authority). If both: the constraint is more entrenched than structural suppression alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the strict_orthodox_reading''s core thesis (metaphysical assent is mandatory; deviation is heresy) logically foreclose the symbolic_confessional_reading (the creed is historically contingent witness) or the liturgical_habituation_reading (the creed functions as identity boundary independent of metaphysical assent)?',
    'Analytic philosophy of religion: can a party hold both ''metaphysical assent is mandatory'' and ''assent is not mandatory, only confessional solidarity'' in the same framework? Or do they logically exclude each other? Which relation is correct: forecloses, coexists_with, or influences?',
    'If forecloses: the strict_orthodox_reading is a competing truth-claim that rules out the others. The engine should compute foreclosure as the canonical relation. If coexists_with: the three readings are live positions held by different factions; the constraint is a site of contestation, not a settled fact. If influences: the strict_orthodox_reading shapes conditions under which the other readings operate (e.g., the symbolic reading must accept the creed''s authority in form even if not in metaphysical substance).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical and structural relationship between the strict_orthodox_reading and its sibling readings.').

omega_variable(
    heterodox_community_exit,
    'Why did heterodox Christian communities (Nestorian, Coptic, Armenian, etc.) survive the anathematization if the suppression and extraction were as high as authored? What allowed them to persist?',
    'Geopolitical and institutional analysis: Did geographic distance (Persian, Arab, Byzantine borders) provide sanctuary? Did state enforcement collapse in those regions? Did the magisterium tolerate heterodoxy when it lacked enforcement capacity? Did heterodox communities develop internal institutional structures that recreated the magisterium''s authority at smaller scale?',
    'If geographic/geopolitical exit was available, the authored exit_options=''trapped'' for heterodox interpreters may be overstated for those communities (they may have had ''constrained'' or ''mobile'' exit). If the magisterium could not enforce universally, the scope and effective extraction may be lower than authored. The suppression and resistance measurements may need adjustment to reflect regional variation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heterodox_community_exit, empirical, 'Mechanisms enabling heterodox community survival despite high suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 325, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 325, 0.15).
narrative_ontology:measurement(nice_tr_t381, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 381, 0.2).
narrative_ontology:measurement(nice_tr_t451, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 451, 0.25).
narrative_ontology:measurement(nice_tr_t800, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 800, 0.35).
narrative_ontology:measurement(nice_tr_t1200, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1200, 0.4).
narrative_ontology:measurement(nice_tr_t1500, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1500, 0.42).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 325, 0.42).
narrative_ontology:measurement(nice_be_t381, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 381, 0.48).
narrative_ontology:measurement(nice_be_t451, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 451, 0.54).
narrative_ontology:measurement(nice_be_t800, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 800, 0.62).
narrative_ontology:measurement(nice_be_t1200, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1200, 0.66).
narrative_ontology:measurement(nice_be_t1500, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1500, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 325, 0.55).
narrative_ontology:measurement(nice_su_t381, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 381, 0.62).
narrative_ontology:measurement(nice_su_t451, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 451, 0.68).
narrative_ontology:measurement(nice_su_t800, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 800, 0.74).
narrative_ontology:measurement(nice_su_t1200, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1200, 0.77).
narrative_ontology:measurement(nice_su_t1500, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1500, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__strict_orthodox_reading, 0.14).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, heresy_prosecution_canon_law).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, clerical_celibacy_enforcement).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, sacramental_validity_doctrine).

% DUAL FORMULATION NOTE:
% The Nicene Creed authority kernel decomposes into three readings: strict_orthodox_reading (this constraint, ε~0.68, high extraction), symbolic_confessional_reading (ε~0.35, moderate extraction, higher theological flexibility), and liturgical_habituation_reading (ε~0.15, low extraction, pure identity coordination). The readings differ structurally in their epistemic grounding (assent-based vs. tradition-based vs. performance-based) and their enforceability (mandatory metaphysics vs. confessional solidarity vs. liturgical participation). The strict_orthodox_reading is the upstream constraint in institutional causation: it justifies the enforcement mechanisms (heresy prosecution, excommunication) that the other readings must operate within or against. Each reading instantiates one constraint with one ε; they are not angles on a single constraint but three distinct constraints grounded in the same kernel and competing for institutional hegemony.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_creed_authority__strict_orthodox_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
