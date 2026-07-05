% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: The Reformation as Theological Climb: Justification by Faith Alone as Doctrinal Breakthrough Requiring Institutional Separation
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This story instantiates the theological_climb_reading of the
 *   reformation_event_boundary kernel: the Reformation as primarily a
 *   doctrinal correction (justification by faith alone) whose institutional
 *   separation from Rome follows necessarily from the theology, not from
 *   prior political motive. Under this reading the Catholic hierarchy is the
 *   payer of a true correction rather than the victim of expropriation,
 *   believers freed from the indulgence economy are the beneficiaries, and
 *   the event is periodized tightly (1517 Ninety-Five Theses to 1555 Peace of
 *   Augsburg) around the doctrinal dispute and its institutional resolution.
 *   This is a deliberately narrow, single reading — the
 *   political_swap_reading and composite_overdetermination_reading are
 *   separate constraints with their own ε, beneficiary/victim structures, and
 *   periodizations, linked via network.affects_constraints, not folded into
 *   this file's classification.
 *
 * KEY AGENTS:
 *   - martin_luther_and_reforming_theologians: doctrinal author, forced institutional separation, identity-locked exit
 *   - territorial_princes_adopting_reform: political beneficiaries downstream of the doctrinal cause on this reading
 *   - roman_catholic_church_hierarchy: payer of the correction, institutional power, constrained exit
 *   - religious_minorities_in_confessionalized_territories: bear enforcement costs of the new territorial settlement
 *   - peasant_reformers_suppressed_after_1525: bear suppression when radical readings of the doctrine are disavowed
 *   - counter_reformation_catholic_theologians: excluded rival theological voice
 *   - ecclesiastical_historians_of_doctrine: analytical observers assessing rediscovery-vs-novelty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.28).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.62).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "The Reformation as Theological Climb: Justification by Faith Alone as Doctrinal Breakthrough Requiring Institutional Separation").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '4399f3e2-bc02-4690-b121-7e072a4005ec').
narrative_ontology:cs_kernel_codification('4399f3e2-bc02-4690-b121-7e072a4005ec', fixed_text).
narrative_ontology:cs_authority_grounding('4399f3e2-bc02-4690-b121-7e072a4005ec', lineage).
narrative_ontology:cs_interpretation_layer_present('4399f3e2-bc02-4690-b121-7e072a4005ec').
narrative_ontology:cs_reading_relation('4399f3e2-bc02-4690-b121-7e072a4005ec', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('4399f3e2-bc02-4690-b121-7e072a4005ec', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('4399f3e2-bc02-4690-b121-7e072a4005ec', foundational, doctrine_is_prior_and_causally_sufficient).
narrative_ontology:cs_axiom_status(doctrine_is_prior_and_causally_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('4399f3e2-bc02-4690-b121-7e072a4005ec', doctrine_is_prior_and_causally_sufficient, deontological).
narrative_ontology:cs_axiom('4399f3e2-bc02-4690-b121-7e072a4005ec', foundational, sola_fide_is_genuine_scriptural_rediscovery).
narrative_ontology:cs_axiom_status(sola_fide_is_genuine_scriptural_rediscovery, holdable).
narrative_ontology:cs_axiom_grounding('4399f3e2-bc02-4690-b121-7e072a4005ec', sola_fide_is_genuine_scriptural_rediscovery, conventional).
narrative_ontology:cs_reference_frame('4399f3e2-bc02-4690-b121-7e072a4005ec', apostolic_pauline_soteriology).
narrative_ontology:cs_drift_state('4399f3e2-bc02-4690-b121-7e072a4005ec', post_indulgence_crisis_1517, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4399f3e2-bc02-4690-b121-7e072a4005ec', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, believers_freed_from_indulgence_doctrine).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_clergy_and_theologians).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, territorial_princes_adopting_reform).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, roman_catholic_church_hierarchy).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, religious_minorities_in_confessionalized_territories).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, peasant_reformers_suppressed_after_1525).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, sola_fide_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, scripture_as_sole_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Luther and allied theologians (Melanchthon, later Calvin) articulate and defend justification by faith alone as a rediscovered scriptural truth, publish it, and refuse recantation despite excommunication threat. Their exit from the Catholic communion is not chosen strategically but forced by the doctrine itself — having concluded the doctrine is true, remaining under Rome's authority becomes theologically impossible for them. They administer the new confessional apparatus (catechisms, ordination, church orders) that the doctrinal break requires.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, martin_luther_and_reforming_theologians, agenda_setter,
    moderate, biographical, identity_locked, regional).

% Lay believers who, on this reading, are relieved of the burden of the indulgence system and works-based anxiety about salvation once justification by faith alone is preached in their territory. Their exit options remain constrained by territorial religious settlement (they inherit whichever confession their prince adopts), but within the theological climb reading they are the intended beneficiaries of the doctrinal correction itself.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, believers_freed_from_indulgence_doctrine, beneficiary,
    powerless, generational, constrained, regional).

% German princes and other territorial rulers who adopt the reformed confession, gaining ecclesiastical authority and property that had flowed to Rome. On the theological-climb reading their political gain is downstream of a genuine doctrinal cause, not the cause itself; they act as secondary agenda-setters administering the institutional separation the doctrine requires, but the doctrine is treated as prior and independent of their interest.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, territorial_princes_adopting_reform, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, territorial_princes_adopting_reform, agenda_setter).

% The papacy and its hierarchy lose doctrinal authority, revenue, and territorial jurisdiction as reformed confessions institutionalize. On this reading they are cast as the party whose doctrine was in error and who bears the cost of correction — their loss is the necessary consequence of a true theological finding, not an extraction they suffer arbitrarily. Their exit option (Counter-Reformation reassertion) is real but constrained by the doctrinal ground having already shifted under them.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, roman_catholic_church_hierarchy, payer,
    institutional, civilizational, constrained, continental).

% Anabaptists, Catholics in reformed territories, and other minorities within the new territorial confessional order (cuius regio, eius religio) who do not share the prince's adopted confession. They bear exile, fine, or execution as the institutional-separation apparatus enforces doctrinal uniformity within each territory. Their situation is a cost of the institutional separation this reading treats as necessitated by the theological breakthrough.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, religious_minorities_in_confessionalized_territories, payer,
    powerless, biographical, trapped, regional).

% Peasants and radical reformers (e.g., in the German Peasants' War) who read the doctrinal break as license for social and economic reordering, and are violently suppressed — with Luther's own endorsement — once the magisterial reformers distinguish theological liberty from social revolt. On the theological climb reading this suppression is framed as protecting the doctrinal breakthrough from misapplication, but it is a real cost borne by a group excluded from the settlement's benefits.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, peasant_reformers_suppressed_after_1525, payer,
    powerless, immediate, trapped, regional).

% Trent-era Catholic theologians who would contest that justification by faith alone is a rediscovery rather than a novel and erroneous departure from apostolic tradition. Their counter-argument (that grace-and-works soteriology is the continuous, correctly transmitted doctrine, and Luther's reading is the innovation requiring correction) is treated by this reading as the losing case rather than a live rival framing.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, counter_reformation_catholic_theologians, excluded,
    institutional, civilizational, constrained, continental).

% Scholars who assess whether the doctrinal content of justification by faith alone was genuinely novel, a recovery of Pauline/Augustinian threads, or an artifact retrospectively foregrounded by confessional historiography. They can evaluate manuscript transmission, prior dissenting traditions (Hus, Wycliffe, nominalism), and the tightness of the 1517-1555 periodization this reading assumes.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, ecclesiastical_historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: On this reading, institutional separation coordinates a community of believers around a corrected soteriological doctrine, providing clear catechesis, liturgy, and church governance consistent with justification by faith alone, replacing an indulgence-and-merit system this reading holds to be doctrinally false.
% TRANSFER_FUNCTION: Moves doctrinal authority, ecclesiastical property, and confessional loyalty from the Roman hierarchy to reformed churches and their territorial sponsors; moves psychological and financial burden away from lay believers previously purchasing indulgences, while shifting coercive costs onto religious minorities and radical reformers who fall outside the new territorial settlements.
% ABSENT_VOICES: Counter-Reformation theologians who deny the 'rediscovery' framing and argue continuity of doctrine; radical/spiritualist reformers (Anabaptists, Müntzer's followers) who read the theological breakthrough as warranting social transformation and were suppressed by the same magisterial reformers who authored the doctrine; both are structurally present in the historical record but excluded from this reading's account of what the event fundamentally was.
% DISAPPEARANCE_RATIONALE: If the theological-climb framing were withdrawn — if justification by faith alone were treated as pretext rather than genuine doctrinal cause — the entire causal architecture of confessional Europe (territorial religious settlements, Protestant ecclesiology, the periodization 1517-1555 itself) loses its organizing premise; historians, theologians, and denominational institutions built on the 'genuine breakthrough' account would need to renarrate their own founding events.
% FOUNDING_PROBLEM: The founding problem, on this reading, was a genuine theological error (works-and-merit soteriology, institutionalized in the indulgence system) that Luther's exegesis of Romans and Galatians is held to have corrected; institutional separation followed because the corrected doctrine could not be practiced within a hierarchy unwilling to abandon the error.
% FOUNDING_PROBLEM_CORROBORATION: Confessional Protestant historiography and systematic theologians within the reformed traditions attest the founding problem was genuine and doctrinal. Independent corroboration from outside the beneficiary set is thin: social and political historians (see the political_swap_reading) attest the same events primarily via territorial asset transfer and princely power consolidation, and note that pre-Luther dissenting traditions (Hus, Wycliffe, via moderna nominalists) raised similar soteriological concerns for a century without producing comparable institutional rupture — suggesting doctrinal content alone does not explain the timing or scale of the separation. No fully disinterested corroborating source exists; the strongest outside check is comparative absence of rupture in earlier doctrinally similar episodes.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).
:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.28 at interval end) because on this reading the primary transfer is doctrinal correction rather than rent extraction — the Church's loss of authority is framed as the necessary cost of error correction, not predation. Suppression is authored substantially higher (0.62) because institutionalizing the corrected doctrine required real coercive apparatus: excommunication, territorial religious uniformity enforcement (cuius regio eius religio), and violent suppression of unauthorized theological conclusions (the 1525 Peasants' War spike to 0.68). Theater ratio stays low throughout (0.15 by 1555) because the confessional institutions built (catechisms, church orders, universities) perform real doctrinal transmission function on this reading, not mere performance. The suppression spike at 1525 and partial retreat by 1530 reflects the magisterial reformers' need to violently distinguish 'true' doctrinal liberty from radical social application, then stabilize into settled territorial enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From Luther's and the reforming theologians' seat, the constraint is coordination around a true and urgently needed doctrinal correction — a rope, or at most a scaffold toward a corrected church. From the suppressed peasant reformers' and religious minorities' seats, the same events computed as institutional separation deliver enforced conformity to a new orthodoxy that excludes them just as the old one did — the engine's per-seat computation should show this divergence without either seat's account being privileged by the claimed_type.
 *
 * DIRECTIONALITY LOGIC:
 *   Believers freed from indulgence doctrine and territorial princes sit toward the beneficiary end: the former gain doctrinal relief, the latter gain ecclesiastical authority as an entailment (on this reading) of the theology being true rather than as the primary motive. The Catholic hierarchy sits toward the target end — institutional, but with constrained rather than trapped exit, since Trent and the Counter-Reformation represent a real (if costly) response. Religious minorities and suppressed peasant reformers sit at the most extractive end: powerless, trapped, bearing the enforcement costs of a settlement whose benefits (freedom from doctrinal error) they do not necessarily share, since the settlement imposes a single confession per territory rather than genuine liberty of conscience.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (indulgence-driven works-soteriology, on this reading) was addressed within the interval itself (1517-1555), yet the institutional separation and its enforcement apparatus persisted long after — this is not classic mandatrophy because the doctrinal claim is treated here as durably true rather than obsolete, but the enforcement mechanism (territorial confessional uniformity) outlived its original justification (protecting a nascent, contested doctrine) and calcified into permanent territorial religious control extending well past 1555. Classifying this as tangled_rope rather than pure rope prevents mislabeling the coercive machinery built to defend the new orthodoxy as costless coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rediscovery_vs_novelty,
    'Was justification by faith alone a genuine rediscovery of scriptural/Pauline/Augustinian content obscured by medieval accretion, or a substantively novel doctrinal formulation retrospectively framed as recovery?',
    'Close comparative textual analysis of patristic and medieval soteriology (Augustine, Anselm, via moderna nominalists, Hus, Wycliffe) against Luther''s formulation, cross-checked against whether prior dissenting traditions with similar soteriological content produced comparable institutional rupture.',
    'If genuinely novel, the ''climb'' framing weakens and this reading''s claim that institutional separation was doctrinally necessitated (rather than chosen among live alternatives) is undercut; if genuine rediscovery, the reading''s core premise is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rediscovery_vs_novelty, conceptual, 'Whether the doctrinal content was rediscovery or innovation — central to whether the climb reading''s founding-problem narrative holds.').

omega_variable(
    periodization_tightness,
    'Does the 1517-1555 window (Theses to Peace of Augsburg) capture the theologically relevant boundary of the event, or does tight periodization itself presuppose the theological-climb framing by choosing endpoints defined by doctrinal/confessional milestones rather than political or social ones?',
    'Compare endpoint sensitivity: does extending the interval to include the 1524-1525 Peasants'' War''s full aftermath, or extending forward to the 1648 Peace of Westphalia, change the measured extractiveness/suppression trajectory materially?',
    'A periodization that is itself reading-dependent (as this schema''s Rule 1 acknowledges — sibling readings would choose different windows) means the tight 1517-1555 boundary is a structural commitment of this reading, not a neutral historical fact; readers should treat the interval choice as part of what is being claimed, not as background.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_tightness, conceptual, 'Whether the chosen periodization is theologically motivated rather than historically neutral.').

omega_variable(
    magisterial_radical_split_as_containment,
    'Was the magisterial reformers'' violent disavowal of radical/peasant applications of the doctrine (1525) a principled theological distinction (spiritual liberty vs. social license) or a containment strategy protecting the fragile new institutional settlement from association with social revolt?',
    'Examine Luther''s own writings against the peasants (Against the Murderous, Thieving Hordes of Peasants) for consistency with prior doctrinal statements on liberty, cross-referenced with the princely political interests the disavowal served.',
    'If containment-driven, the theological_climb_reading''s claim that institutional separation followed purely from doctrinal necessity is weakened at exactly the point where this reading treats the peasant suppression as protecting rather than betraying the doctrine''s implications — strengthening the case for the political_swap_reading at this specific juncture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_radical_split_as_containment, conceptual, 'Whether the 1525 doctrinal/social split reflects principle or political containment — a fault line between this reading and its political sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.05).
narrative_ontology:measurement(refo_tr_t1521, reformation_event_boundary__theological_climb_reading, theater_ratio, 1521, 0.08).
narrative_ontology:measurement(refo_tr_t1525, reformation_event_boundary__theological_climb_reading, theater_ratio, 1525, 0.12).
narrative_ontology:measurement(refo_tr_t1530, reformation_event_boundary__theological_climb_reading, theater_ratio, 1530, 0.14).
narrative_ontology:measurement(refo_tr_t1546, reformation_event_boundary__theological_climb_reading, theater_ratio, 1546, 0.15).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.15).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.12).
narrative_ontology:measurement(refo_be_t1521, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1521, 0.18).
narrative_ontology:measurement(refo_be_t1525, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1525, 0.3).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1530, 0.25).
narrative_ontology:measurement(refo_be_t1546, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1546, 0.27).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.2).
narrative_ontology:measurement(refo_su_t1521, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1521, 0.35).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1525, 0.68).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1530, 0.55).
narrative_ontology:measurement(refo_su_t1546, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1546, 0.6).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__theological_climb_reading, 0.1).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the reformation_event_boundary kernel. theological_climb_reading treats doctrine as prior cause and institutional separation as its necessary entailment; political_swap_reading treats theology as rationalization for princely asset seizure and papal-authority breakdown; composite_overdetermination_reading treats all causal strands (theological, institutional, political, denominational) as irreducibly simultaneous and denies any single-driver account, including this one. Each carries its own ε, beneficiary/victim structure, and periodization; none averages or hedges across the others per DP-001 ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
