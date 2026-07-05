% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__composite_overdetermination_reading, []).

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
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: The Reformation as Composite Overdetermined Event (Theological/Institutional/Political/Denominational)
 *   domain: historical/religious/political epistemology
 *
 * SUMMARY:
 *   This story instantiates the composite-overdetermination reading of the
 *   Reformation kernel: the claim that theological innovation, institutional
 *   collapse, political realignment, and denominational proliferation
 *   occurred simultaneously and irreducibly, such that no single causal
 *   driver or periodization scheme captures the phenomenon. This is one of
 *   three sibling readings of the same historical kernel
 *   (reformation_event_boundary); the theological_climb_reading treats
 *   Luther's doctrinal breakthrough as primary with institutional separation
 *   as necessary consequence, and the political_swap_reading treats theology
 *   as post-hoc rationalization for princely asset seizure. This story does
 *   NOT adjudicate between them — it generates only the composite reading as
 *   its own ε-invariant constraint, with its own beneficiary/victim structure
 *   and its own classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.51).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "The Reformation as Composite Overdetermined Event (Theological/Institutional/Political/Denominational)").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical/religious/political epistemology").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, 'f55cc682-13a9-4bdc-af0e-0e1801f73d0d').
narrative_ontology:cs_kernel_codification('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', distributed).
narrative_ontology:cs_authority_grounding('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', distributed).
narrative_ontology:cs_reading_relation('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', foundational, causal_irreducibility_of_simultaneous_strands).
narrative_ontology:cs_axiom_status(causal_irreducibility_of_simultaneous_strands, holdable).
narrative_ontology:cs_axiom_grounding('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', causal_irreducibility_of_simultaneous_strands, empirically_contingent).
narrative_ontology:cs_axiom('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', secondary, periodization_plurality_is_structural_not_deficiency).
narrative_ontology:cs_axiom_status(periodization_plurality_is_structural_not_deficiency, holdable).
narrative_ontology:cs_axiom_grounding('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', periodization_plurality_is_structural_not_deficiency, conventional).
narrative_ontology:cs_reference_frame('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', single_causal_driver_historiography).
narrative_ontology:cs_drift_state('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', post_confessionalization_thesis_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f55cc682-13a9-4bdc-af0e-0e1801f73d0d', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, territorial_princes_and_magistrates).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, vernacular_print_industry).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, reformed_clergy_class).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, professional_historians_of_composite_school).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, peasant_reform_movements).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, religious_minorities_under_new_confessional_states).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, catholic_populations_in_contested_territories).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, monastic_and_ecclesiastical_property_holders).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, multicausal_historiography_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, irreducible_overdetermination_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rulers who used the theological rupture as cover and occasion to seize ecclesiastical land, revenue, and jurisdiction from Rome and from local bishoprics. They administer the resulting confessional settlements (cuius regio, eius religio) and set the boundary of what counts as legitimate doctrine within their territory. Their exit from the old order was low-cost and highly profitable; they could not be forced back into papal obedience once assets were redistributed.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, territorial_princes_and_magistrates, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, territorial_princes_and_magistrates, agenda_setter).

% Printers and pamphleteers who profited from mass-produced vernacular Bibles, polemical tracts, and catechisms. The proliferation of denominations created a permanently expanding market for confession-specific print. They could relocate between cities depending on which magistrate offered the most favorable printing privileges.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, vernacular_print_industry, beneficiary,
    organized, biographical, mobile, continental).

% Former priests, monks, and university theologians who reconstituted themselves as pastors within new confessional churches, gaining marriage rights, salaried livings under princely patronage, and doctrinal authority within the new institutional order. Their professional survival depended on the new settlements holding.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, reformed_clergy_class, beneficiary,
    organized, generational, constrained, regional).

% Academic historians whose careers, journal space, and disciplinary authority rest on treating the Reformation as irreducibly multicausal — a stance that generates permanent scholarly work (no single-driver account can be declared final) and forecloses simpler periodizations that would settle the field. They administer the interpretive frame under which the event is taught and cited.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, professional_historians_of_composite_school, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, professional_historians_of_composite_school, observer).

% Groups (e.g. the 1525 Peasants' War participants) who took reformers' theological language of Christian liberty literally and demanded material redistribution of land and abolition of serfdom. Their movement was violently suppressed by the same princes and, notably, by Luther himself, who denounced them to preserve princely alliance. The composite reading absorbs their suppression as an unremarkable sub-event of political realignment rather than foregrounding it.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, peasant_reform_movements, payer,
    powerless, biographical, trapped, regional).

% Anabaptists, spiritualists, and other radical sects who did not fit either the Catholic or the magisterial Protestant settlement. Persecuted by both Catholic and Protestant territorial authorities alike, they bore the cost of the denominational-proliferation dimension of the event without gaining a secured confessional state of their own.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, religious_minorities_under_new_confessional_states, payer,
    powerless, biographical, trapped, regional).

% Populations in territories where a prince adopted Protestantism who did not personally convert; subject to displacement, forced conformity, or emigration as territorial religious settlements hardened. Their loss registers differently depending on which sub-event (political, theological) a given historian foregrounds.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, catholic_populations_in_contested_territories, payer,
    powerless, generational, trapped, regional).

% Monasteries, convents, and cathedral chapters whose land and endowments were dissolved and transferred to secular authorities under cover of doctrinal reform. They had no meaningful legal recourse once territorial rulers declared the transfer complete.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, monastic_and_ecclesiastical_property_holders, payer,
    moderate, biographical, trapped, regional).

% The Roman institutional authority whose account of events (illegitimate schism driven by heresy and princely greed) is structurally sidelined by the composite-overdetermination frame, which treats the schism as a symmetric multicausal process rather than an unlawful rupture. Rome's own periodization (the moment of excommunication/heresy as decisive break) is not what the composite reading tracks as the completion point.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, papal_curia_and_roman_hierarchy, excluded,
    institutional, civilizational, constrained, continental).

% Generations born into the resulting confessional states (and the wars of religion that followed) had no voice in how the founding event would be periodized or explained; the composite reading's diffusion of causal responsibility across theology/institution/politics/proliferation makes no single actor accountable for the downstream violence they inherited.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, future_confessional_state_subjects, excluded,
    powerless, civilizational, trapped, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The composite-overdetermination reading coordinates historical explanation across disciplinary specialists (theologians, institutional historians, political historians, social historians) by refusing to award causal priority to any single sub-event, allowing each specialist community's evidence to count as necessary rather than as background noise to a master narrative.
% TRANSFER_FUNCTION: Moves interpretive authority and disciplinary resources toward historians and pedagogical frameworks that treat multicausality as the correct final answer, and moves accountability for reform-era violence away from any single identifiable driver (theological zealotry, princely opportunism) by distributing it across four irreducible causal strands.
% ABSENT_VOICES: The papal hierarchy's own periodization (excommunication as decisive illegitimate break) and the peasant movements' own account (Reformation as betrayed promise of Christian liberty) are both structurally absorbed as sub-strands rather than treated as competing completion points; neither seat gets to say when 'the Reformation' actually ended on its own terms.
% DISAPPEARANCE_RATIONALE: If the composite-overdetermination frame vanished, the underlying events (institutional dissolutions, confessional states, denominational churches) would not rearrange themselves — those are settled historical facts. What would change is the disciplinary consensus and pedagogy built atop the frame: single-driver readings (theological_climb_reading, political_swap_reading) would compete for default status, reshaping how responsibility for reform-era violence is narrated and which victim sets are foregrounded. Historians disagree on how much rides on the frame versus the facts it interprets.
% FOUNDING_PROBLEM: The composite frame was built to resolve a genuine historiographical problem: single-driver accounts of the Reformation (pure theology, pure politics) could not survive sustained archival research showing simultaneous, mutually entangled theological, institutional, economic, and political processes across dozens of independent territories.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the composite-reading's own beneficiary set by social and economic historians (e.g. scholarship on peasant movements and confessionalization studies) who independently document multi-strand causation without necessarily endorsing the composite frame's disciplinary payoff; also corroborated adversarially by proponents of the rival single-driver readings, who accept that multiple processes occurred simultaneously but dispute that they are irreducible rather than ranked by causal weight.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, contested).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.32 to ~0.58) as the initial doctrinal dispute (1517) hardens into territorial settlements, property transfers, and confessional state apparatus by the Peace of Augsburg (1555) and Westphalia (1648) — the composite frame's own extraction is not in the theology but in the disciplinary and territorial machinery built to sustain multicausal explanation as the permanent, unresolvable-by-design answer. Suppression tracks the actual historical violence (Peasants' War 1525, confessional wars) and rises correspondingly; theater_ratio is moderate-low because the historiographical apparatus (academic multicausal consensus) is a genuine analytical achievement, not pure performance, though its stability as 'the' answer has some performative defense built in (interpretive gatekeeping against simpler readings).
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (princes, clergy, historians) the composite reading looks like intellectual honesty — a coordination function solving a real historiographical problem that single-driver accounts cannot handle. From the payer seats (peasants, minorities, dispossessed monastics) the same frame looks like an extraction mechanism: it diffuses moral and causal responsibility for their suffering across four 'irreducible' strands, none of which is ever weighted heavily enough to demand redress or even a clear villain. The engine computes this divergence from the declared structural data; the story does not resolve which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial princes, the print industry, and the reformed clergy sit near the beneficiary end: they gained land, markets, and institutional office respectively, and the composite frame's refusal to assign primary blame to any one of them diffuses accountability. Professional historians of the composite school are a second-order beneficiary: the frame itself is their intellectual property, generating permanent disciplinary work. Peasant movements, religious minorities, contested-territory Catholics, and dispossessed ecclesiastical property holders sit near the target end: trapped exit options, bearing the material costs of a rupture whose causal responsibility the composite frame distributes so widely that no single actor is ever fully answerable for their specific loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite frame's founding problem (single-driver accounts fail against archival evidence) remains genuinely live — this is not a pure zombie mandate. But the frame has also become self-perpetuating disciplinary infrastructure: because overdetermination is unfalsifiable by design (any counter-evidence for one strand is absorbed as confirming multicausality rather than as evidence against the frame), it risks converting a defensible epistemic caution into a permanent excuse against accountability-assigning historical judgment. The tangled_rope classification captures this: genuine coordination function (reconciling multiple evidence bases) coexists with asymmetric extraction (diffusing responsibility away from identifiable beneficiaries, at cost to identifiable victims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreducibility_versus_convenient_agnosticism,
    'Is the claim that the four Reformation strands are genuinely causally irreducible a defensible epistemic conclusion from the evidence, or a convenient agnosticism that protects the historical discipline from having to assign primary responsibility (and therefore primary blame) to any single set of beneficiaries?',
    'Counterfactual and comparative historiographical analysis: examine whether other composite events in different periods/regions that lack the same disciplinary incentive structure are analyzed with similarly total resistance to causal ranking, or whether ranking is more common absent career incentives favoring irreducibility.',
    'If irreducibility is a genuine epistemic finding, the composite reading is closer to a Rope (real coordination of divergent evidence bases with minimal extraction). If it is a convenient agnosticism protecting beneficiaries from accountability, the tangled_rope classification understates the extraction and a snare-leaning read becomes defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreducibility_versus_convenient_agnosticism, conceptual, 'Whether structural overdetermination is a real historical property or a disciplinary cover story.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does adopting the composite-overdetermination reading logically foreclose the theological_climb_reading or the political_swap_reading, or can all three coexist as live historiographical positions?',
    'Examine whether historians who hold the composite view treat the single-driver readings as false (foreclosed) or as partial-truth subsets absorbed into the composite (coexisting, non-foreclosing).',
    'If foreclosing, only one reading can be structurally correct and the kernel''s contest is a genuine either/or; if coexisting, the kernel supports plural simultaneous framings, consistent with how the field''s leading scholars actually treat the dispute (as emphasis, not exclusive truth).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether the composite reading logically excludes its single-driver siblings or merely subsumes them as partial views.').

omega_variable(
    periodization_completion_point_ambiguity,
    'Because the composite reading tracks multiple parallel completion points (doctrinal settlement, institutional dissolution, territorial realignment, denominational stabilization), which of these — if any — should count as ''the'' end of the Reformation for classification purposes, and does the interval boundary chosen here (1517-1648) bias the extraction trajectory?',
    'Re-run the temporal measurement series against alternative endpoints (e.g. 1555 Augsburg vs. 1648 Westphalia) and test whether the extractiveness trajectory''s shape is robust to the choice.',
    'If the trajectory is highly sensitive to the chosen endpoint, the composite reading''s periodization claim (that no single scheme captures the event) is self-validating in a potentially unfalsifiable way; if robust, it strengthens the reading''s descriptive claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_completion_point_ambiguity, empirical, 'Sensitivity of the composite reading''s temporal trajectory to the choice of periodization endpoint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement_basis(refo_tr_t1517, observed).
narrative_ontology:measurement(refo_tr_t1540, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1540, 0.16).
narrative_ontology:measurement_basis(refo_tr_t1540, observed).
narrative_ontology:measurement(refo_tr_t1560, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1560, 0.2).
narrative_ontology:measurement_basis(refo_tr_t1560, observed).
narrative_ontology:measurement(refo_tr_t1580, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1580, 0.24).
narrative_ontology:measurement_basis(refo_tr_t1580, observed).
narrative_ontology:measurement(refo_tr_t1618, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1618, 0.27).
narrative_ontology:measurement_basis(refo_tr_t1618, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1648, 0.28).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1517, 0.32).
narrative_ontology:measurement_basis(refo_be_t1517, observed).
narrative_ontology:measurement(refo_be_t1540, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1540, 0.46).
narrative_ontology:measurement_basis(refo_be_t1540, observed).
narrative_ontology:measurement(refo_be_t1560, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1560, 0.53).
narrative_ontology:measurement_basis(refo_be_t1560, observed).
narrative_ontology:measurement(refo_be_t1580, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1580, 0.55).
narrative_ontology:measurement_basis(refo_be_t1580, observed).
narrative_ontology:measurement(refo_be_t1618, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1618, 0.6).
narrative_ontology:measurement_basis(refo_be_t1618, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1648, 0.58).
narrative_ontology:measurement_basis(refo_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement_basis(refo_su_t1517, observed).
narrative_ontology:measurement(refo_su_t1540, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1540, 0.42).
narrative_ontology:measurement_basis(refo_su_t1540, observed).
narrative_ontology:measurement(refo_su_t1560, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1560, 0.5).
narrative_ontology:measurement_basis(refo_su_t1560, observed).
narrative_ontology:measurement(refo_su_t1580, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1580, 0.47).
narrative_ontology:measurement_basis(refo_su_t1580, observed).
narrative_ontology:measurement(refo_su_t1618, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1618, 0.55).
narrative_ontology:measurement_basis(refo_su_t1618, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1648, 0.51).
narrative_ontology:measurement_basis(refo_su_t1648, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__composite_overdetermination_reading, 0.1).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, political_swap_reading).

% DUAL FORMULATION NOTE:
% This story is the composite_overdetermination_reading member of a three-story kernel family on reformation_event_boundary. theological_climb_reading foregrounds Luther's doctrinal breakthrough as primary driver with institutional separation as necessary consequence; political_swap_reading foregrounds princely asset seizure with theology as post-hoc rationalization; this reading treats all identified strands (theological, institutional, political, denominational) as simultaneously necessary and irreducible. The three readings share overlapping stakeholder populations (princes, clergy, peasants, minorities) but assign different victim/beneficiary weightings and different periodization completion points. Each story's network.affects_constraints links to both siblings to preserve the kernel-family structure required by the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
