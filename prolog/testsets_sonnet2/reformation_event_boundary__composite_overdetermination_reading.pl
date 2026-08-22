% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Reformation as Composite Overdetermined Event (Theological/Institutional/Political/Denominational)
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   Between roughly 1517 and 1648, theological innovation,
 *   ecclesiastical-institutional collapse, territorial political realignment,
 *   and confessional proliferation occurred as an entangled package across
 *   Central and Western Europe. This story treats the entanglement itself as
 *   the structural fact to be modeled: princes captured institutional assets,
 *   reformist clergy gained new platforms contingent on princely protection,
 *   publishers profited from proliferation regardless of doctrinal outcome,
 *   and multiple powerless populations (dissolved religious communities,
 *   minoritized confessional populations, suppressed peasant movements) paid
 *   the costs of processes that no single narrower account fully explains.
 *
 * KEY AGENTS:
 *   - territorial_princes_and_magistrates: institutional beneficiaries who captured ecclesiastical assets and set territorial confessional policy
 *   - vernacular_reformist_clergy: organized beneficiaries whose doctrinal authority depended on princely protection
 *   - emergent_print_publishers: moderate-power beneficiaries of proliferation itself, independent of which confession won
 *   - displaced_monastic_communities: powerless payers of the institutional-collapse component
 *   - confessionally_minoritized_populations: powerless payers of the political-realignment component
 *   - peasant_reform_movements_suppressed: powerless payers whose theological-economic synthesis was violently foreclosed by the same beneficiaries who profited elsewhere
 *   - papal_and_curial_authority: excluded prior authority structure
 *   - historians_of_the_reformation: analytical observers whose periodization disputes are read here as evidence of genuine structural multiplicity, not mere interpretive disagreement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.62).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation as Composite Overdetermined Event (Theological/Institutional/Political/Denominational)").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '663d25b0-23fb-400c-84cd-5485c7866ba9').
narrative_ontology:cs_kernel_codification('663d25b0-23fb-400c-84cd-5485c7866ba9', distributed).
narrative_ontology:cs_authority_grounding('663d25b0-23fb-400c-84cd-5485c7866ba9', distributed).
narrative_ontology:cs_reading_relation('663d25b0-23fb-400c-84cd-5485c7866ba9', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('663d25b0-23fb-400c-84cd-5485c7866ba9', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('663d25b0-23fb-400c-84cd-5485c7866ba9', foundational, causal_irreducibility_of_composite_strands).
narrative_ontology:cs_axiom_status(causal_irreducibility_of_composite_strands, holdable).
narrative_ontology:cs_axiom_grounding('663d25b0-23fb-400c-84cd-5485c7866ba9', causal_irreducibility_of_composite_strands, empirically_contingent).
narrative_ontology:cs_axiom('663d25b0-23fb-400c-84cd-5485c7866ba9', foundational, periodization_multiplicity_is_structural_not_defective).
narrative_ontology:cs_axiom_status(periodization_multiplicity_is_structural_not_defective, holdable).
narrative_ontology:cs_axiom_grounding('663d25b0-23fb-400c-84cd-5485c7866ba9', periodization_multiplicity_is_structural_not_defective, conventional).
narrative_ontology:cs_reference_frame('663d25b0-23fb-400c-84cd-5485c7866ba9', unified_latin_christendom_authority).
narrative_ontology:cs_drift_state('663d25b0-23fb-400c-84cd-5485c7866ba9', peace_of_westphalia_settlement, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('663d25b0-23fb-400c-84cd-5485c7866ba9', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, territorial_princes_and_magistrates).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, vernacular_reformist_clergy).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, emergent_print_publishers).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, displaced_monastic_communities).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, confessionally_minoritized_populations).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, peasant_reform_movements_suppressed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rulers who used cuius regio eius religio and analogous settlements to absorb church lands, revenue, and jurisdiction under the cover of confessional choice. They set the enforcement terms within their territories — which confession is licit, who is expelled — and captured the institutional and fiscal residue of Rome's collapse regardless of their personal theological conviction.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, territorial_princes_and_magistrates, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, territorial_princes_and_magistrates, agenda_setter).

% Preachers and theologians who articulated the doctrinal claims driving the climb component. They gained pulpits, printing platforms, and institutional roles in new church structures, but remained dependent on princely protection for survival — their doctrinal authority and their physical safety were bound together.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, vernacular_reformist_clergy, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, vernacular_reformist_clergy, agenda_setter).

% Printers and pamphleteers who profited from the explosive market in vernacular scripture, polemic, and confessional tracts. They benefited from doctrinal proliferation itself, regardless of which confession ultimately prevailed in a given territory, and could relocate to friendlier jurisdictions when local authorities cracked down.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, emergent_print_publishers, beneficiary,
    moderate, biographical, mobile, continental).

% Monks, nuns, and dependent lay populations whose houses were dissolved and whose lands were seized by territorial authorities during the institutional-collapse (drop) component. They had no meaningful exit from dissolution and often no alternative vocation or income once expelled.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, displaced_monastic_communities, payer,
    powerless, biographical, trapped, regional).

% Populations who found themselves on the losing side of a territorial confessional settlement — Catholics in newly Protestant territories or vice versa — subject to exile, forced conversion, or penal restriction as the political-realignment (swap) component hardened confessional borders.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, confessionally_minoritized_populations, payer,
    powerless, biographical, trapped, regional).

% Peasants and radical reformers who took theological arguments about spiritual equality (from the climb component) as license for social and economic demands, and were violently suppressed by the same princes and magistrates who otherwise benefited from the Reformation's institutional churn — most starkly in the German Peasants' War.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, peasant_reform_movements_suppressed, payer,
    powerless, biographical, trapped, regional).

% The prior unified ecclesiastical authority whose jurisdiction, revenue, and doctrinal monopoly the composite event dismantled. Excluded from most territorial settlements' negotiating table once princely and reformist coalitions formed; its own account of events (institutional continuity threatened by heresy and opportunism) is structurally sidelined by the sources that survive and dominate.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, papal_and_curial_authority, excluded,
    institutional, civilizational, constrained, continental).

% Scholars who periodize and causally weight the event after the fact. Their disagreement about which driver (theological, institutional, political, social) was primary is not merely interpretive noise — it tracks real structural multiplicity in what happened, and different periodization choices serve different disciplinary and confessional interests down to the present.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historians_of_the_reformation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__composite_overdetermination_reading, territorial_princes_and_magistrates).
narrative_ontology:fixing_cost_class(reformation_event_boundary__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No single coordination problem is solved by 'the Reformation' as a composite; instead, several distinct coordination problems were solved in parallel and became entangled: doctrinal disputes needed adjudication outside a discredited hierarchy, territories needed a mechanism to allocate confessional loyalty without perpetual war (eventually cuius regio eius religio), and print culture needed distribution and legitimation channels for vernacular religious material.
% TRANSFER_FUNCTION: Ecclesiastical land, revenue, and jurisdictional authority moved from the papal/monastic institutional structure to territorial princes and magistrates; doctrinal authority moved from a centralized hierarchy to a plurality of confessional structures; social and economic leverage implicit in radical reform theology was extracted from peasant movements and reabsorbed into princely control.
% ABSENT_VOICES: Papal and curial authority is structurally excluded from most surviving negotiated settlements and from the confessional historiography that followed; the peasantry's own theological-economic synthesis (Müntzer and the 1525 war) is suppressed as a live political claim and survives mostly as a cautionary episode in mainstream Reformation narratives, not as a contending reading in its own right.
% DISAPPEARANCE_RATIONALE: If the composite event had not occurred as an overdetermined package — if, counterfactually, only theological dispute had occurred without institutional collapse, or only political realignment without doctrinal innovation — the confessional map of Europe, the fiscal-territorial structure of early modern states, and the print-driven vernacular public sphere would all look substantially different. The composite's irreducibility is demonstrated by how differently the world would look if any one strand were subtracted while the others remained.
% FOUNDING_PROBLEM: No single founding problem exists because this reading denies there was one driving mechanism: it names the coincidence and mutual reinforcement of a doctrinal crisis of authority (indulgences, justification), a fiscal-jurisdictional crisis of the institutional Church, a political opportunity for territorial consolidation, and a technological/social capacity for rapid vernacular proliferation, none of which alone would have produced the historical outcome.
% FOUNDING_PROBLEM_CORROBORATION: Social and economic historians outside the confessional-history tradition (e.g., scholarship on early modern state formation and print culture, not written to vindicate either a Protestant or Catholic narrative) corroborate that the fiscal and political dimensions were operative and irreducible to theological causation alone; conversely, historians of theology corroborate that doctrinal content had causal force independent of princely opportunism. No single outside authority corroborates the composite reading as such — its evidentiary support is the persistent, unresolved disagreement of narrower specialist traditions on cause and periodization, which this reading interprets as itself the corroborating signal rather than a limitation to be explained away.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises across the interval (0.30 to a peak ~0.62 around the confessionalization era and Thirty Years' War, settling to 0.58 at Westphalia) because the composite process increasingly became a mechanism for asset transfer and territorial consolidation layered on top of genuine doctrinal coordination. Suppression rises even more sharply (0.35 to 0.75 near 1618) reflecting the hardening of confessional borders, penal laws, and ultimately the Thirty Years' War's coercive enforcement of territorial religious settlements — then declines somewhat at Westphalia (0.62) as the settlement stabilized exit options (toleration clauses, emigration provisions) for some minoritized populations. Theater ratio is moderate throughout (0.15-0.35): a substantial share of confessional conflict was genuinely doctrinal and institutional, not merely performative, but the share of purely symbolic confessional politics (court religion, dynastic marriage alignments) grew as the era progressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Princes, reformist clergy, and publishers are structural beneficiaries under this composite reading precisely because their gains were robust across which sub-event (theological, institutional, political, denominational) is foregrounded — they profited from the entanglement itself, not from any single strand. The three victim groups are victims of DIFFERENT strands (institutional collapse, political realignment, and the containment of radical theological implications respectively), which is exactly the point this reading insists on: a narrower reading would only see one of these victim populations as central, while the composite reading holds all three are equally structural.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading resists a mandatrophy verdict of either 'coordination succeeded, extraction ended' or 'pure extraction throughout' by refusing the premise that there is one function to check for obsolescence. Each strand has its own founding-problem trajectory: the doctrinal-authority crisis was in some sense resolved by confessional settlement (Westphalia), but the territorial-fiscal capture that rode alongside it did not sunset — princely control over church assets persisted long after doctrinal disputes cooled. This is why founding_problem_status is authored as contested rather than dead: some components genuinely resolved while others calcified into permanent institutional arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    composite_vs_decomposable_event,
    'Is the Reformation genuinely irreducible to a dominant causal driver, or does the appearance of overdetermination result from historians'' failure to properly weight and sequence the theological, institutional, political, and social strands?',
    'Comparative counterfactual and quantitative historical analysis: track cases where one strand was structurally absent (e.g., regions with theological ferment but weak princely opportunity, or vice versa) and observe whether the composite outcome still occurred. If outcomes vary systematically with which strand is present, decomposition may be more accurate than the composite reading claims.',
    'If a dominant driver can be empirically isolated, this composite_overdetermination_reading would be structurally superseded by whichever narrower reading (theological_climb or political_swap) the isolation supports. If no dominant driver can be isolated even with finer-grained data, the composite reading is vindicated as the more accurate structural account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_vs_decomposable_event, conceptual, 'Whether irreducible overdetermination is a real historical structure or an artifact of insufficiently disaggregated causal analysis.').

omega_variable(
    periodization_endpoint_choice,
    'Does treating 1648 (Peace of Westphalia) as the closing boundary understate or overstate the composite event''s true completion point, given that institutional/fiscal consequences (princely control of former church assets) persisted for centuries after doctrinal and political settlement?',
    'Track the fiscal and jurisdictional afterlife of confiscated ecclesiastical assets into the 18th-19th centuries to determine whether the institutional-collapse strand''s ''true'' extraction curve continues well past 1648, even as the political-realignment and theological strands close.',
    'If institutional extraction continues long past 1648, the composite reading''s interval boundary is itself a compromise that privileges the political/doctrinal strands'' completion points over the institutional strand''s — evidence for this reading''s own claim that periodization is inherently contested rather than a flaw to be fixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_endpoint_choice, empirical, 'Whether the chosen interval endpoint reflects genuine multi-strand closure or an artifact of privileging faster-resolving strands.').

omega_variable(
    beneficiary_union_versus_intersection,
    'Should the composite reading''s beneficiary/victim sets be authored as the UNION of what narrower readings would name (as done here), or should overlapping claims be weighted by how much each strand actually contributed to each group''s outcome?',
    'Archival work quantifying, for each named beneficiary/victim group, what share of their gain or loss is attributable to each of the four strands (theological, institutional, political, denominational) versus their joint/interactive effect.',
    'A union-based authoring (as done here) may overstate the composite reading''s distinctiveness by simply combining the narrower readings'' claims; a weighted/interactive approach might show genuine emergent effects not visible in either narrower reading alone, more strongly supporting irreducibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_union_versus_intersection, conceptual, 'Whether this story''s union-based beneficiary/victim authoring adequately captures emergent composite effects or merely aggregates the sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement_basis(refo_tr_t1517, observed).
narrative_ontology:measurement(refo_tr_t1530, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1530, 0.22).
narrative_ontology:measurement_basis(refo_tr_t1530, observed).
narrative_ontology:measurement(refo_tr_t1546, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1546, 0.28).
narrative_ontology:measurement_basis(refo_tr_t1546, observed).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1555, 0.32).
narrative_ontology:measurement_basis(refo_tr_t1555, observed).
narrative_ontology:measurement(refo_tr_t1618, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1618, 0.35).
narrative_ontology:measurement_basis(refo_tr_t1618, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1648, 0.3).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1517, 0.3).
narrative_ontology:measurement_basis(refo_be_t1517, observed).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1530, 0.42).
narrative_ontology:measurement_basis(refo_be_t1530, observed).
narrative_ontology:measurement(refo_be_t1546, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1546, 0.5).
narrative_ontology:measurement_basis(refo_be_t1546, observed).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1555, 0.55).
narrative_ontology:measurement_basis(refo_be_t1555, observed).
narrative_ontology:measurement(refo_be_t1618, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1618, 0.62).
narrative_ontology:measurement_basis(refo_be_t1618, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1648, 0.58).
narrative_ontology:measurement_basis(refo_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1517, 0.35).
narrative_ontology:measurement_basis(refo_su_t1517, observed).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1530, 0.5).
narrative_ontology:measurement_basis(refo_su_t1530, observed).
narrative_ontology:measurement(refo_su_t1546, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1546, 0.6).
narrative_ontology:measurement_basis(refo_su_t1546, observed).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1555, 0.65).
narrative_ontology:measurement_basis(refo_su_t1555, observed).
narrative_ontology:measurement(refo_su_t1618, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1618, 0.75).
narrative_ontology:measurement_basis(refo_su_t1618, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1648, 0.62).
narrative_ontology:measurement_basis(refo_su_t1648, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__composite_overdetermination_reading, 0.1).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, political_swap_reading).

% DUAL FORMULATION NOTE:
% This story is the composite node in a three-member kernel family under reformation_event_boundary. theological_climb_reading and political_swap_reading each isolate one causal strand as dominant; this reading holds both are true-but-partial and that the composite structure is itself the correct unit of analysis. All three share the same underlying historical referent (the 1517-1648 period) but author different ε, different beneficiary/victim sets, and different claimed_type values because they read different structural elements as primary. Affects both siblings because the composite reading's insistence on irreducibility places evidentiary and rhetorical pressure on any narrower reading's claim to have identified THE driver.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
