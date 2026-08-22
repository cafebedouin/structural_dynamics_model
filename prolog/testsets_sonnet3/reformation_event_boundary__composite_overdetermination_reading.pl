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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   This story instantiates the composite-overdetermination reading of the
 *   Reformation event boundary kernel: the claim that theological innovation,
 *   institutional collapse, political realignment, and denominational
 *   proliferation occurred simultaneously and irreducibly, such that no
 *   single causal driver or periodization scheme captures the phenomenon.
 *   This is deliberately NOT a synthesis or average of the sibling readings
 *   (theological_climb_reading, political_swap_reading) — it is a distinct
 *   structural claim: that the event is best modeled as multiple CS patterns
 *   (doctrinal authority relocation, jurisdictional/fiscal transfer,
 *   information-market transformation) operating in parallel on overlapping
 *   populations, each with its own beneficiary/victim structure, rather than
 *   one process with the others as derivative or epiphenomenal. Where the
 *   theological reading would treat institutional collapse as consequence of
 *   doctrinal truth, and the political reading would treat theology as
 *   post-hoc rationalization for elite asset seizure, this reading holds that
 *   treating either as primary requires suppressing evidence the other
 *   reading needs.
 *
 * KEY AGENTS:
 *   - territorial_princes_and_magistrates: primary beneficiary of jurisdictional/fiscal transfer, institutional/arbitrage
 *   - reformed_clergy_networks: beneficiary and co-agenda-setter of doctrinal relocation, organized/constrained
 *   - printing_and_pamphlet_trades: beneficiary of information-market transformation, moderate/mobile
 *   - peasant_reform_movements: primary victim of the political realignment sub-process, powerless/trapped
 *   - religious_minorities_under_new_confessional_states: victim of denominational-proliferation containment, powerless/trapped
 *   - displaced_monastic_and_clerical_orders: victim of institutional collapse sub-process, moderate/constrained
 *   - papacy_and_curial_administration: primary payer at civilizational scale, institutional/constrained
 *   - historians_of_the_reformation: analytical observer whose ongoing disagreement is itself part of the phenomenon this reading models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.62).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation as Composite Overdetermined Event (Theological/Institutional/Political/Denominational)").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '0be9a3c7-70a8-4ce4-9952-2ab0009cf179').
narrative_ontology:cs_kernel_codification('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', distributed).
narrative_ontology:cs_authority_grounding('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', distributed).
narrative_ontology:cs_reading_relation('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', foundational, causal_processes_are_irreducibly_parallel).
narrative_ontology:cs_axiom_status(causal_processes_are_irreducibly_parallel, holdable).
narrative_ontology:cs_axiom_grounding('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', causal_processes_are_irreducibly_parallel, empirically_contingent).
narrative_ontology:cs_axiom('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', foundational, periodization_contest_is_structural_not_remediable).
narrative_ontology:cs_axiom_status(periodization_contest_is_structural_not_remediable, holdable).
narrative_ontology:cs_axiom_grounding('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', periodization_contest_is_structural_not_remediable, conventional).
narrative_ontology:cs_reference_frame('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', medieval_papal_universal_jurisdiction).
narrative_ontology:cs_drift_state('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', peace_of_westphalia_1648, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('0be9a3c7-70a8-4ce4-9952-2ab0009cf179', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, territorial_princes_and_magistrates).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, reformed_clergy_networks).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, printing_and_pamphlet_trades).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, peasant_reform_movements).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, religious_minorities_under_new_confessional_states).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, displaced_monastic_and_clerical_orders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, papacy_and_curial_administration).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, sola_fide_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, territorial_church_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convert theological rupture into a legal instrument (cuius regio, eius religio) that transfers church property, tax exemption, and jurisdictional authority from Rome to the territorial state. They fund reformers, ratify new confessions, and enforce conformity within their territories, capturing both the ideological legitimacy of doctrinal reform and the material assets of the dissolved ecclesiastical structure.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, territorial_princes_and_magistrates, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, territorial_princes_and_magistrates, agenda_setter).

% Gain new institutional positions, printing patronage, and doctrinal authority within emerging territorial churches. Their theological claims (justification by faith, vernacular scripture) are genuine intellectual commitments but also function as the legitimating vocabulary for the concurrent transfer of ecclesiastical power and property.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, reformed_clergy_networks, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, reformed_clergy_networks, agenda_setter).

% Profit directly from the explosion of vernacular religious controversy, producing pamphlets, translated scripture, and polemic across confessional lines. They have structural interest in prolonging doctrinal conflict and denominational proliferation, since each new sect and controversy generates fresh demand.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, printing_and_pamphlet_trades, beneficiary,
    moderate, biographical, mobile, continental).

% Initially read Luther's theological claims as authorizing social and economic liberation (1524-25 Peasants' War), then were violently suppressed with reformer endorsement once magisterial reformers allied with princes. They bore the sharpest costs of the political realignment while gaining none of the institutional settlement.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, peasant_reform_movements, payer,
    powerless, immediate, trapped, regional).

% Anabaptists, spiritualists, and other radical reformers who took theological reform further than magisterial reformers or princes would tolerate were executed, exiled, or forced underground by both Catholic and Protestant territorial authorities. Their existence demonstrates that denominational proliferation was contained and channeled, not left open-ended.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, religious_minorities_under_new_confessional_states, payer,
    powerless, biographical, trapped, regional).

% Monasteries were dissolved and their lands, endowments, and charitable functions transferred to secular authorities or reformed institutions. Some clergy found new roles in reformed churches; many lost livelihood, vocation, and social standing with no institutional recourse.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, displaced_monastic_and_clerical_orders, payer,
    moderate, biographical, constrained, continental).

% Loses direct jurisdictional, financial, and doctrinal authority over roughly half of Western Christendom within a single century. Responds with Counter-Reformation reforms that concede some grievances while defending the institutional core, confirming that the challenge was simultaneously doctrinal and jurisdictional rather than purely either.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, papacy_and_curial_administration, payer,
    institutional, civilizational, constrained, continental).

% Debate which causal driver (theological, institutional, political, social) is primary, and where the event's boundaries lie. The composite reading treats this ongoing disagreement as evidence of the event's actual structure rather than a failure of historiographical method to converge.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historians_of_the_reformation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_event_boundary__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Reformation as composite event solves no single coordination problem — it is better described as several concurrent partial-coordination arrangements (doctrinal clarification among reformers, jurisdictional consolidation among princes, information distribution among printers) that happened to reinforce each other, none of which required the others to occur but all of which accelerated once yoked together.
% TRANSFER_FUNCTION: Multiple simultaneous transfers: doctrinal authority moves from papal magisterium to territorial confessions and vernacular scripture; ecclesiastical property and revenue move from Rome and monastic orders to territorial states; social legitimacy for radical economic claims is extended briefly to peasant movements then violently withdrawn; printing capital and readership move from religious monopoly to competitive vernacular markets.
% ABSENT_VOICES: Radical reformers (Anabaptists, spiritualists) and peasant movements who took the theological logic of individual conscience and scriptural authority to conclusions the magisterial reformers and princes would not tolerate are excluded from the settlement that bears the Reformation's name — the composite reading is one of the few framings that keeps their exclusion visible, since the theological-only and political-only readings each have structural reasons to background them.
% DISAPPEARANCE_RATIONALE: Whether 'the Reformation' as a boundaried event would leave a coherent gap if erased is itself disputed: proponents of the composite reading argue no single counterfactual removal is coherent, since removing the theological dispute alone leaves the fiscal-jurisdictional pressure on Rome intact (and vice versa) — the world would still rearrange, but along none of the single-driver counterfactuals that theological-only or political-only readings can construct cleanly.
% FOUNDING_PROBLEM: No single founding problem: overlapping crises — a legitimacy crisis in Western Christian doctrine and practice, a fiscal-jurisdictional crisis in papal-secular relations, and an information/distribution transformation from printing — arrived in temporal proximity and interacted, such that reformers, princes, and printers each solved a different problem using the same set of events as raw material.
% FOUNDING_PROBLEM_CORROBORATION: Confessional historians (both Protestant and Catholic apologetic traditions) each attest to a single-driver founding problem consistent with their own tradition's legitimacy claim; social and economic historians outside either confessional tradition (e.g., historians of print culture, peasant studies, and state formation) corroborate that at least three distinct causal processes were operating on overlapping populations in the same decades, which is the evidentiary basis for treating the composite reading as structurally supported rather than merely a synthesis of convenience.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, contested).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) and suppression (0.62) are authored at levels reflecting the composite view's claim that no single sub-process alone accounts for the total coercive and extractive structure — the peasant suppression, the confiscation of monastic property, and the persecution of radical sects are each partial extractions that only sum to the full picture when read together. Suppression rises sharply after 1525 (Peasants' War suppression) and again around 1618 (Thirty Years' War, the political-military culmination of confessional territorialization), then eases somewhat by 1648 (Peace of Westphalia institutionalizes toleration among recognized confessions, though not for excluded minorities). Theater ratio rises over the interval as confessional identity increasingly becomes a performed marker of political loyalty (attendance, oath-taking, confessional subscription) layered atop genuine doctrinal commitment — this is exactly the metric substitution pattern the composite reading predicts once theology becomes entangled with statecraft.
 *
 * PERSPECTIVAL GAP:
 *   The princes/clergy/printer seats and the peasant/minority/monastic seats compute structurally differently precisely because they are experiencing DIFFERENT sub-processes of the same composite event: the former experience coordinated institution-building (a rope-like function — solving real problems of doctrinal clarity, territorial administration, information distribution), while the latter experience enforced extraction (a snare-like function — property seizure, violent suppression, exclusion from the settlement). The tangled_rope classification for the composite as a whole reflects that BOTH are true simultaneously and neither seat's experience is the 'real' one that the other seat merely fails to see.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary status derives from capture of the doctrinal, fiscal, or informational rents released by the loosening of papal-institutional monopoly: princes capture jurisdiction and property, reformed clergy capture doctrinal authority and patronage, printers capture a competitive information market. Victim status derives from the reverse: the papacy loses jurisdiction and revenue at civilizational scope with no exit; monastic orders lose institutional existence with only constrained re-entry into reformed structures; peasants and religious minorities are granted a brief theological warrant for liberation-claims which is then forcibly withdrawn once it threatens the emerging prince-clergy settlement, leaving them trapped with no institutional standing in any resulting confession.
 *
 * MANDATROPHY ANALYSIS:
 *   Treating the Reformation as a single-driver event (pure theological breakthrough OR pure political opportunism) risks either romanticizing the settlement as pure coordination (erasing the peasant and minority victims) or reducing it to pure cynical extraction (erasing the genuine intellectual and religious stakes reformers and lay believers held). The composite/tangled_rope reading is structurally required to keep both victim sets and both coordination functions in view at once — it is the reading that resists the temptation to declare the 'true' driver retroactively vindicated by outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    single_driver_recoverability,
    'Could a sufficiently fine-grained causal history reduce the Reformation to a single dominant driver (theological, institutional, political, or informational), with the appearance of overdetermination being an artifact of coarse historiographical categories rather than a genuine structural feature?',
    'Micro-historical case studies tracing specific territorial conversions (e.g., individual German principalities, Swiss cantons) to test whether doctrinal conviction, fiscal motive, or political calculation can be shown to have been causally prior in each case, and whether a consistent ordering holds across cases.',
    'If a consistent single-driver ordering is recoverable across most cases, the composite reading is falsified in favor of one of the sibling readings; if orderings vary by case with no consistent driver, the composite reading is strengthened as the more general structural claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_driver_recoverability, empirical, 'Whether apparent overdetermination is genuine or an artifact of coarse categorization.').

omega_variable(
    committer_kernel_disagreement_location,
    'Where exactly do the theological_climb_reading and political_swap_reading locate their disagreement with this composite reading — is it a disagreement about weighting (all three factors present, differing magnitude) or a disagreement about causal structure (one factor is upstream/generative of the others)?',
    'Comparative analysis of the sibling readings'' own axioms: if both siblings treat their preferred driver as causally prior (generative) rather than merely larger in magnitude, the disagreement is structural, not merely a weighting dispute, and the composite reading''s claim of irreducible parallelism is the substantive point of contention.',
    'If the disagreement is genuinely structural (causal priority, not weighting), the three readings cannot be reconciled by better data alone — they rest on different models of historical causation itself, which is exactly what the composite reading asserts and the siblings each deny in their own way.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_location, conceptual, 'Locates the specific structural disagreement between this reading and its siblings.').

omega_variable(
    victim_set_variance_by_subprocess,
    'Does foregrounding a different sub-process (theological vs. institutional vs. political vs. denominational-proliferation) genuinely change which groups count as victims and beneficiaries, or do the same groups appear as victims/beneficiaries under every framing with only the causal narrative differing?',
    'Cross-tabulate victim/beneficiary lists that would be authored under each sibling reading''s own logic (theological reading would center excluded heterodox voices as victims of doctrinal narrowing; political reading would center the papacy and displaced clergy as victims of asset seizure) against this composite reading''s list.',
    'If victim sets vary substantially by which sub-process is foregrounded, this corroborates the expected structural delta that different readings track genuinely different completion points and different populations — supporting the claim that periodization contest is a structural feature, not a bug to be resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_variance_by_subprocess, conceptual, 'Tests whether the composite reading''s distinguishing claim about victim-set variance holds up against the sibling readings'' own internal logics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(refo_tr_t1525, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1525, 0.2).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1555, 0.32).
narrative_ontology:measurement(refo_tr_t1585, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1585, 0.38).
narrative_ontology:measurement(refo_tr_t1618, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1618, 0.42).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1648, 0.4).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1517, 0.28).
narrative_ontology:measurement(refo_be_t1525, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1525, 0.45).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1555, 0.55).
narrative_ontology:measurement(refo_be_t1585, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1585, 0.6).
narrative_ontology:measurement(refo_be_t1618, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1618, 0.62).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1648, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1517, 0.2).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1525, 0.55).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1555, 0.6).
narrative_ontology:measurement(refo_su_t1585, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1585, 0.63).
narrative_ontology:measurement(refo_su_t1618, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1618, 0.75).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1648, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, political_swap_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the reformation_event_boundary kernel. theological_climb_reading treats doctrinal breakthrough as primary and institutional separation as necessary consequence (closer to a Mountain/Rope profile at the doctrinal level, since the theological claim itself is treated as a genuine discovery rather than extraction). political_swap_reading treats secular power-seizure as primary and theology as post-hoc legitimation (closer to a pure Snare/Tangled Rope profile centered on princely asset capture). This composite reading holds that the three sub-processes are separately real, run in parallel, and jointly compose the tangled_rope profile authored here. Each reading has its own ε, its own beneficiary/victim structure, and its own claimed_type — they are not the same constraint measured three ways; they are three genuinely different structural claims about which sub-events are causally load-bearing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
