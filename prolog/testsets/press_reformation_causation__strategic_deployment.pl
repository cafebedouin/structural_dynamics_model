% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Press as Strategic Tool Deployed by Reformation Agents
 *   domain: historical/technological/religious
 *
 * SUMMARY:
 *   Between roughly 1517 and 1570, protestant reformers and printing
 *   entrepreneurs strategically mobilized the newly mature printing
 *   technology to distribute reform theology and scripture at a scale that
 *   outran suppression capacity. This story instantiates the
 *   strategic_deployment reading of the press_reformation_causation kernel:
 *   agency is upstream; the press is a neutral tool awaiting purposeful use;
 *   reformers and printers extracted doctrinal reach and economic profit
 *   through coordinated, intentional deployment. This reading forecloses
 *   technological_determinism (which treats the press as the causal prime
 *   mover) and coexists with mutual_shaping (which emphasizes feedback loops
 *   between technology and reformer strategy). The historical record supports
 *   strategic intentionality: explicit reformer-printer negotiations,
 *   deliberate vernacular text selection, strategic commissioning of
 *   high-volume editions, and tactical relocation of presses to
 *   weaker-censorship jurisdictions.
 *
 * KEY AGENTS:
 *   - Protestant reformers (Luther, Calvin, Zwingli, Tyndale): strategic deployers of printing for doctrinal amplification
 *   - Printing entrepreneurs (Froben, Torresani, De Gourmont): profit-seeking capital investors who negotiated with reformers and moved presses strategically
 *   - Catholic institutional authority (papal curia, inquisition, bishops): structural payer bearing suppression costs and doctrinal erosion
 *   - Manuscript copyists and scribes: displaced livelihood bearers who paid the transition cost
 *   - Literate urban audiences: coordination beneficiaries gaining access to texts
 *   - Territorial rulers: structurally excluded, facing erosion of doctrinal control through jurisdictional fragmentation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.62).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.71).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.62).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Press as Strategic Tool Deployed by Reformation Agents").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "historical/technological/religious").

domain_priors:requires_active_enforcement(press_reformation_causation__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, '7164ffc4-d996-492d-958f-ef8afec91ea1').
narrative_ontology:cs_kernel_codification('7164ffc4-d996-492d-958f-ef8afec91ea1', distributed).
narrative_ontology:cs_authority_grounding('7164ffc4-d996-492d-958f-ef8afec91ea1', distributed).
narrative_ontology:cs_reading_relation('7164ffc4-d996-492d-958f-ef8afec91ea1', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('7164ffc4-d996-492d-958f-ef8afec91ea1', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('7164ffc4-d996-492d-958f-ef8afec91ea1', foundational, human_agency_upstream_of_technology).
narrative_ontology:cs_axiom_status(human_agency_upstream_of_technology, holdable).
narrative_ontology:cs_axiom_grounding('7164ffc4-d996-492d-958f-ef8afec91ea1', human_agency_upstream_of_technology, deontological).
narrative_ontology:cs_axiom('7164ffc4-d996-492d-958f-ef8afec91ea1', foundational, technology_as_neutral_capacity).
narrative_ontology:cs_axiom_status(technology_as_neutral_capacity, holdable).
narrative_ontology:cs_axiom_grounding('7164ffc4-d996-492d-958f-ef8afec91ea1', technology_as_neutral_capacity, instrumental).
narrative_ontology:cs_reference_frame('7164ffc4-d996-492d-958f-ef8afec91ea1', pre_print_doctrinal_gatekeeping).
narrative_ontology:cs_drift_state('7164ffc4-d996-492d-958f-ef8afec91ea1', post_reformation_establishment_1570, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7164ffc4-d996-492d-958f-ef8afec91ea1', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, printing_entrepreneurs).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_institutional_authority).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, manuscript_scribes_and_copyists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, literate_urban_audiences).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, human_agency_as_historical_driver).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, technology_as_neutral_tool).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, intentional_strategic_use_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Strategically mobilize the printing press to distribute theological texts, vernacular scripture, and polemical tracts at scale. They author reform manifestos knowing print will amplify reach and durability beyond manuscript circulation. Their goal is to reshape religious authority by outrunning institutional suppression through technological acceleration. They actively negotiate with printers, commission editions, and craft messaging for reproducibility.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, protestant_reformers, agenda_setter,
    organized, generational, constrained, continental).

% Operate printing houses as profit-seeking enterprises that capitalize on reform demand. They see theological controversy as a market opportunity and invest capital in presses, type, and distribution networks. They negotiate with reformers for manuscript rights, set print runs strategically, and move presses to jurisdictions with weaker censorship. Their extraction comes from monopolizing the printing bottleneck — reformers must pay or delay reaching audiences.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, printing_entrepreneurs, beneficiary,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, printing_entrepreneurs, agenda_setter).

% Attempts to suppress reform through manuscript confiscation, clergy censorship, and ecclesiastical prohibition — the pre-print toolkit. They face a structural shift: the same investment in suppression that worked on scribal networks fails on distributed printing. They must either match the printing investment (expensive, technologically unfamiliar) or accept erosion of doctrinal control. Their suppression costs rise; their enforcement reach shortens.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_institutional_authority, payer,
    institutional, civilizational, trapped, continental).

% Lose livelihood and institutional placement as print production substitutes for scribal copying. Monasteries and scriptoriums that employed copyists retract as print-based competition undercuts demand for hand-copied texts. Their skilled labor becomes economically obsolete within a generation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, manuscript_scribes_and_copyists, payer,
    powerless, biographical, constrained, local).

% Gain access to reform theology, scripture in vernacular, and polemical debate at unprecedented scale and cost. The coordination benefit is real: cheap, durable texts enable lay theological literacy and participation in doctrinal disputes previously confined to clergy. They experience genuine expansion of choice and access.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, literate_urban_audiences, beneficiary,
    moderate, biographical, mobile, continental).

% Hold power over press licensing and movement within their domains but find themselves structurally outmaneuvered by reformers and printers who exploit jurisdictional fragmentation. A banned printer relocates; a suppressed text circulates through smuggling networks. Rulers pay in lost doctrinal control and face erosion of alliance-based religious uniformity.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, feudal_territorial_rulers, excluded,
    institutional, generational, trapped, regional).

% Examines the historical record to distinguish agency-driven deployment from technological causation. Looks for evidence of strategic intentionality: reformer communications with printers, deliberate manuscript commissioning, tactical choices about vernacular language and format, negotiated print runs, and active suppression tactics by both reformers and catholic authorities. Assesses whether the press constraint operated as coordination (rope) or extraction.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, historian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__strategic_deployment, printing_entrepreneurs).
narrative_ontology:fixing_cost_class(press_reformation_causation__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press, when strategically deployed by reformers and printers, solves a coordination problem: how to propagate doctrinal challenge across fragmented European jurisdictions faster than manuscript networks or monastic suppression can contain it. Without coordinated printing investment and deliberate text selection by reformers, doctrinal circulation remains slow and localized.
% TRANSFER_FUNCTION: Moves economic value from manuscript-dependent institutions (monasteries, cathedral schools, the catholic institutional apparatus dependent on scarce, controlled texts) to printing entrepreneurs and their reform-allied stakeholders. Printing entrepreneurs extract profit from the printing monopoly; reformers extract doctrinal reach and institutional destabilization. Catholic institutional authority pays in suppression costs and doctrinal control loss.
% ABSENT_VOICES: Manuscript copyists and scribes are structurally excluded from the deployment decision — they are displaced by it but have no seat at the table. Laypeople in non-literate, non-urban settings have no access to reform texts and no voice in the strategic deployment choices. Territorial rulers, while powerful, find their authority eroded by the print-enabled blur of jurisdictional boundaries and thus experience constraint differently than they would acknowledge.
% DISAPPEARANCE_RATIONALE: If reformers and printers had NOT strategically deployed the press, Reformation as a mass movement does not occur at scale. Catholic institutional authority retains doctrinal control through manuscript suppression; reformation theology circulates as manuscript fragments and oral dissent within local networks. The religious, political, and intellectual reorganization of Europe depends on the strategic deployment choice and the printing technology that made it feasible.
% FOUNDING_PROBLEM: Reformers face a structural problem: how to challenge catholic doctrinal monopoly when the mechanism of doctrinal control is manuscript scarcity and institutional gatekeeping. Catholic authority can suppress a few hidden copies; it cannot suppress thousands of identical printed texts circulating simultaneously across multiple jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary reformer correspondence (Luther, Calvin, Zwingli, Tyndale) explicitly discusses printing strategy and manuscript commissioning for print. Printer business records and colophons document deliberate negotiation with reformers and strategic print-run decisions. Catholic suppression records (bans, confiscations, inquisitorial proceedings) document the institutional response to printed reform texts. Independent historical scholarship (Eisenstein, Pettegree, Wiesner-Hanks) corroborates intentional strategic deployment as the historical motor, distinguishing it from technological determinism. The founding problem as stated is attested by sources outside the reformer/printer beneficiary set.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.62 because the constraint's initial function is coordination (solving the circulation problem) but printing entrepreneurs extract monopoly rent from the printing bottleneck and reformers extract doctrinal reach through controlled text selection. Suppression requirement climbs from 0.42 to 0.71 as catholic authority must invest massively to contain distributed printing — the suppression cost is structural, not incidental, because the same suppression that worked on scribal networks fails on print. Theater ratio stays low (peaks at 0.28) because the strategic deployment is functionally organized: texts are chosen for doctrinal impact, not pageantry; edition sizes are calibrated to market; print runs serve the coordination goal. The coercion grid shows differential pressure: structural (catholic institutional authority) and organizational (both reformers and printers) experience high stakes inflation and suppression intensity; individual copyists and non-urban classes experience lower direct suppression but accessibility collapse as text circulation accelerates.
 *
 * PERSPECTIVAL GAP:
 *   The reformer/printer coalition experiences the press as coordination rope: a tool they deliberately deployed to solve a problem (getting theology to scale) and from which they extract consistent benefit (doctrinal reach, profit, institutional power). Catholic institutional authority experiences it as snare: they are trapped by civilizational commitment to doctrinal authority, cannot exit their role (cannot abandon doctrine to preserve institutional power), and face suppression costs that rise without bound as reformers and printers escalate deployment. From the reformer seat: 'the press is a magnificent tool we used brilliantly.' From the catholic seat: 'the press became a weapon used against us, and the more we suppress the more it costs, with no exit but theological capitulation.' The engine computes this gap from power (organized vs. institutional), exit (arbitrage vs. trapped + identity_locked), and beneficiary/victim status (beneficiaries vs. victims). Time horizon plays a role too: reformers operate on biographical/generational horizons (commit to doctrinal strategy for a few decades); catholic authority operates on civilizational horizon (doctrine is eternal). The mismatch produces different constraint experiences from same seat over different time horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers and printing entrepreneurs are the primary beneficiaries and agenda-setters — they control deployment decisions, commission texts strategically, negotiate print runs, and collect the doctrinal/economic gains. Their directionality is beneficiary-aligned (d ≈ 0.15-0.25): they benefit without coercion, have arbitrage exit options (relocate presses, shift to different markets), and drive the constraint's persistence. Catholic institutional authority is the primary target (d ≈ 0.85-0.95): they bear the suppression cost, cannot exit (civilizational time horizon, institutional identity fusion), and face erosion of their core function (doctrinal gatekeeping). Manuscript copyists are trapped victims (d ≈ 0.80) with no strategic agency. Literate urban audiences are mildly beneficiary-aligned (d ≈ 0.35-0.45): they gain access but have constrained choice in which texts circulate (reformers and printers still control selection). Directionality overrides are unnecessary; structural derivation from beneficiary/victim declarations and exit options produces the right assignment.
 *
 * MANDATROPHY ANALYSIS:
 *   The strategic deployment reading avoids mandatrophy hazards that catch technological determinism. Technological determinism says 'the press caused the Reformation' — which conflates coordination (the real problem being solved: how to circulate doctrine at speed) with causation (the technology alone did not cause reformers to exist or to want to challenge doctrine). The strategic deployment reading locates the founding problem correctly (reformer need to overcome manuscript scarcity and institutional suppression) and identifies the actual mechanism (intentional deployment by agents with motive and means). The constraint persists because printing entrepreneurs profit from it and reformers continue to use it to drive their doctrinal agenda; it would collapse if either coalition abandoned deployment. This keeps it from sliding into piton territory (zombie persistence with no beneficiary capturing the extraction). The founding problem (how to challenge institutional doctrine at scale) remains live as long as non-catholics seek to propagate alternative theology — which it does throughout the period 1520-1570 and beyond. The reading also avoids false-summit mountain framing by explicitly declaring beneficiaries (reformers, printers) and victims (catholic authority, copyists), which triggers false-summit detection if anyone tried to claim this as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_threshold,
    'How much explicit strategic coordination between reformers and printers is required to establish the deployment as intentional rather than opportunistic adaptation?',
    'Archival analysis of reformer-printer correspondence, business records, contract language, and manuscript commissioning patterns. Identify decision points where either coalition chose printing investment over alternatives.',
    'If abundant coordination records exist (letters discussing strategy, negotiated edition sizes, planned text selection), the reading is strongly supported. If coordination is sparse or post-hoc rationalization, the mutual_shaping reading (feedback loops, unplanned synergies) gains plausibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentionality_threshold, empirical, 'The degree of documented strategic intentionality between reformer and printer coalitions').

omega_variable(
    neutral_tool_counterfactual,
    'Was the printing press truly ''neutral capacity awaiting purposeful use,'' or did its material properties (speed, duplication, scalability) constrain what purposes it could serve?',
    'Comparative history: examine printing''s early uses in non-reformation contexts (legal documents, commercial printing, devotional texts) and ask whether reformers chose to exploit properties the press already had, or whether the press created new possibilities reformers then pursued.',
    'If the press served many purposes equally well pre-reformation, the neutral-tool framing holds. If printing''s properties inherently favored replication, scalability, and vernacular circulation (making reformation use more likely than random selection), the technological_determinism reading gains structural support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutral_tool_counterfactual, conceptual, 'Whether the press''s material properties were truly neutral or structurally shaped what uses were viable').

omega_variable(
    catholic_suppression_counterfactual,
    'Could catholic institutional authority have suppressed the Reformation without printing (through manuscript confiscation, clergy control, territorial suppression)? Or did printing make suppression structurally impossible?',
    'Historical comparison with pre-print heresies (Lollards, Hussites, Waldensians) that were suppressed despite wide support. Trace how their suppression differed from reformation suppression, and whether the difference is printing availability or something else (geographic scope, political fragmentation, institutional readiness).',
    'If pre-print heresies could be suppressed through the same tools (confiscation, clergy enforcement), printing did not make suppression impossible — it made suppression more costly. This supports strategic_deployment (reformers exploited a costly-to-suppress technology). If printing alone made suppression impossible, technological_determinism gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catholic_suppression_counterfactual, empirical, 'Whether printing made suppression structurally impossible or merely expensive').

omega_variable(
    kernel_reading_committer_frame,
    'Is the press_reformation_causation kernel accurately framed as three mutually exclusive readings (strategic_deployment, mutual_shaping, technological_determinism), or are there alternative framings that would decompose the kernel differently?',
    'Historiographic analysis of how the causal question is framed across different scholarly traditions (history of technology, religious history, social history). Identify whether the three-reading set exhausts the conceptually distinct mechanisms or whether other readings (e.g., institutional readiness as upstream driver, religious crisis as independent motivation) would change the kernel structure.',
    'If alternative framings identify genuinely distinct mechanisms, the kernel should be refined to include them as distinct readings. If the three-reading set is structurally complete, the reading distinctions are real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether the kernel decomposition into three readings is structurally complete or whether alternative framings would redefine the dispute').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__strategic_deployment, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(pres_tr_t0, projected).
narrative_ontology:measurement(pres_tr_t10, press_reformation_causation__strategic_deployment, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(pres_tr_t10, observed).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causation__strategic_deployment, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(pres_tr_t20, observed).
narrative_ontology:measurement(pres_tr_t30, press_reformation_causation__strategic_deployment, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(pres_tr_t30, observed).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causation__strategic_deployment, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(pres_tr_t40, observed).
narrative_ontology:measurement(pres_tr_t50, press_reformation_causation__strategic_deployment, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(pres_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__strategic_deployment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(pres_be_t0, projected).
narrative_ontology:measurement(pres_be_t10, press_reformation_causation__strategic_deployment, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(pres_be_t10, observed).
narrative_ontology:measurement(pres_be_t20, press_reformation_causation__strategic_deployment, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(pres_be_t20, observed).
narrative_ontology:measurement(pres_be_t30, press_reformation_causation__strategic_deployment, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(pres_be_t30, observed).
narrative_ontology:measurement(pres_be_t40, press_reformation_causation__strategic_deployment, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(pres_be_t40, observed).
narrative_ontology:measurement(pres_be_t50, press_reformation_causation__strategic_deployment, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(pres_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causation__strategic_deployment, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(pres_su_t0, projected).
narrative_ontology:measurement(pres_su_t10, press_reformation_causation__strategic_deployment, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(pres_su_t10, observed).
narrative_ontology:measurement(pres_su_t20, press_reformation_causation__strategic_deployment, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(pres_su_t20, observed).
narrative_ontology:measurement(pres_su_t30, press_reformation_causation__strategic_deployment, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(pres_su_t30, observed).
narrative_ontology:measurement(pres_su_t40, press_reformation_causation__strategic_deployment, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(pres_su_t40, observed).
narrative_ontology:measurement(pres_su_t50, press_reformation_causation__strategic_deployment, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(pres_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(pres_grid_01, press_reformation_causation__strategic_deployment, accessibility_collapse(class), 0, 0.35).
narrative_ontology:measurement(pres_grid_02, press_reformation_causation__strategic_deployment, accessibility_collapse(class), 50, 0.48).
narrative_ontology:measurement(pres_grid_03, press_reformation_causation__strategic_deployment, accessibility_collapse(individual), 0, 0.28).
narrative_ontology:measurement(pres_grid_04, press_reformation_causation__strategic_deployment, accessibility_collapse(individual), 50, 0.42).
narrative_ontology:measurement(pres_grid_05, press_reformation_causation__strategic_deployment, accessibility_collapse(organizational), 0, 0.42).
narrative_ontology:measurement(pres_grid_06, press_reformation_causation__strategic_deployment, accessibility_collapse(organizational), 50, 0.58).
narrative_ontology:measurement(pres_grid_07, press_reformation_causation__strategic_deployment, accessibility_collapse(structural), 0, 0.38).
narrative_ontology:measurement(pres_grid_08, press_reformation_causation__strategic_deployment, accessibility_collapse(structural), 50, 0.52).
narrative_ontology:measurement(pres_grid_09, press_reformation_causation__strategic_deployment, resistance(class), 0, 0.48).
narrative_ontology:measurement(pres_grid_10, press_reformation_causation__strategic_deployment, resistance(class), 50, 0.62).
narrative_ontology:measurement(pres_grid_11, press_reformation_causation__strategic_deployment, resistance(individual), 0, 0.42).
narrative_ontology:measurement(pres_grid_12, press_reformation_causation__strategic_deployment, resistance(individual), 50, 0.54).
narrative_ontology:measurement(pres_grid_13, press_reformation_causation__strategic_deployment, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(pres_grid_14, press_reformation_causation__strategic_deployment, resistance(organizational), 50, 0.72).
narrative_ontology:measurement(pres_grid_15, press_reformation_causation__strategic_deployment, resistance(structural), 0, 0.52).
narrative_ontology:measurement(pres_grid_16, press_reformation_causation__strategic_deployment, resistance(structural), 50, 0.68).
narrative_ontology:measurement(pres_grid_17, press_reformation_causation__strategic_deployment, stakes_inflation(class), 0, 0.38).
narrative_ontology:measurement(pres_grid_18, press_reformation_causation__strategic_deployment, stakes_inflation(class), 50, 0.58).
narrative_ontology:measurement(pres_grid_19, press_reformation_causation__strategic_deployment, stakes_inflation(individual), 0, 0.32).
narrative_ontology:measurement(pres_grid_20, press_reformation_causation__strategic_deployment, stakes_inflation(individual), 50, 0.48).
narrative_ontology:measurement(pres_grid_21, press_reformation_causation__strategic_deployment, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(pres_grid_22, press_reformation_causation__strategic_deployment, stakes_inflation(organizational), 50, 0.72).
narrative_ontology:measurement(pres_grid_23, press_reformation_causation__strategic_deployment, stakes_inflation(structural), 0, 0.45).
narrative_ontology:measurement(pres_grid_24, press_reformation_causation__strategic_deployment, stakes_inflation(structural), 50, 0.64).
narrative_ontology:measurement(pres_grid_25, press_reformation_causation__strategic_deployment, suppression(class), 0, 0.35).
narrative_ontology:measurement(pres_grid_26, press_reformation_causation__strategic_deployment, suppression(class), 50, 0.68).
narrative_ontology:measurement(pres_grid_27, press_reformation_causation__strategic_deployment, suppression(individual), 0, 0.28).
narrative_ontology:measurement(pres_grid_28, press_reformation_causation__strategic_deployment, suppression(individual), 50, 0.62).
narrative_ontology:measurement(pres_grid_29, press_reformation_causation__strategic_deployment, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(pres_grid_30, press_reformation_causation__strategic_deployment, suppression(organizational), 50, 0.76).
narrative_ontology:measurement(pres_grid_31, press_reformation_causation__strategic_deployment, suppression(structural), 0, 0.38).
narrative_ontology:measurement(pres_grid_32, press_reformation_causation__strategic_deployment, suppression(structural), 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, resource_allocation).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__strategic_deployment, 0.18).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, counter_reformation_institutional_response).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, scribal_economy_displacement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the press_reformation_causation kernel family. Strategic_deployment emphasizes agent intentionality and deliberate technology deployment; it forecloses technological_determinism (which treats the press as the causal prime mover) and coexists with mutual_shaping (which includes feedback loops). The three readings have structurally distinct epsilon values because they locate the causal mechanism differently and identify different beneficiary/victim structures. Do not merge them into one constraint with measurement basis variability; the ε-invariance principle requires separate stories per reading, linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causation__strategic_deployment, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
