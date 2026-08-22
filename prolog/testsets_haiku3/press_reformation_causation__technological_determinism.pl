% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Technological Determinant of Reformation (Deterministic Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The technological-determinism reading claims that the invention of
 *   movable-type printing in the mid-15th century made the Reformation
 *   inevitable by removing the Church's monopoly on text production and
 *   reproduction. In this framing, printing is a mountain—a technological
 *   capacity with intrinsic properties (reproducibility, decentralization,
 *   cost reduction) that cascade through social systems regardless of human
 *   intention or resistance. The Church cannot prevent the Reformation
 *   because it cannot prevent printing; reformers succeed not because of
 *   superior strategy but because printing makes vernacular scripture and
 *   mass mobilization structurally possible. Beneficiaries (reformation
 *   movements, vernacular constituencies, secular rulers) benefit from
 *   exogenous technological capacity; the Church is trapped because
 *   alternatives (reverting to manuscript culture, destroying all presses)
 *   are infeasible. This reading explicitly brackets questions of agency,
 *   strategy, and contestation—it treats these as downstream effects of the
 *   technological determinant, not as independent causes.
 *
 * KEY AGENTS:
 *   - Printing press technology itself (non-agent): reproducibility and decentralization as intrinsic properties
 *   - Reformation movement (moderate power, continental scope): downstream beneficiary of printing's capacity
 *   - Vernacular literacy constituency (organized power, continental scope): new public enabled by cheap reproduction
 *   - Catholic Church authority (institutional power, universal scope): structurally trapped by technological change, resistance futile
 *   - Royal secular authorities (powerful, national scope): beneficiaries of the shift toward distributed authority
 *   - Censorship apparatus (institutional, biographical): doomed to fail against distributed production
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.15).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.08).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Technological Determinant of Reformation (Deterministic Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '9eb92736-55ca-403b-a2ce-9ca90349f835').
narrative_ontology:cs_kernel_codification('9eb92736-55ca-403b-a2ce-9ca90349f835', distributed).
narrative_ontology:cs_authority_grounding('9eb92736-55ca-403b-a2ce-9ca90349f835', expertise).
narrative_ontology:cs_interpretation_layer_present('9eb92736-55ca-403b-a2ce-9ca90349f835').
narrative_ontology:cs_reading_relation('9eb92736-55ca-403b-a2ce-9ca90349f835', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('9eb92736-55ca-403b-a2ce-9ca90349f835', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('9eb92736-55ca-403b-a2ce-9ca90349f835', foundational, technology_determines_social_outcomes).
narrative_ontology:cs_axiom_status(technology_determines_social_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('9eb92736-55ca-403b-a2ce-9ca90349f835', technology_determines_social_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('9eb92736-55ca-403b-a2ce-9ca90349f835', secondary, human_agency_downstream_of_technology).
narrative_ontology:cs_axiom_status(human_agency_downstream_of_technology, holdable).
narrative_ontology:cs_axiom_grounding('9eb92736-55ca-403b-a2ce-9ca90349f835', human_agency_downstream_of_technology, instrumental).
narrative_ontology:cs_reference_frame('9eb92736-55ca-403b-a2ce-9ca90349f835', technological_determinism_framework).
narrative_ontology:cs_drift_state('9eb92736-55ca-403b-a2ce-9ca90349f835', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9eb92736-55ca-403b-a2ce-9ca90349f835', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, reformation_movement).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_literacy_constituency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, royal_secular_authorities).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A technological artifact with intrinsic properties: reproducibility, speed, cost-per-copy reduction, decentralization of production capacity. This is the non-agent entity that the deterministic reading treats as the causal driver.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_press_technology, observer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, printing_press_technology).

% Religious reform advocates (Luther, Calvin, Zwingli, and their networks) whose central claims—textual authority of scripture, vernacular access, doctrinal correction—became distributable at scale once printing lowered the cost of reproduction. In the deterministic reading, they are downstream beneficiaries of the press's exogenous capacity; the press created their possibility.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, reformation_movement, beneficiary,
    moderate, generational, arbitrage, continental).

% Literate non-clergy populations (merchants, artisans, educated laity) who gain access to scripture and theological argument in their own languages rather than Latin. Printing made this constituency both possible (literacy became economically justified) and politically relevant (mass readership created new publics for religious ideas).
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_literacy_constituency, beneficiary,
    organized, generational, arbitrage, continental).

% The Church's monopoly on textual production and interpretation erodes once printing decentralizes the means of reproduction. In the deterministic reading, the Church cannot resist this trend—resistance is futile because the technological capacity itself is the constraint, not negotiable. The Church is structurally trapped by the press's inescapable properties.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church_authority, payer,
    institutional, civilizational, trapped, universal).

% Monarchs and princes whose power over religious doctrine within their territories increases as printing enables mass communication to their subjects and makes Church authority fragmentable. In the deterministic reading, printing shifts the balance of power structurally toward secular authority.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, royal_secular_authorities, beneficiary,
    powerful, generational, constrained, national).

% The institutional mechanisms (Index of Forbidden Books, inquisitorial oversight, licensing) that attempt to control textual circulation. In the deterministic reading, these are doomed to fail because the underlying technology (distributed printing) is intrinsically harder to censor than manuscript production.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, censorship_apparatus, observer,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None declared in the deterministic reading. Printing is not presented as solving a collective-action problem through coordination; it is a technological capacity that cascades through social systems independently of intentional coordination.
% TRANSFER_FUNCTION: The deterministic reading does not frame this as a transfer mechanism but as a technological capability that shifts power: from centralized (Church) to distributed (reformers, vernacular constituencies, secular rulers), from monopoly to competition in the production and interpretation of texts.
% ABSENT_VOICES: Medieval manuscript scribes and the Church's institutional hierarchy benefit from continued scarcity; they would argue for manuscript reproduction as the natural pace and for ecclesiastical control of interpretation. They are absent from the deterministic reading's causal narrative because the reading treats the press as exogenous and overwhelming.
% DISAPPEARANCE_RATIONALE: In the deterministic reading, if the printing press had never been invented, the Reformation would not have occurred—at least not in the form it did, with mass mobilization, vernacular scripture, and the organizational scale it achieved. The technological capacity is the prior cause; without it, the world reverts to manuscript-era constraints on religious innovation.
% FOUNDING_PROBLEM: The deterministic reading does not posit a founding problem the press was designed to solve. Rather, it treats the press as a technology that—once invented—solved problems for reformers (how to distribute ideas at scale) and created problems for the Church (how to maintain doctrinal monopoly against distributed reproduction).
% FOUNDING_PROBLEM_CORROBORATION: The deterministic reading's causal claim is attested by technological historians (Elizabeth Eisenstein's thesis on the print revolution) and some Reformation historians who emphasize infrastructure preconditions for mass mobilization. However, the sibling readings (strategic_deployment, mutual_shaping) contest this causal direction, citing reformers' active use of the press and the co-evolution of technology with social demand. No outside-the-reading party attests determinism as settled; the claim remains historiographically disputed.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the deterministic reading does not frame the printing press as a mechanism of extraction—it is exogenous technological capacity. The measured 0.15 reflects only the historical asymmetry that printing benefited some constituencies (reformers, secular rulers, vernacular readers) while harming the Church's monopoly; this is not extraction in the snare sense (intentional transfer to a capturer) but technological displacement. Suppression is minimal (0.08) because the deterministic reading posits that the Church's suppression efforts (censorship, inquisition, Index) fail not because they are overcome by organized resistance but because the underlying technological capacity is intrinsically harder to suppress than manuscript production—suppression is futile, not because payers resist but because the technology escapes control. Theater is near-zero (0.05) because printing does not persist through performative maintenance; it persists through intrinsic utility (printers continue because printing is economically rational, not because of ritualized enforcement). Accessibility collapse is extremely high (0.92) because in the deterministic framing, alternatives to printing (returning to exclusive manuscript production, preventing the technology's spread) become inaccessible once the technology exists and proves useful. Resistance is near-zero (0.02) because the deterministic reading treats resistance from the Church as futile and therefore understates it—the Church does resist actively, but the reading brackets that resistance as epiphenomenal. The measurement series span 1440–1550 (Gutenberg through the mid-Reformation) with observed basis throughout, tracking the initial ramp of printing diffusion and its acceleration during the early Reformation period. All metrics are authored at every time point on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The deterministic reading produces a wide perspectival gap between the Church's experienced reality and the reformers' experienced reality, but the reading itself brackets the agentic interpretation of this gap. From the Church's position, the Reformation is an organizational catastrophe driven by technological disruption—they are trapped, resistance fails, authority fragments. From the reformers' position, printing enables their vision of scriptural democracy and doctrinal innovation—they are empowered. From the secular rulers' position, printing shifts the balance of power toward them and away from Rome—they are indirect beneficiaries. The deterministic reading does not ask whether these perspectives are *true* or *justified*; it asserts that they all flow from the same technological cause. A different reading (strategic_deployment) would emphasize that reformers *chose* to use printing, that printers *chose* to print reform materials, that the Church *chose* (unsuccessfully) to resist—and that the gap in outcomes reflects differences in strategic choice, not technological determinism. The deterministic reading collapses agency into downstream effects of the technology.
 *
 * DIRECTIONALITY LOGIC:
 *   In the deterministic reading, directionality is not about beneficiary/victim power dynamics in the snare sense (one agent capturing from another through enforcement) but about technological displacement. The printing press benefits reformation_movement and vernacular_literacy_constituency because the technology makes their projects feasible; it harms catholic_church_authority because it erodes the Church's structural monopoly. From the Church's seat, this is high directionality toward the target (d near 1.0)—the Church is trapped by technological change and cannot exit. From the reformation_movement seat, this is near the beneficiary end (d near 0.0)—they benefit from exogenous capacity. From the secular_authorities seat, directionality is moderate (d near 0.3–0.4)—they gain power but are not as completely dependent on printing as reformers are. Censorship apparatus is trapped (d near 1.0) because its enforced suppression collides with distributed printing and cannot win. No directionality overrides are needed because the structural derivation from the beneficiary/victim declarations captures the reading's intent: technological beneficiaries vs. technological losers.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterministic reading does not present a mandatrophy scenario. The founding_problem in this reading is not a problem the press was designed to solve (it was not designed to solve the Reformation); rather, the press is treated as a technology that, once invented, solved problems for reformers and created unsolvable problems for the Church. The founding_problem_status is 'dead' in the deterministic reading because it rejects the premise that the Reformation solved a pre-existing problem—instead, the press created the possibility of the Reformation, and then reformers solved institutional problems (doctrinal monopoly, vernacular access, mass mobilization) that the technology made solvable. The determination claim would be falsified if evidence showed the Reformation happening without printing (in which case printing becomes instrumental but not determinative) or printing failing to enable mass mobilization (in which case the technology is necessary but not sufficient). The reading is internally coherent: printing is the upstream constraint, Reformation outcomes are downstream effects, and no mandatrophy occurs because the constraint is the technology itself, not an outdated mandate for organizational behavior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_inevitability,
    'Is the printing press''s role in the Reformation a natural law (technology intrinsically determines social outcomes) or a constructed inevitability (technology and human agency co-produce outcomes, but the deterministic reading assigns all causality to the technological component)?',
    'Comparative analysis of how printing technology was deployed differently in different regions and confessions. If the same technology produced radically different outcomes depending on how actors used it, the determination claim is undermined. If outcomes were invariant across deployment contexts, the deterministic reading is strengthened.',
    'If the constraint is constructed inevitability masked as natural law, the beneficiary (reformation movement, vernacular constituencies) benefits from technological determinism narrative that obscures their own strategic agency. FSM would reclassify to tangled_rope or snare depending on whether the Church''s suppression operated through active enforcement or through structural disadvantage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_inevitability, empirical, 'Whether printing''s causal role is determined by intrinsic technological properties or by how it was deployed and adopted.').

omega_variable(
    counterfactual_precision_ambiguity,
    'If the printing press had been invented but reformers had not existed, would the Reformation have occurred? Conversely, if reformers existed but the press had not been invented, could they have mobilized at similar scale through manuscript circulation?',
    'Thought experiment grounded in historical evidence: (1) Did pre-Reformation reform movements (Wycliffe, Hus) operate significantly without print? (2) Did printing technology enable organizational forms that were impossible under manuscript constraints? (3) Would manuscript-era reformers have accomplished equivalent doctrinal innovation given different technologies?',
    'High sensitivity to counterfactual assumptions: determinism requires that ONLY the press (exogenous) is the sufficient cause; if reformers'' agency or pre-existing intellectual ferment are necessary, the reading shifts toward mutual_shaping or strategic_deployment. The determination claim depends on how precisely the counterfactual is framed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_precision_ambiguity, conceptual, 'Sensitivity of the deterministic claim to counterfactual assumptions about absent reformers or absent technology.').

omega_variable(
    beneficiary_identity_fusion_ambiguity,
    'Do the reformation_movement and vernacular_literacy_constituency benefit from printing as an exogenous capacity, or do they benefit from a narrative that attributes their success to technology rather than to their own organizational power and strategic choices?',
    'Post-printing analysis: did reformation movements that emphasize technological determinism claim less credit for their own agency? Did they invest less in understanding and defending their strategic choices? Identity fusion would show as reformers internalizing the technological determinism narrative even when archival evidence shows active deployment and strategy.',
    'If the beneficiaries have fused their identity with the technological-determinism narrative, the constraint''s suppression (how completely alternatives are foreclosed) is higher than the raw metrics suggest—the suppression is partly internalized. This would shift directionality for the beneficiary seats and might trigger FSM or reclassification toward snare (suppression of the beneficiaries'' own agency narratives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_fusion_ambiguity, empirical, 'Internalized suppression: do beneficiaries suppress their own agency narrative by accepting technological determinism?').

omega_variable(
    kernel_reading_contest_framing,
    'This constraint instantiates the ''technological_determinism'' reading of the ''press_reformation_causation'' kernel. The sibling readings (strategic_deployment, mutual_shaping) contest whether causality runs technology-to-society (this reading) or society-to-technology (strategic) or bidirectionally (mutual). Can all three readings coexist as live historiographical positions, or does accepting determinism logically foreclose the siblings?',
    'Historiographical assessment: (1) Can a single historian defend both determinism AND strategic deployment (showing when each operated)? (2) Are the readings incommensurable at the foundational level (determinist axiom vs. agency axiom) or merely emphasizing different causal directions in a multifactorial process?',
    'If determinism forecloses the siblings logically, the reading_relations should be ''forecloses'' rather than ''coexists_with''. If determinism and strategic deployment are compatible readings of different parts of the causal process (early print disruption was determined; later deployment was strategic), then coexistence is correct. The omega documents the foundational disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Whether the deterministic reading logically forecloses or coexists with sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1440, press_reformation_causation__technological_determinism, theater_ratio, 1440, 0.01).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causation__technological_determinism, theater_ratio, 1470, 0.02).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__technological_determinism, theater_ratio, 1500, 0.04).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causation__technological_determinism, theater_ratio, 1525, 0.05).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.05).

% Extraction over time
narrative_ontology:measurement(pres_be_t1440, press_reformation_causation__technological_determinism, base_extractiveness, 1440, 0.02).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causation__technological_determinism, base_extractiveness, 1470, 0.04).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__technological_determinism, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causation__technological_determinism, base_extractiveness, 1525, 0.15).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1440, press_reformation_causation__technological_determinism, suppression_requirement, 1440, 0.02).
narrative_ontology:measurement(pres_su_t1470, press_reformation_causation__technological_determinism, suppression_requirement, 1470, 0.03).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.06).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causation__technological_determinism, suppression_requirement, 1525, 0.08).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, global_infrastructure).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__technological_determinism, 0.08).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% The press_reformation_causation kernel has three constraint stories, one per reading. This story (technological_determinism) treats printing as exogenous technological determinant. The sibling story strategic_deployment treats reformers' and printers' purposeful deployment as the driver. The third sibling mutual_shaping treats technology and agency as co-constitutive. All three share the same historical evidence but assign causality differently. The network links them because each reading influences the others—accepting determinism makes strategic deployment and mutual shaping weaker, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
