% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__precondition_convergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__precondition_convergence, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: press_reformation_causality__precondition_convergence
 *   human_readable: Printing Press as Conditional Enabler of Reformation (Precondition Convergence Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint instantiates the precondition_convergence reading of the
 *   press-reformation kernel. The core claim is that printing technology was
 *   a necessary but insufficient condition for the Protestant Reformation;
 *   the reformation required simultaneous alignment of three independent
 *   structural preconditions: (1) weakened papal authority (political
 *   fragmentation of Christendom, Great Schism aftermath, competing power
 *   centers), (2) standardization of vernacular languages (Dante, humanist
 *   philology, emerging print norms), and (3) urban literacy growth (merchant
 *   education, rising merchant power, craft guild literacy requirements). The
 *   technology alone did not 'cause' the reformation; rather, the printing
 *   press became a scaffold — a temporary coordination mechanism that enabled
 *   the reformation to achieve critical mass and consistency across
 *   geographically dispersed groups — but only because the preconditions had
 *   already begun to converge. The Korean and Chinese printing counterfactual
 *   is central to this reading: printing technology existed in these contexts
 *   centuries before Gutenberg, yet no reformation-equivalent occurred. The
 *   structural differences (centralized imperial authority preventing
 *   religious institutional autonomy, established state monopoly on literacy,
 *   integration of religion into state apparatus) made printing a tool of
 *   state standardization rather than a mechanism for theological
 *   decentralization. The measurements show rising extractiveness as the
 *   institutional church (Catholic authority) begins using printing to
 *   enforce doctrinal consistency against Protestant distribution, and rising
 *   suppression requirements as the constraint shifts from enabling to
 *   coercive. Theater ratio remains relatively low (0.48 at endpoint) because
 *   the constraint's primary function is coordination (enabling distributed
 *   theology networks) rather than performative ritual.
 *
 * KEY AGENTS:
 *   - Reformation Theology Network (powerful/mobile/regional): Distributed religious actors benefiting from press-enabled theological consistency and rapid text distribution. Experience the technology as enabling coordination, not extractive.
 *   - Catholic Church Authority (institutional/constrained/regional): Institutional beneficiary of printing for doctrinal standardization, but faces extraction risk as Protestant distribution erodes monopoly on interpretation. Enforces suppression to maintain coordination function.
 *   - Printers and Printing Merchants (institutional/arbitrage/regional): Economic beneficiaries of the technology itself. Low extractiveness from their perspective — they coordinate profitable exchange without enforcing theological alignment.
 *   - Rural Peasantry and Illiterate Populations (powerless/trapped/local): Bearers of the extraction cost. Printing enables institutional enforcement of standardized doctrine they cannot access or contest. No literacy, no exit.
 *   - Technological Inevitability View (analytical/analytical/global): False summit perspective that naturalizes printing as an agent with causal force. The precondition_convergence reading identifies this as false by highlighting that the same technology in East Asia produced different outcomes.
 *   - Print-as-Progress Historiography (institutional/constrained/regional): Institutional narrative that has degraded into performative assignment of causal weight to the press. Persists through educational inertia despite analytical scrutiny revealing it as oversimplified.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__precondition_convergence, 0.35).
domain_priors:suppression_score(press_reformation_causality__precondition_convergence, 0.42).
domain_priors:theater_ratio(press_reformation_causality__precondition_convergence, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__precondition_convergence, extractiveness, 0.35).
narrative_ontology:constraint_metric(press_reformation_causality__precondition_convergence, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(press_reformation_causality__precondition_convergence, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__precondition_convergence, scaffold).
narrative_ontology:human_readable(press_reformation_causality__precondition_convergence, "Printing Press as Conditional Enabler of Reformation (Precondition Convergence Reading)").
narrative_ontology:topic_domain(press_reformation_causality__precondition_convergence, "history_of_technology/religious_history/media_studies").

narrative_ontology:has_sunset_clause(press_reformation_causality__precondition_convergence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__precondition_convergence, '78fb2409-7618-4f29-9ec3-f573c2373fd1').
narrative_ontology:cs_kernel_codification('78fb2409-7618-4f29-9ec3-f573c2373fd1', distributed).
narrative_ontology:cs_authority_grounding('78fb2409-7618-4f29-9ec3-f573c2373fd1', distributed).
narrative_ontology:cs_reading_relation('78fb2409-7618-4f29-9ec3-f573c2373fd1', press_reformation_causality__technological_inevitability, coexists_with).
narrative_ontology:cs_reading_relation('78fb2409-7618-4f29-9ec3-f573c2373fd1', press_reformation_causality__beneficiary_deployment, coexists_with).
narrative_ontology:cs_axiom('78fb2409-7618-4f29-9ec3-f573c2373fd1', foundational, technology_requires_structural_preconditions).
narrative_ontology:cs_axiom_status(technology_requires_structural_preconditions, holdable).
narrative_ontology:cs_axiom_grounding('78fb2409-7618-4f29-9ec3-f573c2373fd1', technology_requires_structural_preconditions, empirically_contingent).
narrative_ontology:cs_axiom('78fb2409-7618-4f29-9ec3-f573c2373fd1', secondary, preconditions_are_structurally_independent).
narrative_ontology:cs_axiom_status(preconditions_are_structurally_independent, holdable).
narrative_ontology:cs_axiom_grounding('78fb2409-7618-4f29-9ec3-f573c2373fd1', preconditions_are_structurally_independent, empirically_contingent).
narrative_ontology:cs_reference_frame('78fb2409-7618-4f29-9ec3-f573c2373fd1', printing_as_inert_technology).
narrative_ontology:cs_drift_state('78fb2409-7618-4f29-9ec3-f573c2373fd1', contemporary_history_of_technology_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('78fb2409-7618-4f29-9ec3-f573c2373fd1', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__precondition_convergence, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__precondition_convergence, reformation_theology_network).
narrative_ontology:constraint_beneficiary(press_reformation_causality__precondition_convergence, vernacular_language_standardization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REFORMATION THEOLOGY NETWORK (SCAFFOLD) — The press enables rapid text distribution and theological consistency across dispersed groups, but only because other preconditions (weakened papal authority, vernacular standardization, urban literacy clusters) converge in 16th-century Western Europe. The technology is necessary but not sufficient; when preconditions align, the press becomes a temporary coordination mechanism with a sunset (once theological consensus solidifies or state suppression hardens, the mobile printing infrastructure becomes entrenched). The network experiences the constraint as enabling, not extractive.
constraint_indexing:constraint_classification(press_reformation_causality__precondition_convergence, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: CATHOLIC CHURCH AUTHORITY (TANGLED ROPE) — The institutional church benefits from the same printing infrastructure for doctrinal dissemination and standardization but faces extraction risk: once printing enables distributed Protestant theology, the church's monopoly on theological interpretation erodes. Suppression attempts (censorship, index, control of presses) become necessary to maintain coordination function. The constraint shows both coordination (standardized doctrine) and extraction (suppression of heretical texts) simultaneously.
constraint_indexing:constraint_classification(press_reformation_causality__precondition_convergence, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRINTERS AND PRINTING MERCHANTS (ROPE) — Economic actors experience the press as a pure coordination mechanism: standardized type, shared technical knowledge, guild organization, and market demand for printed texts enable mutually beneficial exchange. Printers have arbitrage options (relocate to different regions, pivot to secular printing). Low extractiveness — the merchant profits from coordination, not from suppression. Technology enables their livelihood without requiring them to enforce theological alignment.
constraint_indexing:constraint_classification(press_reformation_causality__precondition_convergence, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: RURAL PEASANTRY AND ILLITERATE POPULATIONS (SNARE) — For the vast majority of the European population, the printing press produces no direct benefit and creates new extraction mechanisms. Oral transmission of theology and folk practice (which existed before printing) continue; printed theology reaches them only through mediated authority structures. Printing standardizes doctrine in a form they cannot access, enabling institutional enforcement of orthodoxy through mechanisms they cannot contest. No exit option from theological suppression; no literacy to claim alternative readings. The technology makes extraction more efficient without providing coordination benefit.
constraint_indexing:constraint_classification(press_reformation_causality__precondition_convergence, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: TECHNOLOGICAL INEVITABILITY VIEW — The false summit perspective naturalizes the reformation as an inevitable consequence of printing technology itself, as if the technology possessed agency. This view claims the press 'caused' reformation as a natural law — printing → standardized texts → theological decentralization → reformation (inexorable). The analytical observer from the precondition_convergence reading identifies this as a false summit: the technology is inert without the contextual preconditions (weakened papal authority, vernacular standardization, urban literacy). The same press technology existed in Korea and China centuries earlier but produced no comparable reformation; the difference is structural (political fragmentation vs. centralized imperial authority; established vernacular literacy vs. elite script monopoly), not inherent to the technology. Engine will detect false summit via beneficiary presence + structural data.
constraint_indexing:constraint_classification(press_reformation_causality__precondition_convergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: PRINT-AS-PROGRESS NARRATIVE (PITON) — The historiographical convention that positions 'the printing press' as the pivotal cause of reformation has become inertial and performative. Historians continue to assign causal weight to the technology because the narrative is established in textbooks and institutional curricula, not because it withstands structural scrutiny. The theater ratio reflects that much historical writing about the print-reformation link performs causality rather than establishing it rigorously. The constraint (the historiographical causal claim) persists through institutional teaching momentum despite degradation of its explanatory power once preconditions are analyzed.
constraint_indexing:constraint_classification(press_reformation_causality__precondition_convergence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__precondition_convergence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(press_reformation_causality__precondition_convergence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(press_reformation_causality__precondition_convergence, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(press_reformation_causality__precondition_convergence, TR),
    TR >= 0.70.

:- end_tests(press_reformation_causality__precondition_convergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The press does not extract in the manner of a snare (no malicious suppression at origin; the technology is genuinely enabling for reformers). But as the constraint becomes enforced (Catholic suppression, censorship index), extractiveness rises to 0.35 by the endpoint. The moderate value reflects that the technology itself is neutral; extraction emerges when institutional actors (Catholic Church) use printing to enforce suppression. Suppression (0.42): Moderate-high. The preconditions include institutional barriers (Catholic authority actively censoring printed works, maintaining manuscript-only control in some regions), but suppression is not total (Protestant networks find ways to print clandestinely, distribute across borders). Theatrical ratio (0.48): Moderate-low. The primary function is genuine coordination (enabling theological networks to maintain consistency and distribute rapidly), not ritual performance. The theater component emerges as institutional actors use the technology performatively to demonstrate orthodoxy enforcement. Claimed type (scaffold): The press is a temporary coordination mechanism with a natural sunset — once reformation theology solidifies into institutional churches (Lutheran, Reformed, Anglican), the coordination function of the printing press in enabling rapid theological development ends. The technology persists but the constraint dissolves.
 *
 * PERSPECTIVAL GAP:
 *   The perspective set demonstrates how the same technology can appear as enabling (scaffold), coordinating (rope), mixed coordination-and-extraction (tangled rope), extractive (snare), and as a naturalized false law (mountain). The theology network sees scaffolding and coordination; the Catholic Church sees both coordination benefits and extraction risks; merchants see pure coordination and profit; the powerless see pure extraction; the false-summit perspective naturalizes the technology as causal. The perspectival gap reveals that the 'causal power' of the printing press is not intrinsic to the technology but emerges from how different agents with different structural positions experience and deploy it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim declarations and power-exit combinations. The reformation theology network benefits from printing (low d) and has mobile exit options (further reducing effective extraction experience). The Catholic Church is a nominal beneficiary (printing enables doctrinal standardization) but becomes a victim of erosion (high d) when faced with Protestant distribution — this dual position and constrained exit (cannot simply abandon printing) produces the tangled-rope experience. Printers experience low directionality (beneficiaries with arbitrage options) producing rope classification. Illiterate peasants experience maximum directionality (victims with trapped exit) producing snare classification. The false-summit perspective's directionality is computed as analytical observer (canonical d ≈ 0.73) but the structural data (beneficiary presence, measurable extraction asymmetry) triggers false-summit detection in the engine.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that printing is a scaffold — a temporary coordination mechanism with a natural sunset. The constraint is not 'is printing coordinate or extractive?' (which would collapse perspectives) but 'how do different structural positions experience printing's enabling role?' The theology network experiences scaffolding (temporary, enabling, with natural sunset). The Catholic Church experiences tangled coordination-and-extraction (printing enables doctrinal standardization while threatening monopoly on interpretation). Merchants experience pure coordination (rope). The powerless experience extraction (snare). The false-summit perspective risks naturalizing contingency as law. The mandatrophy is dissolved by the precondition analysis: the technology's apparent causal force is an artifact of the structural convergence of three independent factors. In contexts where preconditions do not align (imperial centralization, monopolized literacy, state-integrated religion), the same technology produces different outcomes. The classification diversity across perspectives is not a problem to be solved but a diagnostic signal that the constraint's classification depends fundamentally on structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    korean_chinese_printing_counterfactual,
    'Why did printing technology in Korea and China (centuries before Gutenberg) not trigger reformation-equivalent religious upheaval or theological decentralization?',
    'Comparative historical analysis of structural differences: (a) political authority (centralized imperial vs. fragmented Christendom), (b) pre-existing vernacular literacy (established in China/Korea vs. monopolized by clergy/elite in Western Europe), (c) religious institutional structure (state-integrated vs. autonomous papal hierarchy), (d) availability of alternative printing media for authority (state control of printing in imperial systems vs. contestation in fragmented Europe). Identify which structural factors are sufficient to suppress reformation-like outcomes despite printing availability.',
    'If imperial centralization alone suffices to suppress reformation regardless of printing: technology is neither necessary nor sufficient; the convergence of multiple preconditions is the actual causal engine. If vernacular literacy monopoly is the gate: technology triggers outcomes only when literacy access itself becomes contested. The precondition_convergence reading claims all three factors (weakened authority, standardized vernacular, urban literacy) must converge; counterfactual resolution tests which combination is actually necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(korean_chinese_printing_counterfactual, empirical, 'Why printing in East Asia did not produce reformation-equivalent outcomes').

omega_variable(
    precondition_independence_assumption,
    'Are the three claimed preconditions (weakened papal authority, vernacular standardization, urban literacy growth) structurally independent or causally entangled? Does one precondition cause or enable the others?',
    'Temporal sequence analysis: which precondition emerged first in different European regions? Causal pathway tracing: does papal authority weaken *because of* literacy growth (people reading alternative theology), or *independently* due to political/economic factors (Great Schism, councils, political fragmentation)? Does vernacular standardization *result from* printing, or does printing exploit pre-existing standardization (Dante''s linguistic argument, vernacular liturgy movements predating press)? Identify whether preconditions are truly independent convergences or whether one drives the others.',
    'If preconditions are independent: they converge by structural accident, and the reformation is contingent on alignment (precondition_convergence reading holds). If causally entangled: one precondition is foundational and others are downstream consequences; the model simplifies to a single causal thread. If printing itself *causes* vernacular standardization: the technology is more generative than the scaffold reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precondition_independence_assumption, empirical, 'Whether preconditions are structurally independent or causally entangled').

omega_variable(
    oral_transmission_efficacy_parity,
    'Could reformation-equivalent theological movements have spread without printing via oral transmission and manuscript networks alone, given the time investment required? Is printing a necessary acceleration mechanism or an inessential convenience?',
    'Historical case analysis of pre-printing heretical movements (Cathars, Waldensians, Lollards, Hussites) and their replication rates vs. post-printing Reformation spread. Measure: geographic range of movement adherents, time to reach critical mass, resistance to institutional suppression, ability to maintain theological consistency across distributed groups without printed texts. Model transmission speed under oral+manuscript constraints vs. printing constraints. Identify the minimum speed required for a movement to escape suppression.',
    'If oral transmission networks could have achieved Reformation scale given sufficient time: printing is an acceleration mechanism but not a necessary condition. The precondition_convergence reading would shift emphasis to temporal factors (how long can a movement persist under suppression before institutional response hardens?). If printing provides qualitatively different transmission properties: the technology becomes more fundamental to the outcome than the scaffold reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_transmission_efficacy_parity, empirical, 'Whether printing is necessary for reformation speed or merely accelerates inevitably-spreading theology').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is one reading of a contested kernel: does printing technology ''cause'' reformation because of inherent properties of the technology, or does reformation result from a contingent convergence of independent structural preconditions that the technology merely facilitates?',
    'Comparative structural analysis across the three kernel readings: technological_inevitability reading claims causal force flows from the technology itself (intrinsic properties of standardization, replicability); beneficiary_deployment reading claims strategic actors (reformers, political powers) deliberately weaponize the technology to advance interests (causal force flows from intentional deployment, not technology); precondition_convergence reading claims technology is necessary but inert without alignment of political authority, literacy, and language standardization (causal force flows from structural convergence, not technology or agency alone). The three readings produce different causal mechanisms and different counterfactuals. Historical resolution requires isolating which reading''s causal model explains the observable outcomes.',
    'If technological_inevitability holds: the press ''causes'' reformation in any context where basic literacy exists. If beneficiary_deployment holds: the press is a tool deployed by agents with strategic interests; reformation occurs where political fragmentation and competing power centers exist. If precondition_convergence holds: reformation is structurally contingent on alignment of multiple independent factors; the East Asian counterfactual becomes diagnostic (printing existed but preconditions did not align, so no reformation). The three readings forecast different outcomes for future technologies in different contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the printing-reformation kernel correctly models causal mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__precondition_convergence, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_ref_tr_t1450, press_reformation_causality__precondition_convergence, theater_ratio, 1450, 0.3).
narrative_ontology:measurement(press_ref_tr_t1500, press_reformation_causality__precondition_convergence, theater_ratio, 1500, 0.4).
narrative_ontology:measurement(press_ref_tr_t1550, press_reformation_causality__precondition_convergence, theater_ratio, 1550, 0.48).

% Extraction over time
narrative_ontology:measurement(press_ref_be_t1450, press_reformation_causality__precondition_convergence, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(press_ref_be_t1500, press_reformation_causality__precondition_convergence, base_extractiveness, 1500, 0.2).
narrative_ontology:measurement(press_ref_be_t1550, press_reformation_causality__precondition_convergence, base_extractiveness, 1550, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(press_ref_su_t1450, press_reformation_causality__precondition_convergence, suppression_requirement, 1450, 0.15).
narrative_ontology:measurement(press_ref_su_t1500, press_reformation_causality__precondition_convergence, suppression_requirement, 1500, 0.32).
narrative_ontology:measurement(press_ref_su_t1550, press_reformation_causality__precondition_convergence, suppression_requirement, 1550, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__precondition_convergence, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__precondition_convergence, press_reformation_causality__technological_inevitability).
narrative_ontology:affects_constraint(press_reformation_causality__precondition_convergence, press_reformation_causality__beneficiary_deployment).

% DUAL FORMULATION NOTE:
% The press-reformation kernel decomposes into three constraint stories corresponding to three readings: precondition_convergence (this story), technological_inevitability (printing causes reformation via intrinsic technology properties), and beneficiary_deployment (agents weaponize the technology to advance interests). Each reading produces a different causal model and different ε values. Precondition_convergence models printing as scaffold (ε=0.35, technology necessary but insufficient). Technological_inevitability models printing as inherent causal force (higher ε, false-summit candidate). Beneficiary_deployment models printing as strategic tool (ε varies by agent position and deployment success). The three stories are linked via network.affects_constraints to enable comparative analysis of which reading's causal mechanism explains observable outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
