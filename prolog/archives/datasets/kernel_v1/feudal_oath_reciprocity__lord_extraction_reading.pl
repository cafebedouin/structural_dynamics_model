% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__lord_extraction_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath as Extraction Mechanism: Maximal Obligation Bounded by Rebellion Threshold
 *   domain: medieval_political_economy/institutional_analysis
 *
 * SUMMARY:
 *   The feudal oath represents a contested kernel in medieval political
 *   economy: what is the oath's fundamental purpose, and who benefits from
 *   its structure? This constraint story instantiates the 'lord extraction'
 *   reading — the oath is analyzed as a mechanism that authorizes maximal
 *   extraction from vassals, bounded only by the rebellion threshold. From
 *   the lord's perspective, the oath solves a critical problem: how to
 *   extract resources, military service, and labor without maintaining a
 *   permanent standing force or paying market-rate compensation. The oath
 *   provides legitimacy (it is not arbitrary taking but contractual
 *   obligation witnessed by God and Church), predictability (vassals know
 *   their duties and consequences), and scalability (the lord multiplies his
 *   effective power through oath-bound hierarchy). Extraction is bounded
 *   pragmatically — if demands exceed rebellion threshold, the oath network
 *   collapses and the lord loses his power base. This constraint exhibits all
 *   six types from different observational positions: the powerless vassal
 *   experiences snare; the institutional lord experiences rope; the organized
 *   coalition experiences scaffold; the analytical observer risks
 *   naturalizing it as mountain. The theater ratio declines over the interval
 *   as the oath's religious theater diminishes and the secular extraction
 *   mechanism becomes explicit. Suppression requirement increases over the
 *   interval as extraction intensifies and resistance must be actively
 *   suppressed.
 *
 * KEY AGENTS:
 *   - Lord Class: Primary beneficiary (institutional/arbitrage) — extracts military service, feudal dues, scutage, knight service obligations, and judges disputes with authority. Has arbitrage options: can seek ecclesiastical sanction, escalate enforcement, or (at extremes) face collective rebellion.
 *   - Vassal Population: Primary victim (powerless/trapped) — bound by oath, obligated to provide service, dues, and labor. No exit except rebellion (collective action), which carries severe punishment. Land tenure and family survival depend on oath compliance.
 *   - Ecclesiastical Authority: Secondary beneficiary (institutional/arbitrage) — validates oath legitimacy through religious ceremony, threat of excommunication, and cosmic sanction. Maintains enforcement leverage and institutional prestige. Authority erodes over time as secular validation becomes sufficient.
 *   - Sub-Vassal Knights: Secondary victim/moderate beneficiary (moderate/constrained) — receive protection and dispute adjudication (coordination benefit) but also bound by extraction obligations to their lord. Can theoretically seek different lord but constrained by interconnected oath network.
 *   - Peasant Commons: Tertiary victim (powerless/trapped) — suffer indirect extraction through vassal obligations that disrupt agricultural cycles, serf labor requirements, and feudal dues that reduce peasant income.
 *   - Rebel Coalition: Organized actor (organized/constrained) — temporary coalitions (Great Barons, Peasant Revolts) treat the oath as reconfigurable through organized violence. See the constraint as temporary and structurally breakable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.68).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.72).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Extraction Mechanism: Maximal Obligation Bounded by Rebellion Threshold").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, 'd20aa18c-b9e3-456c-bc29-276d459daaa1').
narrative_ontology:cs_kernel_codification('d20aa18c-b9e3-456c-bc29-276d459daaa1', formalized).
narrative_ontology:cs_authority_grounding('d20aa18c-b9e3-456c-bc29-276d459daaa1', extraction).
narrative_ontology:cs_interpretation_layer_present('d20aa18c-b9e3-456c-bc29-276d459daaa1').
narrative_ontology:cs_reading_relation('d20aa18c-b9e3-456c-bc29-276d459daaa1', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('d20aa18c-b9e3-456c-bc29-276d459daaa1', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('d20aa18c-b9e3-456c-bc29-276d459daaa1', foundational, oath_maximizes_lord_extraction).
narrative_ontology:cs_axiom_status(oath_maximizes_lord_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d20aa18c-b9e3-456c-bc29-276d459daaa1', oath_maximizes_lord_extraction, empirically_contingent).
narrative_ontology:cs_axiom('d20aa18c-b9e3-456c-bc29-276d459daaa1', secondary, extraction_legitimacy_requires_cosmic_sanction).
narrative_ontology:cs_axiom_status(extraction_legitimacy_requires_cosmic_sanction, overridden).
narrative_ontology:cs_axiom_grounding('d20aa18c-b9e3-456c-bc29-276d459daaa1', extraction_legitimacy_requires_cosmic_sanction, theological).
narrative_ontology:cs_reference_frame('d20aa18c-b9e3-456c-bc29-276d459daaa1', feudal_obligation_hierarchy).
narrative_ontology:cs_drift_state('d20aa18c-b9e3-456c-bc29-276d459daaa1', post_charter_reformation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d20aa18c-b9e3-456c-bc29-276d459daaa1', '2026-02-27T00:00:00Z').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, lord_class).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authority).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassal_population).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, peasant_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOUND VASSAL (SNARE) — The oath is experienced as maximal extraction. Military service obligations, feudal dues (knight service, scutage, aids), labor requirements, and justice submission are non-negotiable. Exit is structural impossibility — land tenure, family survival, and legal status all depend on oath compliance. Rebellion carries collective punishment. The vassal perceives the oath as a trap from which there is no escape.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__lord_extraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EXTRACTING LORD (ROPE) — The oath is experienced as a coordination mechanism that solves the lord's core problem: how to extract and deploy resources without maintaining a permanent standing force. The oath provides legitimacy for extraction (it is not arbitrary taking but contractual obligation), predictability (vassals know their duties), and scalability (the lord can multiply his effective power through oath-bound hierarchy). The lord has options — he can arbitrage between vassal cohorts, seek ecclesiastical sanction, or escalate enforcement through warfare. Exit is available but costly. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__lord_extraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: SUB-VASSAL KNIGHT (TANGLED ROPE) — The oath provides genuine coordination benefit: protection, dispute adjudication, and military support in warfare. But it also extracts through service obligations and the subordination of the knight's own autonomous power. The sub-vassal can theoretically seek a different lord (exit is available but carries land loss and political isolation) but is constrained by the interconnected oath network. This perspective sees both the coordination value and the asymmetric extraction that rides on it.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__lord_extraction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ECCLESIASTICAL MEDIATOR (PITON) — The Church's role in validating oaths (religious sanction, excommunication threat) has degraded by the 12th–14th centuries as secular authority increasingly validates oaths independently. The Church's enforcement mechanism persists through institutional inertia — oath ceremonies retain religious theater — but the Church's actual leverage over oath compliance has eroded. The ecclesiastical authority maintains the appearance of validating the oath's legitimacy while losing functional control over its enforcement.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__lord_extraction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: REBEL COALITION (SCAFFOLD) — Organized vassal resistance (Great Barons' Revolt, peasant uprisings) treats the oath's extraction mechanism as a temporary problem to be broken rather than an immutable law. The Magna Carta reads from this perspective: the oath is reconfigurable, its extraction can be bounded, and organized collective action (temporary, crisis-driven) can force a renegotiation. The scaffold classification reflects that organized agents see the constraint as structurally temporary and within their power to alter, though at high collective cost.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__lord_extraction_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: NATURAL LAW THEORIST (MOUNTAIN) — The medieval natural law tradition (Gratian, Aquinas) treats feudal obligation as deriving from natural law: authority flows from God, oaths reflect natural order, extraction is legitimate because hierarchical subordination is natural. From this perspective, the oath is immutable — challenging it challenges the cosmic order itself. However, this reading naturalizes what this constraint story reveals as a contingent institutional arrangement: the oath's extraction is bounded by rebellion threshold, which means it is structurally breakable, not naturally immutable.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__lord_extraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(feudal_oath_reciprocity__lord_extraction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feudal_oath_reciprocity__lord_extraction_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, TR),
    TR >= 0.70.

:- end_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The oath authorizes the lord to demand military service, feudal dues (paid in kind or labor), scutage (military fee), aids (feudal levies), and justice submission with minimal compensation. The vassal must provide these regardless of personal capacity or consent — the oath removes the negotiating power of the subject. The extraction is limited only by rebellion threshold, not by law or market mechanism. Rising from 0.52 to 0.68 over the interval reflects intensification of extraction as lords press vassals closer to rebellion threshold. Suppression (0.72): High and rising. The oath's enforcement mechanism is coercive: military force, confiscation of lands, collective punishment (destruction of holdings, outlawry of families), and excommunication. As extraction intensifies, suppression machinery must be maintained and escalated to prevent rebellion. The Church's coercive leverage (excommunication, damnation rhetoric) supplements secular force. Theater ratio (0.55, declining): Moderate. The oath ceremony involves religious theater (sworn before God, ecclesiastical witness, ritual language) but the extraction mechanism is explicit — the obligations are clearly stated and enforced through material consequence, not mystification. Theater declines over the interval as secular authority becomes confident enough to enforce without religious ceremony.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces maximal perspectival divergence. The lord sees coordination (rope) — solving the problem of resource extraction and delegation. The vassal sees entrapment (snare) — a mechanism that extracts without consent and leaves no escape. The sub-vassal sees mixed benefit (tangled rope) — receiving protection but losing autonomy. The Church sees declining relevance (piton) — maintaining the appearance of validating oaths while losing actual control. The rebel coalition sees a reconfigurable temporary problem (scaffold) — the oath can be broken and renegotiated through organized action. The natural law theorist risks seeing immutability (mountain) — treating the oath as reflecting cosmic order rather than recognizing its structural reversibility.
 *
 * DIRECTIONALITY LOGIC:
 *   The extraction flow is directional: from vassal to lord, mediated by oath obligation. The lord's institutional power (arbitrage exit options) gives him low d — he experiences the oath as beneficial coordination. The vassal's powerlessness (trapped exit) gives him high d — he experiences the oath as extraction. The sub-vassal's moderate power and constrained exit produce moderate d. The analytical observer (analytical exit, civilizational horizon) derives d from the structure's overall directionality without being embedded in it. The oscillation in the measurements reflects the dynamic: as extraction intensifies (base_extractiveness rises), suppression requirement rises to maintain compliance, but theater ratio falls because the extraction becomes explicit and no longer requires as much legitimacy theater.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading instantiates one of three competing readings of the feudal oath kernel. The mandatrophy is not resolved by choosing 'the' right type — it is resolved by recognizing that three coherent constraint stories exist with different ε values, beneficiary/victim sets, and perspectival structures. The 'lord extraction' reading (this story) models the oath as snare (ε=0.68). The 'vassal coordination' reading would model the oath from the vassal collective's perspective as rope (lower ε, genuine coordination benefit). The 'ecclesiastical mediation' reading would model the Church's role as institutional authority mediating the oath's legitimacy. Each reading is internally consistent; their disagreement is not empirical but structural-interpretive. The three stories together form a presheaf that captures the multivalent reality of feudal oath-taking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_bounded_by_rebellion_threshold,
    'Is the lord''s extraction truly bounded by vassal rebellion capacity, or does the rebellion threshold itself shift as a function of extraction intensity?',
    'Historical analysis of oath modifications post-rebellion: are extraction obligations reduced after major revolts? Comparison of extraction levels and rebellion frequency across regions and centuries.',
    'If threshold is fixed: extraction has a structural ceiling (snare with a natural limit). If threshold shifts with extraction: the constraint is dynamic and potentially unsustainable (→ transition to different constraint type as extraction approaches critical density).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_bounded_by_rebellion_threshold, empirical, 'Whether rebellion threshold is fixed or dynamic relative to extraction intensity').

omega_variable(
    ecclesiastical_validation_necessity,
    'Is ecclesiastical sanction necessary for oath legitimacy, or would the oath structure persist as pure secular power even without Church validation?',
    'Counterfactual analysis of oath-breaking consequences post-Reformation; comparison of oath compliance rates in regions with strong vs weak ecclesiastical authority; analysis of secular oaths that lack religious sanction.',
    'If necessary: the Church is a structural co-beneficiary (ecclesiastical_authority in victim set is incorrect). If contingent: Church validation is a legitimacy theater layer that could be stripped without changing the extraction mechanism fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_validation_necessity, empirical, 'Whether ecclesiastical sanction is necessary or contingent for oath legitimacy').

omega_variable(
    coordination_function_separability,
    'Can the coordination benefits of the oath (protection, dispute resolution) be decoupled from the extraction mechanism (military service, feudal dues), or are they structurally fused?',
    'Historical comparison of oath modifications that reduced extraction without removing protection/adjudication (e.g., Magna Carta variants); analysis of alternative coordination mechanisms (merchant guilds, city charters, religious confraternities) that provide similar benefits without oath-based extraction.',
    'If separable: the lord''s rope perspective is contingent, not necessary (→ constraint could be redesigned as pure coordination). If fused: the tangled rope classification is structural (coordination and extraction cannot be unbound without destroying both).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_separability, empirical, 'Whether oath''s coordination and extraction functions are separable').

omega_variable(
    reading_versus_sibling_commissioning,
    'This is the ''lord extraction'' reading of feudal oath reciprocity. What structural premises distinguish it from the ''vassal coordination'' and ''ecclesiastical mediation'' readings?',
    'Explicit definition of the competing readings'' core claims (see cs_structure.axioms and reading_relations). The sibling readings will inhabit their own constraint stories with their own ε values and perspectival structures. This omega documents that the three readings are structurally distinct constraints, not observational variants of one constraint.',
    'Authoring discipline: this constraint models the lord''s extraction-centric reading exclusively. It does NOT average or hedge ε across sibling readings. It does NOT describe the other readings'' axioms or classify their merits. The network relationship (affects_constraints) links to the sibling stories, which will contain their own contradiction and resolution logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_sibling_commissioning, conceptual, 'Kernel reading decomposition: this story instantiates the lord extraction reading only').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(feudal_tr_t3, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement(feudal_tr_t6, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(feudal_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(feudal_be_t3, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(feudal_be_t6, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(feudal_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(feudal_su_t3, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(feudal_su_t6, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% Feudal oath reciprocity is a contested kernel with three structurally distinct readings: (1) lord_extraction_reading (this story, ε=0.68, snare focus); (2) vassal_coordination_reading (ε=0.35–0.45, rope/tangled_rope, coordination focus); (3) ecclesiastical_mediation_reading (ε=0.42–0.58, tangled_rope, Church authority focus). The three readings are NOT observational variants of one constraint. They are distinct constraints grounded in different interpretive commitments about what the oath's kernel purpose is. Each reading has its own beneficiary/victim set, its own perspectives, and its own perspectival gap structure. They are linked by network relations (affects_constraints) to indicate that changes to one reading's institutional arrangement cascade to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
