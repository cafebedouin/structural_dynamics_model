% ============================================================================
% CONSTRAINT STORY: parliamentary_supremacy_statutes__acts_of_union
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parliamentary_supremacy_statutes__acts_of_union, []).

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
 *   constraint_id: parliamentary_supremacy_statutes__acts_of_union
 *   human_readable: Acts of Union: Statutory Merger of Parliaments
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Acts of Union (1707 Scotland, 1801 Ireland) are watershed moments in
 *   British constitutional history where Parliament enacted reciprocal
 *   legislation merging the separate parliaments of Scotland and Ireland into
 *   a single Westminster Parliament. This constraint exemplifies how
 *   statutory codification of constitutional authority creates an enduring
 *   structural relationship between a metropolitan legislature and peripheral
 *   institutional autonomy. The Acts did not create Parliament's supremacy —
 *   that claim rests on deeper common-law doctrine — but rather demonstrated
 *   it: Parliament exercised the power to remake the very political subject
 *   by statute, abolishing the constituent legislatures and reimposing
 *   governance from Westminster. The constraint's extractiveness has declined
 *   over three centuries (0.72 at union → 0.42 by Scottish devolution in
 *   1999) as peripheral actors have demanded and achieved delegated
 *   governance. Yet suppression remains embedded in the framework: the
 *   statutory Acts can only be repealed by Parliament, and the devolved
 *   assemblies operate under delegated authority that Parliament retains the
 *   right to revoke. The theater ratio has risen sharply (0.15 → 0.68) as the
 *   union's functional coordination content has atrophied and its remaining
 *   force is primarily constitutional doctrine — the performative reassertion
 *   of parliamentary supremacy rather than the management of actual
 *   governance.
 *
 * KEY AGENTS:
 *   - Westminster Parliament: Primary beneficiary (institutional/arbitrage) — exercises statutory authority to merge and govern the constituent kingdoms; experiences the constraint as supreme lawmaking power
 *   - Metropolitan Commerce and Capital: Beneficiary coalition (institutional/arbitrage) — unified markets, common regulatory framework, imperial trade privileges; experiences coordination benefit without extraction cost
 *   - Scottish and Irish Separate Parliaments (pre-union): Primary victim (powerful/trapped pre-union; powerless/trapped post-union) — their institutional autonomy is statutorily abolished with no legal right of exit; suppression is absolute
 *   - Scottish and Irish Regional Representation (post-union): Constrained victim (moderate/constrained) — minority representation within Westminster Parliament; benefit from unified market coordination but subordinated to metropolitan majorities
 *   - Unionist Factions (Scotland and Ireland): Mixed beneficiary-victim (organized/constrained) — benefit from imperial commerce and unified governance but accept suppression of separate national legislatures
 *   - Devolved Assemblies (post-1999): Delegated agents (moderate/constrained) — operate as Westminster delegates with functional autonomy in defined domains; suppression remains in legal framework (Parliament retains revocation right)
 *   - The Constitutional Order: Institutional carrier (institutional/arbitrage) — maintains the Acts as foundational law; experiences the constraint as doctrine requiring performative reassertion as actual functional content declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parliamentary_supremacy_statutes__acts_of_union, 0.58).
domain_priors:suppression_score(parliamentary_supremacy_statutes__acts_of_union, 0.72).
domain_priors:theater_ratio(parliamentary_supremacy_statutes__acts_of_union, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__acts_of_union, extractiveness, 0.58).
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__acts_of_union, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(parliamentary_supremacy_statutes__acts_of_union, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parliamentary_supremacy_statutes__acts_of_union, tangled_rope).
narrative_ontology:human_readable(parliamentary_supremacy_statutes__acts_of_union, "Acts of Union: Statutory Merger of Parliaments").
narrative_ontology:topic_domain(parliamentary_supremacy_statutes__acts_of_union, "political/legal").

domain_priors:requires_active_enforcement(parliamentary_supremacy_statutes__acts_of_union).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parliamentary_supremacy_statutes__acts_of_union, '8d607121-7705-4242-8c45-14d21c544b5a').
narrative_ontology:cs_kernel_codification('8d607121-7705-4242-8c45-14d21c544b5a', formalized).
narrative_ontology:cs_authority_grounding('8d607121-7705-4242-8c45-14d21c544b5a', extraction).
narrative_ontology:cs_interpretation_layer_present('8d607121-7705-4242-8c45-14d21c544b5a').
narrative_ontology:cs_reading_relation('8d607121-7705-4242-8c45-14d21c544b5a', fixed_term_parliaments_act__supremacy_demonstration, influences).
narrative_ontology:cs_reading_relation('8d607121-7705-4242-8c45-14d21c544b5a', parliament_act_1911__chambers_and_money_bills, influences).
narrative_ontology:cs_reading_relation('8d607121-7705-4242-8c45-14d21c544b5a', parliament_act_1949__self_modifying_procedure, influences).
narrative_ontology:cs_axiom('8d607121-7705-4242-8c45-14d21c544b5a', foundational, parliament_can_remake_constitutional_subject_by_statute).
narrative_ontology:cs_axiom_status(parliament_can_remake_constitutional_subject_by_statute, holdable).
narrative_ontology:cs_axiom_grounding('8d607121-7705-4242-8c45-14d21c544b5a', parliament_can_remake_constitutional_subject_by_statute, conventional).
narrative_ontology:cs_axiom('8d607121-7705-4242-8c45-14d21c544b5a', foundational, no_parliament_can_bind_its_successor).
narrative_ontology:cs_axiom_status(no_parliament_can_bind_its_successor, holdable).
narrative_ontology:cs_axiom_grounding('8d607121-7705-4242-8c45-14d21c544b5a', no_parliament_can_bind_its_successor, conventional).
narrative_ontology:cs_reference_frame('8d607121-7705-4242-8c45-14d21c544b5a', parliamentary_supremacy_doctrine).
narrative_ontology:cs_drift_state('8d607121-7705-4242-8c45-14d21c544b5a', post_devolution_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8d607121-7705-4242-8c45-14d21c544b5a', '').
narrative_ontology:cs_kernel_id(parliamentary_supremacy_statutes__acts_of_union, parliamentary_supremacy_statutes).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parliamentary_supremacy_statutes__acts_of_union, westminster_legislature).
narrative_ontology:constraint_beneficiary(parliamentary_supremacy_statutes__acts_of_union, metropolitan_commerce).
narrative_ontology:constraint_victim(parliamentary_supremacy_statutes__acts_of_union, scottish_institutional_autonomy).
narrative_ontology:constraint_victim(parliamentary_supremacy_statutes__acts_of_union, irish_institutional_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL GOVERNANCE (SNARE) — The merged kingdoms' institutional actors (Scottish and Irish parliaments pre-union) face absolute suppression: their separate legislatures are statutorily abolished. No exit exists within the legal framework. The constraint extracts institutional autonomy and remits governance decisions to Westminster. Suppression is maximal (no alternatives legally available), extractiveness is high (peripheral interests are subordinated to metropolitan interests in the unified parliament), and there is minimal coordination benefit for the peripheral actors — the constraint exists to centralize, not to solve a collective action problem at their level.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__acts_of_union, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL REPRESENTATION (TANGLED ROPE) — Over a generational horizon, Scottish and Irish regional representatives within Westminster Parliament experience a mixed structure. The Acts provide coordination function: shared markets, unified legal standards, pooled military power, and common commercial regulation benefit peripheral economies (extraction efficiency and risk pooling). Yet asymmetric extraction persists: peripheral regions have fewer MPs proportionally, peripheral interests are outvoted by metropolitan majorities, and resource flows concentrate toward London. The constraint requires active enforcement (the union must continually reassert Westminster supremacy) and exhibits genuine coordination alongside asymmetric extraction.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__acts_of_union, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WESTMINSTER & METROPOLITAN COMMERCE (ROPE) — The metropolitan legislature and London-centered commerce experience the Acts as pure coordination with no net extraction cost to them. The constraint solves the common-market problem (no tariff wars between constituent kingdoms), unifies debt instruments, creates a single imperial trading bloc, and concentrates political authority in Westminster. The beneficiaries experience the Acts as a coordination mechanism, not an extraction mechanism — the constraint moves value toward them but in the language of shared benefits. Arbitrage exit option available: Westminster could theoretically dissolve the union, but the institutional benefits are so substantial that exit is negligible.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__acts_of_union, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UNIONIST FACTIONS (TANGLED ROPE) — Within Scotland and Ireland, unionist commercial and political elites experience the Acts as coordination that benefits them. Unified markets, military security, and imperial trade networks increase their wealth and power relative to the pre-union landscape. Yet they also accept suppression of separate national legislatures and subordination of regional interests to metropolitan majorities. They are both beneficiaries of the coordination function and subjects of the suppression mechanism. Their exit option is constrained — secession from the union would mean loss of imperial trade privileges and military security.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__acts_of_union, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: IMPERIAL CONSTITUTIONAL ORDER (PITON) — At the civilizational scale, the Acts of Union are treated as a foundational constitutional mechanism that unified the British state and enabled imperial expansion. Yet by the 20th century, the functional coordination content has largely atrophied: Scotland and Ireland have demanded devolution (Scottish Parliament 1999, Irish Home Rule), the 'permanent union' has fractured (Irish independence 1922), and the empire itself has dissolved. The Acts persist in statute as constitutional doctrine, but their practical force is theatrical — the constraint maintains ceremonial and formal supremacy while functional governance has devolved. Theater ratio (0.35) reflects that the Acts' main remaining function is to anchor constitutional authority; the actual coordination and extraction mechanisms operate through other channels (devolved parliaments, trade relationships, defense treaties).
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__acts_of_union, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal framing, the analytical observer risks naturalizing the Acts of Union as an inherent feature of parliamentary sovereignty: 'Parliament is supreme and cannot be bound by its predecessors; the Acts of Union are law because Parliament enacted them; Parliament could repeal them, yet the very supremacy that allows repeal also makes the union irrevocable within the framework of parliamentary law.' This perspective treats the constraint as an immutable structural feature of common-law constitutionalism itself — a logical necessity rather than a contingent institutional arrangement. However, the structural data (identifiable beneficiaries, victims with suppressed alternatives, active enforcement requirement) reveals this as a false summit. The perceived inevitability of parliamentary supremacy naturalizes what is actually a contested institutional framework.
constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__acts_of_union, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parliamentary_supremacy_statutes__acts_of_union_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__acts_of_union, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parliamentary_supremacy_statutes__acts_of_union, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parliamentary_supremacy_statutes__acts_of_union, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parliamentary_supremacy_statutes__acts_of_union, TR),
    TR >= 0.70.

:- end_tests(parliamentary_supremacy_statutes__acts_of_union_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The initial extraction upon union was severe (0.72) — the peripheral kingdoms lost separate legislative authority and were subordinated to metropolitan majorities within a unified Parliament. However, the measured value reflects the constraint's contemporary state (early 21st century), where devolution has returned significant governance authority to peripheral assemblies. The extraction remains substantial because Westminster retains constitutional supremacy (Parliament could repeal devolution) and peripheral regions lack proportional parliamentary representation. Suppression (0.72): High and persistent. The Acts of Union statutorily abolished the Scottish and Irish parliaments. This suppression was absolute in the 18th and 19th centuries (no legal alternative existed). Devolution has reduced formal suppression by creating alternative governance pathways, but statutory suppression persists: the devolved assemblies operate under Westminster delegation, and the Acts remain unrevoked. Theater ratio (0.35): Low-moderate in formation (1707-1800), rising sharply to high (0.68 contemporary). At union, the Acts' function was primarily coordination (managing common markets, military defense, commercial regulation) with extractive asymmetry layered on top. By the 21st century, the Acts' main function is constitutional — reasserting parliamentary supremacy and maintaining the legal fiction of a permanent, irrevocable union. The rising theater ratio reflects that devolution has transferred actual governance functions to peripheral assemblies while the Acts remain as ceremonial constitutional doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows maximum perspectival divergence across the six types. The peripheral governance perspective sees a snare: absolute suppression, high extraction, no legal exit. The Westminster perspective sees rope: pure coordination enabling imperial expansion and common markets. The unionist factions see tangled rope: genuine benefits from coordination alongside acceptance of suppression. The devolved assemblies see piton: constitutional theater maintaining supremacy while functional governance has atrophied. The analytical observer risks seeing a mountain: parliamentary supremacy as an immutable constitutional law. The false summit signal is diagnostic: if the Acts were a natural law of parliamentary organization, they would require no beneficiaries or victims. But identifiable beneficiaries (Westminster, metropolitan commerce) and victims (peripheral institutional autonomy) exist and actively maintain the constraint — this is evidence that the naturalized doctrine conceals a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint exhibits divergent directionalities from each perspective because the beneficiaries and victims occupy structurally distinct positions. Westminster and metropolitan commerce have arbitrage-level exit options (Parliament could theoretically dissolve the union, but the benefits are so substantial that exit is negligible) — they experience low or negative d (they extract value, not the reverse). Scottish and Irish peripheral governance had trapped-level exit options initially (no legal alternatives; suppression was absolute) and continue to face constrained exit options (devolution provides functional autonomy within a framework Parliament retains the right to revoke) — they experience high d (value extracts away from them). The unionist factions have mixed positioning: they benefit from coordination (lower d as beneficiaries) but accept suppression of separate legislatures (higher d as subjects of Westminster supremacy). The engine computes d from these structural facts: beneficiaries with arbitrage → low d → negative f(d); victims with trapped → high d → high f(d); mixed actors with constrained → moderate d → moderate f(d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_supremacy_binding_force,
    'Can Parliament truly bind its successors, or is the claim of permanent union merely a conventional assertion by the current Parliament?',
    'Constitutional history: examination of whether Parliament''s repeal of the Acts of Union would be legally and politically possible; test case through attempted repeal and analysis of judicial response; comparison with other constitutional frameworks where parliament has dissolved or reformed itself',
    'If Parliament can genuinely bind successors: the Acts are a fundamental law (mountain candidate, but only if no alternatives exist). If Parliament retains absolute supremacy: the Acts are law by current parliamentary choice (snare or tangled_rope, depending on who bears the costs of that choice). The mountain classification depends on whether the binding is necessary or merely asserted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliamentary_supremacy_binding_force, conceptual, 'Whether parliamentary supremacy is a binding natural law or a contingent assertion').

omega_variable(
    peripheral_institutional_exit_modality,
    'What is the actual structural relationship between the Acts of Union and peripheral institutional autonomy — is the suppression absolute, or do alternative governance modes (devolution, custom, cultural institutions) provide functional exit for peripheral actors?',
    'Institutional analysis of devolution arrangements (Scottish Parliament post-1999, Welsh Assembly, Northern Ireland Assembly); examination of whether these represent true functional exit from the union constraint or merely delegated governance within it; comparison of pre-union peripheral parliaments to post-devolution parliaments in terms of policy autonomy and legislative scope',
    'If devolution is true exit: the constraint is a tangled_rope or scaffold (depending on whether devolution is permanent or temporary). If devolution is delegated governance within Westminster supremacy: the constraint remains a snare at the absolute institutional level (suppression is permanent, exits are illusory). The classification hinges on whether peripheral actors have genuine authority or merely administrative discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peripheral_institutional_exit_modality, empirical, 'Whether devolution represents functional exit from union constraint').

omega_variable(
    unionist_settlement_stability,
    'Is the Acts of Union constraint stable across time, or does it require continuous renegotiation and enforcement to maintain legitimacy?',
    'Historical analysis of union-related constitutional crises and settlements: Irish Home Rule debates (1880s-1920), Scottish devolution referenda (1979, 1997, 2014), Brexit and its differential impact on constituent nations; measurement of public support for union over time; analysis of enforcement mechanisms (military, legal, electoral) required to sustain the union',
    'If stable without renegotiation: the Acts structure a durable institutional framework (rope or tangled_rope). If requiring continuous enforcement: the constraint exhibits snare characteristics (suppression must be actively maintained). If legitimacy erodes: the constraint approaches piton status (performative maintenance of a degraded mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unionist_settlement_stability, empirical, 'Stability and enforcement burden of the union constraint').

omega_variable(
    imperial_versus_union_extraction,
    'What proportion of the measured extractiveness (0.58) derives from the union mechanism itself versus from the imperial/mercantile extraction system that the union enabled?',
    'Counterfactual analysis: comparison of peripheral regions'' economic and political status pre-union vs post-union vs post-empire; decomposition of extraction flows attributable to union governance versus imperial commerce versus metropolitan capital concentration; examination of whether peripheral economies benefited from or were drained by the unified market and imperial system',
    'If union itself is primarily extractive: the constraint is independent snare/tangled_rope. If the union enabled benign market coordination but imperial commerce was extractive: the Acts should be classified as lower ε (rope or scaffold), with imperialism as a separate constraint. If union and empire are inseparable: the Acts inherit the empire''s full extractiveness, and ε=0.58 understates the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_versus_union_extraction, empirical, 'Extraction attributable to union mechanism versus imperial commerce').

omega_variable(
    kernel_reading_alternative_framing,
    'Is the Acts of Union constraint better understood as statutory codification of parliamentary supremacy (this reading''s framing) or as a moment of peripheral institutional suppression that parliament later struggled to legitimize?',
    'Historiographical analysis: examine contemporary discourse at the time of each Act (1707 Scotland, 1801 Ireland); assess whether the Acts were framed as exercises of parliamentary supremacy or as reciprocal constitutional settlements; examine subsequent constitutional theory (Dicey, Blackstone) and whether parliament naturalizes the Acts or acknowledges their contingency',
    'If statutory codification of supremacy is the dominant framing: this reading (acts_of_union) correctly identifies the constraint as an exercise of parliamentary power to remake the constitution by statute. If the Acts are viewed as historical accident or pragmatic settlement: the constraint''s framing as supreme law is itself a later reinterpretation (omega uncertainty about whether this reading''s axioms hold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Whether Acts of Union instantiate parliamentary supremacy or are contingent settlements later naturalized as supremacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parliamentary_supremacy_statutes__acts_of_union, 1707, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pau_theater_1707_union_functional, parliamentary_supremacy_statutes__acts_of_union, theater_ratio, 1707, 0.15).
narrative_ontology:measurement(pau_theater_1900_union_doctrine, parliamentary_supremacy_statutes__acts_of_union, theater_ratio, 1900, 0.25).
narrative_ontology:measurement(pau_theater_1999_constitutional_ritual, parliamentary_supremacy_statutes__acts_of_union, theater_ratio, 1999, 0.5).
narrative_ontology:measurement(pau_theater_2026_ceremonial_maintenance, parliamentary_supremacy_statutes__acts_of_union, theater_ratio, 2026, 0.68).

% Extraction over time
narrative_ontology:measurement(pau_extractiveness_1707_scotland_union, parliamentary_supremacy_statutes__acts_of_union, base_extractiveness, 1707, 0.72).
narrative_ontology:measurement(pau_extractiveness_1801_ireland_union, parliamentary_supremacy_statutes__acts_of_union, base_extractiveness, 1801, 0.68).
narrative_ontology:measurement(pau_extractiveness_1922_irish_independence, parliamentary_supremacy_statutes__acts_of_union, base_extractiveness, 1922, 0.55).
narrative_ontology:measurement(pau_extractiveness_1999_scottish_devolution, parliamentary_supremacy_statutes__acts_of_union, base_extractiveness, 1999, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(pau_suppression_1707_abolition_scots_parliament, parliamentary_supremacy_statutes__acts_of_union, suppression_requirement, 1707, 0.85).
narrative_ontology:measurement(pau_suppression_1801_abolition_irish_parliament, parliamentary_supremacy_statutes__acts_of_union, suppression_requirement, 1801, 0.8).
narrative_ontology:measurement(pau_suppression_1922_irish_free_state, parliamentary_supremacy_statutes__acts_of_union, suppression_requirement, 1922, 0.65).
narrative_ontology:measurement(pau_suppression_1999_devolved_assemblies, parliamentary_supremacy_statutes__acts_of_union, suppression_requirement, 1999, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parliamentary_supremacy_statutes__acts_of_union, resource_allocation).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__acts_of_union, fixed_term_parliaments_act__supremacy_demonstration).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__acts_of_union, parliament_act_1911__chambers_and_money_bills).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__acts_of_union, parliament_act_1949__self_modifying_procedure).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__acts_of_union, imperial_trade_extraction).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__acts_of_union, scottish_devolution_settlement).
narrative_ontology:affects_constraint(parliamentary_supremacy_statutes__acts_of_union, northern_ireland_governance_compact).

% DUAL FORMULATION NOTE:
% The Acts of Union are the foundational constraint in a family of parliamentary supremacy constraints. They establish the principle that Parliament can remake the constitution by statute. The sibling readings (fixed_term, parliament_act_1911, parliament_act_1949) are distinct constraints that test or apply this principle in different contexts. Each sibling has its own ε, beneficiary/victim structure, and classification. All members of the parliamentary_supremacy_statutes family link to each other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parliamentary_supremacy_statutes__acts_of_union, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
