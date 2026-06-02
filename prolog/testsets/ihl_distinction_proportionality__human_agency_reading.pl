% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Distinction and Proportionality as Human Agency Requirement (Martens Clause Reading)
 *   domain: international_humanitarian_law/military_ethics/autonomous_weapons
 *
 * SUMMARY:
 *   International Humanitarian Law's distinction and proportionality
 *   obligations are anchored in the Martens Clause principle that lethal
 *   force decisions must reflect irreducible human moral judgment. Under the
 *   human agency reading, this means LAWS (Lethal Autonomous Weapons Systems)
 *   that make targeting decisions without human-in-the-loop are categorically
 *   unlawful. This constraint stories one interpretive reading of IHL's
 *   foundational distinction/proportionality obligations — specifically, the
 *   reading that binds lawfulness to human decision-making at the moment of
 *   fire. This is one of three competing readings (human_agency_reading,
 *   outcomes_based_reading, categorical_prohibition_reading) of the same
 *   contested kernel (ihl_distinction_proportionality). Each reading
 *   instantiates a different constraint with different extractiveness,
 *   different beneficiaries, and different implications for autonomous
 *   weapons governance. This story models ONLY the human agency reading, as a
 *   clean ε-invariant constraint. The other readings are separate constraint
 *   stories linked via network.affects_constraints. The human agency reading
 *   has moderate-to-high extractiveness (0.68) because it suppresses all
 *   fully autonomous targeting systems globally, extracting significant
 *   opportunity cost from military operations, autonomous systems developers,
 *   and AI research agendas, while conferring concentrated benefit on IHL
 *   interpretive authorities (ICRC, ICJ) who maintain interpretive control
 *   over what 'human judgment' means in practice. The constraint exhibits
 *   snare characteristics (high suppression, high extraction, minimal
 *   coordination benefit to the suppressed agent), but also shows tangled
 *   rope and scaffold readings from military and moratorium coalition
 *   perspectives respectively, revealing the multi-sided nature of human
 *   agency obligations.
 *
 * KEY AGENTS:
 *   - IHL Interpretive Authorities (ICRC, ICJ, legal scholars): Primary beneficiary (institutional/arbitrage) — maintain centrality in determining what 'human judgment' and 'proportionality' mean in weapons governance
 *   - Military Establishments (state defense institutions): Primary victim (organized/constrained) — face operational efficiency losses, slower targeting cycles, increased operator cognitive load under human-in-the-loop requirements
 *   - Autonomous Systems Developers (AI companies, defense contractors): Secondary victim (powerful/constrained) — suppressed from developing fully autonomous targeting systems globally; constrained to develop only human-supervised systems
 *   - Combatants and Civilians in Conflict Zones: Powerless/trapped agents intended to be protected by distinction/proportionality rules; have no agency over enforcement of the human agency requirement
 *   - Military Operators (soldiers making targeting decisions): Moderate/constrained — bound by rules requiring human judgment; protected by legal framework but burdened by decision-making responsibility under stress
 *   - Humanitarian Advocacy Coalitions (NGOs, moratorium advocates): Organized/constrained beneficiaries — human agency requirement supports their campaign to control autonomous weapons, though they may advocate for even stricter prohibitions
 *   - Analytical Observer: Views the human agency requirement from civilizational horizon; risks naturalizing it as irreducible feature of law rather than as chosen institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.68).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.72).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, snare).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Distinction and Proportionality as Human Agency Requirement (Martens Clause Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_humanitarian_law/military_ethics/autonomous_weapons").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, 'constraint-ihl-human-agency-2026-02-26-001').
narrative_ontology:cs_kernel_codification('constraint-ihl-human-agency-2026-02-26-001', formalized).
narrative_ontology:cs_authority_grounding('constraint-ihl-human-agency-2026-02-26-001', lineage).
narrative_ontology:cs_interpretation_layer_present('constraint-ihl-human-agency-2026-02-26-001').
narrative_ontology:cs_reading_relation('constraint-ihl-human-agency-2026-02-26-001', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_reading_relation('constraint-ihl-human-agency-2026-02-26-001', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('constraint-ihl-human-agency-2026-02-26-001', foundational, human_judgment_irreducible_to_process).
narrative_ontology:cs_axiom_status(human_judgment_irreducible_to_process, holdable).
narrative_ontology:cs_axiom_grounding('constraint-ihl-human-agency-2026-02-26-001', human_judgment_irreducible_to_process, deontological).
narrative_ontology:cs_axiom('constraint-ihl-human-agency-2026-02-26-001', foundational, lawfulness_process_dependent_not_outcome_dependent).
narrative_ontology:cs_axiom_status(lawfulness_process_dependent_not_outcome_dependent, holdable).
narrative_ontology:cs_axiom_grounding('constraint-ihl-human-agency-2026-02-26-001', lawfulness_process_dependent_not_outcome_dependent, conventional).
narrative_ontology:cs_axiom('constraint-ihl-human-agency-2026-02-26-001', secondary, martens_clause_enforceability).
narrative_ontology:cs_axiom_status(martens_clause_enforceability, overridden).
narrative_ontology:cs_axiom_grounding('constraint-ihl-human-agency-2026-02-26-001', martens_clause_enforceability, conventional).
narrative_ontology:cs_reference_frame('constraint-ihl-human-agency-2026-02-26-001', martens_clause_human_accountability_framework).
narrative_ontology:cs_drift_state('constraint-ihl-human-agency-2026-02-26-001', contemporary_autonomous_weapons_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('constraint-ihl-human-agency-2026-02-26-001', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, humanitarian_advocacy_coalitions).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_systems_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, state_defense_establishments).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMBATANT/CIVILIAN UNDER AUTONOMOUS FIRE (SNARE) — Cannot exit the targeting environment. The human agency requirement is explicitly supposed to protect this agent, but the reading's enforcement mechanism (categorical prohibition on fully autonomous systems) creates a binding constraint: any nation adopting this reading must exclude its military from using autonomous weapons, regardless of tactical cost. The targeted agent has no choice but to trust the political commitment is meaningful. If enforcement degrades, the agent bears full cost.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__human_agency_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MILITARY OPERATOR (SNARE) — Structurally constrained by rules of engagement mandating human decision-making at the moment of fire. Cannot delegate targeting decision to autonomous systems even when it would reduce operator error. High suppression: violation of these rules creates legal liability and institutional sanction. The operator benefits from liability protection (knowing their decisions receive legal scrutiny and protection) but bears the extraction cost of slower decision-making under constraint. Moderate power; constrained exit (defection from military or adoption of prohibited systems carries career/legal cost).
constraint_indexing:constraint_classification(ihl_distinction_proportionality__human_agency_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MILITARY ESTABLISHMENT (TANGLED ROPE) — Organized institutional actor with constrained exit. The human agency requirement coordinates genuine military-humanitarian concern: military forces claim to want proportionality and distinction rules as internal discipline (coordination function). But the requirement also extracts significant cost: slower response times, higher operator cognitive load, reduced tactical autonomy. Active enforcement required: military doctrine, training regimes, legal review procedures. Benefits from institutional legitimacy gained by complying with IHL. Extraction: operational efficiency losses. Mixed benefit/cost structure makes this tangled rather than pure rope.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__human_agency_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: IHL INTERPRETIVE AUTHORITY (ROPE) — The ICRC, International Court of Justice, and humanitarian law scholarship see the human agency requirement as coordination mechanism: it clarifies what 'distinction' and 'proportionality' mean in practice (humans making the judgment), maintains interpretive authority over IHL evolution, and prevents legal vacuum where autonomous weapon legality is undetermined. This perspective experiences the constraint as coordination (establishing shared meaning and authority structure) with net benefit: maintains ICRC's centrality in targeting discussions and provides clarity over alternative framings. Arbitrage exit available: can shift interpretation if international consensus changes, but currently well-positioned.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__human_agency_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MARTENS CLAUSE INSTITUTIONAL PRESENCE (PITON) — The Martens Clause ('dictates of public conscience and principles of humanity') originally grounded IHL in broadly shared moral commitments. In contemporary weapons governance discourse, invocation of the 'humanity' principle has become increasingly theatrical: it establishes that any weapons discussion *should* include humanitarian concerns, but the actual mechanism (human moral judgment at moment of fire) persists more through institutional inertia than through demonstrated functional effectiveness. Theater ratio reflects the performative invocation of 'humanity' in contexts where technical constraints (speed-of-fire, sensor reliability, signal latency) often override the judgment requirement. The clause functions as institutional legitimacy maintenance rather than as operational discipline.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__human_agency_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: AUTONOMOUS WEAPONS MORATORIUM COALITION (SCAFFOLD) — Organized actors (humanitarian NGOs, some state delegations, academic ethicists) see the human agency requirement as a temporary measure with sunset logic: until autonomous weapons technology stabilizes and its failure modes are fully understood, human-in-the-loop is a precautionary requirement. Low theater ratio for this perspective: the requirement is functionally justified (preventing catastrophic errors during technology maturation). Sunset clause implicit: once autonomous targeting meets or exceeds human-operator accuracy AND can be audited in real-time AND has fail-safes against drift, the human-in-the-loop requirement becomes less defensible. Constrained exit: moratorium coalition can exit by accepting autonomous systems, but faces reputational and legal cost.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__human_agency_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, human moral agency in lethal decision-making might be viewed as an irreducible feature of lawful warfare: the principle that humans must bear responsibility for deaths they cause is foundational to law itself (actus non facit reum nisi mens rea sit — the act alone does not make one guilty unless the mind is guilty). Under this reading, delegating to machines would structurally violate the foundation of criminal and humanitarian law, making it impossible to assign moral/legal responsibility. This perspective sees human agency not as a policy choice but as a logical requirement for lawful warfare. However, the structural data (clear beneficiaries in ICRC/IHL authorities, clear victims in military efficiency and autonomous systems developers) reveals this as a false summit: the 'irreducibility' of human judgment naturalizes what is actually a chosen institutional arrangement.
constraint_indexing:constraint_classification(ihl_distinction_proportionality__human_agency_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ihl_distinction_proportionality__human_agency_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ihl_distinction_proportionality__human_agency_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, TR),
    TR >= 0.70.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Moderate-high and rising. The human agency requirement imposes significant operational cost on military systems: slower targeting cycles (humans cannot decide in milliseconds), increased operator cognitive load, constraint on autonomous system development globally. Beneficiaries (ICRC/IHL authorities) gain interpretive power; victims (military, autonomous systems developers) bear extraction cost. The measurement trajectory shows extractiveness rising from 0.35 (early periods when human judgment was less contested) to 0.68 (contemporary period when autonomous systems capability makes the constraint more obviously costly). Suppression (0.72): Moderate-high and stable. Multiple enforcement mechanisms: treaty obligations, military doctrine, threat of international legal action, pressure from humanitarian coalitions, domestic legal liability for violations. Suppression is primarily structural (treaty-based, enforceable through state actions) rather than internalized. Measurement trajectory shows slight rise as enforcement infrastructure strengthens (training regimes, legal review procedures). Theater ratio (0.35): Low-to-moderate and rising. The Martens Clause invocation of 'humanity' is increasingly performative: states invoke it while developing increasingly autonomous systems. Early periods showed higher functional content (genuine operational constraint). Contemporary period shows rising theater as the gap widens between stated commitment to human judgment and actual system autonomy levels. The rising trajectory (0.22 → 0.35) reflects degradation of the Martens Clause from operational constraint to aspirational language.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival disagreement. The powerless agent (combatants under fire) experiences this as a snare: the human agency requirement is supposed to protect them, but they have no choice but to trust the political commitment is meaningful. The military establishment sees snare (operational constraints) but also tangled rope (genuine coordination benefit from having clear rules). The ICRC sees rope (coordination of shared meaning and authority). The moratorium coalition sees scaffold (temporary measure with sunset). The Martens Clause as institutional presence sees piton (performative invocation). The analytical observer risks seeing mountain (irreducible feature of lawful warfare) — but the structural data reveals this as a false summit (beneficiaries and victims are clearly identifiable; the 'irreducibility' naturalizes a chosen institutional arrangement). The perspectival gap is not a measurement problem; it reflects genuine structural differences in how different agents experience the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position: beneficiary status + exit options → low d → low χ; victim status + trapped exit → high d → high χ; organized with constrained exit → moderate d → moderate χ. ICRC (institutional/arbitrage): d ≈ 0.12 (beneficiary with exits) → low f(d) → benefits from negative χ. Military establishment (organized/constrained): d ≈ 0.50 (mixed beneficiary from rule clarity + victim from efficiency cost) → moderate f(d) → moderate χ. Autonomous systems developers (powerful/constrained): d ≈ 0.65 (victims with some exits) → higher f(d) → higher χ. Combatants (powerless/trapped): d ≈ 0.95 (victim with no exit) → maximum f(d) → maximum χ. The snare classification for powerless agents reflects the high d value from trapped exit and victim status. The rope classification for ICRC reflects low d from beneficiary status and arbitrage exit. The tangled rope for military reflects mixed d from being both beneficiary (of rule clarity, institutional legitimacy) and victim (of efficiency losses).
 *
 * MANDATROPHY ANALYSIS:
 *   The human agency reading resolves the mandatrophy by showing that snare vs rope classification depends on structural position. From the ICRC's perspective, this is coordination (rope) — establishing shared meaning about what 'proportionality' requires. From the military's perspective, this is mixed coordination and extraction (tangled rope) — genuine rule clarity benefit but also operational cost. From powerless agents' perspective, this is extraction (snare) — they have no agency over enforcement. From the outcomes-based reading perspective (the sibling constraint), this reading is seen as over-constraining: if autonomous systems achieve better targeting accuracy and proportionality, why suppress them? The mandatrophy is resolved by recognizing that mandatrophy exists only from certain perspectives. The human agency reading is NOT trying to resolve whether human judgment is truly irreplaceable (empirically contested). It is making a structural claim: lawfulness depends on human decision process, not outcome. Different readings answer the 'what makes targeting lawful?' question differently, and no single answer dominates without choosing a kernel reading. The false summit signal is critical: the analytical perspective that sees human agency as irreducible natural law (mountain) is actually seeing a chosen institutional reading — the structural data (clear beneficiaries/victims) makes this visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_judgment_quality_erosion,
    'Can human moral judgment in targeting decisions remain meaningfully exercised when decision time compresses below human cognitive processing windows (milliseconds in air combat, microseconds in AI-assisted targeting)?',
    'Empirical study of actual targeting decisions under operational stress: analysis of decision quality, consistency, adherence to distinction/proportionality rules as time pressure increases. Comparison with AI-assisted decision accuracy and audit trails.',
    'If human judgment quality degrades below reliable threshold: the human agency requirement becomes performative (theater-only), and the reading''s extraction mechanism becomes visible — the constraint suppresses autonomous systems not because human judgment is irreplaceable, but because it maintains IHL interpretive authority. If human judgment remains reliably superior: the reading''s classification as snare/tangled_rope is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_judgment_quality_erosion, empirical, 'Whether human judgment remains meaningfully exercisable at contemporary decision speeds').

omega_variable(
    interpretive_authority_entrenchment,
    'Does the human agency requirement derive from principled commitment to human moral accountability, or does it primarily function to maintain the ICRC and ICJ''s interpretive authority over weapons legality?',
    'Discourse analysis of ICRC/ICJ statements on autonomous weapons: do arguments emphasize irreducibility of human judgment, or do they emphasize need for legal clarity and authority control? Structural analysis of how quickly interpretive authorities have adopted autonomous systems in other domains (medicine, finance) where accountability is less clear.',
    'If primarily authority entrenchment: the beneficiary (IHL interpretive authorities) is correctly identified, and the extraction mechanism (suppression of systems that would undermine authority''s centrality) is visible. If principled: the constraint genuinely coordinates IHL values rather than extracting institutional power. This distinction is critical to resolving the false summit question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_authority_entrenchment, conceptual, 'Whether human agency requirement prioritizes moral irreducibility or authority maintenance').

omega_variable(
    reading_committer_identity,
    'Is this constraint''s human agency reading the dominant interpretation of IHL''s distinction/proportionality obligations, or is it one competing reading among several equally legitimate framings?',
    'Survey of IHL scholarship, state military doctrine, ICRC official position, ICJ rulings on autonomous weapons. Identification of which readings command consensus and which remain contested.',
    'If dominant: this reading''s snare classification reflects the actual structure of how IHL is enforced (suppression of alternatives). If contested: the constraint family decomposition (human_agency_reading, outcomes_based_reading, categorical_prohibition_reading) correctly represents that distinct constraint stories apply, and no single reading monopolizes the interpretation space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_identity, empirical, 'Status of human agency reading in IHL consensus and state practice').

omega_variable(
    martens_clause_functional_scope,
    'Does the Martens Clause (''dictates of public conscience'') retain enforcement power in contemporary weapons governance, or has it degraded to aspirational language without operational constraint?',
    'Analysis of recent weapons decisions and adoption patterns: have states invoking Martens Clause principles actually constrained weapons deployment? Or have they invoked it while proceeding with weapons development? Comparison with periods when Martens Clause had stronger enforcement (e.g., chemical weapons bans).',
    'If functional: piton classification is incorrect; the constraint should be rope or tangled_rope. If degraded: piton classification is correct, and the theatrical invocation of ''humanity principles'' masks erosion of actual constraints on weapons development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_functional_scope, empirical, 'Whether Martens Clause currently enforces constraints on weapons adoption').

omega_variable(
    kernel_reading_foreclosure,
    'Does the human agency reading logically foreclose the outcomes-based reading (systems that achieve distinction/proportionality outcomes are lawful regardless of decision mechanism) or do they coexist as fundamentally different interpretive frameworks?',
    'Logical analysis of core axioms: human agency reading = ''legality depends on decision process (human judgment)''; outcomes-based = ''legality depends on targeting accuracy and proportionality results''. Can both be true within a single IHL framework? Or does adopting one require rejecting the other''s core premise?',
    'If they foreclose each other: reading_relations should be ''forecloses''. If they coexist: ''coexists_with''. This determines the logical structure of the constraint family and the possibility of synthetic interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between human agency and outcomes-based readings of IHL').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_human_theater_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ihl_human_theater_t7, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 7, 0.29).
narrative_ontology:measurement(ihl_human_theater_t15, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 15, 0.35).

% Extraction over time
narrative_ontology:measurement(ihl_human_extract_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ihl_human_extract_t7, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(ihl_human_extract_t15, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ihl_human_suppress_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ihl_human_suppress_t7, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 7, 0.68).
narrative_ontology:measurement(ihl_human_suppress_t15, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% The ihl_distinction_proportionality kernel decomposes into three constraint stories, each instantiating a different reading with different ε values and different beneficiary/victim structures. Human_agency_reading (this file) assumes process-based lawfulness (humans decide); outcomes_based_reading assumes result-based lawfulness (systems that achieve accuracy are lawful); categorical_prohibition_reading assumes categorical prohibition (autonomous lethal force per se is unlawful). Each reading has different implications for LAWS governance. The three readings coexist in actual IHL discourse but make logically distinct claims about what makes targeting lawful. They are linked because they compete for interpretive dominance in the same domain (IHL's distinction/proportionality obligations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__human_agency_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
