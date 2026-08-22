% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Human Agency Reading: Distinction/Proportionality Requires Irreducible Human Judgment
 *   domain: legal/military-ethics/technology-governance
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel: IHL's
 *   distinction and proportionality obligations in the age of autonomous
 *   weapons. The human agency reading asserts that IHL's core principles
 *   require irreducible human moral judgment at the moment of lethal force
 *   application, and that Martens Clause principles of humanity prohibit
 *   delegating life/death decisions to machines. This is one of three
 *   structurally distinct readings competing in international law. Under this
 *   reading, LAWS (Lethal Autonomous Weapons Systems) are categorically
 *   unlawful unless they preserve meaningful human control—not because human
 *   judgment always produces better outcomes, but because the law itself
 *   depends on human agency for legitimacy. The constraint carries high
 *   extractiveness because it suppresses a entire class of military
 *   technology and concentrates authority over what counts as 'human agency'
 *   in IHL interpretive bodies. The claim/metric gap is deliberate and
 *   diagnostic: this reading claims tangled_rope structure (genuine
 *   coordination on a boundary, plus extraction of operational capability),
 *   while the authored metrics describe substantially extractive, actively
 *   enforced operation. The engine will compute divergent per-seat
 *   classifications: from the IHL interpretive authority's seat the reading
 *   coordinates on a non-negotiable principle; from the military developer's
 *   seat it extracts capability by foreclosing system designs and imposing
 *   compliance burdens.
 *
 * KEY AGENTS:
 *   - ihl_interpretive_authorities: Sets and enforces the human agency requirement; maintains institutional centrality by gatekeeping what counts as sufficient human control — high power/analytical exit, agenda-setter role
 *   - military_operational_efficiency: Bears the cost of slower decision cycles, scaled human operator requirements, and vulnerability to fatigue — powerful but constrained by the requirement, payer role
 *   - autonomous_weapons_developers: Face market foreclosure and certification burdens; cannot sell fully autonomous systems in jurisdictions adopting this reading — powerful but identity_locked to autonomous weapons programs, payer role
 *   - affected_civilian_populations: Nominally benefit from the principle that targeting decisions are subject to human moral judgment, but remain trapped in conflict zones regardless — powerless, trapped exit, beneficiary role
 *   - combatants: Asymmetric beneficiary/payer: benefit from assurance that adversaries use human judgment, but pay costs of slower opposing forces with human-in-the-loop constraints — moderate power, immediate horizon
 *   - outcomes_based_advocates: Excluded from this reading's framing; they argue the law should assess outcomes not means, but the reading forecloses that argument by anchoring to irreducible human judgment as non-negotiable — excluded role
 *   - categorical_prohibition_advocates: Excluded but partially aligned; they would extend the prohibition further (all autonomous weapons forbidden categorically), but find this reading useful as a stepping stone — excluded role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.68).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.72).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Human Agency Reading: Distinction/Proportionality Requires Irreducible Human Judgment").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "legal/military-ethics/technology-governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '9f594d57-3db9-4516-9fdf-8dfdaee87cc3').
narrative_ontology:cs_kernel_codification('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', fixed_text).
narrative_ontology:cs_authority_grounding('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', lineage).
narrative_ontology:cs_interpretation_layer_present('9f594d57-3db9-4516-9fdf-8dfdaee87cc3').
narrative_ontology:cs_reading_relation('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', ihl_distinction_proportionality__outcomes_based_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', ihl_distinction_proportionality__categorical_prohibition_reading, forecloses).
narrative_ontology:cs_axiom('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', foundational, irreducible_human_judgment_targeting_requirement).
narrative_ontology:cs_axiom_status(irreducible_human_judgment_targeting_requirement, holdable).
narrative_ontology:cs_axiom_grounding('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', irreducible_human_judgment_targeting_requirement, deontological).
narrative_ontology:cs_axiom('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', foundational, human_dignity_in_machine_decisions_prohibited).
narrative_ontology:cs_axiom_status(human_dignity_in_machine_decisions_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', human_dignity_in_machine_decisions_prohibited, deontological).
narrative_ontology:cs_reference_frame('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', ihl_distinction_proportionality_via_human_judgment).
narrative_ontology:cs_drift_state('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', autonomous_weapons_capability_emergence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9f594d57-3db9-4516-9fdf-8dfdaee87cc3', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, combatants).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, affected_civilian_populations).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, combatants).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, martens_clause_humanity_principle).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, human_dignity_in_targeting_decisions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International Committee of the Red Cross, UN Protocol Review Conferences, and national military legal advisors interpret and enforce IHL's distinction and proportionality principles. Under this reading, they adjudicate that lethal targeting decisions must retain irreducible human moral judgment; they maintain institutional centrality by gatekeeping which systems satisfy this requirement. They set the standard for what counts as 'human agency' in the targeting loop.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Military forces incur operational costs from the requirement to keep humans in the targeting loop: slower decision cycles, reduced ability to process high-velocity threats, need for trained personnel at scale, vulnerability of human operators to fatigue and information overload. The constraint trades speed and scalability for the assurance that moral judgment sits at the critical juncture.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency, payer,
    powerful, biographical, constrained, global).

% Defense contractors and military R&D programs face constraints on system autonomy: they must prove systems remain under meaningful human control, cannot sell or deploy fully autonomous targeting systems, and bear certification burdens to demonstrate compliance with the human agency requirement. Investment in autonomous weapons technology faces regulatory uncertainty and market foreclosure in jurisdictions adopting this reading.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    powerful, biographical, constrained, global).

% Soldiers benefit from the principle that adversaries must retain human judgment in targeting (assurance against indiscriminate automated attacks) but pay the cost of slower, more deliberate targeting by opposing forces, and face higher personal risk in engagements where their own forces must move slower because their systems retain human-in-the-loop requirements.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, combatants, beneficiary,
    moderate, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, combatants, payer).

% Civilians in conflict zones gain the principle that their targeting is subject to human moral judgment rather than machine decision, but remain trapped in the zone of conflict regardless. The constraint's enforcement does not reduce civilian presence in target areas; it only changes the deliberation process under which they are or are not targeted.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, affected_civilian_populations, beneficiary,
    powerless, immediate, trapped, global).

% States, weapons developers, and military theorists who argue that IHL obligations should be technology-neutral—satisfied by any system (autonomous or human-operated) that demonstrably achieves distinction and proportionality performance—are structurally excluded from this reading's core premise. They argue the rule should assess outcomes, not means; this reading forecloses that framing by anchoring the rule to irreducible human judgment as a non-negotiable requirement.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, outcomes_based_advocates, excluded,
    powerful, generational, constrained, global).

% Civil society organizations, some states, and philosophical traditions arguing that autonomous weapons are categorically prohibited regardless of performance (per the Martens Clause and human dignity principles) find this reading instrumentally useful but conceptually insufficient. They would extend the prohibition further: this reading anchors only to human judgment, leaving systems that humans could theoretically supervise as permissible; the categorical reading would forbid such systems entirely.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, categorical_prohibition_advocates, excluded,
    organized, generational, constrained, global).

% Arms control verification experts, technical standards bodies, and compliance auditors assess whether particular systems meet the requirement for 'irreducible human agency' in targeting. They develop technical specifications, conduct testing, and produce certification reports. Their assessments determine enforcement outcomes but do not set policy.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, verification_and_compliance_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a consistent global rule for what kinds of decisions must remain under human control in armed conflict, solving the coordination problem of preventing a race to the bottom in autonomous weapons deployment. Absent the rule, states fear that competitors will deploy fully autonomous systems, creating pressure to follow; the rule coordinates on a boundary that preserves human judgment as non-delegable.
% TRANSFER_FUNCTION: Transfers operational capability and technological advantage from autonomous weapons developers and military forces seeking faster decision cycles to IHL interpretive authorities (who maintain authority over what counts as 'human agency') and to civilian protection frameworks (which depend on human deliberation at targeting moments).
% ABSENT_VOICES: States pursuing military technological advantage through autonomous systems, and weapons developers prioritizing technical performance over human-in-the-loop constraints, are excluded from this reading's framing. They would argue that outcomes, not means, should matter; their objections are structurally shut out by the reading's core premise that human judgment is irreducible, not instrumental.
% DISAPPEARANCE_RATIONALE: If this constraint—the requirement that irreducible human judgment remain in lethal targeting decisions—disappeared overnight, military forces and weapons developers would rapidly deploy fully autonomous targeting systems, verification and compliance regimes would collapse, and the interpretive authority of IHL would shift from gate-keeping human agency requirements to negotiating performance standards. The weapons landscape would reorganize around technical capability rather than human judgment preservation.
% FOUNDING_PROBLEM: IHL's distinction and proportionality obligations have historically required human judgment to assess context, intent, and civilian presence at the moment of lethal force application. As autonomous weapons systems advanced from fire-control aids to potential autonomous decision-makers, the question emerged: can machines make the moral distinctions IHL requires, or does the law itself depend on human agency for its operation?
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and UNGA reports on autonomous weapons attest that the founding problem remains live: emerging systems are pushing toward full autonomy and raising questions about IHL compliance. Weapons developers attest that the problem is already partly solved: current systems achieve high distinction/proportionality performance. Civil society and some states attest that the problem reflects a deeper principle-based concern (Martens Clause, human dignity) that no technical solution can address. The contestation is corroborated by testimony and position papers from outside the benefiting parties (military forces seeking efficiency); the disagreement is systematic, not residual.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) and rising through the interval because the constraint's core claim—that human judgment is irreducible—is itself the extraction mechanism: it forecloses entire categories of military technology and reserves authority over what counts as 'sufficient' human control to interpretive bodies. Suppression is slightly higher (0.72) because the constraint must actively foreclose alternatives: without enforcement of the human agency requirement, states and developers would deploy fully autonomous systems. Theater ratio is moderate-low (0.28, rising from 0.12) because the human judgment requirement carries some genuine coordination function (states do coordinate on weapons boundaries) but an increasing share of enforcement activity defends the interpretive authority's gate-keeping role rather than genuine human judgment certification. The measurement series run on one shared time grid; all metrics are authored at t0, t5, t10, t15, t20, t25, t30, t35. The trajectory shows extractiveness plateauing after t15 and suppression hardening—the rise in enforcement intensity reflects the constraint hardening into institutional practice as weapons technology advances. Theater ratio also rises gradually, suggesting that over time more of the activity devoted to 'human agency certification' becomes defensive performance by authorities and developers rather than genuine deliberation about what humans could actually judge.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence should be striking: from the IHL interpretive authority's seat (institutional, analytical exit), the constraint coordinates states on a non-negotiable principle and preserves the authority's role as custodian of humanitarian law. The engine will compute this seat as near-beneficiary (d low, χ low/negative). From the military developer's seat (institutional, constrained exit, identity-locked to autonomous weapons programs), the same constraint operates as enforced foreclosure: entire system designs are prohibited, certification burdens rise, and the developer has no authority over what counts as 'sufficient' human control. The engine will compute this seat as near-target (d high, χ high). From the civilian population's seat (powerless, trapped exit), the constraint is nominally protective but structurally immaterial—whether targeting decisions are made by humans or machines, civilians remain trapped in the conflict zone. The engine will compute this seat as symmetric to slightly extractive (d ~0.5-0.6). The perspectival gap reflects that the same constraint operates as coordination, extraction, and protection depending on the seat's structural relationship to the requirement.
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities are the structural beneficiaries: they maintain authority over what counts as 'human agency' and set the standard that weapons systems must meet. No external party can override their interpretation. Their exit options are analytical (they are not trapped; they can refine their position), and their power is institutional (they are recognized as authoritative interpreters). Directionality for this seat should be low (near-beneficiary). Military operational efficiency and autonomous weapons developers are the structural victims: they pay the cost of slower decision cycles, scaled personnel requirements, and technological foreclosure. Their exit options are constrained (they cannot simply abandon human-in-the-loop requirements if they operate in a jurisdiction enforcing this reading), and their power is powerful-but-constrained by the requirement. Directionality for these seats should be high (near-target). Civilians benefit from the principle that human judgment sits at the targeting decision but have no structural leverage over the constraint—they are trapped. Combatants have asymmetric directionality: they benefit from assurance that adversaries use human judgment (d low on that dimension) but pay the cost of slower opposing forces (d high on that dimension), yielding near-symmetric d (~0.45-0.55). The coercion grid shows that suppression is highest at the organizational and structural levels (military forces and defense contractors) and lowest at the individual level (where operators and soldiers experience the constraint as a normal operational requirement). Accessibility of alternatives collapses most at the structural level (the reading forecloses entire weapons categories) and least at the individual level (where a soldier has few alternatives regardless of weapons policy).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids trivial misclassification by anchoring to a non-trivial coordination function: states do genuinely face a coordination problem (fear that competitors will deploy fully autonomous systems, creating pressure to follow; the constraint solves this by coordinating on a boundary). However, the constraint shows early signs of mandatrophy: the founding problem (what should be the role of human judgment in autonomous weapons?) is substantially contested, and the founding_problem_status is authoritatively disputed. The ICRC and some states attest the founding problem is live; weapons developers and military strategists attest it is partly solved (current systems achieve high distinction/proportionality performance); civil society attests the problem reflects a deeper principle-based concern that no technical solution can address. The theater_ratio rises from 0.12 to 0.28 over the interval, suggesting that an increasing share of enforcement activity is defensive (authorities defending their interpretive centrality, developers proving compliance) rather than genuine deliberation. The measured extraction remains high (0.68 at interval end) because the constraint's core claim—irreducible human judgment—is itself the extraction: it forecloses military technology and reserves authority. If the founding problem were clearly dead (if truly all stakeholders agreed human judgment was no longer necessary), the constraint would shift toward piton status (persisting by inertia). The current state is tangled_rope under stress: the coordination function is real but contested, and extraction is substantial because the boundary (what counts as 'irreducible' human judgment) is subject to interpretive expansion and contraction. The omega variables document the irreducible uncertainties driving the mandatrophy risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreducible_human_judgment_operationalization,
    'What constitutes ''irreducible human judgment'' in the targeting decision? Is it: (a) human authority to veto/override the system in real time, (b) human deliberation before the engagement (pre-authorization of targets), (c) human understanding of the system''s decision process, or (d) human moral accountability after the fact?',
    'Regulatory guidance from ICRC or UN Protocol Review Conference articulating the specific cognitive and decisional content required. Case-law development from international criminal tribunals assessing whether particular systems met the requirement. Field reporting on actual human-machine targeting interactions.',
    'Different operationalizations yield different systems as compliant: real-time veto authority is most restrictive and most burdensome operationally; post-hoc accountability is least restrictive but may not satisfy the principle if humans cannot actually understand or deliberate about machine decisions. The width of the gap between strict and permissive interpretations determines how much of the autonomous weapons landscape remains legally open.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irreducible_human_judgment_operationalization, conceptual, 'The definition of irreducible human judgment is contested and unresolved, creating interpretive uncertainty that affects which systems are lawful.').

omega_variable(
    meaningful_human_control_vs_machine_performance,
    'Can high-performance autonomous systems (that demonstrably achieve or exceed human distinction/proportionality accuracy) satisfy IHL if humans cannot understand their decision process, or does the law require that humans be able to deliberate about the targeting choice regardless of outcomes?',
    'Technical analysis of interpretability/explainability in autonomous targeting systems versus empirical comparative studies of human and machine targeting accuracy. Philosophical analysis of whether IHL''s principles are outcomes-based or means-based. Regulatory disputes between states pursuing different interpretations.',
    'If outcomes can substitute for deliberative capability (high performance + human oversight is sufficient), the constraint becomes less extractive and more of an actual coordination boundary. If deliberative capability is required regardless of outcomes, the constraint remains highly extractive because most advanced systems achieve high performance through processes humans cannot understand and supervise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_human_control_vs_machine_performance, empirical, 'Whether IHL principles are satisfied by outcome performance alone or require human deliberative capability over the targeting choice.').

omega_variable(
    reading_foreclosure_asymmetry,
    'Does the human_agency_reading actually foreclose the categorical_prohibition_reading, or do they describe different scope boundaries (human-supervised autonomy permitted vs. all autonomy forbidden)? Are these logically contradictory or merely different policy preferences?',
    'Philosophical analysis of whether ''meaningful human control is sufficient'' and ''all autonomous weapons are forbidden'' can coexist in a single legal framework. Examination of how states and advocacy organizations actually deploy these readings: do they treat them as mutually exclusive or as different levels of prescription the same state might hold?',
    'If the readings coexist (not logically foreclosing), the network structure is simpler and both readings remain live. If the reading truly forecloses, the coexistence of both readings in different state policies suggests either: (a) states are incoherent, (b) the readings describe different elements (a reading is more granular than a binary choice permits), or (c) one reading is more authoritative and the other is fringe. This determines whether the kernel dispute is over-determined or under-determined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_asymmetry, conceptual, 'Whether the human agency and categorical prohibition readings are logically foreclosing or merely different policy preferences held by different parties.').

omega_variable(
    interpretive_authority_extraction_vs_coordination,
    'Does IHL interpretive authority''s role in defining ''irreducible human judgment'' constitute a genuine coordination function (resolving a collective action problem about weapons boundaries), or is the interpretive authority primarily extracting centrality by reserving the right to determine what counts as compliant?',
    'Historical analysis of how IHL interpretive standards have changed over time. Evidence of whether states voluntarily adopt the human agency requirement without enforcement pressure (voluntary coordination), or whether adoption is driven by coercion, market access, or normative pressure from third parties. Evidence of whether the interpretive body expands or contracts its definition of ''sufficient'' human control over time.',
    'If primarily coordination, the constraint is genuinely tangled_rope: states coordinate on a boundary (good) and IHL authorities maintain centrality (extraction). If primarily extraction, the constraint is closer to snare: the interpretive authority uses the coordination framing to reserve authority over weapons legality. The distinction affects classification and remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_extraction_vs_coordination, empirical, 'Whether IHL interpretive authority''s role in defining compliance is driven by genuine coordination or institutional extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(ihl__tr_t0, observed).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(ihl__tr_t5, observed).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(ihl__tr_t10, observed).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(ihl__tr_t15, observed).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(ihl__tr_t20, projected).
narrative_ontology:measurement(ihl__tr_t25, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(ihl__tr_t25, projected).
narrative_ontology:measurement(ihl__tr_t30, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(ihl__tr_t30, projected).
narrative_ontology:measurement(ihl__tr_t35, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(ihl__tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(ihl__be_t0, observed).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(ihl__be_t5, observed).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(ihl__be_t10, observed).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(ihl__be_t15, observed).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ihl__be_t20, projected).
narrative_ontology:measurement(ihl__be_t25, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ihl__be_t25, projected).
narrative_ontology:measurement(ihl__be_t30, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(ihl__be_t30, projected).
narrative_ontology:measurement(ihl__be_t35, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(ihl__be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(ihl__su_t0, observed).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(ihl__su_t5, observed).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(ihl__su_t10, observed).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(ihl__su_t15, observed).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ihl__su_t20, projected).
narrative_ontology:measurement(ihl__su_t25, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(ihl__su_t25, projected).
narrative_ontology:measurement(ihl__su_t30, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(ihl__su_t30, projected).
narrative_ontology:measurement(ihl__su_t35, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(ihl__su_t35, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(ihl__grid_01, ihl_distinction_proportionality__human_agency_reading, accessibility_collapse(class), 0, 0.52).
narrative_ontology:measurement(ihl__grid_02, ihl_distinction_proportionality__human_agency_reading, accessibility_collapse(class), 35, 0.58).
narrative_ontology:measurement(ihl__grid_03, ihl_distinction_proportionality__human_agency_reading, accessibility_collapse(individual), 0, 0.38).
narrative_ontology:measurement(ihl__grid_04, ihl_distinction_proportionality__human_agency_reading, accessibility_collapse(individual), 35, 0.42).
narrative_ontology:measurement(ihl__grid_05, ihl_distinction_proportionality__human_agency_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(ihl__grid_06, ihl_distinction_proportionality__human_agency_reading, accessibility_collapse(organizational), 35, 0.75).
narrative_ontology:measurement(ihl__grid_07, ihl_distinction_proportionality__human_agency_reading, accessibility_collapse(structural), 0, 0.75).
narrative_ontology:measurement(ihl__grid_08, ihl_distinction_proportionality__human_agency_reading, accessibility_collapse(structural), 35, 0.81).
narrative_ontology:measurement(ihl__grid_09, ihl_distinction_proportionality__human_agency_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(ihl__grid_10, ihl_distinction_proportionality__human_agency_reading, resistance(class), 35, 0.64).
narrative_ontology:measurement(ihl__grid_11, ihl_distinction_proportionality__human_agency_reading, resistance(individual), 0, 0.38).
narrative_ontology:measurement(ihl__grid_12, ihl_distinction_proportionality__human_agency_reading, resistance(individual), 35, 0.43).
narrative_ontology:measurement(ihl__grid_13, ihl_distinction_proportionality__human_agency_reading, resistance(organizational), 0, 0.71).
narrative_ontology:measurement(ihl__grid_14, ihl_distinction_proportionality__human_agency_reading, resistance(organizational), 35, 0.78).
narrative_ontology:measurement(ihl__grid_15, ihl_distinction_proportionality__human_agency_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(ihl__grid_16, ihl_distinction_proportionality__human_agency_reading, resistance(structural), 35, 0.68).
narrative_ontology:measurement(ihl__grid_17, ihl_distinction_proportionality__human_agency_reading, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(ihl__grid_18, ihl_distinction_proportionality__human_agency_reading, stakes_inflation(class), 35, 0.54).
narrative_ontology:measurement(ihl__grid_19, ihl_distinction_proportionality__human_agency_reading, stakes_inflation(individual), 0, 0.35).
narrative_ontology:measurement(ihl__grid_20, ihl_distinction_proportionality__human_agency_reading, stakes_inflation(individual), 35, 0.41).
narrative_ontology:measurement(ihl__grid_21, ihl_distinction_proportionality__human_agency_reading, stakes_inflation(organizational), 0, 0.65).
narrative_ontology:measurement(ihl__grid_22, ihl_distinction_proportionality__human_agency_reading, stakes_inflation(organizational), 35, 0.72).
narrative_ontology:measurement(ihl__grid_23, ihl_distinction_proportionality__human_agency_reading, stakes_inflation(structural), 0, 0.71).
narrative_ontology:measurement(ihl__grid_24, ihl_distinction_proportionality__human_agency_reading, stakes_inflation(structural), 35, 0.78).
narrative_ontology:measurement(ihl__grid_25, ihl_distinction_proportionality__human_agency_reading, suppression(class), 0, 0.42).
narrative_ontology:measurement(ihl__grid_26, ihl_distinction_proportionality__human_agency_reading, suppression(class), 35, 0.51).
narrative_ontology:measurement(ihl__grid_27, ihl_distinction_proportionality__human_agency_reading, suppression(individual), 0, 0.28).
narrative_ontology:measurement(ihl__grid_28, ihl_distinction_proportionality__human_agency_reading, suppression(individual), 35, 0.36).
narrative_ontology:measurement(ihl__grid_29, ihl_distinction_proportionality__human_agency_reading, suppression(organizational), 0, 0.64).
narrative_ontology:measurement(ihl__grid_30, ihl_distinction_proportionality__human_agency_reading, suppression(organizational), 35, 0.74).
narrative_ontology:measurement(ihl__grid_31, ihl_distinction_proportionality__human_agency_reading, suppression(structural), 0, 0.58).
narrative_ontology:measurement(ihl__grid_32, ihl_distinction_proportionality__human_agency_reading, suppression(structural), 35, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__human_agency_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_military_advantage).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, martens_clause_humanity_principle).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading family decomposing the kernel: IHL's distinction and proportionality obligations in the age of autonomous weapons (kernel_id: ihl_distinction_proportionality). The three readings are structurally distinct claims with different ε values, beneficiary/victim structures, and persistence dynamics. Each is authored as a separate constraint story with its own cs_structure.reading_relations data linking to siblings. The human_agency_reading (this story) forecloses the categorical_prohibition_reading (different core premise about whether supervised autonomy is permissible) and coexists with the outcomes_based_reading (different parties hold these simultaneously, neither logically eliminates the other). See commentary.kernel_context for the reading relationship diagram.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__human_agency_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
