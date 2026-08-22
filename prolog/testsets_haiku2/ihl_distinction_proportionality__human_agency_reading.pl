% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: IHL Human Agency Requirement for Lethal Targeting Decisions
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   The human-agency reading of IHL's distinction and proportionality
 *   requirements asserts that Martens Clause principles of humanity mandate
 *   irreducible human moral judgment at the moment of lethal force
 *   application. Under this reading, delegating targeting decisions to
 *   autonomous machines violates IHL regardless of technical performance
 *   equivalence. The constraint operates through the interpretive authority
 *   of bodies like the ICRC, which enforce this reading as authoritative
 *   doctrine. It suppresses autonomous weapons development, imposes
 *   operational efficiency costs on militaries that adopt it, and centralizes
 *   legal authority over life-death decisions in human judgment rather than
 *   in machine logic or performance metrics. The constraint's persistence
 *   depends on maintaining the institutional and legal consensus that
 *   humanity requires human judgment, and on militaries' choice to adopt this
 *   interpretation. Alternative readings (outcomes-based, categorical
 *   prohibition) exist and are held by different states and military
 *   establishments; this story authorizes only the human-agency reading as a
 *   clean ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - IHL interpretive authorities (ICRC, treaty bodies) — agenda-setters, institutional power, maintain centrality of this reading
 *   - Humanitarian advocacy organizations — beneficiaries, organized power, amplify and defend the human-agency requirement
 *   - Military operational efficiency — victim, powerful institutional actor, bears suppression costs
 *   - Autonomous weapons developers — victims, moderate power, face categorical suppression of system development
 *   - National militaries adopting this reading — mixed role (agenda-setter + payer), institutional power, maintain enforcement while bearing operational costs
 *   - States rejecting this reading — excluded, institutional power, unable to shape the authoritative interpretation
 *   - Combat operators — dual role (beneficiary + payer), moderate power, trapped immediate horizon, bear moral load while gaining decision authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.72).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.88).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Human Agency Requirement for Lethal Targeting Decisions").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, 'a1719835-b7a5-45b6-b6d9-64f43f34e967').
narrative_ontology:cs_kernel_codification('a1719835-b7a5-45b6-b6d9-64f43f34e967', formalized).
narrative_ontology:cs_authority_grounding('a1719835-b7a5-45b6-b6d9-64f43f34e967', lineage).
narrative_ontology:cs_interpretation_layer_present('a1719835-b7a5-45b6-b6d9-64f43f34e967').
narrative_ontology:cs_reading_relation('a1719835-b7a5-45b6-b6d9-64f43f34e967', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1719835-b7a5-45b6-b6d9-64f43f34e967', ihl_distinction_proportionality__outcomes_based_reading, influences).
narrative_ontology:cs_axiom('a1719835-b7a5-45b6-b6d9-64f43f34e967', foundational, human_judgment_irreducible_to_lethal_decision).
narrative_ontology:cs_axiom_status(human_judgment_irreducible_to_lethal_decision, holdable).
narrative_ontology:cs_axiom_grounding('a1719835-b7a5-45b6-b6d9-64f43f34e967', human_judgment_irreducible_to_lethal_decision, deontological).
narrative_ontology:cs_axiom('a1719835-b7a5-45b6-b6d9-64f43f34e967', foundational, martens_clause_humanity_requires_human_agency).
narrative_ontology:cs_axiom_status(martens_clause_humanity_requires_human_agency, holdable).
narrative_ontology:cs_axiom_grounding('a1719835-b7a5-45b6-b6d9-64f43f34e967', martens_clause_humanity_requires_human_agency, deontological).
narrative_ontology:cs_reference_frame('a1719835-b7a5-45b6-b6d9-64f43f34e967', martens_clause_human_judgment_standard).
narrative_ontology:cs_drift_state('a1719835-b7a5-45b6-b6d9-64f43f34e967', contemporary_autonomous_weapons_maturity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a1719835-b7a5-45b6-b6d9-64f43f34e967', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, humanitarian_advocacy_organizations).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, combat_soldiers_and_operators).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, affected_civilian_populations).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, national_militaries_enforcing_constraint).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, combat_soldiers_and_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The International Committee of the Red Cross (ICRC) and other authoritative IHL bodies articulate and enforce the interpretation that Martens Clause principles of humanity mandate human judgment in lethal targeting. They convene treaty bodies, issue advisory opinions, and influence military doctrine adoption. They maintain centrality as the interpretive arbiter of what humanity requires in warfare.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, agenda_setter,
    institutional, generational, arbitrage, universal).

% NGOs and civil society organizations advocating for humanitarian limits on weapons technology support and amplify the human-agency requirement as a bright-line rule. They benefit from the categorical framing ('human must decide') which is easier to defend and mobilize around than performance-based standards. Their exit is relatively unconstrained: if this reading weakens, they can shift advocacy to alternative framings.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, humanitarian_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).

% Military institutions bear the operational costs of maintaining human-in-the-loop authority over lethal targeting: trained personnel, decision authority structures, real-time communication architecture, and human cognitive bandwidth during dynamic combat. The constraint suppresses development of systems that would reduce human workload or decision latency. Exit options are politically constrained: openly violating IHL attracts sanctions and delegitimizes the military internationally.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency, payer,
    powerful, biographical, constrained, global).

% Defense contractors, robotics researchers, and AI laboratories developing autonomous targeting systems face suppression: the constraint categorically denies lawful deployment of systems that make lethal decisions without human authorization. Funding for such research is politically constrained; customer bases face reputational and legal risk. Developers can exit by pivoting to civilian robotics or investing in 'human-supervised autonomy' that preserves the human decision point.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    moderate, biographical, constrained, global).

% Militaries that adopt and enforce the human-agency requirement in their targeting doctrine benefit from international legitimacy and humanitarian reputation but also bear operational efficiency costs. Their exit is mobile: militaries in jurisdictions outside IHL enforcement, or states that reinterpret the Martens Clause differently, can adopt systems that violate this reading.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, national_militaries_enforcing_constraint, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, national_militaries_enforcing_constraint, payer).

% Militaries and states that adopt alternative readings (outcomes-based or categorical prohibition) are structurally excluded from the dominant IHL interpretive consensus. They would argue for performance-based or alternative legal standards but are kept out of the conversation by the same institutional structures that enforce the human-agency reading. Their exclusion is the enforcement object itself.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_alternative_readings, excluded,
    institutional, generational, trapped, global).

% Individual soldiers and targeting operators benefit from a decision framework that places moral authority and accountability on them as agents. They also bear the cognitive and moral load: decisions of lethal force remain their responsibility, including the weight of distinguishing lawful targets and assessing proportionality. Their exit is trapped: they cannot refuse the assignment or change the legal framework.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, combat_soldiers_and_operators, beneficiary,
    moderate, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, combat_soldiers_and_operators, payer).

% Civilian populations in conflict zones theoretically benefit from the human-agency requirement: human operators will exercise moral judgment and assess proportionality in targeting. Whether this benefit is realized depends on actual operator training and adherence to IHL. They bear no direct operational cost but face existential risk from targeting errors. Their exit is trapped: they cannot leave the conflict zone or influence targeting decisions.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, affected_civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% Academic researchers, legal scholars, and analytical organizations examine the human-agency reading's coherence, enforcement, and consequences. They produce evidence about whether human operators achieve distinction/proportionality better than systems and assess the reading's fitness relative to alternatives. They take no direct benefit or cost but influence policy through analysis.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, observer_academic_and_analytical, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified interpretive framework for IHL compliance: the Martens Clause principles of humanity and public conscience are centralized in human moral judgment at the moment of lethal decision-making. This solves the coordination problem of how to apply abstract principles (distinction, proportionality, military necessity) in contested, fast-moving combat scenarios — by requiring human operators to be the judges of lawfulness, the framework places responsibility and accountability in a single agent (the decision-maker) rather than distributing it across machines, commanders, and systems.
% TRANSFER_FUNCTION: Transfers operational efficiency and technical capability from military targeting systems to IHL interpretive authorities and humanitarian advocacy organizations. Military actors forgo faster decision cycles, reduced human workload, and greater system autonomy in exchange for international legitimacy and humanitarian reputation. The constraint moves the locus of authority over life-death decisions from machine autonomy to human judgment, which concentrates legal authority in IHL interpretive bodies and their interpretation of what humanity requires.
% ABSENT_VOICES: States and militaries that have adopted alternative readings (outcomes-based or categorical prohibition readings) are structurally excluded from the dominant IHL interpretive consensus. Autonomous weapons developers and advocates for technology-neutral legal standards that assess lawfulness by performance rather than means are kept out of the conversation by the same institutional structures that enforce the human-agency reading. They would argue that lawfulness should be measured by actual distinction and proportionality outcomes rather than by whether a human pressed the button, but this argument is systematically excluded from the authoritative interpretation.
% DISAPPEARANCE_RATIONALE: If the human-agency requirement disappeared overnight, military targeting systems would rapidly expand autonomous decision authority. Defense contractors would accelerate development of fully autonomous systems. Military doctrines would shift to performance-based compliance metrics. The global weapons market would reorganize around technical capability rather than human judgment requirements. IHL interpretive authority would fragment — different states would adopt different readings (outcomes-based, categorical prohibition, or no specific requirement). The humanitarian advocacy position would have to remobilize around alternative arguments or categorical prohibition. International humanitarian law compliance assessment would shift from auditing human decision-making and operator training to measuring system performance metrics.
% FOUNDING_PROBLEM: In mid-20th-century armed conflict, weapons systems (artillery, aircraft, naval systems) operated at ranges and speeds that made real-time human targeting judgment impossible. IHL doctrine evolved to govern decision-making at points where human judgment could actually apply — at the point of final authorization or immediate targeting decision. As autonomous weapon systems matured, the question became whether human judgment could remain central as system speeds increased and decision complexity grew. The Martens Clause — the founding principle that humanity and public conscience are irreducible constraints on warfare — was invoked to argue that regardless of technical capability, moral judgment cannot be delegated to machines.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and major humanitarian organizations attest the founding problem is live and urgent: as autonomous system capability increases, maintaining human judgment in lethal decisions is increasingly necessary and increasingly difficult. Military practitioners counter that the founding problem is partly solved by technical integration of human-machine teaming and that the constraint is now protecting an outdated doctrine. States and military forces implementing outcomes-based readings attest that the founding problem is about achieving distinction and proportionality, not about who makes the decision. Independent research from outside the IHL interpretive community and military establishments (academic roboticists, international relations scholars, technology governance researchers) documents the live technical and policy dispute: whether the founding requirement (humanity in lethal decisions) is best served by mandatory human authority or by performance-based compliance frameworks.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.72) and rising because the constraint concentrates authority over lethal decisions in human judgment and in the interpretive bodies that define what humanity requires. The measurement series shows extractiveness rising from 0.48 at t=0 (early period when autonomous systems were technically immature and the constraint was less costly) to 0.72 at t=20 (contemporary period when autonomous system capability has advanced but the constraint is more actively enforced and more costly to military operations). Suppression is very high (0.88) and stable because the constraint categorically denies lawful deployment of fully autonomous lethal systems — it is an active suppression of a class of technology and a set of alternative interpretations. Theater_ratio is moderate (0.42) and rising slowly because while the constraint has a genuine coordination function (establishing who bears responsibility for targeting decisions), a growing share of its enforcement effort defends the human-judgment requirement against technical challenges, and the humanitarian rhetoric about humanity is increasingly performative as autonomous systems demonstrate comparable or superior distinction/proportionality in some contexts. The temporal pattern reflects the constraint's aging: it was born in a context where human judgment was practically necessary (weapons systems operated at speeds and ranges humans could not access in real time); as technical capability expanded, maintaining the constraint required increasing active suppression and defensive justification. Claimed type is Tangled Rope because it coordinates responsibility and accountability (genuine coordination function) while also extracting operational efficiency from military actors and suppressing a major class of weapons development (asymmetric extraction requiring active enforcement).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (military operators, autonomous weapons developers) and the agenda-setter/beneficiary seats (IHL interpretive authorities, humanitarian organizations) should compute to different types. From the ICRC and humanitarian advocacy perspective, this is a Rope-like coordination function (establishes shared responsibility standards) with modest extraction. From the military and developer perspective, it is a Snare (categorical suppression of capability, enforcement by institutional dominance, no viable exit). The engine computes this divergence per-seat from the structural data. The claimed type is Tangled Rope because the arrangement has genuine coordination (responsibility and accountability are centralized in human judgment) but also has asymmetric extraction (military actors bear suppression without proportional benefit). The seated divergence reflects the fact that this is precisely a tangled constraint: those who coordinate benefit from coordination, those who pay bear suppression and receive no coordination benefit. The human-agency requirement solves a real problem (who bears responsibility for targeting decisions?) for some seats (IHL authorities, humanitarian advocates) but creates pure extraction for others (military operational efficiency).
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities (agenda-setters) benefit from maintaining this reading's dominance: they preserve institutional centrality in defining what humanity and IHL require, they gain authority over military targeting doctrine, and they collect prestige and influence from being seen as the defenders of humanitarian principle. Their directionality is low (near-beneficiary: d ≈ 0.15-0.25). Military actors and autonomous weapons developers face suppression of capability and efficiency gains; they bear extraction. Their directionality is high (d ≈ 0.75-0.90). Humanitarian advocacy organizations benefit from the bright-line rule without bearing direct operational cost; they have unconstrained exit (can shift advocacy strategies if this reading weakens); their directionality is low (d ≈ 0.20). Combat operators face a mixed situation: they gain moral authority and decision agency (beneficiary aspect), but they also bear the cognitive and moral burden of lethal judgment and the operational constraint of human-loop latency (payer aspect). Their directionality is near-symmetric (d ≈ 0.45-0.55). The engine derives directionality from beneficiary/victim declarations and exit options; the structural asymmetry emerges from the fact that agenda-setters and beneficiaries have unconstrained or mobile exit, while military actors and developers have constrained exit and face active enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits signs of mandatrophy: the founding problem was genuine (as technical capability expanded, maintaining human judgment became a real issue). The founding problem status is now contested: IHL authorities and humanitarian advocates maintain that the problem is live and urgent (autonomous systems are advancing and must be constrained), while military practitioners and technology developers argue that the problem is partly solved (human-machine teaming can maintain human accountability at higher speeds) or that the constraint is now a relic of doctrine written for slower weapons systems. The theater_ratio is rising (0.25→0.42 over the interval), suggesting that more of the constraint's enforcement energy goes into defending the principle against technical challenges rather than into coordinating responsibility. The constraint's persistence depends increasingly on institutional dominance and enforcement activity rather than on participant agreement about the coordination value. This is classic mandate drift: the original problem (rapid autonomous systems outpacing human judgment) was real; the current problem (IHL authorities maintaining interpretive centrality in the face of technical change) is partly institutional self-preservation. A rising theater_ratio, combined with contested founding-problem status and asymmetric extraction, marks this as approaching Piton classification — a constraint that originated in genuine coordination but now persists partly due to institutional inertia and the cost to beneficiaries of changing interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_judgment_irreducibility_claim,
    'Is human moral judgment in lethal targeting genuinely irreducible (not replicable by machines), or is the human-agency requirement grounded in Martens Clause principles about human dignity and accountability rather than in irreducible cognitive capability?',
    'Empirical comparison of human vs. autonomous system performance on distinction and proportionality tasks; philosophical/legal analysis of whether the Martens Clause concern is about judgment quality or about decision authority and moral responsibility.',
    'If judgment is truly irreducible, the human-agency requirement tracks a structural necessity. If the requirement is grounded in accountability and dignity rather than capability, alternative readings (outcomes-based) could satisfy the founding principle. This distinction determines whether the constraint is a natural limit or a policy choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_judgment_irreducibility_claim, conceptual, 'Whether human judgment is irreducibly necessary or whether the requirement is grounded in principles of accountability and dignity.').

omega_variable(
    operational_cost_vs_humanitarian_benefit,
    'What is the empirical relationship between maintaining human-agency authority and actual improvement in distinction/proportionality outcomes for civilian protection?',
    'Comparative analysis of civilian casualty rates, targeting accuracy, and proportionality compliance in systems that implement human-agency requirements vs. systems with higher autonomous authority. Audit of operator training and compliance rates.',
    'If human-agency systems demonstrably achieve better humanitarian outcomes, the constraint is justified as both a legal requirement and a practical humanitarian measure. If outcomes are not significantly different or if human operators systematically violate the constraint, the cost to military efficiency would appear misaligned with humanitarian benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_cost_vs_humanitarian_benefit, empirical, 'Relationship between human-agency requirements and actual humanitarian outcomes.').

omega_variable(
    martens_clause_scope_ambiguity,
    'Does the Martens Clause''s reference to ''humanity'' and ''public conscience'' require human judgment in decision-making specifically, or does it establish a principle of humanitarian accountability that could be satisfied by systems that achieve humanitarian outcomes through technical means?',
    'Textual and historical analysis of Martens Clause language and legislative intent. Examination of how different states and legal traditions interpret ''humanity'' (as a requirement for human agency vs. as a requirement for humanitarian outcomes). Review of post-hoc interpretation in military practice and case law.',
    'A narrow reading (human judgment is essential to humanity) supports this constraint''s human-agency requirement and forecloses outcomes-based readings. A broad reading (humanity can be satisfied by outcome standards) permits alternative readings to coexist and influences how the constraint is enforced internationally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_scope_ambiguity, conceptual, 'Whether Martens Clause principles require human judgment specifically or can be satisfied by humanitarian outcomes from any source.').

omega_variable(
    identity_locked_military_doctrine_drift,
    'To what extent is military resistance to the human-agency requirement grounded in genuine concerns about operational effectiveness vs. in institutional identity and sunk costs in existing command-and-control doctrine?',
    'Ethnographic and organizational analysis of military institutions'' adoption of autonomy constraints. Interview and archival study of military technology decisions. Assessment of whether the same militaries adopt efficiency-improving constraints in other domains or whether they systematically resist operational cost.',
    'If resistance is partly identity-locked (military institutions define themselves as human-commanded, and autonomous systems threaten that identity), the constraint''s persistence depends on maintaining the institutional narrative even if operational costs could be reduced. This would increase theater_ratio and suggest the constraint is partly Piton rather than pure Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_military_doctrine_drift, empirical, 'Degree to which military resistance to human-agency requirements is operational vs. institutionally identity-driven.').

omega_variable(
    kernel_reading_coexistence_boundary,
    'Can the human-agency reading coexist in a single international legal framework with the outcomes-based reading, or do they represent genuinely incompatible interpretations of IHL?',
    'Legal analysis of whether a state could adopt a framework that permits outcomes-based compliance (autonomous systems allowed if they demonstrably achieve distinction/proportionality equal to human operators) while other states maintain mandatory human-agency requirements. Examination of treaty language and enforcement mechanisms for compatibility.',
    'If the readings are genuinely incompatible, the human-agency reading''s persistence depends on maintaining dominance in the interpretive consensus. If they can coexist, the constraint''s operation depends on which states adopt which reading, and the constraint becomes regionally or faction-specific rather than globally binding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_boundary, conceptual, 'Whether human-agency and outcomes-based readings are genuinely incompatible or can coexist in a single legal framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ihl__tr_t0, observed).
narrative_ontology:measurement(ihl__tr_t4, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 4, 0.29).
narrative_ontology:measurement_basis(ihl__tr_t4, observed).
narrative_ontology:measurement(ihl__tr_t8, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(ihl__tr_t8, observed).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(ihl__tr_t12, observed).
narrative_ontology:measurement(ihl__tr_t16, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(ihl__tr_t16, projected).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(ihl__tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ihl__be_t0, observed).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement_basis(ihl__be_t4, observed).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(ihl__be_t8, observed).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(ihl__be_t12, observed).
narrative_ontology:measurement(ihl__be_t16, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement_basis(ihl__be_t16, projected).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement_basis(ihl__be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement_basis(ihl__su_t0, observed).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 4, 0.79).
narrative_ontology:measurement_basis(ihl__su_t4, observed).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 8, 0.83).
narrative_ontology:measurement_basis(ihl__su_t8, observed).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement_basis(ihl__su_t12, observed).
narrative_ontology:measurement(ihl__su_t16, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 16, 0.87).
narrative_ontology:measurement_basis(ihl__su_t16, projected).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement_basis(ihl__su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__human_agency_reading, 0.14).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested ihl_distinction_proportionality kernel. The human_agency_reading asserts that IHL distinction/proportionality obligations require irreducible human moral judgment at lethal targeting moments. The categorical_prohibition_reading asserts autonomous weapons are per se unlawful. The outcomes_based_reading asserts IHL obligations are satisfied by demonstrated performance equivalence. These are structurally distinct constraints with different ε values, beneficiary/victim structures, and enforcement mechanisms. They should be analyzed as separate stories linked by kernel membership, not as variants of one constraint. The human_agency reading influences the outcomes_based reading (establishes human-judgment baseline against which outcomes are measured) and coexists with the categorical_prohibition reading (both hold that certain autonomous weapons should be prohibited, but for different reasons and with different scope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__human_agency_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
