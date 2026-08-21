% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: IHL Human Agency Requirement for Lethal Force (Distinction/Proportionality Reading)
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'human agency' reading of International
 *   Humanitarian Law's (IHL) distinction and proportionality obligations,
 *   specifically as applied to autonomous weapons systems. It asserts that
 *   irreducible human moral judgment is required at the moment of lethal
 *   force application, prohibiting the delegation of life/death decisions to
 *   machines based on Martens Clause principles of humanity. This reading
 *   effectively suppresses the development and deployment of fully autonomous
 *   lethal systems, authorizing only human-supervised autonomy. It is a
 *   contested interpretation within the broader debate on autonomous weapons.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.78).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.85).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Human Agency Requirement for Lethal Force (Distinction/Proportionality Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '5e957c33-5654-4bed-ac53-66623b6c6263').
narrative_ontology:cs_kernel_codification('5e957c33-5654-4bed-ac53-66623b6c6263', formalized).
narrative_ontology:cs_authority_grounding('5e957c33-5654-4bed-ac53-66623b6c6263', lineage).
narrative_ontology:cs_interpretation_layer_present('5e957c33-5654-4bed-ac53-66623b6c6263').
narrative_ontology:cs_reading_relation('5e957c33-5654-4bed-ac53-66623b6c6263', ihl_distinction_proportionality__outcomes_based_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e957c33-5654-4bed-ac53-66623b6c6263', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('5e957c33-5654-4bed-ac53-66623b6c6263', foundational, human_moral_judgment_is_irreducible).
narrative_ontology:cs_axiom_status(human_moral_judgment_is_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('5e957c33-5654-4bed-ac53-66623b6c6263', human_moral_judgment_is_irreducible, deontological).
narrative_ontology:cs_axiom('5e957c33-5654-4bed-ac53-66623b6c6263', foundational, martens_clause_prohibits_machine_killing).
narrative_ontology:cs_axiom_status(martens_clause_prohibits_machine_killing, holdable).
narrative_ontology:cs_axiom_grounding('5e957c33-5654-4bed-ac53-66623b6c6263', martens_clause_prohibits_machine_killing, deontological).
narrative_ontology:cs_reference_frame('5e957c33-5654-4bed-ac53-66623b6c6263', ihl_human_centric_interpretation).
narrative_ontology:cs_drift_state('5e957c33-5654-4bed-ac53-66623b6c6263', contemporary_ai_advances, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5e957c33-5654-4bed-ac53-66623b6c6263', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, human_rights_advocates).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_commanders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations like the ICRC and UN bodies that interpret and promote IHL. They benefit from this reading by maintaining human centrality in lethal decision-making, reinforcing their mandate and the moral authority of IHL itself. They actively advocate for this interpretation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, agenda_setter,
    institutional, generational, analytical, global).

% The drive within military forces to achieve faster, more precise, and less risky operations through automation. This reading imposes a cost by requiring human oversight, which can slow down decision cycles and limit the full potential of autonomous systems, thereby reducing perceived efficiency gains.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency, payer,
    institutional, immediate, constrained, global).

% Companies and research institutions developing AI-powered weapons systems. This reading restricts their market and development pathways by prohibiting fully autonomous lethal functions, forcing them to integrate human-in-the-loop or human-on-the-loop designs, which adds complexity and cost.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    organized, biographical, constrained, global).

% NGOs and civil society groups advocating for the protection of civilians and human dignity in armed conflict. This reading aligns with their goals by preserving human accountability and moral judgment in lethal force decisions, preventing the dehumanization of warfare.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Responsible for authorizing and overseeing the use of force. While they seek efficiency, they also bear the legal and moral burden of IHL compliance. This reading places a direct obligation on them to ensure human judgment is present, potentially increasing their cognitive load and legal risk if systems fail.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_commanders, payer,
    powerful, biographical, constrained, national).

% Military strategists and technologists who argue that IHL should focus on outcomes (e.g., minimizing civilian casualties) rather than the means (human vs. machine decision-making). They are excluded from the core interpretive framework of this reading, which prioritizes human agency over mere performance metrics.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, outcomes_based_proponents, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interpretation of IHL to ensure that the principles of distinction and proportionality, particularly regarding civilian protection, are applied through human moral judgment, thereby maintaining the ethical and legal framework of warfare.
% TRANSFER_FUNCTION: Transfers the ultimate responsibility for lethal force decisions from potentially autonomous machines back to human operators, imposing a 'human judgment' cost on military efficiency and autonomous weapons development, while transferring moral authority and interpretive centrality to IHL bodies.
% ABSENT_VOICES: Proponents of purely outcomes-based interpretations of IHL, who argue that machine performance could exceed human capabilities in applying distinction and proportionality, are largely absent from the interpretive consensus that this reading seeks to establish. They would advocate for a technology-neutral approach.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the legal and ethical landscape for autonomous weapons would fundamentally shift. Militaries would rapidly accelerate development and deployment of fully autonomous lethal systems, IHL interpretive bodies would lose significant moral authority, and the debate would pivot entirely to performance metrics, potentially leading to a 'race to the bottom' in ethical considerations.
% FOUNDING_PROBLEM: The problem of ensuring that lethal force in armed conflict is applied with moral judgment, accountability, and adherence to principles of distinction and proportionality, particularly in the face of emerging autonomous technologies that could remove humans from the decision loop.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC, UN experts, and numerous human rights organizations consistently corroborate that this problem is live and urgent, citing rapid advancements in AI and robotics. While some military strategists might contest the 'human judgment' aspect, they generally acknowledge the underlying challenge of IHL compliance with new technologies.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.78) is high because this reading imposes significant costs on military operational efficiency and autonomous weapons developers by restricting automation. Suppression (0.85) is also high, reflecting the active and ongoing efforts by IHL authorities and advocates to enforce this interpretation against strong technological and strategic pressures. The theater ratio is low (0.1) as the interpretive authorities are genuinely committed to this principle, not merely performing. Accessibility collapse is moderate (0.65) as alternatives (fully autonomous systems) are conceptually clear but legally and ethically foreclosed by this reading. Resistance is high (0.7) from military and industry actors seeking greater autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of IHL interpretive authorities, this constraint is a necessary 'rope' to maintain the integrity of IHL and human dignity. From the perspective of military efficiency and autonomous weapons developers, it is a 'snare' that imposes unnecessary costs and hinders technological progress, potentially putting their forces at a disadvantage. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities and human rights advocates are beneficiaries, as this reading reinforces their mandate and ethical positions. Military operational efficiency, autonomous weapons developers, and military commanders are payers, bearing the costs of restricted autonomy and increased oversight. Proponents of outcomes-based approaches are excluded, as their core premise is incompatible with this reading's emphasis on human agency.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_feasibility_of_human_supervision,
    'Is ''meaningful human control'' or ''human-on-the-loop'' supervision technically feasible and effective for all types of lethal autonomous weapons systems, especially in high-speed, complex environments?',
    'Empirical testing and operational deployment of human-supervised systems in realistic scenarios, followed by independent technical and ethical review.',
    'If found infeasible or ineffective, this reading''s practical application would be severely challenged, potentially forcing a re-evaluation towards either full prohibition or outcomes-based approaches. If feasible, it strengthens the reading''s viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_feasibility_of_human_supervision, empirical, 'The practical limits of human oversight in autonomous lethal force decisions.').

omega_variable(
    definition_of_moral_judgment,
    'What constitutes ''irreducible human moral judgment'' in the context of lethal force, and can it be sufficiently operationalized for legal and technical compliance?',
    'Development of clear legal guidelines, ethical frameworks, and technical standards that define and measure the presence of human moral judgment in decision-making processes, with broad international consensus.',
    'Lack of a clear definition could render this reading unenforceable or lead to ''ethics washing'' where superficial human involvement is claimed as ''moral judgment.'' A robust definition would solidify its legal and ethical force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_of_moral_judgment, conceptual, 'The conceptual clarity and operationalization of ''human moral judgment'' in IHL.').

omega_variable(
    martens_clause_scope,
    'To what extent does the Martens Clause, invoking ''principles of humanity and the dictates of public conscience,'' categorically prohibit the delegation of lethal force decisions to machines, irrespective of performance?',
    'Further development of international customary law, state practice, and expert interpretations specifically addressing the Martens Clause''s application to autonomous weapons, potentially through a new international treaty.',
    'A strong consensus on categorical prohibition would reinforce this reading''s legal foundation. A more permissive interpretation would weaken its normative force, pushing towards outcomes-based or more limited regulatory approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_scope, conceptual, 'The scope and binding force of the Martens Clause regarding autonomous weapons.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 10, 0.81).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 15, 0.83).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_development_norms).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, military_ai_ethics_guidelines).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ihl_distinction_proportionality' kernel. It focuses on human agency, distinct from an outcomes-based or categorical prohibition reading. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
