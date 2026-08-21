% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Categorical Prohibition of Autonomous Weapons (Martens Clause Reading)
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents a reading of International Humanitarian Law
 *   (IHL) and the Martens Clause that advocates for a categorical prohibition
 *   on autonomous weapons systems (LAWS). It asserts that the act of
 *   delegating lethal decision-making to machines inherently violates human
 *   dignity and public conscience, regardless of any potential performance
 *   benefits. This reading aims to ban an entire class of technology, placing
 *   a high extractive cost on military technological advantage. The claimed
 *   type is 'snare' because it imposes a high cost on specific actors (states
 *   with LAWS, LAWS developers) by suppressing an entire technological
 *   pathway, with the coordination story (upholding dignity) serving as cover
 *   for the extraction of military advantage from certain states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.95).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.88).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, snare).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Categorical Prohibition of Autonomous Weapons (Martens Clause Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, 'fe89661b-6647-4c15-97f4-cde8e807090c').
narrative_ontology:cs_kernel_codification('fe89661b-6647-4c15-97f4-cde8e807090c', formalized).
narrative_ontology:cs_authority_grounding('fe89661b-6647-4c15-97f4-cde8e807090c', lineage).
narrative_ontology:cs_interpretation_layer_present('fe89661b-6647-4c15-97f4-cde8e807090c').
narrative_ontology:cs_reading_relation('fe89661b-6647-4c15-97f4-cde8e807090c', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('fe89661b-6647-4c15-97f4-cde8e807090c', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('fe89661b-6647-4c15-97f4-cde8e807090c', foundational, machine_killing_inherently_undignified).
narrative_ontology:cs_axiom_status(machine_killing_inherently_undignified, holdable).
narrative_ontology:cs_axiom_grounding('fe89661b-6647-4c15-97f4-cde8e807090c', machine_killing_inherently_undignified, deontological).
narrative_ontology:cs_axiom('fe89661b-6647-4c15-97f4-cde8e807090c', foundational, martens_clause_demands_categorical_prohibition).
narrative_ontology:cs_axiom_status(martens_clause_demands_categorical_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('fe89661b-6647-4c15-97f4-cde8e807090c', martens_clause_demands_categorical_prohibition, conventional).
narrative_ontology:cs_reference_frame('fe89661b-6647-4c15-97f4-cde8e807090c', absolute_human_moral_control_in_warfare).
narrative_ontology:cs_drift_state('fe89661b-6647-4c15-97f4-cde8e807090c', contemporary_ai_advances, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fe89661b-6647-4c15-97f4-cde8e807090c', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_principle).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, laws_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for a complete ban on autonomous weapons, seeing their existence as an inherent violation of human dignity and a threat to international peace. Benefits from the moral clarity and preventative power of a categorical prohibition.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, mobile, global).

% These states benefit from a categorical ban as it levels the playing field, preventing a technological arms race they cannot afford to join. Their security is enhanced by preventing the proliferation of systems that could be used against them.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    institutional, generational, constrained, global).

% The abstract principle that machine-decided killing is an inherent violation of human dignity, regardless of outcomes. This principle is vindicated by the constraint's operation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_principle, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_principle).

% The strategic and tactical benefits derived from deploying advanced autonomous weapon systems. This advantage is directly curtailed by a categorical prohibition, forcing a re-evaluation of military doctrine and investment.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage, payer,
    institutional, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage).

% These states have invested heavily in developing LAWS and view them as crucial for future defense and deterrence. A categorical ban imposes significant costs by rendering these investments obsolete and limiting strategic options.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, payer,
    institutional, generational, constrained, global).

% Researchers, engineers, and companies developing autonomous weapons. A categorical ban would eliminate their market, forcing a pivot to other technologies or domains, impacting careers and R&D investments.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, laws_developers, payer,
    organized, biographical, constrained, global).

% Analyze the legal and ethical implications of autonomous weapons, interpreting IHL principles like the Martens Clause. Their work informs policy debates and legal frameworks, but they do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universal moral and legal boundary against the delegation of lethal force decisions to machines, coordinating international efforts to prevent an autonomous weapons arms race and uphold human dignity in warfare.
% TRANSFER_FUNCTION: Transfers the potential for military advantage from states with advanced autonomous systems to the collective principle of human dignity and to states lacking such capabilities, by prohibiting an entire class of weapons.
% ABSENT_VOICES: Future generations who would inherit a world where machine-decided killing is normalized are absent from current debates, but their interests are represented by civil society groups advocating for a ban. The 'voice' of the Martens Clause itself, as an evolving principle, is interpreted by scholars and advocates.
% DISAPPEARANCE_RATIONALE: If this categorical prohibition vanished, states with advanced military technology would rapidly accelerate LAWS development and deployment, fundamentally altering the nature of warfare, international security dynamics, and the ethical landscape of conflict. The world would rearrange around a new, machine-mediated form of violence.
% FOUNDING_PROBLEM: The problem of maintaining human control and moral responsibility in warfare, particularly in the face of emerging technologies that could automate lethal decision-making, thereby eroding human dignity and the principles of international humanitarian law.
% FOUNDING_PROBLEM_CORROBORATION: Anti-militarist civil society, numerous states (especially those in the Global South), and a significant body of international legal scholars corroborate that the problem of preserving human dignity and control in lethal force decisions remains live and urgent, particularly with rapid advancements in AI and robotics. They cite the ongoing debates at the UN CCW as evidence of the problem's persistence.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.95) is very high because this reading seeks to completely eliminate a significant area of military technological development, imposing substantial costs on states and industries that have invested in LAWS. Suppression (0.88) is also high, as it requires active and continuous enforcement (diplomatic pressure, legal frameworks, public advocacy) to prevent the development and deployment of these systems. Resistance (0.75) is substantial from states pursuing LAWS. Accessibility collapse (0.9) is high because the prohibition, if enacted, would make the development of LAWS almost impossible within the bounds of international law. Theater ratio (0.1) is low, as the advocacy for this prohibition is largely genuine and not performative; the goal is a real, structural change.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states with advanced LAWS programs, this constraint is a snare designed to extract their technological advantage. From the perspective of anti-militarist civil society, it is a necessary rope or even a mountain, upholding fundamental moral principles. The engine's classification as 'snare' reflects the high, asymmetric extraction imposed on specific, identifiable victims, despite the coordination narrative of upholding dignity.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-militarist civil society and states lacking LAWS capability are clear beneficiaries (low d) as the constraint aligns with their moral and strategic interests. The abstract 'human dignity principle' is also a beneficiary, as its vindication is the core aim. States with advanced autonomous systems and LAWS developers are the primary targets (high d) as they bear the direct costs of prohibition. Military technological advantage, as an abstract concept, is also a victim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforceability_of_categorical_ban,
    'Is a categorical prohibition on LAWS genuinely enforceable, or will it lead to clandestine development and a ''black market'' for autonomous weapons?',
    'Empirical observation of compliance rates and evidence of clandestine programs in the event of a ban. Analysis of historical precedents for bans on military technologies (e.g., chemical weapons, landmines).',
    'If unenforceable, the effective suppression and extractiveness of this constraint would be lower than intended, potentially leading to a ''piton'' classification where the ban is performative but not effective. If enforceable, the ''snare'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_of_categorical_ban, empirical, 'Uncertainty regarding the practical enforceability of a total ban on autonomous weapons.').

omega_variable(
    scope_of_human_dignity_violation,
    'Is the violation of human dignity truly ''per se'' (inherent to machine-decided killing), or is it contingent on specific contexts, performance, or levels of human oversight?',
    'Further philosophical and ethical debate, potentially informed by public discourse and evolving societal norms regarding human-machine interaction in lethal contexts. Legal interpretations by international courts.',
    'If the ''per se'' argument weakens, this reading might converge with the ''human_agency_reading'' (requiring human judgment) or even the ''outcomes_based_reading'' (if dignity is tied to minimizing harm), reducing its categorical nature and lowering its effective extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_human_dignity_violation, conceptual, 'Ambiguity regarding the inherent nature of human dignity violation by autonomous weapons.').

omega_variable(
    martens_clause_interpretive_flexibility,
    'How much interpretive flexibility does the Martens Clause (principles of humanity and public conscience) allow for emerging technologies, and does it genuinely support a categorical ban?',
    'Analysis of state practice, opinio juris, and scholarly consensus on the application of the Martens Clause to novel weapons. Decisions by international legal bodies.',
    'If the Martens Clause is interpreted as more flexible or less supportive of categorical bans, the legal grounding for this constraint weakens, potentially shifting its classification towards a ''tangled_rope'' or ''piton'' if its persistence becomes more about political will than legal necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_interpretive_flexibility, conceptual, 'Uncertainty about the Martens Clause''s scope for categorical prohibitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 5, 0.92).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 10, 0.93).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 15, 0.94).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 20, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 5, 0.83).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 15, 0.87).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 20, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ihl_distinction_proportionality' kernel, concerning the application of IHL to autonomous weapons. This reading asserts a categorical prohibition based on human dignity, distinct from readings focusing on human agency or outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
