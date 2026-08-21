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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Martens Clause Categorical Prohibition of Autonomous Weapons
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents a reading of International Humanitarian Law
 *   (IHL) that interprets the Martens Clause as imposing a categorical
 *   prohibition on autonomous weapons systems (LAWS), regardless of their
 *   technical performance. It asserts that machine-decided killing inherently
 *   violates human dignity and public conscience. This reading is one of
 *   several competing interpretations of IHL regarding LAWS, with sibling
 *   readings focusing on human agency or outcomes-based performance. This
 *   story focuses exclusively on the structural implications of the
 *   categorical prohibition reading.
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
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Martens Clause Categorical Prohibition of Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '62bd8222-f7b3-457f-9ffe-39f444405d0d').
narrative_ontology:cs_kernel_codification('62bd8222-f7b3-457f-9ffe-39f444405d0d', formalized).
narrative_ontology:cs_authority_grounding('62bd8222-f7b3-457f-9ffe-39f444405d0d', lineage).
narrative_ontology:cs_interpretation_layer_present('62bd8222-f7b3-457f-9ffe-39f444405d0d').
narrative_ontology:cs_reading_relation('62bd8222-f7b3-457f-9ffe-39f444405d0d', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('62bd8222-f7b3-457f-9ffe-39f444405d0d', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('62bd8222-f7b3-457f-9ffe-39f444405d0d', foundational, machine_killing_inherently_violates_dignity).
narrative_ontology:cs_axiom_status(machine_killing_inherently_violates_dignity, holdable).
narrative_ontology:cs_axiom_grounding('62bd8222-f7b3-457f-9ffe-39f444405d0d', machine_killing_inherently_violates_dignity, deontological).
narrative_ontology:cs_axiom('62bd8222-f7b3-457f-9ffe-39f444405d0d', foundational, martens_clause_implies_categorical_prohibition).
narrative_ontology:cs_axiom_status(martens_clause_implies_categorical_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('62bd8222-f7b3-457f-9ffe-39f444405d0d', martens_clause_implies_categorical_prohibition, conventional).
narrative_ontology:cs_reference_frame('62bd8222-f7b3-457f-9ffe-39f444405d0d', absolute_human_control_over_lethal_force).
narrative_ontology:cs_drift_state('62bd8222-f7b3-457f-9ffe-39f444405d0d', contemporary_laws_development_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('62bd8222-f7b3-457f-9ffe-39f444405d0d', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_principle).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, laws_research_and_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for a complete ban on autonomous weapons, seeing their existence as an inherent violation of human dignity and a threat to peace. Benefits from the moral clarity and preventative power of a categorical prohibition.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, mobile, global).

% These states benefit from a categorical ban as it levels the playing field, preventing a technological arms race they cannot afford to join. They align with the moral arguments to support their strategic interests.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    moderate, biographical, constrained, global).

% The abstract principle that machine-decided killing is an inherent violation of human dignity, regardless of operational outcomes. This principle is vindicated by the categorical prohibition.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_principle, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_principle).

% The strategic and tactical benefits derived from developing and deploying advanced autonomous weapon systems. This advantage is directly curtailed and prohibited by the categorical ban.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage).

% These states have invested heavily in LAWS research and development, viewing them as essential for future defense and deterrence. A categorical ban would force them to abandon these investments and capabilities, incurring significant strategic and economic costs.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, payer,
    institutional, generational, constrained, global).

% The entire field of scientific and engineering effort dedicated to creating autonomous weapon systems. This activity is directly targeted and made illegitimate by a categorical prohibition.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, laws_research_and_development, payer,
    institutional, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(ihl_distinction_proportionality__categorical_prohibition_reading, laws_research_and_development).

% Analyze the legal and ethical implications of autonomous weapons, interpreting IHL principles like the Martens Clause. They provide academic arguments for or against various prohibitions, influencing policy debates.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universal moral and legal boundary against the development and deployment of a specific class of weapons, coordinating international efforts to prevent a perceived ethical catastrophe and arms race.
% TRANSFER_FUNCTION: Transfers the potential for military advantage from states with advanced autonomous systems to the collective principle of human dignity and to states lacking such capabilities, by prohibiting an entire technology class.
% ABSENT_VOICES: Proponents of military innovation and those who believe LAWS could reduce civilian casualties (if perfectly accurate) are excluded from the core premise of this reading, which asserts an inherent, categorical wrong regardless of performance.
% DISAPPEARANCE_RATIONALE: If this categorical prohibition vanished, the international legal and ethical landscape would immediately shift, legitimizing the development and deployment of autonomous weapons. States would accelerate their LAWS programs, and the debate would move from 'if' to 'how' to regulate, fundamentally altering military strategy and international relations.
% FOUNDING_PROBLEM: The existential threat to human dignity and the potential for an uncontrollable arms race posed by the prospect of machines making life-and-death decisions without human intervention.
% FOUNDING_PROBLEM_CORROBORATION: Civil society organizations, numerous states, and prominent ethicists corroborate the live status of this problem, citing ongoing technological advancements and the lack of a binding international treaty. Military strategists and some states contest the premise, arguing that LAWS could enhance IHL compliance.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is very high (0.95) because this reading demands a complete ban on an entire class of military technology, imposing significant costs on states that seek to develop or deploy LAWS. Suppression is also high (0.88) as it requires active and continuous enforcement (diplomatic pressure, legal frameworks, public shaming) to prevent states from pursuing LAWS. The theater ratio is low (0.1) because the core claim is a fundamental ethical and legal principle, not a performance. Accessibility collapse is high (0.9) as it seeks to eliminate the very possibility of LAWS, leaving no legitimate alternatives for their use. Resistance is high (0.75) due to strong opposition from militaries and states invested in LAWS development.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of anti-LAWS advocates, this is a necessary moral safeguard (a 'rope' of ethical coordination). From the perspective of states developing LAWS, it is a 'snare' that unfairly targets their strategic capabilities and technological sovereignty. The engine's classification as 'snare' reflects the high extraction and suppression required to enforce this categorical ban against powerful actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-militarist civil society and states lacking LAWS capability are clear beneficiaries, as the constraint aligns with their moral and strategic interests. The abstract principle of human dignity is also a beneficiary, as its tenets are upheld. States with advanced autonomous systems and the concept of military technological advantage are the primary victims, bearing the full cost of the prohibition. IHL scholars act as observers, analyzing and debating the legal interpretations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_conditional_prohibition,
    'Is the Martens Clause truly a categorical prohibition against LAWS, or does it allow for conditional prohibitions based on specific capabilities or contexts?',
    'International legal precedent from future ICJ or ICC rulings, or a universally adopted, legally binding treaty explicitly clarifying the scope of the Martens Clause regarding LAWS.',
    'If conditional, the constraint''s extractiveness and suppression would decrease, potentially reclassifying it as a ''tangled_rope'' or ''rope'' focused on regulating rather than banning LAWS. If categorical, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_conditional_prohibition, conceptual, 'Ambiguity regarding the absolute nature of the Martens Clause''s prohibition.').

omega_variable(
    human_dignity_operationalization,
    'How is ''human dignity'' operationalized in a way that definitively prohibits machine-decided killing, and is this interpretation universally shared?',
    'Cross-cultural ethical consensus building, philosophical clarification of ''dignity'' in the context of autonomy, and legal codification of specific ''red lines'' for human control.',
    'If the operationalization is contested or not universally shared, the legitimacy and enforceability of the categorical ban are weakened, reducing its effective suppression and extractiveness. If a robust, shared operationalization emerges, the constraint''s power is amplified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_dignity_operationalization, preference, 'The subjective and contested nature of ''human dignity'' as a legal and ethical grounding.').


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
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__categorical_prohibition_reading, 0.1).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, laws_development_and_deployment).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ihl_distinction_proportionality' kernel. This 'categorical_prohibition_reading' asserts an inherent ethical and legal ban on autonomous weapons, distinct from readings focused on human agency or outcomes-based performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
