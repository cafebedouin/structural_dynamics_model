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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Martens Clause Categorical Prohibition on Autonomous Weapons
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint asserts that the Martens Clause, embodying principles of
 *   humanity and public conscience, categorically prohibits autonomous
 *   weapons systems (LAWS) regardless of their technical performance. The
 *   core argument is that machine-decided killing inherently violates human
 *   dignity, making LAWS unlawful per se under international humanitarian
 *   law. This reading aims to ban an entire class of technology, placing a
 *   high extractive burden on military technological development and states
 *   investing in such systems.
 *
 * KEY AGENTS:
 *   - anti_militarist_civil_society: Primary beneficiary (institutional/arbitrage) — benefits from the categorical ban.
 *   - states_lacking_laws_capability: Primary beneficiary (institutional/mobile) — benefits from leveling the playing field.
 *   - military_technological_advantage: Primary victim (institutional/constrained) — bears the cost of prohibition.
 *   - states_with_advanced_autonomous_systems: Primary victim (institutional/constrained) — bears the cost of prohibition.
 *   - ihl_scholars_and_advocates: Agenda setter (analytical/analytical) — actively promotes and interprets this reading.
 *   - laws_developers: Payer (organized/constrained) — bears the cost of R&D prohibition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.9).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.7).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, snare).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Martens Clause Categorical Prohibition on Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, 'a3cc5673-c062-4acf-98bd-fab90015076a').
narrative_ontology:cs_kernel_codification('a3cc5673-c062-4acf-98bd-fab90015076a', formalized).
narrative_ontology:cs_authority_grounding('a3cc5673-c062-4acf-98bd-fab90015076a', lineage).
narrative_ontology:cs_interpretation_layer_present('a3cc5673-c062-4acf-98bd-fab90015076a').
narrative_ontology:cs_reading_relation('a3cc5673-c062-4acf-98bd-fab90015076a', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3cc5673-c062-4acf-98bd-fab90015076a', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('a3cc5673-c062-4acf-98bd-fab90015076a', foundational, machine_killing_violates_human_dignity_per_se).
narrative_ontology:cs_axiom_status(machine_killing_violates_human_dignity_per_se, holdable).
narrative_ontology:cs_axiom_grounding('a3cc5673-c062-4acf-98bd-fab90015076a', machine_killing_violates_human_dignity_per_se, deontological).
narrative_ontology:cs_axiom('a3cc5673-c062-4acf-98bd-fab90015076a', foundational, martens_clause_establishes_categorical_prohibitions).
narrative_ontology:cs_axiom_status(martens_clause_establishes_categorical_prohibitions, holdable).
narrative_ontology:cs_axiom_grounding('a3cc5673-c062-4acf-98bd-fab90015076a', martens_clause_establishes_categorical_prohibitions, conventional).
narrative_ontology:cs_reference_frame('a3cc5673-c062-4acf-98bd-fab90015076a', martens_clause_prohibitory_framework).
narrative_ontology:cs_drift_state('a3cc5673-c062-4acf-98bd-fab90015076a', contemporary_laws_development_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a3cc5673-c062-4acf-98bd-fab90015076a', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_advocates).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, laws_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively campaigns for a categorical ban on LAWS, viewing it as a moral imperative. Benefits from the constraint by aligning with its core mission and gaining legitimacy for its advocacy.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, arbitrage, global).

% Support the categorical ban as it prevents a technological arms race they cannot afford to join, thereby leveling the playing field in international security. Benefits from the constraint by avoiding strategic disadvantage.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    institutional, generational, mobile, global).

% Represents the strategic and tactical benefits derived from advanced military technology, including LAWS. Bears the cost of the prohibition by losing a potential area of innovation and strategic superiority.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage, payer,
    institutional, generational, constrained, global).

% These states have invested heavily in LAWS research and development, viewing them as essential for future defense. They bear the direct cost of a categorical ban, which would invalidate their investments and strategic planning.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, payer,
    institutional, generational, constrained, global).

% Academics and legal experts who interpret and promote the Martens Clause as a source of categorical prohibition. They shape the discourse and legal arguments for the ban, influencing international policy.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_scholars_and_advocates, agenda_setter,
    analytical, generational, analytical, global).

% Engineers and researchers in defense industries and academic institutions developing autonomous weapons systems. They bear the cost of a categorical ban through project cancellations, loss of funding, and career redirection.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, laws_developers, payer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__categorical_prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate international norms around the ethical boundaries of warfare, specifically prohibiting a class of weapons deemed inherently immoral, thereby preventing a perceived 'race to the bottom' in military ethics.
% TRANSFER_FUNCTION: Transfers the 'right' to develop and deploy autonomous weapons from technologically advanced states and their militaries to a global moral and legal prohibition, enforced by international pressure and potential sanctions.
% ABSENT_VOICES: Future generations who might be subject to machine-decided killing are implicitly represented by the Martens Clause's appeal to 'humanity' and 'public conscience'. The voices of those who would be dehumanized by such systems are absent but central to the prohibition's rationale.
% DISAPPEARANCE_RATIONALE: If this categorical prohibition vanished, the development and deployment of LAWS would accelerate significantly, leading to a new arms race and fundamentally altering the nature of warfare, human accountability, and international security dynamics. The world would rearrange around a new technological frontier.
% FOUNDING_PROBLEM: The problem of weapons technologies emerging that challenge fundamental principles of humanity and conscience, particularly those that delegate life-and-death decisions to machines, thereby eroding human dignity and accountability in warfare.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by numerous civil society organizations, the ICRC, and many states, who consistently raise concerns about the ethical and legal implications of LAWS. This corroboration comes from outside the direct beneficiaries of military technological advantage, affirming the ongoing nature of the ethical challenge.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).

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
 *   The extractiveness is high (0.9) because this reading seeks to prohibit an entire class of technology, imposing a significant cost on military innovation and states that view LAWS as a strategic advantage. Suppression is high (0.7) as it requires active legal and diplomatic enforcement to prevent the development and deployment of LAWS. The theater ratio is low (0.1) because the proponents of this reading are genuinely committed to the categorical ban, with little performative maintenance. Resistance is very high (0.85) due to strong opposition from military powers and defense industries.
 *
 * PERSPECTIVAL GAP:
 *   Proponents (civil society, certain states) experience this as a necessary moral safeguard, a 'rope' protecting humanity. Opponents (states with advanced military tech, defense industry) experience it as a 'snare' that unfairly restricts their strategic options and technological development. The engine's classification as 'snare' reflects the high extraction and suppression required to enforce this categorical ban against powerful actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-militarist civil society and states lacking LAWS capability are beneficiaries (d near 0.0) as the ban aligns with their moral and strategic interests. Military technological advantage and states with advanced autonomous systems are victims (d near 1.0) as they face a categorical prohibition on a perceived strategic asset. IHL scholars and advocates act as agenda setters, actively shaping the interpretation and enforcement of this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate is a categorical moral and legal prohibition. The challenge is not that its function has atrophied, but that its legitimacy and enforceability are highly contested by powerful actors. The classification as 'snare' highlights the active, extractive nature of enforcing such a ban against strong resistance, preventing it from being mislabeled as a 'mountain' (natural law) despite its proponents' claims of inherent unlawfulness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    laws_definition_ambiguity,
    'What constitutes an ''autonomous weapon system'' for the purpose of this prohibition? Does it include systems with human-on-the-loop oversight, or only fully autonomous systems?',
    'International legal consensus on a precise, technically informed definition of LAWS, or a UN treaty explicitly defining the scope.',
    'A narrow definition would reduce the scope of the prohibition, potentially allowing some systems currently considered LAWS to operate. A broad definition would reinforce the categorical ban.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(laws_definition_ambiguity, conceptual, 'Ambiguity in the definition of autonomous weapons systems.').

omega_variable(
    martens_clause_interpretive_scope,
    'Is the Martens Clause intended to establish categorical prohibitions on entire classes of weapons, or primarily to guide interpretation of existing IHL principles in novel contexts?',
    'Further jurisprudence from international courts or a new authoritative interpretation by the ICRC.',
    'If the Martens Clause is primarily interpretive, the categorical ban is weakened, potentially shifting the debate to an outcomes-based approach. If it is a source of direct prohibition, the ban is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_interpretive_scope, conceptual, 'Scope of Martens Clause as a source of prohibition vs. interpretation.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''categorical_prohibition_reading'' of the ''ihl_distinction_proportionality'' kernel. What would change if the ''human_agency_reading'' or ''outcomes_based_reading'' were adopted?',
    'Adoption of a UN treaty or customary international law reflecting one of the sibling readings.',
    'The ''human_agency_reading'' would shift the focus to human control requirements, potentially allowing some LAWS with sufficient human oversight. The ''outcomes_based_reading'' would permit LAWS if they meet or exceed human performance on IHL metrics, fundamentally altering the prohibition''s basis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the IHL distinction/proportionality kernel; sibling readings would alter the basis and scope of LAWS legality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 15, 0.85).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 20, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'ihl_distinction_proportionality' kernel. This 'categorical_prohibition_reading' asserts an inherent unlawfulness of LAWS, distinct from readings focused on human control or outcomes-based performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
