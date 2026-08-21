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
 *   human_readable: IHL Human Agency Requirement for Lethal Force
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'human_agency_reading' of the
 *   'ihl_distinction_proportionality' kernel. It asserts that International
 *   Humanitarian Law (IHL) mandates irreducible human moral judgment in the
 *   application of lethal force, explicitly prohibiting the delegation of
 *   life-and-death decisions to fully autonomous machines, grounded in
 *   Martens Clause principles of humanity. From the perspective of IHL
 *   interpretive authorities, this is a 'rope' that coordinates adherence to
 *   fundamental ethical principles. However, the authored metrics reflect its
 *   high extractiveness and suppression on military innovation and
 *   efficiency, which the engine will use to compute its effective
 *   classification from other seats.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.8).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.75).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Human Agency Requirement for Lethal Force").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '2350fed5-d15f-404b-9fe6-d9ac1e854184').
narrative_ontology:cs_kernel_codification('2350fed5-d15f-404b-9fe6-d9ac1e854184', formalized).
narrative_ontology:cs_authority_grounding('2350fed5-d15f-404b-9fe6-d9ac1e854184', lineage).
narrative_ontology:cs_interpretation_layer_present('2350fed5-d15f-404b-9fe6-d9ac1e854184').
narrative_ontology:cs_reading_relation('2350fed5-d15f-404b-9fe6-d9ac1e854184', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_reading_relation('2350fed5-d15f-404b-9fe6-d9ac1e854184', ihl_distinction_proportionality__categorical_prohibition_reading, influences).
narrative_ontology:cs_axiom('2350fed5-d15f-404b-9fe6-d9ac1e854184', foundational, human_moral_judgment_irreducible).
narrative_ontology:cs_axiom_status(human_moral_judgment_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('2350fed5-d15f-404b-9fe6-d9ac1e854184', human_moral_judgment_irreducible, deontological).
narrative_ontology:cs_axiom('2350fed5-d15f-404b-9fe6-d9ac1e854184', foundational, martens_clause_prohibits_machine_killing).
narrative_ontology:cs_axiom_status(martens_clause_prohibits_machine_killing, holdable).
narrative_ontology:cs_axiom_grounding('2350fed5-d15f-404b-9fe6-d9ac1e854184', martens_clause_prohibits_machine_killing, deontological).
narrative_ontology:cs_reference_frame('2350fed5-d15f-404b-9fe6-d9ac1e854184', irreducible_human_moral_agency).
narrative_ontology:cs_drift_state('2350fed5-d15f-404b-9fe6-d9ac1e854184', contemporary_laws_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2350fed5-d15f-404b-9fe6-d9ac1e854184', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, human_dignity_advocates).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_populations).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_developers_of_laws).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_commanders_seeking_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, interpret, and advocate for the application of International Humanitarian Law, particularly regarding new technologies of warfare. They assert the necessity of human moral judgment in lethal force decisions to uphold IHL principles and maintain their institutional centrality.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Develop and integrate Lethal Autonomous Weapons Systems (LAWS) for national defense. They face significant legal and ethical constraints imposed by this reading, requiring costly human-in-the-loop oversight and limiting the full potential of autonomy.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_developers_of_laws, payer,
    organized, biographical, constrained, global).

% Seek to deploy LAWS for tactical advantage, speed, and reduced risk to personnel. This reading imposes operational friction and limits the efficiency gains they could achieve through full automation of lethal force decisions.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_commanders_seeking_efficiency, payer,
    powerful, biographical, constrained, global).

% Champion the moral imperative of human control over lethal force, viewing this reading as a crucial safeguard for human dignity and accountability in warfare. They benefit from the constraint's existence as it aligns with their core values.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, human_dignity_advocates, beneficiary,
    organized, generational, mobile, global).

% Are the ultimate beneficiaries of IHL's protections. This reading aims to reduce the risk of indiscriminate harm and ensure accountability for lethal force decisions, indirectly benefiting civilians by maintaining a human moral buffer.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, global).

% Argue for a technology-neutral approach to IHL compliance, where the legality of LAWS should be judged solely on their ability to achieve distinction and proportionality outcomes, potentially exceeding human performance. Their perspective is actively resisted and excluded by this reading's emphasis on human agency.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, outcomes_based_proponents, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the application of lethal force remains subject to human moral judgment, upholding the principles of distinction and proportionality in armed conflict, and maintaining public trust in the ethical conduct of warfare.
% TRANSFER_FUNCTION: Transfers the burden of moral judgment and accountability for lethal force decisions from potential autonomous systems back to human operators, and transfers potential efficiency gains from militaries to the preservation of human moral agency.
% ABSENT_VOICES: Proponents of purely outcomes-based approaches to IHL compliance, who argue that if LAWS can achieve better IHL compliance than humans, they should be permitted. Their arguments are foreclosed by this reading's emphasis on human agency.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, militaries would rapidly accelerate development and deployment of fully autonomous lethal weapons systems, fundamentally altering the nature of warfare, accountability, and the protection of civilians. The mobile software economy would reorganize around open payment routing.
% FOUNDING_PROBLEM: The potential for dehumanization of warfare, erosion of accountability, and increased risk to civilians if machines were allowed to make life-and-death decisions without human moral judgment.
% FOUNDING_PROBLEM_CORROBORATION: IHL bodies (ICRC, UN experts) and human rights organizations consistently attest to the live nature of this problem, citing ongoing technological advancements and ethical concerns. Military strategists, while seeking efficiency, also acknowledge the ethical dilemmas.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Base extractiveness is high (0.8) because this reading imposes significant costs on military development and deployment of fully autonomous systems, forcing human oversight. Suppression is also high (0.75) as it actively prohibits certain technological pathways and requires continuous enforcement through legal interpretation and advocacy. Theater ratio is low (0.15) because the demand for human judgment is genuinely held, not merely performative. The temporal measurements show increasing extractiveness and suppression as the pressure to develop LAWS grows, requiring stronger assertion and enforcement of this reading.
 *
 * PERSPECTIVAL GAP:
 *   The IHL interpretive authorities (agenda-setter) perceive this constraint as a necessary 'rope' for ethical coordination and protection of civilians. In contrast, military developers and commanders (payers) experience it as a 'snare' or 'tangled rope' due to the high costs and limitations it imposes on their operational efficiency and technological advancement. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities and human dignity advocates are beneficiaries (low d) as the constraint reinforces their mandate and values. Civilian populations are indirect beneficiaries. Military developers and commanders are targets (high d) as they bear the costs of compliance and foregone efficiency. Outcomes-based proponents are excluded, as their alternative framing is actively suppressed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreducibility_of_human_judgment,
    'Is human moral judgment truly irreducible in complex lethal force scenarios, or could advanced AI systems eventually meet or exceed human ethical performance?',
    'Empirical studies comparing human and AI ethical decision-making in simulated and real-world complex scenarios, coupled with philosophical analysis of the nature of moral agency.',
    'If AI could demonstrably exceed human ethical performance, the foundational premise of this reading would be challenged, potentially shifting the constraint towards an outcomes-based classification or weakening its suppressive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreducibility_of_human_judgment, empirical, 'Whether human moral judgment is an inherently superior or unique requirement for lethal force decisions.').

omega_variable(
    martens_clause_public_conscience_evolution,
    'Does the ''public conscience'' aspect of the Martens Clause represent a fixed moral boundary against machine-decided killing, or is it a societal preference that could evolve with technological familiarity and perceived benefits?',
    'Longitudinal sociological studies of public attitudes towards AI in warfare, cross-cultural ethical surveys, and analysis of how ''public conscience'' has been interpreted in other evolving legal contexts.',
    'If public conscience is found to be mutable, the deontological grounding of this reading could weaken, making it more susceptible to instrumental arguments for LAWS deployment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(martens_clause_public_conscience_evolution, conceptual, 'The stability and interpretability of the ''public conscience'' principle in the context of advanced AI.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the ''ihl_distinction_proportionality'' kernel. How would its classification change if the ''outcomes_based_reading'' or ''categorical_prohibition_reading'' were adopted as the dominant interpretation?',
    'Analysis of the structural implications of adopting a sibling reading: the ''outcomes_based_reading'' would likely reduce extractiveness and suppression on LAWS development, while the ''categorical_prohibition_reading'' would increase suppression and extractiveness on all LAWS, potentially shifting this constraint to a Snare for military actors.',
    'The classification of this constraint is highly dependent on which reading of the kernel is dominant. Adopting a different reading would fundamentally alter its beneficiary/victim structure and metric profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The impact of alternative kernel readings on the constraint''s structural classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2005, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(ihl__tr_t2009, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2009, 0.11).
narrative_ontology:measurement(ihl__tr_t2013, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2013, 0.12).
narrative_ontology:measurement(ihl__tr_t2017, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2017, 0.13).
narrative_ontology:measurement(ihl__tr_t2021, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2021, 0.14).
narrative_ontology:measurement(ihl__tr_t2025, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2005, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(ihl__be_t2009, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2009, 0.65).
narrative_ontology:measurement(ihl__be_t2013, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2013, 0.7).
narrative_ontology:measurement(ihl__be_t2017, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2017, 0.75).
narrative_ontology:measurement(ihl__be_t2021, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2021, 0.78).
narrative_ontology:measurement(ihl__be_t2025, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2005, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(ihl__su_t2009, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2009, 0.68).
narrative_ontology:measurement(ihl__su_t2013, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2013, 0.7).
narrative_ontology:measurement(ihl__su_t2017, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2017, 0.72).
narrative_ontology:measurement(ihl__su_t2021, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2021, 0.74).
narrative_ontology:measurement(ihl__su_t2025, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, laws_development_ethics).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, military_ai_procurement_norms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
