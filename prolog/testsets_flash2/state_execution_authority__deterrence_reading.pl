% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__deterrence_reading, []).

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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence reading' of state execution
 *   authority, where capital punishment is justified primarily by its
 *   capacity to prevent future murders by raising the cost of capital crimes.
 *   The executed offender is viewed as an instrumental cost, and potential
 *   victims are the primary beneficiaries. The effectiveness of this
 *   deterrence is a subject of ongoing empirical debate, which significantly
 *   impacts the constraint's perceived legitimacy and its classification. The
 *   metrics reflect a system where the deterrence function is increasingly
 *   questioned, leading to a higher 'theater_ratio' as the justification
 *   becomes more performative than empirically grounded.
 *
 * KEY AGENTS:
 *   - state_prosecutors: Agenda setter (institutional/constrained) — advocates for and implements capital punishment.
 *   - potential_victims_of_murder: Primary beneficiary (powerless/trapped) — hypothetically protected by deterrence.
 *   - executed_offenders: Primary payer (powerless/trapped) — bear the ultimate cost.
 *   - families_of_executed_offenders: Secondary payer (powerless/trapped) — bear long-term costs.
 *   - abolitionist_advocates: Excluded voice (organized/constrained) — challenge the deterrence claim.
 *   - constitutional_courts: Observer (institutional/analytical) — adjudicate legality and application.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.45).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.7).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '83f8ed23-ba79-48a4-a796-86874f0e808e').
narrative_ontology:cs_kernel_codification('83f8ed23-ba79-48a4-a796-86874f0e808e', formalized).
narrative_ontology:cs_authority_grounding('83f8ed23-ba79-48a4-a796-86874f0e808e', lineage).
narrative_ontology:cs_interpretation_layer_present('83f8ed23-ba79-48a4-a796-86874f0e808e').
narrative_ontology:cs_reading_relation('83f8ed23-ba79-48a4-a796-86874f0e808e', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('83f8ed23-ba79-48a4-a796-86874f0e808e', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('83f8ed23-ba79-48a4-a796-86874f0e808e', foundational, execution_deters_future_crime).
narrative_ontology:cs_axiom_status(execution_deters_future_crime, holdable).
narrative_ontology:cs_axiom_grounding('83f8ed23-ba79-48a4-a796-86874f0e808e', execution_deters_future_crime, empirically_contingent).
narrative_ontology:cs_axiom('83f8ed23-ba79-48a4-a796-86874f0e808e', secondary, state_has_right_to_take_life_for_public_safety).
narrative_ontology:cs_axiom_status(state_has_right_to_take_life_for_public_safety, holdable).
narrative_ontology:cs_axiom_grounding('83f8ed23-ba79-48a4-a796-86874f0e808e', state_has_right_to_take_life_for_public_safety, deontological).
narrative_ontology:cs_reference_frame('83f8ed23-ba79-48a4-a796-86874f0e808e', utilitarian_deterrence_framework).
narrative_ontology:cs_drift_state('83f8ed23-ba79-48a4-a796-86874f0e808e', contemporary_criminological_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('83f8ed23-ba79-48a4-a796-86874f0e808e', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, potential_victims_of_murder).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, state_prosecutors).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, families_of_executed_offenders).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, utilitarian_justice_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, state_sovereignty_over_life).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for capital punishment, believing it deters future crime and serves justice. They bear the burden of proof and procedural safeguards, but also gain political capital from securing death sentences. Their careers are often tied to their success in high-profile cases.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_prosecutors, agenda_setter,
    institutional, biographical, constrained, national).

% Are the theoretical beneficiaries of deterrence, as their lives are hypothetically saved by the threat of execution. They have no direct agency in the system but are invoked as the primary justification for its existence.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, potential_victims_of_murder, beneficiary,
    powerless, immediate, trapped, local).

% Bear the ultimate cost of the constraint, their lives. They are the direct target of the state's authority and have no exit once convicted and sentenced. Their existence is treated as an instrumental cost for the greater good of deterrence.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Suffer the loss and stigma associated with the execution. They bear a diffuse, long-term cost without any direct benefit from the deterrence mechanism. Their options for redress are extremely limited.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, families_of_executed_offenders, payer,
    powerless, generational, trapped, local).

% Argue against capital punishment on moral and practical grounds, including its ineffectiveness as a deterrent and the risk of wrongful execution. They are excluded from the direct decision-making process but exert pressure through legal challenges and public campaigns.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

% Review the constitutionality of capital punishment, including its application and procedural fairness. They weigh arguments about deterrence, retribution, and cruel and unusual punishment, shaping the legal boundaries of the state's authority.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, potential_victims_of_murder).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action to impose the ultimate penalty for capital crimes, aiming to deter potential offenders and protect society.
% TRANSFER_FUNCTION: Transfers the life of the executed offender from the individual to the state, justified as a means to prevent future harm to potential victims.
% ABSENT_VOICES: The executed offenders themselves, whose perspectives are silenced by the act. Also, the voices of those who believe in the categorical impermissibility of state killing, who are often marginalized in deterrence-focused debates.
% DISAPPEARANCE_RATIONALE: If state execution authority vanished overnight, the criminal justice system would need to fundamentally re-evaluate its sentencing for capital crimes, potentially shifting to life imprisonment without parole as the maximum penalty. The perceived deterrent effect would be lost, and public discourse on crime prevention would shift.
% FOUNDING_PROBLEM: The problem of preventing heinous crimes and ensuring public safety, particularly against murder, by imposing a penalty severe enough to deter others.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the state and some segments of the public attest that the problem of deterrence is live and execution is a necessary tool. However, criminologists, human rights organizations, and many international bodies, from outside the benefiting parties, contest the deterrence effect, citing empirical studies that show no significant difference in murder rates between abolitionist and retentionist states.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).
:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because, from this reading's perspective, the 'cost' of an offender's life is balanced against the 'benefit' of lives saved. However, the empirical evidence for deterrence is weak, leading to a high theater_ratio (0.60) as the justification becomes more performative. Suppression (0.70) is high due to the state's ultimate coercive power and the lack of exit for the condemned. Resistance (0.75) is also high, reflecting strong opposition from abolitionist movements and legal challenges. The claimed type is 'tangled_rope' because it attempts to coordinate public safety (beneficiary: potential_victims_of_murder) through a mechanism that involves clear, asymmetric extraction (victim: executed_offenders) and requires active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state prosecutors, the constraint is a necessary tool for public safety, a coordination mechanism. From the perspective of executed offenders and their families, it is pure extraction. Constitutional courts view it through a lens of legal precedent and evolving standards of justice, balancing state power against individual rights. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Potential victims are full beneficiaries (d near 0.0) as the constraint theoretically protects them. State prosecutors are also beneficiaries (d near 0.15) due to their role in upholding the system and associated political gains. Executed offenders are full targets (d near 1.0) as they bear the ultimate cost. Their families also sit near the target end (d near 0.9) due to the profound, uncompensated loss. Abolitionist advocates are excluded, their d value reflecting their structural opposition to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope, rather than a pure Rope, prevents mislabeling by highlighting the asymmetric extraction inherent in the deterrence justification. The high theater_ratio and contested founding problem status indicate potential mandatrophy, where the original coordination function (deterrence) is increasingly questioned, but the constraint persists due to institutional inertia and the concentrated benefits for certain actors (e.g., state prosecutors). If deterrence is empirically disproven, the constraint would shift closer to a Snare, as its coordination function would collapse, leaving only extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical_uncertainty,
    'Does capital punishment actually deter future murders more effectively than life imprisonment without parole?',
    'Longitudinal, cross-jurisdictional empirical studies comparing murder rates and crime trends in abolitionist vs. retentionist states, controlling for socioeconomic factors.',
    'If deterrence is empirically disproven, the primary justification for this reading collapses, shifting the constraint towards a Snare (pure extraction) or Piton (theatrical maintenance). If proven, it strengthens the Rope aspect, justifying the extraction as a necessary cost of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical_uncertainty, empirical, 'Uncertainty regarding the empirical effectiveness of capital punishment as a deterrent.').

omega_variable(
    wrongful_execution_error_rate,
    'What is the irreducible error rate of wrongful convictions in capital cases, and what is the societal cost of executing an innocent person?',
    'Systematic review of post-conviction exonerations, statistical modeling of error rates, and ethical analysis of the value of an innocent life.',
    'A high, unmitigable error rate would fundamentally undermine the utilitarian calculus of deterrence, potentially shifting the constraint towards an abolitionist reading or a Snare due to unacceptable collateral damage. It would also increase the perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_error_rate, empirical, 'Uncertainty about the frequency and impact of wrongful executions.').

omega_variable(
    reading_framing_ambiguity,
    'Is this constraint primarily about deterrence, or is deterrence a cover story for retributive impulses?',
    'Analysis of legislative history, judicial opinions, and public discourse to identify the dominant justifications invoked by proponents, especially when deterrence arguments weaken.',
    'If deterrence is found to be a cover, the constraint would be reclassified as a ''retributive_reading'' (a sibling constraint), with different beneficiaries (e.g., victims'' families seeking vengeance) and a potentially higher extractiveness due to the lack of a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Ambiguity in the primary justification for capital punishment (deterrence vs. retribution).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_execution_authority__deterrence_reading, theater_ratio, 1976, 0.4).
narrative_ontology:measurement(stat_tr_t1990, state_execution_authority__deterrence_reading, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(stat_tr_t2000, state_execution_authority__deterrence_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(stat_tr_t2010, state_execution_authority__deterrence_reading, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(stat_tr_t2024, state_execution_authority__deterrence_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_execution_authority__deterrence_reading, base_extractiveness, 1976, 0.55).
narrative_ontology:measurement(stat_be_t1990, state_execution_authority__deterrence_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(stat_be_t2000, state_execution_authority__deterrence_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(stat_be_t2010, state_execution_authority__deterrence_reading, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement(stat_be_t2024, state_execution_authority__deterrence_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_execution_authority__deterrence_reading, suppression_requirement, 1976, 0.8).
narrative_ontology:measurement(stat_su_t1990, state_execution_authority__deterrence_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(stat_su_t2000, state_execution_authority__deterrence_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(stat_su_t2010, state_execution_authority__deterrence_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement(stat_su_t2024, state_execution_authority__deterrence_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_execution_authority' kernel. This 'deterrence_reading' focuses on crime prevention, while the 'retributive_reading' focuses on proportionate punishment, and the 'abolition_reading' on categorical impermissibility. Each is a distinct constraint with different beneficiaries, victims, and classifications, linked as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
