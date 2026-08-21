% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__deterrence_reading, []).

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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: State Killing Legitimacy (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence reading' of state killing
 *   legitimacy, where execution is justified as a rational signal to prevent
 *   future murders. The offender is instrumentalized as a means to a social
 *   end, with potential future victims as the primary beneficiaries. The
 *   claimed type is 'tangled_rope' because it attempts a coordination
 *   function (deterrence) but involves significant, asymmetric extraction
 *   (the offender's life) and requires active enforcement, with the empirical
 *   basis for its coordination function being highly contested. The metrics
 *   reflect this contested nature and the high cost borne by the victim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.65).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.7).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "State Killing Legitimacy (Deterrence Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, '89a068d3-cf4b-4624-b2ec-a3002eaac338').
narrative_ontology:cs_kernel_codification('89a068d3-cf4b-4624-b2ec-a3002eaac338', formalized).
narrative_ontology:cs_authority_grounding('89a068d3-cf4b-4624-b2ec-a3002eaac338', lineage).
narrative_ontology:cs_interpretation_layer_present('89a068d3-cf4b-4624-b2ec-a3002eaac338').
narrative_ontology:cs_reading_relation('89a068d3-cf4b-4624-b2ec-a3002eaac338', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('89a068d3-cf4b-4624-b2ec-a3002eaac338', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('89a068d3-cf4b-4624-b2ec-a3002eaac338', foundational, punishment_as_deterrent).
narrative_ontology:cs_axiom_status(punishment_as_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('89a068d3-cf4b-4624-b2ec-a3002eaac338', punishment_as_deterrent, empirically_contingent).
narrative_ontology:cs_axiom('89a068d3-cf4b-4624-b2ec-a3002eaac338', secondary, state_monopoly_on_violence_for_public_safety).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_for_public_safety, holdable).
narrative_ontology:cs_axiom_grounding('89a068d3-cf4b-4624-b2ec-a3002eaac338', state_monopoly_on_violence_for_public_safety, conventional).
narrative_ontology:cs_reference_frame('89a068d3-cf4b-4624-b2ec-a3002eaac338', classical_utilitarian_punishment).
narrative_ontology:cs_drift_state('89a068d3-cf4b-4624-b2ec-a3002eaac338', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('89a068d3-cf4b-4624-b2ec-a3002eaac338', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, state_prosecutors).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, convicted_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, defense_attorneys).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for capital punishment, arguing its necessity for public safety and crime prevention. Their careers and public image are often tied to securing convictions and severe sentences, including executions. They administer the legal process that leads to executions.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_prosecutors, agenda_setter,
    institutional, biographical, constrained, national).

% Are the direct targets of the constraint, losing their lives. They have no agency in the process once convicted and sentenced, and their appeals are often exhausted. Their situation is one of ultimate extraction.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, convicted_offenders, payer,
    powerless, immediate, trapped, local).

% Are the theoretical beneficiaries, as their lives are supposedly saved by the deterrent effect of executions. They are an abstract group whose interests are represented by the state, rather than active participants.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    powerless, generational, analytical, national).

% Bear the professional and emotional costs of defending clients against capital charges. They face an uphill battle against state resources and public sentiment, often working within a system designed to secure convictions. Their efforts are largely aimed at mitigating the extraction.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, defense_attorneys, payer,
    moderate, biographical, constrained, national).

% Actively campaign against capital punishment, arguing it is immoral and ineffective. While they can influence public opinion and legislation, they are often excluded from the direct legal processes that determine individual sentences, and their arguments are frequently dismissed by proponents of deterrence.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Conduct empirical studies on the deterrent effect of capital punishment. Their findings are often contested and inconclusive, leading to ongoing debate rather than definitive resolution. They provide data that informs the legitimacy claims but do not directly participate in enforcement.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, social_scientists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate societal behavior by establishing a severe consequence for capital offenses, thereby deterring potential murderers and ensuring public safety through a clear, ultimate sanction.
% TRANSFER_FUNCTION: Transfers the life of the convicted offender to the state, in exchange for the theoretical benefit of preventing future murders and maintaining social order.
% ABSENT_VOICES: The executed offenders themselves are permanently silenced. Abolitionist advocates, while vocal, are often marginalized in policy debates where deterrence is the primary justification, their arguments for human dignity and against state violence being deemed irrelevant to the 'practical' goal of crime prevention.
% DISAPPEARANCE_RATIONALE: If the deterrence justification for state killing vanished, the legal and moral framework for capital punishment would collapse. States would be forced to find alternative justifications or abolish it, leading to a significant rearrangement of criminal justice policy and public discourse around punishment.
% FOUNDING_PROBLEM: The problem of preventing heinous crimes and ensuring public safety, particularly murder, by establishing a punishment severe enough to deter others.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (state prosecutors, some politicians) argue the problem is live and deterrence is effective. Opponents (abolitionist advocates, many social scientists) argue the problem is not solved by capital punishment, citing inconclusive empirical evidence and the risk of executing innocent people. Independent academic studies from outside the benefiting parties consistently find no conclusive evidence of a deterrent effect, challenging the core premise.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the ultimate cost (life) is borne by the offender, and the benefit (deterrence) is empirically uncertain. Suppression is also high (0.70) due to the state's monopoly on legitimate force and the finality of execution, which suppresses any further resistance from the victim. Theater ratio is moderate (0.40) as the public performance of justice and deterrence often outweighs the demonstrable efficacy of the deterrent effect, especially given the ongoing empirical debate. The slight dip in extractiveness and suppression towards the end of the interval reflects increasing legal challenges and public scrutiny, making executions harder to carry out, but not fundamentally altering the constraint's nature.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state prosecutors, this is a necessary, albeit severe, coordination mechanism for public safety. From the perspective of convicted offenders and defense attorneys, it is pure extraction. Social scientists and abolitionist advocates view it as a contested claim with high human cost. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Convicted offenders are full targets (d=1.0) as they bear the ultimate cost. Potential future victims are full beneficiaries (d=0.0) as they are theoretically protected without cost. State prosecutors are beneficiaries (d low) as they gain political capital and fulfill their mandate. Defense attorneys are targets (d high) as they fight against the system. The engine will derive these from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (deterrence) is highly contested. If empirical evidence definitively showed no deterrent effect, the constraint would shift from a Tangled Rope (with a contested coordination function) towards a Snare (pure extraction), as its coordination justification would collapse, leaving only the coercive extraction. The classification prevents mislabeling it as a Rope by highlighting the contested coordination and high extraction, and prevents mislabeling as a Snare by acknowledging the *claimed* coordination function, however weak.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_effect_empirical_validity,
    'Does capital punishment actually deter potential murderers more effectively than life imprisonment?',
    'Large-scale, methodologically rigorous, longitudinal empirical studies across multiple jurisdictions, controlling for confounding variables and avoiding selection bias.',
    'If a significant deterrent effect is proven, the extractiveness of the constraint might be re-evaluated downwards (as the ''benefit'' side of the coordination becomes more certain), potentially pushing it closer to a Rope. If no deterrent effect is found, it would strongly support reclassifying it as a Snare, as the coordination function would be revealed as a cover for pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrent_effect_empirical_validity, empirical, 'The empirical question of whether capital punishment has a measurable deterrent effect.').

omega_variable(
    moral_instrumentalization_ambiguity,
    'Is the instrumentalization of an individual (the offender) for a societal goal (deterrence) morally permissible, even if effective?',
    'Philosophical debate and societal consensus on the limits of utilitarian ethics in criminal justice, particularly regarding the inherent dignity of individuals.',
    'If instrumentalization is deemed impermissible, the constraint''s legitimacy would collapse regardless of empirical efficacy, pushing it towards a Snare or even a Mountain of moral law (abolitionist reading). If permissible, the deterrence reading gains stronger moral grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_instrumentalization_ambiguity, conceptual, 'The moral permissibility of using an individual as a means to a societal end.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine deterrence mechanism, or is the deterrence claim a post-hoc rationalization for retributive impulses or state power?',
    'Analysis of legislative history, judicial opinions, and public discourse to determine the primary motivations for capital punishment in practice, rather than just stated justifications.',
    'If primarily a rationalization, the constraint''s true nature might align more with the ''retributive_reading'' or a pure ''snare'' of state power, requiring re-evaluation of its claimed coordination function and beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing genuine deterrence from rationalization for other motives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__deterrence_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__deterrence_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__deterrence_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__deterrence_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__deterrence_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__deterrence_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__deterrence_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__deterrence_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__deterrence_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__deterrence_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__deterrence_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__deterrence_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_legitimacy' kernel. Its empirical claims and instrumental justification are in tension with the retributive reading's focus on desert and the abolitionist reading's focus on inherent dignity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
