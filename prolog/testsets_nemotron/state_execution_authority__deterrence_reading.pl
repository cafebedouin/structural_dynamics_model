% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority — Deterrence Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story captures the deterrence reading of state execution
 *   authority — the claim that capital punishment prevents future murders by
 *   raising the expected cost of capital crimes. The reading instantiates a
 *   utilitarian calculus: the executed offender is an instrumental cost;
 *   future potential victims are the beneficiaries; wrongful executions are
 *   system overhead requiring minimization. The constraint is structurally a
 *   tangled rope: it coordinates a credible lethal threat (genuine
 *   coordination function) while extracting life from a powerless class
 *   (asymmetric extraction) and requiring active enforcement (appellate
 *   machinery, execution protocols, death row maintenance). The claim/metric
 *   gap is deliberate: the deterrence reading claims rope (pure
 *   coordination), but the metrics describe a system whose coordination
 *   function is empirically contested and whose extraction falls on the
 *   powerless.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.55).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.7).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority — Deterrence Reading").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '32be1f02-55ab-4577-ae25-7eebe79231d1').
narrative_ontology:cs_kernel_codification('32be1f02-55ab-4577-ae25-7eebe79231d1', formalized).
narrative_ontology:cs_authority_grounding('32be1f02-55ab-4577-ae25-7eebe79231d1', lineage).
narrative_ontology:cs_interpretation_layer_present('32be1f02-55ab-4577-ae25-7eebe79231d1').
narrative_ontology:cs_reading_relation('32be1f02-55ab-4577-ae25-7eebe79231d1', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('32be1f02-55ab-4577-ae25-7eebe79231d1', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('32be1f02-55ab-4577-ae25-7eebe79231d1', foundational, execution_prevents_future_murder).
narrative_ontology:cs_axiom_status(execution_prevents_future_murder, holdable).
narrative_ontology:cs_axiom_grounding('32be1f02-55ab-4577-ae25-7eebe79231d1', execution_prevents_future_murder, empirically_contingent).
narrative_ontology:cs_axiom('32be1f02-55ab-4577-ae25-7eebe79231d1', secondary, life_without_parole_is_deterrence_substitute).
narrative_ontology:cs_axiom_status(life_without_parole_is_deterrence_substitute, holdable).
narrative_ontology:cs_axiom_grounding('32be1f02-55ab-4577-ae25-7eebe79231d1', life_without_parole_is_deterrence_substitute, empirically_contingent).
narrative_ontology:cs_reference_frame('32be1f02-55ab-4577-ae25-7eebe79231d1', gregg_v_georgia_deterrence_rationale).
narrative_ontology:cs_drift_state('32be1f02-55ab-4577-ae25-7eebe79231d1', post_nrc_2012_review, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('32be1f02-55ab-4577-ae25-7eebe79231d1', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, public_safety_advocates).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, law_enforcement_institutions).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_capital_defendants).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, capital_defendants_facing_execution).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, general_deterrence_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, state_monopoly_on_lethal_force).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, proportional_punishment_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the legal power to impose and carry out death sentences. Designs the procedural safeguards, selects the methods, and controls the execution schedule. Justifies the authority through deterrence claims and retributive legitimacy. The authority's institutional survival and budget depend on maintaining the capital punishment system.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_execution_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% The abstract class of persons who would be murdered absent the deterrent effect of execution. They do not participate in the political process and have no voice in the arrangement. Their inclusion as beneficiaries is the core utilitarian claim of the deterrence reading — the executed offender's life is weighed against their statistical survival.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_victims, beneficiary,
    powerless, biographical, trapped, national).

% Victim-advocacy organizations, police unions, and prosecutors' associations that campaign for capital punishment as a deterrent tool. They receive political capital, funding, and institutional relevance from the system's maintenance. Their exit is mobile — they could pivot to other crime-reduction advocacy if the deterrence rationale collapsed.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, public_safety_advocates, beneficiary,
    organized, biographical, mobile, national).

% Police, prosecutors, and corrections departments that operate the capital punishment machinery. They gain investigative leverage (death-qualified juries, plea-bargain pressure) and institutional missions (death row management, execution protocols). Their exit is constrained — the institutional infrastructure is purpose-built and not easily repurposed.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, law_enforcement_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__deterrence_reading, law_enforcement_institutions, agenda_setter).

% The individuals actually put to death. They bear the full cost of the arrangement — their lives are the instrumental expenditure in the deterrence calculus. No exit exists; the constraint is total and final for this seat. The deterrence reading explicitly treats this cost as a utilitarian input, not a moral claim.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, national).

% Those sentenced to death who are factually innocent or whose convictions rest on flawed evidence. They bear the catastrophic error cost of the deterrence machinery — a utilitarian loss the reading acknowledges as requiring minimization but accepts as structurally inevitable. Their exit is trapped by the same procedural barriers that produced the wrongful conviction.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted_capital_defendants, payer,
    powerless, biographical, trapped, national).

% All persons charged with capital crimes who face the possibility of execution. They bear the threat cost — the psychological and strategic burden of death-eligible prosecution — regardless of eventual outcome. Their exit is constrained by the legal process; plea bargains to life-without-parole are the primary escape route, which the deterrence reading treats as a substitute deterrent.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, capital_defendants_facing_execution, payer,
    powerless, immediate, constrained, national).

% Civil rights groups, religious organizations, and human rights NGOs that categorically oppose state killing. They are structurally excluded from the deterrence calculus — their moral framework rejects the utilitarian trade-off entirely. They would object to every execution and to the system's existence, but their voice is not a parameter in the deterrence reading's optimization function.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolitionist_organizations, excluded,
    organized, generational, mobile, national).

% Criminologists, economists, and legal scholars who study whether execution actually deters murder. They produce the empirical evidence the deterrence reading cites (or ignores). Their seat is analytical — they neither collect nor pay, but their work determines whether the reading's foundational claim survives contact with data.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, deterrence_scholars, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of credible lethal threat: the state must convince potential murderers that the cost of capital crime exceeds the benefit, using the ultimate sanction as the commitment device. The coordination is between the state's threat and the potential offender's calculation — a single, clear price signal for the gravest offense.
% TRANSFER_FUNCTION: Moves the lives of executed offenders (and the error-rate burden of wrongful executions) to the statistical survival of future potential victims. The transfer is utilitarian: N executions are justified if they prevent >N murders, with wrongful executions counted as system overhead requiring minimization.
% ABSENT_VOICES: The abolitionist reading's core constituency — those who hold that state killing is categorically impermissible regardless of consequences — are excluded from the deterrence calculus. They are present in the political arena but their objection is not a variable in the utilitarian optimization; the reading cannot 'hear' a deontological veto. Also absent: the families of executed offenders who bear grief without deterrence benefit, and the international human rights framework that treats abolition as a norm.
% DISAPPEARANCE_RATIONALE: If the deterrence reading vanished overnight (i.e., if the empirical claim that execution deters better than life-without-parole were falsified and the authority accepted this), the capital punishment system would lose its primary utilitarian justification. States would face pressure to substitute life-without-parole, the execution machinery would lose its coordinating rationale, and the retributive reading would become the sole remaining defense — a narrower, more contested foundation. The world rearranges because the arrangement currently coordinates policy, budget, and legal doctrine around a specific empirical claim.
% FOUNDING_PROBLEM: The post-Furman (1972) crisis of legitimacy: the Supreme Court struck down existing death penalty statutes as arbitrarily applied. States needed a constitutionally acceptable rationale that could survive equal-protection and due-process scrutiny. The deterrence claim — measurable, consequentialist, seemingly objective — offered a 'scientific' justification that avoided the arbitrariness and moral subjectivity that doomed the prior regime.
% FOUNDING_PROBLEM_CORROBORATION: The deterrence rationale was explicitly articulated in Gregg v. Georgia (1976) joint opinions as a penological purpose sufficient to satisfy the Eighth Amendment. However, the National Research Council (2012) concluded that existing studies do not establish a deterrent effect. The American Law Institute withdrew its Model Penal Code capital punishment provisions in 2009 citing 'intractable institutional and structural obstacles' including the deterrence question. The founding problem (arbitrariness) was the Court's problem; the deterrence solution was the states' answer. The corroboration comes from the Court's own opinions and subsequent institutional actors (ALI, NRC) outside the benefiting parties.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the moderate but non-trivial cost imposed on capital defendants and executed offenders, moderated by the substitution possibility (life-without-parole as alternative deterrent). Suppression (0.70) is high because the constraint's persistence depends on excluding abolitionist alternatives, restricting executive clemency, and maintaining procedural barriers that limit error-correction — the machinery of death is actively defended against exit. Theater ratio (0.25) is low-moderate: the deterrence claim is the system's primary public justification, but the gap between that claim and the empirical reality generates performative maintenance (elaborate protocols, 'humane' method cycles, extensive appeals that rarely change outcomes). Accessibility collapse (0.45) is moderate — alternatives (life-without-parole, restorative justice, abolition) exist and are implemented in peer nations, but the U.S. constitutional framework makes substitution politically difficult. Resistance (0.40) reflects sustained abolitionist litigation, legislative repeal efforts in some states, and declining public support, but not a systemic threat to the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharp seat divergence: from the state authority and law enforcement seats, the constraint computes toward rope/tangled_rope (coordination with manageable extraction); from the executed offender and wrongfully convicted seats, it computes toward snare (pure extraction with no exit); from the future potential victims seat, it computes toward rope (pure benefit, no cost); from the abolitionist seat, the constraint is invisible (excluded from the calculus). The deterrence reading's utilitarian framing makes this divergence a feature, not a bug — the calculus explicitly weighs seats against each other.
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution authority (agenda_setter, institutional, arbitrage exit) sits at the beneficiary end of directionality — it controls the machinery and collects institutional legitimacy from it. Future potential victims (beneficiary, powerless, trapped) are the theoretical beneficiaries of the deterrence calculus but have no agency in the arrangement. Public safety advocates and law enforcement (beneficiaries, organized/institutional, mobile/constrained exit) gain concrete resources and missions. Executed offenders and wrongfully convicted (payers, powerless, trapped) bear the full and catastrophic cost with zero exit. Capital defendants facing execution (payers, powerless, constrained exit) bear threat cost with only plea-bargain escape. Abolitionist organizations (excluded, organized, mobile) are structurally silenced in the utilitarian calculus. Deterrence scholars (observer, moderate, analytical) provide the empirical check that could destabilize the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Furman-era arbitrariness) has been formally addressed by the guided-discretion statutes the Court approved in Gregg. Yet the deterrence rationale persists as the primary constitutional justification even as the empirical foundation erodes. This is mandatrophy: the mandate (constitutionally acceptable rationale) has outlived its function (the deterrence claim is empirically unsupported), but the constraint persists because the retributive reading provides a fallback justification and the institutional machinery is self-sustaining. The deterrence reading specifically prevents mislabeling the arrangement as pure coordination (rope) by acknowledging the instrumental cost borne by executed offenders, while also preventing mislabeling as pure extraction (snare) by maintaining the coordination function (credible threat) as the stated purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_status,
    'Does execution actually deter murder more effectively than life-without-parole?',
    'Natural experiment from abolition states vs. retentionist states; panel data with controls for policing, demographics, economic conditions; the National Research Council (2012) review concluded existing studies are insufficient to establish an effect either way.',
    'If deterrence is empirically falsified, the reading''s foundational axiom (execution_prevents_murder) collapses, reclassifying the constraint from tangled_rope toward snare (coordination function vanishes, leaving only extraction). If substantiated, the coordination function is validated and the extraction is justified within the reading''s own calculus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_empirical_status, empirical, 'The empirical foundation of the deterrence reading''s coordination claim.').

omega_variable(
    substitutability_of_lwop,
    'Can life-without-parole substitute for execution in the deterrence calculus without loss of deterrent effect?',
    'Comparative deterrence studies in jurisdictions that abolished execution but retain LWOP; analysis of murder rates pre/post abolition with LWOP as the maximum sanction.',
    'If LWOP is an equally effective deterrent, the deterrence reading''s extraction becomes unnecessary — the coordination function is achievable without the lethal transfer, shifting the constraint toward snare (extraction without coordination necessity). If LWOP is inferior, the extraction is structurally necessary for the claimed coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitutability_of_lwop, empirical, 'Whether the coordination function requires the specific extraction of execution.').

omega_variable(
    error_rate_minimization_feasibility,
    'Can the wrongful execution rate be reduced to a level the deterrence calculus accepts as acceptable overhead?',
    'DNA exoneration data, procedural reform tracking, innocence project caseloads; the reading requires a quantified acceptable error rate, which has never been authoritatively declared.',
    'If the error rate has a structural floor above what the utilitarian calculus can tolerate (given the deterrence benefit magnitude), the reading contains an internal contradiction — the overhead exceeds the justified expenditure. This would reclassify toward snare (the extraction includes unavoidable catastrophic errors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_rate_minimization_feasibility, conceptual, 'Whether the deterrence reading''s own error-minimization requirement is structurally satisfiable.').

omega_variable(
    reading_relations_framing,
    'Does the deterrence reading''s utilitarian framework logically foreclose the retributive reading, or do they coexist as distinct justifications held by different parties?',
    'Analyze whether a single authority structure can simultaneously hold ''execution is justified because it deters'' and ''execution is justified because it restores moral balance'' without contradiction — the former is consequentialist, the latter deontological. The Court''s opinions often cite both.',
    'If they coexist, the kernel has multiple live readings (coexists_with). If the deterrence reading''s consequentialism logically displaces retributive desert as the ground of justification, it forecloses the retributive reading within a single framework. This determines the reading_relations declaration and the kernel''s structural topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_framing, conceptual, 'Structural relationship between deterrence and retributive readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_execution_authority__deterrence_reading, theater_ratio, 1976, 0.15).
narrative_ontology:measurement(stat_tr_t1990, state_execution_authority__deterrence_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(stat_tr_t2000, state_execution_authority__deterrence_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(stat_tr_t2010, state_execution_authority__deterrence_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(stat_tr_t2020, state_execution_authority__deterrence_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(stat_tr_t2026, state_execution_authority__deterrence_reading, theater_ratio, 2026, 0.25).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_execution_authority__deterrence_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement(stat_be_t1990, state_execution_authority__deterrence_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(stat_be_t2000, state_execution_authority__deterrence_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(stat_be_t2010, state_execution_authority__deterrence_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(stat_be_t2020, state_execution_authority__deterrence_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(stat_be_t2026, state_execution_authority__deterrence_reading, base_extractiveness, 2026, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_execution_authority__deterrence_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(stat_su_t1990, state_execution_authority__deterrence_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement(stat_su_t2000, state_execution_authority__deterrence_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(stat_su_t2010, state_execution_authority__deterrence_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(stat_su_t2020, state_execution_authority__deterrence_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(stat_su_t2026, state_execution_authority__deterrence_reading, suppression_requirement, 2026, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__deterrence_reading, 0.12).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the state_execution_authority constraint family (kernel). The three readings (deterrence, retributive, abolition) decompose the single natural-language concept 'capital punishment' into structurally distinct constraints with different ε, different beneficiary/victim sets, and different types. The deterrence reading's ε (0.55) is moderate — the coordination claim is contested and substitution is possible. The retributive reading's ε would be higher (no substitution, desert is non-negotiable). The abolition reading's ε would be near-zero for the standing arrangement (it rejects the arrangement entirely) but high for the alternative it proposes (categorical prohibition as coordination). The readings are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__deterrence_reading, institutional, 0.15).
constraint_indexing:directionality_override(state_execution_authority__deterrence_reading, powerless, 0.95).
constraint_indexing:directionality_override(state_execution_authority__deterrence_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
