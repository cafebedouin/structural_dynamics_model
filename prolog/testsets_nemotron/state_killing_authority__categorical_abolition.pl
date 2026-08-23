% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: Categorical Abolition of State Killing Authority
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the categorical_abolition reading of
 *   the state_killing_authority kernel. The reading asserts that state
 *   killing is inherently impermissible regardless of crime or consequence
 *   because life is inalienable — a right that cannot be forfeited, waived,
 *   or overridden by state authority. The condemned person remains a
 *   rights-holder; the state becomes a potential violator if it executes;
 *   victims' families are split, with abolitionist families systematically
 *   marginalized by prosecutorial structures that privilege retributive
 *   voices. The claim/metric independence is deliberate: the constraint is
 *   CLAIMED as mountain (natural law, inalienable right) while the authored
 *   metrics describe its historical trajectory — declining extraction and
 *   suppression as abolition spreads, but persistent resistance from
 *   retributive and deterrence readings. The engine measures the divergence
 *   between claim and metric profile; do not reconcile them.
 *
 * KEY AGENTS:
 *   - condemned_persons: Primary beneficiary (powerless/trapped) — life inalienable, cannot be forfeited
 *   - state_execution_apparatus: Potential violator (institutional/arbitrage) — enters violator set if it kills
 *   - retributive_victims_families: Contested seat (moderate/constrained) — want execution, see abolition as denial of justice
 *   - abolitionist_victims_families: Excluded beneficiary (powerless/identity_locked) — oppose execution, marginalized by prosecutors
 *   - human_rights_institutions: Agenda setter (institutional/analytical) — codify and enforce the prohibition
 *   - retributive_desert_reading: Sibling reading (institutional/analytical) — claims murderers forfeit right to life
 *   - deterrence_instrument_reading: Sibling reading (institutional/analytical) — claims killing justified by future prevention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.12).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.05).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.12).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, mountain).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Categorical Abolition of State Killing Authority").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:emerges_naturally(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '12256fc5-30c2-4241-997d-da258b064de0').
narrative_ontology:cs_kernel_codification('12256fc5-30c2-4241-997d-da258b064de0', formalized).
narrative_ontology:cs_authority_grounding('12256fc5-30c2-4241-997d-da258b064de0', lineage).
narrative_ontology:cs_interpretation_layer_present('12256fc5-30c2-4241-997d-da258b064de0').
narrative_ontology:cs_reading_relation('12256fc5-30c2-4241-997d-da258b064de0', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('12256fc5-30c2-4241-997d-da258b064de0', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_axiom('12256fc5-30c2-4241-997d-da258b064de0', foundational, life_inalienable_absolute).
narrative_ontology:cs_axiom_status(life_inalienable_absolute, holdable).
narrative_ontology:cs_axiom_grounding('12256fc5-30c2-4241-997d-da258b064de0', life_inalienable_absolute, deontological).
narrative_ontology:cs_axiom('12256fc5-30c2-4241-997d-da258b064de0', foundational, state_killing_never_legitimate).
narrative_ontology:cs_axiom_status(state_killing_never_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('12256fc5-30c2-4241-997d-da258b064de0', state_killing_never_legitimate, deontological).
narrative_ontology:cs_axiom('12256fc5-30c2-4241-997d-da258b064de0', secondary, right_to_life_non_derogable).
narrative_ontology:cs_axiom_status(right_to_life_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('12256fc5-30c2-4241-997d-da258b064de0', right_to_life_non_derogable, conventional).
narrative_ontology:cs_reference_frame('12256fc5-30c2-4241-997d-da258b064de0', inalienable_right_to_life_framework).
narrative_ontology:cs_drift_state('12256fc5-30c2-4241-997d-da258b064de0', contemporary_abolitionist_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('12256fc5-30c2-4241-997d-da258b064de0', '2026-08-10T14:32:00Z').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, abolitionist_families).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, human_rights_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, retributive_victims_families).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, retributive_victims_families).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, right_to_life_inalienable).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, state_monopoly_violence_limited_by_rights).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, proportionality_doctrine_excludes_killing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face execution by the state. Under this reading, their right to life is inalienable — it cannot be forfeited by crime, waived by choice, or overridden by state authority. They have no exit: they are physically confined, legally sentenced, and politically silenced. The constraint's prohibition is the only structural barrier between them and state killing. Their identity as rights-holders is fused to the claim that life cannot be taken — identity_locked on the inalienability premise.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, beneficiary,
    powerless, immediate, trapped, universal).

% The institutional machinery that carries out executions: legislatures that authorize capital statutes, prosecutors who seek death sentences, courts that impose them, corrections departments that execute. Under this reading, the apparatus is not a legitimate enforcer but a potential rights-violator. Its exit options are arbitrage-grade: it can comply (abolish), evade (moratoriums, secret executions), or contest (retributive/deterrence justifications). It holds the coercive power to kill but faces normative pressure not to.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_execution_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Families of murder victims who want the perpetrator executed. They experience the abolition constraint as denying them justice (lex talionis) and closure. Prosecutorial systems privilege their voices — victim impact statements, consultation on plea deals, notification of execution dates. Their exit is constrained: they cannot independently execute, but they can advocate politically, lobby legislators, and influence prosecutorial decisions. They are payers (bear the cost of the constraint denying their preferred outcome) and incidental beneficiaries (the constraint also prevents state killing of their loved ones, though they don't claim this).
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_victims_families, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, retributive_victims_families, beneficiary).

% Families of murder victims who oppose execution of the perpetrator — on religious, moral, or principled grounds. They are structurally excluded from prosecutorial decision-making: victim impact statement procedures assume retributive stance; clemency processes weight retributive voices higher; media narratives frame 'victims' families' as monolithically pro-execution. Their identity as victims is fused to opposition to state killing — identity_locked because abandoning that opposition would feel like betraying their loved one's memory or their own conscience. They are the constraint's natural beneficiaries but are excluded from its enforcement.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_victims_families, excluded,
    powerless, biographical, identity_locked, local).

% International and regional bodies (UN Human Rights Committee, European Court of Human Rights, Inter-American Court, African Commission) that codify the right to life as inalienable and monitor abolition compliance. They set the agenda through treaty interpretation, general comments, and case law. They collect no rents from the constraint — their benefit is institutional legitimacy and normative authority. Exit is analytical: they observe and adjudicate, they do not participate in the constraint's operation.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, human_rights_institutions, agenda_setter,
    institutional, generational, analytical, global).

% The sibling reading that murderers forfeit their right to life; proportional punishment (lex talionis) requires death for death. This is not a human agent but a normative position institutionalized in retentionist legal systems, prosecutorial cultures, and public opinion. It coexists with this reading as a live competitor — different parties hold each, neither logically eliminates the other within a single framework. It claims the state_killing_authority kernel authorizes execution as justice.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_desert_reading, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(state_killing_authority__categorical_abolition, retributive_desert_reading).

% The sibling reading that capital punishment is justified if and only if it prevents future murders at acceptable cost. Institutionalized in policy debates, economic studies of deterrence, and legislative hearings. It coexists with this reading — different epistemic premises (empirical contingency vs. deontological absolute) but neither forecloses the other in public discourse. It claims the kernel authorizes killing as prevention.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, deterrence_instrument_reading, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(state_killing_authority__categorical_abolition, deterrence_instrument_reading).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a universal normative prohibition against state killing, replacing the sovereign prerogative of life-and-death with a rights-based limit that binds all states regardless of domestic law. Solves the coordination problem of preventing arbitrary state violence by establishing a non-derogable floor.
% TRANSFER_FUNCTION: Transfers the authority to kill from the state to no one — the power is extinguished, not redistributed. The condemned person retains their life (inalienable); the state loses its claim to legitimate killing. No recipient captures the transfer; it is a negative transfer (removal of a power).
% ABSENT_VOICES: Abolitionist victims' families are structurally excluded from prosecutorial and clemency processes that assume retributive consensus. Future generations who would inherit a world without state killing are absent by definition. The global poor, disproportionately sentenced to death, are absent from the international treaty bodies that codify the prohibition.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, retentionist states would expand executions (more crimes, faster processes, fewer procedural barriers); abolitionist states would face immediate political pressure to reinstate; the normative floor preventing arbitrary state killing would collapse. The world rearranges because the constraint is the only structural barrier between condemned persons and state execution in 55+ retentionist jurisdictions.
% FOUNDING_PROBLEM: The sovereign's unlimited power over life and death — the historical state killing authority that treated execution as a prerogative of sovereignty, not a limited exception. The problem was built to solve: arbitrary monarchical killing, political executions, and the use of death as a tool of suppression rather than justice.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist historiography (Hood & Hoyle, Schabas) attests the founding problem was arbitrary sovereign killing. Retentionist legal traditions (Scalia, Bork, contemporary prosecutors) attest the founding problem was inadequate punishment for heinous crimes — the sovereign power was the solution, not the problem. No neutral arbiter corroborates; the founding problem itself is the kernel's contested territory.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, ExtMetricName, E),
    domain_priors:suppression_score(state_killing_authority__categorical_abolition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(state_killing_authority__categorical_abolition),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint primarily *prevents* extraction (state taking life) rather than extracting. The historical trajectory shows declining extraction as abolition spreads: 1948 (UDHR era, most states retain death penalty) to 2020 (144 abolitionist states). Suppression is low (0.05) because the constraint's persistence does not depend on coercion — it depends on normative recognition. Theater ratio (0.18) reflects performative compliance: some states retain death penalty on books but don't execute (moratorium states), or execute rarely while claiming deterrence. Accessibility collapse (0.22) is low because alternatives (life imprisonment, restorative justice) remain visible and practiced. Resistance (0.78) is high because retributive and deterrence readings remain politically potent — the constraint meets active opposition, not passive acceptance.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different seat classifications: for condemned persons (powerless, trapped, identity_locked on right to life), the constraint is Mountain — their life cannot be taken. For the state execution apparatus (institutional, arbitrage exit), the constraint is experienced as Rope or Scaffold — a coordination limit they must work around. For retributive victims' families (moderate, constrained), the constraint may compute as Snare — it denies them what they see as justice. For abolitionist victims' families (powerless, identity_locked), the constraint is Mountain but they are excluded from its enforcement. The structural asymmetry is the point: the same constraint is Mountain for some seats, extractive for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: condemned_persons (d ≈ 0.0 — full beneficiary, life inalienable), abolitionist_families (d ≈ 0.1 — benefit from prohibition, but excluded from enforcement), human_rights_institutions (d ≈ 0.05 — agenda setters who gain legitimacy from the norm). No victims declared in base_properties because the constraint's *primary* structural function is prevention of killing, not extraction from a victim group. The retributive and deterrence readings are SIBLING CONSTRAINTS (other kernel readings), not victims of this one. The state execution apparatus is a potential violator — its directionality would be high (d ≈ 0.9) if it attempted to execute, but the constraint's low suppression means it mostly complies without active coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state killing as sovereign prerogative) is dead in abolitionist jurisdictions but live in retentionist ones. The constraint's mandate (abolition) remains live because the kernel (state killing authority) persists globally. No mandatrophy resolution: the constraint continues to serve its coordination function (normative prohibition) and has not atrophied into performance. Theater ratio decline (0.45→0.18) tracks genuine normative diffusion, not performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the state_killing_authority kernel, and does its structural identity depend on that kernel membership?',
    'Cross-reading structural comparison: if categorical_abolition, retributive_desert, and deterrence_instrument have fundamentally different victim/beneficiary structures and extractiveness profiles, they are distinct constraints linked by kernel_id, not measurement variants of one constraint.',
    'Confirms ε-invariance principle: each reading is its own constraint with its own ε, not a single constraint measured differently. Prevents conflation of the kernel''s contested readings into one story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint is one reading of a contested kernel rather than a standalone constraint.').

omega_variable(
    natural_law_vs_constructed_norm,
    'Is the inalienability of life a natural law constraint (mountain) or a constructed normative commitment that could be revised?',
    'Historical and cross-cultural survey: if no stable society has operated without some form of state killing authority (war, execution, police use of force), the claim of natural law faces empirical challenge. If the constraint is a normative achievement of specific historical struggles, it is constructed.',
    'If constructed, the mountain claim is a false summit — beneficiaries (human rights institutions, abolitionist families) capture the constraint''s authority while the underlying arrangement remains contestable. FSM signature would trigger reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, empirical, 'Whether the categorical prohibition is a genuine natural law or a constructed norm with identifiable beneficiaries.').

omega_variable(
    victim_family_exclusion_mechanism,
    'Are abolitionist victims'' families structurally excluded from prosecutorial decision-making, or is their exclusion contingent on political circumstance?',
    'Institutional analysis of victim impact statement procedures, prosecutorial discretion guidelines, and clemency processes across jurisdictions. Compare treatment of retributive vs. abolitionist family voices.',
    'If exclusion is structural (prosecutors systematically marginalize abolitionist families), the constraint operates as a snare for that subgroup — the state kills over their objection. If contingent, the constraint''s suppression profile is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_family_exclusion_mechanism, empirical, 'Whether the marginalization of abolitionist victims'' families is a structural feature of the constraint or a variable political outcome.').

omega_variable(
    state_as_potential_violator,
    'Does this reading structurally position the state as a potential rights-violator when it executes, and does that positioning change the constraint''s classification?',
    'Analyze the directional reversal: in retributive/deterrence readings, the state is the legitimate enforcer; in this reading, the state becomes the threat. Trace how this reversal affects beneficiary/victim assignments and extraction directionality.',
    'If the state is a structural violator, the constraint''s suppression metric should reflect the state''s coercive capacity to execute despite the prohibition. The reading''s mountain claim requires that the prohibition holds even against state power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_as_potential_violator, conceptual, 'How the state-as-violator framing structurally inverts the standard enforcement relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 1948, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_killing_authority__categorical_abolition_tr_t1948, state_killing_authority__categorical_abolition, theater_ratio, 1948, 0.45).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_tr_t1972, state_killing_authority__categorical_abolition, theater_ratio, 1972, 0.38).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_tr_t1984, state_killing_authority__categorical_abolition, theater_ratio, 1984, 0.3).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_tr_t1998, state_killing_authority__categorical_abolition, theater_ratio, 1998, 0.24).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_tr_t2007, state_killing_authority__categorical_abolition, theater_ratio, 2007, 0.2).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_tr_t2020, state_killing_authority__categorical_abolition, theater_ratio, 2020, 0.18).

% Extraction over time
narrative_ontology:measurement(state_killing_authority__categorical_abolition_be_t1948, state_killing_authority__categorical_abolition, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_be_t1972, state_killing_authority__categorical_abolition, base_extractiveness, 1972, 0.28).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_be_t1984, state_killing_authority__categorical_abolition, base_extractiveness, 1984, 0.22).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_be_t1998, state_killing_authority__categorical_abolition, base_extractiveness, 1998, 0.18).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_be_t2007, state_killing_authority__categorical_abolition, base_extractiveness, 2007, 0.15).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_be_t2020, state_killing_authority__categorical_abolition, base_extractiveness, 2020, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(state_killing_authority__categorical_abolition_su_t1948, state_killing_authority__categorical_abolition, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_su_t1972, state_killing_authority__categorical_abolition, suppression_requirement, 1972, 0.45).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_su_t1984, state_killing_authority__categorical_abolition, suppression_requirement, 1984, 0.3).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_su_t1998, state_killing_authority__categorical_abolition, suppression_requirement, 1998, 0.15).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_su_t2007, state_killing_authority__categorical_abolition, suppression_requirement, 2007, 0.08).
narrative_ontology:measurement(state_killing_authority__categorical_abolition_su_t2020, state_killing_authority__categorical_abolition, suppression_requirement, 2020, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__categorical_abolition, 0.1).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of state_killing_authority kernel. This reading (categorical_abolition) claims mountain with ε=0.12. Retributive_desert claims snare/tangled_rope with high ε (state extracts life from condemned). Deterrence_instrument claims tangled_rope with moderate ε (state kills for prevention). The ε values differ by >0.5 — they are structurally distinct constraints, not measurement variants. Network edges reflect upstream influence: this reading's normative diffusion creates legitimacy pressure on sibling readings (influences relation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, institutional, 0.85).
constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, powerless, 0.05).
constraint_indexing:directionality_override(state_killing_authority__categorical_abolition, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
