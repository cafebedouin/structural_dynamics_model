% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: State Execution as Categorical Violation of Human Dignity (Abolition Reading)
 *   domain: political philosophy/criminal justice/human rights
 *
 * SUMMARY:
 *   The abolition reading of state killing frames execution as a categorical
 *   violation of human dignity that cannot be justified by desert,
 *   deterrence, or utility. From this reading's perspective, the condemned
 *   person retains full moral status as a rights-bearer despite their crime.
 *   The state's power to execute is constructed as extractive—it takes the
 *   condemned person's life while offering no genuine coordination function,
 *   only legitimating narratives (retribution, deterrence) that the abolition
 *   reading treats as cover stories. This constraint instantiates a single
 *   normative reading of a contested kernel; the retributive and deterrence
 *   readings occupy separate constraint stories with different ε values,
 *   beneficiary structures, and classifications.
 *
 * KEY AGENTS:
 *   - condemned_person: powerless, trapped, immediate time horizon — subject of the constraint, bears the extraction (loss of life)
 *   - state_execution_apparatus: institutional, arbitrage options, generational time horizon — agenda-setter, enforces and justifies the constraint
 *   - retributive_justifiers: organized, constrained options, generational — excluded from this reading's framework, their foundational claim (forfeiture of rights) is rejected
 *   - deterrence_justifiers: organized, constrained options, generational — excluded from this reading's framework, their utilitarian calculus is incompatible with rights constraints
 *   - human_rights_advocates: organized, mobile options, biographical — analytical observers from outside the enforcement structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.92).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.88).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "State Execution as Categorical Violation of Human Dignity (Abolition Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "political philosophy/criminal justice/human rights").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '4c48746c-e6cd-4752-8e19-149c3b659269').
narrative_ontology:cs_kernel_codification('4c48746c-e6cd-4752-8e19-149c3b659269', formalized).
narrative_ontology:cs_authority_grounding('4c48746c-e6cd-4752-8e19-149c3b659269', extraction).
narrative_ontology:cs_interpretation_layer_present('4c48746c-e6cd-4752-8e19-149c3b659269').
narrative_ontology:cs_reading_relation('4c48746c-e6cd-4752-8e19-149c3b659269', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('4c48746c-e6cd-4752-8e19-149c3b659269', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('4c48746c-e6cd-4752-8e19-149c3b659269', foundational, human_dignity_inalienable).
narrative_ontology:cs_axiom_status(human_dignity_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('4c48746c-e6cd-4752-8e19-149c3b659269', human_dignity_inalienable, deontological).
narrative_ontology:cs_axiom('4c48746c-e6cd-4752-8e19-149c3b659269', foundational, state_authority_bounded_by_rights).
narrative_ontology:cs_axiom_status(state_authority_bounded_by_rights, holdable).
narrative_ontology:cs_axiom_grounding('4c48746c-e6cd-4752-8e19-149c3b659269', state_authority_bounded_by_rights, deontological).
narrative_ontology:cs_reference_frame('4c48746c-e6cd-4752-8e19-149c3b659269', dignity_protective_legal_order).
narrative_ontology:cs_drift_state('4c48746c-e6cd-4752-8e19-149c3b659269', contemporary_human_rights_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4c48746c-e6cd-4752-8e19-149c3b659269', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_person_as_rights_bearer).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, condemned_person_as_rights_bearer).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, condemned_person).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces execution by the state. The abolition reading construes the condemned person as a rights-bearer whose dignity is violated categorically by execution, regardless of what crime they committed or whether execution deters future crimes. Their exit is nonexistent—the constraint is the state's power to take their life. They bear the ultimate extraction: loss of life itself.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_person, payer,
    powerless, immediate, trapped, national).

% The state institution that administers executions—legislature, judiciary, executive. Sets and enforces the constraint by maintaining death penalty statutes, carrying out sentences, defending the practice as legitimate punishment. Justifies executions on retributive or deterrent grounds; the abolition reading rejects both justifications as incoherent within a rights framework.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Judicial, legislative, and public advocates who defend execution on grounds of proportional desert (the murderer 'deserves' death). They are excluded from the abolition reading's framework—their foundational claim that murderers forfeit the right to live is categorically rejected as incompatible with human dignity.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retributive_justifiers, excluded,
    organized, generational, constrained, national).

% Criminal justice officials and empiricists who defend execution as a rational signal preventing future murders. They are excluded from this reading's framework—their utilitarian calculus (execution's deterrent effect) is incompatible with categorical dignity rights, which cannot be traded off for collective benefit.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, deterrence_justifiers, excluded,
    organized, generational, constrained, national).

% International human rights organizations, abolitionist jurisdictions, and activists who hold the abolition reading. They document executions, contest their legitimacy on rights grounds, and work for legislative and treaty changes. They are observers of the constraint from outside its enforcement structure, not parties to it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, human_rights_advocates, observer,
    organized, biographical, mobile, global).

% Families of murder victims occupy a structurally contested seat. The retributive and deterrence readings place them as beneficiaries (satisfaction, future protection); the abolition reading excludes them from the legitimacy framework—their suffering does not justify categorical violation of the condemned person's dignity. Some families themselves reject execution; others seek it; they are neither unified nor properly seats for this reading's logic.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, victims_families, excluded,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__abolition_reading, victims_families, observer).

% The abstract principle 'human dignity itself' as it inheres in the condemned person. Listed for narrative completeness (the abolition reading grounds its objection in dignity as a property of personhood, not contingent on action or desert), but does not collect rents or bear costs as an agent—it is a normative commitment, not an actor.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_person_as_nonagent, observer,
    analytical, immediate, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__abolition_reading, condemned_person_as_nonagent).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__abolition_reading, state_execution_apparatus).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function. The abolition reading rejects any framing that makes execution a coordination mechanism. If retributive or deterrence rationales claimed to coordinate (proportional justice systems, collective deterrence through example), the abolition reading treats those as cover stories for extraction—the claimed functions do not constitute real coordination, only legitimation narratives.
% TRANSFER_FUNCTION: Moves the condemned person's life—their capacity for future action, experience, relationships—from the condemned person to the state's sovereign power and the public narrative of justice. The state transfers its authority-claim into the death, and the public collects narrative closure ('justice is done'). This is extractive transfer, not functional exchange.
% ABSENT_VOICES: The condemned person themselves is the primary absent voice—they are the subject of the constraint, not a party consulted in its legitimacy. International abolitionist jurisdictions and human rights bodies are excluded from enforcement jurisdictions' decision-making; victims' families favoring abolition are often marginalized within pro-execution jurisdictions. Religious and philosophical traditions that reject execution are excluded from state policy formation in most pro-execution regimes.
% DISAPPEARANCE_RATIONALE: If execution were abolished overnight, the state would lose a primary tool for expressing sovereign authority over life and death; retributive satisfaction narratives would collapse; the public's sense of closure in murder cases would be reorganized around other penalties (life imprisonment, restorative practices). The constraint does not solve a technical problem that would re-emerge—it expresses a particular vision of state power and justice that would need to be replaced, not recovered.
% FOUNDING_PROBLEM: The state's need to express sovereign authority over its most grave transgressors and to establish proportional justice narratives in the face of murder—to signal that the most heinous acts incur the highest penalty.
% FOUNDING_PROBLEM_CORROBORATION: Pro-execution jurisdictions and retributive theorists attest the founding problem remains live: the state must be able to express absolute justice through ultimate penalty. Abolitionist jurisdictions and international human rights bodies attest the founding problem is a constructed need, not a real functional requirement—life imprisonment, restorative justice frameworks, and state accountability regimes show the founding problem is not live. No neutral corroboration from outside the dispute exists; empirical and philosophical disagreement is the entire substance of the contest.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.92) because the abolition reading frames execution as taking something (life, agency, dignity) with no genuine compensation or coordination function—it is pure loss for the condemned person. Suppression is high (0.88) because the constraint persists through active enforcement: death penalty statutes, judicial conviction, execution protocols, and the state's monopoly on legitimate violence. The constraint must be actively maintained because public opinion is divided and alternative penalties exist; without enforcement machinery the constraint would collapse. Theater is moderate-to-high (0.41) because execution rituals (trials, appeals, formal procedures) constitute a significant share of the constraint's operation—the apparatus spends effort on legitimating narratives (justice, proportionality, closure) alongside the extraction itself. Accessibility to alternatives (0.79) is moderate: condemned persons have no exit; state systems have theoretical options (life imprisonment, restorative justice) but face political and institutional resistance to adopting them, so alternatives are formally accessible but practically blocked. Resistance (0.68) reflects the abolitionist movement's substantial opposition, international human rights pressure, and divided public opinion in many jurisdictions—execution faces meaningful contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus's seat, execution is justified punishment expressing legitimate authority over criminals—the reading would be retributive or deterrent, not abolition. From the condemned person's seat, they are subject to state violence categorically incompatible with their persistent dignity—the abolition reading applies. The abolition reading explicitly adopts the condemned person's perspective (rights-bearer, inalienable dignity) against the state's self-justification (sovereign authority, proportional justice). The engine computes this gap: the two seats should produce radically different type classifications from the same structural data, because the readings apply incommensurable normative frameworks.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned person is the full target (d = 1.0): they bear the extraction (loss of life), have trapped exit options, and powerless status. The state apparatus sits near the beneficiary end (d near 0.0): it collects the extraction (sovereignty expression, narrative closure, authority display) with no countervailing cost—the constraint serves its interests. Retributive and deterrence justifiers are excluded from the abolition reading's framework entirely—their directionality is not computed because the reading does not admit their normative premises. This is a structurally asymmetric constraint: one seat is powerless and trapped; the other is institutional with arbitrage options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state's need to express sovereign authority over grave transgressors) remains contested. Abolitionist jurisdictions show the founding problem is not functionally live—modern democracies function without execution. Pro-execution jurisdictions argue the founding problem is live—proportional justice requires death for murder. This contest is NOT resolvable by data; it is a normative disagreement about whether the state MUST have execution power or merely chooses to. The abolition reading treats the persisting execution apparatus as mandatrophy: the founding problem has become irrelevant (abolitionist jurisdictions demonstrate this), but the constraint persists due to institutional inertia, political resistance to change, and narrative investment in retributive or deterrent legitimation. The constraint's persistence is explained by political path-dependence and sunk cultural meanings, not by the live problem it was built to solve. Theater-ratio increase over the interval (0.32 → 0.41) reflects growing performative maintenance: as empirical arguments about deterrence weaken and international pressure mounts, execution is defended increasingly through ritual (appeals, clemency processes, legal forms) rather than through functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_inalienability_grounding,
    'Is human dignity a property of personhood that cannot be forfeited, suspended, or overridden by any act or circumstance? Or is dignity itself contingent on or modifiable by the transgressor''s choices?',
    'Foundational philosophical analysis and cross-cultural/cross-juridical comparison of dignity concepts. The question is not empirical; it is conceptual—what metaphysics of personhood grounds the abolition reading''s core claim.',
    'If dignity is strictly inalienable (the abolition reading''s assumption), then execution categorically violates it. If dignity is forfeitable (the retributive reading''s assumption), then the abolition reading collapses into the retributive framework and the two readings forecast to the same type (snare, but on different grounds). The divergence is NOT resolvable by evidence—it is a difference in normative foundations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_inalienability_grounding, conceptual, 'Whether human dignity is inalienable or can be forfeited by crime.').

omega_variable(
    alternative_penalty_adequacy,
    'Does the state''s foundational problem (expressing proportional justice, incapacitating the most dangerous offenders, establishing sovereign authority) require death, or can it be served by other penalties?',
    'Empirical observation from abolitionist jurisdictions: comparative crime rates, victim satisfaction, state authority durability, and legitimacy scores. Also: historical practice—most human societies have functioned without execution; the founding problem could be served by alternatives.',
    'If alternatives demonstrably serve the founding problem, the founding_problem_status should shift to ''dead'' or ''live_but_falsely_premised,'' weakening the retributive and deterrence readings. If no alternatives are perceived as adequate, the founding problem remains live and the abolition reading faces a higher burden of proof that dignity constraints override state-functional requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_penalty_adequacy, empirical, 'Whether death penalty is functionally necessary or can be replaced.').

omega_variable(
    reading_incommensurability,
    'Can the abolition reading and the retributive reading both inhabit the same normative framework, or do they depend on incommensurable foundational premises about personhood and forfeiture?',
    'Sustained philosophical dialogue (witness: Jeffrie Murphy, Hugo Bedau, Michael Tonry) shows no resolution. The readings do not disagree on empirical facts about crime or consequences; they disagree on whether personhood and dignity can be suspended by desert. This is a conceptual gap, not an empirical one.',
    'If incommensurable, no amount of empirical evidence about deterrence or retribution can move one reading toward the other. Classification divergence between abolition and retribution seats is then structural, not contingent on data. The engine should flag the kernel contest as unresolvable at the normative level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Whether the abolition and retributive readings can coexist in one framework or depend on incommensurable foundations.').

omega_variable(
    victimhood_framing_ambiguity,
    'In the abolition reading''s framework, is the condemned person the VICTIM of the constraint (as authored in base_properties.victims), or is the constraint applied TO the condemned person as a coercive target, with victimhood reserved for a different analysis?',
    'Conceptual clarification: if the condemned person is the constraint''s victim (bears the extraction—loss of life), they should appear in victims[] and the constraint is a snare on them. If the condemned person is a target of coercion but the constraint''s real victims are others (e.g., the state''s moral agency, the public''s dignity by proxy), the categorization shifts. The abolition reading construes the condemned person as both target AND victim—the coercion IS the extraction.',
    'This affects whether the engine reads the constraint as a snare (powerless victim, high extraction) or a more complex structure. The abolition reading''s framing treats them identically: the condemned person is victimized BY the state''s exercise of death power. Clarity here stabilizes the type classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victimhood_framing_ambiguity, conceptual, 'Whether the condemned person is the constraint''s victim or another category of target.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__abolition_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(stat_tr_t4, state_killing_legitimacy__abolition_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(stat_tr_t8, state_killing_legitimacy__abolition_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement(stat_tr_t12, state_killing_legitimacy__abolition_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(stat_tr_t16, state_killing_legitimacy__abolition_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__abolition_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__abolition_reading, theater_ratio, 24, 0.41).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__abolition_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(stat_be_t4, state_killing_legitimacy__abolition_reading, base_extractiveness, 4, 0.89).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__abolition_reading, base_extractiveness, 8, 0.9).
narrative_ontology:measurement(stat_be_t12, state_killing_legitimacy__abolition_reading, base_extractiveness, 12, 0.91).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__abolition_reading, base_extractiveness, 16, 0.91).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__abolition_reading, base_extractiveness, 20, 0.92).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__abolition_reading, base_extractiveness, 24, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__abolition_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(stat_su_t4, state_killing_legitimacy__abolition_reading, suppression_requirement, 4, 0.84).
narrative_ontology:measurement(stat_su_t8, state_killing_legitimacy__abolition_reading, suppression_requirement, 8, 0.85).
narrative_ontology:measurement(stat_su_t12, state_killing_legitimacy__abolition_reading, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(stat_su_t16, state_killing_legitimacy__abolition_reading, suppression_requirement, 16, 0.87).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__abolition_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(stat_su_t24, state_killing_legitimacy__abolition_reading, suppression_requirement, 24, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_legitimacy__abolition_reading, 0.08).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% The state_killing_legitimacy kernel decomposes into three structurally distinct constraints, each instantiating a different reading of the same institutional practice. The abolition reading treats execution as categorically extractive (high ε, snare); the retributive reading treats it as proportional justice (moderate ε, snare or tangled_rope depending on seat); the deterrence reading treats it as rational signaling (moderate-to-high ε, snare or tangled_rope depending on empirical deterrent effect). Each reading has incommensurable normative foundations: inalienable dignity (abolition), forfeitable desert (retribution), utilitarian prevention (deterrence). The readings do not compete as factual claims about the same thing; they compete as normative frameworks for interpreting the same institutional practice. No empirical evidence can move one reading toward another—the contest is fundamentally about personhood, rights, and state authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
