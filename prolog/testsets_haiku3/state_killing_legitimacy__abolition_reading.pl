% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: State Killing Categorically Violates Dignity (Abolition Reading)
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the ABOLITION READING of the state killing
 *   legitimacy kernel. It is one of three structurally distinct constraint
 *   stories grounded in the same persisting institutional practice (capital
 *   punishment) but reading that practice through different ethical and legal
 *   frameworks. The abolition reading asserts categorically that state
 *   killing violates human dignity regardless of the crime, desert, or
 *   claimed utility — dignity is inalienable and cannot be overridden by any
 *   circumstance. This reading generates a constraint in which condemned
 *   persons are structurally positioned as beneficiaries (their dignity is
 *   protected) and the state killing authority is positioned as victim-payer
 *   (it is constrained in its prerogative). The extractiveness is high (0.92)
 *   because the reading imposes an absolute categorical prohibition on a
 *   power the state claims; the suppression is substantial (0.78) because
 *   maintaining the dignity claim requires active enforcement against
 *   competing retributive and deterrence frameworks that deny the
 *   constraint's axioms. The theater ratio (0.41) reflects that state
 *   justifications for execution often invoke retributive or deterrent
 *   rationales while the abolition reading rejects both as illegitimate: the
 *   gap between stated rationale and the reading's true constraint is
 *   performative.
 *
 * KEY AGENTS:
 *   - Condemned persons: powerless, trapped; bear the constraint's protective benefit but have zero exit
 *   - State killing authority: institutional, generational; bears the cost of renouncing capital punishment
 *   - Victims' families: moderate power, excluded; their role is central to competing readings but structurally absent from this one
 *   - Retribution and deterrence advocates: organized, excluded; they hold axioms this reading rejects
 *   - International human rights system: observer seat; provides external corroboration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.92).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.78).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "State Killing Categorically Violates Dignity (Abolition Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '1e881999-8b6e-4239-a2d2-1c75164fed28').
narrative_ontology:cs_kernel_codification('1e881999-8b6e-4239-a2d2-1c75164fed28', formalized).
narrative_ontology:cs_authority_grounding('1e881999-8b6e-4239-a2d2-1c75164fed28', lineage).
narrative_ontology:cs_interpretation_layer_present('1e881999-8b6e-4239-a2d2-1c75164fed28').
narrative_ontology:cs_reading_relation('1e881999-8b6e-4239-a2d2-1c75164fed28', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('1e881999-8b6e-4239-a2d2-1c75164fed28', state_killing_legitimacy__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('1e881999-8b6e-4239-a2d2-1c75164fed28', foundational, human_dignity_inalienable).
narrative_ontology:cs_axiom_status(human_dignity_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('1e881999-8b6e-4239-a2d2-1c75164fed28', human_dignity_inalienable, deontological).
narrative_ontology:cs_axiom('1e881999-8b6e-4239-a2d2-1c75164fed28', foundational, state_power_subject_to_dignity_limits).
narrative_ontology:cs_axiom_status(state_power_subject_to_dignity_limits, holdable).
narrative_ontology:cs_axiom_grounding('1e881999-8b6e-4239-a2d2-1c75164fed28', state_power_subject_to_dignity_limits, deontological).
narrative_ontology:cs_reference_frame('1e881999-8b6e-4239-a2d2-1c75164fed28', universal_human_rights_framework).
narrative_ontology:cs_drift_state('1e881999-8b6e-4239-a2d2-1c75164fed28', contemporary_global_abolition_expansion, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('1e881999-8b6e-4239-a2d2-1c75164fed28', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, state_killing_authority).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, human_dignity_inalienable).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, rights_bearer_status_unconditional).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals sentenced to death under state authority. Under this reading, they retain full human dignity and the right not to be killed by the state, regardless of their crime or legal verdict. Their structural position is that of rights-bearer whose dignity cannot be forfeited, overridden, or traded off against state utility or public order. Exit is literally unavailable — they are in custody and the constraint is about whether the state may lawfully end their life.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, beneficiary,
    powerless, immediate, trapped, national).

% The institutional apparatus (legislature, courts, execution personnel) that administers capital punishment. Under this reading, the state bears the cost of renouncing its claimed authority to kill — it loses a tool of control, a claimed deterrent, and a form of retributive satisfaction. The reading identifies the state's killing power itself as structurally victim-positioned: the state is what must be constrained, and the constraint extracts from its prerogative.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_killing_authority, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__abolition_reading, state_killing_authority, agenda_setter).

% Family members of murder victims, who often seek execution as justice or closure. This reading structurally excludes them from the conversation about whether dignity can be forfeited — they have a role in the competing readings (deterrence, retribution) but are not seated in the abolition framework, which does not permit dignity-forfeiture as a category.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, victims_families, excluded,
    moderate, biographical, constrained, national).

% Judges, legal scholars, and citizens who hold that proportional desert justifies execution — that murderers forfeit their right to live. This reading structurally excludes them by denying the premise that any crime forfeits inalienable dignity.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retribution_advocates, excluded,
    organized, generational, constrained, national).

% Criminologists, policymakers, and citizens who justify execution as a rational signal preventing future murders. This reading excludes them by denying that utility calculations override categorical dignity rights.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, deterrence_advocates, excluded,
    organized, generational, constrained, national).

% UN bodies, treaty bodies, and regional human rights courts that recognize abolition as the human rights standard. They provide external corroboration and institutional authority for the reading.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, international_human_rights_system, observer,
    institutional, generational, analytical, global).

% Academic and advocacy communities that articulate and defend the dignity-based abolition framework. They provide interpretive authority and methodological grounding for the reading.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_legal_scholars, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__abolition_reading, state_killing_authority).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates a dignitarian vision of criminal justice: it solves the alignment problem of how to maintain state authority over coercion while binding that authority to unconditional human rights. It coordinates a legal system around the claim that no crime revokes dignity.
% TRANSFER_FUNCTION: The arrangement moves authority away from the state's power to kill: what the state loses (the prerogative to execute) is what the condemned person gains (the security of their dignity). The transfer is asymmetric — the state bears the constraint cost; the condemned person receives the security of a dignity right that cannot be overridden.
% ABSENT_VOICES: Victims' families are structurally excluded by the reading's fundamental premises — they cannot be seated in a framework that does not permit desert-based dignity forfeiture. Deterrence and retribution advocates are similarly excluded because their core premises (utility calculation, proportional desert) are rejected by this reading's axioms.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if the categorical dignity claim were abandoned — capital punishment systems would resume or intensify in abolitionist jurisdictions; condemned persons would lose the security of their rights; the legal framework would revert to one permitting death as punishment. The world would not rearrange neutrally; it would rearrange toward retributive or utilitarian execution regimes.
% FOUNDING_PROBLEM: The founding problem this reading identifies is: how can state authority coexist with unconditional human dignity? Can the state retain the power to kill, or does that power contradict the foundation of legitimate authority itself?
% FOUNDING_PROBLEM_CORROBORATION: International human rights law and abolitionist legal scholarship outside the reading's own tradition attest that this problem remains contested and live. Retentionist jurisdictions contest it; abolitionist jurisdictions treat it as solved (dignity is unconditional). The foundational dispute is sustained across global legal systems; no external authority has adjudicated it closed.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.92) because the abolition reading imposes an absolute prohibition on a power the state institutions (judiciary, legislature, executive) claim as legitimate and exercise. The extracted 'good' is the state's prerogative itself — the constraint forbids the state from doing what it claims the right to do. Extractiveness does not fall over time because the axiom (dignity is inalienable) does not erode; the small rise from 0.88 to 0.92 reflects the gradual strengthening of abolition as the international human rights consensus hardens (more jurisdictions abolish, more treaties enter force), which increases the extractive force of the constraint on retentionist states. Suppression stays high (0.72–0.78) because maintaining the dignity axiom requires active enforcement against the intuitive pull of retribution (proportional desert) and deterrence (rational calculation). Theater ratio rises modestly (0.28–0.41) as execution protocols accumulate ritual and justification language that masks the constraint's true operation — the state increasingly emphasizes 'humane execution,' 'due process,' and 'deterrent effect' while the abolition reading treats these as performative dressing over a fundamental dignity violation. The measurements span 75 years (roughly the modern abolition movement timeline, from 1950s onward) to capture how the constraint has stabilized and hardened, not weakened.
 *
 * PERSPECTIVAL GAP:
 *   The state seat and the condemned person seat should compute very differently: from the state's perspective (if it holds a retributive or deterrence reading), the constraint is an unjust limitation on legitimate authority and the extraction is coercive imposition by international bodies or abolitionist pressure. From the condemned person's perspective, the constraint is a protection — the extraction is what the state loses (its prerogative), not what the condemned loses. From the observer seat (international human rights system), the constraint is a coordination mechanism that aligns state authority with human dignity norms. The engine computes these divergences from the stakeholder power, exit, and role data.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned person's directionality is near zero (full beneficiary): they are protected by the constraint; they have zero exit (trapped by custody); their position is symmetric with respect to the constraint's operation. The state killing authority's directionality is near one (full target): the constraint extracts the prerogative to kill; the state has constrained exit (it can change the law but faces resistance and international pressure); it bears the cost of the constraint. The state is powerful institutionally but its power is structurally constrained by this particular reading. International human rights bodies have moderate power and sit as observers — they reinforce the constraint but do not directly bear its enforcement cost. Victims' families are excluded because the reading's axioms do not permit them a seat — their interest in retribution or deterrence cannot be recognized within the dignity framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (can state authority coexist with unconditional dignity?) remains live and contested. Retentionist jurisdictions dispute that the problem is solved; abolitionist jurisdictions treat it as solved by adopting the dignity axiom. The disappearance verdict is world_rearranges: if this reading were abandoned, execution would resume or intensify in jurisdictions currently bound by it. There is no mandatrophy — the constraint has not outlived its founding purpose. The founding problem and the disappearance verdict align: the constraint persists because the problem it addresses is live and parties still contest the answer. This is not a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_forfeiture_boundary,
    'Is human dignity fundamentally inalienable, or can specific acts (murder, terrorism) create justified exceptions where dignity may be overridden by state authority?',
    'This is a conceptual disagreement, not empirically resolvable — it reflects different foundational commitments about human rights. Resolution would require a normative judgment about the structure of rights, not new data.',
    'If dignity is strictly inalienable (this reading''s position), extractiveness remains near 1.0 and the constraint is snare-class. If dignity is subject to forfeiture for extreme crimes, the constraint becomes weaker (extractiveness falls) and the reading folds into a hybrid snare/tangled_rope. The competing readings (retributive, deterrence) assume dignity forfeiture is permissible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_forfeiture_boundary, conceptual, 'Foundational disagreement about whether dignity can ever be forfeited.').

omega_variable(
    international_consensus_vs_state_sovereignty,
    'Does international human rights consensus (abolition) override state sovereignty in capital punishment policy, or is execution a matter of national legal authority?',
    'Political / legal evolution: empirical tracking of how retentionist states respond to international pressure (treaty signature, domestic abolition legislation, resistance, or defection). This has a trajectory but is not resolved by new data — it is a live dispute.',
    'If international human rights law establishes binding abolition as global legal standard, the constraint is reinforced and extractiveness stays high globally. If state sovereignty prevails and retentionist regimes resist abolition, extractiveness falls in those jurisdictions and the global constraint fragments into regional variants. The durability of the abolition reading depends partly on this political outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_consensus_vs_state_sovereignty, preference, 'Whether international human rights consensus can override state sovereignty in capital punishment policy.').

omega_variable(
    retribution_vs_dignity_incommensurability,
    'Are retributive justice (proportional desert) and categorical dignity protection logically incompatible, or can they coexist in a mixed framework?',
    'Legal-philosophical: examination of whether any coherent legal theory can hold both that murderers deserve death (retribution) and that no human can be killed by the state (dignity). Abolitionist scholars argue they are incompatible; retributivists argue they can be reconciled through narrower desert claims.',
    'If incommensurable, the abolition reading forecloses the retributive reading within any single framework — they are structurally exclusive. If compatible, the readings coexist as rival interpretations and the abolition reading influences but does not eliminate retributive alternatives. This affects whether the reading_relations should be ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retribution_vs_dignity_incommensurability, conceptual, 'Whether retributive justice and categorical dignity are logically incompatible.').

omega_variable(
    kernelhood_of_capital_punishment,
    'Is capital punishment itself a kernel (a persisting institutional commitment with multiple readings), or is abolition a simple policy change that removes the practice entirely?',
    'Structural analysis: if capital punishment continues to exist in any jurisdiction and remains institutionally defended under alternative readings, it is a persistent kernel. If it is abolished everywhere, the kernel dissolves and what remains is only historical reading of a defunct practice.',
    'If abolition is globally achieved, the kernel loses its structure (no contending readings, no live dispute) and this constraint becomes historically resolved rather than live. If retentionist jurisdictions persist, the kernel remains active and the three readings remain structurally distinct constraints. This affects the temporal trajectory — terminal state of the constraint depends on whether capital punishment persists globally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernelhood_of_capital_punishment, empirical, 'Whether capital punishment persists as an active kernel with contending readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__abolition_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__abolition_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t25, state_killing_legitimacy__abolition_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__abolition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(stat_tr_t40, observed).
narrative_ontology:measurement(stat_tr_t55, state_killing_legitimacy__abolition_reading, theater_ratio, 55, 0.41).
narrative_ontology:measurement_basis(stat_tr_t55, observed).
narrative_ontology:measurement(stat_tr_t75, state_killing_legitimacy__abolition_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(stat_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__abolition_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__abolition_reading, base_extractiveness, 10, 0.89).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t25, state_killing_legitimacy__abolition_reading, base_extractiveness, 25, 0.91).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__abolition_reading, base_extractiveness, 40, 0.92).
narrative_ontology:measurement_basis(stat_be_t40, observed).
narrative_ontology:measurement(stat_be_t55, state_killing_legitimacy__abolition_reading, base_extractiveness, 55, 0.92).
narrative_ontology:measurement_basis(stat_be_t55, observed).
narrative_ontology:measurement(stat_be_t75, state_killing_legitimacy__abolition_reading, base_extractiveness, 75, 0.92).
narrative_ontology:measurement_basis(stat_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__abolition_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__abolition_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t25, state_killing_legitimacy__abolition_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__abolition_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement_basis(stat_su_t40, observed).
narrative_ontology:measurement(stat_su_t55, state_killing_legitimacy__abolition_reading, suppression_requirement, 55, 0.78).
narrative_ontology:measurement_basis(stat_su_t55, observed).
narrative_ontology:measurement(stat_su_t75, state_killing_legitimacy__abolition_reading, suppression_requirement, 75, 0.78).
narrative_ontology:measurement_basis(stat_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_legitimacy__abolition_reading, 0.12).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the state_killing_legitimacy kernel. The kernel structures a live institutional dispute: does the state legitimately hold power to execute? The abolition reading answers NO (categorical dignity violation). The retributive reading answers YES under proportional desert (murderers forfeit life-right). The deterrence reading answers YES under rational crime-prevention (execution as crime signal). These are three structurally distinct constraints with different ε values, beneficiaries, and axioms. They are linked via affects_constraints because the same practice (capital punishment) is the object, but each reading generates its own constraint story with its own classification. The ε values differ substantially: abolition reads very high extractiveness (dignity violation), while retribution and deterrence authorize the practice under their own frameworks (lower extractiveness). The decomposition follows the ε-invariance principle: when the same natural-language practice can be evaluated through different observable axioms (dignity, desert, deterrence) yielding different ε values, decompose into separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_legitimacy__abolition_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
