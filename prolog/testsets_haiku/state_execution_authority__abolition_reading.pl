% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority: Abolition Reading
 *   domain: criminal_justice/constitutional_law
 *
 * SUMMARY:
 *   The abolition reading claims that state execution is categorically
 *   impermissible regardless of crime severity, procedural safeguards, or the
 *   certainty of guilt. Under this reading, all executed persons — guilty and
 *   innocent alike — are victims of the state's illegitimate exercise of
 *   ultimate power. The reading rejects retributive and deterrence
 *   justifications as inadequate to ground such authority and treats
 *   execution as extractive (the state claims the exclusive right to kill in
 *   the name of justice and collects the coercive benefit). This is one
 *   reading of the contested kernel state_execution_authority; the
 *   retributive and deterrence readings are alternatives, held by different
 *   institutional and intellectual actors. The claim/metric divergence is
 *   deliberate: the abolition reading is CLAIMED as a snare (pure extraction
 *   using justice as cover), and the metrics describe a highly extractive,
 *   actively enforced operation with substantial suppression and moderate
 *   theater. The engine computes whether the claimed snare classification
 *   holds; divergence flags the measurement.
 *
 * KEY AGENTS:
 *   - State execution apparatus: administers capital punishment, maintains infrastructure, enforces the authority to execute
 *   - Executed persons: victims of the constraint regardless of guilt; exit is impossible
 *   - Families of executed persons: bear permanent loss through state action; constrained exit
 *   - Wrongfully convicted persons: paradigm victims; their existence proves system illegitimacy
 *   - Victims of capital crimes: seated as beneficiaries in the retributive/deterrence frame; the abolition reading rejects this
 *   - Abolition movement: pays costs of organizing against entrenched institutional practice
 *   - International human rights regime: benefits from abolition (vindicated norms) without directly profiting
 *   - Executing states: agenda-setters that maintain the apparatus and claim exclusive authority to kill
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.91).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.88).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority: Abolition Reading").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '77960c5a-9119-48af-b695-429a621e1fee').
narrative_ontology:cs_kernel_codification('77960c5a-9119-48af-b695-429a621e1fee', fixed_text).
narrative_ontology:cs_authority_grounding('77960c5a-9119-48af-b695-429a621e1fee', extraction).
narrative_ontology:cs_interpretation_layer_present('77960c5a-9119-48af-b695-429a621e1fee').
narrative_ontology:cs_reading_relation('77960c5a-9119-48af-b695-429a621e1fee', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('77960c5a-9119-48af-b695-429a621e1fee', state_execution_authority__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('77960c5a-9119-48af-b695-429a621e1fee', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('77960c5a-9119-48af-b695-429a621e1fee', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('77960c5a-9119-48af-b695-429a621e1fee', foundational, state_lacks_moral_authority_to_kill).
narrative_ontology:cs_axiom_status(state_lacks_moral_authority_to_kill, holdable).
narrative_ontology:cs_axiom_grounding('77960c5a-9119-48af-b695-429a621e1fee', state_lacks_moral_authority_to_kill, deontological).
narrative_ontology:cs_reference_frame('77960c5a-9119-48af-b695-429a621e1fee', human_dignity_and_state_authority_limits).
narrative_ontology:cs_drift_state('77960c5a-9119-48af-b695-429a621e1fee', contemporary_post_dna_exoneration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77960c5a-9119-48af-b695-429a621e1fee', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, wrongfully_convicted_persons).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.91) because the abolition reading asserts that state execution is categorically impermissible — no degree of severity or procedural refinement can justify it. This means the constraint extracts an irreversible, irreplaceable value (human life) from those it governs, with no legitimate exchange. Suppression is high (0.88) because the constraint's persistence depends on actively preventing alternatives: abolition movements are suppressed through resistance to legislative change, wrongful conviction cases are framed as exceptional rather than systemic, and the retributive/deterrence justifications are maintained despite contestation. Theater is moderate (0.42) because while the procedural safeguards and justice rhetoric are real, they increasingly function as performance as the abolition critique gains ground — the system expends energy on error-correction theater while the underlying extraction persists. The measurements show slight upward drift in extractiveness and suppression over the 50-year interval, consistent with the abolition movement's growing empirical and normative challenge making the system's self-justifications more difficult to maintain (theater compensates). All metrics are authored on a single shared time grid so the engine has consistent data across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The state execution apparatus (agenda-setter seat) experiences the constraint as a necessary expression of state authority, proportionate to crime, and justified by deterrence and retribution. From this seat, the constraint solves a genuine coordination problem (how to punish heinous crime) and should compute as Rope or even a natural principle of proportionality. The executed persons and their families (payer seats) experience the constraint as state killing — the exercise of ultimate coercive power with no legitimate justification. From these seats, it should compute as Snare. The abolition movement (payer seat with mobile exit) experiences it as an entrenched institutional practice maintained by inertia and self-justifying narratives. The wrongfully convicted persons (powerless, trapped payer) are the canonical victims whose existence undermines all justifications. The engine derives each seat's experienced type from the structural data (power, exit, beneficiary/victim declarations); the authored claim reflects the abolition reading's perspective that the constraint is snare-like from every seat that is not the state apparatus itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution apparatus has high directionality toward beneficiary (d near 0.0) because it sets and administers the constraint, extracts legitimacy from it, and has arbitrage-grade exit (it could, if it chose, abolish capital punishment). Executed persons have maximum directionality toward target (d near 1.0) because they are trapped (no exit) and bear the ultimate cost (death) that the constraint imposes. Families of executed persons are targets (d ~0.8–0.9) because they are constrained in exit and bear permanent loss. Wrongfully convicted persons are paradigm targets (d = 1.0) because their trapping is involuntary (wrongful conviction) and their loss is irreversible. Abolition activists are moderate payers (d ~0.6–0.7) because they bear organizational costs but retain mobile exit (they can cease activism). Victims of capital crimes are positioned as beneficiaries (d ~0.2–0.3) in the retributive apparatus's logic, but the abolition reading rejects this positioning: they do not actually benefit from execution, and the role is a mischaracterization imposed by the constraint's self-justifying rhetoric. The international human rights regime has low directionality (d ~0.1–0.2) because it benefits from abolition (vindicated norms) without running the apparatus or bearing direct costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolition reading reveals a severe mandatrophy: the constraint's founding mandate (from the retributive and deterrence perspectives) asserts that execution serves justice and prevents crime. But the empirical case for deterrence has been substantially challenged (no robust evidence that execution deters more than life imprisonment), wrongful convictions demonstrate the retributive claim cannot be reliably executed, and jurisdictions without capital punishment do not show inferior justice outcomes. The mandate is dead in the empirical sense, but the constraint persists through institutional inertia, budget appropriations, and the state's claim to exclusive authority. This is the classic zombie constraint: the founding problem (proportionate punishment for heinous crime) can be and is solved through other means (life without parole, lengthy sentences), yet the execution apparatus persists. The abolition reading's Snare classification captures this: the extraction (execution authority) is maintained despite the solving of the founding problem, using justice language as cover. The mandatrophy is not resolved — it is the core of the abolition claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_of_state_killing,
    'Is the prohibition on state execution a natural law / mathematical truth about permissible state action, or is it a constructed norm that benefits certain advocates and institutions?',
    'Genealogical analysis of the abolition movement''s emergence and institutional backing; examination of whether non-executing jurisdictions arose from principled abolitionist conviction or pragmatic drift; investigation of whether international human rights institutions have material incentives to promote abolition.',
    'If the prohibition is natural/logical (like the illegality of forcing someone to be in two places at once), it would be reclassified as a Mountain. If it is constructed, it remains a Snare with potential beneficiaries (human rights institutions, certain political movements). The reading''s strength depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_of_state_killing, conceptual, 'Whether the categorical prohibition on execution is a discovered natural principle or a constructed normative claim.').

omega_variable(
    procedural_sufficiency_debate,
    'Can any procedural safeguard (evidence standards, appellate review, DNA testing, error-correction mechanisms) make execution categorically permissible, or is execution intrinsically impermissible regardless of procedure?',
    'Philosophical analysis of whether the objection to execution is grounded in epistemic limits (we can never be certain enough) or in principle (the state lacks moral authority to kill). Empirical observation of whether error rates converge to zero with perfect procedure.',
    'If safeguards could suffice in principle, the constraint is not categorical — it becomes context-dependent. If execution is intrinsically impermissible, the procedural approach is a false hope and feeds the theater ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_sufficiency_debate, conceptual, 'Whether procedural improvements can validate execution or whether the prohibition is intrinsic.').

omega_variable(
    wrongful_execution_frequency_and_epistemic_status,
    'How many executions of innocent persons does a system contain? Is the rate empirically measurable, or is it irreducibly uncertain?',
    'DNA exoneration data, statistical estimates of false-conviction rates in capital cases, post-conviction innocence investigation. Compare estimate of innocents-executed against execution volume.',
    'High certainty of wrongful executions (>0.5% of executions estimated innocent) would strengthen the abolition reading as a snare driven by state illegitimacy. Low or zero wrongful executions would undermine the ''system illegitimacy'' claim and move the debate to pure retributive/deterrence grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_frequency_and_epistemic_status, empirical, 'The empirical rate of executing innocent persons and its epistemic status.').

omega_variable(
    beneficiary_identification_ambiguity,
    'Who benefits from the execution system''s persistence? Is there an identifiable beneficiary, or is the system maintained by institutional inertia despite diffuse costs?',
    'Analysis of state budget allocations to capital litigation and execution infrastructure; examination of constituencies who actively defend capital punishment; investigation of whether abolition would reduce state power or merely transfer resources.',
    'If a clear beneficiary exists (state power apparatus, certain politicians, retributive-theory advocates), the snare classification is solid. If execution persists mainly through institutional inertia with no concentrated benefit, it might be a Piton rather than a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Identification of concentrated beneficiaries versus diffuse inertial persistence.').

omega_variable(
    kernel_reading_under_determination,
    'This constraint instantiates the abolition_reading of the state_execution_authority kernel. The other readings (retributive, deterrence) claim the same kernel admits different legitimate ε-values and beneficiary structures. How is the choice of reading itself justified?',
    'Analysis of the epistemic authority grounding each reading: Do retributive and deterrence readings rest on empirical claims that could be falsified? Do they rest on normative premises that the abolition reading rejects as illegitimate? What would convince an adopter of one reading to switch to another?',
    'If the reading choice is empirical (deterrence works/doesn''t work), evidence could shift the boundary. If it is deontological (execution is intrinsically impermissible), evidence cannot settle it, and the readings remain in genuine contest. The classification assumes the abolition reading is the correct one; if the retributive reading is correct instead, the constraint type would be Rope or even Mountain (natural proportionality).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'The kernel-reading selection problem: on what basis is the abolition reading chosen over sibling readings?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(stat_tr_t6, state_execution_authority__abolition_reading, theater_ratio, 6, 0.39).
narrative_ontology:measurement(stat_tr_t12, state_execution_authority__abolition_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(stat_tr_t25, state_execution_authority__abolition_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(stat_tr_t37, state_execution_authority__abolition_reading, theater_ratio, 37, 0.42).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.87).
narrative_ontology:measurement(stat_be_t6, state_execution_authority__abolition_reading, base_extractiveness, 6, 0.88).
narrative_ontology:measurement(stat_be_t12, state_execution_authority__abolition_reading, base_extractiveness, 12, 0.89).
narrative_ontology:measurement(stat_be_t25, state_execution_authority__abolition_reading, base_extractiveness, 25, 0.9).
narrative_ontology:measurement(stat_be_t37, state_execution_authority__abolition_reading, base_extractiveness, 37, 0.91).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(stat_su_t6, state_execution_authority__abolition_reading, suppression_requirement, 6, 0.83).
narrative_ontology:measurement(stat_su_t12, state_execution_authority__abolition_reading, suppression_requirement, 12, 0.84).
narrative_ontology:measurement(stat_su_t25, state_execution_authority__abolition_reading, suppression_requirement, 25, 0.86).
narrative_ontology:measurement(stat_su_t37, state_execution_authority__abolition_reading, suppression_requirement, 37, 0.88).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__abolition_reading, 0.05).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, life_imprisonment_as_alternative_sanction).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, wrongful_conviction_and_dna_exoneration).

% DUAL FORMULATION NOTE:
% The state_execution_authority kernel is contested across three readings (abolition, retributive, deterrence), each representing a different legitimate interpretation of Constitutional authority and a different constraint structure. This file (abolition_reading) generates the constraint from the perspective that execution is categorically impermissible and purely extractive. The retributive and deterrence readings (separate files) instantiate the kernel from perspectives that execution can be legitimate coordination. All three readings are linked via network.affects_constraints; they share a kernel but have divergent ε, beneficiary/victim structure, and type. The choice of reading is not determined by evidence alone — it depends on foundational normative premises (deontological vs. empirical). The false-summit candidate here is the retributive and deterrence readings, which may appear as natural-law constraints (proportionate punishment, crime prevention) but actually rest on contestable normative premises that the abolition reading rejects as illegitimate. The abolition reading assumes execution is never legitimate; that assumption is not falsifiable by evidence about crime rates or proportionality, which is why the readings are in genuine contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__abolition_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
