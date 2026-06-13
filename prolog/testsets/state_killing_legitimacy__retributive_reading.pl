% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: State Killing Legitimacy (Retributive Reading)
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel: the
 *   legitimacy of state killing. The retributive reading grounds execution in
 *   proportional desert: a murderer forfeits their right to life by violating
 *   the victim's right to life. The constraint coordinates adherence to the
 *   proportionality principle and transfers the condemned murderer's life to
 *   the state/moral order as symbolic repayment. The reading's core claim is
 *   that dignity is forfeitable through grave wrongdoing, which directly
 *   contradicts the abolition reading (dignity is inalienable). The
 *   deterrence reading accepts state killing but grounds it in prevention,
 *   not desert—a different epistemic frame that can coexist institutionally
 *   but rests on incompatible first principles. The authored metrics describe
 *   a substantially extractive, actively enforced arrangement: extractiveness
 *   increased from 1970 to 2000 as death-penalty doctrine hardened, then
 *   stabilized as executions became rarer but the principle remained
 *   defended. Suppression rose sharply (from 0.55 to 0.72) as abolition
 *   movements gained strength, requiring the retributive framing to be more
 *   actively defended. Theater rose steadily (0.28 to 0.42+), indicating an
 *   increasing share of retributive activity devoted to legitimacy
 *   construction—victim statements, proportionality arguments in trial,
 *   ceremony—relative to the act of execution itself. The claim/metric gap is
 *   intentional: the constraint is CLAIMED as tangled rope (genuine
 *   coordination of the proportionality principle + asymmetric enforcement
 *   against the condemned) but the metrics reveal substantial extraction and
 *   suppression, which the engine will evaluate per-seat.
 *
 * KEY AGENTS:
 *   - state_justice_apparatus: Administers retributive logic, determines guilt and proportionality, executes sentences
 *   - victim_families_retribution: Receive symbolic retribution and closure through the constraint's operation
 *   - condemned_murderers: Forfeit their life-right under retributive principle; positioned as moral debtors, not victims
 *   - moral_order_vindication: Non-agent beneficiary—the proportional desert principle itself
 *   - abolition_advocates: Excluded from legitimacy frame; their dignity-as-inalienable contradicts the retributive reading
 *   - deterrence_proponents: Excluded from retributive justification; offer alternative framing grounded in prevention
 *   - legal_scholars_retributive: Observe and articulate the coherence and tradition of the retributive framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.68).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "State Killing Legitimacy (Retributive Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '3d868c2a-b995-4f89-97c0-e27f71cb41bc').
narrative_ontology:cs_kernel_codification('3d868c2a-b995-4f89-97c0-e27f71cb41bc', fixed_text).
narrative_ontology:cs_authority_grounding('3d868c2a-b995-4f89-97c0-e27f71cb41bc', lineage).
narrative_ontology:cs_interpretation_layer_present('3d868c2a-b995-4f89-97c0-e27f71cb41bc').
narrative_ontology:cs_reading_relation('3d868c2a-b995-4f89-97c0-e27f71cb41bc', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_reading_relation('3d868c2a-b995-4f89-97c0-e27f71cb41bc', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('3d868c2a-b995-4f89-97c0-e27f71cb41bc', foundational, life_forfeiture_through_grave_offense).
narrative_ontology:cs_axiom_status(life_forfeiture_through_grave_offense, holdable).
narrative_ontology:cs_axiom_grounding('3d868c2a-b995-4f89-97c0-e27f71cb41bc', life_forfeiture_through_grave_offense, deontological).
narrative_ontology:cs_axiom('3d868c2a-b995-4f89-97c0-e27f71cb41bc', foundational, dignity_forfeitable_by_wrongdoing).
narrative_ontology:cs_axiom_status(dignity_forfeitable_by_wrongdoing, holdable).
narrative_ontology:cs_axiom_grounding('3d868c2a-b995-4f89-97c0-e27f71cb41bc', dignity_forfeitable_by_wrongdoing, deontological).
narrative_ontology:cs_reference_frame('3d868c2a-b995-4f89-97c0-e27f71cb41bc', proportional_desert_classical_tradition).
narrative_ontology:cs_drift_state('3d868c2a-b995-4f89-97c0-e27f71cb41bc', contemporary_abolitionist_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d868c2a-b995-4f89-97c0-e27f71cb41bc', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order_vindication).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victim_families_retribution).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, condemned_murderers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, crime_victims_general).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, crime_victims_general).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, proportional_desert_principle).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, life_forfeiture_through_grave_offense).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, state_authority_to_execute_justly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the retributive logic: determines guilt, assesses whether the offense warrants execution under the proportionality principle, conducts trials, and carries out sentences. Justifies state killing as vindicating the moral order by proportionally repaying the murderer's violation of the victim's right to life. The apparatus itself neither profits nor loses materially from executions.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_justice_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Receive the symbolic and psychological satisfaction of proportional retribution: the murderer forfeits the right to life that they violated in their victim. Their participation (victim impact statements, presence at execution, closure narratives) is structurally enrolled in the legitimacy claim. Alternatives are confined to non-capital sentences.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victim_families_retribution, beneficiary,
    moderate, biographical, constrained, national).

% Forfeit their right to life through their violation of the victim's right to life. Under retributive logic, they are not victims of injustice but recipients of just desert. Their structural position is that of a moral debtor whose only discharge is death. Appeals and clemency are theoretically available but practically rare, and the constraint's legitimacy depends on treating death as the proportional price of murder.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, condemned_murderers, payer,
    powerless, immediate, trapped, national).

% The abstract commitment to proportional desert and the principle that grave violations of rights result in forfeiture of equivalent rights. State execution under retributive logic is the mechanism that vindicates this proposition. The 'beneficiary' is not a concrete actor but the normative framework itself—the reading's foundational axiom.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_order_vindication, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__retributive_reading, moral_order_vindication).

% Would object that state killing violates human dignity categorically, regardless of desert or offense. They are excluded from the legitimacy conversation by retributive framing, which treats dignity as forfeitable through grave wrongdoing. Their alternative framing (dignity as inalienable) contradicts the retributive reading's core premise.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolition_advocates, excluded,
    organized, generational, constrained, national).

% Would ground execution's legitimacy in its signaling function (preventing future murders) rather than the offender's moral desert. They occupy a different epistemic frame: execution is justified by consequentialist outcomes, not retributive principle. The retributive and deterrence readings can coexist institutionally but rest on incompatible foundational claims.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, deterrence_proponents, excluded,
    organized, generational, constrained, national).

% Benefit from the constraint's articulation that their violation is grave enough to forfeit the offender's life. They also bear the diffuse cost of living in a society that executes: the psychological weight of state killing in their name, uncertainty about whether execution brings closure, and exposure to wrongful convictions that may execute the innocent.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, crime_victims_general, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, crime_victims_general, payer).

% Articulates and defends the retributive reading: that proportional desert is a coherent principle, that it justifies state killing of those who forfeit their right to life, and that it remains intellectually defensible. Their authority derives from systematic engagement with the tradition (lineage authority). They produce the framework's internal consistency.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, legal_scholars_retributive, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, state_justice_apparatus).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a coherent moral principle: the right to life can be forfeited through grave violation of another's right to life. The constraint coordinates adherence to this principle across the justice system, ensuring that severity of punishment maps to severity of offense. Without this principle articulated and enforced, the claim goes, the moral order lacks proportionality and becomes arbitrary or merely instrumental.
% TRANSFER_FUNCTION: Transfers the life of the condemned murderer to the state/moral order as symbolic repayment for the violation of the victim's life. The movement is both material (death) and normative (vindication of the proportional desert principle). A secondary transfer: from society to victim families as closure through execution.
% ABSENT_VOICES: Abolitionists who hold that dignity is inalienable are structurally excluded by the retributive framing itself—their core claim (no one's right to life can be forfeited) contradicts the reading's foundational premise. Deterrence proponents are excluded from the legitimacy conversation because they would ground execution in prevention, not desert. Wrongfully convicted individuals (only identifiable post-execution in some cases) are absent during the trial process that determines who enters the victim set.
% DISAPPEARANCE_RATIONALE: If state killing under retributive logic disappeared overnight, the justice system would reorganize around alternative proportional penalties (life imprisonment, partial forfeiture of rights short of execution). The moral claim that forfeiture of life is proportional to murder would lose its institutional instantiation. Victim families would face a revised structure of closure (alternative ceremonies, symbolic remedies). The retributive principle would persist in discourse but would no longer be collectively enforced through state killing.
% FOUNDING_PROBLEM: How can a just legal order respond to grave violations of rights in a way that respects both the offender's residual dignity and the victim's violated right? The retributive reading answers: through proportional desert—the offender forfeits the same right they violated.
% FOUNDING_PROBLEM_CORROBORATION: Retributive theorists (Kant, Moore, contemporary desert-based criminologists) attest that proportional retribution is the founding problem and that execution answers it justly. Abolitionists and empirical criminologists studying deterrence effects attest that the founding problem has been redefined: the real question is not how to give proportional punishment but how to reduce harm and respect dignity. European human-rights authorities attest that execution violates dignity categorically and that the founding problem is better solved through life imprisonment. No agreement exists outside the retributive tradition itself.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint extracts a severe cost (life) from the condemned, and the justification—proportional desert—is a normative claim that lacks independent empirical verification. The retributive reading asserts that life-forfeiture is morally deserved, but this assertion is precisely what the abolished and deterrence readings contest. Suppression (0.72) is elevated because the constraint's persistence depends on actively suppressing the abolition reading's core claim (dignity as inalienable) and the empirical deterrence debate (whether execution actually prevents future murders). Theater (0.42) reflects an increasing share of retributive practice devoted to legitimacy construction—victim-impact narratives, proportionality arguments, ceremonial dimensions—as executions became rarer in the US (declining from ~70/year in 1995 to ~15-20/year in 2020s) while the principle remained defended. The measurement series shows extractiveness rising 1970–2000 (as retributive doctrine became more systematized and applied more harshly), then stabilizing despite execution rarity; suppression rising 1970–2012 as abolition movements gained institutional voice, then plateauing as the constraint hardened around its reduced execution count. This trajectory models a constraint that has become more theatrical and more actively defended while materially exercised less—a piton-trajectory risk signal, though the constraint remains sufficiently extractive (ε=0.68) and suppressive (σ=0.72) to resist full piton classification.
 *
 * PERSPECTIVAL GAP:
 *   The state justice apparatus and retributive legal scholars occupy a seat from which the constraint appears as genuine coordination: the proportional desert principle is articulated, defended, and applied systematically. The condemned murderers and abolition advocates occupy a seat from which the same constraint appears as enforced extraction: a normative claim (desert) weaponized to legitimize state killing. The engine computes directionality separately for each seat: the apparatus has low d (it administers the constraint, derives authority from it, exits analytically); the condemned have high d (powerless, trapped, subjected to the constraint's extraction). The gap is structural, not observational.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus benefits from the constraint (derives authority, vindicates the moral order, administers the principle) and has analytical exit—d near 0.1. Victim families benefit symbolically (closure, retribution) and have constrained exit (alternatives limited to non-capital sentences)—d near 0.3. The condemned forfeit everything and have no exit—d approaches 1.0. The abolition advocates are excluded from the beneficiary/victim structure entirely because the retributive framing treats them as absent from the legitimacy conversation, not as payers. This asymmetry is the structural heart of the reading: who is recognized as a stakeholder whose interests matter? The retributive reading includes the condemned as payers (their debt to the moral order); the abolition reading would include the state as the payer (for violating dignity). No override is needed; the structural derivation follows cleanly from beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The retributive reading avoids Mandatrophy on the temporal axis: the founding problem (proportional response to grave violation) remains live, though contested. The constraint does not show signs of persisting because its primary function has atrophied; rather, it persists through active enforcement of the legitimacy claim. However, the rising theater_ratio (0.28 to 0.42) and declining execution frequency suggest a decoupling between the principle's continued assertion and its material application. A future measurement showing theater_ratio > 0.5 would flag theater-driven persistence (Mandatrophy risk); currently it remains in the mid-range, consistent with a constraint that is actively defended but materially exercised less frequently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_desert_empirical_content,
    'Is proportional desert an empirically verifiable principle, or a non-empirical normative commitment?',
    'Philosophical analysis of whether desert-claims can be falsified by evidence. Cross-cultural comparison of desert intuitions to determine whether proportionality is culturally universal or contingent.',
    'If desert is empirically contingent (varies by culture or historical period), the retributive reading''s claim to universal moral truth weakens, and the constraint risks reclassification as culturally-specific extraction. If desert is non-empirical (a foundational axiom), the constraint is insulated from empirical challenge but occupies a different epistemic status than deterrence claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_desert_empirical_content, conceptual, 'Whether proportional desert is an empirical or foundational normative claim.').

omega_variable(
    dignity_forfeiture_logical_status,
    'Can dignity logically be forfeited through wrongdoing, or is dignity categorically inalienable by definition?',
    'Systematic engagement with philosophical traditions: deontological frameworks (does duty-violation entail right-loss?), virtue ethics (can a person forfeit their status as a person?), and human-rights frameworks (is dignity prior to or derived from conduct?). The resolution depends on which framework is adopted.',
    'If dignity is inalienable (the abolition reading''s axiom), the retributive reading''s foundational claim is foreclosed—no further analysis can override the incompatibility. If dignity can be forfeited (the retributive reading''s axiom), the abolition reading is foreclosed. This is a logical foreclosure, not an empirical resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_forfeiture_logical_status, conceptual, 'Logical status of dignity as forfeitable or inalienable.').

omega_variable(
    deterrence_empirical_evidence,
    'Does execution deter future murders more effectively than alternative severe penalties (life imprisonment)?',
    'Econometric studies controlling for jurisdiction, time period, criminal-history variables, and sentence certainty. Meta-analysis of existing deterrence research. Natural experiments from jurisdictions that abolished execution.',
    'If deterrence evidence is strong, the deterrence reading becomes institutionally viable as an alternative justification for execution. If deterrence evidence is weak, the deterrence reading loses its empirical ground, but the retributive reading persists (desert is not grounded in deterrence). The relationship between readings shifts: deterrence becomes a failed justification rather than a coexisting one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_evidence, empirical, 'Empirical deterrent effect of capital punishment.').

omega_variable(
    wrongful_conviction_risk,
    'What is the actual rate of wrongful convictions in capital cases, and does this rate undermine the retributive reading''s claim to proportional justice?',
    'Exoneration data, DNA evidence review, innocence-project case tracking, and comparative analysis of capital-case error rates vs. non-capital felony convictions.',
    'High wrongful-conviction rates would demonstrate that the retributive principle (proportional desert) cannot be reliably implemented—the constraint''s claim to justice becomes empirically indefensible. This does not refute the retributive axiom (desert remains a valid principle) but refutes its applicability under current institutional conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_risk, empirical, 'Institutional capacity to reliably apply proportional desert in capital cases.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.72) primarily structural (legal barriers to abolition, institutional lock-in) or internalized (belief in the retributive principle, legitimacy acceptance)?',
    'Post-abolition trajectory analysis: if suppression persists among individuals after the legal constraint is removed (e.g., in abolition jurisdictions), suppression is partially internalized; if it diminishes, suppression was primarily structural.',
    'If suppression is internalized, the retributive reading''s legitimacy is more durable and less dependent on enforcement; if structural, removing the institutional constraint would enable rapid legitimacy shift. This affects predictions about whether abolition would require sustained suppression or would consolidate quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the retributive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1970, state_killing_legitimacy__retributive_reading, theater_ratio, 1970, 0.28).
narrative_ontology:measurement_basis(stat_tr_t1970, observed).
narrative_ontology:measurement(stat_tr_t1985, state_killing_legitimacy__retributive_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement_basis(stat_tr_t1985, observed).
narrative_ontology:measurement(stat_tr_t2000, state_killing_legitimacy__retributive_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement_basis(stat_tr_t2000, observed).
narrative_ontology:measurement(stat_tr_t2012, state_killing_legitimacy__retributive_reading, theater_ratio, 2012, 0.43).
narrative_ontology:measurement_basis(stat_tr_t2012, observed).
narrative_ontology:measurement(stat_tr_t2018, state_killing_legitimacy__retributive_reading, theater_ratio, 2018, 0.44).
narrative_ontology:measurement_basis(stat_tr_t2018, observed).
narrative_ontology:measurement(stat_tr_t2024, state_killing_legitimacy__retributive_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(stat_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1970, state_killing_legitimacy__retributive_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement_basis(stat_be_t1970, observed).
narrative_ontology:measurement(stat_be_t1985, state_killing_legitimacy__retributive_reading, base_extractiveness, 1985, 0.64).
narrative_ontology:measurement_basis(stat_be_t1985, observed).
narrative_ontology:measurement(stat_be_t2000, state_killing_legitimacy__retributive_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement_basis(stat_be_t2000, observed).
narrative_ontology:measurement(stat_be_t2012, state_killing_legitimacy__retributive_reading, base_extractiveness, 2012, 0.69).
narrative_ontology:measurement_basis(stat_be_t2012, observed).
narrative_ontology:measurement(stat_be_t2018, state_killing_legitimacy__retributive_reading, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement_basis(stat_be_t2018, observed).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__retributive_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(stat_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1970, state_killing_legitimacy__retributive_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement_basis(stat_su_t1970, observed).
narrative_ontology:measurement(stat_su_t1985, state_killing_legitimacy__retributive_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement_basis(stat_su_t1985, observed).
narrative_ontology:measurement(stat_su_t2000, state_killing_legitimacy__retributive_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement_basis(stat_su_t2000, observed).
narrative_ontology:measurement(stat_su_t2012, state_killing_legitimacy__retributive_reading, suppression_requirement, 2012, 0.73).
narrative_ontology:measurement_basis(stat_su_t2012, observed).
narrative_ontology:measurement(stat_su_t2018, state_killing_legitimacy__retributive_reading, suppression_requirement, 2018, 0.72).
narrative_ontology:measurement_basis(stat_su_t2018, observed).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__retributive_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(stat_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_legitimacy__retributive_reading, 0.12).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the state_killing_legitimacy kernel. The retributive reading (this story) grounds execution in proportional desert—the murderer forfeits their right to life. The deterrence reading (sibling constraint) grounds it in crime prevention—execution as a rational signal. The abolition reading (sibling constraint) rejects execution categorically on dignity grounds. Each reading has a distinct ε value, beneficiary/victim structure, and epistemic grounding. They coexist or foreclose depending on foundational axioms: retributive vs. abolition foreclose (incompatible dignity claims); retributive and deterrence coexist institutionally but rest on different justifications. The three stories are linked by network.affects_constraints to model the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
