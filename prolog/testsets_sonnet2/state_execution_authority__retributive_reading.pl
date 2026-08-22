% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority — Retributive Justice Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the RETRIBUTIVE reading of the contested
 *   state-execution-authority kernel: execution is justified because it
 *   restores proportionate moral balance for the gravest crimes, independent
 *   of whether it deters future crime. This is one of three sibling
 *   constraints sharing the kernel (deterrence_reading, abolition_reading are
 *   separate files with their own ε, beneficiary/victim structure, and
 *   classification, per the ε-invariance principle). Under this reading, the
 *   executed offender's death is not a regrettable cost but the mechanism's
 *   intended output — proportionality logically requires it, so wrongful
 *   execution is authored here as tragic procedural error that does not
 *   invalidate the framework's legitimacy claim, exactly as the expected
 *   structural delta specifies.
 *
 * KEY AGENTS:
 *   - victims_families_seeking_closure: primary beneficiary (moderate/constrained) — receives the moral-restoration the framework promises
 *   - prosecutors_and_death_penalty_seeking_states: agenda_setter (institutional/arbitrage) — administers and defends the apparatus
 *   - condemned_offenders: primary payer (powerless/trapped) — bears the intended cost of the framework
 *   - wrongfully_convicted_death_row_inmates: payer (powerless/trapped) — bears an irreversible cost the framework treats as external error, not internal defect
 *   - indigent_capital_defendants: payer (powerless/trapped) — systematic representation disparity invisible to crime-based proportionality measure
 *   - retributivist_moral_order_advocates: beneficiary (organized/mobile) — gains vindication of moral theory without bearing case-level costs
 *   - abolitionist_and_defense_advocates: excluded (organized/mobile) — categorical objection heard but treated as external value conflict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.71).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.62).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority — Retributive Justice Reading").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'c8f24aec-4181-425e-a662-60a67a879c0a').
narrative_ontology:cs_kernel_codification('c8f24aec-4181-425e-a662-60a67a879c0a', formalized).
narrative_ontology:cs_authority_grounding('c8f24aec-4181-425e-a662-60a67a879c0a', lineage).
narrative_ontology:cs_interpretation_layer_present('c8f24aec-4181-425e-a662-60a67a879c0a').
narrative_ontology:cs_reading_relation('c8f24aec-4181-425e-a662-60a67a879c0a', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_reading_relation('c8f24aec-4181-425e-a662-60a67a879c0a', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('c8f24aec-4181-425e-a662-60a67a879c0a', foundational, proportionate_desert_requires_commensurate_punishment).
narrative_ontology:cs_axiom_status(proportionate_desert_requires_commensurate_punishment, holdable).
narrative_ontology:cs_axiom_grounding('c8f24aec-4181-425e-a662-60a67a879c0a', proportionate_desert_requires_commensurate_punishment, deontological).
narrative_ontology:cs_axiom('c8f24aec-4181-425e-a662-60a67a879c0a', foundational, execution_is_non_substitutable_for_gravest_crimes).
narrative_ontology:cs_axiom_status(execution_is_non_substitutable_for_gravest_crimes, holdable).
narrative_ontology:cs_axiom_grounding('c8f24aec-4181-425e-a662-60a67a879c0a', execution_is_non_substitutable_for_gravest_crimes, deontological).
narrative_ontology:cs_axiom('c8f24aec-4181-425e-a662-60a67a879c0a', secondary, wrongful_execution_is_external_procedural_error).
narrative_ontology:cs_axiom_status(wrongful_execution_is_external_procedural_error, holdable).
narrative_ontology:cs_axiom_grounding('c8f24aec-4181-425e-a662-60a67a879c0a', wrongful_execution_is_external_procedural_error, conventional).
narrative_ontology:cs_reference_frame('c8f24aec-4181-425e-a662-60a67a879c0a', lex_talionis_proportional_desert).
narrative_ontology:cs_drift_state('c8f24aec-4181-425e-a662-60a67a879c0a', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c8f24aec-4181-425e-a662-60a67a879c0a', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families_seeking_closure).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributivist_moral_order_advocates).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, prosecutors_and_death_penalty_seeking_states).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, condemned_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, indigent_capital_defendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have lost a family member to a heinous crime and hold that only proportionate punishment — the offender's death — restores the moral order the crime disrupted. Participate in sentencing hearings, victim-impact statements, and often witness the execution. Cannot substitute imprisonment for this restoration without, on this reading, leaving the moral debt unpaid.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families_seeking_closure, beneficiary,
    moderate, biographical, constrained, regional).

% Charge capital cases, seek death sentences, and administer the execution apparatus. Frame the practice as restoring proportionality between crime and punishment on behalf of the moral community, and control charging discretion, appeals posture, and clemency recommendations.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, prosecutors_and_death_penalty_seeking_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Have been convicted of a capital crime and sentenced to death. On this reading their execution is the legitimate cost the moral-balance framework requires — not an unfortunate side effect but the mechanism's intended output. Exit is categorically foreclosed once sentence is carried out; appeals are the only avenue prior to execution.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, condemned_offenders, payer,
    powerless, immediate, trapped, local).

% Are factually innocent but sentenced under the same apparatus. On this reading a wrongful execution is a tragic procedural error, not evidence against the retributive framework itself — the moral-balance logic survives the error because the error is attributed to fact-finding, not to the legitimacy of proportionate punishment as such. They bear the full, irreversible cost of that attribution.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, local).

% Face capital charges without resources for adequate defense counsel, expert witnesses, or mitigation investigation comparable to wealthier defendants. The proportionality the framework claims to deliver is measured against the crime, not against the quality of process that determined guilt and sentence, so systematic defense disparity does not register as a framework-level cost on this reading.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, indigent_capital_defendants, payer,
    powerless, biographical, trapped, national).

% Advocacy organizations, victims'-rights coalitions, and retributivist scholars whose worldview holds that justice requires proportionate suffering for the gravest crimes. Gain vindication of a moral theory and political standing each time the state carries out an execution consistent with that theory; do not bear the direct costs of any individual case.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retributivist_moral_order_advocates, beneficiary,
    organized, civilizational, mobile, national).

% Argue that no crime justifies state killing and that the retributive framework cannot be salvaged by procedural fixes. Their arguments are heard in courts and legislatures but the retributive reading treats their categorical objection as a different value system to be outvoted, not as a defect internal to the framework.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_and_defense_advocates, excluded,
    organized, generational, mobile, national).

% Review capital sentences for procedural error and constitutional compliance, and can commute sentences or vacate convictions. Operate inside the retributive framework's own terms — reviewing whether proportionality was correctly applied, not whether proportionality-by-execution is the right measure.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, clemency_and_appellate_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, state-administered mechanism for society to express that the gravest crimes receive a punishment proportionate to their gravity, channeling the demand for retribution through legal process rather than private vengeance.
% TRANSFER_FUNCTION: Moves the offender's life, as the proportionate cost of the crime, from the offender to the satisfaction of the moral order the crime is held to have disrupted — with the victim's family and the retributivist public as the recipients of that restored balance.
% ABSENT_VOICES: Wrongfully convicted individuals cannot testify to the framework's fallibility after execution; abolitionist advocates are present in the debate but their categorical objection is treated as an outside value system rather than an internal defect. Indigent defendants' systematically worse representation is rarely centered in retributive-framework proportionality analysis.
% DISAPPEARANCE_RATIONALE: If the execution authority disappeared overnight, capital sentences would convert to life imprisonment, execution chambers and associated review infrastructure would be decommissioned, and the retributivist claim that only death restores moral balance would lose its only implementing mechanism — victims' families seeking that specific form of closure would have no state avenue to it.
% FOUNDING_PROBLEM: Communities historically feared that without a proportionate, state-sanctioned response to the gravest crimes, private vengeance, blood feuds, or a sense of unpunished moral debt would fracture social order and delegitimize the state's monopoly on punishment.
% FOUNDING_PROBLEM_CORROBORATION: Retributivist scholars and victims'-rights organizations (benefiting parties) attest the problem remains live — that only death restores proportionate balance for the worst crimes. Independent criminological review and international human-rights bodies outside those benefiting parties attest that life imprisonment without parole satisfies the same social-order and non-vengeance functions without the irreversibility risk, suggesting the founding problem's function is substantially separable from the execution mechanism specifically.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71) because the retributive reading's moral-balance claim is, by its own logic, non-substitutable — imprisonment cannot satisfy it, so the framework requires the offender's life as payment, which is the maximal possible extraction from a single agent. Suppression is authored substantial but lower than extraction (0.62) because the mechanism operates through due process rather than raw coercion, though appeals exhaustion and execution irreversibility function as a hard suppression floor once sentence is final. Theater is authored low-moderate (0.28) — clemency review and appellate process are largely functional, not performative, though their functionality operates entirely within the retributive premise rather than questioning it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, the constraint is coordination — a shared, legitimate, process-bound answer to moral injury. From the condemned-offender and wrongfully-convicted seats, the identical structure is irreversible extraction with no exit once final. The engine should compute these divergently from the same structural data; the retributive claim does not average across the divergence, it authors one side of it as the reading's own referent.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims' families and retributivist advocates are declared beneficiaries because the moral-restoration the framework delivers is the entire justification for the reading; they collect the state's action without bearing its cost. Condemned offenders are the structural target — their death is the transfer, not a byproduct. Wrongfully convicted inmates and indigent defendants are also declared victims because the framework's own logic (proportionality is measured against the crime as adjudicated, not against the fairness of adjudication) systematically fails to register their costs as framework-relevant, exactly as the expected structural delta specifies for wrongful execution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing private vengeance, maintaining state monopoly on legitimate punishment) is contested as live: retributivist advocates hold it fully live, while independent criminological and human-rights sources hold that non-lethal proportionate punishment discharges the same social function, meaning the execution-specific mechanism may have outlived necessity even if the state's general punishment authority has not. This is exactly the seat divergence the framework is built to surface: the agenda-setter and beneficiary seats see current, active moral necessity; outside corroborating sources see mandatrophy risk in the execution-specific instrument.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wrongful_execution_framework_validity,
    'Does an established wrongful execution (proven innocence post-execution) constitute evidence against the retributive framework''s legitimacy, or is it fully absorbed as procedural error external to the framework''s moral logic?',
    'Track whether documented wrongful-execution cases produce framework-level reform (moratoria, evidentiary-standard overhauls) versus case-level remedies only (compensation, individual apology) with no change to the retributive justification itself.',
    'If wrongful executions consistently produce only case-level remedies with no framework reconsideration, this corroborates the retributive reading''s own claim that error is external to the framework — but also indicates the framework has no internal mechanism for self-correction, which is itself diagnostic of extraction rather than pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_framework_validity, conceptual, 'Whether wrongful execution is framework-external error or framework-internal defect.').

omega_variable(
    moral_balance_non_substitutability_claim,
    'Is the retributive reading''s core claim — that only execution, not lifelong imprisonment, restores proportionate moral balance for the gravest crimes — an empirically or philosophically testable claim, or is it an axiomatic value commitment immune to counter-evidence?',
    'Comparative analysis of jurisdictions that abolished capital punishment for equivalent crimes: does victims''-family-reported sense of justice/closure differ systematically between execution and life-imprisonment outcomes, controlling for case severity?',
    'If closure/justice outcomes are empirically similar across execution and non-execution jurisdictions, this weakens the non-substitutability claim central to this reading and strengthens the case that the mechanism''s extraction (the offender''s life) exceeds what the coordination function (social order, victim closure) actually requires.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_balance_non_substitutability_claim, empirical, 'Whether moral-balance restoration empirically requires execution specifically or is substitutable.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the retributive framing the dominant lived justification in jurisdictions that retain capital punishment, or is it primarily an ex-post philosophical rationalization layered onto a deterrence-and-incapacitation practice that predates the retributive theory?',
    'Historical and legislative-record analysis of stated justifications at the time capital statutes were enacted versus contemporary appellate and public discourse justifications.',
    'If retributive framing is a later rationalization rather than the founding justification, the founding_problem_status assessment here would shift, and the constraint family''s upstream/downstream influence structure (which reading most shapes public legitimacy discourse) would need re-evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether retributive justification is primary or a later overlay on the practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__retributive_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__retributive_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__retributive_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__retributive_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__retributive_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__retributive_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__retributive_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__retributive_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__retributive_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__retributive_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__retributive_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__retributive_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% Part of the state_execution_authority kernel family (3 readings). This retributive_reading and its siblings (deterrence_reading: empirically-falsifiable crime-prevention claim; abolition_reading: categorical impermissibility claim) share the same underlying institutional kernel — the state's legal authority to execute — but instantiate structurally distinct constraints with different ε, different beneficiary/victim sets, and different classifications. The retributive reading is authored here as tangled_rope (genuine social-order coordination function coexisting with high, non-substitutable extraction from the condemned); the abolition reading would be expected to classify the same standing arrangement as snare (no coordination function it recognizes as legitimate); the deterrence reading's classification depends on contested empirical deterrence-effect data. Do not average ε across these three files — each is a complete, independent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
