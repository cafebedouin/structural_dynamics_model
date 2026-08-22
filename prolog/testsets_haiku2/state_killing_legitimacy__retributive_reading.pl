% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: State Execution Legitimated by Desert (Retributive Reading)
 *   domain: political_philosophy/criminal_justice
 *
 * SUMMARY:
 *   The retributive reading of state killing legitimacy holds that a murderer
 *   who unjustly takes a life forfeits their own right to life. Proportional
 *   justice — lex talionis — requires the state to execute. Under this
 *   reading, the executed offender is positioned as a violator of the moral
 *   order whose death restores balance and vindicates the victim's right.
 *   This is one reading of a contested kernel: 'state_killing_legitimacy.'
 *   Sibling readings (deterrence and abolition) instantiate different
 *   constraints with different extraction profiles and beneficiary
 *   structures. The retributive reading claims high ε from the desert-based
 *   legitimacy framework: the constraint's persistence depends on the belief
 *   that proportional forfeiture is morally necessary, not optional. This
 *   claim is substantive and disputed; the omegas document the irreducible
 *   uncertainties in the desert/dignity framework split and the institutional
 *   closure that makes alternative readings inaccessible from within the
 *   retributive frame.
 *
 * KEY AGENTS:
 *   - state_authority: Institutional agenda-setter. Administers execution as a proportional response to murder.
 *   - executed_offenders: Powerless payer. Positioned as moral violators whose forfeiture is justified by desert.
 *   - murder_victims_and_families: Moderate beneficiary/payer. Frame execution as honoring the victim; also bear the cost of seeing another person die in the victim's name.
 *   - alternative_justice_advocates: Organized excluded voice. Would argue for non-capital alternatives; kept out by the framework's axiomatic closure.
 *   - moral_order (doctrine, not agent): Analytical beneficiary. Framed as the constraint's true beneficiary — the order that execution restores.
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
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "State Execution Legitimated by Desert (Retributive Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "political_philosophy/criminal_justice").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '0d55761e-178b-494c-9e1d-e10b40387ddc').
narrative_ontology:cs_kernel_codification('0d55761e-178b-494c-9e1d-e10b40387ddc', fixed_text).
narrative_ontology:cs_authority_grounding('0d55761e-178b-494c-9e1d-e10b40387ddc', extraction).
narrative_ontology:cs_interpretation_layer_present('0d55761e-178b-494c-9e1d-e10b40387ddc').
narrative_ontology:cs_reading_relation('0d55761e-178b-494c-9e1d-e10b40387ddc', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d55761e-178b-494c-9e1d-e10b40387ddc', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('0d55761e-178b-494c-9e1d-e10b40387ddc', foundational, proportional_desert_foundational).
narrative_ontology:cs_axiom_status(proportional_desert_foundational, holdable).
narrative_ontology:cs_axiom_grounding('0d55761e-178b-494c-9e1d-e10b40387ddc', proportional_desert_foundational, deontological).
narrative_ontology:cs_axiom('0d55761e-178b-494c-9e1d-e10b40387ddc', foundational, life_right_forfeitable_through_violation).
narrative_ontology:cs_axiom_status(life_right_forfeitable_through_violation, holdable).
narrative_ontology:cs_axiom_grounding('0d55761e-178b-494c-9e1d-e10b40387ddc', life_right_forfeitable_through_violation, deontological).
narrative_ontology:cs_reference_frame('0d55761e-178b-494c-9e1d-e10b40387ddc', proportional_desert_moral_necessity).
narrative_ontology:cs_drift_state('0d55761e-178b-494c-9e1d-e10b40387ddc', contemporary_human_rights_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0d55761e-178b-494c-9e1d-e10b40387ddc', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, state_authority).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order_vindication).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, executed_offenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, murder_victims_and_families).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, murder_victims_and_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the execution law. Claims authority derives from the moral necessity to restore proportional balance through punishment. Administers clemency, reviews sentences, executes warrants. Collects the legitimacy benefit of performing a 'just desert' institution. Could reinterpret or commute, but chooses not to within the retributive framework.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Convicted of murder, sentenced to death under proportional desert doctrine. Have no exit from the jurisdiction's reach; appeals exhaust within the system. Positioned as moral violators whose forfeiture of life-right is justified by their violation of another's right. Face immediate, certain extraction of life.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, executed_offenders, payer,
    powerless, immediate, trapped, national).

% Positioned as beneficiaries of proportional restoration: execution honors the victim's violated right. Also bear the cost of seeing another human die in the victim's name. Can participate in clemency proceedings but do not set the retributive framework. Experience closure and harm simultaneously.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, murder_victims_and_families, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, murder_victims_and_families, payer).

% Abolitionists, restorative-justice advocates, and comparative-penology scholars would argue against capital punishment on grounds of dignity, efficacy, or alternative means of addressing victim needs. Excluded from renegotiating the core legitimacy axiom (proportional desert) within a retributive framework. Can testify but cannot alter the framework's foundational premises.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, alternative_justice_advocates, excluded,
    organized, generational, constrained, national).

% Courts and clemency boards review death sentences and decide mercy cases. Sit at the enforcement interface. Can reduce the constraint's scope through commutation but operate within a framework that treats execution as a legitimate penalty. Observe the relationships but do not set the retributive axioms.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, judicial_and_clemency_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, state_authority).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The retributive reading coordinates on a single moral principle: murder violates the victim's right to life; proportional justice demands the murderer forfeit that same right. This principle solves the problem of what legitimate punishment looks like when a fundamental right has been violated — it offers a decision rule (proportionality) for assigning punishment severity.
% TRANSFER_FUNCTION: Moves the executed offender's life to the state, framed as a transfer to the victim's right-violating principle: the offender's death performs the restoration of what they took. The state administers this transfer; the victim's violated right and the moral order are positioned as the beneficiaries of the rebalancing.
% ABSENT_VOICES: Abolished in many jurisdictions; excluded from the retributive framework itself. Abolitionists, restorative-justice advocates, and comparative-penology scholars would argue that proportional desert does not require death, that state killing violates a different moral order (dignity-based), and that alternatives (life imprisonment, restorative practices) satisfy justice without execution. They are kept out by the framework's axiomatic closure: retributive desert is treated as foundational, not as one reading among others.
% DISAPPEARANCE_RATIONALE: If the retributive execution constraint vanished, judicial systems in retaining jurisdictions would reorganize around a different reading of state killing legitimacy (deterrence, incapacitation) or would transition to non-capital penalties. The moral logic that uniquely justifies execution through desert would no longer sustain the constraint; institutions would restructure their punishment frameworks.
% FOUNDING_PROBLEM: Murder takes a life unjustly; justice requires restoration of moral balance. The founding problem is philosophical and historical: how do you restore a violated right when the victim is dead and cannot be compensated? The retributive answer is through proportional forfeiture by the offender — the offender's death restores cosmic or social balance.
% FOUNDING_PROBLEM_CORROBORATION: Retributive philosophers and jurisdictions that retain capital punishment attest the founding problem is live: proportional desert remains the unique way to address the violation of the most fundamental right. Abolitionists, international human rights bodies, and jurisdictions that have abolished execution attest from outside the retributive benefiting parties that the problem IS solved without execution: life imprisonment incapacitates indefinitely; restorative processes address victim and community healing; dignity protections foreclose forfeiture. The founding problem's status is CONTESTED because the readings cannot agree on whether proportional desert is necessary for justice or incompatible with it.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68) reflects the constraint's high reliance on the desert axiom. The constraint persists not because coordination is efficient (alternatives exist — life imprisonment) but because the desert framework treats execution as the unique morally legitimate response. Suppression (0.72) is high because the constraint requires excluding alternative readings from the core legitimacy debate: you cannot say 'I accept your coordination framework but disagree on whether it justifies execution' — that disagreement collapses the framework itself. Theater (0.42) is moderate: the constraint has real performative elements (death-penalty trials are public, ritualized; clemency decisions are theatrical) but also maintains a functional logic (offenders are incapacitated, the victim's family can point to a final act of state recognition). The measurement series shows base_extractiveness and suppression_requirement rising over the interval: as alternative readings gain institutional legitimacy in abolitionist jurisdictions, the retributive reading must work harder to sustain itself (more suppression needed); as the constraint's moral warrant is increasingly contested, the theater ratio rises (more effort spent on ritual re-affirmation). Accessibility_collapse and resistance are both high (0.78 and 0.81 respectively): from the retributive perspective, alternatives collapse once you accept desert as foundational, but resistance is also high because the alternative readings maintain live institutional presences (many jurisdictions have abolished).
 *
 * PERSPECTIVAL GAP:
 *   The state_authority and executed_offender seats compute fundamentally different types from identical structural facts. From the state's seat, the constraint is a tangled rope: genuine coordination function (desert-based legitimacy assigns punishment severity through a principle) + asymmetric extraction (the state administers the principle and collects the legitimacy benefit). From the offender's seat, the constraint appears as a snare: the coordination function is opaque (the offender is positioned as already having forfeited their right through the act of murder); the extraction is plain (the state takes their life without their consent). The divergence is driven by the retributive axiom itself: desert legitimacy assigns to the state the role of adjudicator of whether a right has been forfeited. This structural asymmetry is not a flaw in the analysis — it is the point. The engine captures it through per-seat directionality and per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority is the structural agenda-setter with high power and exit via arbitrage (could reinterpret the framework, but does not) — directionality near 1.0 (target seat collecting the legitimacy benefit of performing desert). Executed offenders are powerless and trapped (no jurisdiction exit, no exemption from desert logic) — directionality near 1.0 (full target). Murder victims and families occupy an asymmetric position: they benefit from the symbolic restoration (role = beneficiary) but also pay through psychological re-traumatization and the knowledge that the state kills in their name — directionality near 0.5 (symmetric cost/benefit, but asymmetric roles: they do not set the agenda). Alternative justice advocates are excluded from the framework — directionality undefined within the retributive reading (they are outside the scope of the constraint as a retributive claim). The moral order is framed as the primary beneficiary (directionality near 0.0), but it is a doctrine, not an agent — it collects the symbolic capital of execution without bearing any cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has a founded problem that remains live under the retributive reading but is contested across readings. Murder violates the moral order; proportional response restores it. The retributive reading treats this as an unsolved problem (execution is the solution). Abolition and deterrence readings treat the problem as differently solved or as solved by non-capital means. The constraint does not show classic mandatrophy (founding problem dead, constraint persists through inertia) — instead, it shows reading-mandatrophy: the founding problem persists within the retributive frame but is treated as solved (or unsolvable) by alternative readings. This is not a defect in classification; it is exactly the contention the kernel structure captures. The theater_ratio rise over the interval suggests the constraint is increasingly relying on ritual rather than functional coordination (as empirical doubt about desert's efficacy grows), which may presage actual mandatrophy if abolitionist readings continue gaining institutional ground.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    desert_vs_dignity_framework_split,
    'Is proportional desert a foundational moral principle that can override categorical human rights protections, or does human dignity categorically foreclose forfeiture regardless of desert?',
    'Philosophical debate and jurisprudential precedent: the question hinges on whether dignity is inalienable (can never be forfeited) or conditional (can be forfeited through grave transgression). Constitutional courts in abolitionist jurisdictions have ruled dignity inalienable; retentionist courts have ruled desert-based forfeiture compatible with dignity. The resolution is institutional and reflects which framework the jurisdiction adopts as foundational.',
    'If dignity is inalienable, the retributive reading forecloses itself — execution would violate the dignity principle that grounds the right to life. If dignity is conditional on respecting others'' rights, desert-based forfeiture remains available. This determines whether retributive and abolition readings coexist (different readings, no foreclosure) or whether one forecloses the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(desert_vs_dignity_framework_split, conceptual, 'Whether proportional desert can override categorical human dignity protections.').

omega_variable(
    moral_order_restoration_empirical_content,
    'Does execution actually restore the violated moral order, or is the restoration purely symbolic? What would count as empirical evidence either way?',
    'Measure recidivism, victim-family satisfaction, public confidence in justice, and moral order indicators (belief in fairness, trust in institutions) before and after execution. If execution measurably restores these, the moral order claim has empirical content. If restoration is purely symbolic (ritual without material effect), the constraint''s extractiveness is higher — it extracts the offender''s life without delivering on its coordination promise.',
    'If restoration is empirically empty, the constraint reclassifies as pure snare (execution as symbolic theater, extraction of life without functional coordination). If empirically grounded, the constraint remains tangled rope (real coordination function + asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_order_restoration_empirical_content, empirical, 'Whether moral order restoration through execution has empirical content or is purely ritual.').

omega_variable(
    alternative_readings_institutional_closure,
    'Within a single jurisdiction''s institutional framework, can a retributive reading and an abolition reading coexist, or does one necessarily displace the other?',
    'Examine jurisdictions that have transitioned from capital to non-capital punishment: did they explicitly foreclose retributive desert as legitimate, or did they adopt a different reading (deterrence, dignity-based limits) that makes desert compatible with life imprisonment? If explicit foreclosure, readings are in true conflict; if they adopted an alternative reading, coexistence is possible at the inter-jurisdictional level but not within a single framework.',
    'If readings foreclose each other, the retributive reading claims a unique moral warrant that competing readings cannot share. If readings coexist across jurisdictions, the retributive reading is one among several legitimate approaches, and its persistence depends on continued institutional endorsement rather than logical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_institutional_closure, conceptual, 'Whether alternative readings of state killing legitimacy can coexist within a single institutional framework.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression the retributive reading requires (keeping alternative readings excluded from the core legitimacy debate) structural (enforced by law, institutional closure) or internalized (offenders accept proportional desert as morally binding even outside the jurisdiction)?',
    'Examine death-row inmates'' own framing: do they accept desert-based forfeiture as legitimate, or do they contest it as imposed? Compare acceptance rates across jurisdictions with different cultural foundations for retributive doctrine. If acceptance is widespread, suppression is partially internalized (the doctrine has captured moral belief); if resistance is consistent, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — offenders carry the constraint''s logic with them. If structural, suppression depends on continued institutional enforcement; removal of enforcement might destabilize the constraint more quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of alternative readings is structural or internalized in offender acceptance.').

omega_variable(
    kernel_reading_committer_frame,
    'This constraint is one reading of the contested kernel ''state_killing_legitimacy.'' The sibling readings (deterrence, abolition) instantiate different constraints with different ε values and different beneficiary structures. Does modeling them as separate constraints (per ε-invariance) capture the true structure, or is there a meta-level commitment system that all three readings inhabit?',
    'Examine whether the three readings are genuinely incommensurable (each with its own ε and classification) or whether they share a common commitment kernel (the legitimacy of state killing) and differ only in interpretation. The first model treats them as separate constraints; the second treats them as readings of a single kernel. The resolution depends on whether the kernel-level commitment is prior (foundational to all three readings) or emergent (the product of choosing one reading).',
    'If kernel-prior, the three readings are siblings in a commitment system, linked by network.affects_constraints and documented in cs_structure. If reading-emergent, the kernel is a retrospective label for the cluster, not a prior commitment, and the three constraints are independent. This determines whether cross-reading validation (comparing readings against a shared kernel) is structural or conceptual.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether the three readings of state killing instantiate separate ε-invariant constraints or readings of a shared kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(stat_tr_t8, state_killing_legitimacy__retributive_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(stat_tr_t16, state_killing_legitimacy__retributive_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__retributive_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(stat_tr_t32, state_killing_legitimacy__retributive_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__retributive_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__retributive_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__retributive_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__retributive_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(stat_be_t32, state_killing_legitimacy__retributive_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__retributive_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(stat_su_t8, state_killing_legitimacy__retributive_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(stat_su_t16, state_killing_legitimacy__retributive_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(stat_su_t24, state_killing_legitimacy__retributive_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(stat_su_t32, state_killing_legitimacy__retributive_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__retributive_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_legitimacy__retributive_reading, 0.18).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'state_killing_legitimacy.' The deterrence and abolition readings instantiate different constraints with incommensurable ε values and beneficiary structures. They are linked because they contest the same institutional kernel and would influence one another's viability if hegemonic dominance shifted. Each reading is a clean, ε-invariant constraint; the sibling readings are NOT alternative framings of this one — they are structurally distinct claims. Decomposition follows the ε-invariance principle: when a natural-language concept ('state killing') covers multiple structurally distinct claims (justified by desert / justified by deterrence / never justified), separate constraint stories capture their distinct extraction profiles and beneficiary asymmetries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
