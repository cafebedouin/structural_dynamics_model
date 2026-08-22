% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__deterrence_instrument, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Capital Punishment as Deterrence Instrument
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   In retentionist jurisdictions, the state maintains capital punishment
 *   under the explicit justification that executions prevent future murders
 *   through general deterrence. The condemned person is treated as an
 *   instrumental cost in a social cost-benefit calculus: their life is
 *   forfeit if the state judges that the taking of it will save other lives.
 *   This arrangement creates a structural relationship where a diffuse,
 *   unorganized population is claimed as beneficiary, a discrete and legally
 *   trapped population bears the terminal cost, and the state administers the
 *   lethal exchange. The constraint's persistence depends on active
 *   enforcement of death sentences, exclusion of abolitionist alternatives,
 *   and continuous empirical assertion that the deterrent effect is real and
 *   outweighs the costs of error, delay, and moral hazard.
 *
 * KEY AGENTS:
 *   - state_execution_authority (agenda_setter / institutional / constrained exit): Maintains statutory, carceral, and lethal apparatus; justifies killings by citing deterrence research and cost-benefit rhetoric.
 *   - condemned_persons (payer / powerless / trapped): Bear the extraction directly through state-administered death; no exit from death row except through rare clemency or exoneration.
 *   - future_potential_victims (beneficiary / powerless / constrained): Diffuse, unorganized population claimed to be spared homicide by the threat of execution; cannot verify the benefit.
 *   - empirical_researchers (observer / analytical / analytical exit): Evaluate deterrence claims; structurally outside the exchange but their findings are weaponized by both sides.
 *   - international_human_rights_bodies (excluded / institutional / constrained): Reject instrumental killing on rights grounds; excluded from domestic sentencing validity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.85).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.76).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Capital Punishment as Deterrence Instrument").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, 'd2219501-6e88-47fe-9bc1-10ee6029cf72').
narrative_ontology:cs_kernel_codification('d2219501-6e88-47fe-9bc1-10ee6029cf72', formalized).
narrative_ontology:cs_authority_grounding('d2219501-6e88-47fe-9bc1-10ee6029cf72', lineage).
narrative_ontology:cs_interpretation_layer_present('d2219501-6e88-47fe-9bc1-10ee6029cf72').
narrative_ontology:cs_reading_relation('d2219501-6e88-47fe-9bc1-10ee6029cf72', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('d2219501-6e88-47fe-9bc1-10ee6029cf72', state_killing_authority__categorical_abolition, coexists_with).
narrative_ontology:cs_axiom('d2219501-6e88-47fe-9bc1-10ee6029cf72', foundational, execution_conditional_on_deterrence).
narrative_ontology:cs_axiom_status(execution_conditional_on_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('d2219501-6e88-47fe-9bc1-10ee6029cf72', execution_conditional_on_deterrence, empirically_contingent).
narrative_ontology:cs_axiom('d2219501-6e88-47fe-9bc1-10ee6029cf72', secondary, state_may_instrumentalize_life_for_safety).
narrative_ontology:cs_axiom_status(state_may_instrumentalize_life_for_safety, holdable).
narrative_ontology:cs_axiom_grounding('d2219501-6e88-47fe-9bc1-10ee6029cf72', state_may_instrumentalize_life_for_safety, instrumental).
narrative_ontology:cs_reference_frame('d2219501-6e88-47fe-9bc1-10ee6029cf72', deterrence_justification_framework).
narrative_ontology:cs_drift_state('d2219501-6e88-47fe-9bc1-10ee6029cf72', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d2219501-6e88-47fe-9bc1-10ee6029cf72', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, general_public).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the statutory, carceral, and lethal apparatus for capital sentencing, death row incarceration, and execution. Justifies the practice by citing general deterrence and social cost-benefit analyses. Controls the timing, method, and legal procedures of execution, and actively defends the constraint against abolitionist challenges.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_execution_authority, agenda_setter,
    institutional, generational, constrained, national).

% Individuals sentenced to death under the deterrence rationale. They bear the ultimate cost of the constraint: loss of life. Their exit is blocked by physical confinement on death row, exhaustion of appeals, and the state's monopoly on lethal force. They are instrumentalized as the means to a claimed social benefit they do not share.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, immediate, trapped, local).

% Diffuse, unorganized population of persons who, according to the deterrence claim, are spared from murder because the threat of execution dissuades potential killers. They do not choose to be beneficiaries, cannot verify the benefit, and have no voice in whether the exchange takes place.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, future_potential_victims, beneficiary,
    powerless, biographical, constrained, national).

% Citizenry claimed to benefit from reduced homicide rates via deterrence. They fund the system through taxation and receive the claimed public good of safety. Their influence over the constraint is mediated through representative politics and diluted by the diffuse nature of the benefit.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, general_public, beneficiary,
    moderate, biographical, constrained, national).

% Criminologists and economists who evaluate whether executions yield marginal deterrence effects. Their findings are systematically mixed and contested; they are structurally outside the beneficiary-payer dyad, but their work is cited by both sides to legitimate or challenge the constraint.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, empirical_researchers, observer,
    analytical, generational, analytical, global).

% Treaty bodies and human rights monitors that reject instrumental killing as a violation of the right to life. They are formally excluded from the domestic legal process that validates executions, and their objections are treated as external political pressure rather than binding authority.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, international_human_rights_bodies, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents future murders by raising the expected cost of homicide through the threat of state execution, thereby coordinating potential offenders away from lethal violence.
% TRANSFER_FUNCTION: Transfers the life of the condemned person from the condemned to the state's claimed public-safety ledger, in exchange for a hypothesized reduction in future victimization that accrues to the diffuse public.
% ABSENT_VOICES: Wrongfully convicted persons who have been executed cannot testify to the error rate; future potential victims who are supposedly saved by deterrence are absent because their non-victimization is unobservable and they are not organized; international human rights bodies are excluded from domestic capital sentencing proceedings.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, death rows would empty, sentences would convert to life terms, prosecutorial charging practices would shift away from capital specifications, and the claimed deterrence signal would disappear. The criminal justice system would rearrange around incapacitation and rehabilitation rather than lethal instrumentalization.
% FOUNDING_PROBLEM: High homicide rates and the perceived inadequacy of non-lethal sanctions to dissuade the most serious offenders, coupled with public demand for decisive state action against lethal violence.
% FOUNDING_PROBLEM_CORROBORATION: Retentionist legislators and law enforcement assert the problem is still live, citing violent crime. Abolitionist movements and empirical researchers attest that the founding problem is solved or misdiagnosed because LWOP provides equivalent incapacitation and deterrence is unproven. Corroboration from outside the benefiting parties comes from criminological meta-analyses and international human rights monitors who dispute both the problem's severity and the constraint's efficacy.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__deterrence_instrument_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.85 because the constraint takes life â the maximum extraction from an individual â as its core operational mechanism. Suppression is 0.76 because the arrangement depends on active legal suppression of abolition, restrictive clemency practices, and procedural barriers that prevent condemned persons from escaping the sentence. Theater_ratio is 0.48: the lethal injection protocol, extended death row stays, and legislative deterrence rhetoric are partly performative maintenance of a shrinking practice whose empirical foundation has eroded. Accessibility_collapse is 0.62: within retentionist jurisdictions, the deterrence frame makes abolition politically inaccessible, though LWOP and abolitionist alternatives remain visible globally. Resistance is 0.74: the constraint faces persistent legal challenge, abolitionist advocacy, and international condemnation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is a regrettable but necessary instrument of public safety; from the condemned seat, it is the terminal extraction of their life for a social benefit they do not share and cannot verify. The future_potential_victims seat experiences the constraint as an invisible, unchosen protection, while the empirical_researcher seat sees a contested empirical claim propping up an irreversible act. The engine computes these divergences from the structural data: agenda_setter has constrained exit and institutional power, condemned has trapped exit and no power, beneficiaries have constrained exit and no power.
 *
 * DIRECTIONALITY LOGIC:
 *   The structural beneficiary is the diffuse future_potential_victims population (low d, near the full-beneficiary end), with general_public also oriented toward beneficiary status. The condemned_persons are the clear victim group (high d, near full-target). The state_execution_authority is not a beneficiary of the extraction in the sense of capturing the extracted life; rather, it administers the transfer. Its directionality is structurally intermediate, but because it is neither beneficiary nor victim, the derivation places it closer to symmetric or moderate beneficiary (it gains political legitimacy and budgetary flow, but these are secondary). No override is required.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence_instrument reading contains the seeds of its own mandatrophy: it justifies the constraint solely by a contingent empirical claim. If the deterrence premise is falsified or the cost (including wrongful executions) is judged unacceptable, the reading's own logic demands abolition. Mandatrophy is detected when the practice persists despite the collapse of its coordinating premise. The current state â where empirical findings are contested, the practice shrinks, yet retentionist jurisdictions maintain the apparatus â signals incipient piton transition. The authored metrics (theater_ratio 0.48, high resistance) reflect this: the constraint is increasingly maintained through performance rather than demonstrated coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_ambiguity,
    'Does capital punishment produce a marginal deterrent effect on homicide that exceeds the effect of life imprisonment without parole?',
    'Meta-analysis of natural experiments comparing homicide trends across abolitionist and retentionist jurisdictions, controlling for demographic and economic variables, plus panel studies around moratoria and commutations.',
    'If general deterrence is negligible, the coordination function collapses and the constraint reclassifies toward snare or piton; if robust, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_ambiguity, empirical, 'Whether the core empirical premise of the deterrence reading is true.').

omega_variable(
    acceptable_cost_instrumentality,
    'What costs â wrongful executions, racial disparity, fiscal burden, moral hazard â are ''acceptable'' under the deterrence calculus, and who decides?',
    'Comparative policy analysis of retentionist versus abolitionist jurisdictions measuring error rates, cost per execution, and distributional disparity; normative analysis of whose preferences count in the social welfare function.',
    'If the cost threshold is set high enough to tolerate known wrongful executions, the constraint reads as more extractive; if set low, the constraint becomes self-abolishing under its own logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(acceptable_cost_instrumentality, preference, 'The normative threshold for acceptable cost in the deterrence calculus.').

omega_variable(
    kernel_reading_contest,
    'Is the deterrence_instrument reading of state killing authority held as a genuine empirical policy position, or primarily as a rhetorical cover for retributive sentiment?',
    'Discourse analysis of legislative debates and judicial opinions in retentionist jurisdictions to measure the relative salience of deterrence versus desert rhetoric, paired with voting behavior when deterrence evidence is presented.',
    'If deterrence is primarily cover, the constraint''s coordination type is misidentified and the effective extraction is higher than the instrumental framing suggests; if genuine, the reading remains structurally distinct from retributive_desert.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the deterrence reading is a sincere empirical position or a cover for retributive aims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t8, state_killing_authority__deterrence_instrument, theater_ratio, 8, 0.34).
narrative_ontology:measurement(stat_tr_t16, state_killing_authority__deterrence_instrument, theater_ratio, 16, 0.38).
narrative_ontology:measurement(stat_tr_t24, state_killing_authority__deterrence_instrument, theater_ratio, 24, 0.43).
narrative_ontology:measurement(stat_tr_t32, state_killing_authority__deterrence_instrument, theater_ratio, 32, 0.46).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__deterrence_instrument, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(stat_be_t8, state_killing_authority__deterrence_instrument, base_extractiveness, 8, 0.83).
narrative_ontology:measurement(stat_be_t16, state_killing_authority__deterrence_instrument, base_extractiveness, 16, 0.83).
narrative_ontology:measurement(stat_be_t24, state_killing_authority__deterrence_instrument, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(stat_be_t32, state_killing_authority__deterrence_instrument, base_extractiveness, 32, 0.84).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__deterrence_instrument, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stat_su_t8, state_killing_authority__deterrence_instrument, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(stat_su_t16, state_killing_authority__deterrence_instrument, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(stat_su_t24, state_killing_authority__deterrence_instrument, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(stat_su_t32, state_killing_authority__deterrence_instrument, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__deterrence_instrument, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the state_killing_authority kernel. The deterrence_instrument reading differs from retributive_desert in its empirical rather than deontological grounding, and from categorical_abolition in its permissibility of state killing under contingent conditions. Each reading warrants its own constraint story with independent epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
