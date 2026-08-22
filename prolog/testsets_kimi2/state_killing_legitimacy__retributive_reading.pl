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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Retributive State Killing Legitimacy (Lex Talionis)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint is the retributive reading of the
 *   state_killing_legitimacy kernel: the claim that a murderer forfeits their
 *   right to life through proportional desert, and that the state is the
 *   legitimate agent of that forfeiture. Under this reading, the condemned
 *   offender is placed in the victim set not as an innocent wronged party but
 *   as a morally deserving target of the state's ultimate extraction. The
 *   constraint operates through elaborate legal theaterâlengthy appeals,
 *   medicalized execution protocols, and victim-impact ritualsâthat masks
 *   the raw violence of the taking with procedural legitimacy. The
 *   retributive public and victim-survivors are the named beneficiaries,
 *   receiving moral satisfaction and symbolic closure. This story is authored
 *   as one reading of a contested kernel; the abolition and deterrence
 *   readings instantiate structurally distinct constraints and are not folded
 *   into this constraint's classification.
 *
 * KEY AGENTS:
 *   - Condemned offenders (powerless/trapped): Primary target â bear the extraction of life.
 *   - State execution apparatus (institutional/analytical): Agenda-setter â administers the constraint and could abolish it.
 *   - Retributive public (organized/constrained): Primary beneficiary â receives moral satisfaction and order-restoration narrative.
 *   - Victim-survivors (moderate/constrained): Secondary beneficiary â promised closure through proportional desert.
 *   - Abolitionist advocates (organized/constrained): Excluded â objections are structurally dismissed within the retributive framework.
 *   - Human rights observers (institutional/analytical): Analytical observer â monitors and documents without direct control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.95).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.92).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "Retributive State Killing Legitimacy (Lex Talionis)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '2dac802e-e1cc-450f-b683-f35a5c064bff').
narrative_ontology:cs_kernel_codification('2dac802e-e1cc-450f-b683-f35a5c064bff', formalized).
narrative_ontology:cs_authority_grounding('2dac802e-e1cc-450f-b683-f35a5c064bff', lineage).
narrative_ontology:cs_interpretation_layer_present('2dac802e-e1cc-450f-b683-f35a5c064bff').
narrative_ontology:cs_reading_relation('2dac802e-e1cc-450f-b683-f35a5c064bff', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_reading_relation('2dac802e-e1cc-450f-b683-f35a5c064bff', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('2dac802e-e1cc-450f-b683-f35a5c064bff', foundational, murderer_forfeits_life_right).
narrative_ontology:cs_axiom_status(murderer_forfeits_life_right, holdable).
narrative_ontology:cs_axiom_grounding('2dac802e-e1cc-450f-b683-f35a5c064bff', murderer_forfeits_life_right, deontological).
narrative_ontology:cs_axiom('2dac802e-e1cc-450f-b683-f35a5c064bff', secondary, state_monopoly_on_capital_punishment).
narrative_ontology:cs_axiom_status(state_monopoly_on_capital_punishment, holdable).
narrative_ontology:cs_axiom_grounding('2dac802e-e1cc-450f-b683-f35a5c064bff', state_monopoly_on_capital_punishment, conventional).
narrative_ontology:cs_reference_frame('2dac802e-e1cc-450f-b683-f35a5c064bff', lex_talionis_framework).
narrative_ontology:cs_drift_state('2dac802e-e1cc-450f-b683-f35a5c064bff', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2dac802e-e1cc-450f-b683-f35a5c064bff', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, retributive_public).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victim_survivors).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, condemned_offenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convicted of capital murder and sentenced to death under a desert-based framework. They are physically incarcerated on death row, exhaust appeals within the retributive procedural channel, and are executed by state apparatus. Their objections to the legitimacy of the punishment are procedurally recorded but structurally dismissed as irrelevant because they are deemed to have forfeited their right to object.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, condemned_offenders, payer,
    powerless, immediate, trapped, local).

% Administers capital punishment through statutes, prosecutorial charging decisions, appellate review, and execution protocols. Justifies the practice as proportional justice and moral balance. Could abolish the practice by legislative or executive action but maintains it in response to retributive constituency demand.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Segments of the public and media that demand proportional punishment for murder and derive moral satisfaction from the claim that justice has been done. They benefit symbolically from the assertion that the moral order has been restored, without bearing the direct costs of administration or execution.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, retributive_public, beneficiary,
    organized, biographical, constrained, national).

% Families and communities of murder victims who are promised closure and moral balance through the offender's execution. They are incorporated into the process as beneficiaries of desert, sometimes through victim-impact statements and witnessing protocols, and their continued support is cited as evidence the constraint serves a genuine need.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victim_survivors, beneficiary,
    moderate, biographical, constrained, local).

% Human rights organizations, defense attorneys, and religious groups arguing that state killing is categorically impermissible. They are procedurally allowed to file briefs and lobby but are structurally excluded from the retributive legitimacy framework, which treats their position as morally illegitimate because the offender deserves death.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_advocates, excluded,
    organized, biographical, constrained, national).

% International bodies and NGOs that monitor state compliance with human rights norms. They document execution practices, racial disparities, and procedural flaws, publishing findings that exert reputational pressure on retentionist jurisdictions but do not directly control the constraint.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, human_rights_observers, observer,
    institutional, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels private vengeance and communal blood-debt into a state-administered proportionate response, theoretically preventing cycles of retaliation and satisfying the moral community's demand for balanced justice.
% TRANSFER_FUNCTION: Transfers the condemned offender's life to the moral community as proportional payment for murder; transfers the authority to execute to the state apparatus as monopoly legatee of private vengeance.
% ABSENT_VOICES: The condemned themselves, abolitionist advocates, and international human rights monitors are procedurally heard in appeals but structurally excluded from the legitimacy conversationâthe retributive framework treats their objections as irrelevant because the offender morally deserves the outcome.
% DISAPPEARANCE_RATIONALE: The retributive constituency would lose its primary vehicle for expressing proportional moral balance; victim-survivors would be denied the closure the framework promises; the state would need to construct an alternative symbolic mechanism to absorb blood-debt and prevent resurgence of private vengeance claims.
% FOUNDING_PROBLEM: Unchecked private vengeance after homicide and the unsatisfied psychological and social demand for proportionate moral balance.
% FOUNDING_PROBLEM_CORROBORATION: Victim-survivors and the retributive public attest the problem remains live. Criminological research and the operational experience of abolitionist jurisdictions outside the beneficiary set demonstrate that homicide can be addressed without execution, corroborating that the founding problem is not structurally dependent on the death penalty.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.95, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is near-total (0.95) because the constraint extracts the entirety of the condemned's remaining life. Suppression is comparably high (0.92): the state physically isolates the condemned, excludes abolitionist alternatives from the legitimacy framework, and hardens procedural barriers against commutation. Theater ratio is elevated (0.65) and rising over the interval, reflecting the increasing ritualization of execution as it becomes rarerâdouble-lock protocols, witness galleries, and medicalized procedures perform civilization while concealing the violence they facilitate. Accessibility collapse is very high (0.9) because, once the retributive framework fixes desert, no alternative sentence satisfies its own logic. Resistance is moderate (0.45) because abolitionist movements persist but are structurally disadvantaged in retentionist jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state apparatus) experiences the constraint as the administration of justice and moral balance; the payer seat (condemned offender) experiences it as total extraction dressed in procedural legitimacy. The beneficiary seats (retributive public, victim-survivors) experience symbolic restoration. The engine will compute divergent per-seat classifications from this structural asymmetry: the payer seat should register near-total effective extraction, while the beneficiary seats register negative or negligible extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned offender is the sole structural victim (payer); their directionality is driven to the full-target end by their status as the extracted life, their powerlessness, and their trapped exit. The retributive public and victim-survivors are beneficiaries; their directionality sits near the beneficiary end because the constraint subsidizes their demand for moral closure rather than extracting from them. The state apparatus sits between: it is the enforcement agent and accrues authority, but does not personally bear the cost or receive the moral benefit. No directionality overrides are needed because the structural derivation from beneficiary/victim declarations and exit options already captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunchecked private vengeance and the need for proportionate moral balanceâis contested. While blood feuds were historically real, modern abolitionist jurisdictions handle homicide without execution, suggesting the founding problem no longer requires this specific arrangement. The constraint persists not because private vengeance would return overnight, but because the retributive public continues to demand it and the state apparatus retains the infrastructure. This is mandatrophy risk: a mandate that may have outlived its original function but persists because beneficiaries still assert the problem is live. The temporal measurements show rising theater and stable-high extraction, consistent with a constraint whose functional justification has weakened while its performative maintenance has intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_identity,
    'This constraint is the retributive reading of the state_killing_legitimacy kernel; how would the classification change if the abolition reading were adopted as the operative framework?',
    'Comparative analysis of the abolition reading''s constraint story (zero extraction, no beneficiaries, high resistance) against this reading''s metric profile.',
    'Would dissolve the extraction structure entirely or reclassify the remaining state-killing practice as a distinct snare with different legitimating cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_identity, conceptual, 'Kernel reading identity and sibling structural delta').

omega_variable(
    sibling_reading_structural_delta,
    'What specific structural element differentiates the retributive reading from its deterrence and abolition siblings?',
    'Examine the victim set composition and legitimating narrative: retributive places condemned offenders in the victim set as morally deserving; abolition places them as wronged innocents; deterrence places them as instrumental sacrifices.',
    'Determines whether the extraction is framed as deserved justice, wrongful violence, or efficient social engineeringâchanging the directionality logic and the moral legitimacy of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural locus of disagreement across kernel readings').

omega_variable(
    desert_as_natural_or_constructed,
    'Is the retributive desert claim a constructed moral convention or a natural moral law?',
    'Cross-cultural and historical analysis of punishment norms, plus evolutionary and anthropological evidence on reciprocity and blood-debt.',
    'If a natural law, the extractiveness metric might be mis-calibrated as extraction rather than enforcement of a moral constant; if constructed, the constraint is a false-summit candidate masquerading as moral necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_as_natural_or_constructed, empirical, 'Natural law versus constructed legitimacy of desert').

omega_variable(
    retributive_coordination_or_cover,
    'Does the retributive constraint genuinely coordinate the prevention of private vengeance, or is the blood-feud prevention story a legitimating cover for state violence?',
    'Comparative criminological study of homicide-recidivism and vigilantism rates in abolitionist versus retentionist jurisdictions, controlling for rule-of-law indicators.',
    'If private vengeance resurges significantly upon abolition, the constraint carries a genuine coordination function and may reclassify as tangled_rope; if not, the coordination story is cover and the snare classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retributive_coordination_or_cover, empirical, 'Coordination function veracity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skl_retrib_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(skl_retrib_tr_t14, state_killing_legitimacy__retributive_reading, theater_ratio, 14, 0.47).
narrative_ontology:measurement(skl_retrib_tr_t28, state_killing_legitimacy__retributive_reading, theater_ratio, 28, 0.53).
narrative_ontology:measurement(skl_retrib_tr_t42, state_killing_legitimacy__retributive_reading, theater_ratio, 42, 0.58).
narrative_ontology:measurement(skl_retrib_tr_t56, state_killing_legitimacy__retributive_reading, theater_ratio, 56, 0.62).
narrative_ontology:measurement(skl_retrib_tr_t70, state_killing_legitimacy__retributive_reading, theater_ratio, 70, 0.65).

% Extraction over time
narrative_ontology:measurement(skl_retrib_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(skl_retrib_be_t14, state_killing_legitimacy__retributive_reading, base_extractiveness, 14, 0.89).
narrative_ontology:measurement(skl_retrib_be_t28, state_killing_legitimacy__retributive_reading, base_extractiveness, 28, 0.91).
narrative_ontology:measurement(skl_retrib_be_t42, state_killing_legitimacy__retributive_reading, base_extractiveness, 42, 0.93).
narrative_ontology:measurement(skl_retrib_be_t56, state_killing_legitimacy__retributive_reading, base_extractiveness, 56, 0.94).
narrative_ontology:measurement(skl_retrib_be_t70, state_killing_legitimacy__retributive_reading, base_extractiveness, 70, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(skl_retrib_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(skl_retrib_su_t14, state_killing_legitimacy__retributive_reading, suppression_requirement, 14, 0.87).
narrative_ontology:measurement(skl_retrib_su_t28, state_killing_legitimacy__retributive_reading, suppression_requirement, 28, 0.88).
narrative_ontology:measurement(skl_retrib_su_t42, state_killing_legitimacy__retributive_reading, suppression_requirement, 42, 0.9).
narrative_ontology:measurement(skl_retrib_su_t56, state_killing_legitimacy__retributive_reading, suppression_requirement, 56, 0.91).
narrative_ontology:measurement(skl_retrib_su_t70, state_killing_legitimacy__retributive_reading, suppression_requirement, 70, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the state_killing_legitimacy kernel. The retributive reading isolates desert-based justification and the forfeiture of life-right, while the abolition and deterrence readings instantiate structurally distinct constraints with different victim sets, beneficiary structures, and epsilon values. They are linked as a constraint family under the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
