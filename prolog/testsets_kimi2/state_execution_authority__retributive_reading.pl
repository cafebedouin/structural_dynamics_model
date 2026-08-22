% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: State Execution Authority (Retributive Reading)
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This constraint is the retributive reading of the
 *   state_execution_authority kernel. Under this reading, the state's power
 *   to execute is not merely instrumental (deterrence) nor categorically
 *   forbidden (abolition), but a positive moral duty to restore proportionate
 *   balance after heinous crimes. The executed offender is structurally the
 *   payer; victims' families are beneficiaries; the state apparatus is the
 *   agenda-setter. The claim is tangled_rope because the arrangement
 *   coordinates a genuine moral-order function for victims and society while
 *   asymmetrically extracting the ultimate cost from a powerless, trapped
 *   population.
 *
 * KEY AGENTS:
 *   - state_execution_apparatus: agenda_setter (institutional/constrained) â enforces the retributive framework and derives legitimacy from delivering proportionate punishment
 *   - victims_families: beneficiary (organized/constrained) â receive moral restoration through the execution of the offender
 *   - condemned_offenders: payer (powerless/trapped) â bear the irreducible cost of the constraint, with imprisonment rejected as insufficient
 *   - abolitionist_advocates: excluded (organized/constrained) â contest the framework but are structurally outside its decision logic
 *   - retributive_philosophers: observer (analytical/analytical) â provide normative justification without material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.88).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.78).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority (Retributive Reading)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'b8fda539-0ceb-4257-af48-caa5109ddb5e').
narrative_ontology:cs_kernel_codification('b8fda539-0ceb-4257-af48-caa5109ddb5e', formalized).
narrative_ontology:cs_authority_grounding('b8fda539-0ceb-4257-af48-caa5109ddb5e', lineage).
narrative_ontology:cs_interpretation_layer_present('b8fda539-0ceb-4257-af48-caa5109ddb5e').
narrative_ontology:cs_reading_relation('b8fda539-0ceb-4257-af48-caa5109ddb5e', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_reading_relation('b8fda539-0ceb-4257-af48-caa5109ddb5e', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('b8fda539-0ceb-4257-af48-caa5109ddb5e', foundational, execution_restores_moral_balance).
narrative_ontology:cs_axiom_status(execution_restores_moral_balance, holdable).
narrative_ontology:cs_axiom_grounding('b8fda539-0ceb-4257-af48-caa5109ddb5e', execution_restores_moral_balance, deontological).
narrative_ontology:cs_axiom('b8fda539-0ceb-4257-af48-caa5109ddb5e', foundational, proportionality_requires_capital_punishment).
narrative_ontology:cs_axiom_status(proportionality_requires_capital_punishment, holdable).
narrative_ontology:cs_axiom_grounding('b8fda539-0ceb-4257-af48-caa5109ddb5e', proportionality_requires_capital_punishment, deontological).
narrative_ontology:cs_reference_frame('b8fda539-0ceb-4257-af48-caa5109ddb5e', proportional_moral_balance).
narrative_ontology:cs_drift_state('b8fda539-0ceb-4257-af48-caa5109ddb5e', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b8fda539-0ceb-4257-af48-caa5109ddb5e', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, condemned_offenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers capital sentences through corrections departments and courts; enforces the moral-restitution framework by carrying out executions; its legitimacy depends on delivering punishment proportionate to the offense, and it is politically constrained by the retributive expectations of the electorate.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Receive the state's imposition of proportionate punishment as a form of moral restoration for the murder of their family member; their need for retribution is cited as a primary justification for the constraint; they depend on the state apparatus to deliver the execution and have no alternative route to the same symbolic satisfaction.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    organized, biographical, constrained, national).

% Bear the ultimate cost of the retributive framework; condemned to death for heinous crimes; imprisonment is explicitly rejected by this reading as insufficient moral payment; they have no exit from the sentence once imposed, and their voice is excluded from the framework's moral calculus by the fact of their condemnation.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, condemned_offenders, payer,
    powerless, immediate, trapped, national).

% Argue that state execution is categorically impermissible and that moral balance cannot be restored through killing; structurally excluded from the retributive framework's decision-making but mount legal and political resistance, including challenges based on wrongful convictions and evolving standards of decency.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_advocates, excluded,
    organized, biographical, constrained, national).

% Analyze and justify the constraint from deontological and proportionalist frameworks; they do not benefit materially or suffer the constraint directly, but provide the normative architecture that distinguishes this reading from instrumental deterrence or abolitionist positions.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retributive_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restores social moral balance after heinous crimes by imposing a punishment proportionate to the offense, thereby vindicating the victim's worth and preventing the moral disorder that would follow if the worst crimes were answered by imprisonment alone.
% TRANSFER_FUNCTION: Transfers the condemned offender's life to the moral ledger of the state and the victim's community; the offender bears the total and irreducible cost, while victims' families and the broader society receive the restoration of moral proportion.
% ABSENT_VOICES: The condemned offenders themselves are silenced by the legitimacy of their condemnation within the retributive framework; abolitionist advocates are treated as morally mistaken about the permissibility of proportional killing and are excluded from agenda-setting.
% DISAPPEARANCE_RATIONALE: The retributive framework depends on execution as the unique and irreducible payment for heinous crimes; if it disappeared, the moral order would demand substitution, but imprisonment is explicitly rejected as insufficient, leaving a structural gap in the theory of justice and removing the prescribed form of restoration for victims' families.
% FOUNDING_PROBLEM: The problem of moral disorder following heinous crimes where the offender's life is deemed the only proportionate payment; the need to vindicate the victim's dignity and prevent the appearance that the worst crimes are merely manageable by imprisonment.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated externally by victim-advocacy organizations and some procedural-justice research citing family satisfaction with sentences. Challenged externally by human-rights bodies and empirical studies showing no clear evidence that execution restores victim well-being; the abolitionist reading explicitly denies the problem as framed, asserting that moral balance is not achievable through state killing.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.88, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is very high (0.88) because the constraint demands an irreducible, non-substitutable payment: the offender's life. Suppression is high (0.78) because the constraint's persistence depends on active state enforcement and on excluding abolitionist alternatives such as life without parole. Theater ratio rises over the interval (0.20 to 0.45) because the legal apparatus persists and performs moral proportionality even as execution volumes decline in many jurisdictions, making the constraint increasingly performative. Accessibility collapse is high (0.75) because the retributive logic explicitly rejects imprisonment as a moral substitute for the worst crimes. Resistance is moderate (0.55) because abolitionist movements, innocence projects, and evolving human-rights norms generate sustained pushback.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as necessary moral coordination; the payer seat experiences it as total, irrevocable extraction. The abolitionist observer seat, though excluded from the constraint's internal logic, experiences it as a snare masquerading as justice. The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned offender sits at full target (d near 1.0): they are trapped, powerless, and bear the total extraction of the constraint. Victims' families sit at full beneficiary (d near 0.0): they receive the moral restoration the constraint is designed to deliver. The state apparatus sits near the beneficiary end but not at zero (d ~0.25) because it derives institutional legitimacy and political support from enforcing the framework, though it does not capture the extraction as a material rent. Abolitionist advocates are excluded from the directionality derivation because they are not seated within the constraint's operating structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (moral disorder after heinous crime) is contested as to whether it remains live. The retributive reading maintains it is live, but the rising theater ratio and growing resistance suggest the constraint may be drifting toward performance. It does not satisfy the piton condition because there is a concentrated beneficiary (victims' families) and active agenda-setting by the state, which distinguishes it from an atrophied constraint with no maintainer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retributive_abolition_foreclosure,
    'Does the retributive reading''s claim that execution restores moral balance logically foreclose the abolitionist reading''s categorical imperative within a single normative framework, or do they remain incommensurable but coexisting political positions?',
    'Jurisdictional mapping: if no single legal framework has ever simultaneously held both that execution is morally required and categorically impermissible, foreclosure is established; if hybrid frameworks exist, coexistence is established.',
    'If foreclosure holds, the kernel is structurally bipolar with no middle ground; if coexistence holds, the readings are policy alternatives within a shared legal grammar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retributive_abolition_foreclosure, conceptual, 'Logical relationship between retributive and abolition readings').

omega_variable(
    wrongful_execution_validity,
    'Does the occurrence of wrongful executions structurally invalidate the retributive framework, or is it a tragic error separable from the moral logic of proportionate punishment?',
    'Innocence-project exoneration rates and procedural error analysis in capital cases.',
    'If wrongful executions are systemic and irreducible, the constraint''s moral legitimacy collapses and the retributive reading drifts toward snare; if rare and correctable, the framework absorbs the error as cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_validity, empirical, 'Whether wrongful execution invalidates the retributive framework').

omega_variable(
    moral_balance_measurability,
    'Can the restoration of moral balance be empirically observed in victim-family outcomes, or is it a purely symbolic-deontological claim?',
    'Longitudinal psychological studies of victim-family well-being in death-penalty versus life-sentence cases.',
    'If no restorative effect is found, the coordination story lacks empirical support and the constraint reads as pure extraction; if found, it substantiates the retributive coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_balance_measurability, empirical, 'Empirical status of moral restoration claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__retributive_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__retributive_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__retributive_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__retributive_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__retributive_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__retributive_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__retributive_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.87).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__retributive_reading, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__retributive_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__retributive_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__retributive_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__retributive_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
