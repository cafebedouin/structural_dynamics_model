% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: Capital Punishment as Retributive Desert (Lex Talionis Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the retributive-desert reading of the
 *   state-killing-authority kernel: murderers forfeit their right to life,
 *   and proportional punishment requires death for death, independent of
 *   whether execution deters future crime. This is a distinct constraint from
 *   the deterrence_instrument reading (which conditions justification on
 *   preventing future murders — an empirical, falsifiable claim) and from
 *   categorical_abolition (which denies the forfeiture premise entirely,
 *   holding life inalienable regardless of the crime). Under the ε-invariance
 *   principle, these are not the same constraint measured three ways; they
 *   are three constraints with different beneficiary/victim structures,
 *   different failure modes, and different ε values. This file authors only
 *   the retributive-desert reading's own structure and metrics.
 *
 * KEY AGENTS:
 *   - condemned_persons: primary target (powerless/trapped) — forfeits life under the theory
 *   - wrongfully_convicted_death_row_inmates: subset bearing the theory's failure mode when guilt determination errs
 *   - murder_victims_posthumous_vindication: named non-agent beneficiary whose forfeited life sets the proportionality measure
 *   - victims_surviving_family: living beneficiary seat claiming moral satisfaction from proportional punishment
 *   - prosecutors_and_capital_case_offices, state_execution_apparatus: institutional agenda-setters administering the forfeiture theory into sentences and executions
 *   - categorical_abolitionists: excluded voice whose core premise the theory cannot accommodate
 *   - constitutional_courts: analytical observer testing the theory against constitutional limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.58).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.72).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "Capital Punishment as Retributive Desert (Lex Talionis Reading)").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '7d54fc87-358c-49e6-ada5-7b75d55a76f5').
narrative_ontology:cs_kernel_codification('7d54fc87-358c-49e6-ada5-7b75d55a76f5', distributed).
narrative_ontology:cs_authority_grounding('7d54fc87-358c-49e6-ada5-7b75d55a76f5', distributed).
narrative_ontology:cs_reading_relation('7d54fc87-358c-49e6-ada5-7b75d55a76f5', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_reading_relation('7d54fc87-358c-49e6-ada5-7b75d55a76f5', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_axiom('7d54fc87-358c-49e6-ada5-7b75d55a76f5', foundational, murder_forfeits_right_to_life).
narrative_ontology:cs_axiom_status(murder_forfeits_right_to_life, holdable).
narrative_ontology:cs_axiom_grounding('7d54fc87-358c-49e6-ada5-7b75d55a76f5', murder_forfeits_right_to_life, deontological).
narrative_ontology:cs_axiom('7d54fc87-358c-49e6-ada5-7b75d55a76f5', foundational, proportionality_requires_life_for_life_equivalence).
narrative_ontology:cs_axiom_status(proportionality_requires_life_for_life_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('7d54fc87-358c-49e6-ada5-7b75d55a76f5', proportionality_requires_life_for_life_equivalence, deontological).
narrative_ontology:cs_reference_frame('7d54fc87-358c-49e6-ada5-7b75d55a76f5', classical_lex_talionis_desert_framework).
narrative_ontology:cs_drift_state('7d54fc87-358c-49e6-ada5-7b75d55a76f5', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7d54fc87-358c-49e6-ada5-7b75d55a76f5', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_posthumous_vindication).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, victims_surviving_family).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, retributive_justice_system_legitimacy).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, capital_defense_indigent_class).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, proportionality_norm_of_desert).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, moral_equivalence_of_punishment_and_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convicted of murder and sentenced to death under a proportionality theory holding that the killing forfeited their right to life. They have no exit from the sentence except appeal, clemency, or exoneration; the retributive framework treats their execution as the moral settlement of the crime, not as a policy choice weighing costs and benefits.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_persons, payer,
    powerless, immediate, trapped, national).

% A documented subset of the condemned population who did not commit the murder for which they are sentenced. The forfeiture logic that justifies execution presupposes accurate guilt determination; when that presupposition fails, the same structure that vindicates true victims kills innocents under the same warrant, with no proportionate remedy available post-execution.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, national).

% Indigent capital defendants disproportionately represented by under-resourced public defense relative to well-funded prosecution. Their exposure to the forfeiture-and-execution pipeline is structurally higher than a similarly-situated wealthy defendant's, meaning the retributive norm is applied unevenly along class and often racial lines even though the theory claims to track only the crime.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, capital_defense_indigent_class, payer,
    powerless, biographical, trapped, national).

% The murdered person, under this reading, is treated as having a moral claim that only proportional punishment can satisfy. They are not a living actor with agency going forward, but the retributive theory names their forfeited life as the measure the state's response must equal — the execution is framed as vindicating what was done to them, not as protecting the living.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims_posthumous_vindication, beneficiary,
    powerless, immediate, analytical, local).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murder_victims_posthumous_vindication).

% Family members of the murdered person who, under retributive theory, are owed the moral satisfaction of proportional punishment being carried out. Their standing to demand execution is treated as flowing from the desert claim itself, not from any independent policy benefit; many participate in sentencing hearings and clemency proceedings on this basis, though outcomes and years-long appeals often leave the promised closure unrealized.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, victims_surviving_family, beneficiary,
    moderate, biographical, constrained, national).

% Decide whether to seek the death penalty, invoking the forfeiture-desert theory as the justification for the charging decision. They administer the machinery that converts the philosophical claim into an actual sentence, control charging discretion, and bear no personal cost if the theory's premises (accurate guilt, proportionate application) fail in a given case.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, prosecutors_and_capital_case_offices, agenda_setter,
    institutional, biographical, arbitrage, regional).

% The correctional and judicial infrastructure that carries out death sentences — courts, corrections departments, execution teams. This apparatus's continued authority to take life is grounded, under this reading, in the forfeiture-desert theory rather than in any showing that executions reduce future murders; the theory itself is what licenses the killing.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_execution_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Hold that no crime forfeits the right to life and that state killing is inherently impermissible. Their premise directly contradicts the forfeiture axiom this reading depends on; they participate in the same public debate but their core claim cannot be accommodated within this reading's framework, only rejected or accepted wholesale.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, categorical_abolitionists, excluded,
    organized, generational, constrained, national).

% Adjudicate whether the forfeiture-desert theory satisfies constitutional constraints (cruel and unusual punishment, equal protection, due process). They do not administer executions but rule on whether the retributive justification, as applied, survives constitutional scrutiny — their rulings can narrow or widen the class of condemned persons without disturbing the underlying theory.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared public standard for what the state owes in response to murder — a determinate, proportionate response (death for death) rather than an ad hoc or purely discretionary one, aiming to prevent both under-punishment and disproportionate escalation.
% TRANSFER_FUNCTION: Moves the condemned person's life, treated as forfeited by the act of murder, into the state's disposal as the payment that equals the harm done; nothing material returns to the victim, who cannot be restored, but the theory holds that a moral ledger is balanced for the victim's survivors and for the legitimacy of the justice system.
% ABSENT_VOICES: Categorical abolitionists reject the forfeiture premise outright and are structurally unable to be satisfied by any application of this reading, however careful; wrongfully convicted persons executed under this framework have no voice at all post-execution, and their exclusion from any subsequent correction is total and irreversible.
% DISAPPEARANCE_RATIONALE: If the forfeiture-desert justification vanished, capital sentencing statutes grounded in retribution would lose their doctrinal basis; jurisdictions would need to re-ground capital punishment in deterrence (a separate, empirically contestable reading) or abolish it. Prosecutorial charging practices, sentencing procedures, and appellate doctrine built around proportionality-of-desert would all require restructuring.
% FOUNDING_PROBLEM: Historically, lex talionis emerged to cap private vengeance and blood feuds by fixing punishment at a determinate proportional level (an eye for an eye, not a life and a village), transferring the response from the victim's kin to a public, rule-bound authority.
% FOUNDING_PROBLEM_CORROBORATION: Retributive theorists and victims'-rights advocates attest the problem — the need for a determinate, non-escalating, publicly legitimate response to killing — remains live. Legal historians and comparative-law scholars outside the advocacy communities note that the original vengeance-capping function was largely achieved once punishment moved to public adjudication generally, and that the specific requirement of a life-for-a-life equivalence adds a further moral claim beyond what the vengeance-capping problem itself requires; wrongful-conviction researchers and constitutional scholars, also outside the beneficiary set, document that the guilt-accuracy presupposition the theory depends on fails at a measurable, nonzero rate.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__retributive_desert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is substantial but not maximal: the theory has a genuine coordination function (capping private vengeance with a determinate public standard) which keeps it below a pure-snare reading, but the extraction from condemned persons — especially the wrongfully convicted subset, for whom the theory's guilt-accuracy presupposition demonstrably fails at a nonzero rate — is real and irreversible. Suppression (0.72) is high because the arrangement depends on an active state apparatus (courts, corrections, execution infrastructure) to carry out an outcome that cannot be undone if later found erroneous, and because abolitionist and reform advocacy is a live, resisted political fight rather than settled consensus. Accessibility collapse is moderate (0.5): unlike a natural law, real jurisdictional alternatives (life imprisonment, restorative approaches) persist and are actively debated, so alternatives have not collapsed. Resistance is high (0.68), consistent with an actively contested constitutional and political battleground.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons and especially wrongfully convicted inmates sit at the target end: the constraint extracts their life directly, they cannot exit, and for the wrongfully convicted the extraction is total and irreversible with no proportionate correction available. Victims' surviving family and the posthumous victim occupy the beneficiary end: the theory's entire justificatory structure exists to satisfy their moral claim, though the living family's satisfaction is contingent and often unrealized after years of appeals. Prosecutors and the execution apparatus are institutional agenda-setters whose authority is legitimized by the theory but who bear none of its downside risk if the theory's premises fail in a given case — this asymmetry is central to why the tangled-rope reading holds rather than a clean rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (capping private vengeance with a determinate, publicly administered response) is largely solved by the existence of public adjudication itself; the additional, more specific claim that proportionality requires a life-for-a-life equivalence is a further moral commitment layered on top of the vengeance-capping function, and it is this additional layer that is contested as having outlived, or never actually served, an independent coordination need beyond what non-capital sentencing already provides. Classifying this as tangled_rope rather than snare preserves the genuine coordination residue (determinate public response to killing) while still registering the asymmetric extraction from an unconsented, ex-post-irreversible class (the wrongfully convicted) that the retributive theory's own success conditions cannot fully guard against.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_premise_versus_categorical_abolition,
    'Is the forfeiture-of-life premise (that committing murder eliminates the perpetrator''s right to life) a defensible moral claim, or does it presuppose exactly the alienability of the right to life that the abolitionist reading denies as a starting axiom?',
    'This is not empirically resolvable; it is a foundational disagreement about whether any act can forfeit an inalienable right. Resolution would require either a shared meta-ethical framework the two readings currently lack, or a legal/constitutional settlement (e.g., a court holding the right to life absolutely inalienable) that would foreclose the retributive reading as a matter of law even if it remained philosophically arguable.',
    'If the forfeiture premise is rejected as incoherent, this entire reading collapses into either the deterrence_instrument reading (retaining execution only on consequentialist grounds) or is foreclosed by categorical_abolition. If upheld, the reading retains independent justificatory force distinct from deterrence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(forfeiture_premise_versus_categorical_abolition, conceptual, 'Whether the forfeiture axiom coherently survives challenge from the inalienable-rights premise of the abolitionist sibling reading.').

omega_variable(
    wrongful_conviction_rate_and_theory_validity,
    'At what rate must wrongful capital convictions occur before the forfeiture-desert theory''s presupposition of accurate guilt determination is considered unreliable enough to undermine the theory''s practical application, even if the theory remains valid in principle for factually guilty defendants?',
    'Ongoing empirical research (DNA exoneration studies, innocence project data, post-execution forensic re-examination) establishing base rates of wrongful capital conviction and, where possible, wrongful execution.',
    'A higher documented rate strengthens the case that the theory''s application-in-practice, however sound in principle, produces extraction from an innocent-defendant class the theory itself would not endorse — pushing the classification toward snare at the applied level even if the in-principle theory remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_rate_and_theory_validity, empirical, 'Whether the empirically measured wrongful-conviction rate undermines the theory''s application independent of its philosophical validity.').

omega_variable(
    vindication_versus_restoration_ambiguity,
    'Does executing the murderer actually provide the moral ''vindication'' the theory claims for the victim and surviving family, or is this a psychologically and philosophically unverifiable claim that functions as rhetorical cover for a retributive impulse the theory does not actually satisfy in practice?',
    'Longitudinal studies of victims'' family members'' reported psychological outcomes pre- and post-execution, compared against outcomes in comparable non-capital cases (life sentences), to assess whether the claimed vindication/closure effect is empirically observed.',
    'If executions do not reliably produce the claimed vindication effect, the beneficiary designation for victims_surviving_family is weaker than authored, which would lower ε''s beneficiary-side justification and push the classification closer to snare (extraction without the claimed offsetting benefit materializing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vindication_versus_restoration_ambiguity, empirical, 'Whether the posthumous vindication and family closure claimed by the theory is empirically realized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.18).
narrative_ontology:measurement(stat_tr_t8, state_killing_authority__retributive_desert, theater_ratio, 8, 0.2).
narrative_ontology:measurement(stat_tr_t16, state_killing_authority__retributive_desert, theater_ratio, 16, 0.22).
narrative_ontology:measurement(stat_tr_t24, state_killing_authority__retributive_desert, theater_ratio, 24, 0.24).
narrative_ontology:measurement(stat_tr_t32, state_killing_authority__retributive_desert, theater_ratio, 32, 0.26).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stat_be_t8, state_killing_authority__retributive_desert, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(stat_be_t16, state_killing_authority__retributive_desert, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(stat_be_t24, state_killing_authority__retributive_desert, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(stat_be_t32, state_killing_authority__retributive_desert, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t8, state_killing_authority__retributive_desert, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(stat_su_t16, state_killing_authority__retributive_desert, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(stat_su_t24, state_killing_authority__retributive_desert, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(stat_su_t32, state_killing_authority__retributive_desert, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the state_killing_authority kernel. retributive_desert grounds justification in forfeiture-and-proportionality (deontological, non-empirical); deterrence_instrument grounds justification in a falsifiable consequentialist claim about crime prevention; categorical_abolition denies the forfeiture premise outright and forecloses this reading within any single framework. Each reading carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle — they are not measurement variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
