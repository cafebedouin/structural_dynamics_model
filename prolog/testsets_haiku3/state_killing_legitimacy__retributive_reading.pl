% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: State Execution Legitimacy: Retributive Desert Reading
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the RETRIBUTIVE READING of the contested
 *   kernel 'state_killing_legitimacy.' The retributive reading asserts that a
 *   murderer forfeits the right to life through proportional desert (lex
 *   talionis): intentional life-taking places the taker outside the moral
 *   order's protection, and the state acts as agent of proportional justice
 *   in executing the murderer. This reading vindicates proportional-justice
 *   doctrine and treats execution as legitimate moral response to murder. The
 *   claim is Tangled Rope (coordination of proportional justice + extraction
 *   from the condemned + victim family as dual beneficiary/payer). The
 *   measurement series show gradual increase in extraction (as procedural
 *   legitimacy comes under pressure) and theater ratio (as the justificatory
 *   narrative grows disconnected from empirical guilt-finding reliability).
 *   This reading coexists with deterrence and abolition readings as distinct
 *   constraint stories, each with its own ε, beneficiary/victim structure,
 *   and type.
 *
 * KEY AGENTS:
 *   - convicted_murderer — powerless, trapped, pays with life-right
 *   - victim_family — moderate power, constrained exit, benefits from closure narrative, pays through emotional/procedural burden
 *   - state_justice_apparatus — institutional agenda-setter, maintains the constraint through sentencing and execution
 *   - moral_order_vindication — institutional beneficiary (non-agent), the abstract entity whose legitimacy rides on proportional desert
 *   - exonerees/innocent_at_risk — powerless, trapped, pays if executed despite innocence
 *   - abolition_advocates — excluded, organized resistance to the entire reading's premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.68).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.71).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "State Execution Legitimacy: Retributive Desert Reading").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '9273bc97-b041-44b2-9e42-682ec2bf7209').
narrative_ontology:cs_kernel_codification('9273bc97-b041-44b2-9e42-682ec2bf7209', formalized).
narrative_ontology:cs_authority_grounding('9273bc97-b041-44b2-9e42-682ec2bf7209', lineage).
narrative_ontology:cs_interpretation_layer_present('9273bc97-b041-44b2-9e42-682ec2bf7209').
narrative_ontology:cs_reading_relation('9273bc97-b041-44b2-9e42-682ec2bf7209', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_reading_relation('9273bc97-b041-44b2-9e42-682ec2bf7209', state_killing_legitimacy__deterrence_reading, influences).
narrative_ontology:cs_axiom('9273bc97-b041-44b2-9e42-682ec2bf7209', foundational, murderer_forfeits_life_right_through_desert).
narrative_ontology:cs_axiom_status(murderer_forfeits_life_right_through_desert, holdable).
narrative_ontology:cs_axiom_grounding('9273bc97-b041-44b2-9e42-682ec2bf7209', murderer_forfeits_life_right_through_desert, deontological).
narrative_ontology:cs_axiom('9273bc97-b041-44b2-9e42-682ec2bf7209', secondary, state_as_agent_of_proportional_moral_order).
narrative_ontology:cs_axiom_status(state_as_agent_of_proportional_moral_order, holdable).
narrative_ontology:cs_axiom_grounding('9273bc97-b041-44b2-9e42-682ec2bf7209', state_as_agent_of_proportional_moral_order, conventional).
narrative_ontology:cs_reference_frame('9273bc97-b041-44b2-9e42-682ec2bf7209', proportional_desert_framework).
narrative_ontology:cs_drift_state('9273bc97-b041-44b2-9e42-682ec2bf7209', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9273bc97-b041-44b2-9e42-682ec2bf7209', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order_vindication).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victim_family_closure).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, state_legitimacy_apparatus).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, convicted_murderer).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, exonerees_at_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victim_family).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, victim_family).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, innocent_condemned).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, proportional_justice_doctrine).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, lex_talionis_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death by the state. Under this reading, the murderer has forfeited the right to life through the desert principle: by intentionally taking another's life, the murderer has placed themselves outside the moral order that protects life. The reading authorizes state killing as proportional response. Exit consists only of clemency or exoneration; both are contingent on powers external to the condemned.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, convicted_murderer, payer,
    powerless, immediate, trapped, national).

% Experiences the constraint as providing closure, vindication of the victim's value, and proportional response to the harm suffered. Also bears the cost of ongoing legal proceedings, testimony burdens, and the emotional weight of witnessing state killing carried out in the victim's name. May experience state killing as continuing rather than resolving trauma.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victim_family, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, victim_family, payer).

% Administers capital punishment and sets execution policy. Under the retributive reading, the state acts as agent of moral order, executing murderers to restore proportional balance. The apparatus maintains the constraint through sentencing, appeals oversight, and execution. Its authority to kill derives from the desert principle and the claim to represent collective moral judgment.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_justice_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The abstract beneficiary: proportional desert is vindicated when murderers forfeit life-right and the state executes accordingly. The moral order's legitimacy rides on the constraint's operation — its failure to execute would, on this reading, constitute moral failure of the justice system itself.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_order_institution, beneficiary,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__retributive_reading, moral_order_institution).

% Exonerees and those at genuine risk of execution despite factual innocence. The desert principle acknowledges moral culpability as the condition for forfeiting life-right, but the constraint's operation depends on accurate guilt determination. Factual innocence is no defense once the state has determined guilt and the execution is carried out. Exit consists only of exoneration before execution, which is contingent on post-conviction review mechanisms.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, innocent_condemned, payer,
    powerless, immediate, trapped, national).

% Reject the retributive reading's core premise: that state killing is categorically legitimate under any moral theory, including desert. They argue state killing violates human dignity regardless of culpability. They are excluded from the policy-setting process in retentionist jurisdictions, though they mount legal and political challenges to the constraint's application.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolition_advocates, excluded,
    organized, generational, constrained, national).

% Support capital punishment on grounds of crime prevention utility rather than desert. They occupy a different epistemic seat: deterrence reading justifies execution by future-oriented consequences; retributive reading justifies it by past moral fact (the murderer's forfeiture). They coexist as competing legitimating narratives but are distinct constraint framings.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, deterrence_justifiers, excluded,
    organized, generational, constrained, national).

% Appellate courts and post-conviction review bodies tasked with verifying that guilt determination meets constitutional standards. They measure whether the factual premise for desert (the accused actually committed murder with requisite mental state) has been reliably established. They see the constraint from the side of procedural legitimacy: does the state's guilt-finding process reliably identify murderers and exclude the innocent?
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, due_process_apparatus, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, state_justice_apparatus).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a coordinated legal framework that responds to homicide consistently through proportional punishment: murder (intentional life-taking) is met with proportional consequence (state-authorized death). Addresses the coordination problem of how a moral order responds to the violation of its core principle (the prohibition on murder).
% TRANSFER_FUNCTION: Transfers the life-right from the convicted murderer to the state, which exercises it as agent of proportional justice. The state kills the murderer to vindicate the moral order and the victim's value.
% ABSENT_VOICES: Exonerees and death-row innocence advocates would testify that accurate guilt determination is structurally unreliable; once execution occurs, innocence cannot be remedied. They are partially excluded from policy-setting in retentionist jurisdictions and their empirical claims about error rates meet structured resistance. Religious and philosophical opponents of capital punishment are also absent from policy forums in retentionist states, though they mount legal and political opposition.
% DISAPPEARANCE_RATIONALE: If state-authorized execution for murder disappeared overnight, the retributive reading would treat this as a collapse of proportional justice: murderers would retain the life-right they had forfeited through desert, the moral order would suffer a legitimacy crisis (by this reading's lights), and victim families would lose the proportional remedy this reading treats as their due. A retributive agent would treat this as moral catastrophe and move to restore executions. Deterrence and abolition agents would read the same disappearance differently (as beneficial or as vindication of prior abolitionist argument).
% FOUNDING_PROBLEM: How does a moral order respond when one of its members intentionally violates its foundational principle (the prohibition against murder)? The retributive reading answers: through proportional forfeiture of the same right the murderer violated in others — life for life. The founding problem is the legitimacy crisis posed by unpunished murder, where the victim's death and the murderer's continued life-right constitute simultaneous violations of proportional justice.
% FOUNDING_PROBLEM_CORROBORATION: Retributive philosophers (including contemporary proponents of desert-based punishment) attest that the founding problem is live: unpunished murder or inadequate punishment leaves a proportionality gap that justice requires be closed. Abolition and deterrence advocates attest that the founding problem is misconceived: either the state should never kill (abolition) or should kill only if proven necessary for crime prevention (deterrence), not because desert demands it. Empirical researchers on victim impact and closure testify that victim families experience the constraint variously — some report closure from execution, others report continued trauma from state killing carried out in their name. No voice outside the retributive framework attests that proportional desert is the binding legitimacy condition.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) is moderately high because the constraint transfers the condemned murderer's life-right to the state as payment for moral desert. The reading claims this is not extraction but legitimate proportional response; however, the extraction is measured from the murdered person's position (no moral claim justifies the taking of the murderer's life from the murderer's seat). Suppression (0.71) is high because the constraint requires active maintenance against abolition advocacy, procedural challenges, and the empirical evidence that guilt-finding is imperfect. Theater ratio (0.42) is moderate and rising: as DNA exonerations and statistical error analysis accumulate, the proportional-desert justification increasingly coexists with performative ritual (the state carrying out killing to maintain legitimacy narrative). The measurement series show extractiveness and suppression rising over the 50-year interval as opposition and empirical doubt mount, requiring increasing enforcement effort. Accessibility_collapse (0.72) is moderately high: once a person is sentenced to death under this reading, alternatives (exoneration, clemency) exist but are contingent on external intervention; the condemned person's own choice set is collapsed. Resistance (0.78) is high: abolition movements, innocence advocates, and religious and philosophical opposition provide substantial and organized resistance.
 *
 * PERSPECTIVAL GAP:
 *   Payer seat (condemned) vs. agenda-setter seat (state apparatus): The state apparatus and proportional-justice framework treat execution as legitimate proportional response. The condemned and exonerees treat it as state killing justified by post-hoc moral narrative. The constraint persists because the state holds enforcement power, not because the condemned consent to desert doctrine. This is a structural asymmetry: one party (state) maintains a system that legitimates extraction from another party (condemned) based on a moral premise (desert) the condemned may reject.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state, victim family): derive legitimacy and narrative closure from executing murderers according to desert principle. Victims (condemned, innocent-at-risk): lose life-right with no choice and cannot consent to or exit the desert premise. Excluded (abolitionists): resist the constraint's legitimacy claim itself, arguing state killing is categorically illegitimate regardless of desert. No seat has arbitrage-grade exit; all are constrained by state authority over execution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proportional response to murder) remains contested. Retributive philosophers argue it is live: murder without proportional response leaves a justice gap. Abolitionists argue it is dead: the constraint persists not because proportional desert is necessary but because retributive ideology legitimates state power. Deterrence proponents argue the founding problem is misframed: the real problem is crime prevention, not desert-matching. This mandatrophy contest (live vs. dead founding problem) is NOT resolved by the constraint's classification but is documented via the six_questions. Theater ratio rising over time suggests the constraint is maintained increasingly through performative ritual (solemn courtroom proceedings, formal appeals) rather than through genuine coordination of proportional justice. The measurement data show extractiveness and suppression both rising, indicating that enforcement effort is increasing even as the constraint's functional legitimacy is questioned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    desert_principle_bindingness,
    'Is proportional desert a binding legitimacy condition for state killing, or is it a post-hoc justification for killing motivated by other institutional interests (power, revenge, incapacitation)?',
    'Historical and comparative analysis of when retributive justifications are invoked, whether desert doctrine is applied consistently across crime categories and offender demographics, and whether state killing persists in jurisdictions that formally reject desert doctrine. If desert doctrine is selectively applied or invoked only when institutional interests align, the principle is contingent rather than binding.',
    'If desert is non-binding (post-hoc), the constraint reclassifies from Tangled Rope (genuine coordination function + extraction) toward Snare (extraction justified by contingent narrative). If desert is binding, the constraint''s classification as Tangled Rope holds: genuine coordination function (proportional justice) coexists with extraction from the condemned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_principle_bindingness, empirical, 'Whether proportional desert is the true legitimating principle or a legitimating narrative for institutional killing.').

omega_variable(
    guilt_determination_reliability,
    'Can capital punishment reliably identify murderers and exclude innocent persons, or is the constraint structurally dependent on executing some innocent people as acceptable cost of proportional justice?',
    'Meta-analysis of exoneration rates, DNA evidence reversals, error studies, and comparative data from jurisdictions with and without capital punishment. If error rates exceed the retributive reading''s tolerance for false positives, the constraint cannot reliably implement its own principle.',
    'If guilt determination is unreliable, the constraint systematically executes innocents under the guise of desert-matching. The innocent victims would need to be reclassified as structural payers (not accidental), and the constraint would show evidence of mandatrophy (founding problem dead: proportional justice is impossible if guilt is unreliable; constraint persists because state institutions collect power/legitimacy from execution regardless).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(guilt_determination_reliability, empirical, 'Whether capital punishment can reliably identify the guilty and exclude the innocent.').

omega_variable(
    reading_logically_forecloses_which_sibling,
    'Does the retributive reading logically foreclose (rule out) the abolition reading within a single moral framework, or do they merely coexist as incompatible positions held by different moral communities?',
    'Analytical reconstruction: does the retributive reading''s core premise (murderers forfeit life-right through desert) logically entail that state killing is obligatory or at least morally permissible? If so, it forecloses the abolition reading''s premise that state killing is categorically impermissible. If the retributive reading only asserts permissibility and abolition asserts impermissibility (one allows, one forbids), they coexist rather than foreclose.',
    'If retributive FORECLOSES abolition: the engine treats the readings as logically incompatible, foreclosure routing applies, and one reading''s falsification undermines the other''s framework (rare). If they COEXIST: both remain live despite fundamental disagreement; they are held by different parties and neither dissolves the other''s coherence. Coexistence is the expected outcome for readings grounded in different foundational axioms (desert vs. dignity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_logically_forecloses_which_sibling, conceptual, 'Whether the retributive and abolition readings logically foreclose each other or coexist as incompatible live positions.').

omega_variable(
    victim_family_directionality_ambiguity,
    'Does the victim family benefit from state execution of the murderer, or does execution compound trauma and prevent healing in ways the closure narrative obscures?',
    'Longitudinal qualitative research with victim families post-execution vs. post-life-sentence: do families report closure and proportional justice satisfaction, or ongoing trauma? Post-execution, do reported outcomes differ based on whether the family supported execution before it occurred?',
    'If families structurally benefit, victim_family remains beneficiary and the constraint''s extraction is asymmetrically borne by the condemned. If execution compounds trauma, victim_family should be reclassified as payer despite their role as beneficiary in the retributive narrative — they would then be dual extractors, paying for the state''s proportional killing in the name of their harm. This affects directionality computation and the classification of the constraint from the victim-family seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_family_directionality_ambiguity, empirical, 'Whether state execution provides genuine closure to victim families or compounds trauma despite closure rhetoric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__retributive_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__retributive_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__retributive_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__retributive_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(stat_tr_t40, observed).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__retributive_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(stat_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__retributive_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__retributive_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__retributive_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__retributive_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(stat_be_t40, observed).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__retributive_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(stat_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__retributive_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__retributive_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__retributive_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__retributive_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(stat_su_t40, observed).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__retributive_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(stat_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_legitimacy__retributive_reading, 0.12).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% The kernel 'state_killing_legitimacy' decomposes into three structurally distinct constraint stories: (1) RETRIBUTIVE_READING (this file): legitimate because murderer forfeits life-right through desert; high extraction justified by proportional justice; Tangled Rope. (2) DETERRENCE_READING: legitimate because execution prevents future murders; extraction justified by future utility; Tangled Rope (different beneficiary/victim structure and ε). (3) ABOLITION_READING: categorically illegitimate; state killing violates human dignity regardless of desert or utility; Snare (the state extracts justification for killing from a false premise that dignity can be forfeited). Each reading has its own ε, beneficiary/victim set, and type. They share the kernel (the standing commitment that state killing MAY be legitimate) and contest its interpretation. Network edges link them because a shift in one reading's empirical premises (e.g., empirical falsification of deterrence) affects the salience of sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_legitimacy__retributive_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
