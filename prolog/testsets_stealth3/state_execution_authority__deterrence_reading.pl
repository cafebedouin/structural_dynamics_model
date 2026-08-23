% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__deterrence_reading, []).

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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority — Deterrence Reading (Capital Punishment as Prospective Cost-Raising)
 *   domain: criminal justice/political philosophy/constitutional law
 *
 * SUMMARY:
 *   A retentionist jurisdiction maintains capital statutes whose stated
 *   warrant — instantiated in this file — is forward-looking: execution
 *   raises the expected price of premeditated killing above what permanent
 *   imprisonment prices it at, purchasing protection for people who are not
 *   yet victims. The ε referent is the standing arrangement under contest —
 *   the operating capital-punishment system itself (statutes, capital trials,
 *   death row, the appeals apparatus, delivered executions) — assessed by
 *   this reading's own lights: the condemned offender's death is booked as
 *   the priced demonstration cost, the wrongfully convicted defendant's death
 *   is booked as pure utilitarian loss, and each statistically credited
 *   prevented murder is the return on the expenditure. The claim/metric gap
 *   is deliberate and load-bearing: the reading CLAIMS the arrangement as
 *   justified protective coordination with identified payers through the same
 *   structure (tangled rope), while the metrics describe what forty-eight
 *   years of operation actually show — moderate, slowly rising extraction,
 *   heavy coercive machinery, and a growing expressive share. The engine
 *   measures that divergence; the claim is not reconciled to the metrics. KEY
 *   AGENTS (by structural relationship): - capital_punishment_states:
 *   Agenda-setter (institutional/constrained) — administers statutes,
 *   captures political capital and prosecutorial leverage -
 *   capital_case_appeals_system: Secondary agenda-setter
 *   (institutional/constrained) — runs the error-minimization layer that sets
 *   the realized error rate - future_potential_victims: Primary beneficiary
 *   (powerless/trapped) — receives diffuse, unverifiable protection -
 *   survivor_families_supporting_sentences: Beneficiary
 *   (organized/constrained) — receives expressive closure, supplies political
 *   demand - convicted_capital_offenders: Primary payer (powerless/trapped) —
 *   bears the terminal priced cost - wrongfully_convicted_death_row_inmates:
 *   Payer (powerless/trapped) — bears the reading's own acknowledged loss
 *   term - families_of_executed_persons: Payer (powerless/trapped) — absorbs
 *   unpriiced collateral costs - taxpayers_in_retentionist_states: Payer
 *   (moderate/constrained) — funds the capital-over-life-imprisonment premium
 *   - habeas_and_postconviction_counsel: Resistance seat (organized/mobile) —
 *   litigates the error record case by case -
 *   international_human_rights_bodies: Excluded voice (institutional/trapped)
 *   — objects from outside the domestic process - deterrence_researchers:
 *   Analytical observer (analytical/analytical) — owns the efficacy evidence
 *   the warrant rides on
 *
 * KEY AGENTS:
 *   - capital_punishment_states: agenda-setter (institutional/constrained) — enacts, administers, and politically defends the arrangement; captures its gains
 *   - capital_case_appeals_system: secondary agenda-setter (institutional/constrained) — appellate and clemency machinery determining the realized error rate
 *   - future_potential_victims: primary beneficiary (powerless/trapped) — holders of the protection the raised price purchases
 *   - survivor_families_supporting_sentences: beneficiary (organized/constrained) — expressive-closure recipients and political-demand suppliers
 *   - convicted_capital_offenders: primary payer (powerless/trapped) — bearers of the terminal demonstration cost
 *   - wrongfully_convicted_death_row_inmates: payer (powerless/trapped) — bearers of the utilitarian loss term
 *   - families_of_executed_persons: payer (powerless/trapped) — collateral cost bearers with no procedural standing
 *   - taxpayers_in_retentionist_states: payer (moderate/constrained) — funders of the capital premium over life imprisonment
 *   - habeas_and_postconviction_counsel: resistance seat (organized/mobile) — error-record litigators
 *   - international_human_rights_bodies: excluded (institutional/trapped) — external critics barred from the domestic process
 *   - deterrence_researchers: analytical observer (analytical/analytical) — producers of the warrant-condition evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.55).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority — Deterrence Reading (Capital Punishment as Prospective Cost-Raising)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal justice/political philosophy/constitutional law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, 'd4a42c82-9e6e-434b-9708-5346cb4e6824').
narrative_ontology:cs_kernel_codification('d4a42c82-9e6e-434b-9708-5346cb4e6824', fixed_text).
narrative_ontology:cs_authority_grounding('d4a42c82-9e6e-434b-9708-5346cb4e6824', lineage).
narrative_ontology:cs_interpretation_layer_present('d4a42c82-9e6e-434b-9708-5346cb4e6824').
narrative_ontology:cs_reading_relation('d4a42c82-9e6e-434b-9708-5346cb4e6824', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4a42c82-9e6e-434b-9708-5346cb4e6824', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('d4a42c82-9e6e-434b-9708-5346cb4e6824', foundational, execution_justified_solely_by_deterrent_effect).
narrative_ontology:cs_axiom_status(execution_justified_solely_by_deterrent_effect, holdable).
narrative_ontology:cs_axiom_grounding('d4a42c82-9e6e-434b-9708-5346cb4e6824', execution_justified_solely_by_deterrent_effect, empirically_contingent).
narrative_ontology:cs_axiom('d4a42c82-9e6e-434b-9708-5346cb4e6824', secondary, wrongful_execution_counts_as_net_loss).
narrative_ontology:cs_axiom_status(wrongful_execution_counts_as_net_loss, holdable).
narrative_ontology:cs_axiom_grounding('d4a42c82-9e6e-434b-9708-5346cb4e6824', wrongful_execution_counts_as_net_loss, instrumental).
narrative_ontology:cs_reference_frame('d4a42c82-9e6e-434b-9708-5346cb4e6824', effective_deterrence_equilibrium).
narrative_ontology:cs_drift_state('d4a42c82-9e6e-434b-9708-5346cb4e6824', post_dna_exoneration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d4a42c82-9e6e-434b-9708-5346cb4e6824', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, survivor_families_supporting_sentences).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, capital_punishment_states).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, convicted_capital_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, families_of_executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, taxpayers_in_retentionist_states).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, marginal_deterrence_hypothesis).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, rational_choice_deterrence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures enact capital statutes, governors sign warrants, prosecutors seek death sentences as the top of the sentencing ladder, and corrections departments carry out executions. Continuation of the arrangement yields political capital for tough-on-crime positioning and a maximum-pressure lever in plea negotiations; the same states bear the budgetary weight of the appeals-and-death-row apparatus and reputational costs internationally. Statutory repeal is procedurally available in a single legislative session and has been exercised by peer jurisdictions.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, capital_punishment_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__deterrence_reading, capital_punishment_states, beneficiary).

% Multi-tier direct appeal, post-conviction habeas review, and clemency boards constitute the error-minimization layer of the arrangement. This apparatus decides which death sentences survive, absorbs most of the system's cost and delay, and effectively sets the realized error rate that the arrangement's own accounting must book. Courts cannot resign from jurisdiction; shrinking the review layer is itself a policy act by the states.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, capital_case_appeals_system, agenda_setter,
    institutional, generational, constrained, national).

% People currently alive whose exposure to premeditated killing the raised-price claim protects. They receive the benefit invisibly and counterfactually — no receipt is ever issued for a murder that did not occur — and cannot organize around a protection they cannot verify. No one can opt out of the risk class.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_victims, beneficiary,
    powerless, biographical, trapped, national).

% Relatives of murder victims who campaign for death sentences, attend proceedings, and witness executions. When sentences are carried out they receive expressive closure and assurance; their advocacy organizations supply durable political demand that sustains the statutes. Their grief-stake persists regardless of advocacy, so disengagement from the arrangement is partial at best.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, survivor_families_supporting_sentences, beneficiary,
    organized, biographical, constrained, national).

% Persons sentenced to death for completed capital crimes. They bear the arrangement's terminal cost: years of death-row confinement followed, in a subset of cases, by execution. In this reading's accounting their deaths are the priced demonstration that maintains the raised cost of capital crime. Appeals either exhaust (carrying the sentence through) or succeed (relief within the arrangement, not exit from it). Death-row conditions sever ordinary social ties and coalition capacity.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, convicted_capital_offenders, payer,
    powerless, biographical, trapped, national).

% Persons under death sentences for crimes they did not commit. Some are discovered by exoneration; an unknown remainder are undiscovered. They bear exactly the same treatment as the guilty-condemned until discovery separates the classes. In this reading's own ledger their deaths are pure utilitarian loss — the term the reading explicitly books and builds the appeals apparatus to minimize.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, biographical, trapped, national).

% Kin of condemned and executed persons. They absorb collateral costs the sentencing decision never prices: trauma, public stigma, legal debt from retained post-conviction counsel, and loss of household earners. They hold no formal standing anywhere in the capital process that determines their exposure.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, families_of_executed_persons, payer,
    powerless, biographical, trapped, national).

% Residents of retentionist jurisdictions who fund the premium of capital punishment over life imprisonment: decades of specialized appeals, enhanced death-row confinement, and execution logistics. The premium recurs annually; redirecting it requires electing repeal-minded legislators or relocating to an abolitionist jurisdiction.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, taxpayers_in_retentionist_states, payer,
    moderate, biographical, constrained, national).

% Attorneys litigating death-row cases — building the exoneration record, delaying execution dates case by case, and generating the error data the system's self-accounting depends on. Paid from public defender funds or pro bono networks; they neither administer the arrangement nor fund it, and can move their practice to other work or other states.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, habeas_and_postconviction_counsel, observer,
    organized, biographical, mobile, national).

% UN treaty bodies and regional human-rights institutions that classify capital punishment as a rights violation and press retentionist states through reviews, resolutions, and diplomatic channels. They are barred from any seat in domestic sentencing processes; their objection enters the arrangement only as external pressure that domestic proponents can discount.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, international_human_rights_bodies, excluded,
    institutional, generational, trapped, global).

% Criminologists and economists estimating whether execution reduces murder rates beyond the level permanent imprisonment achieves, using matched panels, interrupted time series, and syntheses. They hold no stake in any statute, and the validity of this arrangement's warrant rests on findings produced at seats like theirs.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, deterrence_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, capital_punishment_states).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prices premeditated killing at the maximum sanction the state can impose, intending to tip potential offenders' calculations away from capital crime, and anchors a terminal tier atop the sentencing ladder against which plea offers and lesser sentences are calibrated.
% TRANSFER_FUNCTION: Moves the ultimate sanction — life itself — out of convicted capital offenders, nominally toward the protection of future potential victims; at the realized error rate it also takes life from wrongly convicted defendants, an unbudgeted transfer this reading explicitly books as loss. Secondarily it moves recurring public funds from taxpayers into the enlarged appeals-and-confinement apparatus, and political capital toward officeholders associated with the sanction.
% ABSENT_VOICES: Executed persons are removed from every subsequent conversation by design; the undiscovered wrongfully executed are absent by definition and can never testify; international human-rights bodies and abolition advocates stand outside retentionist jurisdictions' legislative processes; future potential victims cannot yet speak for the protection claimed on their behalf.
% DISAPPEARANCE_RATIONALE: By this reading's own prediction, overnight disappearance of the execution premium would degrade the extra price signal protecting potential victims — a rearrangement. Yet the same reading concedes that if life-without-parole prices the crime identically, repeal merely swaps the carrier of the terminal tier and little rearranges. Even internally the verdict therefore hangs on the unresolved efficacy comparison, which is why the verdict is contested rather than asserted.
% FOUNDING_PROBLEM: Early modern states needed a sanction able to outweigh any gain a rational actor could expect from premeditated killing, and a publicly visible terminal act marking the community's outermost boundary.
% FOUNDING_PROBLEM_CORROBORATION: The problem is corroborated from outside the benefiting parties: national research-council syntheses and successive criminological panels attest both that homicidal offending persists (problem live) and that whether execution discharges it better than permanent imprisonment remains empirically open. Commission records from repealing jurisdictions document the same persistence alongside abandonment of the execution remedy — corroborating the problem while disputing this reading's answer to it.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, contested).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon (0.55 at interval end) is authored over the fixed referent — the standing capital-punishment arrangement — by this reading's lights: guilty-condemned deaths count as purchased demonstration cost, wrongfully-convicted deaths count as pure loss, credited prevented murders count as return. The value is moderate rather than low because both the error term and the contested efficacy term weigh against the reading's own accounting, and it rises across the interval as DNA-era exonerations made the error class visible and panel syntheses pushed deterrence estimates toward null. Suppression (0.72) is authored as raw, unscaled structure: a terminal sanction cannot be enforced softly — softening the machinery is repeal, not moderation — so death-row isolation, drug and procedure secrecy protocols, and appeals exhaustion carry high coercive force per case. Theater (0.44 at end) tracks the expressive share: as deliveries became rarer and more ceremonial while efficacy evidence thinned, a growing fraction of activity functions as political signaling and periodic demonstration rather than calibrated deterrence delivery. Accessibility_collapse (0.40) is honest: the life-without-parole alternative is fully visible and available; understanding this constraint invites substitution rather than foreclosing it. Resistance (0.60): sustained abolition movements, moratoria campaigns, litigation waves, and religious/international opposition meet the arrangement continuously without displacing it in retentionist cores. Claimed type tangled_rope is authored from structure — a real protective-coordination half conditional on efficacy, identifiable payers bearing costs through the same structure, and active enforcement — independently of the metric values, per the claim/metric independence rule. Temporal series share one grid (t = 0,8,…,48 years, mapping approximately to 1976–2024, the post-reinstatement era to the present), and the dynamics are partly cyclical rather than monotonic: expansion phases follow salient murders (demand spikes), retrenchment follows botched executions and exoneration scandals. The oscillation is partly the mechanism itself — each crisis-driven execution doubles as the periodic public demonstration this reading's deterrence theory requires — so the series was authored at cycle-neutral phase midpoints rather than at crisis peaks.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. The agenda-setting state seat experiences the arrangement as coordination it deliberately built and defends; its effective extraction is damped toward subsidy because it writes the statutes and captures the political returns. The appeals apparatus experiences the same structure as an error-management burden whose output — the realized error rate — is the variable the whole reading's legitimacy hangs on. The payer seats experience terminal extraction: the condemned bear the priced cost and the wrongfully condemned bear the loss term, and the two classes are structurally identical in power, exit, and treatment at sentencing, differing only in an attribute (factual guilt) the apparatus cannot observe at decision time — the deepest same-level divergence in this story. Future potential victims occupy a beneficiary seat they can never verify: their protection is counterfactual and unorganizable, so the benefit arrives without receipts. Coalition note: the powerless payer classes are kept coalition-incapable by design — death-row isolation severs inmate coordination — leaving families and taxpayers to coalize externally with abolition movements, which is where the observed resistance actually originates.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives d without overrides: future_potential_victims and survivor_families_supporting_sentences sit at the beneficiary pole (extraction damped or inverted into subsidy for them); convicted_capital_offenders, wrongfully_convicted_death_row_inmates, families_of_executed_persons, and taxpayers sit at the target pole, with the trapped exit pushing the death-row classes toward the full-target end; capital_punishment_states sits low-mid (an administrator that also captures gains — beneficiary-adjacent but still bearing the apparatus's costs); capital_case_appeals_system sits mid (administers without collecting); international_human_rights_bodies is excluded rather than positioned; deterrence_researchers hold the analytical seat. No directionality_overrides are authored: derivation from declared roles plus exit options already separates the seats correctly, and override keys bind to power atoms — an override on the shared institutional atom would smear three institutionally-seated agents with opposed structural relationships (administrator-capturer, error-manager, excluded critic) onto one value.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a sanction that outranks any gain from premeditated killing — remains live: murder persists and sentencing ladders still need a top rung, so the dead-problem zombie flag should not fire from status alone. The dangerous mismatch is the other diagonal: if the efficacy omega resolves to null and error rates resolve high, founding_problem_status flips contested while disappearance_verdict stays world_rearranges (the apparatus, its litigation economy, and its political economy all persist independently of the warrant) — precisely the capture/zombie signature the consumer cross-checks against the computed piton/theater path. Typing the arrangement as tangled_rope rather than snare keeps the reading's genuine protective half on the books (erasing it would misread a conditional-function constraint as pure cover); refusing rope keeps the identified payers visible (erasing them would launder the wrongful-execution class as mere coordination cost). Piton is avoided while statutes actively deliver sentences; the theater series is the leading indicator of atrophy — if deliveries cease and ceremonies persist, the piton transition goes live and mandatrophy resolution becomes mandatory rather than advisory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the deterrence_reading of kernel state_execution_authority. Which structural elements would change if a sibling reading were instantiated instead?',
    'Not resolvable by data inside this framework: resolution consists of adopting a different reading. Abolition_reading deletes future_potential_victims from the beneficiary set, voids the protective credit entirely, and recomputes every execution as uncompensated extraction; retributive_reading replaces the prospective-victim benefit with retrospective moral-balance restoration and recasts the offender''s death from instrumental demonstration cost to constitutive payment, changing which transfers the reading books as legitimate.',
    'Classification is reading-relative: the same statutes compute as a moderate-extraction hybrid here, as near-total uncompensated extraction under the abolition seat, and as a differently weighted exchange under the retributive seat. Cross-reading comparison of epsilon is invalid unless the referent fix (standing arrangement, reading-indexed assessment) is respected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings of the state-execution kernel; the disagreement is located in the beneficiary set and the warrant condition, not in the physical acts.').

omega_variable(
    marginal_deterrent_effect,
    'Does execution produce a marginally larger reduction in murder rates than life-without-parole, holding other factors constant?',
    'Matched-panel designs across retention and abolition episodes, interrupted time-series at commutation events, and research-council-style syntheses of the deterrence literature.',
    'If the marginal effect is null, the arrangement''s counted benefit collapses, epsilon for the standing arrangement rises sharply, and the computed type drifts snare-ward; a robustly positive effect lowers epsilon and stabilizes the protective-coordination half of the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_deterrent_effect, empirical, 'The empirical warrant condition of the reading — the claim the entire classification leans on.').

omega_variable(
    realized_error_rate,
    'What fraction of death sentences are imposed on factually innocent persons, counting undiscovered as well as exonerated cases?',
    'Posthumous audit studies, exoneration-survival statistical models bounding the hidden class, and comparative error tracking between capital and non-capital homicide convictions.',
    'Each increment of undetected error adds uncompensated loss that this reading''s own ledger must book; high rates push epsilon upward, inflate the loss term, and erode the cost-benefit the warrant rests on, accelerating drift toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(realized_error_rate, empirical, 'Size of the innocent-payer class embedded in the arrangement — the reading''s self-declared loss term.').

omega_variable(
    lwop_substitutability,
    'Is permanent imprisonment a functional substitute for execution as the terminal pricing act, or does execution carry an irreducibly distinct signaling weight that imprisonment cannot replicate?',
    'Natural experiments from jurisdictions converting between death and commutation regimes, plus offender-perception studies of sanction salience at the decision margin.',
    'Full substitutability makes the execution premium separable excess cost removable without losing the coordination function; irreducible distinctness converts part of the measured extraction into the unavoidable price of the coordination itself, changing where the rope/snare boundary sits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lwop_substitutability, empirical, 'Whether the constraint''s pricing function is separable from its lethal carrier — the substitution question the reading itself flags as decisive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__deterrence_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__deterrence_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__deterrence_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__deterrence_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(stat_tr_t48, state_execution_authority__deterrence_reading, theater_ratio, 48, 0.44).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__deterrence_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__deterrence_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__deterrence_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__deterrence_reading, base_extractiveness, 32, 0.52).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(stat_be_t48, state_execution_authority__deterrence_reading, base_extractiveness, 48, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__deterrence_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__deterrence_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__deterrence_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__deterrence_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(stat_su_t48, state_execution_authority__deterrence_reading, suppression_requirement, 48, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel state_execution_authority: the colloquial label 'capital punishment' conflates three structurally distinct warrants. This file (deterrence_reading) authors the prospective cost-raising claim: beneficiaries include future potential victims, the offender's death is instrumental cost, and epsilon is keyed to the efficacy and error-rate terms. state_execution_authority__retributive_reading authors the retrospective proportional-payment claim: the beneficiary is restored moral order and the offender's death is constitutive. state_execution_authority__abolition_reading authors the categorical-impermissibility claim: no beneficiary exists and every execution is uncompensated extraction. Each reading carries its own epsilon, beneficiary set, and classification per the epsilon-invariance principle; the files are linked pairwise because they cite the same statutes and compete over the same cases. Upstream/downstream: the deterrence and retributive readings jointly sustain retention against the abolition challenge, and the deterrence reading's empirically contingent axiom is the one evidence can move — which is why abolition pressure characteristically routes through efficacy findings rather than directly against the retributive seat.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
