% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__deterrence_reading, []).

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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: Capital Punishment Justified as Marginal Deterrence Signal
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the deterrence reading of the
 *   state-killing-legitimacy kernel: the claim that a state's deliberate
 *   killing of a convicted murderer is legitimate if and insofar as it
 *   functions as a rational signal that raises the expected price of murder
 *   and thereby prevents future killings. The constraint under classification
 *   is the standing arrangement — capital statutes administered in
 *   retentionist jurisdictions and publicly justified in deterrence terms —
 *   assessed by this reading's own lights. Its epsilon referent is that
 *   standing arrangement, not the abolitionist alternative this reading
 *   argues against and not a hypothetical perfectly calibrated regime. Under
 *   the reading's own evaluative standard, each execution takes a life as a
 *   means toward an aggregate benefit whose existence is the reading's own
 *   load-bearing empirical premise; because that premise is contested by the
 *   best available external review, the arrangement carries
 *   moderate-to-substantial extraction: real deaths borne by identifiable,
 *   non-consenting individuals against a promissory, unverified protective
 *   return. Sibling readings (retributive, abolition) are separate constraint
 *   stories with their own epsilon values and victim sets; they are linked
 *   through the network, not folded into this one. The claim and the metrics
 *   are independent authored facts: tangled_rope is asserted from structure
 *   (a real coordination of the punitive apparatus plus asymmetric life-costs
 *   plus continuous enforcement), while the metrics describe observed
 *   operation.
 *
 * KEY AGENTS:
 *   - - condemned_offenders: primary target (powerless/trapped) — their execution is the instrument of the signal
 *   - - wrongfully_convicted_capital_defendants: irreducible-error targets (powerless/trapped) — the false-positive class priced into the protective promise
 *   - - families_of_executed_prisoners: collateral bearers (moderate/constrained) — absorb losses the aggregate calculus does not register
 *   - - potential_future_murder_victims: declared beneficiary class (powerless/generational) — protection is promissory pending the deterrence question
 *   - - elected_officials_and_prosecutors: agenda-setters and political collectors (institutional/mobile) — enact, enforce, and campaign on the arrangement
 *   - - appellate_review_courts: procedural referees (institutional/analytical) — define the lawful envelope, collect and pay nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.58).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "Capital Punishment Justified as Marginal Deterrence Signal").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'd5e1ac41-dd5c-436b-9e46-5991ed19382c').
narrative_ontology:cs_kernel_codification('d5e1ac41-dd5c-436b-9e46-5991ed19382c', formalized).
narrative_ontology:cs_authority_grounding('d5e1ac41-dd5c-436b-9e46-5991ed19382c', expertise).
narrative_ontology:cs_interpretation_layer_present('d5e1ac41-dd5c-436b-9e46-5991ed19382c').
narrative_ontology:cs_reading_relation('d5e1ac41-dd5c-436b-9e46-5991ed19382c', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5e1ac41-dd5c-436b-9e46-5991ed19382c', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('d5e1ac41-dd5c-436b-9e46-5991ed19382c', foundational, execution_permissible_only_if_marginally_deterrent).
narrative_ontology:cs_axiom_status(execution_permissible_only_if_marginally_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('d5e1ac41-dd5c-436b-9e46-5991ed19382c', execution_permissible_only_if_marginally_deterrent, empirically_contingent).
narrative_ontology:cs_axiom('d5e1ac41-dd5c-436b-9e46-5991ed19382c', foundational, individual_life_rights_aggregable_against_lives_saved).
narrative_ontology:cs_axiom_status(individual_life_rights_aggregable_against_lives_saved, holdable).
narrative_ontology:cs_axiom_grounding('d5e1ac41-dd5c-436b-9e46-5991ed19382c', individual_life_rights_aggregable_against_lives_saved, instrumental).
narrative_ontology:cs_axiom('d5e1ac41-dd5c-436b-9e46-5991ed19382c', secondary, unverified_punishment_loses_legitimacy).
narrative_ontology:cs_axiom_status(unverified_punishment_loses_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d5e1ac41-dd5c-436b-9e46-5991ed19382c', unverified_punishment_loses_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('d5e1ac41-dd5c-436b-9e46-5991ed19382c', rational_choice_sanction_calibration).
narrative_ontology:cs_drift_state('d5e1ac41-dd5c-436b-9e46-5991ed19382c', post_nrc_committee_assessment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d5e1ac41-dd5c-436b-9e46-5991ed19382c', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_murder_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, elected_officials_and_prosecutors).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, wrongfully_convicted_capital_defendants).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, families_of_executed_prisoners).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, rational_choice_criminology).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, marginal_deterrence_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A person convicted of capital murder in a jurisdiction whose statutes are publicly defended on the ground that executions prevent future killings. After sentence, they spend years in specialized housing under a warrant schedule they cannot influence; the state's announced reason for the final act is the lesson it teaches others. Once sentence is final there is no route out of the category; clemency is discretionary and rare.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, condemned_offenders, payer,
    powerless, immediate, trapped, national).

% Members of the false-positive class every capital system produces: people convicted and sentenced for killings they did not commit. Their deaths are the operational cost the protective promise is calculated against. Discovery typically arrives through volunteered evidence or luck rather than the system's own checks, and for some it arrives after the warrant has been carried out.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, wrongfully_convicted_capital_defendants, payer,
    powerless, biographical, trapped, national).

% Parents, children, and spouses of the executed. They lose a family member to the state's act, absorb stigma and economic disruption, and grieve without the social recognition accorded other bereaved. Their objections register nowhere in the accounting that weighs aggregate lives saved against individual deaths; some organize publicly, most endure privately.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, families_of_executed_prisoners, payer,
    moderate, biographical, constrained, national).

% The class the arrangement's protective promise names: everyone who would be murdered if the signal failed. They receive nothing observable unless the marginal-prevention premise holds, cannot opt into or out of the protection, and cannot verify the promise. Their interest is invoked on every side of the debate; their actual exposure is changed by nothing they do.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_murder_victims, beneficiary,
    powerless, generational, constrained, national).

% Governors, legislators, attorneys general, and district attorneys who enact the statutes, decide which charges carry death eligibility, seek the sentences, and sign or refuse the warrants. Executions generate campaign material, plea leverage, and demonstration-of-resolve credentials. They can reverse position and survive politically — several prominent former supporters have done exactly that — so leaving the arrangement is available to them in a way it is not to anyone below.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, elected_officials_and_prosecutors, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, elected_officials_and_prosecutors, beneficiary).

% State and federal judges who define what procedures make a death sentence lawful, hear the appeals, and periodically narrow or widen the practice through doctrine. They take testimony and evidence from every other seat, commission nothing and collect nothing, and their rulings are the main channel through which external scrutiny reaches the arrangement.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, appellate_review_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__deterrence_reading, elected_officials_and_prosecutors).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__deterrence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's response to its gravest crime around a single maximal sanction, drawing a bright public line at murder and organizing charging, sentencing, review, and clemency around it; purportedly also coordinates private incentives by raising the expected price of killing.
% TRANSFER_FUNCTION: Moves the condemned person's remaining life — and the years of conditioned waiting preceding it — from the offender to the state's punitive account, where it is converted into a public signal, promised security for third parties, and electoral and prosecutorial capital.
% ABSENT_VOICES: The executed are permanently absent — once carried out, the arrangement's harshest outcome silences its own chief witnesses. Exonerees who came within days of execution are rarely seated in retentionist legislatures' hearings. Capital defendants' home communities (poor, disproportionately Black in US practice) are thinly represented in the bodies that vote the statutes. Surviving families of murder victims are present but split, and the dissenting fraction is routinely sidelined.
% DISAPPEARANCE_RATIONALE: Death rows, capital litigation machinery, clemency offices, prosecutor charging strategies built around death eligibility, and political coalitions organized around the sanction would all dissolve or repurpose overnight; the condemned population transfers to life-without-parole housing; the signal the arrangement sends would be replaced by whatever the next-highest sanction communicates. Several jurisdictions have already run this rearrangement as a live test.
% FOUNDING_PROBLEM: The sovereign's oldest punitive problem: how to answer the gravest interpersonal violence with a sanction severe enough to demonstrate resolve, deter imitation, and close the gap left when lesser penalties failed to protect the public.
% FOUNDING_PROBLEM_CORROBORATION: The underlying problem (murder's persistence) is attested universally; whether THIS arrangement still serves it is attested by no one outside the benefiting parties with a settled affirmative. The National Research Council's 2012 committee — external to the benefiting coalition — concluded the existing deterrence literature is uninformative about marginal effects; state study commissions and criminological associations reach compatible agnostic or negative conclusions. Prosecutors' associations and victims'-rights organizations, inside the benefiting coalition, attest continued necessity. The strongest external verdict is agnosticism, which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: each execution takes a life as a means to a return this reading itself concedes is empirically unverified; the wrongful-conviction class is irreducible, and the political collection layered on top is documented. Suppression 0.72: persistence rides entirely on state coercive machinery — statutes, warrants, custody, execution protocols — with zero participant exit anywhere below the official seats. Theater 0.34: the killing is real, but a growing share of surrounding activity is signaling — last-statement rituals, witness protocols, announcement cycles, campaign imagery — and the signaling share grows as the evidentiary foundation erodes. Accessibility_collapse 0.42: the principal alternative (life imprisonment without parole) demonstrably operates across abolitionist jurisdictions, so understanding the arrangement does not close off alternatives; the world runs fine without it in much of its territory. Resistance 0.62: sustained abolition movements, innocence infrastructure, international treaty pressure, and religious bodies keep continuous pressure on the practice. The temporal series runs on one shared six-point grid across all three tracked metrics. Suppression_requirement is authored deliberately because this story tracks enforcement-capacity change: machinery build-up through the middle of the interval (procedural hardening, curtailed review) followed by capacity decay (litigation attrition, lethal-injection drug supply collapse) — a build-and-decay arc, not a cycle. Coalition note: the payer seats are individually powerless but have produced effective coalitions (exoneree networks, survivors' organizations opposing the practice), which is why resistance sits at 0.62 despite the power profile of the individual seats.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute the arrangement as an enforced taking of life with an unverifiable return: trapped exit amplifies their effective burden to near the full-target end. The official seat computes the same structure as a policy instrument it controls, administers, and visibly profits from: mobile exit and beneficiary position damp its burden toward the subsidy end. The judicial seat computes it as a procedurally bounded practice it referees without collecting from. Identity-lock dynamics operate on the official seat: for prosecutors and governors, professional and electoral identity fuses with capital enforcement, so structural mobility overstates felt exit — reversal reads as self-repudiation. When that identity frame breaks (as it has for several former supporters who became opponents), the official seat's maintenance contribution thins quickly, which is visible in the late-interval suppression decay.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map directly: condemned_offenders, wrongfully_convicted_capital_defendants, and families_of_executed_prisoners sit at the target end (high d), with trapped or constrained exits amplifying effective burden. potential_future_murder_victims derive a beneficiary-end d from their declaration, but their benefit is contingent on the unresolved marginal-prevention premise — a directional override toward symmetric was considered and rejected because overrides key on power_atom, and the powerless atom is shared with the genuine payer seats, so the override would wrongly damp their burden. The contingency is routed to the marginal_deterrence_effect omega instead. elected_officials_and_prosecutors combine beneficiary position with mobile exit, giving a low d; their collection is political, documented, and survives even if the protective return is null — which is why the receipt surface names them rather than the promissory beneficiary class. appellate_review_courts take the analytical treatment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (answering the gravest violence) is contested, not dead — murder persists — so the arrangement cannot be flagged as a zombie on status alone; the mismatch consumer watches status=contested crossed with verdict=world_rearranges. The classification prevents mislabeling in both directions. It refuses certification as pure coordination on the strength of the arrangement's own promissory claim: the extraction half is structural (identifiable payers, concentrated political collection) and does not wait on the empirical question. It equally refuses certification as pure extraction while the coordination question remains open: if the marginal-deterrence omega resolves positive, part of the measured burden re-prices as the cost of genuine protective coordination; if it resolves null, the coordination half collapses and the arrangement migrates toward extraction maintained by cover story. The rising theater_ratio series is the early-warning signature of that second path — signaling growing as verification recedes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_deterrence_effect,
    'Does execution produce a marginal reduction in homicide relative to the next-best sanction (life imprisonment without parole), or is the deterrent increment statistically indistinguishable from zero?',
    'Matched-jurisdiction natural experiments around abolition and reinstatement events, analyzed to National Research Council standards that avoid the aggregation biases the 2012 committee identified in the prior literature.',
    'A null result dissolves this reading''s justification from within: the arrangement loses its coordination half and migrates toward pure extraction maintained by cover story. A robust positive result re-prices part of the measured extraction as the cost of the protective coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_deterrence_effect, empirical, 'Whether the signal actually prevents murders at the margin.').

omega_variable(
    brutalization_counterhypothesis,
    'Do executions increase subsequent homicide (brutalization), reversing the sign of the arrangement''s protective output?',
    'Replication of state-panel and event-study designs with pre-registered specifications separating execution-period months from baseline enforcement levels.',
    'Confirmation would make the arrangement anti-coordinative — burden yielding negative social return — collapsing any residual coordination characterization and strengthening the reading that persistence is maintained by political collection rather than protective function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brutalization_counterhypothesis, empirical, 'Possible sign reversal in the arrangement''s protective output.').

omega_variable(
    kernel_reading_contestation,
    'Which reading of the state_killing_legitimacy kernel governs the legitimacy assessment of the same executions — this deterrence reading, the retributive reading (death owed as desert), or the abolition reading (categorical violation)?',
    'Normative-political settlement (constitutional, legislative, or doctrinal), not empirical data; the corpus stores each reading as its own story precisely because no dataset resolves it.',
    'Under the abolition reading the same conduct''s epsilon approaches its ceiling regardless of efficacy; under the retributive reading epsilon drops sharply if desert is granted. Classification of identical acts is reading-indexed; cross-reading comparison must join on the kernel, never merge the stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame ambiguity: one kernel, three readings, three constraints.').

omega_variable(
    wrongful_execution_error_tolerance,
    'What false-positive rate in capital conviction is compatible with using convicted persons as means to aggregate safety — and who has standing to set it?',
    'Deliberative preference articulation; no dataset settles it. Revealed tolerance (documented exonerations per execution) supplies descriptive input only.',
    'Sets the floor beneath which this reading''s own lights cannot push epsilon: a zero-tolerance answer makes every wrongful execution uncompensable and raises the arrangement''s burden accordingly; a nonzero answer prices part of the extraction in advance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wrongful_execution_error_tolerance, preference, 'Acceptable-error threshold under instrumental justification.').

omega_variable(
    authority_grounding_framing,
    'Is the authority structure adjudicating this kernel grounded in expertise (criminological evidence communities evaluating the deterrence premise) or in extraction (political actors whose benefit flows from the kernel remaining stable regardless of evidence)?',
    'Trace whose assessments actually move practice: if statute and warrant decisions track research findings, the expertise framing holds; if they persist unchanged through null findings, the extraction framing holds.',
    'Under the expertise framing the interpretive layer is genuine and drift can surface as revision; under the extraction framing the interpretive layer is decorative, the theater ratio understates performative maintenance, and the arrangement trends inertial as evidence decays while practice persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination: expertise versus extraction authority for the same structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_killing_deterrence_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(state_killing_deterrence_tr_t0, observed).
narrative_ontology:measurement(state_killing_deterrence_tr_t10, state_killing_legitimacy__deterrence_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(state_killing_deterrence_tr_t10, observed).
narrative_ontology:measurement(state_killing_deterrence_tr_t20, state_killing_legitimacy__deterrence_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(state_killing_deterrence_tr_t20, observed).
narrative_ontology:measurement(state_killing_deterrence_tr_t30, state_killing_legitimacy__deterrence_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(state_killing_deterrence_tr_t30, observed).
narrative_ontology:measurement(state_killing_deterrence_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(state_killing_deterrence_tr_t40, observed).
narrative_ontology:measurement(state_killing_deterrence_tr_t50, state_killing_legitimacy__deterrence_reading, theater_ratio, 50, 0.34).
narrative_ontology:measurement_basis(state_killing_deterrence_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(state_killing_deterrence_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(state_killing_deterrence_be_t0, observed).
narrative_ontology:measurement(state_killing_deterrence_be_t10, state_killing_legitimacy__deterrence_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(state_killing_deterrence_be_t10, observed).
narrative_ontology:measurement(state_killing_deterrence_be_t20, state_killing_legitimacy__deterrence_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(state_killing_deterrence_be_t20, observed).
narrative_ontology:measurement(state_killing_deterrence_be_t30, state_killing_legitimacy__deterrence_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(state_killing_deterrence_be_t30, observed).
narrative_ontology:measurement(state_killing_deterrence_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(state_killing_deterrence_be_t40, observed).
narrative_ontology:measurement(state_killing_deterrence_be_t50, state_killing_legitimacy__deterrence_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(state_killing_deterrence_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(state_killing_deterrence_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(state_killing_deterrence_su_t0, observed).
narrative_ontology:measurement(state_killing_deterrence_su_t10, state_killing_legitimacy__deterrence_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(state_killing_deterrence_su_t10, observed).
narrative_ontology:measurement(state_killing_deterrence_su_t20, state_killing_legitimacy__deterrence_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(state_killing_deterrence_su_t20, observed).
narrative_ontology:measurement(state_killing_deterrence_su_t30, state_killing_legitimacy__deterrence_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement_basis(state_killing_deterrence_su_t30, observed).
narrative_ontology:measurement(state_killing_deterrence_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement_basis(state_killing_deterrence_su_t40, observed).
narrative_ontology:measurement(state_killing_deterrence_su_t50, state_killing_legitimacy__deterrence_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(state_killing_deterrence_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% 'State killing legitimacy' is a colloquial label covering three structurally distinct constraints. This file is the deterrence reading: epsilon indexed to contested marginal-prevention evidence, referent fixed to the standing arrangement. The retributive reading prices the same conduct by desert (epsilon low if desert is granted); the abolition reading prices it categorically (epsilon near ceiling regardless of efficacy). The readings share a kernel but not a constraint; each links to the others rather than averaging across them, per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
