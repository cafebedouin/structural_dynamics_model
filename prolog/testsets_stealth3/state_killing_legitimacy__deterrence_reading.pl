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
 *   human_readable: Capital Punishment as Rational Deterrent Signal (Deterrence Reading)
 *   domain: criminal justice/political philosophy/legal theory
 *
 * SUMMARY:
 *   Capital punishment in retentionist jurisdictions, read through its
 *   deterrence justification: the state kills convicted murderers not because
 *   they deserve death (the retributive reading) and not despite every
 *   justification (the abolition reading), but because the killing is claimed
 *   to operate as a rational signal that lowers the future murder rate. This
 *   story authors THAT reading only, as a clean epsilon-invariant constraint.
 *   The referent of epsilon is the standing arrangement under contest —
 *   capital punishment as actually practiced — assessed by the deterrence
 *   reading's own lights: the reading prices the arrangement as justified if
 *   and only if execution produces a marginal deterrent effect beyond life
 *   imprisonment, and the evidentiary record leaves that condition
 *   unresolved, hence moderate epsilon rather than near-zero. The claim and
 *   the metrics are independent authored facts: the claimed type
 *   (tangled_rope) reflects the structure this reading itself describes — a
 *   genuine protective coordination function wrapped around the ultimate
 *   asymmetric extraction — while the metrics describe the arrangement's
 *   observed operation, including the erosion of the functional content the
 *   reading depends on. KEY AGENTS (by structural relationship): -
 *   condemned_offenders: Primary target (powerless/trapped) — bears the full
 *   cost up to and including life; instrumentalized as the signal's carrier -
 *   wrongfully_convicted_capital_defendants: Secondary target
 *   (powerless/trapped) — absorb the irreversibility risk; pure cost in the
 *   reading's own ledger - families_of_the_executed: Collateral bearers
 *   (powerless/constrained) — lose kin and carry stigma without standing -
 *   potential_future_murder_victims: Declared primary beneficiary
 *   (powerless/constrained) — the diffuse class whose non-murder is the
 *   claimed product; exists only if the marginal-deterrence premise holds -
 *   general_public_retentionist_jurisdictions: Beneficiary with payer residue
 *   (organized/mobile) — claimed security and expressive satisfaction, funded
 *   by above-LWOP litigation costs - tough_on_crime_elected_officials:
 *   Concentrated beneficiary (institutional/arbitrage) — collects electoral
 *   rents while bearing none of the sanction's costs -
 *   capital_punishment_legislatures: Agenda setter (institutional/arbitrage)
 *   — could dissolve the constraint by ordinary bill -
 *   capital_sentencing_courts: Enforcement administrator
 *   (institutional/constrained) — applies and reviews inside statutory
 *   bounds; absorbs drift through interpretation -
 *   international_human_rights_bodies: Excluded voice (institutional/mobile)
 *   — barred from the retentionist domestic conversation; their exclusion
 *   marks the arrangement's boundary
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.55).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "Capital Punishment as Rational Deterrent Signal (Deterrence Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal justice/political philosophy/legal theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, '601bc409-d316-4cd1-ab0e-7eb9cd71d0d5').
narrative_ontology:cs_kernel_codification('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', formalized).
narrative_ontology:cs_authority_grounding('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', practice).
narrative_ontology:cs_interpretation_layer_present('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5').
narrative_ontology:cs_reading_relation('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', foundational, execution_justified_by_marginal_deterrent_effect).
narrative_ontology:cs_axiom_status(execution_justified_by_marginal_deterrent_effect, holdable).
narrative_ontology:cs_axiom_grounding('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', execution_justified_by_marginal_deterrent_effect, empirically_contingent).
narrative_ontology:cs_axiom('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', secondary, individual_life_may_be_committed_as_social_protection_instrument).
narrative_ontology:cs_axiom_status(individual_life_may_be_committed_as_social_protection_instrument, holdable).
narrative_ontology:cs_axiom_grounding('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', individual_life_may_be_committed_as_social_protection_instrument, instrumental).
narrative_ontology:cs_reference_frame('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', rational_deterrence_equilibrium).
narrative_ontology:cs_drift_state('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', post_nrc_2012_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('601bc409-d316-4cd1-ab0e-7eb9cd71d0d5', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_murder_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, general_public_retentionist_jurisdictions).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, tough_on_crime_elected_officials).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, wrongfully_convicted_capital_defendants).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, families_of_the_executed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, general_public_retentionist_jurisdictions).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, marginal_deterrence_hypothesis).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, rational_actor_model_of_homicide).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, signal_superiority_of_execution_over_lwop).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons sentenced to death under capital statutes. They exhaust mandatory appeals in custody, are held isolated on death row for years, and are ultimately killed by the state against their will unless clemency intervenes. They bear the entire physical cost of the arrangement and serve as the carriers of whatever signal the execution transmits. Exit does not exist from their position: custody is total, and the only relief routes (commutation, pardon) are controlled by the same offices that campaign on the sanction.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, condemned_offenders, payer,
    powerless, biographical, trapped, national).

% Defendants convicted of capital crimes who are in fact innocent, identified chiefly through post-conviction DNA exonerations and innocence-project investigations. They absorb the arrangement's irreversibility risk: an erroneous life sentence can be partially repaired, an erroneous execution cannot. In the deterrence reading's own utility ledger their deaths are pure cost with no offsetting protective product.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, wrongfully_convicted_capital_defendants, payer,
    powerless, biographical, trapped, national).

% Kin of condemned and executed persons. They lose a family member to the sanction, frequently after decades of appeals, and carry the stigma of association with the crime and the process. They hold no formal standing in sentencing or execution decisions and are split internally, with some supporting and some opposing the killing of their relative.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, families_of_the_executed, payer,
    powerless, biographical, constrained, local).

% The diffuse, unidentified class of people who would be murdered in the future but for the deterrent effect the arrangement claims to produce. They receive the arrangement's entire claimed protective product, yet they are not an organized constituency: no member knows they belong to the class, and the class exists at all only if the marginal-deterrence premise holds. Their protection cannot be opted out of or into.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_murder_victims, beneficiary,
    powerless, generational, constrained, national).

% Citizens and voters of jurisdictions that retain capital punishment. They receive claimed security and the expressive satisfaction of seeing homicide answered at the maximum level, and polls show durable majority support in many retentionist states. They also fund the arrangement: capital prosecutions, decades of death-row custody, and appellate machinery cost substantially more than life-without-parole dispositions. Their exit runs through the ballot box, where the same majority can repeal what it sustains.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, general_public_retentionist_jurisdictions, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, general_public_retentionist_jurisdictions, payer).

% Prosecutors, governors, and legislators who campaign on and administer the capital sanction. They collect concentrated electoral returns from its maintenance — endorsements, deterrence rhetoric, displays of resolve — while bearing none of its costs personally: they are constitutionally ineligible for the sanction they seek, and they can reposition between electoral cycles at low cost. Several have converted opposition to the sanction into its own electoral asset when districts shifted.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, tough_on_crime_elected_officials, beneficiary,
    institutional, immediate, arbitrage, national).

% State legislatures that enact, narrow, expand, or repeal capital statutes. They set the agenda for the whole arrangement: the list of capital crimes, the method, the appellate posture. Repeal is available to them by ordinary bill, and a number of jurisdictions have exercised it within a single session; the binding constraint on doing so is electoral exposure, not legal or material difficulty.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, capital_punishment_legislatures, agenda_setter,
    institutional, biographical, arbitrage, national).

% Trial and appellate courts that impose, review, and uphold death sentences inside statutory bounds. They administer the enforcement machinery — proportionality review, ineffective-assistance doctrine, habeas standards — and absorb drift through interpretation without revisiting the kernel question of whether the sanction should exist. Individual judges cannot exit the system they administer; they can only shape it from within.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, capital_sentencing_courts, agenda_setter,
    institutional, generational, constrained, national).

% UN treaty bodies, regional human rights courts, and abolitionist foreign governments that condemn capital punishment and condition cooperation on its restriction. They are structurally excluded from retentionist domestic deliberation: federal doctrines and sovereignty claims keep their rulings advisory at best inside executing jurisdictions. Their exclusion defines the arrangement's outer boundary and supplies the standing external criticism the domestic debate filters out.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, international_human_rights_bodies, excluded,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__deterrence_reading, tough_on_crime_elected_officials).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__deterrence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses the collective protection problem posed by premeditated homicide: permanent incapacitation of convicted murderers, a publicized maximum sanction intended to raise the expected price of killing above its payoff for instrumentally rational actors, and a collective declaration that homicidal violence will be answered at the highest available level of state response.
% TRANSFER_FUNCTION: Moves the condemned offender's remaining life-years from the offender to the state's protective and expressive purposes; moves electoral credit to officials who campaign on the sanction; moves the excess cost of capital litigation to taxpayers; delivers felt security and expressive satisfaction to the supporting public.
% ABSENT_VOICES: The condemned after final judgment have no standing voice; the wrongfully executed can never testify; international human rights bodies are excluded from retentionist domestic deliberation; abolitionist members of victims' families are crowded out of the official closure narrative; residents of the communities hosting executions bear concentrated siting burdens with little say.
% DISAPPEARANCE_RATIONALE: Capital statutes, death-row facilities, specialized appellate and habeas machinery, prosecutorial charging practices, clemency offices, and tough-on-crime electoral coalitions all depend on the arrangement. Overnight disappearance would force statutory rewrites, convert death rows to life-without-parole housing, strand pending capital prosecutions, and require every elected official positioned on the issue to reposition.
% FOUNDING_PROBLEM: Centralizing the response to homicide: replacing feud-cycle private vengeance with a sovereign, proportionate, publicly administered maximum penalty — later reframed in the modern era as forward-looking crime control, preventing future murders rather than answering past ones.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's liveness is corroborated from outside the benefiting parties by FBI Uniform Crime Reporting data and WHO mortality statistics, which attest that homicide persists at scale. The National Research Council's 2012 committee on deterrence and the death penalty — convened independently of retentionist beneficiaries — attests the problem is live while concluding that the existing literature does not establish that capital punishment deters homicide relative to lesser sanctions. No body outside the benefiting parties attests that the arrangement itself solves the problem; that gap is itself the signal.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored at 0.55 (moderate, matching the expected structural delta): the reading's own lights impose the ultimate cost on identifiable individuals for a social end whose efficacy the reading itself concedes must be demonstrated, and the demonstration is unresolved — half-vindicated justification over a fully-imposed cost. Suppression is 0.72 and is authored as a raw structural property, unscaled by power or scope: the arrangement runs on total custody, isolation, and an enforcement machinery that visibly hardened over the interval (habeas-limiting legislation in the mid-1990s, lethal-injection secrecy statutes in the 2010s). Theater_ratio is 0.50 and rising: as the functional deterrence content eroded under accumulating contrary evidence, the arrangement's activity shifted toward the signal-performance itself — publicity protocols, execution witnesses, official statements — which is Goodhart drift of the proxy (the signal) away from the function (fewer murders). Accessibility_collapse is low (0.30) because the principal alternative — life imprisonment without parole — is fully available, well understood, and demonstrably sufficient in abolitionist jurisdictions; the constraint does not close exits, it prices them rhetorically. Resistance is high (0.75): abolition movements, innocence projects, religious bodies, and international pressure constitute continuous organized opposition unusual for a legal arrangement. The measurement series run on one shared time grid (points 0, 10, 20, 30, 40, 50) with all three tracked metrics authored at every point; the extractiveness series peaks mid-interval as error-rate evidence accumulated under peak execution rates, then declines as execution frequency fell sharply — a decline in activity, not in the per-event cost the reading imposes.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute very different types from identical structural data. From the condemned offender's position the arrangement is indistinguishable from pure extraction: total cost, zero exit, no consent, and a justification that may be empty. From the elected official's position it is a functioning enforcement instrument that reliably produces electoral returns. From the general public's position it is a mixed good: expressive satisfaction received, excess tax cost paid, exit available at the ballot. The engine computes these per-seat classifications from the power, exit, and role data; the divergence between the computed payer-seat type and the computed agenda-seat type is the measurement this story exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Condemned offenders, wrongfully convicted defendants, and their families are declared victims with trapped or constrained exit, placing them near the full-target end of directionality — amplified effective extraction, maximally so for the trapped seats. Potential future murder victims are declared beneficiaries but are powerless and unorganized; their low directionality is contingent on the marginal-deterrence premise, which is why the empirical-status omega governs whether their seat is real. The general public carries a dual role (beneficiary with payer residue): the derivation nets their diffuse benefit against their tax burden, landing them nearer symmetric than either pole. Tough-on-crime officials are beneficiaries with arbitrage-grade exit — nearest the beneficiary end, since they can abandon the position at will while collecting from it. No directionality overrides are needed: the structural derivation from declared roles plus exit options reproduces the true relationships for every seat. On coalition power: the powerless payer seats cannot rescue themselves through coalition — the condemned are individually isolated post-judgment and processed one at a time, and the wrongfully convicted are invisible until posthumous exoneration — so coalition formation does not soften the target-end classification here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (centralizing the response to homicide, then preventing future murders) is live — homicide persists — so the R5 mismatch consumer sees status=live paired with verdict=world_rearranges and correctly fires no zombie flag: this is not an arrangement outliving its problem. Mandatrophy_resolved is therefore deliberately left undeclared. The tangled_rope claim is what prevents both mislabelings: reading the arrangement as pure snare would erase the real coordination content (incapacitation is genuine and immediate; expressive condemnation solves a real collective-signaling problem), while reading it as pure rope would erase the asymmetric extraction (one agent pays with life for diffuse benefits, and the concentrated collector is an official who bears none of the cost). The rising theater_ratio series documents the drift path the mandatrophy lens watches: the signal-proxy progressively displacing the deterrence-function, which is precisely the degradation route by which a tangled rope decays toward piton or snare if the functional content is never vindicated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the deterrence_reading of the state_killing_legitimacy kernel; how would instantiating the retributive_reading or the abolition_reading instead restructure the beneficiary/victim surface and epsilon?',
    'Compare against the sibling stories linked in network.affects_constraints: the retributive reading relocates justification to proportional desert (a victim-and-moral-order-centered beneficiary structure, epsilon indexed to forfeiture rather than utility), while the abolition reading deletes the beneficiary class entirely — no utility can ground the killing — and drives epsilon toward the full-cost end.',
    'Classification is reading-indexed: the same underlying practice classifies differently under each sibling, and cross-reading comparison must route through the reading_relations edges rather than by averaging epsilon across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three readings of the state-killing-legitimacy kernel; the disagreement is located in the justificatory premise (utility vs desert vs dignity).').

omega_variable(
    marginal_deterrence_empirical_status,
    'Does execution produce a marginal deterrent effect on homicide beyond that already achieved by life imprisonment without parole?',
    'Panel-grade systematic review plus natural experiments comparing matched retentionist and abolitionist jurisdictions over equivalent intervals, following the evidentiary standards the National Research Council''s 2012 committee specified; single-study econometrics explicitly excluded.',
    'If no marginal effect exists, the declared beneficiary class potential_future_murder_victims is empty, the coordination function reduces to incapacitation (which life-without-parole already supplies), and the constraint collapses toward pure extraction; a demonstrated marginal effect would strengthen the rope-side reading and lower effective extraction for every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_deterrence_empirical_status, empirical, 'Whether the reading''s foundational empirical premise holds.').

omega_variable(
    wrongful_execution_rate,
    'What fraction of capital sentences are or were imposed on factually innocent defendants, and what is the implied rate of irreversible wrongful execution?',
    'Exoneration rates from post-conviction DNA and non-DNA investigation extrapolated with survival-analysis methods over the full death-row population, anchored on innocence-project registries as a lower bound.',
    'Wrongful executions enter the reading''s own utility ledger as pure cost with no offsetting benefit; above a modest threshold the expected-value justification fails even granting deterrence, pushing effective extraction upward for every seat and strengthening the snare-side classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_rate, empirical, 'Error-rate weight in the reading''s own calculus.').

omega_variable(
    deterrence_mechanism_attribution,
    'If any deterrent effect exists, does it operate through the rational-calculation channel the reading premises (the perceived expected price of murder), or entirely through the incapacitation of the executed individuals themselves?',
    'Disentangle execution effects from incarceration effects using execution-moratorium episodes and commutation waves: if homicide rates move with execution events net of imprisonment changes, a signal channel exists; if not, incapacitation explains everything.',
    'If incapacitation does all the work, the killing component adds no protective value, the signal framing is theater, and the authored theater_ratio is understated; the reading''s distinctive premise would fail while a weaker incapacitation rationale survives — changing which axiom carries the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_mechanism_attribution, empirical, 'Channel attribution for whatever deterrent effect exists.').

omega_variable(
    cs_framing_underdetermination,
    'Is the right commitment-system frame the statutory-judicial apparatus declared here (formalized kernel, practice-grounded authority), or the electoral-punitive culture layered above it, whose operational-success narrative is what actually sustains official confidence in the deterrence premise?',
    'Run both framings through the cs_pattern machinery: under the electoral-culture framing, authority_grounding shifts toward extraction (officeholders benefit from preventing revision of the deterrence narrative) and the drift profile deepens; diagnostic signals include campaign-material reliance on the premise and legislative indifference to contrary evidence.',
    'Under the alternative framing the constraint reads as a captured commitment system whose kernel stability is maintained for officeholders'' benefit, tightening the snare-side classification; under the declared framing it remains a practice-grounded legal arrangement with contested empirical foundations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two coherent CS framings yield different classifications; the framing choice is documented here rather than left implicit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__deterrence_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__deterrence_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__deterrence_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.46).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__deterrence_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__deterrence_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__deterrence_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__deterrence_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__deterrence_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__deterrence_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__deterrence_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__deterrence_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__deterrence_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the state_killing_legitimacy kernel. The colloquial label 'capital punishment debate' conflates three structurally distinct constraints: this deterrence reading (justification runs through marginal utility; beneficiary class is potential future victims; epsilon moderate and hostage to contested empirical evidence), the retributive reading (justification runs through proportional desert; beneficiary is the moral order and the victim's memory; epsilon indexed to forfeiture rather than utility), and the abolition reading (no utility can ground the killing; beneficiary class empty by construction; epsilon maximal). Each story carries its own epsilon, beneficiary/victim structure, and classification; they are linked here rather than averaged. The deterrence reading sits upstream of the other two in legitimacy terms: its empirical premise, once challenged, pressures the retributive reading to stand on desert alone and hands the abolition reading its strongest evidentiary ammunition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
