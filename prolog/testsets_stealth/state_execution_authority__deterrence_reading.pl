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
 *   human_readable: Capital Punishment Under the Deterrence Justification
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the deterrence reading of state execution
 *   authority: the standing arrangement under contest is capital punishment
 *   as maintained in jurisdictions that justify it by its preventive effect —
 *   statutes defining capital-eligible offenses, sentencing hearings, a
 *   decade-scale appellate and clemency apparatus, and the execution itself.
 *   The reading's own structure fixes the epsilon referent: epsilon is
 *   authored for this existing arrangement as the deterrence reading assesses
 *   it, never for the life-without-parole regime the reading would accept as
 *   substitute. On the reading's own lights the arrangement is justified only
 *   insofar as it prevents murders that imprisonment would not prevent; the
 *   condemned are the instrumental cost of that prevention, the wrongfully
 *   executed are pure loss requiring error-rate minimization, and the
 *   protected class of future potential victims is the beneficiary the whole
 *   apparatus exists to serve. Because the efficacy premise is empirically
 *   unresolved, the reading tolerates moderate-to-high extraction pending
 *   evidence rather than treating the arrangement as either free protection
 *   or naked killing.
 *
 * KEY AGENTS:
 *   - condemned_inmates: Primary target (powerless/trapped) — bears the arrangement's terminal cost with no exit short of the review machinery itself
 *   - wrongfully_convicted_executed: Error-bearing target (powerless/trapped) — the reading's own utilitarian loss term, contributing nothing to the preventive calculus
 *   - executing_states: Agenda-setter and principal collector (institutional/constrained) — defines the offenses, runs the machinery, collects plea leverage and signaling value
 *   - appellate_review_courts: Enforcement administrator (institutional/constrained) — controls how much error correction the machinery performs through finality doctrine
 *   - future_potential_victims: Protected beneficiary class (powerless/constrained) — receives the non-occurrence the arrangement claims to produce
 *   - surviving_victims_families: Beneficiary class, internally divided (organized/constrained) — receives the protective promise and the closure ritual; a faction organizes against the arrangement
 *   - prosecutors: Secondary beneficiary (powerful/mobile) — converts the capital statute into plea leverage and career outcomes
 *   - taxpayers: Diffuse payer (moderate/constrained) — funds the capital track's substantial premium over the substitute sanction
 *   - families_of_the_condemned: Collateral payer (powerless/constrained) — absorbs decades of litigation and stigma with no formal standing
 *   - innocence_movement_litigators: Analytical observer (organized/analytical) — surfaces the error record the arrangement's own justification must answer
 *   - international_rights_bodies: Excluded objector (institutional/trapped) — presses abolition from outside the domestic legal order with no vote in it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.58).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.66).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "Capital Punishment Under the Deterrence Justification").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '27436629-77e5-4674-b3a5-e8467273b44f').
narrative_ontology:cs_kernel_codification('27436629-77e5-4674-b3a5-e8467273b44f', fixed_text).
narrative_ontology:cs_authority_grounding('27436629-77e5-4674-b3a5-e8467273b44f', lineage).
narrative_ontology:cs_interpretation_layer_present('27436629-77e5-4674-b3a5-e8467273b44f').
narrative_ontology:cs_reading_relation('27436629-77e5-4674-b3a5-e8467273b44f', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('27436629-77e5-4674-b3a5-e8467273b44f', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('27436629-77e5-4674-b3a5-e8467273b44f', foundational, execution_permissible_if_marginally_deterrent).
narrative_ontology:cs_axiom_status(execution_permissible_if_marginally_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('27436629-77e5-4674-b3a5-e8467273b44f', execution_permissible_if_marginally_deterrent, empirically_contingent).
narrative_ontology:cs_axiom('27436629-77e5-4674-b3a5-e8467273b44f', secondary, wrongful_execution_counts_as_utilitarian_loss).
narrative_ontology:cs_axiom_status(wrongful_execution_counts_as_utilitarian_loss, holdable).
narrative_ontology:cs_axiom_grounding('27436629-77e5-4674-b3a5-e8467273b44f', wrongful_execution_counts_as_utilitarian_loss, instrumental).
narrative_ontology:cs_reference_frame('27436629-77e5-4674-b3a5-e8467273b44f', preventive_utilitarian_sanction_framework).
narrative_ontology:cs_drift_state('27436629-77e5-4674-b3a5-e8467273b44f', post_systematic_evidence_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('27436629-77e5-4674-b3a5-e8467273b44f', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, surviving_victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, prosecutors).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, executing_states).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, condemned_inmates).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_executed).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, families_of_the_condemned).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines capital-eligible offenses by statute, funds the prosecution and appellate machinery, and carries out sentences through executive clemency and warrant processes. Collects plea leverage, electoral signaling value, and the fiscal flows that run through the capital litigation apparatus. Can abolish the penalty by ordinary legislation — many jurisdictions have — but faces concentrated political cost for doing so while the protective promise retains public support.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executing_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__deterrence_reading, executing_states, beneficiary).

% Administer the review layer: direct appeal, post-conviction petitions, stays, and proportionality review. Their dockets and doctrinal choices (finality rules, filing deadlines, harmless-error standards) determine how much error correction the machinery actually performs. They cannot resign from the role the statutes assign them, though they shape its intensity.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, appellate_review_courts, agenda_setter,
    institutional, generational, constrained, national).

% Charging decisions under a capital statute convert the death penalty into plea-bargaining leverage: defendants accept life terms to avoid death qualification and trial risk. Career advancement tracks capital-case outcomes. Individual prosecutors can retire, transfer, or run for office elsewhere; the leverage travels with the statute, not with them.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, prosecutors, beneficiary,
    powerful, biographical, mobile, regional).

% The class of persons whose future murders the arrangement claims to prevent by raising the expected cost of killing. They receive nothing observable — their benefit is the non-occurrence of a crime against them — and they cannot opt out of the risk pool except by moving, which changes exposure but not membership in the class.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_victims, beneficiary,
    powerless, generational, constrained, national).

% Survivors of homicide victims receive the arrangement's protective promise and, case by case, the closure ritual of a capital prosecution and execution. The class is internally divided: some families pursue execution for decades, others organize against it, and both factions are politically mobilized. No member can exit the fact of their loss; participation in the process is voluntary.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, surviving_victims_families, beneficiary,
    organized, biographical, constrained, national).

% Persons sentenced to death bear the arrangement's terminal cost. Between sentence and execution they live under heightened confinement with a scheduled legal deadline approaching. Exit routes are the appellate and clemency machinery itself, which succeeds rarely and on timelines measured in decades; there is no purchase, migration, or withdrawal from the sentence.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, condemned_inmates, payer,
    powerless, immediate, trapped, national).

% The subset of the condemned who did not commit the crime. They bear the full terminal cost while contributing nothing to the preventive calculus the arrangement claims — the reading's own framework registers them as pure loss. Their existence is established by posthumous clearances and last-minute exonerations; the true rate is unknown because detection stops at execution.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted_executed, payer,
    powerless, immediate, trapped, national).

% Parents, children, and spouses of the condemned absorb decades of litigation, stigma, and the anticipation of a state-administered death in the family. They have no formal standing in the process that determines the outcome and cannot leave the relationship that exposes them to it.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, families_of_the_condemned, payer,
    powerless, biographical, constrained, national).

% Fund the capital track, which costs substantially more per case than the life-without-parole alternative — longer trials, death-qualified juries, decade-scale appeals. The differential is a diffuse annual cost borne by everyone taxed in the jurisdiction; individual exit is limited to moving or voting.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Defense organizations and investigative journalists who surface wrongful convictions, litigate post-conviction claims, and compile the exoneration record. They sit outside the enforcement coalition, see the machinery's error rate from the inside of its files, and supply the data on which the arrangement's error-minimization claims are tested.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, innocence_movement_litigators, observer,
    organized, generational, analytical, national).

% Treaty bodies, foreign governments, and human-rights organizations that classify capital punishment as a rights violation and press for abolition through reporting, diplomatic pressure, and extradition conditions. They hold no vote in the retaining jurisdictions' legislatures and no docket in their courts; their objection is registered but structurally outside the domestic conversation.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, international_rights_bodies, excluded,
    institutional, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, executing_states).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Raises the expected cost of capital murder above the cost imposed by the next-most-severe lawful sanction, aiming to tip the prospective killer's calculus away from killing, and concentrates the state's punitive response at a single maximum tier administered through uniform statute and review.
% TRANSFER_FUNCTION: Moves the ultimate sanction — life itself — from condemned offenders (and, erroneously, some who are innocent) into the state's account of preventive protection; moves a fiscal premium from taxpayers to the capital litigation apparatus; moves plea concessions from capital defendants to prosecuting authorities.
% ABSENT_VOICES: The executed are permanently silent — their testimony exists only as posthumous clearance records compiled by others. International rights bodies object from outside the domestic legal order. The populations whose behavior constitutes the benefit term (those deterred, and those not deterred) are counterfactual and cannot appear in any forum.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, capital statutes would lapse into life-without-parole regimes, charging and plea structures would reprice immediately, prosecutors would lose their strongest lever, the appellate and clemency apparatus would shed its capital dockets, and the political coalition organized around retention would dissolve into the ordinary punishment debate.
% FOUNDING_PROBLEM: Whether the state's ultimate sanction can be deployed to prevent the gravest crimes rather than merely answer ones already committed — built at founding moments when lesser sanctions were believed insufficient to protect the public from killing.
% FOUNDING_PROBLEM_CORROBORATION: No party outside the dispute attests a settled answer, and that division is itself the signal: national research-panel reviews of the deterrence literature conclude the evidence is inconclusive, econometric studies on both sides remain in print, and jurisdiction-pair comparisons (retentionist versus abolitionist, matched for demographics) show no divergence robust enough for either camp to concede. Criminological bodies, innocence organizations, and retentionist advocacy groups all attest — to opposite readings of the same record.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.58: the arrangement takes life from a targeted class — the maximal taking available to a legal order — discounted in the reading's own accounting by the protective output it claims, which remains empirically unresolved and therefore cannot be credited at full value. Suppression is 0.66 as a raw structural property (unscaled by power or scope): finality doctrine, filing deadlines, rare clemency, and execution notwithstanding residual doubt are coercive infrastructure, not participant preference. Theater ratio sits just under half (0.48): the appellate layer performs real error correction — exonerations demonstrably flow through it — while a growing share of activity is performative finality ritual, medicalized execution protocol, and last-minute litigation drama that legitimizes rather than corrects. Accessibility collapse is low-moderate (0.45): the substitute sanction is visible, functioning, and adopted by peer jurisdictions, so alternatives do not vanish once the arrangement is understood. Resistance is high (0.65): a sustained abolition movement, innocence litigation, declining death sentences, and periodic moratoria meet the arrangement continuously. The temporal series share one grid (t = years since the modern reinstatement, endpoints 0 and 48): extractiveness climbs through the enforcement-buildup decades, peaks around t=24, and settles slightly lower as exonerations accumulate; theater rises monotonically as the demonstrable deterrence yield fails to grow while procedural performance expands; suppression_requirement is included because enforcement capacity genuinely moved — it ratcheted up through the finality-legislation era, peaked near t=24, decayed under moratoria and botched-execution reviews, and ticked back up with resumed federal and state scheduling. The trajectory is a rise-peak-partial-retreat, not a cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from identical structural data. From the executing-state seat the arrangement is a deliberate policy instrument it built, staffs, and defends — coordination it administers. From the condemned seat the same structure is annihilation on a schedule behind locked doors — extraction with zero exit. From the future-victim seat it is insurance whose payout is invisible by construction. From the prosecutor's seat it is leverage. None of these perceptions is authored as a role; each is computable from the power, exit, and position data, and the divergence between them is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: future_potential_victims and surviving_victims_families sit near the subsidized end (the arrangement exists for them), prosecutors lower still given mobile exit and concentrated gain, and executing_states near the beneficiary end as collector-administrator. Victim declarations drive high directionality: condemned_inmates and wrongfully_convicted_executed sit at the full-target end, amplified by trapped exit — no arbitrage, no mobility, identity irrelevant to the taking. Taxpayers and families_of_the_condemned occupy intermediate target positions: real but diffuse costs, constrained exit. Appellate courts derive from neither list and take the canonical fallback, which suits their administrator position. No directionality overrides were needed: the beneficiary/victim plus exit data already separates the seats correctly, and a power-atom-keyed override would misfire across the multiple agents sharing each atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the arrangement as pure rope — protective coordination with acceptable overhead — hides the asymmetric extraction: the entire cost lands on a class with no exit, including members who are innocent, while the benefit is unobservable in principle. Reading it as pure snare — coercion wearing a coordination costume — erases the possibility, live on the current evidence, that the preventive function is real and that some extraction is the price of it. Tangled rope holds both halves open and routes the decision to the marginal-deterrence omega: if the deterrent premium over imprisonment is zero, the coordination leg collapses and the computed classification should migrate toward snare; if the premium is robust, the protective leg licenses the arrangement's core. The genealogy interview shows no zombie signature: the founding problem (whether the ultimate sanction prevents the gravest crimes) is contested rather than dead, and the disappearance verdict is world_rearranges rather than world_unchanged — the arrangement's mandate is still fighting, not lingering after its function left.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_deterrence_over_lwop,
    'Does execution produce a marginal deterrent effect on capital murder beyond what life-without-parole produces — the operative substitute the reading itself accepts as comparator?',
    'Panel-reviewed synthesis of abolition and adoption natural experiments and execution-frequency variation, with sensitivity analysis for aggregation artifacts and covariate instability that have plagued the econometric literature on both sides.',
    'If no marginal effect exists, the coordination half of this reading collapses and the arrangement computes as extraction without compensating benefit, migrating the classification toward snare; a robust premium would license the protective core and hold the tangled-rope reading stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_deterrence_over_lwop, empirical, 'Whether the deterrent premium of execution over imprisonment is real — the load-bearing empirical premise of this reading.').

omega_variable(
    innocent_execution_rate,
    'What fraction of carried-out executions are of factually innocent persons once all recoverable post-hoc evidence is in?',
    'Cumulative exoneration and posthumous-clearance records projected against total executions, with survival-adjusted bounds — acknowledging that detection ceases at execution, so the observed rate is a floor.',
    'Every increment of innocent execution raises effective extraction and pushes the computed classification toward snare; within the reading''s own framework it converts error-rate minimization from aspiration into a binding constraint on permissible deployment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innocent_execution_rate, empirical, 'The true rate of wrongful execution — the reading''s own utilitarian loss term.').

omega_variable(
    counterfactual_deterrence_unobservability,
    'Can the reading''s benefit term — the murders that did not occur because of the arrangement — ever be verified rather than modeled, given that the counterfactual is unobservable in principle?',
    'Only bounded inference: cross-jurisdiction homicide comparisons, offender decision studies, and interrupted time-series around moratoria; no direct observation of prevented murders is possible.',
    'Permanent under-determination of the benefit term keeps epsilon assessment reading-indexed and prevents any single dataset from settling the arrangement''s justification; classification must remain contingent on the empirical omegas rather than closing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_deterrence_unobservability, conceptual, 'Structural unobservability of the preventive benefit — why this reading''s central claim cannot be fully verified.').

omega_variable(
    kernel_reading_allocation,
    'This constraint is one reading of the state_execution_authority kernel; which structural elements would change under the sibling readings, and where exactly is the disagreement located?',
    'Comparative authorship of the sibling stories: abolition_reading deletes the future-victim beneficiary set entirely and maximizes extraction from the condemned''s seat; retributive_reading relocates beneficiaries to moral-order restoration and decouples justification from measurable consequence. The disagreement is located in whether consequences (prevented murders) or intrinsic justice constitutes the warrant for the same physical act.',
    'Classification is reading-indexed: the same executions classify differently across the three readings because beneficiary/victim structure and epsilon differ per reading. Cross-reading comparison is valid only through the network edges linking the family — never by averaging their metrics into one verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer structure: how this reading''s constraint differs structurally from its kernel siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__deterrence_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__deterrence_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__deterrence_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__deterrence_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(stat_tr_t48, state_execution_authority__deterrence_reading, theater_ratio, 48, 0.48).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__deterrence_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__deterrence_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__deterrence_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__deterrence_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(stat_be_t48, state_execution_authority__deterrence_reading, base_extractiveness, 48, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__deterrence_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__deterrence_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__deterrence_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__deterrence_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(stat_su_t48, state_execution_authority__deterrence_reading, suppression_requirement, 48, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, abolition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'capital punishment' covers three structurally distinct constraints instantiated from the state_execution_authority kernel. This file authors the deterrence_reading — epsilon anchored to the preventive-efficiency requirement, beneficiaries including the future-victim class, the executed as instrumental cost. retributive_reading shares the same physical act but different warrant (desert, not consequence) and therefore different beneficiary structure and epsilon; abolition_reading denies the act's permissibility categorically and authors maximal extraction from the condemned's seat with an empty legitimate-beneficiary set. Each member links the others here; no member's metrics subsume another's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
