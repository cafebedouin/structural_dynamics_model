% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Capital Punishment as Conditional Deterrence Instrument
 *   domain: criminal justice/political philosophy/constitutional law
 *
 * SUMMARY:
 *   A retentionist jurisdiction maintains the authority to execute people
 *   convicted of capital murder, and this story authors that arrangement as
 *   the deterrence_instrument reading of the state-killing kernel: the
 *   practice is warranted if and only if it prevents future murders at
 *   acceptable cost. Under this reading the beneficiary structure is defined
 *   by the conditional itself — future potential murder victims are the class
 *   whose lives are the claimed return, the condemned person is the
 *   instrumental cost paid to produce that return, and the state's authority
 *   is legitimate exactly insofar as the efficacy premise holds. The
 *   arrangement requires continuous enforcement machinery: capital statutes,
 *   specialized sentencing procedures, decades of appellate review, clemency
 *   processes, and execution infrastructure. Family note
 *   (epsilon-invariance): this is one of three sibling stories decomposing
 *   the colloquial label 'capital punishment'; the siblings —
 *   retributive_desert and categorical_abolition — instantiate different
 *   constraints over the same kernel, with different beneficiary/victim sets
 *   and different reading-indexed epsilon values over the same referent (the
 *   standing execution regime). This file links both siblings via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   condemned_capital_defendants: Primary target (powerless/trapped) — bears
 *   the arrangement's total cost - wrongfully_convicted_capital_defendants:
 *   Primary target, innocent subclass (powerless/trapped) — bears the full
 *   cost without the culpability premise - families_of_the_condemned:
 *   Collateral target (powerless/trapped) — lose kin to the process -
 *   future_potential_murder_victims: Primary beneficiary
 *   (powerless/trapped-to-vulnerability) — the claimed return; counterfactual
 *   and mute - survivors_of_homicide_victims: Incidental beneficiary
 *   (organized/constrained) — collect satisfaction and believed protection -
 *   taxpayers_in_retentionist_jurisdictions: Diffuse cost-bearer
 *   (moderate/constrained) — fund the fiscal premium -
 *   capital_jurisdiction_state_authorities: Agenda setter
 *   (institutional/constrained) — administer the arrangement and collect its
 *   legitimacy yield - criminological_research_bodies: Analytical observer
 *   (analytical/analytical) — evaluate the efficacy premise
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.82).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.7).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.82).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Capital Punishment as Conditional Deterrence Instrument").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal justice/political philosophy/constitutional law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '04f4c066-1f4e-4610-9eed-44313fab9247').
narrative_ontology:cs_kernel_codification('04f4c066-1f4e-4610-9eed-44313fab9247', formalized).
narrative_ontology:cs_authority_grounding('04f4c066-1f4e-4610-9eed-44313fab9247', expertise).
narrative_ontology:cs_interpretation_layer_present('04f4c066-1f4e-4610-9eed-44313fab9247').
narrative_ontology:cs_reading_relation('04f4c066-1f4e-4610-9eed-44313fab9247', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('04f4c066-1f4e-4610-9eed-44313fab9247', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('04f4c066-1f4e-4610-9eed-44313fab9247', foundational, execution_justified_solely_by_marginal_deterrence).
narrative_ontology:cs_axiom_status(execution_justified_solely_by_marginal_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('04f4c066-1f4e-4610-9eed-44313fab9247', execution_justified_solely_by_marginal_deterrence, empirically_contingent).
narrative_ontology:cs_axiom('04f4c066-1f4e-4610-9eed-44313fab9247', secondary, identified_condemned_life_tradeable_against_statistical_lives_saved).
narrative_ontology:cs_axiom_status(identified_condemned_life_tradeable_against_statistical_lives_saved, holdable).
narrative_ontology:cs_axiom_grounding('04f4c066-1f4e-4610-9eed-44313fab9247', identified_condemned_life_tradeable_against_statistical_lives_saved, instrumental).
narrative_ontology:cs_reference_frame('04f4c066-1f4e-4610-9eed-44313fab9247', efficacy_conditioned_execution_warrant).
narrative_ontology:cs_drift_state('04f4c066-1f4e-4610-9eed-44313fab9247', post_nrc_2012_synthesis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('04f4c066-1f4e-4610-9eed-44313fab9247', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, future_potential_murder_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, capital_jurisdiction_state_authorities).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_capital_defendants).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, wrongfully_convicted_capital_defendants).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, families_of_the_condemned).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, survivors_of_homicide_victims).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, taxpayers_in_retentionist_jurisdictions).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, marginal_deterrence_hypothesis).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, rational_actor_deterrence_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death after trial and direct appeal. Every remaining avenue — post-conviction review, clemency petition, commutation — runs through the same courts and executive offices that imposed the sentence, and none can be initiated from outside the system holding them. Execution arrives years or decades after sentencing; their consent, cooperation, or account of their own case plays no role in whether the practice continues.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_capital_defendants, payer,
    powerless, biographical, trapped, national).

% People sentenced to death for killings they did not commit, documented by DNA testing, recanted testimony, and investigative journalism. They bear the practice's full severity without the culpability premise that nominally selects its subjects; some are released after decades, others posthumously. Their route out runs through the same appellate machinery that failed them, staffed by institutions with reputations invested in their guilt.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, wrongfully_convicted_capital_defendants, payer,
    powerless, biographical, trapped, national).

% Parents, children, and spouses of the sentenced. They lose a family member twice — to the decades of death-row waiting and then to the execution itself — while carrying the stigma of association. They cannot exit the relationship and hold no standing in the sentencing or clemency process beyond that of petitioner.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, families_of_the_condemned, payer,
    powerless, biographical, trapped, local).

% The class of people who would be killed by future murderers if the threatened sanction changes no one's behavior. They never appear as identifiable individuals, cannot attest to any life actually saved, and collect only if the preventive premise holds. Their protection is the return the arrangement promises; their silence is structural, since the counterfactual in which they were victims never occurs.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, future_potential_murder_victims, beneficiary,
    powerless, generational, trapped, national).

% Family members of people already murdered. Many campaign for the practice's retention and expansion, testify at hearings, and witness executions, collecting a sense that the killing is answered and that others are protected. Community expectation inside bereaved networks binds most to support; a minority break ranks and organize against it, at relational cost.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, survivors_of_homicide_victims, beneficiary,
    organized, biographical, constrained, national).

% Fund the premium the practice carries over its nearest substitute: costlier trials, longer and more numerous appeals, specialized death-row housing, and execution litigation. The premium is documented in state fiscal studies; the promised return — prevented killings — is not observable in any taxpayer's experience. Their lever is the ballot box, exercised rarely and at long intervals.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, taxpayers_in_retentionist_jurisdictions, payer,
    moderate, biographical, constrained, national).

% Legislatures define capital crimes and fund the apparatus; courts impose sentences and review them; corrections departments house the condemned and carry out executions. Officials campaign on the practice, prosecutors build careers on capital cases, and each completed execution reaffirms the claim to ultimate penal authority. Individual agencies cannot stand down unilaterally; abolition happens only when legislatures accept the electoral risk, which in the remaining jurisdictions no governing coalition has.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, capital_jurisdiction_state_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Statistical agencies, academic criminologists, and review panels that evaluate whether executions prevent homicides. They publish findings, testify, and are cited selectively by both sides; their conclusions bind no one and their funding does not depend on the answer.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, criminological_research_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, capital_jurisdiction_state_authorities).
narrative_ontology:fixing_cost_class(state_killing_authority__deterrence_instrument, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses the collective problem of protecting members from intentional homicide by attaching the state's maximum credible sanction to murder, aiming to raise the expected price of killing above what lesser sanctions achieve and to concentrate societal commitment behind a single calibrated penalty.
% TRANSFER_FUNCTION: Moves the condemned person's life, and their family's companionship, to the state's penal objective; moves a documented fiscal premium from taxpayers to the legal apparatus; moves claimed risk reduction to the general population; and moves legitimacy and electoral capital to the officials who maintain the arrangement.
% ABSENT_VOICES: The beneficiary class itself is absent by construction: future murder victims whose lives deterrence would save cannot speak, and no one can attest firsthand to being deterred or saved — the arrangement's entire return is counterfactual. The wrongfully executed are permanently silent. The condemned's own account of their case is structurally discounted, since their individual culpability is irrelevant to whether executing them changes third-party behavior. Every voice that could confirm the premise from inside is gone; only statistical proxies remain.
% DISAPPEARANCE_RATIONALE: Death rows would be resentenced to the next-highest sanction within months; capital statutes, specialized sentencing hearings, the appellate ladder's capital track, clemency machinery, and execution protocols would be dismantled or repurposed; prosecutor charging strategy and defense-resource allocation would reorganize; the deterrence-research program would lose its object; and the political coalitions organized around the penalty would dissolve or redirect.
% FOUNDING_PROBLEM: Once Enlightenment scrutiny and rights-conscious constitutionalism stripped execution of self-evident legitimacy, retentionist legal orders needed a secular, measurable warrant for the state to kill; this reading was constructed to supply one — re-grounding the practice in prevented homicides rather than desert, divine sanction, or sovereign prerogative.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the National Research Council's 2012 committee on deterrence and the death penalty concluded that the existing studies are not informative about whether the practice deters — an external attestation that the founding problem's solution remains unestablished; Beccaria's 1764 engagement with the same efficacy question, from outside any benefiting party, concluded that perpetual imprisonment deters at least as strongly at lower cost; successive state abolition commissions reached parallel findings. Retentionist legislatures attest the problem is live; no source outside the benefiting parties attests that it is solved.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.82, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored high (0.82) because the arrangement's per-application cost is total — the condemned person's life — and the documented wrongful-execution subclass means the cost demonstrably includes people selected by error rather than culpability; lengthening death-row residence draws the cost out over decades. Suppression (0.70) is a raw structural property, unscaled by power or scope: the condemned are physically and legally trapped, every exit route runs through the institutions imposing the sentence, and in retentionist cores the political space for the nearest substitute (life without parole) is actively narrowed by tough-on-crime enforcement politics. Theater (0.46, rising through the series) reflects the growing ceremonial share: witness-limited executions, clemency rituals that almost never commute, and appellate ceremony — the signaling function the deterrence premise needs has thinned even as ritual thickened. Accessibility collapse is LOW (0.30): life imprisonment without parole is a fully workable substitute that already absorbs the function in half the country, so understanding the arrangement does not foreclose alternatives. Resistance is HIGH (0.78): sustained abolition organizing, exoneree-led coalitions, victims'-family opposition networks, European supply pressure on execution drugs, and repeated legislative repeal. Coalition note: the condemned cannot coalition (isolated by design), but families and exonerees did — that coalition is the principal resistance channel. The suppression series dips at 2005 (the moratorium wave) then hardens past its 1995 peak (the drug-supply crisis answered with secrecy statutes and improvised protocols) — enforcement-capacity change is the dynamic the series tracks. Identity-lock operates on two seats: survivor advocacy fuses with grief identity, and the bureaucratic caretakers of the apparatus fuse with its institutional identity; breaking either frame would shift their computed position substantially.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same statute as four different arrangements. From the condemned seat the practice is total uncompensated cost with zero exit. From the future-victim seat it is pure protection — but a protection no member of that class can ever verify or attest, which makes the seat's subsidy wholly conditional on an empirical premise the seat cannot check. From the state-authority seat it is a coordination instrument it administers and a recurring legitimacy yield. From the taxpayer seat it is a documented fiscal premium purchased against an unobservable return. The engine computes these divergent per-seat classifications from the structural data; the divergence, not any single seat's verdict, is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Condemned and wrongfully convicted defendants are declared victims with trapped exit — derivation places them near the full-target end, with trapping amplifying effective extraction to its ceiling. Families of the condemned are declared victims whose trap is relational rather than custodial — high target-side d. Taxpayers bear pure cost with no verifiable benefit stream — moderately high d. Future potential murder victims are declared beneficiaries — derivation places them near the beneficiary end; their trapped exit encodes vulnerability to murder, not extraction, and the beneficiary declaration dominates. Survivors are incidental beneficiaries — low-to-moderate d. State authorities administer the arrangement and collect its legitimacy yield — low d. No directionality overrides are authored: the derivation chain produces the correct structure from the declarations, and the one genuinely ambiguous seat (future victims, trapped-and-benefiting) is handled by declaration priority rather than an override. Receipt note: the arrangement's yields — penal authority, electoral capital, institutional budgets — demonstrably accrue to the state-authority seat, which is why gain_flow names it; fixing_cost is authored 'prohibitive' because in the residual retentionist core the political cost of repeal exceeds any governing coalition's appetite, as the frozen jurisdictional map of the last decade attests.
 *
 * MANDATROPHY ANALYSIS:
 *   Authoring tangled_rope blocks two symmetrical mislabels. Reading the arrangement as pure extraction (the abolitionist totalization) erases the genuine coordination aim the reading is built on — homicide prevention is a real collective-action problem, and the arrangement exists to address it by pricing murder at the state's maximum credible sanction. Reading it as pure coordination (the retentionist totalization) erases the total, asymmetric cost the same structure imposes on the condemned and their families. The hybrid is the honest shape: coordination function and extraction ride one apparatus. On obsolescence: the founding problem — a secular, measurable warrant for state killing — is contested rather than dead, so no zombie flag fires; but the combination of contested founding status, rising theater, and an efficacy premise left undemonstrated by the strongest available external review is the exact precursor profile for drift into performance-maintained persistence. The temporal series is the watch item: if theater crosses 0.5 while the efficacy omega resolves null, the arrangement is coasting on inherited authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_deterrence_efficacy,
    'Does capital punishment actually prevent homicides at the margin relative to life imprisonment without parole — the empirical premise on which this reading''s entire justification rests?',
    'Panel-grade synthesis of natural experiments (execution moratoria, highly publicized botched executions, matched state-pair comparisons) meeting the methodological standards the 2012 National Research Council committee specified; randomized designs are unavailable, so identification turns on discontinuity and interrupted-time-series designs.',
    'A robust null dissolves the coordination half of the arrangement''s hybrid structure and leaves pure extraction on the condemned; a robust positive effect confirms genuine coordination and makes the ''iff'' condition satisfiable as stated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_deterrence_efficacy, empirical, 'Whether the deterrence premise of the conditional justification holds.').

omega_variable(
    acceptable_cost_threshold_indeterminacy,
    'What quantity of wrongful executions, fiscal premium, and condemned-family harm does ''acceptable cost'' permit, and who is entitled to set that threshold — the reading''s second condition is a value judgment it cannot settle internally?',
    'No empirical resolution is available; resolution would require an explicit political specification of the cost ceiling (for example, a statutory innocent-life tradeoff ratio), which no retentionist jurisdiction has enacted.',
    'If no threshold can be specified, the ''iff'' condition is unfalsifiable as applied and the justification reduces to assertion; a specified threshold converts the reading into a computable and auditable tradeoff.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_threshold_indeterminacy, preference, 'The cost side of the conditional justification is normatively underdetermined.').

omega_variable(
    wrongful_conviction_irreducible_floor,
    'What fraction of death sentences are wrongful, and is that fraction reducible below any level compatible with continued use?',
    'Cumulative death-row exoneration rates adjusted for detection probability as DNA testing and prosecutor-led sentence reviews expand; comparison of error rates across capital and otherwise-comparable non-capital homicide prosecutions.',
    'Sets the floor on innocent extraction inside the cost term; an irreducible positive floor means the acceptable-cost clause must openly price innocent executions, which the reading currently avoids stating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_conviction_irreducible_floor, empirical, 'Irreducible wrongful-execution rate bearing on the cost condition.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the deterrence_instrument reading of the state_killing_authority kernel — would the retributive_desert or categorical_abolition readings of the same kernel change the beneficiary/victim structure and the reading-indexed epsilon, and where exactly is the disagreement located?',
    'Conceptual: compare the three sibling stories'' structural declarations — retributive_desert seats the guilty murderer as the proper object of the arrangement with no future-victim beneficiary; categorical_abolition seats every person as protected and the state as rights-violator; the disagreement is located in the foundational axiom (contingent efficacy versus proportional desert versus categorical bar).',
    'Switching readings changes which seats count as beneficiaries (future potential victims enter the set only under this reading), changes the reading-indexed epsilon over the fixed referent, and activates or deactivates the foreclosure relation this reading holds toward categorical_abolition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one kernel, three readings, disagreement located in the foundational axiom.').

omega_variable(
    theater_drift_attribution,
    'Is the rising theater ratio Goodhart drift (signaling and ritual substituting for a deterrence function that was never demonstrated) or adaptive maintenance under supply shock (drug scarcities and secrecy laws forcing improvisation that merely looks theatrical)?',
    'Compare ceremonial-intensity indicators across jurisdictions with and without execution-drug supply disruption; if ritual thickening proceeds uniformly regardless of supply conditions, the drift is functional atrophy rather than logistical adaptation.',
    'Goodhart attribution supports piton-drift monitoring (function replaced by performance); supply-shock attribution keeps the function nominally intact and locates degradation in enforcement logistics instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_drift_attribution, empirical, 'Attribution of the rising performative share of the arrangement''s activity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_authority__deterrence_instrument, theater_ratio, 1976, 0.24).
narrative_ontology:measurement_basis(stat_tr_t1976, observed).
narrative_ontology:measurement(stat_tr_t1985, state_killing_authority__deterrence_instrument, theater_ratio, 1985, 0.28).
narrative_ontology:measurement_basis(stat_tr_t1985, observed).
narrative_ontology:measurement(stat_tr_t1995, state_killing_authority__deterrence_instrument, theater_ratio, 1995, 0.33).
narrative_ontology:measurement_basis(stat_tr_t1995, observed).
narrative_ontology:measurement(stat_tr_t2005, state_killing_authority__deterrence_instrument, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(stat_tr_t2005, observed).
narrative_ontology:measurement(stat_tr_t2012, state_killing_authority__deterrence_instrument, theater_ratio, 2012, 0.43).
narrative_ontology:measurement_basis(stat_tr_t2012, observed).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__deterrence_instrument, theater_ratio, 2024, 0.46).
narrative_ontology:measurement_basis(stat_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_authority__deterrence_instrument, base_extractiveness, 1976, 0.78).
narrative_ontology:measurement_basis(stat_be_t1976, observed).
narrative_ontology:measurement(stat_be_t1985, state_killing_authority__deterrence_instrument, base_extractiveness, 1985, 0.79).
narrative_ontology:measurement_basis(stat_be_t1985, observed).
narrative_ontology:measurement(stat_be_t1995, state_killing_authority__deterrence_instrument, base_extractiveness, 1995, 0.8).
narrative_ontology:measurement_basis(stat_be_t1995, observed).
narrative_ontology:measurement(stat_be_t2005, state_killing_authority__deterrence_instrument, base_extractiveness, 2005, 0.81).
narrative_ontology:measurement_basis(stat_be_t2005, observed).
narrative_ontology:measurement(stat_be_t2012, state_killing_authority__deterrence_instrument, base_extractiveness, 2012, 0.82).
narrative_ontology:measurement_basis(stat_be_t2012, observed).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__deterrence_instrument, base_extractiveness, 2024, 0.82).
narrative_ontology:measurement_basis(stat_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_authority__deterrence_instrument, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement_basis(stat_su_t1976, observed).
narrative_ontology:measurement(stat_su_t1985, state_killing_authority__deterrence_instrument, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement_basis(stat_su_t1985, observed).
narrative_ontology:measurement(stat_su_t1995, state_killing_authority__deterrence_instrument, suppression_requirement, 1995, 0.66).
narrative_ontology:measurement_basis(stat_su_t1995, observed).
narrative_ontology:measurement(stat_su_t2005, state_killing_authority__deterrence_instrument, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement_basis(stat_su_t2005, observed).
narrative_ontology:measurement(stat_su_t2012, state_killing_authority__deterrence_instrument, suppression_requirement, 2012, 0.67).
narrative_ontology:measurement_basis(stat_su_t2012, observed).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__deterrence_instrument, suppression_requirement, 2024, 0.7).
narrative_ontology:measurement_basis(stat_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% The colloquial label 'capital punishment' decomposes into three structurally distinct constraints sharing one kernel (state_killing_authority): this deterrence_instrument reading (conditional, efficacy-grounded; future potential victims as beneficiaries, the condemned as instrumental cost), the retributive_desert reading (unconditional, desert-grounded; the guilty murderer as the proper object, no future-victim beneficiary), and the categorical_abolition reading (unconditional prohibition; every person protected, the state as rights-violator). Each sibling gets its own reading-indexed epsilon over the same referent — the standing execution regime — because epsilon is a property of the reading while the referent is fixed. This reading holds a foreclosure edge toward categorical_abolition (conditional permissibility and categorical impermissibility cannot coexist in one framework) and a coexistence edge toward retributive_desert (conjunctive hybrid positions are coherent). Historically the retributive reading supplied the practice's legitimacy; this reading now supplies its modern defense, so each pressures the other's operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
