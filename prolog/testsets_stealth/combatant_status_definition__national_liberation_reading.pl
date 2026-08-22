% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: AP I Article 1(4) National Liberation Combatant Status Extension
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   Additional Protocol I (1977), Article 1(4) extends the international
 *   armed conflict framework — and with it combatant privilege and
 *   prisoner-of-war protections — to organized armed forces fighting
 *   'colonial domination and alien occupation and racist regimes' in exercise
 *   of the right of self-determination, provided they meet organization and
 *   responsible-command criteria drawn from Geneva III Article 4A. This story
 *   authors ONE reading of that arrangement: the national-liberation reading,
 *   under which the extension is a genuine (if conditional and gatekept)
 *   widening of lawful belligerency. The epsilon referent is the standing
 *   arrangement under contest — the combatant-status allocation regime as
 *   structured by this reading — assessed by the reading's own lights: the
 *   reading endorses the extension yet still registers the obligations it
 *   imposes on states that rejected the bargain and the recognition gate that
 *   remains in adverse hands, hence a moderate base epsilon of 0.48 rather
 *   than a near-zero figure. The claim/metric pair is authored independently:
 *   the claimed type is what I believe structurally true (a hybrid with a
 *   real coordination core and a real asymmetric-transfer shell), and the
 *   metrics describe the arrangement's actual fifty-year operation. Family
 *   note: the colloquial label 'combatant status' decomposes into three
 *   structurally distinct claims; this file links to its two siblings via the
 *   network section, and the contest itself is carried in kernel_context and
 *   the committer omega, not inside the classification.
 *
 * KEY AGENTS:
 *   - - national_liberation_movements: Primary beneficiary (organized/constrained) — claims conditional lawful-belligerent standing for organized forces in self-determination conflicts
 *   - - captured_liberation_fighters: Direct beneficiary (powerless/trapped) — receive prisoner-of-war treatment when the extension applies, criminal trial when it does not
 *   - - occupying_administrative_powers: Primary payer and de facto gatekeeper (institutional/arbitrage) — owe detention duties to qualifying detainees and shape whether any conflict is treated as qualifying
 *   - - detaining_state_judicial_systems: Secondary payer (institutional/constrained) — surrender prosecutorial jurisdiction over qualifying detainees
 *   - - major_nonparty_military_powers: Excluded obligors (powerful/arbitrage) — principal intended duty-bearers who stand outside the regime
 *   - - icrc_protection_mandate: Analytical observer (institutional/analytical) — monitors treatment and interprets scope in the field
 *   - - sponsoring_state_coalition: Agenda-setter and normative beneficiary (organized/mobile) — drafted the provision and maintains it diplomatically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.48).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.38).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "AP I Article 1(4) National Liberation Combatant Status Extension").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, 'e4072921-38ef-4667-b9f5-50c02a483cb4').
narrative_ontology:cs_kernel_codification('e4072921-38ef-4667-b9f5-50c02a483cb4', fixed_text).
narrative_ontology:cs_authority_grounding('e4072921-38ef-4667-b9f5-50c02a483cb4', lineage).
narrative_ontology:cs_interpretation_layer_present('e4072921-38ef-4667-b9f5-50c02a483cb4').
narrative_ontology:cs_reading_relation('e4072921-38ef-4667-b9f5-50c02a483cb4', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('e4072921-38ef-4667-b9f5-50c02a483cb4', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('e4072921-38ef-4667-b9f5-50c02a483cb4', foundational, combatant_status_tracks_self_determination_cause).
narrative_ontology:cs_axiom_status(combatant_status_tracks_self_determination_cause, holdable).
narrative_ontology:cs_axiom_grounding('e4072921-38ef-4667-b9f5-50c02a483cb4', combatant_status_tracks_self_determination_cause, deontological).
narrative_ontology:cs_axiom('e4072921-38ef-4667-b9f5-50c02a483cb4', secondary, organization_criteria_suffice_for_immunity).
narrative_ontology:cs_axiom_status(organization_criteria_suffice_for_immunity, holdable).
narrative_ontology:cs_axiom_grounding('e4072921-38ef-4667-b9f5-50c02a483cb4', organization_criteria_suffice_for_immunity, instrumental).
narrative_ontology:cs_reference_frame('e4072921-38ef-4667-b9f5-50c02a483cb4', self_determination_inclusive_belligerency).
narrative_ontology:cs_drift_state('e4072921-38ef-4667-b9f5-50c02a483cb4', post_decolonization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e4072921-38ef-4667-b9f5-50c02a483cb4', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, captured_liberation_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_administrative_powers).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, detaining_state_judicial_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, sponsoring_state_coalition).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, self_determination_principle).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, geneva_framework_universalization_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized armed wings of peoples asserting self-determination against colonial administration, foreign occupation, or minority racial rule. They maintain command structures and claim lawful-belligerent standing for their members; captured members are entitled to prisoner-of-war treatment when the conflict is recognized as falling within the protocol's scope. That recognition rests with the adverse party and with international organs, so the standing they claim is conditional on determinations they do not control. Abandoning the claim would mean accepting criminal-capture framing of their own members, which their struggle identity does not permit.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, generational, constrained, regional).

% Rank-and-file members detained by the adverse party. When the status extension applies they must be held as prisoners of war — immune from prosecution for lawful acts of war, entitled to camp conditions and eventual repatriation. When it does not apply they face domestic criminal trial, potentially with severe penalties. They control neither the determination nor their detention; their access to protection runs entirely through decisions taken above and beyond them.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, captured_liberation_fighters, beneficiary,
    powerless, immediate, trapped, national).

% States administering territory whose population resists under organized command. They owe prisoner-of-war treatment to qualifying detainees, lose the ability to try such detainees as common criminals, and bear the security consequences of lawful-belligerent immunity for operations against occupation assets. Several declined to join the protocol or filed declarations refusing the scope extension, keeping a treaty-level exit open; those that remain inside administer day-to-day application and effectively shape whether any conflict is ever treated as qualifying.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_administrative_powers, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, occupying_administrative_powers, agenda_setter).

% Domestic courts and prosecutors of capturing states. Where the extension applies they must divert captured fighters out of criminal dockets into prisoner-of-war custody, surrendering jurisdiction they would otherwise exercise; where it does not they try insurgents under domestic security law. Their caseload and doctrine shift with each recognition decision made elsewhere, over which they have no vote.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, detaining_state_judicial_systems, payer,
    institutional, biographical, constrained, national).

% Large military states that stayed outside the protocol or announced they would not accept the scope extension. They are the regime's principal intended obligors yet sit outside its governance: they neither ratify, nor vote on interpretations, nor accept determinations, while their practice is cited by both sides of every argument about the provision's reach. Their objections were recorded at the diplomatic conference and remain the main counterweight to the extension's authority.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, major_nonparty_military_powers, excluded,
    powerful, generational, arbitrage, global).

% The neutral humanitarian agency mandated to visit prisoners, register detainees, and monitor treatment standards in armed conflict. It interprets and applies the status framework in the field, publishes authoritative commentary on its scope, and its determinations about whom it may visit and in what capacity carry practical weight even where formal recognition of a conflict's qualification is absent.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, icrc_protection_mandate, observer,
    institutional, generational, analytical, global).

% The bloc of non-aligned and socialist states that drafted and carried the scope extension through the 1974-1977 diplomatic conference. They continue to defend the provision in United Nations fora, sponsor reaffirming resolutions, and press for its application in live occupation disputes. They collect normative vindication whenever the framework is invoked and invest diplomatic capital in keeping it on the agenda, at little material cost to themselves.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, sponsoring_state_coalition, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, sponsoring_state_coalition, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends a defined legal status — combatant privilege and prisoner-of-war protections — into conflicts the interstate Geneva framework did not reach, so that organized irregular forces have a known standing, captors have known treatment duties, and fighters have a concrete incentive to observe the laws of war.
% TRANSFER_FUNCTION: Moves legal standing and immunity from the exclusive preserve of state militaries to organized non-state forces in self-determination conflicts, and correspondingly moves prosecutorial discretion away from capturing states, which must hold qualifying detainees as prisoners of war rather than try them.
% ABSENT_VOICES: Civilian populations of occupied territories, who live with the security consequences of lawful-belligerent immunity yet had no seat at the diplomatic conference; judges and prosecutors of capturing states, whose jurisdiction shifts with decisions taken in treaty bodies they do not staff; and the major non-party powers, whose objections are on the record but who stand outside the regime they are asked to obey.
% DISAPPEARANCE_RATIONALE: Movement seats report that their members' status and protection would collapse back into criminal-capture practice overnight, and the judicial seats report their dockets would refill. Adverse-party and non-party seats report that nothing observable would change, since no conflict has ever been formally determined to qualify and the machinery never runs. Both reports are sincere; the arrangement's load-bearing role differs radically by seat, so the parties dispute the verdict itself rather than the facts behind it.
% FOUNDING_PROBLEM: Wars of national liberation against colonial domination, alien occupation, and racist regimes fell outside the prisoner-of-war regime because they were not fought between state armies: captured fighters faced execution or criminal trial as bandits, and the absence of any lawful-standing prospect removed their incentive to observe the laws of war.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: ICRC commentary to the protocols acknowledges the pre-1977 status gap for wars of self-determination; the 1974-1977 diplomatic conference records show the gap framed by delegations across blocs; and United Nations General Assembly resolutions — adopted over the objection of several military powers — attest that the alien-occupation limb is regarded as live. Against this, the declarations of the major non-party powers attest that they regard the problem as mischaracterized or resolved. The attestation is therefore split along the same lines as the seats, and no single external source settles it.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, contested).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48: the arrangement imposes real, non-consensual obligations (prisoner-of-war treatment, surrendered prosecutorial jurisdiction) on states that expressly rejected the bargain, while the benefit it grants the other side is conditional on a recognition determination no organ has ever made — a moderate, partly latent transfer. Suppression 0.38 is the raw structural property, unscaled by power or scope: the enforcement machinery is thin, a treaty-level exit remains open and was exercised by the largest military powers, but for states inside the regime the duty binds regardless of preference. Theater 0.38: after five decades not a single conflict has been formally determined to qualify, while the provision is kept alive by reaffirming resolutions and doctrinal citation — a growing share of the arrangement's activity is rhetorical maintenance of machinery that never runs. Accessibility_collapse 0.40: alternatives persist and govern most actual practice — the state-centric baseline still decides detention outcomes in nearly all conflicts, and non-ratification remains available. Resistance 0.70: recorded declarations refusing the scope extension, non-ratification by major military powers, and sustained doctrinal rejection. Temporal series share one grid (t = 0, 10, 20, 30, 40, 49, spanning 1977–2026) with every tracked metric authored at every point: base_extractiveness rises then plateaus (accumulation is latent, parked in the recognition gap rather than realized), theater_ratio rises monotonically (application failing while rhetoric continues), and suppression_requirement falls (enforcement capacity attrited as the major powers stood outside — a decay trajectory, authored because the story specifically tracks enforcement-capacity change). The suppression scalar and the suppression_requirement series measure different things: the scalar is the structural coercive residue on parties; the series is the operating enforcement machinery, which decayed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the occupying-power seat the arrangement presents as an obligation imposed over recorded objection, administered by others, and avoided in practice — an enforced-transfer experience. From the movement seat it presents as earned standing withheld by an uncooperative gatekeeper — an incomplete-recognition experience. From the captured-fighter seat it is a lottery: the same capture yields prisoner-of-war treatment or criminal trial depending on a determination made far above them. The sponsoring coalition experiences vindication; the excluded non-party powers experience being addressed by a rule they never accepted. The engine derives these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the movement and fighter seats toward the beneficiary end (low d); the fighter seat's trapped exit pins it nearest full subsidy, the movement's constrained exit slightly less so. Victim declarations drive the occupying-power and judicial-system seats toward the target end (high d); the occupying powers' arbitrage-grade exit (non-ratification, declarations) then damps their effective burden in the engine's arithmetic — which is descriptively correct, since the regime barely binds those who left. The sponsoring coalition declares as agenda-setter with a beneficiary secondary role (low d, it collects normative returns without bearing duties); the ICRC sits at the analytical seat with no extraction vector; the non-party powers are excluded with arbitrage exit, outside the regime's pull altogether. No directionality overrides are authored: the derivation from declarations plus exit atoms reproduces every relationship, and the one candidate distortion (arbitrage pulling the occupying-power seat's d toward the beneficiary end) is correctly handled as exit-modulation of effective burden rather than as a misread structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is half-dead: colonial domination and racist-regime limbs have largely lapsed with decolonization, while the alien-occupation limb is invoked in live disputes. The arrangement persists with rising theatrical maintenance and zero formal applications — the classic profile of machinery kept running rhetorically. The cost-asymmetry test finds no administrator with skin in the game: the sponsoring coalition maintains the provision at negligible cost because invocation is free, and the adverse powers ignore it at negligible cost because non-application is free; nobody profits enough to fix it and nobody hurts enough to scrap it, which is what lets it drift toward inertia. The R5 mismatch consumer watches the (contested status x contested verdict) cell here: if the founding problem resolves dead while the movement seats still depend on the arrangement, the zombie flag should fire. The classification discipline cuts both ways: reading the arrangement as pure coordination ignores the transfer imposed on rejecting states and the gate the paying side controls; reading it as pure extraction ignores the real status vacuum it was built to fill and the compliance incentive a defined standing offers irregular forces. The hybrid claim keeps both halves visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the national_liberation_reading of the combatant_status_definition kernel; how would the sibling readings (state_centric_reading, functional_protection_reading) restructure the beneficiary/victim surface and the resulting classifications?',
    'Comparative read of the three linked family stories: the state-centric sibling deletes the liberation-movement beneficiary seat and recasts fighters as criminal defendants; the functional-protection sibling flattens status into universal detainee minimums, equalizing the computed burden across detainee seats.',
    'The kernel''s allocation regime classifies as exclusionary (state-centric), status-stratified (this reading), or status-indifferent (functional protection); where extraction concentrates moves accordingly. Cross-reading comparison is only valid across the separate files, never by averaging inside one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    qualification_gatekeeping_void,
    'Who decides whether a given conflict qualifies under Article 1(4), given that no competent organ has ever issued a binding determination in roughly five decades?',
    'State-practice survey, or a formal determination by an international organ or the ICRC in a live occupation dispute.',
    'If gatekeeping remains with the adverse party, the movement-side benefit is largely contingent and the arrangement''s transfer runs through a valve the paying seat controls; an independent determiner would convert conditional standing into an enforceable entitlement and raise the paying seats'' effective burden sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qualification_gatekeeping_void, empirical, 'Unassigned gatekeeping authority over the scope determination.').

omega_variable(
    customary_hardening_status,
    'Has the Article 1(4) status extension hardened into customary international law binding non-parties, or does it remain treaty-relative?',
    'ICJ and ad hoc tribunal pronouncements, military-manual surveys, and opinio juris studies of major non-party powers.',
    'Customary status would extend the obligation to the major non-party military powers, collapsing their treaty-level exit and raising their effective burden substantially; treaty-relative status preserves that exit and keeps the arrangement''s reach partial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_hardening_status, empirical, 'Whether the extension binds beyond the treaty parties.').

omega_variable(
    dormancy_interpretation,
    'Does the absence of any formal Article 1(4) determination indicate the provision is obsolete (its founding problem dissolved) or that it successfully raises the cost of the conduct it targets?',
    'Counterfactual analysis of decolonization-era and occupation-era conflicts: whether conduct, detention practice, or negotiation positions would plausibly have differed had the provision operated.',
    'The obsolescence reading supports drift toward inertial, rhetorically maintained operation; the deterrence reading supports live coordination value and argues against resolving the mandate as dead. The two readings diverge on whether the rising theater series signals decay or quiet success.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_interpretation, conceptual, 'Whether non-application evidences obsolescence or deterrent effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 0, 49).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__national_liberation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(comb_tr_t0, observed).
narrative_ontology:measurement(comb_tr_t10, combatant_status_definition__national_liberation_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(comb_tr_t10, observed).
narrative_ontology:measurement(comb_tr_t20, combatant_status_definition__national_liberation_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(comb_tr_t20, observed).
narrative_ontology:measurement(comb_tr_t30, combatant_status_definition__national_liberation_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(comb_tr_t30, observed).
narrative_ontology:measurement(comb_tr_t40, combatant_status_definition__national_liberation_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(comb_tr_t40, observed).
narrative_ontology:measurement(comb_tr_t49, combatant_status_definition__national_liberation_reading, theater_ratio, 49, 0.38).
narrative_ontology:measurement_basis(comb_tr_t49, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__national_liberation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(comb_be_t0, observed).
narrative_ontology:measurement(comb_be_t10, combatant_status_definition__national_liberation_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(comb_be_t10, observed).
narrative_ontology:measurement(comb_be_t20, combatant_status_definition__national_liberation_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(comb_be_t20, observed).
narrative_ontology:measurement(comb_be_t30, combatant_status_definition__national_liberation_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(comb_be_t30, observed).
narrative_ontology:measurement(comb_be_t40, combatant_status_definition__national_liberation_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement_basis(comb_be_t40, observed).
narrative_ontology:measurement(comb_be_t49, combatant_status_definition__national_liberation_reading, base_extractiveness, 49, 0.48).
narrative_ontology:measurement_basis(comb_be_t49, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__national_liberation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(comb_su_t0, observed).
narrative_ontology:measurement(comb_su_t10, combatant_status_definition__national_liberation_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(comb_su_t10, observed).
narrative_ontology:measurement(comb_su_t20, combatant_status_definition__national_liberation_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(comb_su_t20, observed).
narrative_ontology:measurement(comb_su_t30, combatant_status_definition__national_liberation_reading, suppression_requirement, 30, 0.37).
narrative_ontology:measurement_basis(comb_su_t30, observed).
narrative_ontology:measurement(comb_su_t40, combatant_status_definition__national_liberation_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement_basis(comb_su_t40, observed).
narrative_ontology:measurement(comb_su_t49, combatant_status_definition__national_liberation_reading, suppression_requirement, 49, 0.33).
narrative_ontology:measurement_basis(comb_su_t49, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'combatant status' covers three structurally distinct claims with different epsilon values, victim sets, and failure modes, so the kernel emits three stories. The state-centric reading is the inherited baseline (upstream: it is the rule most actual detention practice still follows); this national-liberation reading modifies the baseline for one conflict class by adding a cause-based route to standing; the functional-protection reading bypasses status allocation entirely and lays a status-independent minimum floor beneath all detainees. Each story links to the other two; the upstream baseline is routinely cited as authority both for and against this reading, which is why the edges run in both directions across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
