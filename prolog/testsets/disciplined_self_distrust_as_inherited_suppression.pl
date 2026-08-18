% ============================================================================
% CONSTRAINT STORY: disciplined_self_distrust_as_inherited_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_disciplined_self_distrust_as_inherited_suppression, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: disciplined_self_distrust_as_inherited_suppression
 *   human_readable: Disciplined Self-Distrust: Inherited Deferral of Perceptual Authority
 *   domain: institutional/psychological/political
 *
 * SUMMARY:
 *   Across professions and institutional lineages where perceptual or
 *   intuitive judgment matters — frontline medicine, intelligence analysis,
 *   disaster early-warning, field engineering, minority-community
 *   risk-sensing — capacity-holders are trained, generation over generation,
 *   to treat their own private sensing as provisionally invalid until an
 *   external authority confirms it. The rule is taught and experienced as
 *   discipline, humility, and professional integrity, not as suppression.
 *   Early in a career this genuinely catches error. But the lag is codified
 *   as a fixed rule rather than a calibrated one: it does not shrink as the
 *   capacity-holder's private judgment is repeatedly vindicated. The rule is
 *   inherited as the capacity-holder trains the next generation to distrust
 *   themselves in turn, transmitting the deferral requirement as an
 *   unquestioned feature of competence itself. The structural crisis case is
 *   the one where the withheld private knowledge — a clinician's early read
 *   of a deteriorating patient, a sensor operator's early read of an anomaly,
 *   a community elder's early read of encroaching danger — was correct, and
 *   the confirmation lag converted timely warning into fatal delay. This is
 *   claimed rope (self-discipline, genuine coordination against error) but
 *   authored with tangled_rope metrics because the persistence of the lag
 *   past demonstrated reliability, and the institutional capture of the
 *   confirming function, indicate active extraction riding on the
 *   coordination story.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(disciplined_self_distrust_as_inherited_suppression, 0.71).
domain_priors:suppression_score(disciplined_self_distrust_as_inherited_suppression, 0.86).
domain_priors:theater_ratio(disciplined_self_distrust_as_inherited_suppression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(disciplined_self_distrust_as_inherited_suppression, extractiveness, 0.71).
narrative_ontology:constraint_metric(disciplined_self_distrust_as_inherited_suppression, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(disciplined_self_distrust_as_inherited_suppression, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(disciplined_self_distrust_as_inherited_suppression, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(disciplined_self_distrust_as_inherited_suppression, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(disciplined_self_distrust_as_inherited_suppression, tangled_rope).
narrative_ontology:human_readable(disciplined_self_distrust_as_inherited_suppression, "Disciplined Self-Distrust: Inherited Deferral of Perceptual Authority").
narrative_ontology:topic_domain(disciplined_self_distrust_as_inherited_suppression, "institutional/psychological/political").

domain_priors:requires_active_enforcement(disciplined_self_distrust_as_inherited_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(disciplined_self_distrust_as_inherited_suppression, central_authority).
narrative_ontology:constraint_victim(disciplined_self_distrust_as_inherited_suppression, capacity_holder_and_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(disciplined_self_distrust_as_inherited_suppression, confirming_authority_apparatus).
narrative_ontology:constraint_victim(disciplined_self_distrust_as_inherited_suppression, training_institution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the training curriculum that teaches capacity-holders (sensors, field operators, junior clinicians, frontline analysts, apprentices) to withhold private perceptual judgment until an external confirming signal arrives. Frames this as professional discipline, epistemic humility, and quality control. Captures the benefit of a predictable, centrally-routed information chain: no capacity-holder acts or speaks without passing through the authority's confirmation bottleneck, which preserves the authority's monopoly on legitimate judgment and insulates it from being bypassed or found unnecessary.
narrative_ontology:constraint_stakeholder(disciplined_self_distrust_as_inherited_suppression, central_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(disciplined_self_distrust_as_inherited_suppression, central_authority, beneficiary).

% Trained from entry into the role to treat their own private sensing — a felt anomaly, an early-warning pattern, a clinical instinct, a read of an unfolding situation — as inherently unreliable until confirmed externally. The lag between private knowledge and permitted disclosure is measured in the training itself: 'wait for confirmation' is codified as a rule, not merely encouraged as caution. Even after their private sensing proves correct case after case, the lag persists — reinforced as evidence that discipline is working, not that it is costly. Dependents (patients, subordinates, communities relying on the capacity-holder's judgment) bear the downstream cost when the withheld signal was the one that mattered; in a crisis, the delay converts what could have been timely warning into fatal or near-fatal loss.
narrative_ontology:constraint_stakeholder(disciplined_self_distrust_as_inherited_suppression, capacity_holder_and_dependents, payer,
    moderate, biographical, identity_locked, local).

% Administers the pedagogy that transmits the deferral rule across cohorts — academies, residency programs, apprenticeship chains, intelligence training pipelines. Genuinely believes it is teaching rigor and error-reduction, and in fact does reduce some false-positive error. But it also absorbs the reputational cost when the deferral proves fatal, and has limited power to unilaterally revise doctrine set by the central authority above it.
narrative_ontology:constraint_stakeholder(disciplined_self_distrust_as_inherited_suppression, training_institution, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(disciplined_self_distrust_as_inherited_suppression, training_institution, payer).

% The formal apparatus (review boards, command hierarchies, senior sign-off chains) whose confirmation is required before the capacity-holder's perception becomes actionable. Its institutional relevance and authority are constituted by being the necessary gate; if capacity-holders' private judgment were trusted directly, this apparatus's confirming function — and much of its claim to necessity — would shrink.
narrative_ontology:constraint_stakeholder(disciplined_self_distrust_as_inherited_suppression, confirming_authority_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Patients, residents, communities, or subordinate units whose fate rides on whether the capacity-holder's early private signal reaches action in time. Have no voice in setting the deferral rule and often never learn that a signal existed and was withheld pending confirmation until after a crisis makes the record public.
narrative_ontology:constraint_stakeholder(disciplined_self_distrust_as_inherited_suppression, downstream_populations, excluded,
    powerless, immediate, trapped, regional).

% Post-incident review bodies, journalists, and historians who reconstruct the lag between private sensing and disclosure after a disaster, malpractice case, or intelligence failure, and who are typically the first to name the deferral rule as a contributing cause rather than as prudent discipline.
narrative_ontology:constraint_stakeholder(disciplined_self_distrust_as_inherited_suppression, crisis_investigators, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(disciplined_self_distrust_as_inherited_suppression, central_authority).
narrative_ontology:fixing_cost_class(disciplined_self_distrust_as_inherited_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely reduces some error: untrained or unconfirmed perceptual judgments, acted on directly, would sometimes be wrong, and a confirmation step catches a portion of those errors, especially early in a capacity-holder's development when calibration is unreliable.
% TRANSFER_FUNCTION: Moves the authority to act on time-sensitive knowledge from the person who has it earliest (the capacity-holder) to the institution positioned to confirm it later, and moves the cost of the resulting delay onto the capacity-holder's own credibility and onto dependents downstream who bear the consequences of late action.
% ABSENT_VOICES: Downstream populations who suffer when the withheld signal was correct have no role in setting or revising the deferral rule; the capacity-holders themselves, once identity-fused with the discipline, often do not experience the rule as suppression and so do not object even when they are the ones the rule harms.
% DISAPPEARANCE_RATIONALE: The central authority and training institutions would argue the world rearranges into chaos — unverified perception acted on directly, error rates rising. Capacity-holders whose private judgment has been repeatedly vindicated, and crisis investigators reviewing fatal lags, would argue the world rearranges toward faster, more accurate action — the deferral is removing information, not adding reliability, once the capacity-holder is sufficiently calibrated. Which reading is correct is precisely the contested empirical question the omega below tries to isolate.
% FOUNDING_PROBLEM: Early or uncalibrated perceptual judgment is genuinely error-prone; the deferral rule was built to prevent premature, wrong action by insufficiently experienced capacity-holders and to create an auditable chain of confirmed decisions.
% FOUNDING_PROBLEM_CORROBORATION: Central authority and training institutions attest the problem remains live — perceptual error is still possible. Crisis investigators, and capacity-holders whose independently-verified track record has since accumulated, attest that for experienced holders the founding problem is substantially resolved by track record itself, and that the persisting lag no longer functions as error-prevention but as institutional control; this outside corroboration is the basis for classifying the constraint as tangled rather than pure rope.
narrative_ontology:disappearance_verdict(disciplined_self_distrust_as_inherited_suppression, contested).
narrative_ontology:founding_problem_status(disciplined_self_distrust_as_inherited_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(disciplined_self_distrust_as_inherited_suppression, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(disciplined_self_distrust_as_inherited_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(disciplined_self_distrust_as_inherited_suppression, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(disciplined_self_distrust_as_inherited_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(disciplined_self_distrust_as_inherited_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(disciplined_self_distrust_as_inherited_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.38 to 0.71) because the diagnostic signal here is not the existence of the confirmation step but its non-adaptivity: a genuine error-correction mechanism would narrow the lag as the capacity-holder's track record accumulates, but the codified deferral rule does not narrow — if anything the theater_ratio climbs alongside it (0.22 to 0.58), meaning an increasing share of the confirmation ritual is performative validation of the hierarchy rather than functional error-catching. Suppression is high throughout and rises further (0.68 to 0.86) because the mechanism is not merely 'ask before acting' but 'distrust your own perception as a matter of professional identity' — internalized suppression that persists independent of the confirming authority's actual batting average. Accessibility collapse (0.62) reflects that once the discipline is internalized as integrity, the capacity-holder can no longer easily imagine acting on unconfirmed private knowledge even in situations of extreme time pressure; resistance is comparatively low (0.44) precisely because the suppression has been internalized rather than merely imposed.
 *
 * PERSPECTIVAL GAP:
 *   From the central authority's seat the arrangement looks like durable, functioning rigor — errors are still being caught, discipline is still producing calm, verifiable action. From the capacity-holder's seat, especially one whose private sensing has been vindicated repeatedly, the same structure computes as extraction: the lag no longer buys error-reduction, it buys institutional deference, and the capacity-holder pays for that deference in the currency of delayed, sometimes fatal, action. Dependents experience the gap most starkly and most silently — they generally never know a signal existed until a crisis retrospective surfaces it.
 *
 * DIRECTIONALITY LOGIC:
 *   central_authority and confirming_authority_apparatus are the structural beneficiaries: their institutional necessity is constituted by remaining the required gate, so they collect legitimacy and control from the arrangement without bearing the delay cost. capacity_holder_and_dependents are the structural targets: the capacity-holder bears the professional and psychological cost of self-distrust, and dependents bear the material cost when a correct private signal arrives too late to act on. training_institution sits in between — it administers the discipline (a form of agenda-setting) but also absorbs blame when the lag proves fatal, making it a partial payer as well as a partial agenda-setter.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabelings. First, against treating the entire arrangement as pure snare: the deferral rule did solve a real problem (uncalibrated perceptual error) and the coordination function is genuine for inexperienced capacity-holders, so this is not manufactured from nothing. Second, and more importantly, against treating it as pure rope in perpetuity: the founding problem's status is contested precisely because it has been substantially resolved for experienced capacity-holders (their track record establishes reliability) while the institutional deferral rule has not adapted to that resolution — the mandate has outlived its calibrated function and now persists as institutional control dressed as discipline. Tangled_rope captures both: real coordination at the point of origin, active extraction now that the coordination need has partially dissolved but the enforcement (professional norms, credentialing consequences for 'jumping the gate') has not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    calibration_versus_control,
    'Does the confirmation lag actually track the capacity-holder''s demonstrated reliability, shrinking as their private judgment is repeatedly vindicated, or does it remain fixed regardless of track record?',
    'Longitudinal audit comparing individual capacity-holders'' private-sensing accuracy rates against the institutionally mandated lag length over their careers; a flat lag despite rising accuracy is direct evidence of control rather than calibration.',
    'If the lag is genuinely calibrated to individual reliability, the arrangement is closer to a legitimate rope with a decaying error-correction function. If the lag is fixed regardless of demonstrated accuracy, the coordination story is cover and the arrangement is functionally a snare wearing rope''s clothing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_versus_control, empirical, 'Whether the deferral rule adapts to demonstrated reliability or remains a fixed control mechanism.').

omega_variable(
    internalized_versus_structural_suppression,
    'Is the measured suppression primarily structural (formal sign-off requirements, credentialing penalties for acting without confirmation) or internalized (the capacity-holder has fused professional identity with self-distrust and would hesitate even absent any formal penalty)?',
    'Post-exit or post-retirement trajectory: observe whether capacity-holders who leave the formal hierarchy (retire, change professions, move to unregulated contexts) continue to withhold and defer private judgment absent any structural requirement to do so.',
    'If suppression persists after the formal structure is removed, the effective suppression is higher than the structural measure suggests, and the harm is transmitted as an internal trait across generations independent of institutional enforcement — making simple policy reform (removing the formal rule) insufficient to resolve the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_versus_structural_suppression, empirical, 'Structural versus internalized suppression mechanism in the transmitted deferral discipline.').

omega_variable(
    crisis_threshold_ambiguity,
    'How severe must a crisis be, and how repeated, before the institution revises the deferral rule rather than treating a fatal lag as an isolated failure of individual judgment (blaming the capacity-holder for hesitating, rather than the rule for requiring hesitation)?',
    'Comparative case review of post-crisis institutional responses: track whether reforms target the deferral rule itself or target individual compliance with the existing rule.',
    'If institutions consistently respond to fatal lags by reinforcing compliance with the existing deferral rule rather than revising the rule, this is strong evidence the arrangement functions primarily to preserve the confirming authority''s necessity rather than to minimize harm — sharpening the classification toward tangled_rope''s extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crisis_threshold_ambiguity, conceptual, 'Whether institutional response to crisis targets the rule or targets compliance with the rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(disciplined_self_distrust_as_inherited_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disc_tr_t0, disciplined_self_distrust_as_inherited_suppression, theater_ratio, 0, 0.22).
narrative_ontology:measurement(disc_tr_t8, disciplined_self_distrust_as_inherited_suppression, theater_ratio, 8, 0.31).
narrative_ontology:measurement(disc_tr_t16, disciplined_self_distrust_as_inherited_suppression, theater_ratio, 16, 0.4).
narrative_ontology:measurement(disc_tr_t24, disciplined_self_distrust_as_inherited_suppression, theater_ratio, 24, 0.48).
narrative_ontology:measurement(disc_tr_t32, disciplined_self_distrust_as_inherited_suppression, theater_ratio, 32, 0.54).
narrative_ontology:measurement(disc_tr_t40, disciplined_self_distrust_as_inherited_suppression, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(disc_be_t0, disciplined_self_distrust_as_inherited_suppression, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(disc_be_t8, disciplined_self_distrust_as_inherited_suppression, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(disc_be_t16, disciplined_self_distrust_as_inherited_suppression, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(disc_be_t24, disciplined_self_distrust_as_inherited_suppression, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(disc_be_t32, disciplined_self_distrust_as_inherited_suppression, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(disc_be_t40, disciplined_self_distrust_as_inherited_suppression, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(disc_su_t0, disciplined_self_distrust_as_inherited_suppression, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(disc_su_t8, disciplined_self_distrust_as_inherited_suppression, suppression_requirement, 8, 0.73).
narrative_ontology:measurement(disc_su_t16, disciplined_self_distrust_as_inherited_suppression, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(disc_su_t24, disciplined_self_distrust_as_inherited_suppression, suppression_requirement, 24, 0.81).
narrative_ontology:measurement(disc_su_t32, disciplined_self_distrust_as_inherited_suppression, suppression_requirement, 32, 0.84).
narrative_ontology:measurement(disc_su_t40, disciplined_self_distrust_as_inherited_suppression, suppression_requirement, 40, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(disciplined_self_distrust_as_inherited_suppression, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(disciplined_self_distrust_as_inherited_suppression, 0.12).

% DUAL FORMULATION NOTE:
% This story is one reading of a broader family concerning inherited epistemic deference across professional and institutional lineages (medical hierarchy, intelligence chain-of-command, early-warning systems, minority risk-sensing traditions). A sibling story for any specific domain instantiation (e.g. nursing early-warning suppression in hospital hierarchies, or indigenous ecological-risk knowledge suppressed pending settler-institution confirmation) would carry its own epsilon and its own beneficiary/victim structure and should be linked here via affects_constraints if authored.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
