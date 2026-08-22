% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Harm-Threshold Speech Protection Regime (harm_threshold_reading)
 *   domain: constitutional law/political philosophy/communication rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the speech_protection_kernel: the
 *   harm-threshold reading, under which speech protection is conditional on
 *   the absence of demonstrable harm to victims. The standing arrangement
 *   under contest is the operative settlement of liberal-democratic speech
 *   law: the harm principle operationalized through doctrines of defamation,
 *   incitement, true threats, and harassment, administered by courts and
 *   bounded by statute. The referent of epsilon is that standing arrangement
 *   as this reading assesses it: a regime the reading endorses in principle
 *   while observing real extraction in its operation. Sibling readings
 *   (absolutist, marketplace, dignity, democratic_participation) are separate
 *   constraint stories with their own epsilon values and beneficiary/victim
 *   structures, and are not averaged into this one. The colloquial label of
 *   speech protection decomposes across them because each locates the
 *   protection boundary, and therefore who benefits and who pays,
 *   differently. KEY AGENTS (by structural relationship):
 *   constitutional_courts: agenda-setter (institutional/analytical exit),
 *   defines demonstrable harm and administers the threshold; legislatures:
 *   co-agenda-setter (institutional/analytical exit), enacts the statutory
 *   carve-outs the doctrine interprets; resourced_media_institutions: primary
 *   beneficiary (powerful/mobile), the demonstrability gate shields them and
 *   their budgets defeat surviving claims; victims_with_documented_harm:
 *   secondary beneficiary (moderate/constrained), collect remedy when they
 *   can carry the demonstration burden; targets_of_undemonstrable_harm:
 *   primary payer (powerless/trapped), bear real harm the standard cannot
 *   see; unresourced_speakers: payer (moderate/constrained), bear the
 *   deterrence shadow and litigation asymmetry; civil_liberties_advocates:
 *   observer (organized/mobile), contests the boundary from both directions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.55).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.55).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Harm-Threshold Speech Protection Regime (harm_threshold_reading)").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional law/political philosophy/communication rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '36b9ccb5-5e27-45a5-b28b-0c2b8776b58c').
narrative_ontology:cs_kernel_codification('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', fixed_text).
narrative_ontology:cs_authority_grounding('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', lineage).
narrative_ontology:cs_interpretation_layer_present('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c').
narrative_ontology:cs_reading_relation('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', foundational, demonstrable_victim_harm_defeats_protection).
narrative_ontology:cs_axiom_status(demonstrable_victim_harm_defeats_protection, holdable).
narrative_ontology:cs_axiom_grounding('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', demonstrable_victim_harm_defeats_protection, empirically_contingent).
narrative_ontology:cs_axiom('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', secondary, evidentiary_burden_rests_on_harm_claimant).
narrative_ontology:cs_axiom_status(evidentiary_burden_rests_on_harm_claimant, holdable).
narrative_ontology:cs_axiom_grounding('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', evidentiary_burden_rests_on_harm_claimant, conventional).
narrative_ontology:cs_reference_frame('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', harm_principle_conditioned_protection).
narrative_ontology:cs_drift_state('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('36b9ccb5-5e27-45a5-b28b-0c2b8776b58c', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, resourced_media_institutions).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, victims_with_documented_harm).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, targets_of_undemonstrable_harm).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, unresourced_speakers).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, harm_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, proportionality_balancing_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define what counts as demonstrable harm, set the stringency of the threshold through doctrine (categories such as defamation, incitement, true threats, harassment, and the tiers of scrutiny), and adjudicate every contest the threshold generates. They could revise or replace the framework; their institutional role and craft authority are bound up with the case-by-case balancing method. Exit is analytical: they stand outside the speech market they referee.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Enact the statutory harm carve-outs and procedural statutes (harassment law, defamation standards, anti-SLAPP protections) that judicial doctrine interprets. They absorb electoral pressure from both speech advocates and victims advocates, and can widen or narrow the unprotected categories by statute, though constitutional doctrine bounds what they may do.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, legislatures, agenda_setter,
    institutional, biographical, analytical, national).

% Publish and broadcast at scale. The demonstrability requirement shields them from weak harm claims, and their litigation budgets let them defeat or outlast the claims that survive the gate. They pay occasionally when harm is clearly demonstrated, but their net position under the regime is strongly favorable, and they can relocate publication or forum when a jurisdiction turns hostile.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, resourced_media_institutions, beneficiary,
    powerful, generational, mobile, global).

% Hold harms the legal system can see: documented defamation, direct threats, targeted harassment with records. They invoke the threshold and collect remedies when they can carry the demonstration burden, but they pay litigation costs and bear the risk that harm they know to be real will be judged insufficiently demonstrable.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, victims_with_documented_harm, beneficiary,
    moderate, biographical, constrained, national).

% Suffer harm that is real but hard to attribute, quantify, or document to the legal standard: diffuse reputational damage, coordinated harassment campaigns, cumulative degradation of participation in public life. The demonstrability standard is precisely what denies them remedy. They cannot exit the speech environment they live in and have no seat in the doctrinal conversation that sets the bar.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, targets_of_undemonstrable_harm, payer,
    powerless, biographical, trapped, national).

% Speak without institutional backing: independent writers, small publishers, ordinary participants in public debate. The vagueness of demonstrable harm casts a deterrence shadow they cannot price, and a single harm claim, meritless or not, can exceed their resources, so they self-censor or settle regardless of the merits.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, unresourced_speakers, payer,
    moderate, biographical, constrained, national).

% Litigate and advocate at the boundary of the threshold: challenging overbroad harm carve-outs as censorship while opposing the demonstrability bar as too high for victims. They collect no rents from the regime and pay none of its direct costs; their seat is analytical advocacy, and their contestation comes from both directions at once.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, civil_liberties_advocates, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__harm_threshold_reading, resourced_media_institutions).
narrative_ontology:fixing_cost_class(speech_protection_kernel__harm_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the standing conflict between robust speech protection and remedy for injurious speech: it gives speakers a protected zone, gives harm claimants an adjudicable claim, and gives courts a decision procedure, so that dissent, inquiry, and journalism can proceed while documented victims are not left without recourse. It coordinates expectations about where the actionable boundary sits across millions of daily speech acts.
% TRANSFER_FUNCTION: Moves remedy payments and reputational vindication from speakers to claimants when harm is demonstrated; moves litigation costs from both sides into the legal system; and, through the demonstrability requirement, moves the cost of evidentiary uncertainty onto harm claimants and the cost of doctrinal vagueness onto unresourced speakers as deterrence.
% ABSENT_VOICES: Targets of real-but-indemonstrable harm have no effective seat: the demonstrability bar is set in doctrinal contests between speakers advocates and documented claimants, and the gray-zone harmed appear only as losing parties, if at all. Future speakers chilled by standards they had no hand in setting are likewise absent, because deterrence operates on people who never enter the courtroom.
% DISAPPEARANCE_RATIONALE: If the harm threshold vanished overnight, speech law would reorganize around a sibling reading: categorical protection would strip documented victims of remedy, while dignity- or participation-based regimes would redraw the unprotected set entirely. The settled body of doctrine, statutes, and expectations built on the threshold would lose its framework within roughly a decade.
% FOUNDING_PROBLEM: The regime was built to operationalize the harm principle: a society that protects speech as its default needed a principled line marking where protection yields to demonstrated injury, so that dissent and inquiry would not be suppressed by mere offense, while victims of genuine injury would not be told their harm is the price of liberty.
% FOUNDING_PROBLEM_CORROBORATION: Both opposing camps attest the underlying conflict is live: speech-advocacy organizations document continuing censorship pressure, and victims advocates document continuing unremedied harm; comparative constitutional practice shows every liberal democracy operating some harm-conditional settlement rather than either extreme. No seat attests that this threshold is the uniquely correct resolution. The corroboration covers the problem, not the solution, and comes from outside the beneficiary set on both flanks.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55 at interval end) because the settlement delivers genuine coordination, being the only speech/harm boundary that has held across liberal democracies, while its demonstrability requirement operates as a resource filter: claimants whose harm is real but hard to attribute are denied remedy, and speakers without budgets are deterred by vagueness they cannot price. Suppression (0.55) is authored as a raw structural property and is not scaled by the engine; it reflects active enforcement machinery plus the deterrence shadow it casts, since the regime suppresses speech once harm is demonstrated and chills speech before any adjudication. Only extractiveness is scaled by directionality and scope downstream. Theater (0.28) is low-moderate: balancing tests decide real cases, but a growing share of doctrinal activity recites tests without deciding anything new. Accessibility collapse (0.30) is low: the sibling readings remain live, argued alternatives, and platform self-regulation operates alongside the legal regime. Resistance (0.60) is high and bidirectional: absolutists attack the threshold as censorship while victims advocates attack the demonstrability bar as denial. The temporal series run on one shared grid (t=0 to 60, roughly mapping the mid-1960s to the mid-2020s). Suppression_requirement is tracked because this story specifically traces enforcement-capacity change: harm-based suppression machinery matured and hardened over the interval as harassment law, injunctive remedies, and the platform-era deterrence shadow built out; extraction accumulated in parallel as litigation asymmetry grew and the gray zone of indemonstrable harm widened.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the agenda-setter seat (courts) the threshold is adjudicative craft: a decision procedure that constitutes the court's role. From the resourced-media seat the demonstrability gate is a shield they can afford to raise. From the documented-victim seat it is a remedy worth its litigation cost. From the gray-zone-target seat the identical standard is the mechanism that denies remedy, because their harm exists but cannot be demonstrated, so the protection boundary excludes them by construction. From the unresourced-speaker seat the vagueness is a deterrence tax paid in self-censorship. Courts and legislatures share agenda-setting at the same nominal power level but experience the constraint differently: courts gain craft authority from case-by-case balancing, while legislatures absorb electoral pressure from both camps and can move the boundary only within doctrinal limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: resourced_media_institutions sit near the beneficiary end, since the gate's shield accrues to them and mobility lets them forum-shop; victims_with_documented_harm are beneficiaries but the demonstration burden keeps them short of the full-beneficiary end. Victim declarations: targets_of_undemonstrable_harm sit near the full-target end, trapped, powerless, with the threshold's evidentiary standard being precisely what extracts their remedy; unresourced_speakers sit high, constrained in exit and bearing deterrence plus asymmetric litigation costs. Courts and legislatures are declared as agenda-setters rather than beneficiaries: they administer the arrangement and could revise it, and any collection they do (jurisdiction, craft authority) is secondary to administration, so no directionality override is authored; the structural derivation from role and exit is adequate for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming this settlement a rope would erase its asymmetric extraction: the demonstrability gate is not costless coordination but a resource filter whose burden lands on the least resourced on both sides of the docket. Claiming it a snare would erase its genuine coordination: the remedy delivered to documented victims is real, the protected zone held open for dissent is real, and the settlement's durability across democracies is maintained less by suppressing exits than by out-competing the alternatives. Tangled rope holds both facts. On the genealogy: the founding problem, a principled line between protection and demonstrated injury, is live rather than dead, so no mandate-atrophy flag is authored and the status-by-verdict mismatch consumer should not fire. The watch item is the widening gap between the demonstrability standard and the forms harm now takes; if coordinated, algorithmic, and cumulative harm becomes systematically indemonstrable, the coordination claim thins toward extraction and the classification should be revisited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (harm_threshold_reading) of the speech_protection_kernel; how would the structural classification change under a sibling reading''s instantiation of the same kernel?',
    'Author the sibling-reading stories (absolutist, marketplace, dignity, democratic_participation) and compare epsilon and beneficiary/victim structure across the family; the disagreement is located in where the protection boundary sits and who bears the cost of boundary indeterminacy.',
    'An absolutist instantiation would shift extraction from speakers toward unremedied targets; a dignity instantiation would widen the unprotected set and shift extraction toward speakers of subordinating speech; the kernel''s overall coordination-versus-extraction balance cannot be read from this seat alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel membership and sibling-reading structural delta for the harm-threshold reading.').

omega_variable(
    demonstrability_stringency,
    'Where does the effective demonstrability bar actually sit, and who bears the cost of its vagueness: claimants denied remedy or speakers deterred by uncertainty?',
    'Track dismissal rates of harm claims, settlement patterns under litigation-cost asymmetry, and survey evidence of self-censorship among unresourced speakers; compare jurisdictions with anti-SLAPP statutes and lower evidentiary bars.',
    'A high effective bar means the regime extracts remedy from targets, drifting the balance snare-ward; a low bar means it extracts autonomy from speakers; the tangled-rope balance between coordination and extraction moves with the answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrability_stringency, empirical, 'Effective stringency and incidence of the demonstrability standard.').

omega_variable(
    chilling_vs_unremedied_harm,
    'Which failure mode dominates the regime''s operation: speech suppressed by the deterrence shadow, or harm left unremedied behind the demonstration bar?',
    'Natural experiments around anti-SLAPP adoption, comparative speech-volume studies near litigation events, and longitudinal measurement of unremedied-harm reporting.',
    'If chilling dominates, measured suppression understates the constraint''s coercive reach and extraction is understated; if unremedied harm dominates, the coordination function is under-delivered and the settlement''s legitimacy claim weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_vs_unremedied_harm, empirical, 'Relative weight of the regime''s two characteristic failure modes.').

omega_variable(
    balancing_necessity_vs_self_maintenance,
    'Is case-by-case harm balancing structurally necessary for speech adjudication, or does it persist because it preserves adjudicative discretion and the legal profession''s role?',
    'Compare outcomes and error rates under rule-like categorical standards versus balancing across comparable jurisdictions; analyze the doctrine''s historical resistance to rule-like reform proposals.',
    'If balancing is discretionary self-maintenance, theater_ratio is understated and the regime drifts toward inertial persistence; if it is necessary, the coordination function is genuine and the measured extraction is partly the price of the procedure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_necessity_vs_self_maintenance, conceptual, 'Whether the balancing method is functional coordination or institutional self-perpetuation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_harm_threshold_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spk_harm_threshold_tr_t10, speech_protection_kernel__harm_threshold_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(spk_harm_threshold_tr_t20, speech_protection_kernel__harm_threshold_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(spk_harm_threshold_tr_t30, speech_protection_kernel__harm_threshold_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(spk_harm_threshold_tr_t40, speech_protection_kernel__harm_threshold_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(spk_harm_threshold_tr_t50, speech_protection_kernel__harm_threshold_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(spk_harm_threshold_tr_t60, speech_protection_kernel__harm_threshold_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(spk_harm_threshold_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spk_harm_threshold_be_t10, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(spk_harm_threshold_be_t20, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(spk_harm_threshold_be_t30, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(spk_harm_threshold_be_t40, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(spk_harm_threshold_be_t50, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 50, 0.53).
narrative_ontology:measurement(spk_harm_threshold_be_t60, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 60, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(spk_harm_threshold_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(spk_harm_threshold_su_t10, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(spk_harm_threshold_su_t20, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(spk_harm_threshold_su_t30, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(spk_harm_threshold_su_t40, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(spk_harm_threshold_su_t50, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(spk_harm_threshold_su_t60, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label of speech protection is a kernel, not a single constraint: it decomposes into readings with different epsilon values, victim sets, and unprotected categories. This file is the harm-threshold reading; the four sibling readings are separate stories linked here as one constraint family. The harm-threshold reading sits upstream of the dignity reading in litigated practice, because dignity claims must be packaged as demonstrable harm to survive a harm-threshold bench, and it directly contradicts the absolutist reading's core premise. No single file can carry multiple readings without violating epsilon invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
