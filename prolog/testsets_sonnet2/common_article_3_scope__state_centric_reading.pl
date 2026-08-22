% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Applicability Threshold — State-Centric Reading
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions establishes minimum
 *   humanitarian protections in 'armed conflict not of an international
 *   character.' The text does not define the threshold precisely, leaving
 *   states, tribunals, and humanitarian bodies to fill the gap. Under the
 *   state-centric reading, the government experiencing internal violence is
 *   the primary — often sole practical — arbiter of whether the intensity and
 *   organization of that violence meets the threshold, and the government's
 *   classification is rarely subject to binding external review in real time.
 *   This reading maximizes state operational discretion and correspondingly
 *   minimizes protection for those engaged in or caught up in violence the
 *   state prefers to characterize as crime rather than conflict.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.79).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Applicability Threshold — State-Centric Reading").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '0528347e-d828-4aa0-833c-cf06b0b0b0fd').
narrative_ontology:cs_kernel_codification('0528347e-d828-4aa0-833c-cf06b0b0b0fd', fixed_text).
narrative_ontology:cs_authority_grounding('0528347e-d828-4aa0-833c-cf06b0b0b0fd', extraction).
narrative_ontology:cs_interpretation_layer_present('0528347e-d828-4aa0-833c-cf06b0b0b0fd').
narrative_ontology:cs_reading_relation('0528347e-d828-4aa0-833c-cf06b0b0b0fd', common_article_3_scope__expansive_human_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('0528347e-d828-4aa0-833c-cf06b0b0b0fd', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('0528347e-d828-4aa0-833c-cf06b0b0b0fd', foundational, sovereign_classification_primacy).
narrative_ontology:cs_axiom_status(sovereign_classification_primacy, holdable).
narrative_ontology:cs_axiom_grounding('0528347e-d828-4aa0-833c-cf06b0b0b0fd', sovereign_classification_primacy, conventional).
narrative_ontology:cs_axiom('0528347e-d828-4aa0-833c-cf06b0b0b0fd', secondary, threshold_gating_prevents_ihl_overreach).
narrative_ontology:cs_axiom_status(threshold_gating_prevents_ihl_overreach, holdable).
narrative_ontology:cs_axiom_grounding('0528347e-d828-4aa0-833c-cf06b0b0b0fd', threshold_gating_prevents_ihl_overreach, instrumental).
narrative_ontology:cs_reference_frame('0528347e-d828-4aa0-833c-cf06b0b0b0fd', state_sovereignty_preserving_threshold).
narrative_ontology:cs_drift_state('0528347e-d828-4aa0-833c-cf06b0b0b0fd', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0528347e-d828-4aa0-833c-cf06b0b0b0fd', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, sitting_governments).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, internal_security_forces).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_unclassified_violence).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, detainees_in_contested_conflicts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determine, through their own military and legal apparatus, whether a given internal conflict has crossed the intensity and organization thresholds that trigger CA3. Where they classify violence as riot, banditry, or ordinary law enforcement rather than armed conflict, CA3 obligations do not attach, and domestic criminal law and security force rules of engagement govern instead — rules the government itself writes and enforces.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, sitting_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Conduct counterinsurgency and internal security operations. Below the threshold, they operate under domestic rules of engagement rather than CA3's minimum humanitarian floor (no summary execution, no torture, no degrading treatment), retaining broader operational discretion over detention, interrogation, and use of force against persons the state does not recognize as party to an 'armed conflict.'
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_militaries, beneficiary,
    institutional, generational, arbitrage, national).

% Police and paramilitary units handling unrest, insurgency, or organized crime that the state prefers to characterize as law enforcement rather than armed conflict. Classification below the CA3 threshold keeps their conduct within domestic legal review rather than international humanitarian law and its attendant scrutiny.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, internal_security_forces, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, internal_security_forces, agenda_setter).

% Members of loosely organized armed groups, gangs engaged in politically inflected violence, or nascent insurgencies that have not yet reached the organizational cohesion or sustained intensity the threshold demands. When captured or targeted, they fall outside CA3's protections against summary killing, torture, and inhumane treatment, and are treated instead as ordinary criminals or unlawful combatants under domestic law, with no minimum humanitarian floor guaranteed by international law.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, immediate, trapped, local).

% Live in areas experiencing sustained but not-yet-threshold violence — communal clashes, gang warfare, low-grade insurgency. Because the state has not classified the situation as an armed conflict, they cannot invoke CA3 protections or the accountability mechanisms that attach to it; their protection depends entirely on domestic law enforcement standards, which the same government both sets and applies.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilians_in_unclassified_violence, payer,
    powerless, immediate, trapped, local).

% Held by state forces in conflicts whose classification is disputed. Below the threshold, they lack access to CA3-mandated humane treatment guarantees and cannot appeal to international monitors on that basis; the state's classification decision is effectively unreviewable by any body the state does not control.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, detainees_in_contested_conflicts, payer,
    powerless, biographical, trapped, national).

% Argue for a lower, protection-maximizing threshold and seek access to detainees and conflict zones regardless of classification. Under the state-centric reading, their access and mandate to invoke CA3 depend on the government's own threshold determination, which they cannot compel or override — they can advocate and document but not adjudicate applicability.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, icrc_and_human_rights_monitors, excluded,
    organized, generational, constrained, global).

% May later assess, after the fact and often years removed, whether a situation met the CA3 threshold for purposes of war crimes prosecution. Their retrospective judgment does not bind the government's real-time operational classification and rarely reaches individual victims in time to alter their treatment.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__state_centric_reading, state_militaries).
narrative_ontology:fixing_cost_class(common_article_3_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides states a workable, administrable line distinguishing genuine armed conflict (triggering the international law of armed conflict) from ordinary internal disorder (governed by domestic criminal and human rights law), avoiding indiscriminate application of a wartime legal regime to routine policing.
% TRANSFER_FUNCTION: Moves legal exposure and accountability away from states and their security forces and away from below-threshold combatants and civilians, who lose access to CA3's guaranteed humanitarian floor and the international scrutiny that attaches once a conflict is classified as covered.
% ABSENT_VOICES: Irregular combatants, affected civilians, and detainees have no seat in the classification decision — the same government whose forces would be constrained by a CA3 finding is the body deciding whether CA3 applies. The ICRC and human rights monitors can protest classifications but cannot compel a different one.
% DISAPPEARANCE_RATIONALE: States would argue that removing the threshold collapses the distinction between armed conflict and ordinary policing, over-extending IHL into domestic law enforcement and human rights treaty territory. Victims' advocates and the ICRC would argue that removing the threshold (or lowering it, as in the sibling readings) would mean protection depends on the nature of violence experienced rather than a government's self-interested classification — for the powerless stakeholders here, the world would rearrange substantially; for the state seats, comparatively little would change since domestic law would still govern much of their conduct.
% FOUNDING_PROBLEM: Common Article 3 was drafted to extend a minimum humanitarian floor to internal armed conflicts — civil wars, insurgencies — without requiring states to recognize insurgents as belligerents or to apply the full law of international armed conflict, which sovereign states feared would legitimize rebellion.
% FOUNDING_PROBLEM_CORROBORATION: States and their legal advisors attest the threshold is still necessary to prevent IHL from swallowing ordinary domestic policing and criminal law. The ICRC's own commentaries, UN human rights treaty bodies, and independent IHL scholars (writing from outside the state apparatus that benefits from a high threshold) attest that the threshold as operationalized by states has drifted from a workable administrability line into a self-serving discretion that leaves protracted low-intensity violence and detainee mistreatment unaddressed — corroboration exists but comes from outside the benefiting governments, not from within them.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, contested).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) reflects the substantial legal and physical protection state actors withhold from below-threshold combatants and civilians by controlling the classification gate; this has risen over the measured interval as states have become more sophisticated at characterizing insurgencies, gang violence, and communal conflict as ordinary crime specifically to avoid CA3 exposure and the international scrutiny it invites. Suppression (0.79) is high and rising because maintaining this reading requires active resistance to external classification challenges — states litigate, obstruct ICRC access, and reject tribunal findings that would apply CA3 retroactively. Theater ratio (0.42) captures a meaningful gap between declared threshold criteria (intensity, protracted armed violence, organized command structure — drawn from Tadić-era jurisprudence) and their actual invocation, which functions substantially as a discretionary shield rather than a good-faith administrability test.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat, the threshold is a necessary line preventing IHL from displacing ordinary domestic criminal law and human rights frameworks in cases of riot or organized crime — a genuine coordination function protecting sovereign legal order. From the payer seats, the same threshold, administered by the party with every incentive to keep it high, operates as a suppression mechanism that strips protection precisely where it is most needed — in contested, ambiguous, low-visibility violence where victims have the least capacity to document or appeal their situation. The engine's per-seat computation should register this divergence structurally rather than resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   States and their security forces are structural beneficiaries: they both set the classification rule and apply it to their own conduct, retaining maximum discretion when they judge it favorable to do so — a textbook self-grading arrangement. Irregular combatants, civilians in unclassified violence, and detainees are structural victims: they bear the consequence of a threshold determination made by the party whose conduct the determination would otherwise constrain, with no meaningful appeal channel operating in real time. The ICRC and human rights monitors occupy an excluded advocacy position — informed, organized, globally networked, but structurally unable to compel a different classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing genuine internal armed conflict from ordinary disorder so as not to over-extend wartime law — remains partly live (some threshold is administratively necessary) but the state-centric operationalization of that problem has drifted: the classification authority sits entirely with the party whose interest lies in classifying violence downward. This is not classic mandatrophy (the founding function hasn't vanished) but a captured-function pattern: the coordination purpose persists in name while the actual operation increasingly serves extraction (shielding state conduct from scrutiny) rather than the original administrability goal. Tangled Rope captures this better than snare because a real coordination problem (avoiding indiscriminate wartime-law application to routine policing) is genuinely served by SOME threshold — the extraction lies in who controls where that threshold sits and how it is enforced, not in the concept of a threshold itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_centric_vs_customary_threshold_locus,
    'Should CA3 applicability be determined by the affected state''s own real-time classification (this reading), by an unconditional low floor triggered by any organized armed violence (expansive_human_rights_reading), or by externally tracked customary international law and state practice (icrc_customary_reading)?',
    'Comparative analysis of how each reading''s classification authority performs in contested cases (e.g., counter-narcotics operations, communal violence, prolonged low-intensity insurgency) — specifically whether external review bodies (ICTY/ICTR jurisprudence, UN treaty bodies) have in practice displaced state self-classification, which would evidence a drift toward the customary reading.',
    'If external review increasingly displaces state self-classification in practice, the state-centric reading''s descriptive accuracy erodes even where states continue to assert it doctrinally — this would support reclassifying real-world CA3 practice toward the icrc_customary_reading rather than treating state-centric authority as the operative rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_centric_vs_customary_threshold_locus, conceptual, 'Where the true locus of CA3 threshold-setting authority sits among the three sibling readings.').

omega_variable(
    threshold_administrability_vs_shield_function,
    'Is the intensity/organization threshold, as actually operationalized by states, still serving its original administrability function (preventing IHL from swallowing ordinary policing) or has it become primarily a discretionary shield against accountability?',
    'Longitudinal case study of classification decisions across multiple states and conflicts, comparing declared threshold criteria against outcomes — specifically whether classification denials correlate with allegations of state abuse or with genuinely low-intensity, disorganized violence.',
    'If denials correlate strongly with abuse allegations rather than genuine intensity/organization shortfalls, this substantiates the tangled_rope classification (real coordination function contaminated by extraction) over a claim that the threshold is a neutral administrability tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_administrability_vs_shield_function, empirical, 'Whether the threshold functions as neutral line-drawing or as a captured accountability shield.').

omega_variable(
    kernel_text_ambiguity_exploitation,
    'Is the underlying textual ambiguity in Common Article 3 (no defined threshold in the treaty text itself) a genuine drafting gap requiring interpretive supplementation, or a structural feature that state-centric readings exploit precisely because no single external authority can bindingly resolve it in real time?',
    'Examination of the 1949 travaux préparatoires to determine whether drafters intended state discretion, external adjudication, or deliberate ambiguity; comparison with subsequent codification efforts (Additional Protocol II''s more explicit threshold) that narrowed the ambiguity for higher-intensity conflicts.',
    'If drafters intended external adjudicable standards and state discretion is a post-hoc appropriation, the state-centric reading''s claim to fidelity with the kernel text weakens considerably, supporting a Mandatrophy finding that this reading has drifted from the founding intent while retaining the founding text''s authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_text_ambiguity_exploitation, conceptual, 'Whether the treaty''s threshold ambiguity was drafted intentionally to preserve state discretion or has been appropriated for that purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__state_centric_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(comm_tr_t1975, common_article_3_scope__state_centric_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__state_centric_reading, theater_ratio, 1995, 0.33).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__state_centric_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(comm_tr_t2015, common_article_3_scope__state_centric_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__state_centric_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__state_centric_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(comm_be_t1975, common_article_3_scope__state_centric_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__state_centric_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__state_centric_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(comm_be_t2015, common_article_3_scope__state_centric_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__state_centric_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__state_centric_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(comm_su_t1975, common_article_3_scope__state_centric_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(comm_su_t1995, common_article_3_scope__state_centric_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(comm_su_t2005, common_article_3_scope__state_centric_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(comm_su_t2015, common_article_3_scope__state_centric_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__state_centric_reading, suppression_requirement, 2024, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints sharing the common_article_3_scope kernel (the 1949 Geneva Conventions text). The state_centric_reading authors the highest suppression and the narrowest victim set among the three, because classification authority sits with the party least incentivized to find CA3 applicable. The expansive_human_rights_reading would author a much lower ε (protection is unconditional, not gated) and a broader beneficiary set among affected persons. The icrc_customary_reading sits structurally between the two, with applicability tracked externally through accumulating state practice and opinio juris rather than fixed by either an unconditional floor or unilateral state discretion. Each story's ε, beneficiary/victim sets, and classification are authored independently per the ε-invariance principle; only the network edges and this note record the kinship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
