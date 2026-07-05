% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm Reduction Reading of Substance Control Legitimacy
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the harm-reduction reading of the
 *   substance_control_legitimacy kernel: substance use is framed as a public
 *   health condition, and state authority is justified by a duty to minimize
 *   harm rather than by moral condemnation or pure criminalization. In
 *   practice this produces a diversion-and-treatment apparatus layered on top
 *   of a criminal-legal system that is softened but not dismantled —
 *   possession is medicalized while supply remains criminalized, and
 *   'voluntary' treatment is frequently backed by a criminal-prosecution
 *   threat for noncompliance. The reading genuinely reduces some harms
 *   (overdose response, disease transmission) relative to straight
 *   prosecution, but it also creates a new extraction surface: a
 *   treatment-and-monitoring industry with guaranteed referral volume, and a
 *   persistent black market whose participants bear the costs the framework
 *   does not resolve. This is a distinct constraint from the
 *   legalization_reading (which would remove the criminal backstop and the
 *   coerced-treatment mechanism entirely) and from the prohibition_reading
 *   (which retains full criminalization without the medical diversion layer)
 *   — each reading has a different ε, a different beneficiary/victim
 *   structure, and is authored as its own story per the ε-invariance
 *   principle.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter (institutional/analytical) — administers diversion and treatment criteria
 *   - treatment_industry_providers: beneficiary (organized/mobile) — captive referral volume, state-guaranteed billing
 *   - mandated_treatment_participants: payer (powerless/trapped) — coerced compliance under criminal-prosecution threat
 *   - unlicensed_drug_users: payer (powerless/trapped) — residual criminalization outside the diversion track
 *   - black_market_participants: payer (powerless/trapped) — bears the cost of persistent supply-side criminalization
 *   - criminal_justice_system: agenda_setter/beneficiary (institutional/analytical) — retains the coercive backstop that makes diversion possible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.52).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.58).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction Reading of Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, 'e4a817d0-28c6-40e7-ba76-0276dd841806').
narrative_ontology:cs_kernel_codification('e4a817d0-28c6-40e7-ba76-0276dd841806', distributed).
narrative_ontology:cs_authority_grounding('e4a817d0-28c6-40e7-ba76-0276dd841806', expertise).
narrative_ontology:cs_interpretation_layer_present('e4a817d0-28c6-40e7-ba76-0276dd841806').
narrative_ontology:cs_reading_relation('e4a817d0-28c6-40e7-ba76-0276dd841806', substance_control_legitimacy__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('e4a817d0-28c6-40e7-ba76-0276dd841806', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('e4a817d0-28c6-40e7-ba76-0276dd841806', foundational, substance_use_is_medical_not_moral_failing).
narrative_ontology:cs_axiom_status(substance_use_is_medical_not_moral_failing, holdable).
narrative_ontology:cs_axiom_grounding('e4a817d0-28c6-40e7-ba76-0276dd841806', substance_use_is_medical_not_moral_failing, empirically_contingent).
narrative_ontology:cs_axiom('e4a817d0-28c6-40e7-ba76-0276dd841806', foundational, state_duty_is_harm_minimization_not_punishment).
narrative_ontology:cs_axiom_status(state_duty_is_harm_minimization_not_punishment, holdable).
narrative_ontology:cs_axiom_grounding('e4a817d0-28c6-40e7-ba76-0276dd841806', state_duty_is_harm_minimization_not_punishment, instrumental).
narrative_ontology:cs_reference_frame('e4a817d0-28c6-40e7-ba76-0276dd841806', public_health_medicalization_framework).
narrative_ontology:cs_drift_state('e4a817d0-28c6-40e7-ba76-0276dd841806', post_opioid_crisis_diversion_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e4a817d0-28c6-40e7-ba76-0276dd841806', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_industry_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, harm_reduction_nonprofits).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, mandated_treatment_participants).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, unlicensed_drug_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, black_market_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, insurers_and_payers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, criminal_justice_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the medicalized framework: sets diversion criteria, certifies treatment providers, defines what counts as compliance versus continued criminal referral. Derives legitimacy and budget from framing substance use as a treatable condition rather than a moral or criminal failing. Controls the diagnostic and administrative apparatus that decides who is 'in treatment' and who is 'noncompliant.'
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Receives court-mandated and diversion-program referrals as a captive client base; billing is often guaranteed by the state regardless of treatment efficacy. Benefits directly from the volume of people funneled through the system under threat of criminal-legal fallback if they fail to comply.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_industry_providers, beneficiary,
    organized, biographical, mobile, national).

% Operates needle exchanges, overdose-reversal distribution, and low-barrier outreach under the public-health framing's legitimacy, but is chronically underfunded relative to the treatment-mandate apparatus and often excluded from setting the terms of what counts as acceptable non-abstinence-based practice.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, harm_reduction_nonprofits, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, harm_reduction_nonprofits, excluded).

% Ordered into treatment programs by courts or diversion boards as an alternative to incarceration; noncompliance (missed appointments, positive tests, inability to pay program fees) triggers reversion to criminal prosecution. Bears program costs, urine-testing fees, and the loss-of-liberty threat that makes the 'voluntary' medical framing largely nominal for this seat.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, mandated_treatment_participants, payer,
    powerless, immediate, trapped, local).

% Uses substances outside any diversion or treatment contact and remains subject to residual criminal enforcement — possession statutes, paraphernalia laws, and civil commitment remain on the books even under the harm-reduction framing. Experiences the system as criminalization with a medical veneer rather than genuine decriminalization.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, unlicensed_drug_users, payer,
    powerless, immediate, trapped, local).

% Supplies the substances that the harm-reduction framework does not legalize or regulate; because supply remains criminalized, this seat absorbs violence, adulterated-product risk, and incarceration exposure that a regulated market would eliminate. The persistence of illegal supply is the structural cost the reading does not resolve.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_participants, payer,
    powerless, immediate, trapped, regional).

% Reimburses mandated and voluntary treatment episodes and helps set clinical necessity criteria; benefits from a steady, state-guaranteed treatment volume without bearing responsibility for outcomes or for the criminal-legal backstop that drives referrals.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, insurers_and_payers, beneficiary,
    institutional, biographical, arbitrage, national).

% Retains the underlying criminal statutes and courts that make diversion possible; supplies the coercive threat that gives treatment mandates their teeth. Continues to process black-market supply offenses at full criminal severity even as the health framing softens treatment for possession-level use.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, criminal_justice_system, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, criminal_justice_system, beneficiary).

% Studies overdose mortality, treatment retention, and diversion outcomes; can document whether the medicalized framework reduces harm relative to straight criminalization or relative to full legalization, informing which reading better fits the evidence.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, treatment_industry_providers).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces overdose deaths, disease transmission, and incarceration-driven social harm by routing substance users toward treatment and health services instead of straight criminal prosecution, and by funding low-barrier harm-reduction services (naloxone, needle exchange, supervised use sites where legal).
% TRANSFER_FUNCTION: Moves users from prison-facing exposure into a treatment-and-monitoring apparatus; moves public funds and mandated client volume to treatment providers and insurers; moves the residual criminal-legal risk onto unlicensed users and onto the black-market supply chain, which the framework leaves fully criminalized.
% ABSENT_VOICES: Illicit drug suppliers and unregulated market participants have no seat in shaping the framework, since any voice acknowledging their function would concede the persistence of a market the reading claims to be minimizing; drug users who reject the disease framing entirely (autonomy-based objectors) are also structurally unheard within a system that only offers medicalization or prosecution.
% DISAPPEARANCE_RATIONALE: Public health agencies and treatment providers would say the world rearranges catastrophically — diversion programs collapse, overdose deaths rise, court systems revert to straight incarceration. Autonomy-oriented critics and some harm-reduction advocates would say the underlying black market and criminalized supply chain — the actual driver of most substance-related harm — persists regardless, so the disappearance of THIS specific legitimacy claim mainly removes a legitimating veneer over continued criminal-legal control.
% FOUNDING_PROBLEM: Mass incarceration for low-level drug offenses was overwhelming courts and prisons while doing little to reduce use, overdose deaths were rising sharply (opioid crisis), and purely punitive enforcement was demonstrably failing on its own stated public-safety terms.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers and some judicial reform commissions, sitting outside the treatment-industry beneficiary group, attest that overdose and incarceration data motivated the shift and that diversion programs show measurable harm reduction relative to pure prosecution. Independent legal scholars and drug-policy reform organizations counter that the founding problem (overcriminalization) persists largely unaddressed because supply-side criminalization and the coercive treatment-mandate backstop were preserved rather than dismantled — corroboration is genuinely split rather than unanimous.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, contested).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52 by interval end) is moderate rather than high or low: real harm-reduction coordination exists (overdose reversal, disease prevention, diversion from incarceration) alongside a genuine transfer to the treatment-industry beneficiary group, whose referral volume is guaranteed by the criminal-legal threat rather than by voluntary demand. Suppression (0.58) reflects that noncompliance with treatment mandates reverts to criminal prosecution — the 'medical' framing does not remove coercion, it relocates it. Theater ratio (0.38) captures a rising share of activity that is compliance-monitoring and diagnostic gatekeeping rather than actual harm reduction, which increases over the interval as diversion programs formalize and add administrative layers. Accessibility collapse is moderate (0.45): some genuine alternatives persist (self-directed recovery, harm-reduction-only engagement) but courts increasingly funnel toward the certified-treatment track specifically.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_agencies and treatment_industry_providers seats, this looks like rope or scaffold — a transitional, humane alternative to mass incarceration. From the mandated_treatment_participants and unlicensed_drug_users seats, the same structure computes closer to tangled_rope or snare: coordination language covering continued coercion, now administered through clinical rather than penal machinery. The engine should surface this divergence from the structural data (trapped exit, powerless power atom, criminal-prosecution threat) rather than from either seat's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and the criminal justice system jointly set the terms (agenda_setter) and the criminal justice system retains the coercive backstop that makes the whole arrangement enforceable — hence its secondary beneficiary role (its funding and institutional relevance are partly sustained by continued referral volume). Treatment providers and insurers are beneficiaries with low derived d: they receive guaranteed volume without bearing enforcement or outcome risk. Mandated participants, unlicensed users, and black-market participants are victims with high derived d: trapped exit options, powerless power atom, and direct exposure to the criminal-legal threat that underwrites the whole framework place them near the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (overincarceration and rising overdose deaths under pure prohibition) is genuinely partially live — overdose mortality remains a real public health emergency the diversion apparatus responds to. But the mandate has also partially outlived the founding problem in a specific way: the treatment-industry beneficiary structure now has an interest in maintaining coerced referral volume independent of whether coercion is the most effective harm-reduction mechanism available. This is exactly the tangled_rope signature — a genuine coordination function (harm reduction) persists alongside an asymmetric extraction structure (guaranteed client volume backed by a criminal threat) that active enforcement (the criminal-legal backstop) is required to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coerced_treatment_vs_genuine_care,
    'Is the treatment mandate a genuine health intervention that happens to require legal leverage to reach a hard-to-engage population, or is the health framing primarily a legitimating cover for continued coercive control repackaged as care?',
    'Compare treatment retention, relapse, and overdose outcomes for court-mandated participants against a matched cohort in voluntary, non-coerced treatment programs; also examine whether program design (frequency of testing, sanctions for relapse-as-noncompliance) tracks clinical best practice or tracks punitive administrative convenience.',
    'If mandated outcomes are no better than voluntary outcomes and program design tracks punitive logic, the reading functions closer to a relabeled prohibition regime (supporting reclassification toward tangled_rope or snare); if mandated engagement produces meaningfully better outcomes than the counterfactual of no engagement at all, the coordination function is more substantively real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coerced_treatment_vs_genuine_care, empirical, 'Whether treatment mandates are substantively therapeutic or primarily coercive continuity.').

omega_variable(
    supply_side_criminalization_persistence,
    'Does retaining full criminalization of the supply chain, while medicalizing personal possession, represent an internally coherent harm-reduction position, or does it undermine the reading''s own stated purpose by preserving the adulterated, violent illegal market that drives much of the measured harm?',
    'Track overdose deaths attributable to supply contamination/adulteration versus deaths attributable to use itself; compare harm trajectories in jurisdictions that pair possession diversion with regulated supply pilots against jurisdictions that pair diversion with continued full supply-side criminalization.',
    'If contamination-driven harm dominates and regulated-supply jurisdictions show materially better outcomes, the harm_reduction_reading''s retention of supply-side criminalization is a significant internal contradiction, strengthening the case that black_market_participants are structural victims of an incompletely realized harm-reduction logic rather than of an unrelated policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_criminalization_persistence, empirical, 'Whether partial decriminalization without supply regulation is internally coherent.').

omega_variable(
    kernel_reading_selection,
    'Given that all three kernel readings (harm_reduction, prohibition, legalization) claim to minimize some form of harm, is the choice among them primarily a factual/empirical dispute about which regime produces better measured outcomes, or an irreducible values dispute about the proper scope of state authority over personal conduct?',
    'This is unlikely to fully resolve empirically — even with complete outcome data, the readings differ on whether state authority over competent adults'' substance use is legitimate at all (a values question), not only on which regime produces lower harm counts.',
    'If primarily empirical, better data collection could eventually converge the readings; if primarily a values dispute over the legitimate scope of state authority, the three readings will persist as coexisting positions regardless of outcome data, which is why they are authored as separate constraints rather than resolved into one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Whether the kernel contest is empirically resolvable or an irreducible values dispute over state authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'substance use policy legitimacy' per the ε-invariance principle. harm_reduction_reading (this story) carries moderate extractiveness (0.52) via treatment-industry capture riding on genuine public-health coordination. prohibition_reading is expected to carry higher extractiveness and suppression with no medicalization layer. legalization_reading is expected to carry the lowest extractiveness and suppression, removing both the criminal backstop and the coerced-treatment mechanism, but faces its own distinct victim structure (e.g., externalized third-party harms). All three share the same underlying kernel (substance_control_legitimacy) but instantiate structurally distinct constraints with different beneficiary/victim sets and different ε values — they must not be averaged or merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
