% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Vaccine Mandate Proportionality Gate (Robust-Exemption Reading)
 *   domain: public health ethics / constitutional law / political philosophy
 *
 * SUMMARY:
 *   This story instantiates the proportionality_reading of the
 *   vaccine_mandate_balance kernel: vaccine mandates are legitimate only when
 *   disease severity, transmission risk, and vaccine safety jointly clear
 *   strict thresholds, and exemption processes must be robust
 *   (individualized, appealable, non-theatrical). The standing arrangement
 *   under contest is the vaccine-mandate governance complex as practiced —
 *   school-entry requirements, employment mandates, threshold-review boards,
 *   exemption statutes — assessed by this reading's own lights. The reading
 *   finds the arrangement partly faithful (mandates do fire where parameters
 *   justify them) and partly deviant (crisis-period mandates outran the
 *   thresholds; exemption review hardened in places into near-certain denial
 *   dressed as individualized assessment). CONSTRAINT FAMILY NOTE: the
 *   colloquial label 'vaccine mandate balance' decomposes into three
 *   structurally distinct governing commitments, written as separate stories
 *   per the epsilon-invariance principle. This reading authors epsilon 0.52
 *   for the shared referent; public_health_primary authors substantially
 *   lower epsilon (mandates are coordination, coercion justified by outcome);
 *   bodily_autonomy_primary authors near-maximal epsilon (any compelled
 *   intervention is violation). Same referent, reading-indexed values. The
 *   victim sets here are conditional on disease parameters — that
 *   conditionality is this reading's structural signature and distinguishes
 *   it from both siblings.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda-setter (institutional/constrained) — administers threshold review, imposes mandates when parameters clear the bar, receives compliance, bears review and litigation costs
 *   - courts_of_general_jurisdiction: agenda-setter (institutional/constrained) — adjudicates proportionality challenges and exemption-process claims from both directions; their rulings fix how strict 'strict' is in practice
 *   - state_legislatures: agenda-setter (institutional/mobile) — codify exemption categories and mandate enabling acts; the cheapest lever for changing the arrangement, exposed to pressure from both camps
 *   - mandated_healthcare_workers: primary target (organized/constrained) — bear the ultimatum of vaccination-or-termination under licensure constraints that limit regional exit
 *   - exemption_seeking_individuals: primary beneficiary (moderate/constrained) — collect robust exemption rights where review is genuine; hold paper rights where denial rates approach certainty
 *   - immunocompromised_high_risk_patients: dual-positioned target-beneficiary (powerless/trapped) — absorb residual exposure risk when exemptions run broad, gain protection when mandates fire legitimately
 *   - vaccine_hesitant_households: target-beneficiary (moderate/constrained) — face exclusion when thresholds are met and their category is narrowed, pass through when exemption channels are wide
 *   - school_age_children_under_mandate: excluded voice (powerless/trapped) — subject to entry requirements and quarantine rules they cannot consent to or vote on
 *   - constitutional_scholars_bioethicists: analytical observer (analytical/analytical) — analyze the doctrine from outside the enforcement loop
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.52).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.58).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Vaccine Mandate Proportionality Gate (Robust-Exemption Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public health ethics / constitutional law / political philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, 'd10fd26e-176f-4955-a2e8-129ba635be7d').
narrative_ontology:cs_kernel_codification('d10fd26e-176f-4955-a2e8-129ba635be7d', formalized).
narrative_ontology:cs_authority_grounding('d10fd26e-176f-4955-a2e8-129ba635be7d', expertise).
narrative_ontology:cs_interpretation_layer_present('d10fd26e-176f-4955-a2e8-129ba635be7d').
narrative_ontology:cs_reading_relation('d10fd26e-176f-4955-a2e8-129ba635be7d', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('d10fd26e-176f-4955-a2e8-129ba635be7d', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_axiom('d10fd26e-176f-4955-a2e8-129ba635be7d', foundational, mandate_legitimacy_requires_proportional_justification).
narrative_ontology:cs_axiom_status(mandate_legitimacy_requires_proportional_justification, holdable).
narrative_ontology:cs_axiom_grounding('d10fd26e-176f-4955-a2e8-129ba635be7d', mandate_legitimacy_requires_proportional_justification, empirically_contingent).
narrative_ontology:cs_axiom('d10fd26e-176f-4955-a2e8-129ba635be7d', secondary, exemptions_must_be_robust_against_categorical_denial).
narrative_ontology:cs_axiom_status(exemptions_must_be_robust_against_categorical_denial, holdable).
narrative_ontology:cs_axiom_grounding('d10fd26e-176f-4955-a2e8-129ba635be7d', exemptions_must_be_robust_against_categorical_denial, deontological).
narrative_ontology:cs_reference_frame('d10fd26e-176f-4955-a2e8-129ba635be7d', threshold_gated_conditional_legitimacy).
narrative_ontology:cs_drift_state('d10fd26e-176f-4955-a2e8-129ba635be7d', post_pandemic_mandate_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d10fd26e-176f-4955-a2e8-129ba635be7d', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, exemption_seeking_individuals).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, mandated_healthcare_workers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, immunocompromised_high_risk_patients).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, immunocompromised_high_risk_patients).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_households).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, least_restrictive_means_principle).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, police_power_proportionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the threshold assessments — severity, transmission, vaccine safety — and impose workplace or school-entry mandates when the numbers clear the bar. They receive compliance from mandated workers and institutions, publish the evidentiary basis, and defend exemption decisions in court. They cannot mandate at will: every imposition must survive the proportionality review they themselves administer, and every broad exemption grant cuts against the coverage targets they answer for. Exit is not available to them — the mandate question arrives whether or not they want it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Hear challenges from both directions: workers arguing a mandate lacks proportional justification, objectors arguing the exemption process is a sham, agencies arguing broad exemptions endanger wards. Their rulings fix how strict 'strict' is in practice. They cannot decline the docket while the standard governs, and each ruling reallocates legitimacy between the camps.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, courts_of_general_jurisdiction, agenda_setter,
    institutional, generational, constrained, national).

% Write the exemption statutes and mandate enabling acts, and recalibrate them after each election. They can widen or narrow exemption categories by ordinary bill, which makes them the cheapest lever for changing the arrangement — and exposes them to constituent pressure from mandate opponents and from high-risk constituents simultaneously.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, state_legislatures, agenda_setter,
    institutional, biographical, mobile, regional).

% Staff hospitals and clinics under policies tying employment to vaccination status once the authority's threshold review clears a mandate. Refusal means reassignment or termination; moving to another system in the same region rarely escapes the same policy, and licensure ties them to accredited employers. Union representation and grievance procedures soften but do not remove the ultimatum.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, mandated_healthcare_workers, payer,
    organized, biographical, constrained, national).

% File for medical, religious, or conscientious exemption and rely on the robustness of the process — documented criteria, individualized review, appeal paths, non-trivial approval. Where review is genuine they keep access to schools and jobs without the intervention; where denial rates approach certainty the right exists on paper only. Their practical position depends on review quality they do not control.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, exemption_seeking_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Cannot be vaccinated or respond poorly if vaccinated, so their safety depends on the coverage around them. Broad exemptions lower that coverage and they absorb the resulting exposure in daily life — grocery trips, clinics, transit. At the same time, whenever a mandate does fire legitimately and lifts community protection, they are among its principal beneficiaries. They cannot exit their exposure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, immunocompromised_high_risk_patients, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, immunocompromised_high_risk_patients, beneficiary).

% Weigh vaccine safety claims against school enrollment and employment. When the parameters justify a mandate and their exemption category is narrowed, they face the same exclusion as any refuser; when exemption channels are wide, they pass through without the intervention. Their position flips with the disease parameters and with the exemption statute — the arrangement treats them differently in different seasons.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_households, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, vaccine_hesitant_households, beneficiary).

% Are subject to school-entry requirements, classroom exclusion, and quarantine rules they did not consent to and cannot vote on. No seat represents their objection in the forums where thresholds are set or exemption categories are drawn; their interests arrive only filtered through parents, agencies, and courts.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, school_age_children_under_mandate, excluded,
    powerless, biographical, trapped, national).

% Analyze the doctrine from outside the enforcement loop — publishing on proportionality standards, the Jacobson-line precedent, exemption design, and comparative jurisdiction outcomes. No compliance flows to or from them; their contribution is the standing record of how the standard is being interpreted and where it is drifting.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, constitutional_scholars_bioethicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives pluralistic societies a shared evidentiary protocol for deciding when compulsory vaccination is legitimate: severity, transmissibility, and safety thresholds convert a culture-war dispute into a reviewable determination, letting mandates fire where they protect and restraining them where they do not, at the price of preserving the public trust that categorical coercion would burn.
% TRANSFER_FUNCTION: Moves decisional authority over compulsory medical intervention from individuals to public health authorities, but only inside threshold-bounded windows; moves residual infection risk onto those who cannot vaccinate when exemptions run broad; moves review, documentation, and litigation costs onto agencies, employers, and courts.
% ABSENT_VOICES: Children subject to school-entry mandates have no seat where thresholds or exemption categories are set; unvaccinable patients are spoken for by advocacy proxies rather than seated at exemption-policy tables; in jurisdictions that do not publish exemption denial rates, denied applicants cannot find one another to aggregate their objection.
% DISAPPEARANCE_RATIONALE: If the proportionality standard vanished overnight, mandate policy would reorganize around whichever categorical reading held local power: blanket mandates without meaningful exemption review where the collective-protection camp prevails, categorical prohibition of compulsion where the autonomy camp prevails. Threshold-review boards, exemption statutes, and the surrounding litigation docket would dissolve, and high-risk patients' protection would become wholly contingent on local political fortune.
% FOUNDING_PROBLEM: Reconcile collective protection against lethal epidemic risk with individual consent over bodily intervention — the problem every liberal state confronts when voluntary uptake fails and the unvaccinated include both refusers and the unvaccinable.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: constitutional courts applying structured proportionality review to mandate challenges, the Jacobson v. Massachusetts lineage treating mandate power as conditional on demonstrated necessity, and the bioethics literature's Millian harm-principle analyses all attest the founding tension remains live. Public-health professional bodies independently attest the collective-protection half; exemption claimants and civil-liberties litigators attest the consent half. The problem's liveness is multiply attested across opposed seats, not asserted by the arrangement's beneficiaries alone.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.52 is reading-indexed: the proportionality seat sees real unjustified coercion in the standing arrangement (crisis mandates imposed before or beyond what the parameters justified; exemption processes in several jurisdictions functioning as near-certain denial), while crediting the gate for preventing the categorical coercion both sibling readings would entrench. Suppression 0.58 is structural, not internalized: job termination and school exclusion are external barriers, softened by functioning exemption channels and cross-jurisdiction alternatives — hence accessibility_collapse 0.42 (alternatives persist at real cost: homeschooling, private employment, relocation). Resistance 0.62 reflects an unusually dense counter-pressure environment: litigation from objectors and workers, legislative preemption and restoration bills, union grievances, public protest. Theater_ratio 0.30: much of the review machinery is functional, but a growing share is performative — denial-rate opacity, post-hoc evidentiary rationalization — concentrated in the surge period. The measurement series runs on ONE shared grid (t=0..24, step 4) with all three metrics authored at every point. The shape is a crisis pulse, not a sustained oscillation: extraction, suppression, and theater all spike at t=20 (the surge wave) and partially retreat by t=24 as litigation and legislative correction bite; base_properties values are the end-state (t=24) readings. Because the pattern is a single pulse rather than a repeating cycle, seven points suffice; had the record shown repeated surge-relaxation loops, eight to ten points documenting intermittent reinforcement would be required.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats should compute differently, and the structural data supports that divergence. From the administrator's desk, the threshold review is diligence: every imposition must survive scrutiny the authority itself administers, which reads as legitimacy-preserving discipline. From the worker's or applicant's side, the identical review reads as a gauntlet — an ultimatum with paperwork attached, where the reviewer and the beneficiary of compliance are the same institution. Courts sit between: they experience the standard as a docket that never closes, binding them to referee both directions indefinitely. The engine computes per-seat classifications from the authored power, exit, and role data; this commentary explains why the divergence is real rather than reconciling it away.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: exemption_seeking_individuals (declared beneficiary, constrained exit) derive near the beneficiary pole; mandated_healthcare_workers (declared victim, organized power, constrained exit) derive near the target pole. Two overrides correct derivations the declarations alone get wrong. First, institutional -> 0.38: public_health_authorities are declared beneficiaries (they collect the trust dividend and retain a lawful mandate instrument), but pure-beneficiary derivation would undershoot their self-interest — they also bear review costs, lost blanket discretion, and litigation exposure, and they retain institutional appetite for mandate capacity. The override places them moderately target-ward of symmetric, which also lands approximately right for the other institutional seats (courts near-symmetric referees, legislatures mildly invested in the mediating position). Second, powerless -> 0.66: immunocompromised_high_risk_patients are declared victims (they absorb residual risk from broad exemptions), but pure-victim derivation overshoots — they gain materially whenever a mandate fires legitimately and lifts community coverage. The override pulls them back toward, but not across, the target side. Vaccine_hesitant_households carry a residual imprecision the power-atom-keyed override surface cannot fix (they share the moderate atom with pure beneficiaries): their true directionality flips with the parameters, which is the structural signature of this reading. Suppression enters the engine unscaled; only extractiveness is scaled by directionality and spatial scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling collective protection against lethal epidemic risk with individual consent over bodily intervention — is live: epidemics recur, and the tension regenerates with each pathogen. The R5 mismatch consumer reads founding_problem_status=live crossed with disappearance_verdict=world_rearranges: no dead-mandate zombie flag, correctly, because the gate exercises its function every review cycle. This is not a piton: theater_ratio 0.30 sits well below performative-maintenance territory, the administrator could not cheaply discard the standard without abandoning its legitimacy dividend, and no seat profits from the standard's mere persistence. The classification prevents mislabeling in both directions: a public_health_primary authorship would read this standard as pure obstruction (extraction of agency capacity, exemptions as free-riding licenses); a bodily_autonomy_primary authorship would read every mandate the gate admits as pure snare. From the proportionality seat, both the coordination function (shared evidentiary protocol converting a culture war into a reviewable determination) and the extraction (unjustified coercion where thresholds were outrun or exemptions hollowed) are visible at once — which is the tangled-rope structure claimed. Watch item: if threshold-setting authority is captured (see omega threshold_setting_authority) and review degenerates into rubber-stamping, theater_ratio sustains above 0.5 and the trajectory bends toward snare with a proportionality costume.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the proportionality_reading of the vaccine_mandate_balance kernel; what would the sibling readings (public_health_primary, bodily_autonomy_primary) change structurally if adopted in place of this one?',
    'Compare against the sibling stories'' authored victim sets and epsilon values: public_health_primary makes the victim set categorical (all refusers under lethal-exposure conditions) and authors low epsilon; bodily_autonomy_primary makes every mandated person a victim regardless of parameters and authors near-maximal epsilon.',
    'Switching readings relocates the victim set from conditional-on-parameters to categorical in one direction or the other, and moves epsilon to the extremes; this story''s conditional victim sets and mid-range epsilon are artifacts of the proportionality seat, not of the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption changes victim sets and epsilon categorically.').

omega_variable(
    epsilon_pathogen_regime_variance,
    'Does this reading''s epsilon hold across pathogen regimes, given that mandate legitimacy is conditional on severity, transmission, and safety parameters?',
    'Re-run the classification under contrasting parameter profiles: a smallpox-class pathogen (high severity, high transmission, established safety) versus a seasonal-influenza-class pathogen (low severity, modest transmission).',
    'Under a smallpox-class profile the thresholds are met, mandates fire legitimately, and effective extraction collapses toward coordination cost (rope-like operation); under a seasonal-flu profile the thresholds are unmet and any imposed mandate reads as unjustified coercion (snare-adjacent). The single authored epsilon describes the contemporary mixed-parameter landscape, not a pathogen-invariant constant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_pathogen_regime_variance, empirical, 'Epsilon varies by pathogen regime because this reading makes legitimacy parameter-conditional.').

omega_variable(
    threshold_setting_authority,
    'Who sets the strict proportionality thresholds, and with what epistemic warrant — advisory expert bodies, courts, or legislatures? The inter-reading disagreement is located precisely here.',
    'Comparative institutional analysis of threshold-setting bodies across jurisdictions: composition, evidence standards, publication practices, and reversal rates.',
    'If threshold-setting is captured by the mandating institutions themselves, the strictness of ''strict'' decays and the standard becomes a rubber stamp, bending the trajectory toward theatrical maintenance; independent or judicially reviewed threshold-setting preserves the gating function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_setting_authority, conceptual, 'Location of the kernel contest: the authority that operationalizes the thresholds determines whether the reading is live or nominal.').

omega_variable(
    exemption_robustness_scope,
    'What counts as a robust exemption — medical-only, plus religious, plus personal-belief? The reading requires robustness but does not fix the category set.',
    'Preference aggregation through legislation and case law, with comparative-jurisdiction outcome tracking (coverage levels, outbreak incidence, denial rates).',
    'A narrower category set raises measured injustice-to-objectors (more people coerced without individually warranted grounds); a broader set shifts residual infection risk onto the unvaccinable. The epsilon value sits inside this unresolved trade-off.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_scope, preference, 'The robustness requirement is underspecified at the boundary of exemption categories.').

omega_variable(
    surge_deviation_permanence,
    'Was the crisis-period departure from strict threshold gating (mandates imposed ahead of or beyond proportional justification, exemption processes hardened) a temporary suspension of the standard or a permanent ratchet?',
    'Post-surge tracking of statute amendments, exemption-category restorations, and doctrinal statements by courts reviewing the surge-era mandates.',
    'A permanent ratchet keeps suppression and theater elevated and bends the constraint toward enforced extraction wearing a proportionality costume; full reversion restores the tangled-rope steady state with the gate doing real work each review cycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surge_deviation_permanence, empirical, 'Whether the observed practice drift away from the reference frame consolidates or reverses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmb_proportionality_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(vmb_proportionality_tr_t0, observed).
narrative_ontology:measurement(vmb_proportionality_tr_t4, vaccine_mandate_balance__proportionality_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement_basis(vmb_proportionality_tr_t4, observed).
narrative_ontology:measurement(vmb_proportionality_tr_t8, vaccine_mandate_balance__proportionality_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(vmb_proportionality_tr_t8, observed).
narrative_ontology:measurement(vmb_proportionality_tr_t12, vaccine_mandate_balance__proportionality_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement_basis(vmb_proportionality_tr_t12, observed).
narrative_ontology:measurement(vmb_proportionality_tr_t16, vaccine_mandate_balance__proportionality_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement_basis(vmb_proportionality_tr_t16, observed).
narrative_ontology:measurement(vmb_proportionality_tr_t20, vaccine_mandate_balance__proportionality_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement_basis(vmb_proportionality_tr_t20, observed).
narrative_ontology:measurement(vmb_proportionality_tr_t24, vaccine_mandate_balance__proportionality_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(vmb_proportionality_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(vmb_proportionality_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(vmb_proportionality_be_t0, observed).
narrative_ontology:measurement(vmb_proportionality_be_t4, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement_basis(vmb_proportionality_be_t4, observed).
narrative_ontology:measurement(vmb_proportionality_be_t8, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement_basis(vmb_proportionality_be_t8, observed).
narrative_ontology:measurement(vmb_proportionality_be_t12, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(vmb_proportionality_be_t12, observed).
narrative_ontology:measurement(vmb_proportionality_be_t16, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement_basis(vmb_proportionality_be_t16, observed).
narrative_ontology:measurement(vmb_proportionality_be_t20, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(vmb_proportionality_be_t20, observed).
narrative_ontology:measurement(vmb_proportionality_be_t24, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement_basis(vmb_proportionality_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(vmb_proportionality_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(vmb_proportionality_su_t0, observed).
narrative_ontology:measurement(vmb_proportionality_su_t4, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement_basis(vmb_proportionality_su_t4, observed).
narrative_ontology:measurement(vmb_proportionality_su_t8, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement_basis(vmb_proportionality_su_t8, observed).
narrative_ontology:measurement(vmb_proportionality_su_t12, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement_basis(vmb_proportionality_su_t12, observed).
narrative_ontology:measurement(vmb_proportionality_su_t16, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement_basis(vmb_proportionality_su_t16, observed).
narrative_ontology:measurement(vmb_proportionality_su_t20, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(vmb_proportionality_su_t20, observed).
narrative_ontology:measurement(vmb_proportionality_su_t24, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(vmb_proportionality_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'vaccine mandate balance' conflates three structurally distinct governing commitments, decomposed into three stories sharing one referent with reading-indexed epsilon values. This reading (proportionality) creates downstream structural pressure on both siblings without resolving the dispute: courts importing proportionality language into public-health rulings changes the operating environment of public_health_primary, and documented threshold failures supply bodily_autonomy_primary with its strongest evidentiary cases. Neither sibling is foreclosed as a live public position by this story's existence; the logical foreclosure relation runs only between this reading and bodily_autonomy_primary within a single party's framework (see cs_structure.reading_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, institutional, 0.38).
constraint_indexing:directionality_override(vaccine_mandate_balance__proportionality_reading, powerless, 0.66).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
