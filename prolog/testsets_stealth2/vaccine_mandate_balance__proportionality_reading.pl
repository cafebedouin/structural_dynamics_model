% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Conditional-Legitimacy Gate for Vaccine Mandates (Proportionality Reading)
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The arrangement under examination is a proportionality gate operated by
 *   courts and public health authorities: a mandate - school-entry
 *   requirement, hospital employment condition, emergency order - is
 *   legitimate only when the disease-severity, transmission-risk, and
 *   vaccine-safety record clears strict thresholds, and a working exemption
 *   route must accompany any compulsion that issues. The epsilon referent is
 *   this standing arrangement of mandate governance under proportionality
 *   review, assessed by this reading's own lights: coercion deployed only
 *   where the showing is made, refusal accommodated wherever the showing
 *   fails. The gate's costs do not fall evenly. In ordinary seasons it
 *   operates as near-pure restraint on official power and near-pure
 *   protection of objectors; in threshold-meeting epidemics its
 *   robust-exemption guarantee enlarges the unprotected pool around people
 *   who cannot vaccinate at all, transferring risk onto them without their
 *   consent. The claim and the metrics are authored independently: the claim
 *   is tangled_rope because a genuine, load-bearing allocation function and
 *   an episodic asymmetric cost-imposition run through the same exemption
 *   structure; the metrics describe the arrangement's observed operation
 *   without being tuned to that claim.
 *
 * KEY AGENTS:
 *   - constitutional_courts: Agenda setter ([institutional]/[analytical]) - defines when the threshold test is satisfied and polices exemption adequacy
 *   - public_health_authorities: Administering enforcer ([institutional]/[constrained]) - proposes compulsion under the thresholds, banks or burns legitimacy, runs exemption processing
 *   - conscientious_objectors: Primary beneficiary ([organized]/[identity_locked]) - holds the robust exemption guarantee; sheds epidemic risk outward
 *   - immunocompromised_patients: Primary payer ([powerless]/[trapped]) - absorbs the transmission externality of broadened exemptions
 *   - vaccine_ineligible_infants: Pure payer ([powerless]/[trapped]) - too young for the schedule; exposed through coverage gaps
 *   - healthcare_worker_objectors: Conditional payer ([organized]/[constrained]) - mandatable once thresholds are met; fully protected when they are not
 *   - public_health_ethicists: Analytical observer ([moderate]/[analytical]) - formulates and critiques the criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.44).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.3).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Conditional-Legitimacy Gate for Vaccine Mandates (Proportionality Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, 'f9fed353-bccc-4d98-8627-180cb92a45f4').
narrative_ontology:cs_kernel_codification('f9fed353-bccc-4d98-8627-180cb92a45f4', formalized).
narrative_ontology:cs_authority_grounding('f9fed353-bccc-4d98-8627-180cb92a45f4', lineage).
narrative_ontology:cs_interpretation_layer_present('f9fed353-bccc-4d98-8627-180cb92a45f4').
narrative_ontology:cs_reading_relation('f9fed353-bccc-4d98-8627-180cb92a45f4', vaccine_mandate_balance__public_health_primary, influences).
narrative_ontology:cs_reading_relation('f9fed353-bccc-4d98-8627-180cb92a45f4', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_axiom('f9fed353-bccc-4d98-8627-180cb92a45f4', foundational, compulsion_requires_proportional_justification).
narrative_ontology:cs_axiom_status(compulsion_requires_proportional_justification, holdable).
narrative_ontology:cs_axiom_grounding('f9fed353-bccc-4d98-8627-180cb92a45f4', compulsion_requires_proportional_justification, deontological).
narrative_ontology:cs_axiom('f9fed353-bccc-4d98-8627-180cb92a45f4', foundational, exemptions_must_be_robust).
narrative_ontology:cs_axiom_status(exemptions_must_be_robust, holdable).
narrative_ontology:cs_axiom_grounding('f9fed353-bccc-4d98-8627-180cb92a45f4', exemptions_must_be_robust, deontological).
narrative_ontology:cs_reference_frame('f9fed353-bccc-4d98-8627-180cb92a45f4', jacobson_proportionality_framework).
narrative_ontology:cs_drift_state('f9fed353-bccc-4d98-8627-180cb92a45f4', post_pandemic_emergency_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f9fed353-bccc-4d98-8627-180cb92a45f4', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, conscientious_objectors).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, contraindication_patients).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, immunocompromised_patients).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, vaccine_ineligible_infants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, immunocompromised_patients).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, healthcare_worker_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, healthcare_worker_objectors).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, jacobson_reasonable_relation_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, strict_scrutiny_analogy_for_medical_compulsion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Review mandate statutes, emergency orders, and employer rules against the severity, transmission, and safety record; decide when the evidence clears the bar for compulsion and whether the exemption process on offer is real. Their written standards define what counts as a justified mandate. They collect no fees from the arrangement and cannot be compelled by it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Run immunization programs and, during outbreaks, propose compulsory measures. Before compelling they must assemble the severity and safety case and defend it in review; honoring the thresholds banks credibility they spend later, stretching them burns it. They also administer the exemption paperwork, whose volume grows with every expansion of exemption categories.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, public_health_authorities, beneficiary).

% Hold religious or conscientious objections to specific vaccines and claim exemption from mandatable programs. The arrangement guarantees them a working exemption route. During epidemics their unvaccinated status contributes to transmission around them. For the committed core the objection is bound up with religious identity and community membership, so abandoning it is not a live option in the way changing jobs is.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, conscientious_objectors, beneficiary,
    organized, biographical, identity_locked, national).

% Cannot safely take particular vaccines - prior anaphylaxis, severe adverse reaction history - and hold medical exemptions that every side of the mandate debate accepts. The guarantee of a robust exemption route is aimed squarely at protecting people in their position from being forced. They remain exposed to whatever circulates in the exempted pool around them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, contraindication_patients, beneficiary,
    powerless, biographical, trapped, national).

% Transplant recipients, people on chemotherapy, and others whose immune status makes vaccination ineffective or unsafe. They hold medical exemptions themselves but depend on the vaccinated fraction of the community around them; every broadened conscientious or religious exemption during an outbreak enlarges the unprotected pool they live inside. They cannot change their immune status and cannot opt out of their neighbors' choices.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, immunocompromised_patients, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, immunocompromised_patients, beneficiary).

% Too young for the relevant vaccine doses during outbreak windows. No exemption category applies to them because no consent question arises; they simply absorb whatever circulates. They appear in exemption litigation only as figures invoked by the adults arguing.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vaccine_ineligible_infants, payer,
    powerless, immediate, trapped, regional).

% Nurses, aides, and clinical staff with conscientious objections to specific vaccines. When a severe novel pathogen meets the thresholds, the arrangement permits their employers and licensing boards to condition employment on vaccination with only narrow carve-outs - they face inoculation, testing regimes where offered, or leaving clinical work. In ordinary seasons they retain full refusal rights.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, healthcare_worker_objectors, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, healthcare_worker_objectors, beneficiary).

% Organize on behalf of people who cannot be vaccinated and argue that exemption policy is set over their principals' heads: the state and the objectors negotiate the breadth of exemptions while the people who absorb the consequence hold no seat. They petition for standing in threshold-setting and for risk-weighting of exemption categories, with consultative access at best.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, disability_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Publish the proportionality criteria, sit on advisory panels, and testify in litigation over specific mandates. They shape how thresholds are formulated but neither administer mandates nor hold exemptions; their professional stake is in the quality of the framework's reasoning.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_ethicists, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, conscientious_objectors).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared decision procedure for when the state may convert a vaccination recommendation into a legal obligation: severity, transmissibility, and vaccine-safety evidence are weighed against the intrusion, and a working exemption route is guaranteed - resolving the autonomy-versus-collective-protection conflict case by case instead of by categorical rule or raw political force.
% TRANSFER_FUNCTION: Moves coercive latitude and risk. In threshold-meeting epidemics it moves compliance obligations onto objectors and residual infection risk onto those exempted around - the immunocompromised and the too-young; in sub-threshold periods it moves restraint onto public health authorities, who must persuade rather than compel.
% ABSENT_VOICES: The unable-to-vaccinate have no seat in exemption litigation: the contest is argued between the state and the objectors, with the people who absorb the outcome present only as rhetorical figures invoked by both sides. Disability-rights advocates hold consultative status at best and would demand a vote on exemption breadth and a risk-weighting of exemption categories.
% DISAPPEARANCE_RATIONALE: Every school-entry requirement, hospital employment condition, and emergency-order template currently cites threshold satisfaction and offers an exemption route; remove the gate overnight and each instrument loses its stated warrant. Jurisdictions would split immediately between categorical compulsion and categorical prohibition, and the exemption-adjudication machinery - forms, hearings, review boards - would dissolve with nothing replacing it.
% FOUNDING_PROBLEM: After compulsory-vaccination riots and patchwork local rules in the late nineteenth century, the founding problem was to state when a government may convert a vaccination recommendation into a legal obligation: what showing of disease severity, transmission risk, and vaccine safety must precede compulsion, and what accommodation dissenters are owed.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: appellate courts continually restate the severity-transmission-safety showing requirement when upholding or striking specific mandates; WHO and national ethics bodies publish proportionality criteria for immunization policy independently of any exemption claim; and the bioethics literature treats the calibration problem as open. Objector litigants, the arrangement's principal beneficiaries, dispute the thresholds' application in every outbreak while conceding the framework itself - corroboration of liveness therefore does not rest on the benefiting parties alone.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope on structural grounds: the gate solves a real allocation problem (it states in advance what showing converts persuasion into compulsion, which keeps justified emergency coercion available while blocking arbitrary compulsion), and simultaneously its exemption guarantee imposes concentrated costs on identifiable non-consenting third parties during epidemics - coordination and cost-imposition through one structure, held up by active judicial and administrative enforcement. Metrics are authored independently of the claim. Extractiveness 0.44: episodic and bounded (confined to threshold-meeting windows and to the exempted-around population) but real and lethal at the margin. Suppression 0.30, unscaled by construction: the gate forecloses few alternatives (voluntary incentive schemes and categorical rules remain legally available; rival resolution mechanisms are live), and it relies on judicial enforcement rather than exit-blocking - indeed it manufactures exits. Theater 0.26: threshold reviews consume real epidemiological and safety-surveillance data, but crisis-period balancing hearings and rubber-stamp exemption processing add a performative layer. Accessibility collapse 0.28: understanding the framework does not eliminate rival approaches to the autonomy/collective-protection conflict. Resistance 0.58: continuously litigated from both directions - authorities chafe at the showing requirement, objectors at its uneven application. The measurement series share one grid (points 0, 4, 8, 12, 16, 20, 25) and trace a crisis cycle: gradual accumulation through the early period, an emergency-era spike (mandates issued under expedited review, exemption processes narrowed in practice, employment conditioned on inoculation - enforcement ratcheting to 0.43), then post-emergency recalibration as courts reassert the thresholds and several emergency frameworks retire, settling above the pre-crisis baseline. The oscillation is partly the cost-imposition mechanism itself: each emergency temporarily suspends exemption robustness (intermittent reinforcement of official capacity), and the ratchet residue is why the endpoint values sit above the starting ones.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the court seat the arrangement is a legitimacy-preserving balance that lets coercion happen only with a showing. From the public health authority seat it is both a cage (coercive capacity held in reserve behind a evidentiary hurdle) and a credential (compliance with the thresholds is what makes later mandates credible). From the objector seat it is a hard-won guarantee perpetually at risk of administrative narrowing. From the immunocompromised and infant seats it is a risk imposition negotiated entirely by other parties. Same-level lateral divergence is sharp: conscientious objectors and immunocompromised patients are similarly situated private civilians, but the former hold an organized, identity-anchored claim the machinery must honor, while the latter hold a trapped, unorganized exposure the machinery merely talks about - identical nominal standing, opposite structural positions, and the difference is carried entirely by exit options and organization, not by formal power.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: conscientious_objectors (identity-locked, organized) sit near the full-beneficiary end - the guarantee exists for them; contraindication_patients likewise, since the robust medical-exemption route is aimed at exactly their situation. Declared victims derive high directionality: immunocompromised_patients (trapped, powerless) and vaccine_ineligible_infants (trapped, powerless) sit near the full-target end - they bear the externality and cannot exit it. Healthcare_worker_objectors are declared payers with organized power and constrained exit: mandatable once thresholds are met, so high but not maximal. Public health authorities are deliberately left out of both arrays: their position is genuinely dual (they pay administrative burden and capacity loss, and collect legitimacy when they honor the thresholds), and neither a beneficiary nor a victim declaration would describe them; the stakeholder layer carries the dual role and the engine's fallback treats them as the administered middle. No directionality overrides were needed - every declared seat's derived directionality matches its structural position. On receipt: the arrangement's operative gain - compulsion-free standing during epidemics - demonstrably accrues to the objector seat while the shed risk lands on the unable-to-vaccinate, which is why gain_flow names that seat rather than reporting diffuse. Note the bidirectional failure structure: the victim sets are conditional. Over-broad exemptions during a threshold-meeting epidemic convert the unable-to-vaccinate into payers (the chronic direction, reflected in the victims array); compulsion issued without the showing converts objectors into payers (the acute direction, visible as the crisis-phase rise in the extractiveness series).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the gate as pure coordination would erase the epidemic-time risk transfer onto the unable-to-vaccinate - the cost half that makes the exemption guarantee more than a free good. Reading it as pure extraction would erase the allocation function that keeps justified emergency coercion available and that both camps implicitly rely on when they litigate thresholds rather than abolish them. Tangled rope holds both halves. On genealogy: the founding problem - stating when compulsion is justified and what dissenters are owed - is live, reopened by every novel pathogen, and corroborated from outside the benefiting parties (courts restating the showing requirement, international and national ethics bodies publishing proportionality criteria, objector litigants conceding the framework while disputing its application). Founding_problem_status is therefore live and disappearance_verdict world_rearranges, so no mismatch flag arises and no mandatrophy resolution is declared. Fixing cost is prohibitive: the gate is constitutionally entrenched through a century of case law, and replacing it with a categorical rule would require overturning settled doctrine at a cost to whoever could fix it (courts, legislatures) far exceeding any benefit they would collect - which is also why the theater symptoms tracked in the measurement series are treated as drift indicators, not as evidence the arrangement is already vestigial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of the vaccine_mandate_balance kernel; how would the victim set and extraction profile change if the kernel were instantiated by a sibling reading instead?',
    'Generate and classify the sibling stories (public_health_primary, bodily_autonomy_primary) and compare computed per-seat classifications against this file''s.',
    'Under a collective-protection-first reading the objector seat becomes an unconditional payer during any outbreak with failing voluntary compliance and the robust-exemption guarantee disappears from the structure; under a consent-inviolable reading the unable-to-vaccinate lose the threshold-qualified protection this reading extends to them. This file''s parameter-conditional victim sets are the distinguishing structural fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame contingency: classification is indexed to the proportionality reading of the shared kernel.').

omega_variable(
    epsilon_pathogen_class_bimodality,
    'The gate''s operation is bimodal by pathogen class - against smallpox-class agents nearly every mandate clears the thresholds and the exemption externality dominates; against seasonal influenza almost none do and the gate operates as near-pure restraint. Does a single epsilon represent the standing arrangement adequately?',
    'Stratified re-authoring: estimate epsilon separately for severe-airborne, severe-contact-transmitted, and mild-seasonal pathogen classes and compare against the pooled value.',
    'If stratified values diverge widely, decompose into per-pathogen-class constraint stories linked by network edges; the pooled 0.44 would then be a weighted artifact of the historical pathogen mix rather than an intrinsic property of the gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_pathogen_class_bimodality, empirical, 'Whether the constraint''s epsilon is stable across the pathogen classes that drive its operation.').

omega_variable(
    exemption_robustness_operationalization,
    '''Robust exemptions'' is the reading''s load-bearing but underspecified term - does robustness mean universal self-attestation, documented religious affiliation, or medical-only plus narrow conscientious review, and how does the chosen breadth trade off against third-party infection risk?',
    'Comparative jurisdictional analysis: correlate exemption-administration designs with coverage rates and attack rates in exempt clusters during comparable outbreaks.',
    'A narrow operationalization shrinks the externality borne by the unable-to-vaccinate and pulls the reading toward the collective-protection pole; a broad one does the reverse and risks converting the exemption guarantee into the arrangement''s dominant cost-imposition channel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_operationalization, conceptual, 'Operational indeterminacy of the robustness requirement that fixes the size of the externality.').

omega_variable(
    threshold_review_politicization,
    'Are threshold determinations driven by the severity, transmission, and safety evidence as stated, or by political pressure operating through the review process?',
    'Blinded retrospective audit of completed threshold determinations against the contemporaneous evidence base.',
    'High politicization would raise the theater ratio, date the inertial-drift risk earlier, and suggest the review layer performs justification rather than conducts it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_review_politicization, empirical, 'Whether the threshold-review layer is substantive or performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.17).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__proportionality_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement_basis(vacc_tr_t4, observed).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__proportionality_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(vacc_tr_t8, observed).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__proportionality_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(vacc_tr_t12, observed).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__proportionality_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(vacc_tr_t16, observed).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__proportionality_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement_basis(vacc_tr_t20, observed).
narrative_ontology:measurement(vacc_tr_t25, vaccine_mandate_balance__proportionality_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(vacc_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement_basis(vacc_be_t4, observed).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement_basis(vacc_be_t8, observed).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 12, 0.43).
narrative_ontology:measurement_basis(vacc_be_t12, observed).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement_basis(vacc_be_t16, observed).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(vacc_be_t20, observed).
narrative_ontology:measurement(vacc_be_t25, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 25, 0.44).
narrative_ontology:measurement_basis(vacc_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.23).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement_basis(vacc_su_t4, observed).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 8, 0.27).
narrative_ontology:measurement_basis(vacc_su_t8, observed).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement_basis(vacc_su_t12, observed).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement_basis(vacc_su_t16, observed).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(vacc_su_t20, observed).
narrative_ontology:measurement(vacc_su_t25, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 25, 0.3).
narrative_ontology:measurement_basis(vacc_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'vaccine mandate balance' covers three structurally distinct arrangements and is decomposed into three stories sharing the kernel vaccine_mandate_balance. This file instantiates the proportionality_reading: mandate legitimacy is conditional on disease parameters, so the victim sets are parameter-conditional and epsilon is authored for the standing arrangement across the observed pathogen mix (see omega epsilon_pathogen_class_bimodality). The sibling stories instantiate categorical readings whose victim sets hold unconditionally - under the collective-protection-first reading objectors pay whenever voluntary compliance fails; under the consent-inviolable reading officials and third parties bear whatever an absolute refusal right costs. The epsilon values differ across the family because the victim sets differ, not because one constraint is measured two ways. Edges run from this reading to both siblings because the threshold-review machinery this reading builds is the terrain on which the categorical readings are argued.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
