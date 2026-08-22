% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Vaccination Mandate Authority (Public Health Reading)
 *   domain: public_health/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint embodies the public-health-primary reading of vaccination
 *   mandate legitimacy: the state may compel vaccination when necessary to
 *   protect vulnerable populations (immunocompromised, infants,
 *   unvaccinatable) from serious harm. The reading asserts that collective
 *   protection duties can override individual choice when the vulnerable are
 *   genuinely defenseless. The constraint is CLAIMED as tangled_rope (genuine
 *   coordination of herd immunity + asymmetric extraction from those
 *   compelled) while authored metrics show substantive suppression (0.72) and
 *   extractiveness (0.68) — the gap is intentional and reflects the core
 *   contestation: is the extraction a necessary price of coordination, or is
 *   it disguised compulsion where the coordination story conceals state
 *   overreach? This story instantiates ONLY the public-health-primary
 *   reading; the bodily_autonomy_primary and proportionality_reading are
 *   separate constraints with different beneficiary/victim structures and
 *   different ε values.
 *
 * KEY AGENTS:
 *   - immunocompromised_populations: powerless victims of disease; depend entirely on mandate for protection
 *   - state_public_health_authority: institutional agenda-setter; controls enforcement machinery
 *   - unvaccinated_adults: moderate-power payers; forced to bear medical intervention or face exclusion
 *   - vaccine_hesitant_populations: dual position; accept vaccination benefit but resist compulsion
 *   - medical_autonomy_advocates: excluded from mandate-setting; mount opposition but do not govern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.68).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.72).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Vaccination Mandate Authority (Public Health Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '0aa68053-1b02-4d98-b66a-d2b351b11f89').
narrative_ontology:cs_kernel_codification('0aa68053-1b02-4d98-b66a-d2b351b11f89', formalized).
narrative_ontology:cs_authority_grounding('0aa68053-1b02-4d98-b66a-d2b351b11f89', extraction).
narrative_ontology:cs_interpretation_layer_present('0aa68053-1b02-4d98-b66a-d2b351b11f89').
narrative_ontology:cs_reading_relation('0aa68053-1b02-4d98-b66a-d2b351b11f89', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('0aa68053-1b02-4d98-b66a-d2b351b11f89', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('0aa68053-1b02-4d98-b66a-d2b351b11f89', foundational, collective_protection_duty_overrides_individual_choice).
narrative_ontology:cs_axiom_status(collective_protection_duty_overrides_individual_choice, holdable).
narrative_ontology:cs_axiom_grounding('0aa68053-1b02-4d98-b66a-d2b351b11f89', collective_protection_duty_overrides_individual_choice, deontological).
narrative_ontology:cs_axiom('0aa68053-1b02-4d98-b66a-d2b351b11f89', foundational, state_authority_to_compel_medical_intervention_legitimate_for_vulnerable_defense).
narrative_ontology:cs_axiom_status(state_authority_to_compel_medical_intervention_legitimate_for_vulnerable_defense, holdable).
narrative_ontology:cs_axiom_grounding('0aa68053-1b02-4d98-b66a-d2b351b11f89', state_authority_to_compel_medical_intervention_legitimate_for_vulnerable_defense, deontological).
narrative_ontology:cs_reference_frame('0aa68053-1b02-4d98-b66a-d2b351b11f89', state_duty_to_protect_vulnerable_populations).
narrative_ontology:cs_drift_state('0aa68053-1b02-4d98-b66a-d2b351b11f89', endemic_phase_low_threat, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0aa68053-1b02-4d98-b66a-d2b351b11f89', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, infants_and_unvaccinatable).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_adults).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot mount immune response to infection; depend entirely on population-level vaccination barriers to avoid life-threatening disease. Their protection exists only when unvaccinated individuals are excluded or compelled into vaccination. They have no realistic exit — they cannot leave the jurisdiction to find an unvaccinated population, nor can they opt out of needing protection.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Too young to vaccinate or medically unable to receive vaccines; depend on herd immunity thresholds maintained by others' vaccination or mandate compliance. Their survival from vaccine-preventable disease is entirely contingent on the surrounding population's status. They cannot consent to or refuse mandates; their protection requires others' compelled cooperation.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, infants_and_unvaccinatable, beneficiary,
    powerless, biographical, trapped, national).

% Face mandatory vaccination under the constraint or exclusion from employment, education, or public spaces. Their choice set is medical intervention without refusal option, departure from jurisdiction, or acceptance of exclusion. The mandate asserts a duty to vaccinate in order to protect others — a duty they dispute on grounds of bodily autonomy or risk assessment.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_adults, payer,
    moderate, biographical, constrained, national).

% Accept the general benefit of vaccination but resist compulsion on grounds that the decision should be informed and voluntary. They may vaccinate when mandates are absent or when presented with clear evidence, but internalize the mandate as coercive. They bear both the extraction (compulsion) and indirect benefit (disease prevention), creating a dual position.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_populations, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_populations, beneficiary).

% Sets vaccination requirements, enforces them through license denial, employment restrictions, school exclusions, and public-space access rules. Justifies the mandate as necessary to maintain herd immunity and protect vulnerable populations. Collects no direct rents but exercises coercive authority over medical decision-making, backed by law enforcement and institutional policy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, state_public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Would argue that medical intervention without informed consent violates fundamental bodily integrity and that collective benefit cannot override individual choice. They are excluded from the mandate-setting process; their voice appears in courtroom dissent and public opposition but does not govern the constraint itself.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, medical_autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% Measures herd immunity thresholds, vaccine efficacy and safety, and disease burden in protected vs. unprotected populations. Provides evidence to both the state (supporting mandate necessity) and to dissenting parties (establishing disease severity and vaccine safety profiles). Their role is epistemic, not decisional.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, epidemiological_research_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__public_health_primary, state_public_health_authority).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of achieving herd immunity: individual incentives favor free-riding on others' vaccination, but the public health goal requires near-universal coverage. Without mandate enforcement, coverage typically stalls below protective thresholds, leaving vulnerable populations exposed.
% TRANSFER_FUNCTION: Moves the burden of medical intervention and bodily exposure to vaccines from the state/medical professionals onto unvaccinated individuals, who bear the risk (however small) of adverse effects in service to protecting others. The state transfers its protective duty onto individual bodies.
% ABSENT_VOICES: Unvaccinated individuals and those who believe medical autonomy is inviolable are partially silenced: they can mount legal challenge and public opposition, but they do not participate in setting the mandate threshold or determining enforcement mechanisms. Populations with past medical trauma or distrust of state institutions are structurally underrepresented in mandate-setting deliberation.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, vaccination rates would fall in the absence of compulsion; disease circulation would accelerate; vulnerable populations would face serious morbidity and mortality risk. The state would lose its primary enforcement tool for herd immunity. The constraint's disappearance would directly cascade into restructured disease burden.
% FOUNDING_PROBLEM: Vaccine-preventable diseases pose existential risk to immunocompromised and unvaccinatable populations; voluntary vaccination does not achieve herd immunity thresholds necessary to protect them; free-riding incentives cause individual choice aggregates to fall below collective need.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological research confirms unvaccinated populations generate disease reservoirs that threaten vulnerable groups; immunologists attest immunocompromised populations genuinely cannot protect themselves. Medical autonomy advocates dispute the founding problem, arguing the state overstates disease risk and understates vaccine risk, but do not deny that vulnerable populations exist and face real disease burden — their dispute is about mandate necessity, not about the existence of vulnerable populations.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint imposes medical intervention on unwilling subjects despite the coordination function being genuine. The beneficiaries (immunocompromised, infants) cannot produce the protection themselves — they are genuinely dependent, not free-riding. The payers (unvaccinated adults, vaccine-hesitant) bear both direct risk (vaccine adverse effects, however small) and indirect burden (coerced choice). Theater is low-moderate (0.28): the public-health justification is real — herd immunity is a genuine coordination problem — but enforcement also performs legitimation of state medical authority beyond what minimalist disease control would require. Suppression is high (0.72) because non-compliance is met with employment termination, school exclusion, and access denial; alternatives collapse once the mandate is understood (0.79 accessibility_collapse). Resistance remains substantial (0.61) because bodily autonomy advocates, vaccine-hesitant populations, and anti-mandate political movements mount active opposition. The measurement series show extractiveness and suppression plateauing after year 10, suggesting the constraint has reached a stable coercive equilibrium — initial escalation of enforcement intensity levels off as compliance reaches an attractor point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state authority) perceives the constraint as necessary coordination, justified by vulnerable populations' genuine defenselessness. From this seat, extractiveness appears low-to-moderate and suppression appears minimal — what looks like coercion appears as enforcement of a duty to protect. The payer seats (unvaccinated, vaccine-hesitant) perceive the same constraint as coercive overreach justified post-hoc with vulnerable-population rhetoric. From these seats, extractiveness is high and suppression is visible — the constraint appears as state power claiming medical authority it should not hold. The excluded seat (medical autonomy advocates) perceives the constraint as categorical violation of bodily integrity regardless of health outcomes. The divergence is not perceptual confusion — it is structural: the constraint genuinely coordinates herd immunity (supporting the beneficiary reading) while genuinely imposing medical intervention without consent (supporting the autonomy reading). The engine computes per-seat types from the structural data; this is where they should diverge most sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   The public-health-primary reading generates sharp directionality divergence between seats. Immunocompromised populations sit at d≈0.0 (full beneficiaries — the constraint subsidizes their survival, they bear no cost except dependency vulnerability). The state sits at d≈0.3 (beneficiary-leaning — it achieves its mandate, incurs enforcement costs, but collects legitimacy and institutional authority). Unvaccinated adults sit at d≈0.9 (near-full targets — they bear the extraction, have constrained exit, and resist the framing). Vaccine-hesitant populations sit at d≈0.65 (targets, but with secondary beneficiary position because they genuinely do receive disease protection even against their stated preferences). This divergence is the core feature the reading must model: under public-health-primary framing, the powerless vulnerable become genuine beneficiaries with no exit (inversion of typical snare patterns), while moderate-power unvaccinated become isolated targets with constrained but non-zero exit. A snare would require all payers to be powerless and excluded; this reading's structure requires powerless beneficiaries, making it tangled_rope rather than snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that vulnerable populations face unprotected disease exposure without herd immunity — is live. The state still enforces mandates, vaccination coverage remains above herd-immunity thresholds in most jurisdictions, and disease burden in unvaccinated populations remains observable. However, mandatrophy risk arises when the mandate persists at high intensity (suppression 0.72, accessibility_collapse 0.79) after the original founding pathogen (e.g., SARS-CoV-2) has evolved to endemic status with lower case-fatality rates. If theater_ratio rises to >0.4 while extractiveness remains high, that signals the constraint is performing state medical authority maintenance more than disease control — a mandatrophy trajectory. The measurement series show theater plateauing at 0.28, so mandatrophy is not yet manifesting, but watchpoint conditions are: if the pathogen threat diminishes further while suppression intensity remains constant or rises, mandatrophy becomes the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_necessity_vs_rhetorical_cover,
    'Is the mandate genuinely necessary to achieve herd immunity thresholds, or is it a means of asserting state medical authority that could be achieved through voluntary uptake campaigns?',
    'Comparative analysis of vaccination rates across jurisdictions with and without mandates, controlling for education/outreach intensity, disease threat perception, and vaccine confidence. Determine the counterfactual: what coverage would voluntary campaigns achieve?',
    'If mandates add little marginal coverage beyond what intensive voluntary campaigns achieve, the extraction becomes visible as unjustified coercion. If mandates are necessary for herd immunity, the extraction becomes justifiable coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_necessity_vs_rhetorical_cover, empirical, 'Whether mandate extraction is structurally necessary for herd immunity or merely sufficient/convenient for state authority.').

omega_variable(
    vulnerable_population_composition_and_exit,
    'Are the beneficiaries (immunocompromised, infants) genuinely trapped, or do they have hidden exit options (migration to unvaccinated regions, self-isolation, pharmaceutical prophylaxis) that reduce their dependence on the mandate?',
    'Ethnographic documentation of vulnerable-population coping strategies and mobility; pharmaceutical landscape analysis for non-vaccine protective options.',
    'If vulnerable populations have real exit options, their position as full beneficiaries weakens, and the mandate''s extractiveness becomes more visible — it might persist not because they need it, but because it serves state authority interests. If truly trapped, the mandate''s justification is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vulnerable_population_composition_and_exit, empirical, 'Degree of actual vs. structural dependency of vulnerable populations on mandate maintenance.').

omega_variable(
    kernel_framing_instability,
    'Is the kernel stable as ''state medical authority legitimacy'' or does it dissolve into separate kernels — one about emergency disease control, one about routine public health, one about state power scope — once the founding pathogen threat recedes?',
    'Post-endemic phase observation: does the same mandate authority architecture persist for endemic pathogens with lower mortality, or does it revert to informed-choice framing?',
    'If the kernel dissolves, this reading''s claim-to-legitimacy becomes reading-specific rather than universalizable. If the kernel remains stable, it suggests state medical authority claims a broader domain than crisis response.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_instability, conceptual, 'Whether the contested kernel is stable across threat levels or threat-dependent.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (employment loss, school exclusion, access denial are real external barriers), or do unvaccinated populations also carry internalized suppression (shame, isolation from community, belief they are endangering others) that persists even if external barriers were removed?',
    'Post-mandate-removal trajectory: if suppression persists in attitudes/behavior even after structural mechanisms end, the suppression is partially internalized (the constraint has fused into identity).',
    'If suppression is primarily structural, the constraint becomes less extractive post-removal (enforcement cost drops, compliance drops, exit opens). If partially internalized, the constraint has deeper purchase — even unvaccinated populations may continue complying or internalizing duty-to-protect narratives after formal mandate ends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression is external enforcement or partially fused into subject identity.').

omega_variable(
    reading_foreclosure_boundary,
    'Does the public-health-primary reading''s core premise — ''state authority to compel vaccination is legitimate when necessary to protect vulnerable populations'' — logically foreclose the bodily-autonomy-primary reading''s core premise — ''medical intervention without consent violates bodily integrity regardless of public benefit'' — or do both premises coexist in a single framework via threshold-dependent legitimacy?',
    'Axiomatic analysis of the two readings'' foundational claims. If both can be true (legitimacy requires BOTH public health necessity AND bodily-autonomy respect, with thresholds determining which dominates), they coexist. If one directly contradicts the other (no compromise possible), one forecloses the other.',
    'If forecloses: only one reading can be true in a single institutional framework, and choosing this one rules out the other. If coexists: both readings remain live options, and different jurisdictions can implement different readings without logical contradiction — the contest is political, not logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether public-health-primary forecloses bodily-autonomy-primary or both can coexist as live institutional choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(mand_tr_t0, observed).
narrative_ontology:measurement(mand_tr_t3, mandate_legitimacy_scope__public_health_primary, theater_ratio, 3, 0.2).
narrative_ontology:measurement_basis(mand_tr_t3, observed).
narrative_ontology:measurement(mand_tr_t6, mandate_legitimacy_scope__public_health_primary, theater_ratio, 6, 0.23).
narrative_ontology:measurement_basis(mand_tr_t6, observed).
narrative_ontology:measurement(mand_tr_t10, mandate_legitimacy_scope__public_health_primary, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(mand_tr_t10, observed).
narrative_ontology:measurement(mand_tr_t15, mandate_legitimacy_scope__public_health_primary, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(mand_tr_t15, observed).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__public_health_primary, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(mand_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(mand_be_t0, observed).
narrative_ontology:measurement(mand_be_t3, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 3, 0.61).
narrative_ontology:measurement_basis(mand_be_t3, observed).
narrative_ontology:measurement(mand_be_t6, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 6, 0.64).
narrative_ontology:measurement_basis(mand_be_t6, observed).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(mand_be_t10, observed).
narrative_ontology:measurement(mand_be_t15, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(mand_be_t15, observed).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(mand_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(mand_su_t0, observed).
narrative_ontology:measurement(mand_su_t3, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 3, 0.65).
narrative_ontology:measurement_basis(mand_su_t3, observed).
narrative_ontology:measurement(mand_su_t6, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(mand_su_t6, observed).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(mand_su_t10, observed).
narrative_ontology:measurement(mand_su_t15, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(mand_su_t15, observed).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(mand_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__public_health_primary, 0.18).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'mandate_legitimacy_scope.' The sibling constraints are bodily_autonomy_primary (ε high, victims = all vaccinated under coercion, beneficiaries = none; pure extraction reading) and proportionality_reading (ε moderate, beneficiary/victim structure conditional on disease severity and vaccine safety; coordination with thresholds). The three readings share a kernel (state authority to compel medical intervention) but instantiate different constraints with different ε values because they assess the same standing arrangement (vaccination mandate) against different normative frames (public health protection vs. bodily autonomy vs. means-ends proportionality). The public-health-primary reading assesses the mandate as meeting its coordination function (herd immunity) and treats the extractive costs (coerced vaccination) as justifiable coordination prices. The bodily-autonomy-primary reading assesses the same mandate as coercive violation regardless of outcome. The proportionality-reading asserts the mandate's legitimacy is conditional on empirical parameters (disease severity, vaccine risk, alternatives). All three are valid constraints under the ε-invariance principle because they evaluate the same standing arrangement under different reading commitments, producing genuinely different structural assessments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
