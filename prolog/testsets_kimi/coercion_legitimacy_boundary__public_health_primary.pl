% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: State Compelled Medical Intervention for Collective Harm Prevention
 *   domain: public health policy / medical ethics / constitutional law
 *
 * SUMMARY:
 *   This constraint instantiates the public_health_primary reading of the
 *   coercion_legitimacy_boundary kernel: the claim that state authority to
 *   compel medical intervention is legitimate when collective harm prevention
 *   outweighs individual bodily autonomy. Under this reading, unvaccinated
 *   individuals become coerced subjects (victims), immunocompromised
 *   individuals become protected beneficiaries, and the state deploys active
 *   enforcement apparatus to secure compliance. The constraint carries a
 *   genuine coordination function (herd immunity protecting the vulnerable)
 *   alongside asymmetric extraction (bodily autonomy violation). Sibling
 *   readings are bodily_autonomy_primary (categorical prohibition) and
 *   proportionality_reading (context-scaled coercion).
 *
 * KEY AGENTS:
 *   - state_public_health_authority: Agenda-setter (institutional/constrained) â designs and enforces mandates
 *   - unvaccinated_individuals: Primary target (moderate/constrained) â bear coerced medical intervention and penalties
 *   - immunocompromised_individuals: Primary beneficiary (powerless/constrained) â receive transmission protection without coercion costs
 *   - public_health_system: Secondary beneficiary (institutional/constrained) â receives reduced caseload
 *   - civil_liberties_institutions: Analytical observer (institutional/analytical) â monitors constitutional boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.78).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.75).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "State Compelled Medical Intervention for Collective Harm Prevention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public health policy / medical ethics / constitutional law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '84b58467-47e4-40da-878a-ee4106008f1c').
narrative_ontology:cs_kernel_codification('84b58467-47e4-40da-878a-ee4106008f1c', formalized).
narrative_ontology:cs_authority_grounding('84b58467-47e4-40da-878a-ee4106008f1c', lineage).
narrative_ontology:cs_interpretation_layer_present('84b58467-47e4-40da-878a-ee4106008f1c').
narrative_ontology:cs_reading_relation('84b58467-47e4-40da-878a-ee4106008f1c', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('84b58467-47e4-40da-878a-ee4106008f1c', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('84b58467-47e4-40da-878a-ee4106008f1c', foundational, collective_harm_overrides_bodily_autonomy).
narrative_ontology:cs_axiom_status(collective_harm_overrides_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('84b58467-47e4-40da-878a-ee4106008f1c', collective_harm_overrides_bodily_autonomy, instrumental).
narrative_ontology:cs_axiom('84b58467-47e4-40da-878a-ee4106008f1c', foundational, state_police_power_includes_medical_compulsion).
narrative_ontology:cs_axiom_status(state_police_power_includes_medical_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('84b58467-47e4-40da-878a-ee4106008f1c', state_police_power_includes_medical_compulsion, conventional).
narrative_ontology:cs_reference_frame('84b58467-47e4-40da-878a-ee4106008f1c', police_power_public_health_sovereign).
narrative_ontology:cs_drift_state('84b58467-47e4-40da-878a-ee4106008f1c', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('84b58467-47e4-40da-878a-ee4106008f1c', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_system).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, herd_immunity_collective_good).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, police_power_public_health).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers vaccination mandates and public health emergency powers. Sets criteria for compelled medical intervention, deploys enforcement through legal penalties and exclusions. Justifies actions by citing epidemiological models and constitutional police power. Retains authority to modify or lift mandates based on declared threat levels.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, state_public_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Subject to legal coercion to undergo medical intervention against their personal judgment. Face job exclusion, fines, or social restrictions if non-compliant. Bear the physical risks of the intervention and the psychological cost of autonomy violation. Exit options are limited to accepting penalties or leaving the jurisdiction, both high-cost.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Unable to mount protective immune responses themselves. Depend on high community vaccination rates to avoid infection. Receive protection from reduced transmission without bearing the coercive burden of the mandate. Cannot exit their medical vulnerability.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, constrained, national).

% Absorbs reduced caseload and hospitalization burden when community transmission drops due to high compliance. Benefits operationally from the mandate but does not set it. Operates within state funding and policy constraints.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_system, beneficiary,
    institutional, generational, constrained, national).

% Monitor and litigate against coercion measures, arguing for heightened scrutiny of bodily autonomy intrusions. Publish analyses of mandate proportionality and legal precedent. Do not bear the direct costs of the constraint but track its constitutional implications.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, civil_liberties_institutions, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing epidemic transmission and protecting those who cannot be medically protected through individual action alone by solving the collective-action problem of vaccination externalities.
% TRANSFER_FUNCTION: Moves the burden of medical compliance and autonomy sacrifice from the collective to the unvaccinated individual; moves health protection and reduced transmission risk to immunocompromised individuals and the public health system.
% ABSENT_VOICES: Unvaccinated individuals are structurally excluded from the policy design table; civil liberties advocates are often marginalized under emergency framing; alternative medicine practitioners and community health workers who favor voluntary engagement over coercion are rarely consulted.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, disease transmission dynamics would shift, immunocompromised individuals would lose protection, the unvaccinated would regain full autonomy, and public health strategy would reorganize around voluntary measures or alternative containment. The legal-constitutional landscape around police power would also shift.
% FOUNDING_PROBLEM: Infectious disease outbreaks where individual non-compliance generates severe negative externalities and vulnerable populations cannot self-protect.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and public health historians attest to the negative externality problem from outside the beneficiary seat; civil liberties scholars and bioethicists from outside the state authority attest that the problem's severity is often overstated or that alternative interventions exist.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint authorizes state invasion of bodily integrity, a severe cost. Suppression is high (0.75) because persistence depends on legal penalties, employment exclusions, and social restrictions that actively suppress non-compliance. Theater ratio is moderate (0.33): much enforcement is functional (reducing transmission), but some is performative crisis management signaling state competence. Accessibility collapse is high (0.72) because once the mandate is enacted, the alternative of voluntary, uncoordinated individual choice collapses as a viable public health strategy within the jurisdiction. Resistance is substantial (0.68) due to organized anti-mandate movements, litigation, and political backlash. Measurements track a 36-month mandate lifecycle: extraction and suppression rise as enforcement matures, then plateau as resistance stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the state_public_health_authority seat, the constraint is legitimate coordination protecting vulnerable populations; effective extraction is inverted into administrative burden. From the unvaccinated_individuals seat, the identical structure is direct extraction of bodily autonomy; their high directionality amplifies effective extraction toward the full-target end. From the immunocompromised_individuals seat, the constraint appears as subsidy (low directionality, protective benefit) despite their powerlessness. The engine computes this divergence from structural data; the authored claim (tangled_rope) reflects the claim that both coordination and extraction are real and co-constituted.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (immunocompromised_individuals, public_health_system) receive low directionality: the constraint subsidizes their health and operational security. Victims (unvaccinated_individuals) receive high directionality: the constraint extracts their autonomy and subjects them to medical risk. The state_public_health_authority, as agenda_setter, sits near the beneficiary end for authority accumulation but is not a financial capturer. No override is needed because the structural derivation matches the known relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â severe infectious disease outbreaks with negative externalities â is historically real and can be corroborated by epidemiological record. The mandate structure prevents mislabeling by requiring both beneficiaries and victims: if the constraint were pure extraction (snare), it would lack the genuine protective function for immunocompromised individuals; if it were pure coordination (rope), it would lack the coerced victim set. The tangled_rope classification captures the hybrid. Mandatrophy risk arises if the constraint persists after the founding epidemic subsides: the temporal measurements show rising theater_ratio over the interval, suggesting performance may be layering onto function, but the base properties are measured for the active mandate phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the public_health_primary reading of the coercion_legitimacy_boundary kernel. How would the classification change if bodily_autonomy_primary or proportionality_reading were adopted instead?',
    'Compare across the three constraint files in the kernel family; each reading carries its own invariant epsilon, beneficiary/victim structure, and directionality profile.',
    'Adopting bodily_autonomy_primary would eliminate the victim set (unvaccinated) and collapse epsilon; adopting proportionality_reading would make the victim set context-dependent and reduce epsilon for low-severity diseases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega documenting this constraint''s position within the coercion_legitimacy_boundary kernel.').

omega_variable(
    collective_harm_threshold_determinacy,
    'Is the threshold of ''collective harm outweighing individual autonomy'' objectively determinable by epidemiological data, or is it irreducibly constructed by political and institutional processes?',
    'Cross-jurisdictional comparison of mandate triggers for identical diseases; if thresholds vary systematically with political systems rather than disease severity, the threshold is constructed.',
    'If constructed, the constraint''s extraction is partially a function of political power rather than natural necessity, supporting tangled_rope or snare classification; if objectively determinable, the coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_harm_threshold_determinacy, empirical, 'Whether the triggering threshold for coercion is objective or constructed.').

omega_variable(
    coercion_efficacy_vs_voluntary_alternative,
    'Does compelled medical intervention produce superior public health outcomes compared to the voluntary-uptake counterfactual, net of enforcement costs, resistance, and trust erosion?',
    'Natural experiments across jurisdictions with differing mandate intensity for the same pathogen; meta-analysis of outbreak rates controlling for baseline voluntarism.',
    'If coercion adds no marginal protection, the coordination function is cover for extraction (snare); if coercion adds substantial protection unavailable by other means, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_efficacy_vs_voluntary_alternative, empirical, 'Efficacy of coerced versus voluntary public health compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coer_tr_t6, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 6, 0.22).
narrative_ontology:measurement(coer_tr_t12, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 12, 0.25).
narrative_ontology:measurement(coer_tr_t18, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 18, 0.28).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 24, 0.3).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 30, 0.32).
narrative_ontology:measurement(coer_tr_t36, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 36, 0.33).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(coer_be_t6, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(coer_be_t12, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(coer_be_t18, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 18, 0.73).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(coer_be_t36, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 36, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(coer_su_t6, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(coer_su_t12, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(coer_su_t18, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 18, 0.75).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(coer_su_t36, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 36, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the coercion_legitimacy_boundary kernel, decomposed from bodily_autonomy_primary and proportionality_reading per the epsilon-invariance principle. Each reading carries a distinct epsilon and stakeholder geometry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
