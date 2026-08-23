% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate as Categorical Bodily Sovereignty Violation
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the public_health_mandate_authority kernel. It reads the mandate not as a
 *   coordination tool for collective protection but as a categorical
 *   violation of bodily sovereignty — the state's claim to compel medical
 *   intervention against individual consent. The reading denies that any
 *   collective benefit (protection of immunocompromised, healthcare capacity
 *   preservation, herd immunity) can legitimize non-consensual bodily
 *   intrusion. Unvaccinated individuals are the primary victims, experiencing
 *   coercive extraction of bodily autonomy and material penalties.
 *   Immunocompromised individuals are excluded from the victim set — this
 *   reading rejects the claim that their vulnerability creates a duty in
 *   others to submit to bodily invasion. Public health authorities,
 *   pharmaceutical industry, and the state emergency apparatus are
 *   beneficiaries who capture the mandate's gains. The claimed type is snare:
 *   pure extraction maintained by active enforcement, with identifiable
 *   victims and no genuine coordination function acknowledged by this
 *   reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.9).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate as Categorical Bodily Sovereignty Violation").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '0528dfb8-270b-4a7c-85fe-d79332f842f7').
narrative_ontology:cs_kernel_codification('0528dfb8-270b-4a7c-85fe-d79332f842f7', formalized).
narrative_ontology:cs_authority_grounding('0528dfb8-270b-4a7c-85fe-d79332f842f7', lineage).
narrative_ontology:cs_interpretation_layer_present('0528dfb8-270b-4a7c-85fe-d79332f842f7').
narrative_ontology:cs_reading_relation('0528dfb8-270b-4a7c-85fe-d79332f842f7', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('0528dfb8-270b-4a7c-85fe-d79332f842f7', public_health_mandate_authority__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('0528dfb8-270b-4a7c-85fe-d79332f842f7', foundational, bodily_sovereignty_categorical).
narrative_ontology:cs_axiom_status(bodily_sovereignty_categorical, holdable).
narrative_ontology:cs_axiom_grounding('0528dfb8-270b-4a7c-85fe-d79332f842f7', bodily_sovereignty_categorical, deontological).
narrative_ontology:cs_axiom('0528dfb8-270b-4a7c-85fe-d79332f842f7', foundational, no_collective_benefit_justifies_bodily_invasion).
narrative_ontology:cs_axiom_status(no_collective_benefit_justifies_bodily_invasion, holdable).
narrative_ontology:cs_axiom_grounding('0528dfb8-270b-4a7c-85fe-d79332f842f7', no_collective_benefit_justifies_bodily_invasion, deontological).
narrative_ontology:cs_reference_frame('0528dfb8-270b-4a7c-85fe-d79332f842f7', constitutional_bodily_integrity_1905).
narrative_ontology:cs_drift_state('0528dfb8-270b-4a7c-85fe-d79332f842f7', contemporary_pandemic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0528dfb8-270b-4a7c-85fe-d79332f842f7', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, state_emergency_apparatus).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, bodily_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, informed_consent_principle).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, medical_freedom_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face coercive mandates requiring medical intervention as condition for employment, education, travel, and public participation. Their refusal is grounded in bodily sovereignty conviction; exit requires abandoning livelihood, community, or identity. The constraint extracts bodily autonomy and imposes material penalties for non-compliance.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    moderate, biographical, identity_locked, national).

% Administer mandate infrastructure: define vaccine requirements, set compliance deadlines, enforce penalties, and control exemption criteria. They collect institutional authority, budgetary resources, and emergency powers through the mandate apparatus. Their position is sustained by the legal framework they interpret and enforce.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Would claim protection via universal mandates in the public_health_primary reading, but this reading excludes them from the victim set — it denies a duty to protect them through others' bodily invasion. Their vulnerability is acknowledged but not accepted as a claim on others' bodies.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_individuals, excluded,
    moderate, biographical, constrained, national).

% Advocate for mandate legitimacy based on collective protection of vulnerable populations and healthcare infrastructure. This reading assigns them zero extractiveness — they experience no coercion from mandates they support. They occupy an analytical seat observing the constraint's operation on others.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, observer,
    organized, biographical, analytical, national).

% Receives guaranteed population-scale demand and liability protection through mandate structures. The mandate apparatus functions as a de facto procurement and market-creation mechanism. Their influence shapes the mandate schedule through regulatory capture and advisory committee positions.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, pharmaceutical_industry, beneficiary,
    powerful, generational, arbitrage, global).

% Expands and normalizes emergency powers through recurring mandate cycles. Each mandate activation ratchets institutional capacity for bodily regulation, surveillance, and compliance enforcement. The mandate is a vehicle for permanent emergency architecture.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, state_emergency_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, state_emergency_apparatus, agenda_setter).

% Evaluates the constraint's structural properties across readings: extraction profile, suppression mechanisms, beneficiary/victim distribution, and the kernel's drift across pandemic and post-pandemic eras.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level disease suppression through universal compulsory medical intervention, replacing voluntary uptake with state-enforced compliance to achieve herd immunity thresholds deemed necessary by public health authorities.
% TRANSFER_FUNCTION: Moves bodily autonomy and medical decision-making authority from individuals to the state/public health apparatus; moves compliance costs (employment loss, educational exclusion, travel restriction, social marginalization) to unvaccinated individuals; moves guaranteed revenue and liability protection to pharmaceutical manufacturers; moves expanded emergency powers to state apparatus.
% ABSENT_VOICES: Immunocompromised individuals who would claim a right to protection via others' mandated vaccination are excluded from this reading's victim set — their perspective that collective protection creates a duty of bodily contribution is not represented. Children and future generations who inherit the precedent of state bodily authority are structurally absent. Dissenting medical professionals who challenge mandate necessity or safety are excluded from the official consensus.
% DISAPPEARANCE_RATIONALE: If mandate authority vanished overnight, vaccination would revert to voluntary; disease dynamics would shift to endemic management; public health authorities would lose compulsory tools and revert to persuasion-based campaigns; pharmaceutical revenue models would adjust to market demand; the legal precedent of Jacobson v. Massachusetts would be effectively overturned or narrowed; the emergency powers architecture would lose its primary activation trigger.
% FOUNDING_PROBLEM: The mandate authority was built to solve the coordination problem of achieving population immunity against infectious disease threats (historically smallpox, polio) through compulsory rather than voluntary means, grounded in the state's police power to override individual liberty for collective survival.
% FOUNDING_PROBLEM_CORROBORATION: Public health historians document the origin in 1905 Jacobson v. Massachusetts (smallpox) and 1922 Zucht v. King (school mandates); legal scholars outside the public health establishment (e.g., bodily autonomy advocates, originalist jurists) corroborate the coercive character and the expansion beyond original threat profile; the reading's proponents attest the founding problem is either dead (smallpox eradicated, polio near-eradication) or never justified the categorical override principle now applied to novel pathogens with different risk profiles.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.85 because the mandate claims total authority over the bodily interior — the most intimate domain of self-ownership — and enforces compliance through existential penalties (livelihood, education, movement). Suppression is 0.9 because alternatives (voluntary vaccination, targeted protection, early treatment) are actively suppressed by the mandate structure itself; the constraint's persistence depends on foreclosing exit. Theater ratio is low (0.15) because the enforcement machinery is functional and consequential, not performative — people actually lose jobs, licenses, and access. Accessibility collapse is 0.9 because once the mandate activates, the unvaccinated's social and economic participation collapses nearly completely. Resistance is 0.7 reflecting sustained legal, political, and civil disobedience pushback. The measurement series shows extractiveness and suppression accelerating sharply in the 2020-2024 pandemic period, while theater remains low — the constraint became more extractive and more suppressive without becoming more performative.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the unvaccinated seat, the constraint is snare (pure extraction, high suppression, identity-locked exit). From the public health authority seat, the same constraint may compute as rope or tangled_rope (they see coordination function, they administer it, they have arbitrage exit). From the pharmaceutical industry seat, it computes as beneficiary extraction (they capture gains, no coercion). The analytical observer sees the full structure. This divergence is the measurement — the constraint is not one type but a structure that types differently per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals are full targets (d ≈ 1.0): they bear the full coercive force, have identity-locked exit (bodily sovereignty conviction makes compliance identity-destructive), and capture zero benefit. Public health authorities are agenda_setters with arbitrage exit — they administer the constraint and can move between institutional roles. Pharmaceutical industry and state emergency apparatus are beneficiaries with arbitrage exit — they capture gains without bearing coercion. Immunocompromised individuals are excluded voices: this reading structurally denies their claim on others' bodies, so they experience neither extraction nor benefit from this constraint's operation. Public health primary advocates are analytical observers with zero extractiveness — they support the mandate and experience no coercion from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (smallpox/polio eradication via compulsion) is historically dead — those diseases are eradicated or near-eradication. Yet the mandate authority persists and expands to novel pathogens with radically different risk profiles (IFR, age stratification, transmission dynamics). The authority has not sunset; it has metastasized. The mandate structure now serves as a permanent emergency architecture that ratchets state bodily authority with each activation. The coordination function (disease suppression) is real but the mandate's categorical scope — no sliding scale, no exit, no proportionality — exceeds the coordination need and functions as extraction of bodily sovereignty precedent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_structure,
    'How does the public_health_mandate_authority kernel structurally decompose into the three declared readings, and what does each reading''s ε-invariance imply about the kernel''s contested nature?',
    'Map each reading''s beneficiary/victim structure, extractiveness profile, and suppression mechanism to identify the irreducible structural ambiguity at the kernel level: is the kernel a coordination mechanism (public_health_primary), a calibrated tool (proportionality), or an extraction apparatus (bodily_autonomy_primary)?',
    'If the kernel admits multiple ε-invariant readings with divergent victim sets, the kernel itself is not a single constraint but a contested commitment structure — the decomposition into separate constraint stories is analytically necessary, not optional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_structure, conceptual, 'Kernel decomposition into three ε-invariant readings with mutually exclusive victim/beneficiary structures').

omega_variable(
    coordination_function_denial,
    'Does the mandate apparatus possess any genuine coordination function from the bodily_autonomy_primary reading''s perspective, or is the coordination story entirely cover for extraction?',
    'Counterfactual: if mandates were repealed, would voluntary uptake + targeted protection achieve comparable disease outcomes? If yes, the coordination function is substitutable and the mandate''s marginal coordination value is near zero — supporting snare classification. If no, the mandate has irreducible coordination value that this reading denies — requiring tangled_rope reassessment.',
    'Determines whether this reading''s snare claim (pure extraction, zero coordination) is descriptively accurate or ideologically motivated denial of coordination reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_denial, empirical, 'Whether the mandate has any irreducible coordination function that this reading must acknowledge').

omega_variable(
    immunocompromised_exclusion_justice,
    'Is the exclusion of immunocompromised individuals from the victim set a structural feature of this reading''s logic, or a moral blind spot that would collapse under scrutiny?',
    'Test whether the reading''s categorical principle (no duty to protect via bodily invasion) consistently applies to other domains (e.g., environmental pollution, secondhand smoke, drunk driving laws) or selectively exempts vaccine mandates. Consistency would support structural coherence; selective application would indicate motivated reasoning.',
    'If selective, the reading''s victim/beneficiary structure is unstable — immunocompromised individuals would re-enter the victim set under consistent application, altering the extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_exclusion_justice, conceptual, 'Whether immunocompromised exclusion is principled or selective').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 1905, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phm_bodily_autonomy_tr_t1905, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 1905, 0.05).
narrative_ontology:measurement(phm_bodily_autonomy_tr_t1955, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 1955, 0.08).
narrative_ontology:measurement(phm_bodily_autonomy_tr_t1977, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(phm_bodily_autonomy_tr_t2000, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(phm_bodily_autonomy_tr_t2020, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(phm_bodily_autonomy_tr_t2024, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(phm_bodily_autonomy_be_t1905, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 1905, 0.3).
narrative_ontology:measurement(phm_bodily_autonomy_be_t1955, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 1955, 0.4).
narrative_ontology:measurement(phm_bodily_autonomy_be_t1977, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 1977, 0.5).
narrative_ontology:measurement(phm_bodily_autonomy_be_t2000, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(phm_bodily_autonomy_be_t2020, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(phm_bodily_autonomy_be_t2024, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(phm_bodily_autonomy_su_t1905, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 1905, 0.4).
narrative_ontology:measurement(phm_bodily_autonomy_su_t1955, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement(phm_bodily_autonomy_su_t1977, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 1977, 0.6).
narrative_ontology:measurement(phm_bodily_autonomy_su_t2000, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(phm_bodily_autonomy_su_t2020, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2020, 0.88).
narrative_ontology:measurement(phm_bodily_autonomy_su_t2024, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the public_health_mandate_authority kernel. The bodily_autonomy_primary reading forecloses both siblings: its categorical bodily sovereignty premise logically contradicts the public_health_primary reading's collective duty premise and the proportionality_reading's sliding-scale premise. All three stories share the kernel_id but instantiate distinct constraints with divergent ε, victim sets, and claimed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__bodily_autonomy_primary, moderate, 0.95).
constraint_indexing:directionality_override(public_health_mandate_authority__bodily_autonomy_primary, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
