% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: State-Mandated Medical Intervention (Bodily Autonomy Reading)
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   A state health authority mandates a medical intervention (vaccination,
 *   treatment, quarantine, or bodily procedure) as a condition of employment,
 *   school attendance, or public participation. The authority justifies the
 *   mandate by epidemiological necessity—reducing disease transmission or
 *   protecting vulnerable populations. This constraint is authored from the
 *   BODILY AUTONOMY READING, which asserts that legitimacy requires informed
 *   individual consent and that state coercion violates bodily integrity as a
 *   matter of constitutional principle, regardless of public health benefit.
 *   The claim/metric gap is deliberate and central: the authority CLAIMS the
 *   arrangement is rope (genuine coordination solving a real
 *   collective-action problem); this reading's author CLAIMS it is
 *   tangled_rope (coordination function plus asymmetric extraction via state
 *   coercion); the authored metrics (high extractiveness, high suppression,
 *   rising theater as enforcement infrastructure hardens and justifications
 *   become ritualized) reflect the bodily autonomy reading's perception. The
 *   engine will compute divergent seat classifications: from the state health
 *   authority's position the constraint solves a real coordination problem
 *   with justified coercion; from the refused individual's position the same
 *   structure operates as bodily seizure dressed in epidemiological language.
 *   This divergence is the point—the constraint is contested exactly because
 *   the two readings produce incommensurable legitimacy verdicts.
 *
 * KEY AGENTS:
 *   - mandate_refused_individuals: Powerless, trapped; bear the extraction (bodily disposition loss, legal penalties, employment loss)
 *   - occupationally_coerced_workers: Moderate power, constrained exit; forced choice between autonomy and livelihood
 *   - public_health_administration: Institutional power, sets and enforces the mandate; collects authority over bodily compliance
 *   - mandate_compliant_citizens: Organized, mobile; benefit from disease reduction and social coordination
 *   - independent_medical_practitioners: Moderate power, excluded from mandate-setting; face licensing threat if they advise refusal
 *   - constitutional_courts: Institutional power, analytical seat; adjudicate whether coercion is constitutional
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "State-Mandated Medical Intervention (Bodily Autonomy Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '2ce6471e-b395-47cd-bf5f-a6160b606ea6').
narrative_ontology:cs_kernel_codification('2ce6471e-b395-47cd-bf5f-a6160b606ea6', formalized).
narrative_ontology:cs_authority_grounding('2ce6471e-b395-47cd-bf5f-a6160b606ea6', extraction).
narrative_ontology:cs_interpretation_layer_present('2ce6471e-b395-47cd-bf5f-a6160b606ea6').
narrative_ontology:cs_reading_relation('2ce6471e-b395-47cd-bf5f-a6160b606ea6', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('2ce6471e-b395-47cd-bf5f-a6160b606ea6', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('2ce6471e-b395-47cd-bf5f-a6160b606ea6', foundational, bodily_autonomy_categorically_inviolable).
narrative_ontology:cs_axiom_status(bodily_autonomy_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('2ce6471e-b395-47cd-bf5f-a6160b606ea6', bodily_autonomy_categorically_inviolable, deontological).
narrative_ontology:cs_axiom('2ce6471e-b395-47cd-bf5f-a6160b606ea6', foundational, coercion_invalidates_legitimacy).
narrative_ontology:cs_axiom_status(coercion_invalidates_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2ce6471e-b395-47cd-bf5f-a6160b606ea6', coercion_invalidates_legitimacy, deontological).
narrative_ontology:cs_reference_frame('2ce6471e-b395-47cd-bf5f-a6160b606ea6', liberal_consent_doctrine).
narrative_ontology:cs_drift_state('2ce6471e-b395-47cd-bf5f-a6160b606ea6', pandemic_emergency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2ce6471e-b395-47cd-bf5f-a6160b606ea6', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, population_disease_prevention).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, public_health_administration).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_refused_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, occupationally_coerced_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, occupationally_coerced_workers).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, mandate_compliant_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens who refuse a state-mandated medical intervention (vaccine, quarantine, or treatment) on grounds of bodily autonomy, religious conviction, or medical judgment. Face legal sanctions: fines escalating to criminal charges, employment termination, school exclusion, or institutional confinement. No meaningful alternative exists within the jurisdiction; exit means abandoning citizenship or livelihood. Their explicit refusal is overridden by state authority framing the intervention as necessary for collective welfare.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_refused_individuals, payer,
    powerless, biographical, trapped, national).

% Healthcare workers, educators, and other licensed professionals faced with mandate-or-lose-employment. Technically they choose; practically the choice is coerced by occupational gatekeeping and licensing authority. They receive continued employment and social standing (secondary benefit) but at the cost of bodily disposition they did not freely consent to. Their exit option is career termination.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, occupationally_coerced_workers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, occupationally_coerced_workers, beneficiary).

% State health departments and regulatory agencies that author and enforce the mandate. They justify interventions by epidemiological benefit (reducing disease prevalence, protecting vulnerable populations) and claim enforcement is necessary because voluntary uptake is insufficient. They determine who is exempt, which interventions are mandatory, and what penalties attach to refusal. Collectivist reasoning dominates their legitimacy frame.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_administration, agenda_setter,
    institutional, generational, analytical, national).

% The collective outcome of reduced disease prevalence, lower transmission, and herd immunity thresholds. Not an agent, but the vindicated outcome the mandate's enforcer claims justifies overriding individual refusal. This is the abstract good that coercion is framed as serving.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, population_disease_prevention, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(legitimate_health_intervention__bodily_autonomy_primary, population_disease_prevention).

% Those who consent to or voluntarily adopt the mandated intervention. They receive reduced disease risk and social permission to participate in public life without restriction. From their position, the mandate protects them and solves the collective-action problem of free-riding refusers.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_compliant_citizens, beneficiary,
    organized, biographical, mobile, national).

% Physicians and practitioners who hold informed-consent-based clinical judgment that differs from the state mandate (recommending individual risk-benefit analysis rather than universal prescription). They are structurally excluded from the mandate-setting process and face licensing threat if they advise refusal or provide exemptions outside narrow criteria. Their professional judgment is subordinated to state epidemiological claims.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, independent_medical_practitioners, excluded,
    moderate, biographical, constrained, national).

% Judicial bodies tasked with reviewing whether mandates respect constitutional protections of bodily integrity and medical autonomy. They take testimony from all seats, commission expert analysis, and adjudicate whether the mandate is a legitimate public health measure or an unconstitutional seizure of bodily disposition. Their rulings can void or modify the constraint.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__bodily_autonomy_primary, public_health_administration).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem in epidemiology: without state enforcement, voluntary uptake of medical interventions (especially in early pandemic conditions or for chronic disease prevention) falls below the threshold required for herd immunity or disease suppression. The mandate coordinates behavior toward a shared disease-prevention goal by removing the free-rider option and spreading the intervention burden equally.
% TRANSFER_FUNCTION: Moves bodily disposition and medical decision-making authority from individuals to the state health apparatus. The state collects the power to determine who must undergo what medical procedure; individuals lose the capacity to refuse on grounds of autonomy, conscience, or personal risk calculus. The transfer is asymmetric: the state gains enforcement authority while individuals lose veto rights.
% ABSENT_VOICES: Independent medical practitioners whose clinical judgment diverges from state epidemiology (recommending individual risk-benefit evaluation) are structurally excluded from mandate-setting. Patients whose medical conditions create legitimate contraindications but do not fit bureaucratic exemption criteria are kept out of the conversation. Communities with historical trauma from coerced medical experimentation (Black Americans, Indigenous populations, incarcerated persons) are largely unheard in mandate justifications.
% DISAPPEARANCE_RATIONALE: If the mandate and its enforcement infrastructure vanished overnight, individuals would resume decision-making over their own medical interventions. Uptake rates would likely fall in some populations; disease prevalence would shift; the state would lose direct control over bodily compliance. Public health outcomes would reorganize around voluntary mechanisms and targeted protection of the vulnerable rather than universal coercion.
% FOUNDING_PROBLEM: Communicable disease threatens population health; voluntary medical intervention uptake is insufficient to prevent epidemiological harm, particularly in early-stage outbreaks or when disease suppression requires very high coverage thresholds (e.g., >90% vaccination for certain pathogens). Individual refusal imposes externalities on the vulnerable.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and epidemiologists attest the founding problem is live and justifies coercion. Constitutional scholars, bioethicists, and medical practitioners outside public health administration attest that the problem is overstated—that voluntary uptake combined with targeted protection of vulnerable groups solves the epidemiological problem without coercion. Comparative jurisdiction evidence (countries that relied on voluntary adoption, incentive structures, or targeted rather than universal mandates) and post-mandate analyses challenging necessity claims provide external corroboration for the contested status.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval as the enforcement apparatus hardens and compliance mechanisms deepen (career consequences, school exclusions, legal penalties escalate). The extraction is the state's seizure of bodily decision-making authority, backed by legal force. Suppression is high (0.52 initially, rising to 0.72 by time 4 and plateauing) because alternatives to compliance are structurally closed: refusal brings escalating penalties; exit from the jurisdiction is economically infeasible for most; claiming medical exemption requires documentation frameworks the state controls. Theater rises early (0.22 to 0.31 by time 4) as public health messaging emphasizes 'community responsibility' and 'we're all in this together' framing, then stabilizes (0.41) as the performative layer is maintained but the enforcement function becomes routine. The asymmetry is clear: the state benefits from centralized bodily control (coordination dividend plus political legitimacy); mandate-refused individuals bear the cost (bodily autonomy loss, legal jeopardy, occupational exclusion). This is tangled rope, not pure rope: a genuine coordination problem (disease suppression) solved alongside asymmetric extraction (bodily seizure) via active enforcement (legal penalties, employment gatekeeping, school exclusions). Suppression is structural: alternatives are not merely harder, they are legally and economically foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   The state health authority and the mandate-compliant citizen see genuine coordination: the mandate solves the free-rider problem in disease prevention, protects the vulnerable, and distributes the intervention burden fairly across the population. From their seats, d is low (beneficiary position), χ is low or negative (subsidy framing). The mandate-refused individual and occupationally-coerced worker see extraction: the state is seizing bodily disposition without consent, using legal force and employment gatekeeping to override medical judgment, and framing coercion as public health. From their seats, d is high (target position), χ is high (extraction). The constitutional court observer sees a structural conflict: two readings of 'legitimacy' are incommensurable—one grounds legitimacy in consent, the other in collective welfare. The engine computes each seat's type from the structural data (power, exit, beneficiary/victim status); the divergence it produces is not error but diagnosis of a genuinely contested constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Mandate-refused individuals: powerless, trapped exit (leaving the jurisdiction is economically infeasible; refusing the intervention brings escalating penalties). They are named in base_properties.victims. Directionality (d) is high—near 1.0 (full target). Occupationally-coerced workers: moderate power, constrained exit (can theoretically exit their profession, but at enormous career cost). Also victims. Their d is high (~0.75–0.85). The state health administration: institutional power, analytical exit (can revise or lift mandates, but internal incentives lock them into enforcement). They are beneficiaries (collect authority, legitimacy, disease suppression as political success). Their d is low (~0.1–0.2). Mandate-compliant citizens: organized power, mobile exit (can vote, can protest, can relocate). Beneficiary role (enjoy disease reduction, social coordination). Their d is symmetric to slightly beneficiary (~0.35–0.45). Independent practitioners: moderate power, constrained exit (licensing revocation is the threat). Excluded from agenda-setting; would oppose if heard. Their d is moderate-target (~0.60–0.70). No directionality override is needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is epidemiologically live (disease transmission is a real collective-action problem, especially for novel pathogens or high-transmission diseases). However, the mandate's persistence increasingly depends on institutional inertia and legitimacy framing rather than on ongoing necessity. By time 12–20 in the interval, extractiveness plateaus while suppression remains high—a signature of mandatrophy (the original function is sustained but the enforcement has become routine and the founding problem has receded). The theater ratio rising early (0.22→0.41) suggests that public health justification (the legitimacy story) is doing more work than actual disease dynamics would require. The reading's claim that 'state coercion violates bodily integrity regardless of public benefit' is exactly the lens that detects mandatrophy: even if the public health benefit is real and substantial, the means (bodily seizure without consent) are asserted as categorically impermissible. This reading cannot resolve the tension between benefit and coercion in favor of coercion; therefore, any sustained mandate becomes mandatrophic from this reading's perspective—the founding problem's solution is the problem itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_collective_harm,
    'Can individual informed consent be overridden by demonstrable collective harm (e.g., transmission of a highly lethal pathogen that would kill 30% of the unvaccinated), or is bodily autonomy categorically inviolable regardless of harm scale?',
    'Philosophical reasoning about rights tradability and harm thresholds; comparative case law across jurisdictions with different constitutional baselines (US, Europe, etc.); empirical outcomes comparing jurisdictions that mandated vs. relied on voluntary uptake with incentives.',
    'If consent can be overridden above some harm threshold, the reading transitions from categorical bodily autonomy to proportionality reading. If autonomy is inviolable, the constraint is snare (pure extraction via coercion) not tangled rope. If harm threshold exists but is context-dependent, the reading is contested internally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_vs_collective_harm, conceptual, 'Whether bodily autonomy is absolute or calibrated to threat level.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.72) structural (legal penalties, employment gatekeeping, school exclusion—external barriers) or partly internalized (compliance achieved through shame, socialization, internalized obligation to the collective)?',
    'Post-mandate trajectory: if suppression persists after enforcement mechanisms are formally removed (e.g., penalties lifted but mandate remains nominally in force), the suppression is internalized. Survey data on compliance drivers (fear of penalty vs. felt obligation vs. social pressure).',
    'If suppression is internalized, the constraint''s effective grip is higher than structural barriers suggest—the target carries suppression after exit. If structural only, removing enforcement mechanisms would dissolve compliance quickly. Internalized suppression indicates deeper identity fusion and suggests piton trajectory (maintenance through shame and social normalization rather than active legal enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    necessity_erosion,
    'As the epidemiological threat recedes (lower disease prevalence, variant mildness, vaccine effectiveness declines but disease severity does not rise), does the mandate ease or persist at prior enforcement intensity?',
    'Temporal measurement of mandate lift-off timing relative to objective epidemiological metrics (case rates, hospitalization, mortality). Comparative jurisdictions: do mandates ease faster where threat objectively subsides? Analysis of policy statements justifying continued enforcement after threat reduction.',
    'If mandates persist despite threat reduction, the constraint transitions from justified coordination to pure extraction (snare) and the founding problem is dead (mandatrophy confirmed). If mandates ease with threat, the constraint is proportional and the founding problem remains live. Persistent mandates after threat reduction are the clearest signal that the constraint has been captured for extraction purposes unrelated to epidemiology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_erosion, empirical, 'Whether mandate persistence tracks epidemiological necessity or institutional inertia.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the bodily autonomy reading and the public health primary reading logically foreclosed of each other, or do they coexist as different parties'' live positions?',
    'Examine judicial and legislative record: do courts/legislatures treat the readings as contradictory (one must be chosen, the other rejected) or as different-but-both-valid framings held by different political coalitions? Historical trajectory: have either readings been formally overridden or abandoned?',
    'If foreclosed: one reading''s core premise directly contradicts the other; they cannot coexist in any single framework. (Example: personhood-at-conception and birth-only readings foreclose each other.) If coexisting: both remain live options, held by different parties, neither ruling out the other. The kernel itself is permanently contested. Incommensurability is a structural property of the kernel—not a sign of poor analysis, but a sign that the commitment is genuinely unresolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether sibling readings of the kernel foreclose or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(legi_tr_t2, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 2, 0.26).
narrative_ontology:measurement(legi_tr_t4, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 4, 0.31).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 8, 0.39).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.41).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 16, 0.41).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(legi_be_t2, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 2, 0.54).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(legi_su_t2, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 2, 0.61).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 4, 0.67).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 8, 0.72).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, occupational_licensing_gatekeeping).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, medical_exemption_bureaucracy).

% DUAL FORMULATION NOTE:
% The kernel 'legitimate_health_intervention' decomposes into three structurally distinct constraints, one per reading: bodily_autonomy_primary (this story), public_health_primary, and proportionality_reading. Each reading instantiates a different ε value and different victim set because the referent (what 'legitimacy' means for a mandate) is reading-dependent. Bodily autonomy reading judges legitimacy by consent; public health reading judges by health outcomes; proportionality reading judges by weighing both. These are not the same constraint from different angles—they are different constraints unified by a common kernel. The family is linked via network.affects_constraints; each sibling documents its relationship to the others in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
