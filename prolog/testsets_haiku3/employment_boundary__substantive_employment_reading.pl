% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Definition: Economic Dependence and Algorithmic Control
 *   domain: labor_economics/social_policy
 *
 * SUMMARY:
 *   This constraint embodies the substantive employment reading of a
 *   contested kernel: employment is defined by economic dependence and
 *   algorithmic control, not by formal contract. Under this reading, platform
 *   workers are employees and platforms bear corresponding obligations
 *   (social insurance, job security, minimum wage compliance). Platforms
 *   resist reclassification through legal argumentation, contract design, and
 *   political lobbying. The measurement series tracks rising suppression
 *   (platforms must actively defend contractor classification against
 *   regulatory and legal challenges) and rising theater ratio
 *   (public-relations narratives about flexibility and independence displace
 *   actual commitment to worker autonomy). The constraint claim
 *   (tangled_rope) reflects the simultaneous genuine coordination function
 *   (task matching) and asymmetric extraction (workers depend, platforms
 *   extract). This is ONE READING of the employment_boundary kernel; other
 *   readings (formalist, hybrid) produce different ε values and different
 *   victim/beneficiary structures — they are separate constraint stories
 *   linked by network.affects_constraints.
 *
 * KEY AGENTS:
 *   - platform_operators: institutional beneficiary, sets contract terms and algorithmic rules unilaterally
 *   - platform_workers: powerless victims, dependent on platform access, subject to algorithmic management, identity-locked
 *   - traditional_employers: organized beneficiaries, benefit from labor-cost arbitrage created by platform contractor status
 *   - labor_regulators: institutional observers, administer employment law but enforcement depends on adoption of substantive reading
 *   - worker_advocates: excluded, would argue for reclassification but lack voice in platform governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.68).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.72).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Definition: Economic Dependence and Algorithmic Control").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, 'fbf97845-9f26-4332-9e5e-0957316db31c').
narrative_ontology:cs_kernel_codification('fbf97845-9f26-4332-9e5e-0957316db31c', fixed_text).
narrative_ontology:cs_authority_grounding('fbf97845-9f26-4332-9e5e-0957316db31c', extraction).
narrative_ontology:cs_interpretation_layer_present('fbf97845-9f26-4332-9e5e-0957316db31c').
narrative_ontology:cs_reading_relation('fbf97845-9f26-4332-9e5e-0957316db31c', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbf97845-9f26-4332-9e5e-0957316db31c', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('fbf97845-9f26-4332-9e5e-0957316db31c', foundational, economic_dependence_triggers_employment).
narrative_ontology:cs_axiom_status(economic_dependence_triggers_employment, holdable).
narrative_ontology:cs_axiom_grounding('fbf97845-9f26-4332-9e5e-0957316db31c', economic_dependence_triggers_employment, deontological).
narrative_ontology:cs_axiom('fbf97845-9f26-4332-9e5e-0957316db31c', foundational, algorithmic_control_constitutes_supervision).
narrative_ontology:cs_axiom_status(algorithmic_control_constitutes_supervision, holdable).
narrative_ontology:cs_axiom_grounding('fbf97845-9f26-4332-9e5e-0957316db31c', algorithmic_control_constitutes_supervision, empirically_contingent).
narrative_ontology:cs_reference_frame('fbf97845-9f26-4332-9e5e-0957316db31c', employment_law_protective_intent).
narrative_ontology:cs_drift_state('fbf97845-9f26-4332-9e5e-0957316db31c', contemporary_gig_economy_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fbf97845-9f26-4332-9e5e-0957316db31c', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_operators).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, traditional_employers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, consumers_of_platform_services).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, consumers_of_platform_services).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, gig_sector_non_platform_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the algorithmic systems that assign work, set task compensation, evaluate performance, and terminate access. Frame workers as independent contractors to avoid employment obligations (payroll taxes, benefits, job security, minimum wage compliance). Reclassification as employers would require restructuring compensation, extending health insurance, providing unemployment insurance, and accepting wrongful-termination liability.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Depend on platform access for primary or substantial income with no meaningful negotiation over terms. Subject to algorithmic management (task assignment, performance rating, wage deduction for customer complaints, deactivation without due process). Exit is theoretically available but carries high costs: loss of income stream, loss of reputation/rating, exclusion from a primary employment channel in their gig sector. Many have professionalized around platform work and identify as platform workers.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, payer,
    powerless, biographical, identity_locked, global).

% Benefit from a labor-cost arbitrage: platform workers undercut formally employed workers on price because they bear their own payroll taxes, benefits, and insurance. Reclassification of platform workers as employees would compress the wage differential and raise the cost baseline for competing employers.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, traditional_employers, beneficiary,
    organized, generational, mobile, national).

% Administer employment law and social insurance systems. Substantive employment readings require them to enforce reclassification and mandate benefits; formalist readings permit them to treat platform workers as outside their jurisdiction. They see fiscal impact (reduced payroll tax evasion vs. compliance costs) and labor-market effect (whether gig work is genuine self-employment or disguised dependency).
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% Would argue that economic dependence + algorithmic control = employment, that platforms deliberately structure arrangements to evade obligations, and that reclassification is a correction not a burden. They are excluded from rate-setting and contract negotiations; their voice enters only through regulatory and legal channels.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_worker_advocates, excluded,
    moderate, biographical, constrained, national).

% Benefit from low platform-service costs enabled partly by labor-cost arbitrage (workers bear own benefits). Reclassification and mandatory benefits would raise prices. They pay indirectly through price inflation if workers are reclassified; they benefit from low prices under the current arrangement.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, consumers_of_platform_services, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, consumers_of_platform_services, payer).

% Independent contractors (cleaners, handypersons, task-based service providers) not algorithmically managed by a single platform. Reclassification of platform workers as employees would raise their relative labor costs and competitive pressure; they pay through compressed wages and reduced work opportunities as platforms absorb reclassification costs.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, gig_sector_non_platform_workers, payer,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__substantive_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches workers to tasks at scale, managing quality and accountability across millions of interactions. The algorithmic system solves information asymmetry (workers find tasks, requesters find reliable workers) and establishes reputation mechanisms that substitute for traditional employer-employee trust relationships.
% TRANSFER_FUNCTION: Moves income from platform operators to workers in the form of per-task compensation, while retaining employer-normal controls (algorithmic task assignment, performance evaluation, wage deduction, unilateral termination). The operator captures the coordination markup and avoids the cost of employment obligations by classifying workers as contractors.
% ABSENT_VOICES: Platform workers themselves have limited voice in the reading's establishment — they cannot negotiate contract terms or algorithmic parameters. Worker advocates, labor organizers, and regulators who see substantive dependency would argue for reclassification but are structurally excluded from the platforms' policy-setting. Their view enters through legislative and legal channels, not through the platforms' own governance.
% DISAPPEARANCE_RATIONALE: If the substantive employment reading were enforced and platforms reclassified all workers as employees, the gig economy would reorganize around different labor models: reduced per-task availability (tasks bundled into shifts), higher compensation to cover employer taxes and benefits, reduced workforce size, or shift toward hybrid models mixing employed and contractor tiers. The current work availability and pricing depend on the contractor classification.
% FOUNDING_PROBLEM: Decentralized task matching at scale required an intermediary that could evaluate workers, aggregate tasks, and manage transaction costs without the overhead of traditional employment. Platforms emerged to solve the matching problem and reduce friction in on-demand service markets.
% FOUNDING_PROBLEM_CORROBORATION: Platforms and economists studying matching markets attest the problem remains live: on-demand matching is genuinely hard and platforms provide real value. Labor advocates and worker researchers attest the problem is substantially solved (markets adapted) and the arrangement now persists primarily as a cost-avoidance mechanism. Regulatory bodies in multiple jurisdictions have begun treating substantive dependency as decisive, suggesting the founding problem's resolution is contested across institutions.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the platforms' structural power to unilaterally set terms, deduct for customer complaints, assign work algorithmically, and terminate access without due process. Suppression (0.72) is high because the constraint's persistence depends on active defense: platforms must legally argue that algorithmic control does not constitute employment, must lobby regulators to adopt formalist readings, and must structure worker incentives (ratings, bonuses tied to acceptance) to discourage exit. Theater ratio (0.42) is moderate-high: platforms invest heavily in narratives of worker autonomy and flexibility (marketing theater) while the actual operation involves tight algorithmic control and wage compression. Accessibility collapse (0.64) reflects that workers can theoretically exit platform work, but many have built livelihoods and identity around it, and alternatives often pay less. Resistance (0.71) is substantial because worker advocates, labor unions, and regulatory bodies in multiple jurisdictions actively contest the contractor classification. The measurement trajectory shows all three metrics rising over the interval (extractiveness, theater, suppression all increasing), indicating platforms hardening enforcement as legal and regulatory pressure mounts.
 *
 * PERSPECTIVAL GAP:
 *   The platform operator seat sees tangled_rope as coordination with a payment mechanism (matching is the rope; commission/markup is legitimate). The worker seat sees snare (dependence locked in, no real exit, extraction masked as flexibility). The regulatory seat sees the gap itself as the problem — formalist law permits contractor status, but substantive employment readings reveal the status as cover for extraction. The engine computes per-seat classifications from these structural asymmetries. The authored claim (tangled_rope) reflects the substantive reading's frame: genuine coordination exists, but extractive asymmetry also exists and must be actively suppressed (legal defense, narrative work, algorithmic nudges toward acceptance).
 *
 * DIRECTIONALITY LOGIC:
 *   From the platform operator seat (institutional power, arbitrage exit, global scope), the constraint delivers benefit: control over labor terms without employment obligations. Directionality near 0.0 (full beneficiary). From the platform worker seat (powerless, identity_locked, constrained exit, biographical horizon), the constraint extracts: dependence without security, algorithmic control without voice, unilateral termination without recourse. Directionality near 1.0 (full target). From the traditional employer seat (organized, mobile exit, national scope), modest benefit accrues through labor-cost arbitrage. From consumer and gig-sector non-platform worker seats, modest cost from service price stability and wage compression respectively. The key asymmetry: platform workers cannot exit without material loss; platforms can shift risk structures if regulatory pressure mounts. Substantive employment readings lock directionality toward extraction by asserting economic dependence overrides formal contract.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (on-demand task matching at scale) was live when platforms emerged. At interval end, the problem is substantially solved: labor markets have adapted, gig work is normalized, and task-matching technology is commoditized. Yet the constraint persists because platforms benefit from contractor classification and actively defend it. Mandatrophy is incipient: the founding rationale (we need a novel matching solution) has been replaced by a cost-avoidance rationale (we need to avoid employment obligations). A tangled_rope classification with high suppression_requirement and rising theater_ratio suggests a constraint where the coordination function serves as cover for extraction that is no longer justified by the original problem-solution fit. Regulatory drift (multiple jurisdictions testing substantive employment definitions) and worker resistance (growing organizer activity, class-action litigation) indicate the constraint's mandatrophy is becoming visible to institutional observers. A snare reading would deny the coordination function entirely; the tangled_rope reading acknowledges it while identifying extraction-protection as the primary enforcement objective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_control_vs_flexibility,
    'Does algorithmic task assignment constitute employment-level control, or does worker flexibility to refuse tasks without penalty preserve independent contractor status?',
    'Comparative labor law analysis: jurisdictions that treat refusal penalty as a control marker (acceptance-rate penalties, deactivation for low acceptance, selective task access tied to metrics) will classify workers as employees; those treating refusal right as dispositive will classify as contractors. Case law and regulatory guidance will accumulate evidence for one direction.',
    'If flexibility-to-refuse is deemed decisive, workers exit the victim set and platforms'' extraction claim weakens (worker choice becomes meaningful). If algorithmic nudges toward acceptance count as control, workers remain victims and platforms'' suppression obligation rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_control_vs_flexibility, empirical, 'Whether algorithmic control coexists with genuine flexibility or negates it through penalty structures.').

omega_variable(
    economic_dependence_threshold,
    'What level of income dependence triggers employment status? Is a worker who derives 50% of income from one platform employed? 80%? Does income thresholds vary by jurisdiction?',
    'Legislative action establishing numerical thresholds (e.g., ''more than 50% of income'' = employee); regulatory guidance operationalizing dependence; court precedents treating income concentration as evidence of control.',
    'A high threshold (80%+) allows platforms to keep most workers as contractors; a low threshold (30%+) reclassifies most full-time platform workers. Threshold variance across jurisdictions creates arbitrage opportunities (platforms relocate operations or use multi-platform worker strategies to stay below thresholds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_dependence_threshold, empirical, 'The income-concentration level that triggers substantive employment status.').

omega_variable(
    structural_vs_internalized_dependence,
    'To what extent is platform worker dependence structural (limited gig-market alternatives, barriers to traditional employment) vs. internalized (workers have adopted platform identity, normalized precarity, internalized algorithmic evaluation)?',
    'Post-reclassification trajectory data: if worker suppression (accepting algorithmic management, tolerating low pay) persists after reclassification provides employment security, dependence was partly internalized; if suppression dissipates with structural change, dependence was primarily structural.',
    'If internalized, the constraint''s suppression is higher than metrics suggest — workers carry the suppression with them. If structural, regulatory intervention addressing contracts and benefits would reduce suppression markedly. Internalization affects the remedy design and timeline for worker transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_vs_internalized_dependence, empirical, 'Whether platform worker dependence is structural or internalized.').

omega_variable(
    reading_contention_scope,
    'Is the substantive employment reading universally applicable across all platform-work contexts (delivery, rideshare, task platforms, creative platforms), or is it sector-specific (heavy platforms like delivery differ structurally from light platforms like task marketplaces)?',
    'Regulatory and case law differentiation: some jurisdictions may apply substantive employment to delivery and rideshare (high algorithmic control, high income concentration) while permitting contractor status for task platforms (lower control, more worker choice). Sector-level evidence on algorithmic intensity and income dependency will inform differentiation.',
    'If universally applied, all gig workers are employees and platforms face uniform compliance. If sector-specific, platforms can restructure within lighter sectors to remain contractors; the constraint''s footprint shrinks. This affects the scope of victim set and platform extraction vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_scope, conceptual, 'Whether substantive employment readings apply uniformly or vary by gig-work sector.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the substantive employment reading logically foreclose the formalist reading within a single legal framework, or can both coexist as competing interpretations of the same employment law?',
    'Constitutional law and statutory interpretation doctrine: if the statute is deemed to have a single correct meaning (textualism, originalism), one reading forecloses; if multiple readings are legitimate (living constitutionalism, purposivism), both can coexist. The resolution depends on jurisdictional interpretation methodology, not on the empirical facts of platform work.',
    'If foreclosure occurs, the kernel will collapse into a single dominant reading (the victor determines victim set and enforcement). If coexistence persists, the kernel remains contested and multiple constraints (formalist and substantive) remain live simultaneously across different institutional seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether substantive and formalist readings can coexist within a single legal framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(empl_tr_t5, employment_boundary__substantive_employment_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__substantive_employment_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__substantive_employment_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__substantive_employment_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(empl_tr_t25, employment_boundary__substantive_employment_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(empl_be_t5, employment_boundary__substantive_employment_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(empl_be_t10, employment_boundary__substantive_employment_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(empl_be_t15, employment_boundary__substantive_employment_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(empl_be_t20, employment_boundary__substantive_employment_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(empl_be_t25, employment_boundary__substantive_employment_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(empl_su_t5, employment_boundary__substantive_employment_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(empl_su_t10, employment_boundary__substantive_employment_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(empl_su_t15, employment_boundary__substantive_employment_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(empl_su_t20, employment_boundary__substantive_employment_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(empl_su_t25, employment_boundary__substantive_employment_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__substantive_employment_reading, 0.18).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel comprises three structurally distinct constraint stories, each instantiating a different reading of statutory employment law. (1) substantive_employment_reading (this story): economic dependence + algorithmic control = employment, platforms are obligated employers, high extraction, victims = workers. (2) formalist_employment_reading: formal contract + direct supervision = employment, algorithmic systems do not constitute supervision, zero extraction, no victims. (3) hybrid_security_reading: third statutory category, custom protections, moderate extraction, workers are beneficiaries of protections but also bear costs. Each reading produces a different ε value (substantive = 0.68, formalist ≈ 0.05, hybrid ≈ 0.35) and different victim/beneficiary structures. The readings are not observational perspectives on a single constraint; they are alternative constraints grounded in alternative legal interpretations of the same kernel text. Network edges model legal and institutional influence: formalist prevalence suppresses reclassification and delays substantive implementation; hybrid adoption creates a middle path that potentially resolves the contest by statute. All three remain live positions across different jurisdictions as of interval end.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__substantive_employment_reading, powerless, 0.92).
constraint_indexing:directionality_override(employment_boundary__substantive_employment_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
