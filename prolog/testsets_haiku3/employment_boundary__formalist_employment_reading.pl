% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__formalist_employment_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary: Platform Workers as Independent Contractors
 *   domain: labor/economic/social_policy
 *
 * SUMMARY:
 *   Platform labor markets operate on a contractual classification that
 *   defines workers as independent contractors despite algorithmic control,
 *   work-assignment dependence, and lack of exit alternatives. This
 *   constraint is the FORMALIST EMPLOYMENT READING of a contested kernel:
 *   employment is defined by formal contract + direct supervision, not by
 *   economic dependence or control. Under this reading, platform workers are
 *   structurally outside the employment relationship, and platforms bear no
 *   employment-cost obligations. The reading is challenged by substantive and
 *   hybrid readings that locate employment status in economic realities
 *   rather than contractual form. This story instantiates the formalist
 *   reading alone—not the contest, not the averaging—and author ε as the
 *   formalist reading measures the standing arrangement (contractor
 *   classification) under its own lights: high extraction via cost
 *   externalization to workers and state systems.
 *
 * KEY AGENTS:
 *   - platform_operators: institutional power, arbitrage exit, sets the contractor classification and enforces it via deactivation rules
 *   - platform_workers: powerless, identity-locked exit, perform revenue-generating work, bear employment costs and income volatility
 *   - labor_enforcement_agencies: organized power, constrained exit, tasked with labor protection but lack jurisdiction over contractors
 *   - state_insurance_systems: institutional power, constrained exit, absorb safety-net costs shifted from platforms
 *   - traditional_employers: powerful beneficiary, mobile exit, benefit from competitive pressure
 *   - labor_advocates: moderate power, excluded from platform decision-making
 *   - regulatory_authorities: institutional analytical seat, investigating classification legitimacy across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.79).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.72).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, snare).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary: Platform Workers as Independent Contractors").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor/economic/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, '13021598-a271-4a10-8a11-df9460bb7d24').
narrative_ontology:cs_kernel_codification('13021598-a271-4a10-8a11-df9460bb7d24', fixed_text).
narrative_ontology:cs_authority_grounding('13021598-a271-4a10-8a11-df9460bb7d24', extraction).
narrative_ontology:cs_interpretation_layer_present('13021598-a271-4a10-8a11-df9460bb7d24').
narrative_ontology:cs_reading_relation('13021598-a271-4a10-8a11-df9460bb7d24', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('13021598-a271-4a10-8a11-df9460bb7d24', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('13021598-a271-4a10-8a11-df9460bb7d24', foundational, employment_status_determined_by_contractual_form).
narrative_ontology:cs_axiom_status(employment_status_determined_by_contractual_form, holdable).
narrative_ontology:cs_axiom_grounding('13021598-a271-4a10-8a11-df9460bb7d24', employment_status_determined_by_contractual_form, conventional).
narrative_ontology:cs_axiom('13021598-a271-4a10-8a11-df9460bb7d24', foundational, contractor_status_precludes_employment_obligations).
narrative_ontology:cs_axiom_status(contractor_status_precludes_employment_obligations, holdable).
narrative_ontology:cs_axiom_grounding('13021598-a271-4a10-8a11-df9460bb7d24', contractor_status_precludes_employment_obligations, instrumental).
narrative_ontology:cs_reference_frame('13021598-a271-4a10-8a11-df9460bb7d24', classical_employment_law_categories).
narrative_ontology:cs_drift_state('13021598-a271-4a10-8a11-df9460bb7d24', contemporary_platform_dominance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('13021598-a271-4a10-8a11-df9460bb7d24', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, labor_enforcement_agencies).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_insurance_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, traditional_employers).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, contractual_freedom_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, labor_market_flexibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate labor-matching platforms (rideshare, delivery, task platforms) and define workers contractually as independent contractors. Control dispatch algorithms, pricing, deactivation, and performance standards while claiming workers choose when/whether to work. Justify the classification as preserving worker flexibility and reducing platform liability. Benefit from outsourced employment costs (no health insurance, payroll tax, unemployment insurance, workers' compensation).
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Perform core platform revenue-generating work (driving, delivery, task completion) under algorithmic management—algorithms dispatch work, set piece rates, and can deactivate workers without recourse. No access to unemployment insurance, health insurance, or workers' compensation, and carry the full cost of maintaining their own tools and vehicle insurance. Formally classified as independent contractors despite economic dependence on the platform. Identity becomes fused with platform participation (gig platform work becomes primary livelihood, professional identity).
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, biographical, identity_locked, global).

% Tasked with enforcing labor standards and protecting worker welfare but lack jurisdiction over independent contractors. The formalist boundary shifts costs onto public enforcement—agencies cannot compel platform benefits, cannot investigate wage theft via algorithmic deductions, cannot set minimum standards for gig workers. Constrained by jurisdictional rules that defer to the contractual form the platforms establish.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_enforcement_agencies, payer,
    organized, generational, constrained, national).

% Bear the burden of providing safety-net coverage when platform workers cannot afford private insurance: unemployment assistance, Medicaid for uninsured workers and their families, subsidized healthcare. The contractor classification externalizes benefit costs from platforms to public systems, forcing state insurance mechanisms to absorb the difference between gig work income volatility and livelihood security.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_insurance_systems, payer,
    institutional, generational, constrained, national).

% Benefit indirectly from the contractor boundary by facing competitive pressure from platforms that avoid employment costs. Can cite platform cost structure as justification for their own labor-cost-minimization strategies, outsourcing, and temp-worker conversion, narrowing the competitive disadvantage of non-employment models.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers, beneficiary,
    powerful, generational, mobile, national).

% Argue that formalist classification misses economic dependence and algorithmic control, and that workers deserve employment protections. Excluded from the platform's own decision-making about worker classification, barred from collective bargaining on platform terms, and face resource constraints litigating worker status in each jurisdiction separately.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_advocates, excluded,
    moderate, biographical, constrained, national).

% Investigate whether formalist contractor classification is a genuine independent relationship or a device to evade employment law. Conduct rulemaking (some jurisdictions have passed substantive-employment statutes), issue guidance, and in some cases impose remedies (worker classification as employees, mandatory benefits). Different regulatory seats across jurisdictions (US, EU, UK) adopt different readings of the kernel.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Platforms solve a matching coordination problem: workers seeking flexible tasks connect with customers needing services, algorithmically mediated. The matching reduces search costs and enables workers to accept/reject individual tasks. However, the coordination function could operate with workers classified as employees—Germany's Deliveroo model and UK/EU case law show that 'flexibility' and 'employment' are not mutually exclusive.
% TRANSFER_FUNCTION: Moves employment costs (health insurance, payroll taxes, unemployment insurance, workers' compensation) from platform operators to workers and state insurance systems. Moves labor-law compliance costs from platforms to enforcement agencies that cannot reach independent contractors. Concentrates pricing power in the platform (piece rates are set unilaterally, not negotiated).
% ABSENT_VOICES: Platform workers who have been deactivated or algorithmically suppressed are outside the conversation; worker organizing efforts are kept off platform-controlled communication channels; comparative international standards (EU directives on platform worker rights, UK employment classification case law) are excluded from US domestic policy making by jurisdiction barriers.
% DISAPPEARANCE_RATIONALE: If the contractor classification disappeared overnight and workers were reclassified as employees, platforms would need to absorb employment costs, adjust piece rates downward or reduce available work, provide unemployment insurance, workers' comp, and health benefits. Worker earnings stability would increase, state safety-net costs would decrease, and labor enforcement agencies would gain jurisdiction. The labor market structure of platforms would reorganize substantially.
% FOUNDING_PROBLEM: Early gig platforms needed flexibility: matching workers who wanted task-based income (supplemental, occasional) with customers needing immediate services. Classifying workers as employees appeared to conflict with task-based, on-demand work structure and would impose costs that made the business model unviable at launch.
% FOUNDING_PROBLEM_CORROBORATION: Platforms assert the founding problem is still live: workers demand flexibility and low friction entry. Labor advocates and regulatory authorities attest the founding problem was real at launch but has been replaced by a different economic reality—platforms have become primary income sources for millions of workers, algorithmic control has increased, and flexibility is asymmetric (platforms control availability, not workers). Legislative testimony from workers and independent economic analysis by academics outside the benefiting parties support the 'problem solved, extraction persists' reading.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__formalist_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__formalist_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79 at interval end) because the formalist reading externalizes all employment costs to workers and state systems while concentrating control (pricing, dispatch, deactivation) in the platform. Suppression is substantial (0.72) because the constraint's persistence depends on active exclusion: deactivation systems suppress worker organizing, contractual terms suppress wage negotiation, legal strategies suppress regulatory reclassification attempts. Theater is moderate-high (0.48) because platforms emphasize worker autonomy and flexibility in marketing while algorithmic systems constrain actual autonomy; the constraint's legitimacy story diverges from its operational reality. The measurement series shows rising extractiveness and suppression over the 20-year interval as platforms scaled globally and algorithmic control intensified—the coordination problem (matching) was solved early, but extraction mechanisms matured and hardened. Theater rises more slowly because the flexibility narrative remains effective despite growing operational control.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (platform) and payer (worker) seats should compute very different constraint types. From the platform's seat the arrangement is flexible, low-friction coordination—the platform controls rules but offers task choice. From the worker's seat the arrangement is algorithmic control with constrained exit (identity locked: gig work becomes livelihood, switching platforms means restarting reputation/earnings). The engine computes this divergence from structural data—power asymmetry, exit options, and directionality. Labor enforcement sits between: the constraint protects platforms from enforcement but constrains agency capacity. The formalist reading defines this asymmetry as legitimate (workers chose the form, got flexibility); substantive readings locate it as false (economic dependence overrides choice).
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are near-maximum beneficiary directionality (d ≈ 0.05): the constraint subsidizes their operation by externalizing costs, they control rule-setting, and exit is arbitrage (they can change classification rules and workers must adapt or leave). Platform workers are near-maximum target directionality (d ≈ 0.95): economic dependence (platform is their livelihood), algorithmic control (they cannot negotiate terms), identity lock (gig work identity is primary), and the constraint ensures they bear all employment risk. Labor agencies and state insurance are payers (d ≈ 0.75): they bear costs they did not authorize (workers turn to public insurance when platform provides none) and have constrained exit (they cannot refuse to provide safety-net coverage). No directionality override needed—the derivation from beneficiary/victim + power + exit captures the structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for flexible task matching at launch) has decomposed. The coordination function (matching) persists and is real. The extraction function (cost externalization) has grown and hardened—platforms reinvest coordination savings into control infrastructure, not into lower prices or better worker terms. The formalist reading prevents mandatrophy detection by defining away the problem: 'workers chose this, so it is not extraction, it is trade.' This is exactly where mandatrophy hides—a constraint whose original justification has dissolved but whose beneficial framing persists through contractual form. The theater ratio rising from 0.32 to 0.48 indicates increasing gap between the flexibility narrative and algorithmic control reality. A substantive reading would call this a zombie constraint (problem solved, extraction persists); the formalist reading forestalls that diagnosis by denying the extraction itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalist_vs_substantive_reading,
    'Is employment status a matter of contractual form (formalist reading) or economic dependence + control (substantive reading)? Can both readings coexist in one legal framework, or does one logically foreclose the other?',
    'Cross-jurisdictional case law evolution: EU directives have adopted substantive reading; UK courts have created a middle category; US remains formalist. If a single jurisdiction adopts both readings in sequence (legislative reversal), that would show coexistence is possible but unstable; if the readings remain jurisdictionally partitioned indefinitely, coexistence is the stable state.',
    'If substantive reading prevails, platform workers are reclassified as employees, extraction measures flip (workers gain access to employment protections, platforms absorb costs), and the constraint computes as tangled_rope or piton depending on whether coordination value persists. If formalist reading remains dominant, extraction persists as described.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalist_vs_substantive_reading, conceptual, 'Whether employment is defined by form or substance—a foundational jurisdictional choice about the kernel.').

omega_variable(
    identity_lock_mechanism_internalized_vs_structural,
    'Is the identity lock of platform workers structural (platforms control access, reputation, algorithms exclude exit) or internalized (workers have internalized the gig-work identity, making exit psychologically costly even where technically possible)?',
    'Post-deactivation trajectory: if deactivated workers rapidly find equivalent work on rival platforms or traditional employment, the lock is primarily structural (barrier removal enables exit); if deactivation leads to prolonged income loss and identity crisis, the lock is partially internalized (the worker has fused identity with platform participation).',
    'If structural, the constraint''s suppression is an artifact of platform control mechanisms; if internalized, workers carry the suppression after exit, requiring longer remediation. Internalized lock suggests deeper extraction than structural lock alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalized_vs_structural, empirical, 'Whether the constraint''s exit barrier is a platform-imposed structure or a worker-internalized identity fusion.').

omega_variable(
    cost_externalization_authorization,
    'Did platform workers genuinely ''choose'' contractor status understanding the cost externalization (no health insurance, no unemployment), or was the choice structured through default framing and asymmetric information?',
    'Survey evidence on worker understanding of contractor costs at entry; comparative evidence from workers who later discover the cost gap; legislative testimony from workers about decision context.',
    'If informed choice, the formalist reading''s framing (workers chose flexibility) is structurally sound. If choice was structured/uninformed, the contractor classification is a device for concealment rather than genuine consent, and extraction measures should be recalibrated upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_externalization_authorization, empirical, 'Whether cost externalization was a known and chosen trade-off or an obscured consequence of contractor classification.').

omega_variable(
    kernel_reading_commitment_system,
    'Is the formalist employment reading grounded in a commitment system (labor law tradition, precedent, doctrinal development), or is it a contemporary invention by platforms to escape prior labor-law commitments?',
    'Historical analysis: did labor law before platforms recognize the contractor-as-employed distinction? If the formalist reading predates platforms and was applied to traditional independent work (sales reps, consultants), the reading has lineage. If it was retrofitted to platforms and conflicts with prior labor-law doctrine, it is a reading-capture device, not a coherent interpretation.',
    'If the reading has lineage within labor-law tradition, its legitimacy is higher. If retrofitted, the reading''s authority grounding is weaker (extraction rather than expertise/lineage), and its vulnerability to reversal is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_system, conceptual, 'Whether the formalist reading is a traditional interpretation or a contemporary device for classification escape.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(empl_tr_t0, observed).
narrative_ontology:measurement(empl_tr_t3, employment_boundary__formalist_employment_reading, theater_ratio, 3, 0.37).
narrative_ontology:measurement_basis(empl_tr_t3, observed).
narrative_ontology:measurement(empl_tr_t6, employment_boundary__formalist_employment_reading, theater_ratio, 6, 0.41).
narrative_ontology:measurement_basis(empl_tr_t6, observed).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__formalist_employment_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(empl_tr_t10, observed).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__formalist_employment_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement_basis(empl_tr_t15, observed).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__formalist_employment_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(empl_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(empl_be_t0, observed).
narrative_ontology:measurement(empl_be_t3, employment_boundary__formalist_employment_reading, base_extractiveness, 3, 0.68).
narrative_ontology:measurement_basis(empl_be_t3, observed).
narrative_ontology:measurement(empl_be_t6, employment_boundary__formalist_employment_reading, base_extractiveness, 6, 0.72).
narrative_ontology:measurement_basis(empl_be_t6, observed).
narrative_ontology:measurement(empl_be_t10, employment_boundary__formalist_employment_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(empl_be_t10, observed).
narrative_ontology:measurement(empl_be_t15, employment_boundary__formalist_employment_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement_basis(empl_be_t15, observed).
narrative_ontology:measurement(empl_be_t20, employment_boundary__formalist_employment_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(empl_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(empl_su_t0, observed).
narrative_ontology:measurement(empl_su_t3, employment_boundary__formalist_employment_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement_basis(empl_su_t3, observed).
narrative_ontology:measurement(empl_su_t6, employment_boundary__formalist_employment_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement_basis(empl_su_t6, observed).
narrative_ontology:measurement(empl_su_t10, employment_boundary__formalist_employment_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(empl_su_t10, observed).
narrative_ontology:measurement(empl_su_t15, employment_boundary__formalist_employment_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(empl_su_t15, observed).
narrative_ontology:measurement(empl_su_t20, employment_boundary__formalist_employment_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(empl_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__formalist_employment_reading, 0.18).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__hybrid_security_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, algorithmic_control__worker_autonomy).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, state_insurance__cost_externalization).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel has three instantiated readings: formalist (this story), substantive, and hybrid. Each reading produces a different constraint with different ε, beneficiary/victim structures, and computed types. The readings coexist as different jurisdictions adopt different interpretations of the same legal and economic question. This story instantiates ONLY the formalist reading—the constraint here is what formalist interpretation describes, not an average or median across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
