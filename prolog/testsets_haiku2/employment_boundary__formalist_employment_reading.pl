% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Formalist Employment Boundary: Contract + Supervision Doctrine
 *   domain: labor_economics/social_policy
 *
 * SUMMARY:
 *   The formalist employment boundary defines employment by two criteria: a
 *   written contract establishing contractor status and direct human
 *   supervision. Platform operators use this reading to classify workers as
 *   independent contractors, exempting themselves from payroll taxation,
 *   benefits provision, workers' compensation liability, and wage-and-hour
 *   protections. This is ONE READING of the contested employment_boundary
 *   kernel. The substantive_employment_reading argues employment should be
 *   defined by economic dependence and algorithmic control; the
 *   hybrid_security_reading argues for a third category with tailored
 *   protections. This story instantiates the formalist reading: it authors
 *   the standing arrangement under contest (the formalist classification as
 *   platforms hold and defend it) and assesses its extractiveness from the
 *   formalist reading's own lights, not from the competing readings' lights.
 *   The ε referent is fixed: the standing formal-contract-based arrangement.
 *   The reading-indexed value (0.81 extractiveness) is what the formalist
 *   reading's own metric assessment yields for that arrangement.
 *
 * KEY AGENTS:
 *   - platform_operators: institutional agenda-setters who define and enforce the boundary via contract language and litigation
 *   - platform_workers: powerless, identity-locked payers bearing full employment-insurance cost externalization
 *   - state_insurance_systems: organized payers absorbing social costs when workers cannot access private coverage
 *   - traditional_employers: institutional beneficiaries of precedent and wage-pressure effects
 *   - labor_advocates: excluded voices arguing substantive-employment readings
 *   - courts_and_regulators: observers whose jurisdictional interpretations determine constraint persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.81).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.72).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, snare).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary: Contract + Supervision Doctrine").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, '43856464-efeb-4584-8438-6fb2409344a7').
narrative_ontology:cs_kernel_codification('43856464-efeb-4584-8438-6fb2409344a7', fixed_text).
narrative_ontology:cs_authority_grounding('43856464-efeb-4584-8438-6fb2409344a7', extraction).
narrative_ontology:cs_interpretation_layer_present('43856464-efeb-4584-8438-6fb2409344a7').
narrative_ontology:cs_reading_relation('43856464-efeb-4584-8438-6fb2409344a7', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('43856464-efeb-4584-8438-6fb2409344a7', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('43856464-efeb-4584-8438-6fb2409344a7', foundational, formal_contract_defines_employment_status).
narrative_ontology:cs_axiom_status(formal_contract_defines_employment_status, holdable).
narrative_ontology:cs_axiom_grounding('43856464-efeb-4584-8438-6fb2409344a7', formal_contract_defines_employment_status, conventional).
narrative_ontology:cs_axiom('43856464-efeb-4584-8438-6fb2409344a7', foundational, direct_human_supervision_required_for_employment).
narrative_ontology:cs_axiom_status(direct_human_supervision_required_for_employment, overridden).
narrative_ontology:cs_axiom_grounding('43856464-efeb-4584-8438-6fb2409344a7', direct_human_supervision_required_for_employment, empirically_contingent).
narrative_ontology:cs_reference_frame('43856464-efeb-4584-8438-6fb2409344a7', formalist_employment_doctrine).
narrative_ontology:cs_drift_state('43856464-efeb-4584-8438-6fb2409344a7', contemporary_algorithmic_control_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('43856464-efeb-4584-8438-6fb2409344a7', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_insurance_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, traditional_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classifies workers as independent contractors via formal contract language and algorithmic rather than human supervision. This classification exempts them from payroll taxation, benefits provision, workers' compensation contributions, and overtime liability. The operators maintain that flexibility is what workers want, that they set their own hours, and that the absence of a human supervisor establishes the independence status. They actively defend this boundary through litigation and lobbying against substantive-employment readings.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Are classified as independent contractors, meaning they bear the full cost of employment insurance (self-employment tax, health insurance, disability, unemployment), receive no paid leave, have no job security or advance notice protections, and have no recourse to wage-and-hour law. They face algorithmic deactivation without due process, cannot collectively bargain, and have no grievance mechanisms. The 'flexibility' they ostensibly chose is often a condition of market access, not a choice. Many have fused their identity with platform participation and lack credible exit paths.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, biographical, identity_locked, global).

% Bear the social cost when platform workers cannot access private insurance. Emergency rooms treat uninsured injuries, public assistance programs provide income when workers are sick or injured, unemployment insurance is stretched by workers who should qualify but are classified out. The formalist boundary externalize these costs to the state, which must either absorb them or deny them to those in need.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_insurance_systems, payer,
    organized, generational, constrained, national).

% Benefit indirectly: the formalist boundary creates downward wage pressure and a precedent for reclassifying other precarious work (gig delivery, freelance creative, temp agencies) as contractor status. This reduces their own labor costs and regulatory burden, though it also destabilizes the workforce they depend on.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers, beneficiary,
    institutional, generational, constrained, national).

% Argue that the formalist definition ignores economic reality: algorithmic control is control, financial dependence is dependence, and the 'flexibility' framing masks coercion. They would testify that the boundary should be redrawn. They are excluded from the core framing that justifies the constraint.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_advocates, excluded,
    moderate, biographical, mobile, national).

% Adjudicate the boundary in litigation and regulatory proceedings. Some jurisdictions apply formalist tests; others apply substantive control tests. The constraint's persistence depends on regulatory capture or doctrinal alignment at the jurisdictional level, which is why the reading is contested and platform operators actively litigate to maintain it.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates labor supply matching at scale: workers express availability and location, platforms match them to tasks with minimal matching friction. The formalist boundary purports to enable this by removing employment-law overhead.
% TRANSFER_FUNCTION: Transfers the cost of employment insurance, safety provision, and income security from platforms to workers and the state. The workers pay self-employment tax and private insurance; the state subsidizes health and emergency care; platforms keep the margin.
% ABSENT_VOICES: Workers advocating substantive-employment readings are structurally excluded: classified as contractors before they can organize, algorithmic deactivation suppresses collective voice, and the legal framing pre-decides the dispute before labor advocates can testify. Labor economists and worker testimony supporting alternative readings are present in regulatory proceedings but are overridden by the formalist doctrine.
% DISAPPEARANCE_RATIONALE: If the formalist boundary were suddenly invalidated and platform workers reclassified as employees, platforms would face massive retroactive payroll tax and benefits liability, would need to restructure operational control to reduce algorithmic management (currently their competitive advantage), and would either shrink the workforce or raise prices. The entire competitive advantage of platform models depends on this boundary.
% FOUNDING_PROBLEM: Early gig platforms needed a labor-cost-minimization mechanism to undercut traditional services (taxis, delivery, moving). The formalist boundary was invented to provide legal cover for contractor classification despite operational control that looks like employment.
% FOUNDING_PROBLEM_CORROBORATION: Platform executives have testified that the 'founding problem' (low-cost entry) is solved; their business models are now mature and profitable. Worker advocacy organizations and labor economists attest that the founding rationale no longer applies but the boundary persists as extracted rent. Courts in substantive-reading jurisdictions (California, EU) have found the founding problem no longer justifies the exclusion.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81) because the formalist boundary is decoupled from the actual labor conditions: workers face algorithmic control, financial dependence, and no meaningful exit or collective voice, yet are classified outside employment law. The extraction is the cost externalization: workers pay full self-employment tax and insurance; the state absorbs uninsured costs; platforms capture the margin. Suppression is substantial (0.72) because the constraint's persistence depends on actively suppressing workers' collective voice (no unionization, algorithmic deactivation, misclassification as independent contractors), defending against substantive readings through litigation, and maintaining regulatory capture or judicial alignment in key jurisdictions. Theater is moderate (0.48): the 'flexibility' narrative is genuine marketing and some workers do value it, but a large proportion of workers are locked in by identity (platform participation is their only income option) or structural constraint (no alternative markets), making the autonomy claim partially theatrical. The measurement series shows extraction and suppression intensifying over the interval: as platforms mature and profitability rises, the extracted margin grows; as worker organizing attempts mount, suppression machinery thickens.
 *
 * PERSPECTIVAL GAP:
 *   From the platform-operator seat, this is a natural, market-based classification: workers chose flexibility, no deception occurred, and the arrangement should compute as rope (genuine coordination with low overhead). From the worker seat (especially identity-locked workers), the same structure is a snare: they cannot exit without abandoning their livelihood, control is real though algorithmic, and the 'choice' of flexibility is a condition of market access. From the state's seat, it is extraction via cost externalization: budgets for emergency healthcare and social assistance are consumed by inadequately insured platform workers. The engine computes per-seat directionality from the authored structural data (beneficiary/victim + power + exit); the authored claim (snare) reflects this reading's assessment, not a pre-engine consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are structural beneficiaries: they set the boundary, defend it through litigation and regulatory engagement, and capture the margin of cost externalization (d ≈ 0.1–0.2, near beneficiary). Platform workers are structural targets: they bear the cost of insurance, lack exit options (identity-locked into platform participation for income), and have no voice in the boundary's definition or maintenance (d ≈ 0.85–0.95, near target). State insurance systems are secondary targets: constrained exit (cannot refuse to treat uninsured workers, cannot opt out of social responsibility), diffuse payer role (costs distributed across taxpayers and public budgets), and no control over the boundary (d ≈ 0.70–0.80). Courts and regulators are analytical observers: they have the power to redraw the boundary but are often captured or ideologically aligned with the formalist reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (low-cost labor entry for platform disruption of traditional services) is now dead: platforms are mature, highly profitable (billions in market cap), and no longer need the cost-minimization mechanism to survive. Yet the boundary persists and strengthens, indicating mandatrophy: the function the rule was built for is gone, but the extraction it enables has become the primary beneficiary value. The theater ratio rising from 0.35 to 0.48 indicates increasing performative maintenance: platforms invest more in 'flexibility' marketing and autonomy rhetoric as the reality of control and dependence becomes more visible to workers and regulators. This is the signature of a mandatrophic constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalist_vs_substantive_kernel_reading,
    'Is the employment boundary defined by formal contract-plus-supervision (formalist reading) or by economic dependence-plus-control (substantive reading)?',
    'Statutory clarification, court precedent unification across jurisdictions, or empirical evidence from jurisdictions that have adopted substantive readings (California, EU) showing either system viability or collapse.',
    'If substantive reading is adopted, platform workers are reclassified as employees, platforms face retroactive liability and benefits obligations, and the ε of this constraint drops to near-zero (the extraction mechanism disappears). This reading forecloses the formalist reading within a unified legal framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalist_vs_substantive_kernel_reading, conceptual, 'Core kernel disagreement: the definition of employment itself').

omega_variable(
    algorithmic_control_as_supervision,
    'Does algorithmic assignment, performance monitoring, and behavioral enforcement constitute ''supervision'' under employment law, or does supervision require human intermediation?',
    'Legal interpretation through case law or statute; empirical assessment of actual control exercised over workers by algorithms (task assignment, pace control, deactivation).',
    'If algorithmic control is counted as supervision, the formalist reading''s core distinction (direct human supervision) collapses, and workers move toward substantive-reading classification. If human supervision is required, the formalist reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_as_supervision, conceptual, 'Whether algorithmic control satisfies the supervision criterion').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (platforms actively suppress voice through deactivation, classification, litigation) or internalized (workers internalize the contractor identity and compliance)?',
    'Post-reclassification trajectory: if workers who are reclassified to employees show reduced suppression (higher organizing, grievance filing, collective action), the suppression is largely structural. If suppression persists after reclassification, it is partially internalized.',
    'If structural, suppression is platform-agency-dependent and will fall if the boundary is redrawn. If internalized, workers carry the suppression with them even if legal status changes, indicating deeper identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural machinery or internalized identity lock').

omega_variable(
    flexibility_preference_vs_coercion,
    'Do workers choose contractor classification for genuine flexibility preference, or is it a condition of market access they accept because no alternative is available?',
    'Counterfactual scenario: offer workers the choice between contractor and employee status while holding market access constant. Measure what percentage choose each. Cross-check with qualitative interviews about exit options and choice constraints.',
    'If preferences are genuine, some suppression is the cost of coordination for workers who value flexibility (justifying lower theater_ratio). If choices are coerced, theater_ratio should be higher and the constraint moves toward pure snare with no coordination benefit to workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_preference_vs_coercion, empirical, 'Whether ''flexibility'' is a genuine worker preference or coerced acceptance').

omega_variable(
    reading_instantiation_boundary,
    'Is this JSON properly instantiating the formalist reading alone, or does it mix in critiques and structural pressures from substantive and hybrid readings?',
    'Logical consistency check: the narrative, beneficiary/victim sets, and metrics should cohere around the formalist reading''s own internal logic, not an external critique. The omegas route contestation; the base story should be clean.',
    'If the story mixes readings, sibling stories will not be able to properly instantiate the alternatives without redundancy or contradiction. If clean, each reading stands on its own structural footing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_instantiation_boundary, conceptual, 'Whether this constraint story cleanly instantiates the formalist reading without contamination from alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(empl_tr_t0, observed).
narrative_ontology:measurement(empl_tr_t5, employment_boundary__formalist_employment_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(empl_tr_t5, observed).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__formalist_employment_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(empl_tr_t10, observed).
narrative_ontology:measurement(empl_tr_t15, employment_boundary__formalist_employment_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(empl_tr_t15, observed).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__formalist_employment_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(empl_tr_t20, observed).
narrative_ontology:measurement(empl_tr_t25, employment_boundary__formalist_employment_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(empl_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(empl_be_t0, observed).
narrative_ontology:measurement(empl_be_t5, employment_boundary__formalist_employment_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(empl_be_t5, observed).
narrative_ontology:measurement(empl_be_t10, employment_boundary__formalist_employment_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement_basis(empl_be_t10, observed).
narrative_ontology:measurement(empl_be_t15, employment_boundary__formalist_employment_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement_basis(empl_be_t15, observed).
narrative_ontology:measurement(empl_be_t20, employment_boundary__formalist_employment_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement_basis(empl_be_t20, observed).
narrative_ontology:measurement(empl_be_t25, employment_boundary__formalist_employment_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(empl_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(empl_su_t0, observed).
narrative_ontology:measurement(empl_su_t5, employment_boundary__formalist_employment_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(empl_su_t5, observed).
narrative_ontology:measurement(empl_su_t10, employment_boundary__formalist_employment_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(empl_su_t10, observed).
narrative_ontology:measurement(empl_su_t15, employment_boundary__formalist_employment_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(empl_su_t15, observed).
narrative_ontology:measurement(empl_su_t20, employment_boundary__formalist_employment_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(empl_su_t20, observed).
narrative_ontology:measurement(empl_su_t25, employment_boundary__formalist_employment_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(empl_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__formalist_employment_reading, 0.18).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel decomposes into three constraint stories, one per reading. The formalist_employment_reading (this file) instantiates the legal doctrine that employment is defined by formal contract and direct supervision. The substantive_employment_reading instantiates the alternative that employment is defined by economic dependence and algorithmic control, treating the same workers as employees. The hybrid_security_reading instantiates a third way: a distinct legal category for platform workers with tailored protections. Each story has a different ε referent (the standing arrangement assessed under that reading's lights), different beneficiary/victim structure, different classification. They are linked via network.affects_constraints to enable the engine to track how contestation propagates across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__formalist_employment_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
