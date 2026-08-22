% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Platform Worker Hybrid Security Boundary
 *   domain: labor/social_policy
 *
 * SUMMARY:
 *   The hybrid security reading instantiates a third legal category for
 *   platform workers: neither employees (no supervisory relationship, no
 *   permanent assignment) nor true independents (no pricing power,
 *   algorithmic assignment without worker choice, unilateral termination).
 *   This reading claims the hybrid boundary solves the coordination problem
 *   by providing workers portable benefits (medical 91.5%, injury 86.2%)
 *   while preserving platform flexibility. The constraint extracts precarity
 *   from workers (no pension, retirement security, or paid leave) while
 *   limiting platform obligation. Extractiveness is moderate (~0.58) because
 *   both basic protections and persistent gaps are real; suppression is
 *   moderate (~0.52) because workers can legally organize and advocate but
 *   lack traditional exit routes.
 *
 * KEY AGENTS:
 *   - Platform operators: set the hybrid boundary, define what portability means, algorithmic control of assignment — institutional power, arbitrage exit
 *   - Platform workers: algorithmically assigned, income volatile, receive basic protections but lack career development/retirement — powerless power atom, constrained exit
 *   - Worker advocates: secured initial protections, dispute sufficiency — organized power, mobile exit, beneficiary+payer role
 *   - Regulatory agencies: enforce the boundary, interpret mandates, evolve the kernel — institutional power, analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.58).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.52).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Platform Worker Hybrid Security Boundary").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, '7ff88522-20be-4bf7-ad04-f5a43a6adee2').
narrative_ontology:cs_kernel_codification('7ff88522-20be-4bf7-ad04-f5a43a6adee2', formalized).
narrative_ontology:cs_authority_grounding('7ff88522-20be-4bf7-ad04-f5a43a6adee2', lineage).
narrative_ontology:cs_interpretation_layer_present('7ff88522-20be-4bf7-ad04-f5a43a6adee2').
narrative_ontology:cs_reading_relation('7ff88522-20be-4bf7-ad04-f5a43a6adee2', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ff88522-20be-4bf7-ad04-f5a43a6adee2', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('7ff88522-20be-4bf7-ad04-f5a43a6adee2', foundational, platform_workers_require_distinct_category).
narrative_ontology:cs_axiom_status(platform_workers_require_distinct_category, holdable).
narrative_ontology:cs_axiom_grounding('7ff88522-20be-4bf7-ad04-f5a43a6adee2', platform_workers_require_distinct_category, conventional).
narrative_ontology:cs_axiom('7ff88522-20be-4bf7-ad04-f5a43a6adee2', secondary, portable_benefits_substitute_for_employment_security).
narrative_ontology:cs_axiom_status(portable_benefits_substitute_for_employment_security, holdable).
narrative_ontology:cs_axiom_grounding('7ff88522-20be-4bf7-ad04-f5a43a6adee2', portable_benefits_substitute_for_employment_security, empirically_contingent).
narrative_ontology:cs_reference_frame('7ff88522-20be-4bf7-ad04-f5a43a6adee2', third_category_worker_protection).
narrative_ontology:cs_drift_state('7ff88522-20be-4bf7-ad04-f5a43a6adee2', contemporary_enforcement_drift, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7ff88522-20be-4bf7-ad04-f5a43a6adee2', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, worker_advocates).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers_precarious).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, consumers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, worker_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate digital labor platforms (gig delivery, ride-share, task marketplaces). Define the hybrid classification as protecting workers while preserving operational flexibility: they offer portable benefits (medical 91.5%, injury 86.2%), mandatory insurance, and algorithmic assignment rather than formal employment. They benefit from avoiding full employment obligations (pension, unemployment insurance, paid leave) while claiming a protection-forward reading.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Perform work algorithmically assigned by platforms; income fluctuates with platform demand and algorithmic ranking. Receive basic medical and injury protections but lack career development pathways, retirement security, paid leave, or formal severance. Exit means losing income immediately; alternatives (formal employment, independent business) require capital or credentials they often lack. The hybrid boundary institutionalizes their precarity while nominally protecting them.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers_precarious, payer,
    powerless, biographical, constrained, global).

% Push for platform worker protections through legislation, organizing, and litigation. Benefit from the hybrid category as a policy compromise that established baseline protections and worker agency (injury insurance, medical coverage, right-to-counsel) they fought to secure. Also bear costs: the hybrid category prevents full employment classification, which would grant more comprehensive protections; they remain in ongoing dispute over sufficiency.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, worker_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, worker_advocates, payer).

% Traditional employment sectors (manufacturing, retail, professional services) see the hybrid category as establishing a precedent for downward pressure on employment protections. They monitor regulatory outcomes but are not direct parties to the platform worker arrangement itself.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employers, observer,
    institutional, generational, analytical, national).

% Enforce the hybrid classification in their jurisdictions, interpret what mandatory protections entail, investigate disputes between workers and platforms, and revise the boundary as case law and legislation evolve. They are the administrative keepers of the kernel.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, regulatory_agencies, observer,
    institutional, generational, analytical, national).

% Access on-demand services (delivery, rides, tasks) at low friction and often subsidized pricing. Benefit from the hybrid arrangement's structure, which enables platforms to offer rapid service and low costs by externalize worker security burdens onto the hybrid boundary rather than pricing them fully.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, consumers, beneficiary,
    moderate, immediate, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a third legal category distinct from employment and independent contracting, permitting platform operators to organize algorithmic labor markets while guaranteeing workers portable benefits (medical coverage, injury insurance) and basic labor protections (right-to-counsel, transparency in algorithmic assignment) without full employment obligations.
% TRANSFER_FUNCTION: Moves security risk from platforms to workers (who absorb income volatility, algorithmic termination without cause, no pension) while moving minimal protection obligations from traditional employment onto platforms (portable medical and injury insurance, but not paid leave, unemployment insurance, or career development). Transfers surplus to platform operators and consumers through lower service prices.
% ABSENT_VOICES: Workers excluded from hybrid arrangements (self-employed, informal gig workers in unregulated jurisdictions); competing labor-law traditions (substantive employment reading, formalist contracting reading); jurisdictions that have rejected the hybrid category outright or mandated full employment status; worker-owned and cooperative platform models that would operate under different incentive structures.
% DISAPPEARANCE_RATIONALE: If the hybrid boundary vanished, platforms would face immediate pressure to reclassify workers as either employees (triggering full pension, leave, unemployment obligations) or to exit jurisdictions imposing that standard. Workers would reorganize toward either stable employment or full independent status with its full risk. The regulatory and competitive landscape of platform labor would restructure within months.
% FOUNDING_PROBLEM: Platform labor does not fit traditional employment (no direct supervision, no permanent assignment, algorithmic rather than managerial direction) or independent contracting (worker has no client choice, no pricing power, algorithmic termination without cause). Existing law had no coherent category, leaving workers without baseline protections and platforms in legal ambiguity.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies in multiple jurisdictions (EU, California, UK) acknowledge the classification problem as real and unsolved. Worker advocates attest the problem is live but dispute whether the hybrid solution adequately addresses it. Platform operators argue the problem is substantially managed by the hybrid category; independent labor economists and worker testimony dispute that claim, citing retirement insecurity and algorithmic precarity as persistent founding-problem symptoms.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises slightly over the 16-year interval (0.44 → 0.58) as platforms operationalize algorithmic control and worker income volatility proves persistent despite basic insurance. Theater ratio rises from 0.31 to 0.41, reflecting growing gap between public framing ('we protect workers with portable benefits') and operational reality (basic protections coexist with algorithmic precarity, no career path, stagnant real income). Suppression requirement is modest and stable (~0.46–0.52) because the hybrid arrangement provides legal recognition and worker voice (right-to-counsel, transparency mandates) while foreclosing the more protective exit that full employment status would provide. Workers can organize and protest within the hybrid category but cannot escape it to better protections without leaving platform work entirely. Accessibility collapse (0.48) reflects that alternatives do exist but are severely constrained: formal employment requires employer willingness to hire; independent business requires capital; informal gig work foregoes the portable benefits. The hybrid boundary itself creates a barrier to seeing alternatives as viable.
 *
 * PERSPECTIVAL GAP:
 *   The operator and worker seats diverge sharply on whether the hybrid boundary is coordination or extraction. Operators perceive it as genuine coordination: they offer transparency, portable benefits, algorithmic assignment rules, and worker voice mechanisms that didn't exist before. Workers perceive it as institutionalized precarity: the same features (algorithmic assignment, income volatility, no career development, no retirement accumulation) persist and are now legally enshrined as acceptable. The engine computes per-seat classifications from the power and exit data: operators sit near beneficiary (institutional power, arbitrage exit options), workers sit near target (powerless, constrained exit). The moderate extractiveness reflects that genuine protections exist alongside genuine gaps — neither seat can call this pure coordination or pure extraction, but they disagree on whether the gaps are intentional features or acceptable costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators (institutional power, arbitrage exit) derive d near 0.2 — they benefit from the arrangement, define its terms, and can exit to other business models if enforcement tightens. Platform workers (powerless, constrained exit) derive d near 0.85 — they bear the precarity, cannot exit to better legal status without leaving platform work, and have no role in setting the boundary. Worker advocates (organized, mobile) derive d near 0.45 — they secured real protections but remain in dispute over sufficiency and lack the institutional power to push the boundary further without legislation. Regulatory agencies (institutional, analytical) observe but do not extract or benefit; their d is at the analytical anchor. The hybrid arrangement itself extracts by design: it locks workers into a third category that provides less comprehensive protection than employment while foreclosing independent business viability (workers lack the capital and client bases that true independence requires).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (labor markets need a category between employment and independence) is live and contested. The hybrid solution claimed to solve it while both protecting workers and preserving platform flexibility. The theater ratio (0.41 at interval end) shows that a growing share of enforcement is theatrical: platforms publicize their protections (medical, injury, right-to-counsel) while algorithmic control over assignment and termination intensifies. The gap between claimed coordination (we've created a protective third category) and measured extraction (workers remain in precarious income situations with no retirement path) is the mandatrophy signature: the boundary persists because it benefits operators and appears to benefit workers relative to having no protections at all, but the founding problem of providing meaningful security in platform labor remains unsolved. The constraint prevents reclassification to either full employment or full independence, both of which would force a reckoning with the precarity question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.52) structural (barriers to exit, regulatory foreclosure of full employment classification) or internalized (workers have accepted algorithmic precarity as normal, normalized their own expendability)?',
    'Post-reclassification longitudinal study: if a jurisdiction mandates full employment status for platform workers, tracking worker subjective experience (whether suppression decreases after the structural barrier is removed) reveals whether suppression was internalized. Contrast with jurisdictions maintaining the hybrid category.',
    'If suppression is internalized, the constraint''s effective grip is stronger than the structural measure suggests — workers carry the precarity with them even after exit, making the constraint''s cultural/psychological entrenchment part of what enforcement maintains. If structural, the suppression reflects genuine barriers to exit and would decrease sharply if those barriers were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression persists as internalized acceptance or would dissolve with structural reclassification.').

omega_variable(
    portable_benefits_sufficiency_boundary,
    'Are the portable benefits (medical 91.5%, injury 86.2%) genuine coordination benefits that address a real coordination problem, or cover stories for limiting platform obligation?',
    'Comparative analysis: do comparable independent contractors (actual business owners, not platform workers in independent-contractor contracts) achieve equivalent benefit coverage through personal purchase or business operations? Do platform workers'' portable benefits cover the same risk scope as employment-package benefits?',
    'If portable benefits are genuine coordination (solving the real problem that workers transition between platforms and need continuous coverage), they support the hybrid reading''s claim. If they are coverage masking (medical coverage at 91.5% but no dental, vision, mental health; injury insurance but no long-term disability) the benefits are performative, pushing the constraint toward snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(portable_benefits_sufficiency_boundary, empirical, 'Whether portable benefits represent genuine coordination or limited coverage deployed as justification.').

omega_variable(
    retirement_security_gap_structural,
    'Is the absence of retirement accumulation (pensions, mandatory savings, employer contribution) a consequence of hybrid status that could be remedied within the hybrid framework, or is it structurally baked into the algorithmic assignment model?',
    'Policy experiments: mandatory savings accounts (workers + platforms both contribute a percentage of transaction value) or portable pension funds (platform contributions to a worker-owned retirement pool). If these can be operationalized within algorithmic assignment, the gap is remediable; if they create intractable operational or accounting conflicts, the gap reveals a structural tension between platform economics and retirement security.',
    'If remediable, the hybrid category can be strengthened without reclassification; if structural, the hybrid boundary institutionalizes retirement insecurity by design, pushing extractiveness upward and supporting mandate for employment reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retirement_security_gap_structural, empirical, 'Whether retirement absence is remediable within the hybrid framework or structurally inherent to platform assignment.').

omega_variable(
    kernel_reading_coexistence_stability,
    'Can the three readings (formalist, hybrid, substantive) remain permanently coexistent across different jurisdictions, or does regulatory/competitive pressure inevitably force convergence toward one reading?',
    'Longitudinal regulatory tracking: do jurisdictions that adopt the hybrid reading remain stable, or do enforcement problems (classification disputes, regulatory interpretation drift) gradually push them toward either formalist or substantive readings? Do competitive pressures from jurisdictions using different readings create arbitrage that destabilizes coexistence?',
    'If permanent coexistence is stable, the kernel remains genuinely contested and this constraint continues as tangled_rope. If pressure toward convergence is high, the kernel is effectively decided by power (which jurisdiction/regime sets the standard), and this constraint becomes temporary or becomes a snare for workers in losing jurisdictions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_stability, conceptual, 'Whether the three readings can remain in stable coexistence or regulatory/competitive pressure forces convergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.31).
narrative_ontology:measurement(empl_tr_t2, employment_boundary__hybrid_security_reading, theater_ratio, 2, 0.34).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__hybrid_security_reading, theater_ratio, 4, 0.37).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__hybrid_security_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__hybrid_security_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__hybrid_security_reading, theater_ratio, 16, 0.41).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(empl_be_t2, employment_boundary__hybrid_security_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(empl_be_t4, employment_boundary__hybrid_security_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(empl_be_t8, employment_boundary__hybrid_security_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(empl_be_t12, employment_boundary__hybrid_security_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(empl_be_t16, employment_boundary__hybrid_security_reading, base_extractiveness, 16, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(empl_su_t2, employment_boundary__hybrid_security_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(empl_su_t4, employment_boundary__hybrid_security_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(empl_su_t8, employment_boundary__hybrid_security_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(empl_su_t12, employment_boundary__hybrid_security_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(empl_su_t16, employment_boundary__hybrid_security_reading, suppression_requirement, 16, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__hybrid_security_reading, 0.18).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__substantive_employment_reading).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel has three readings, each instantiating distinct constraints with different ε values, beneficiary/victim structures, and classification trajectories. The hybrid_security_reading claims a third legal category (moderate ε ≈ 0.58, beneficiary operators + worker advocates + consumers, victim workers). The formalist_employment_reading claims workers are legally independent contractors (low ε ≈ 0.22, minimal victim set, primarily mountain or rope classification). The substantive_employment_reading claims workers are economically employees regardless of contract form (high ε ≈ 0.78, victim set workers, beneficiary job-market competitors and labor standards advocates, snare classification). All three readings affect one another through regulatory competition and jurisdictional harmonization pressure. This constraint (hybrid) sits between the formalist boundary (too permissive of operator control without protection) and substantive boundary (too restrictive of operational flexibility) in the policy landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__hybrid_security_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
