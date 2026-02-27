% ============================================================================
% CONSTRAINT STORY: job_hunt_volume_system_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_job_hunt_volume_system_2026, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: job_hunt_volume_system_2026
 *   human_readable: The Algorithmic Volume Filter in Modern Recruitment
 *   domain: economic/labor_market
 *
 * SUMMARY:
 *   The algorithmic volume filter in recruitment represents a structural
 *   transformation of the labor market matching process. Originally, ATS
 *   systems were designed as talent identification tools — database search
 *   and ranking to find qualified candidates among thousands. Over the past
 *   15 years, they have evolved into volume management platforms that shift
 *   screening burden from recruiters to job seekers. The constraint exhibits
 *   both coordination (solving the problem of 500+ daily applications per
 *   role) and extraction (imposing resume rewriting labor, keyword
 *   optimization busywork, application spam arms race) simultaneously. The
 *   system is now past the tipping point where the extractive burden exceeds
 *   the coordination benefit for most job seekers. The theater ratio (0.64)
 *   reflects that much ATS operational activity is performative: resume
 *   parsing accuracy is 60-70%, keyword matching is mechanical pattern
 *   detection unmoored from actual job requirements, and the 'intelligence'
 *   in the system is largely cosmetic. Meanwhile, job seekers invest
 *   significant labor in application optimization (keywords, formatting,
 *   resume tailoring) that may have zero correlation with job suitability.
 *   The constraint generates asymmetric extraction: recruiters and ATS
 *   vendors benefit from high-volume processing and switching costs; job
 *   seekers bear the busywork costs and face algorithmic filtering with no
 *   transparency or appeal mechanism. The system persists not because it
 *   works well, but because the alternative (human resume review at scale)
 *   seems impossibly resource-intensive.
 *
 * KEY AGENTS:
 *   - Job Seekers: Primary victim (powerless/trapped) — forced to optimize resumes for algorithms they cannot see or appeal; face 2-3% application-to-interview conversion rates driven by filter noise
 *   - Enterprise Recruiters: Primary beneficiary (institutional/arbitrage) — benefit from volume management and audit trail for legal compliance; can customize filter thresholds to game outcomes
 *   - ATS Vendors: Secondary beneficiary (institutional/arbitrage) — vendor lock-in and switching costs create recurring revenue; resist transparency that would expose filtering inaccuracy
 *   - HR Function Under Pressure: Organized victim (organized/constrained) — attempt to counterbalance bias and unfairness while being locked into ATS by corporate policy and vendor agreements
 *   - Direct-Hire Movement: Alternative pathway (organized/mobile) — startups and tech firms experimenting with portfolio-based screening, referral networks, direct-to-candidate outreach
 *   - Labor Market Efficiency: System-level victim (analytical/analytical) — extraction of hiring signal quality, application spam arms race, time waste across millions of job seekers
 *   - Career-Switcher: Mixed victim/beneficiary (moderate/constrained) — benefits from standardized screening reducing discrimination risk; victim to algorithmic filtering when credentials don't match keyword patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(job_hunt_volume_system_2026, 0.58).
domain_priors:suppression_score(job_hunt_volume_system_2026, 0.68).
domain_priors:theater_ratio(job_hunt_volume_system_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(job_hunt_volume_system_2026, tangled_rope).
narrative_ontology:human_readable(job_hunt_volume_system_2026, "The Algorithmic Volume Filter in Modern Recruitment").
narrative_ontology:topic_domain(job_hunt_volume_system_2026, "economic/labor_market").

domain_priors:requires_active_enforcement(job_hunt_volume_system_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(job_hunt_volume_system_2026, enterprise_recruiters).
narrative_ontology:constraint_beneficiary(job_hunt_volume_system_2026, applicant_tracking_vendors).
narrative_ontology:constraint_victim(job_hunt_volume_system_2026, job_seekers).
narrative_ontology:constraint_victim(job_hunt_volume_system_2026, labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JOB SEEKER (SNARE) — Structurally trapped. Must submit applications through vendor-controlled ATS systems; has no alternative pathway to hiring managers. Suppression is total: keyword optimization becomes mandatory busywork, application volume requirements create artificial scarcity of attention, and algorithmic filtering hides rejection mechanisms from view. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. Bears extraction with zero exit options.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CAREER-SWITCHER (TANGLED ROPE) — Constrained by geographic/credential barriers. Benefits from standardized screening (reduces face-to-face discrimination risk, enables remote applications). Victim to algorithmic filtering: resume rewriting labor, keyword stuffing, multiple applications needed to overcome filter. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.61. Mixed coordination (access) and extraction (filtering).
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENTERPRISE RECRUITER (ROPE) — Institutional beneficiary. ATS systems solve a genuine coordination problem: filtering 500+ daily applications into manageable pools. Experiences the constraint as coordination infrastructure. Can arbitrage by customizing filter thresholds, gaming vendor metrics. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; negative effective extraction = coordination benefit.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HR FUNCTION UNDER PRESSURE (TANGLED ROPE) — Organized institutional actor (HR departments, DEI initiatives) attempting to counterbalance the ATS extraction. Benefits from standardized screening (reduces liability for discrimination claims, creates audit trails). Victim to vendor lock-in and algorithmic bias propagation — spending resources on bias audits, fairness metrics, retraining models. Enforcement: internal mandate to use ATS despite known harms. d≈0.52, f(d)≈0.68, σ=1.1 → χ≈0.43. Active coordination function (fairness) meets active enforcement (vendor requirement).
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ATS VENDOR ECOSYSTEM (PITON) — Maintains market dominance through inertia and switching costs, not superior function. Original purpose (talent identification) has largely atrophied; current function is cost-shifting (moving screening burden from employers to job seekers). Theater ratio 0.64: much of ATS operational activity is performative (resume parsing accuracy is ~60-70%, keyword matching is mechanical). The ecosystem persists because abandoning ATS would require coordinating thousands of HR departments to implement alternative workflows — collective action is harder than incremental vendor patches. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Piton classification from theater gate (≥0.70 threshold not met; 0.64 is borderline); function degradation is evident but still marginally exceeds rope threshold.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DIRECT-HIRE MOVEMENT (TANGLED ROPE) — Organized actors (startups, tech firms, university recruiting) creating alternative hiring pathways (referrals, direct-to-candidate outreach, portfolio-based screening). Benefits: bypass ATS filtering, reduce job seeker labor. Victim: constrained by need to hire at scale, limited resources for human screening, pressure from investors to adopt 'standard' ATS for due diligence. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.29. Lower extraction than mainstream; represents incipient scaffold/rope transition.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: LABOR MARKET EFFICIENCY (ANALYTICAL SNARE) — From a civilizational/analytical view, the ATS system represents pure extraction from labor market efficiency. Mismatched hiring (poor signal-to-noise in filtering), application spam arms race (resume inflation), job seeker time waste (modal application-to-interview rate ~2-3%), and risk of bias amplification all reduce market efficiency. The constraint extracts labor-hour value from both job seekers (application busywork) and employers (poor hires). d≈0.78, f(d)≈1.12, σ=1.2 → χ≈0.78. Structural extraction from system-level efficiency.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(job_hunt_volume_system_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(job_hunt_volume_system_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(job_hunt_volume_system_2026, TR),
    TR >= 0.70.

:- end_tests(job_hunt_volume_system_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The ATS system extracts significant labor from job seekers (resume optimization, keyword stuffing, application volume inflation) and captures switching cost value for vendors. The extraction is not total because the system does provide some genuine coordination value (filtering *some* unqualified applications, creating standardized workflows). The 0.58 value reflects the trajectory: extractiveness has risen from ~0.32 (2010, when ATS was tool-like) to 0.58 (2026, when it is platform-like). Suppression (0.68): Significant. Job seekers have limited alternatives (must use ATS or face near-zero visibility), keywords are not transparent, filtering logic is proprietary, appeal mechanisms are nonexistent, and resume optimization has become mandatory busywork. However, suppression is not total because some job seekers successfully advocate for resume drops, networking, or referrals. Theater ratio (0.64): Moderate-high. Much ATS activity is performative: resume parsing is mechanical (accuracy 60-70%), keyword matching is pattern detection divorced from actual job requirements, and the 'AI' label on sorting algorithms provides legitimacy theater. Job seekers perceive theater: 73% of surveyed job seekers report that ATS keyword optimization feels like busywork unrelated to job fit. The theater has increased over time as ATS vendors added 'AI' branding and 'intelligence' layers without improving actual matching quality. Claimed type (Tangled Rope): The system simultaneously provides coordination (volume management solution) and extraction (busywork, switching costs, algorithmic opacity). The requirements gate is met: `requires_active_enforcement: true` (HR departments mandate ATS use despite known harms), beneficiaries are identified (recruiters, vendors), victims are identified (job seekers, labor market efficiency). This is not a Snare because the coordination function is real, even if degraded. This is not a Rope because the extraction is asymmetric and suppression is substantial. Tangled Rope is the correct classification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence from the same structural data. The recruiter sees a coordination solution (Rope) — volume management that enables hiring at scale. The ATS vendor sees a fee-generating business model (Rope, nearly Piton) — maintenance of switching costs and vendor lock-in. The job seeker sees pure extraction (Snare) — mandatory busywork, opaque filtering, trapped status. The direct-hire movement sees a temporary, solvable problem (incipient Scaffold) — alternative pathways are emerging and proving viable. The HR function sees an enforcement nightmare (Tangled Rope) — required to use ATS while bearing responsibility for bias and fairness failures. The labor market efficiency observer sees structural extraction from system-level matching quality (Snare from analytical perspective). The career-switcher sees mixed outcomes (Tangled Rope) — benefits from standardization but victim to algorithmic misclassification. No single perspective is 'wrong' — the constraint is genuinely perceived as coordination by beneficiaries and extraction by victims. The perspectival gaps reveal: (1) the extraction is real and asymmetric, (2) the coordination benefit is real but degrading, (3) the theater is increasing, and (4) the system is past the tipping point where alternatives would produce better outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Job seeker: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction directionality. Career-switcher: Victim + constrained (some alternatives available) → d≈0.68, f(d)≈1.05. High extraction but not maximal. Recruiter: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Low directionality; net beneficiary. ATS vendor: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Low directionality; net beneficiary through fee structure. HR function: Mixed (enforcer + constraint victim) + constrained → d≈0.52, f(d)≈0.68. Moderate extraction — they benefit from audit trail and bias mitigation tools but victim to vendor lock-in and responsibility for failures. Direct-hire movement: Organized + mobile → d≈0.35, f(d)≈0.35. Low extraction; mobile agents with alternative pathways. Labor market efficiency: Analytical victim → d≈0.78, f(d)≈1.12. High extraction from system perspective. The derivation chain produces a perspectival gap in d-values that ranges from 0.08 (beneficiaries) to 0.92 (trapped victims), confirming the tangled_rope classification: high variance in who benefits and who bears costs within the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the ATS system is genuinely a hybrid: it provides real coordination (volume management, standardized workflows, audit trail for compliance) AND real extraction (busywork, opacity, switching costs, algorithmic noise). The error would be to classify this as pure Rope (ignoring the extractive burden) or pure Snare (ignoring the coordination function). The Tangled Rope classification captures the duality: the constraint is NOT a natural evolution of recruitment efficiency; it is an institutional arrangement where the coordination benefit (volume management) has been decoupled from the job-matching benefit (finding qualified candidates). The system optimizes for *filtering scale*, not *hiring quality*. This is enforcement (HR departments mandate ATS use) + extraction (job seekers bear busywork burden) + coordination (recruiters can process volume). The mandatrophy resolution: the system is NOT a necessary tool (alternatives exist and work), and the persistence of ATS dominance reflects switching costs and vendor lock-in, not functional superiority. If the classification were 'Mountain' (immutable necessity), that would be false summit. If the classification were 'Rope' (pure coordination), that would miss the systematic extraction. If the classification were 'Snare' (pure extraction), that would miss the real coordination value. Tangled Rope correctly identifies the hybrid and predicts the system's trajectory: as alternatives mature (direct-hire, portfolio-based screening, AI-free matching networks), the coordination-to-extraction ratio will decline, and the system will degrade toward Piton (maintained by inertia, not function) before finally transitioning to Scaffold (acknowledging the temporary nature of ATS dominance, with a true sunset clause).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ats_filtering_accuracy_threshold,
    'What resume filter accuracy threshold distinguishes legitimate screening from extractive busywork?',
    'Longitudinal study comparing false negative rates (qualified candidates filtered out) vs true positive rates (selected candidates who succeed); correlation between ATS score and job performance metrics',
    'If accuracy > 75%: filtering is predominantly coordination (lower chi for job seekers). If accuracy < 60%: filtering is predominantly extraction (higher chi, approaches snare classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ats_filtering_accuracy_threshold, empirical, 'Threshold for distinguishing ATS screening accuracy from noise').

omega_variable(
    alternative_hiring_pathway_viability,
    'Can direct-to-candidate outreach and portfolio-based screening scale to enterprise hiring volumes without reintroducing discrimination risk?',
    'Pilot studies at 5-10 large firms; comparison of hiring outcomes (diversity, performance, time-to-hire) vs ATS-based cohorts; cost analysis of human-intensive screening',
    'If viable: alternative pathways become structural (scaffold classification confirmed; sunset for ATS dominance is real). If not: ATS persistence is forced (snare deepens for job seekers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_hiring_pathway_viability, empirical, 'Whether alternative hiring pathways can scale without discrimination reintroduction').

omega_variable(
    job_seeker_coalition_formation,
    'Can job seekers organize to demand transparent filtering criteria, making powerless agents collective powerful?',
    'Track growth of job seeker advocacy movements (#ATS boycott, alternative hiring networks); measure adoption rates of transparent hiring practices by major employers',
    'If coalition forms: powerless agent''s exit_options upgrade from trapped to constrained/mobile; d-value decreases; chi drops significantly. Snare classification shifts to tangled_rope. If coalition fails: powerless agent remains trapped; snare classification persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(job_seeker_coalition_formation, conceptual, 'Whether job seeker coalition can shift from trapped to organized status').

omega_variable(
    regulatory_intervention_likelihood,
    'Will labor regulators mandate transparency in algorithmic hiring (EU AI Act style) or algorithmic disclosure requirements?',
    'Monitor regulatory proposals in US, EU, UK labor law; track precedent from finance algorithmic transparency requirements (e.g., COMPAS, credit scoring)',
    'If yes (probability ~60% by 2030): regulatory enforcement converts piton/snare to scaffold (sunset clause emerges). If no: extractiveness persists or deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention_likelihood, preference, 'Whether regulatory intervention will mandate algorithmic hiring transparency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(job_hunt_volume_system_2026, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jvf_tr_t0, job_hunt_volume_system_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jvf_tr_t8, job_hunt_volume_system_2026, theater_ratio, 8, 0.55).
narrative_ontology:measurement(jvf_tr_t16, job_hunt_volume_system_2026, theater_ratio, 16, 0.64).

% Extraction over time
narrative_ontology:measurement(jvf_be_t0, job_hunt_volume_system_2026, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(jvf_be_t8, job_hunt_volume_system_2026, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(jvf_be_t16, job_hunt_volume_system_2026, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(job_hunt_volume_system_2026, resource_allocation).
narrative_ontology:affects_constraint(job_hunt_volume_system_2026, labor_market_information_asymmetry).
narrative_ontology:affects_constraint(job_hunt_volume_system_2026, resume_credential_inflation).
narrative_ontology:affects_constraint(job_hunt_volume_system_2026, hiring_bias_amplification_algorithms).

% DUAL FORMULATION NOTE:
% The algorithmic volume filter is upstream of (and reinforces) multiple related constraints in the labor market. Resume credential inflation is driven by the ATS keyword arms race. Hiring bias amplification is enabled by the opacity of ATS filtering. Labor market information asymmetry is sustained by the one-way filtering (job seekers cannot see why they are rejected). These constraints form a causal cluster: the volume filter is the enabling mechanism for the ecosystem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(job_hunt_volume_system_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
