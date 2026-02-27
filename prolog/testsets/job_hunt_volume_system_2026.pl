% ============================================================================
% CONSTRAINT STORY: job_hunt_volume_system_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: economic/labor_markets
 *
 * SUMMARY:
 *   The algorithmic volume filter in recruitment represents a structural
 *   constraint at the intersection of labor market scaling and information
 *   asymmetry. As digital job boards have lowered application friction, firms
 *   face genuinely high application volumes (100-500+ per position at scale).
 *   The applicant tracking system (ATS) emerged as a coordination tool to
 *   manage this volume. However, the constraint exhibits characteristics of
 *   pure extraction (Snare) from the job seeker perspective: opaque filtering
 *   criteria, uncompensated labor optimizing for algorithmic parsing,
 *   suppressed transparency, and no appeal mechanism. The same system appears
 *   as legitimate coordination (Rope) from the hiring firm's perspective,
 *   which genuinely needs some filtering mechanism. The theater ratio (0.64)
 *   reflects that firms publicly claim ATS provides 'fair and efficient
 *   screening' while knowing the system produces false negatives, filters
 *   based on keyword proximity rather than capability, and amplifies
 *   demographic biases. The extractiveness trajectory (0.35→0.58 over 10
 *   years) shows degradation as keyword gaming has intensified and
 *   algorithmic criteria have become more opaque, not more refined.
 *
 * KEY AGENTS:
 *   - Job Seekers: Primary victims (powerless/trapped) — must apply through ATS but receive no feedback on rejection criteria; uncompensated labor perfecting applications for algorithmic parsing
 *   - Hiring Firms: Primary beneficiaries (institutional/arbitrage) — benefit from volume reduction and coordination but have option to revert to manual screening if ATS becomes counterproductive
 *   - Qualified Candidates Outside Keyword Proximity: Secondary victims (moderate/constrained) — have real qualifications but filtered by algorithmic criteria they cannot influence or negotiate
 *   - Recruitment Platform Providers: Institutional beneficiaries (institutional/arbitrage) — maintain ATS as part of product suite; benefit from opacity and reduced transparency obligations
 *   - Regulatory/Advocacy Coalition: Organized secondary actors (organized/constrained) — labor advocates, EEOC, employment attorneys see both coordination function and bias amplification; have limited enforcement power
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing volume-driven filtering as inevitable law rather than policy-contingent outcome
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
narrative_ontology:constraint_claim(job_hunt_volume_system_2026, snare).
narrative_ontology:human_readable(job_hunt_volume_system_2026, "The Algorithmic Volume Filter in Modern Recruitment").
narrative_ontology:topic_domain(job_hunt_volume_system_2026, "economic/labor_markets").

domain_priors:requires_active_enforcement(job_hunt_volume_system_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(job_hunt_volume_system_2026, hiring_firms).
narrative_ontology:constraint_beneficiary(job_hunt_volume_system_2026, recruitment_platforms).
narrative_ontology:constraint_victim(job_hunt_volume_system_2026, job_seekers).
narrative_ontology:constraint_victim(job_hunt_volume_system_2026, labor_market_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JOB SEEKER (SNARE) — Trapped in a system where application volume far exceeds human review capacity. Must apply repeatedly without visibility into why applications are rejected. No alternative to the applicant tracking system (ATS) filter for most formal employment. Experiences maximum extraction: time spent optimizing resume formatting for algorithmic parsing, uncompensated labor perfecting applications, emotional cost of silent rejections. Cannot exit without abandoning formal job search.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HIRING FIRM (ROPE) — Uses ATS as a genuine coordination tool: filters 500+ applications to 20 viable candidates per position, enabling specialization. Benefits from automation reducing screening labor. Experiences the constraint as solving a real coordination problem (managing volume). Can exit by returning to manual screening if algorithms become counterproductive, giving them arbitrage options.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: QUALIFIED CANDIDATE OUTSIDE KEYWORD PROXIMITY (SNARE) — Has real qualifications but does not use the precise terminology encoded in the keyword filter. Constrained by need to apply but suppressed from advancing. Lower power than purely powerless but still trapped — cannot negotiate with the algorithm, cannot appeal, cannot know the exact criteria they failed.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY/ADVOCACY COALITION (TANGLED ROPE) — Labor advocates, employment agencies, and equity regulators see both coordination function (managing scale) and extraction mechanism (bias amplification, opacity). Have some agency through legislative proposals (algorithmic transparency laws, ATS audits) but constrained by industry resistance. Benefits from visible labor market function but bears cost of legitimizing opaque systems.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RECRUITMENT PLATFORM PROVIDER (PITON) — Maintains ATS infrastructure as part of larger HRIS ecosystem. Primary revenue from employers, not from improving matching. The algorithm persists largely for institutional inertia (all competitors have similar systems) rather than because it optimizes matching. Theater high: firms claim ATS is 'efficient' and 'fair' while knowing it creates false negatives. Low functional verification — no transparency mechanism to validate quality of filtering. Theater ratio high because platform providers benefit from opacity.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION ASYMMETRY VIEW (MOUNTAIN) — From a universal analytical context, high-volume labor markets create irreducible information asymmetry: firms cannot individually evaluate thousands of applications without some filtering mechanism. The volume itself is a fixed constraint (mathematical limit on human review capacity). This perspective risks naturalizing what is actually a policy-contingent outcome: the 'high volume' is a product of low job-application friction and broad job boards, not an immutable law.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(job_hunt_volume_system_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.58): High-moderate. Job seekers experience significant extraction: uncompensated labor spent on keyword optimization, opportunity cost of silent rejections preventing learning, emotional cost of volume-based anonymity. Hiring firms extract value by outsourcing screening labor to candidates. However, the extraction is not maximal (0.70+) because some firms do invest in recruitment specialists who can navigate and supplement ATS, and because the coordination function is genuine — without any volume management, hiring at scale becomes infeasible. The rise to 0.58 from 0.35 reflects that keyword gaming has become more prevalent and more demanding as candidates have learned that ATS feedback is unavailable. Suppression (0.68): High. Mechanisms include: (1) opacity of filtering criteria — candidates rarely know which keywords triggered rejection, (2) no appeal mechanism — filtered applications are final, (3) platform design makes individual company contact difficult, (4) social norm that job seeking is 'private' discourages collective action, (5) fear of being 'difficult' discourages requests for transparency, (6) alternative employment pathways require social capital or network access that many candidates lack. Theater ratio (0.64): High-moderate. Firms publicly market ATS as 'eliminating bias' and 'improving fairness' while using the system to reduce transparency and shift evaluation burden to candidates. Resume screening theater (formatting for ATS parsing) substitutes for actual capability demonstration. The theater has increased over time as candidates have learned ATS requirements and begun performing compliance rather than authenticity.
 *
 * PERSPECTIVAL GAP:
 *   Six perspectives, three distinct classifications. Job seekers and qualified candidates see Snare (high suppression, high d, no exit). Hiring firms see Rope (genuine coordination, arbitrage exit, low d). Regulatory coalition sees Tangled Rope (both coordination and extraction present, constrained exit). ATS providers see Piton (high theater, low functional improvement, institutional inertia). Analytical observer risks Mountain (naturalizing policy-contingent volume as law). The perspectival gap reveals that the constraint's type depends entirely on structural position — what is coordination for the firm is extraction for the job seeker.
 *
 * DIRECTIONALITY LOGIC:
 *   Job seekers derive high d (0.85-0.95) from victim status + trapped exit. They bear full cost (optimization labor, emotional burden, lost opportunity) with no compensation or leverage. Hiring firms derive low d (0.15-0.25) from beneficiary status + arbitrage exit. They capture value (labor cost reduction) with option to exit if ATS becomes counterproductive. Qualified candidates outside keyword proximity derive high-moderate d (0.70-0.80) from moderate power + constrained exit. They have some agency (can adjust resume, apply through networks) but are suppressed by the system. Regulatory coalition derives moderate d (0.55-0.65) from organized power + constrained exit. They have leverage (law, audits) but limited enforcement power and must work within competitive market constraints. ATS providers derive low d (0.20-0.30) from institutional power + arbitrage exit. They benefit from opacity and have option to increase transparency if forced by regulation. Analytical observer derives moderate d (0.70-0.75) as analytical context. The derivation chain produces perspectival spread: job seekers experience χ ≈ 1.0 (high f(d) × suppression), hiring firms experience χ ≈ 0.25 (negative f(d) scales down even high suppression), qualified candidates experience χ ≈ 0.7 (moderate f(d)).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE-TROPHY RISK: The system risks being misclassified as pure coordination (Rope) if measured from hiring firm perspective alone. Firms can claim 'we have to filter volume somehow' (true, coordination function) while suppressing evidence that the specific filtering mechanism (keyword proximity rather than capability) extracts value beyond coordination (false negative rate studies would reveal this). The mitigation requires measuring from multiple perspectives simultaneously: job seeker experience (Snare), hiring firm experience (Rope), and qualified candidate experience (Snare) together establish that the same technical system serves dual functions — coordination for the firm, extraction for the candidate. Transparency mechanisms (revealing filter criteria, enabling appeal) would reduce extraction by giving candidates exit options (ability to reframe qualifications, challenge decisions). Currently mandatrophy is NOT resolved because no measurement simultaneously validates the dual function across perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_filter_validity,
    'Do current ATS keyword filters actually predict job performance, or are they simply convenient volume reducers that approximate demographic biases?',
    'Longitudinal matching study: correlate ATS filter outcomes with 2-year job performance ratings for hired candidates; separate effect sizes for candidates who passed vs were filtered out',
    'If predictive: filters are coordination mechanism (Rope). If non-predictive: filters are pure extraction masquerading as efficiency (Snare from all perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_filter_validity, empirical, 'Whether ATS filters predict job performance or merely reduce volume').

omega_variable(
    qualified_false_negatives_rate,
    'What fraction of applicants filtered out by ATS would have succeeded in the role?',
    'Randomized control: sample 1% of algorithmically filtered applications, conduct human review, hire subset, track 2-year retention and performance. Compare to hired candidate population.',
    'If false negative rate > 30%: extraction mechanism is severe (high snare classification). If < 10%: filters are genuinely accurate (rope dominates). Mid-range: tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qualified_false_negatives_rate, empirical, 'False negative rate in ATS filtering').

omega_variable(
    transparency_feasibility,
    'Can firms provide real-time feedback to rejected applicants on filter criteria without creating new extraction or gaming vectors?',
    'Pilot transparency intervention: select firms provide candidates with specific filter feedback (e.g., ''missing keyword X'', ''experience level below threshold''). Measure: does feedback enable correction, or does it create resume-gaming arms race?',
    'If transparency enables genuine improvement: constraint becomes less extractive (candidates have exit path). If transparency creates gaming: extraction persists (theater ratio increases as candidates perform compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_feasibility, empirical, 'Whether transparency reduces or redirects extraction').

omega_variable(
    volume_necessity_threshold,
    'At what application volume does human screening become genuinely infeasible, and below that threshold, do algorithmic filters extract value beyond coordination?',
    'Time-motion analysis: measure reviewer capacity for detailed screening (applications per hour). Calculate breakeven volume for different role types (entry-level, specialized). Compare to actual application volumes.',
    'If most roles are below breakeven: ATS is pure extraction (high snare). If most roles exceed breakeven: ATS solves real coordination problem (rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(volume_necessity_threshold, empirical, 'Volume threshold at which algorithmic filtering becomes necessary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(job_hunt_volume_system_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jobvol_tr_t0, job_hunt_volume_system_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jobvol_tr_t5, job_hunt_volume_system_2026, theater_ratio, 5, 0.55).
narrative_ontology:measurement(jobvol_tr_t10, job_hunt_volume_system_2026, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(jobvol_be_t0, job_hunt_volume_system_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jobvol_be_t5, job_hunt_volume_system_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(jobvol_be_t10, job_hunt_volume_system_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(job_hunt_volume_system_2026, resource_allocation).
narrative_ontology:affects_constraint(job_hunt_volume_system_2026, resume_signaling_arms_race).
narrative_ontology:affects_constraint(job_hunt_volume_system_2026, labor_market_information_asymmetry).
narrative_ontology:affects_constraint(job_hunt_volume_system_2026, hiring_discrimination_amplification).

% DUAL FORMULATION NOTE:
% The algorithmic volume filter decomposes into three linked constraints: (1) volume management (ε≈0.25, Rope) — genuine coordination need, (2) opaque filtering (ε≈0.58, Snare) — extraction mechanism, (3) bias amplification (ε≈0.42, Tangled Rope) — mixing coordination and discriminatory extraction. This story focuses on the opaque filtering mechanism. The three stories should be linked in network to show how volume management becomes extraction when transparency is withheld.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(job_hunt_volume_system_2026, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
