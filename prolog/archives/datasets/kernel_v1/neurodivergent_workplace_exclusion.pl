% ============================================================================
% CONSTRAINT STORY: neurodivergent_workplace_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_neurodivergent_workplace_exclusion, []).

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
 *   constraint_id: neurodivergent_workplace_exclusion
 *   human_readable: Neurodivergent Workplace Exclusion
 *   domain: employment/organizational/social
 *
 * SUMMARY:
 *   Neurodivergent workplace exclusion is a composite structural constraint
 *   operating through interlocking mechanisms: algorithmic screening
 *   optimized for neurotypical resume patterns; interview formats that test
 *   social performance rather than task capability; office environments
 *   designed for neurotypical sensory tolerance and interruption-management;
 *   real-time communication norms that penalize different processing speeds;
 *   and internalized masking requirements that make exit from suppression
 *   psychologically unthinkable even when material barriers relax. The
 *   constraint appears as pure extraction (Snare) from the perspective of
 *   excluded job seekers, as beneficial coordination (Rope) from the hiring
 *   system's perspective, as mixed coordination-extraction (Tangled Rope)
 *   from the perspective of neurodivergent insiders who masked through
 *   hiring, as a collective structural problem requiring organized pressure
 *   (Snare with organized power) from advocacy coalitions, and as
 *   institutional inertia coupled with performative commitments (Piton) from
 *   diversity-declaring organizations that maintain neurotypical-optimized
 *   screening. The constraint exhibits a false-summit signature: the
 *   analytical observer risks naturalizing it as an immutable feature of
 *   organizational efficiency (Natural Law), but the identification of
 *   beneficiaries, the structural constructedness of screening algorithms and
 *   office design, and the demonstrated existence of alternative
 *   neurodiversity-affirming organizations all confirm that the constraint is
 *   constructed and reclassifiable. The extractiveness trajectory (0.42 →
 *   0.50 → 0.58) reflects increasing algorithmic sophistication and cultural
 *   internalization of neurotypicality standards. The suppression trajectory
 *   (0.65 → 0.70 → 0.72) reflects both increasing enforcement (stricter
 *   algorithmic filters, more demanding interview formats) and increasing
 *   internalization (masking becomes habituated, neurodivergent identity
 *   becomes foreclosed). The theater ratio trajectory (0.55 → 0.62 → 0.68)
 *   reflects growing decoupling between stated diversity commitments and
 *   functional screening mechanisms — organizations declare neurodiversity
 *   values while algorithmic and interview infrastructure remains
 *   neurotypical-optimized.
 *
 * KEY AGENTS:
 *   - Excluded Neurodivergent Job Seekers: Primary victims (powerless/trapped) — systematically excluded from employment opportunity at every stage; cannot exit job market without economic destitution
 *   - Neurotypical Hiring System: Primary beneficiary (institutional/arbitrage) — captures benefits of exclusion (lower recruitment cost, cultural homogeneity, reduced accommodation burden) with maximal optionality to change course but no incentive to do so
 *   - Unmasked Neurodivergent Insiders: Secondary victims (moderate/constrained) — who masked through hiring face dual constraints: ongoing masking requirement plus organizational accommodation barriers; moderate extraction offset by some coordination benefits
 *   - Neurodivergent Advocacy Coalition: Organized agent (organized/constrained) — recognizing systemic extraction and building alternatives; constrained by resource limits and institutional resistance
 *   - Neurodiversity-Affirming Employer: Mixed beneficiary/coordinator (institutional/constrained) — recognizes both ethical case and business case for inclusion; faces genuine coordination gains (underutilized talent) and costs (accommodation infrastructure); experiences as tangled rope
 *   - Performative Diversity Function: Institutional actor with decoupled commitments (institutional/arbitrage) — declares neurodiversity values while maintaining neurotypical-optimized screening; represents piton inertia
 *   - Algorithmic Screening Vendor: Beneficiary (institutional/arbitrage) — captures margin from 'optimized' screening tools that exclude neurodivergent candidates; business model depends on continued neurotypical optimization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(neurodivergent_workplace_exclusion, 0.58).
domain_priors:suppression_score(neurodivergent_workplace_exclusion, 0.72).
domain_priors:theater_ratio(neurodivergent_workplace_exclusion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(neurodivergent_workplace_exclusion, extractiveness, 0.58).
narrative_ontology:constraint_metric(neurodivergent_workplace_exclusion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(neurodivergent_workplace_exclusion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(neurodivergent_workplace_exclusion, snare).
narrative_ontology:human_readable(neurodivergent_workplace_exclusion, "Neurodivergent Workplace Exclusion").
narrative_ontology:topic_domain(neurodivergent_workplace_exclusion, "employment/organizational/social").

domain_priors:requires_active_enforcement(neurodivergent_workplace_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(neurodivergent_workplace_exclusion, neurotypical_workforce).
narrative_ontology:constraint_beneficiary(neurodivergent_workplace_exclusion, screening_algorithm_vendors).
narrative_ontology:constraint_beneficiary(neurodivergent_workplace_exclusion, hr_gatekeeping_function).
narrative_ontology:constraint_victim(neurodivergent_workplace_exclusion, autistic_workers).
narrative_ontology:constraint_victim(neurodivergent_workplace_exclusion, adhd_workers).
narrative_ontology:constraint_victim(neurodivergent_workplace_exclusion, dyslexic_workers).
narrative_ontology:constraint_victim(neurodivergent_workplace_exclusion, other_neurodivergent_cohorts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEURODIVERGENT JOB SEEKER (SNARE) — Faces cumulative barriers at every stage: algorithmic screening rejects resumes that use non-standard formatting or resume structures; interviews test social performance (eye contact, small talk, rapid response) rather than technical competence; office sensory environments (open plan, fluorescent lighting, constant interruption) are incompatible with their cognitive style; rapid-fire communication norms penalize processing time. Cannot exit employment market without economic destitution. Experiences constraint as pure extraction — systematically excluded from opportunity regardless of capability.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: NEUROTYPICAL HIRING SYSTEM (ROPE) — Experiences the constraint as coordination: standardized screening mechanisms enable scale, rapid filtering, and cultural fit assessment. The hiring system benefits from the exclusion (lower recruitment cost, cultural homogeneity, reduced accommodation burden). Net beneficiary with maximal exit optionality — can arbitrage to alternative talent sources but chooses not to because homogeneous hiring reduces friction. Effective extraction d ≈ 0.10 (full beneficiary + arbitrage exit).
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: NEURODIVERGENT INSIDER WHO UNMASKED POST-HIRE (TANGLED ROPE) — Successfully passed neurotypical screening (by masking) but now faces dual constraints: ongoing need to mask cognitive style in real-time interactions (suppression), but also genuine coordination benefits from team collaboration and structured role expectations (some agents benefit from routine and clear task definition). Moderate extracted cost (mask maintenance, social energy drain) offset by organizational resources and identity safety gains if disclosure is supported. Exit constrained by career investment, social bonds, and accommodation uncertainty — but not trapped as trapped.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NEURODIVERGENT ADVOCACY COALITION (SNARE) — Organized agents (neurodiversity advocacy groups, self-advocates, disability rights organizations) recognize the systemic extraction and are building alternatives (neurodiversity-affirming hiring, sensory-friendly interview formats, asynchronous communication norms, strengths-based assessment). However, these coalitions operate against entrenched screening infrastructure and cultural defaults. Their power is organized but constrained by resource limits and institutional resistance. They experience the snare as a collective-level structural problem that requires sustained organizing pressure to shift.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: NEURODIVERSITY-AFFIRMING EMPLOYER (TANGLED ROPE) — Some institutional actors (tech companies investing in neurodiversity hiring, organizations with autism/ADHD specialist recruitment) recognize both the ethical case for inclusion AND the business case (underutilized talent pool, diverse cognitive strengths). These employers face genuine coordination gains (tapping excluded talent, innovation benefits of cognitive diversity) but also real coordination costs (accommodation infrastructure, communication norm redesign, manager training). They experience the constraint as a hybrid: genuine coordination function coupled with asymmetric implementation burden (they bear setup costs; society benefits from inclusion).
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PERFORMATIVE DIVERSITY INITIATIVE (PITON) — Some organizations declare neurodiversity commitments (diversity recruiting statements, unconscious bias training, neurodiversity resource groups) while maintaining screening mechanisms that functionally exclude neurodivergent applicants. The performative layer (stated values) has decoupled from functional mechanisms (actual screening and environment design). Theater ratio is high (0.68): the stated commitment is real but the infrastructure remains neurotypical-optimized. This represents institutional inertia — the organization's espoused values are in tension with its operational defaults.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing the constraint: 'workplaces require fast-paced communication, social quick-thinking, and neurotypical sensory tolerance because complex organizations inherently demand these traits.' This frame sees neurodivergent exclusion as a necessary consequence of organizational efficiency requirements, not as a contingent design choice. The engine will identify this as a false summit — the constraint is entirely constructed from institutional choices (algorithm design, interview format, office layout, communication norms), not natural law. Beneficiaries and structural data confirm the false-summit classification.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neurodivergent_workplace_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(neurodivergent_workplace_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(neurodivergent_workplace_exclusion, TR),
    TR >= 0.70.

:- end_tests(neurodivergent_workplace_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantial cost from excluded neurodivergent workers (income loss, identity suppression, psychological harm from repeated rejection), but this is not the maximal extraction of a snare because some neurodivergent workers do find employment and some organizations are building alternatives. The extractiveness reflects the systematic gatekeeping across the hiring funnel and the internalized suppression of neurodivergent identity in workplace contexts. The trajectory from 0.42 to 0.58 reflects increasing algorithmic sophistication in screening and increasing cultural internalization of neurotypicality as organizational requirement. Suppression (0.72): High. Multiple binding mechanisms: (1) Structural barriers — algorithmic screening filters, interview formats testing social performance, office environments incompatible with neurodivergent sensory/processing needs, rapid-fire communication norms. (2) Material barriers — employment dependency for economic survival; relocation costs if remote work unavailable; lack of alternative job markets. (3) Internalized suppression — learned masking, identity foreclosure, stereotype threat, imposter syndrome internalized from repeated exclusion. The suppression trajectory (0.65 → 0.72) reflects increasing enforcement (tighter filters) and increasing internalization. Theater ratio (0.68): Moderate-high. The hiring process contains significant performative content: interviews test social presentation rather than task capability; diversity statements declare commitment to inclusion while screening mechanisms functionally exclude; unconscious bias training creates impression of addressing exclusion without redesigning infrastructure. The theater has increased (0.55 → 0.68) as organizations have added diversity commitments while maintaining unchanged screening mechanisms. Claimed type (Snare): Correct. From the perspective of excluded job seekers, the constraint exhibits all snare characteristics: high extraction (systematic exclusion from opportunity), high suppression (material barriers + internalized masking), minimal coordination benefit (the constraint does not solve a genuine job-matching problem — it solves the problem of 'avoiding the cost of accommodation and cognitive difference'), and reliance on suppression for existence (without the screening exclusion, neurotypical incumbents would face competition from a larger talent pool). However, from the hiring system's perspective, the constraint functions as Rope — a coordination mechanism. The perspectival gap is diagnostic.
 *
 * PERSPECTIVAL GAP:
 *   The hiring system (institutional/arbitrage) sees Rope: 'We need consistent screening mechanisms to process large volumes of applications efficiently. Screening filters enable cultural fit assessment and reduce recruitment cost.' The excluded neurodivergent worker (powerless/trapped) sees Snare: 'Every stage of the hiring process filters out candidates with my cognitive style, regardless of capability. I am systematically excluded from economic opportunity.' The neurodiversity-affirming employer (institutional/constrained) sees Tangled Rope: 'We benefit from access to a large, underutilized talent pool (coordination gain), but we must build accommodation infrastructure and redesign communication norms (extraction cost to us, benefit to excluded workers).' The unmasked neurodivergent insider (moderate/constrained) sees Tangled Rope: 'My work is valued once hired, but I must sustain masking in meetings and rapid-fire communication contexts. Some coordination (clear role expectations, structured tasks) benefits my cognitive style, but the masking requirement extracts psychological cost.' The advocacy coalition (organized/constrained) sees Snare: 'This is a systemic gatekeeping mechanism that requires organized pressure to shift.' The performative diversity organization (institutional/arbitrage) sees Piton: 'Our stated diversity values are real, but our screening infrastructure has not actually changed — we maintain the neurotypical defaults while declaring commitment to change.' The analytical observer risks seeing Mountain: 'Workplaces inherently require fast-paced communication and neurotypical social performance because organizations are complex and demand cognitive alignment.' But the structural data (identifiable beneficiaries, constructed screening algorithms, alternative neurodiversity-affirming organizations) reveals the mountain as a false summit. The constraint is entirely constructed from institutional choices, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary group (neurotypical workforce + screening vendors + HR gatekeeping function) derives d ≈ 0.10-0.15 (institutional + arbitrage exit = full beneficiary, lowest directionality). They experience the constraint as beneficial coordination (Rope) because they have not internalized it as a limit — they can arbitrage to alternative hiring approaches but choose not to because the neurotypical-optimized default is convenient. The victim group (autistic workers + ADHD workers + dyslexic workers + other neurodivergent cohorts) derives d ≈ 0.92-0.98 (powerless + trapped exit = full target, highest directionality). They experience the constraint as pure extraction (Snare) because they cannot exit employment without destitution and have no alternative job markets. The unmasked insider (moderate/constrained) derives d ≈ 0.62-0.75 (moderate + constrained exit = mid-range target): constrained exit options (career investment, social bonds, disclosure risk) keep them in the constraint even though they are not as powerless as job seekers. The forward-thinking employer (institutional/constrained) derives d ≈ 0.35-0.45 (mixed beneficiary-victim status: they benefit from talent access and innovation but bear setup costs; constrained exit — they have decided to commit to inclusion): moderate extraction direction reflecting their hybrid position. The hiring system writ large (institutional/arbitrage) derives d ≈ 0.08 (full beneficiary, arbitrage optionality).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that neurodivergent workplace exclusion is simultaneously (1) a rational response to organizational structure (the hiring system sees coordination — hence Rope), (2) a mechanism of pure extraction (excluded workers see Snare), and (3) a problem that forward-thinking organizations are solving (hence Tangled Rope and Scaffold perspectives are real). The constraint exemplifies how a single structural phenomenon can have multiple legitimate classifications depending on observer position and structural relationship. The false-summit risk is high: the analytical observer can naturalize the constraint as an immutable feature of organizational efficiency ('complex work requires neurotypical cognitive style'). But the structural data contradicts this: (a) alternative organizations demonstrate that high-performing teams exist with neurodiversity-affirming norms; (b) the screening mechanisms are constructed (algorithm design choices, interview format choices, office layout choices), not discovered as natural law; (c) the beneficiaries are identifiable (neurotypical incumbents, screening vendors, cost-minimizing HR functions). The mandatrophy resolves to: This constraint is real and damaging (Snare classification holds for excluded workers), but it is entirely constructed and reclassifiable through organizational redesign (alternative coordination norms, asynchronous communication, strengths-based assessment, sensory-friendly environments). The constraint persists not because it is natural law but because neurotypical incumbents benefit from the exclusion and have not invested in alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accommodation_cost_measurement,
    'Are workplace accommodations for neurodivergent workers genuinely costly, or is the cost externalized perception driven by neurotypical baseline assumptions?',
    'Comparative cost analysis: actual expenditure on accessibility modifications vs. claimed costs in budget proposals; audit of organizations with neurodiversity hiring showing real vs. projected accommodation expenses; measurement of productivity outcomes post-accommodation',
    'If accommodation costs are genuinely high: the tangled_rope classification is correct — real coordination burden. If costs are low but perceived as high: the constraint is driven by status-quo bias and organizational inertia, pushing toward snare classification from the hiring system''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_cost_measurement, empirical, 'Whether workplace accommodations are genuinely costly or cost-perception driven').

omega_variable(
    neurotype_performance_correlation,
    'Does neurotypical communication style and sensory profile actually correlate with job performance, or does the correlation reflect selection bias (only neurotypical-compatible workers get hired and visible)?',
    'Longitudinal study comparing neurodivergent workers hired despite mismatched screening with neurotypical hires; performance outcome measurement controlling for role compatibility; meta-analysis of accommodation outcome studies',
    'If correlation is causal: screening mechanisms are legitimate efficiency tools (snare reclassifies toward tangled_rope). If correlation is selection bias: screening mechanisms are extractive gatekeeping with no validity (snare classification confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neurotype_performance_correlation, empirical, 'Whether neurotype-performance correlation reflects genuine performance difference or selection bias').

omega_variable(
    identity_lock_masking_mechanism,
    'To what extent do neurodivergent workers internalize the requirement to mask as part of their professional identity, making exit unthinkable even when economic constraints relax?',
    'Qualitative interviews with neurodivergent professionals in high-masking environments; analysis of disclosure barriers and identity salience; measurement of psychological distress associated with identity suppression vs. exit barriers',
    'If identity-lock is primary binding mechanism: exit classification shifts from trapped to identity_locked; constraint operates through cognitive capture (internalized neurotypicality standards) rather than structural barriers alone. Suggests different intervention targets (identity reframing vs. accommodation infrastructure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_masking_mechanism, conceptual, 'Role of internalized masking requirement in binding neurodivergent workers to exclusionary constraints').

omega_variable(
    algorithmic_bias_measurement_basis,
    'Are the screening algorithm exclusions optimized for neurotypical patterns, or are they detecting actual role-fit signal that happens to correlate with neurotype?',
    'Algorithm audit: reverse-engineer training data and decision rules; compare algorithm output with human screening decisions on same resumes; test algorithm on neurodiversity-adjusted resume formats; measure false-positive/false-negative rates by neurotype',
    'If bias is optimization artifact (algorithms trained on neurotypical-heavy hiring data): redesign algorithms with diverse training sets and accessibility-aware features (snare reclassifies toward tangled_rope — the problem is solvable at moderate cost). If algorithms are detecting genuine role-fit signal: the screening mechanism is structurally sound but the job design itself is neurotype-biased (the constraint moves upstream to job architecture, not hiring mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_bias_measurement_basis, empirical, 'Whether algorithmic screening bias reflects optimization for neurotypical patterns or genuine role-fit signal').

omega_variable(
    false_summit_natural_law_claim,
    'Is neurodivergent workplace exclusion an immutable feature of organizational efficiency (natural law), or a constructed institutional arrangement that benefits neurotypical incumbents?',
    'Organizational case studies of neurodiversity-affirming workplaces showing comparable or superior performance; comparative institutional analysis showing how different organizations achieve coordination through different communication and sensory norms; historical analysis of when neurotypical-optimized designs became organizational defaults',
    'If exclusion is inherent to efficiency: mountain classification holds. If exclusion is constructed: false summit confirmed, reclassification to tangled_rope/snare depending on coordination function analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether neurodivergent exclusion is a natural law of organizational efficiency or a constructed arrangement').

omega_variable(
    masking_suppression_measurement,
    'How much of the suppression metric reflects structural barriers (algorithmic screening, environmental design) vs. internalized suppression (learned masking, identity foreclosure)?',
    'Comparative analysis: suppression experienced by neurodivergent workers in disclosure-safe environments vs. high-stakes interview contexts; measurement of stereotype threat effects and imposter syndrome; analysis of suppression persistence after barrier removal',
    'If suppression is primarily structural: exit constraints improve as barriers lower (trapped → constrained → mobile). If suppression is primarily internalized: even barrier removal doesn''t eliminate binding (identity_locked category captures this; suggests psychological intervention targets).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(masking_suppression_measurement, empirical, 'Relative contribution of structural barriers vs. internalized suppression to total suppression metric').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(neurodivergent_workplace_exclusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neuro_tr_t0, neurodivergent_workplace_exclusion, theater_ratio, 0, 0.55).
narrative_ontology:measurement(neuro_tr_t5, neurodivergent_workplace_exclusion, theater_ratio, 5, 0.62).
narrative_ontology:measurement(neuro_tr_t10, neurodivergent_workplace_exclusion, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(neuro_be_t0, neurodivergent_workplace_exclusion, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(neuro_be_t5, neurodivergent_workplace_exclusion, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(neuro_be_t10, neurodivergent_workplace_exclusion, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(neuro_su_t0, neurodivergent_workplace_exclusion, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(neuro_su_t5, neurodivergent_workplace_exclusion, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(neuro_su_t10, neurodivergent_workplace_exclusion, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(neurodivergent_workplace_exclusion, identity_coordination).
narrative_ontology:affects_constraint(neurodivergent_workplace_exclusion, masking_identity_suppression).
narrative_ontology:affects_constraint(neurodivergent_workplace_exclusion, diversity_washing_performative_inclusion).
narrative_ontology:affects_constraint(neurodivergent_workplace_exclusion, algorithmic_bias_hiring_systems).

% DUAL FORMULATION NOTE:
% Neurodivergent workplace exclusion decomposes into three linked constraints: (1) algorithmic_bias_hiring_systems (ε ≈ 0.45) — technical mechanism of exclusion through screening optimization; (2) masking_identity_suppression (ε ≈ 0.62) — internalized suppression requirement; (3) diversity_washing_performative_inclusion (ε ≈ 0.38) — organizational theater that masks unchanged exclusionary mechanisms. Each has distinct ε, beneficiaries, and intervention points. This story represents the composite meta-constraint bridging them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(neurodivergent_workplace_exclusion, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
