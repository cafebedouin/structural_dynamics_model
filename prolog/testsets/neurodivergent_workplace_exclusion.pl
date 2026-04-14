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
 *   Neurodivergent workplace exclusion is a structural constraint that
 *   appears primarily as an employment barrier but operates through multiple
 *   interlocking mechanisms: screening algorithms optimized for neurotypical
 *   resume patterns; interview formats that test social performance rather
 *   than task capability; office environments designed for neurotypical
 *   sensory tolerance and interruption-management; rapid real-time
 *   communication norms that penalize different processing speeds; and
 *   internalized narratives that frame neurodivergent cognitive differences
 *   as individual deficits requiring invisible conformity (masking). The
 *   constraint exhibits genuine coordination components — some work genuinely
 *   requires synchronous attention and multi-person coordination — but
 *   maintains and amplifies requirements beyond what the actual work demands,
 *   creating unnecessary exclusion. The extractive layer is the optimization
 *   of workplace norms for neurotypical performance beyond coordination
 *   necessity. The constraint is actively enforced through hiring criteria,
 *   performance management, and social sanctioning of disclosure. Theater has
 *   increased over the measurement interval as diversity and inclusion
 *   programs create performative responses (training, diverse job
 *   descriptions, disability awareness campaigns) while core exclusion
 *   mechanisms remain unchanged. The constraint generates a perspectival
 *   spectrum from pure extraction (powerless job seeker), to identity-locked
 *   internalization (employed worker who attributes exclusion to personal
 *   inadequacy), to mixed coordination and extraction (disclosed employee
 *   with accommodations), to pure coordination (compliance officer), to
 *   alternative pathway construction (organized neurodiversity advocates), to
 *   degraded institutional response (D&I theater). This range indicates a
 *   Tangled Rope: genuine coordination function coexists with asymmetric
 *   extraction, and active enforcement maintains the arrangement despite
 *   available alternatives.
 *
 * KEY AGENTS:
 *   - Neurodivergent Job Seekers: Primary victim (powerless/trapped) — encounter algorithmic filtering, interview format barriers, and discrimination. No exit options.
 *   - Employed Neurodivergent Workers: Secondary victim (powerless/identity_locked) — experience extraction through masking requirement, which is internalized as professional duty. Identity fused with invisible conformity.
 *   - Neurodivergent Employees with Disclosure: Mixed victim/beneficiary (moderate/constrained) — access accommodations that enable function but face subtle discrimination risk from documented disability status.
 *   - Organizational Management: Primary beneficiary (institutional/arbitrage) — benefits from self-selecting labor force optimized for neurotypical performance. No extraction cost perceived.
 *   - Neurotypical Employees: Secondary beneficiary (powerful/mobile) — default workspace optimization grants performance advantage without requiring accommodation.
 *   - Corporate D&I Programs: Institutional actor (institutional/arbitrage) — maintains performative response theater while core exclusion mechanisms persist; enables organization to appear inclusive without systemic change.
 *   - Neurodiversity Advocacy Organizations: Organized coalition (organized/constrained) — building alternative employment models and accessibility standards; working toward sunset through normalized remote work and asynchronous communication.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — can identify which workplace requirements are structurally necessary vs. culturally conventional and therefore contingent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(neurodivergent_workplace_exclusion, 0.58).
domain_priors:suppression_score(neurodivergent_workplace_exclusion, 0.68).
domain_priors:theater_ratio(neurodivergent_workplace_exclusion, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(neurodivergent_workplace_exclusion, extractiveness, 0.58).
narrative_ontology:constraint_metric(neurodivergent_workplace_exclusion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(neurodivergent_workplace_exclusion, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(neurodivergent_workplace_exclusion, tangled_rope).
narrative_ontology:human_readable(neurodivergent_workplace_exclusion, "Neurodivergent Workplace Exclusion").
narrative_ontology:topic_domain(neurodivergent_workplace_exclusion, "employment/organizational/social").

domain_priors:requires_active_enforcement(neurodivergent_workplace_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(neurodivergent_workplace_exclusion, neurotypical_employees).
narrative_ontology:constraint_beneficiary(neurodivergent_workplace_exclusion, organizational_management).
narrative_ontology:constraint_beneficiary(neurodivergent_workplace_exclusion, productivity_metrics_optimizers).
narrative_ontology:constraint_victim(neurodivergent_workplace_exclusion, autistic_workers).
narrative_ontology:constraint_victim(neurodivergent_workplace_exclusion, adhd_workers).
narrative_ontology:constraint_victim(neurodivergent_workplace_exclusion, dyslexic_workers).
narrative_ontology:constraint_victim(neurodivergent_workplace_exclusion, neurodivergent_job_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEURODIVERGENT JOB SEEKER (SNARE) — Faces insurmountable barriers: screening algorithms filter resumes by neurotypical communication patterns; interview formats (open-ended, real-time, social performance) directly disadvantage autistic and ADHD applicants; disclosure triggers discrimination; concealment creates unsustainable masking. Exit options collapse to zero — cannot leave employment system, cannot access unfiltered labor market. Maximum suppression and extraction.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYED NEURODIVERGENT WORKER (SNARE) — Structurally constrained by employment dependence but identity-locked by internalized narrative: 'I am lazy/broken/not trying hard enough.' Masking is constituted as professional duty and personal virtue. Burnout is reframed as weakness. The worker cannot exit because their identity as 'professional' requires invisible conformity. Exit would mean admitting systemic disability rather than personal failure — identity breaks.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: NEURODIVERGENT EMPLOYEE WITH DISCLOSURE (TANGLED ROPE) — Has disclosed diagnosis and obtained accommodations. Experiences genuine coordination benefit (modified sensory environment, asynchronous communication, task clarity reduce cognitive friction). But also experiences asymmetric extraction: accommodation requests are documented in personnel files; managers track compliance with disability status; subtle reduction in advancement opportunities; colleagues perceive accommodation as special treatment. Mixed experience: coordination function is real but enforcement layer creates extraction.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZATION / COMPLIANCE OFFICER (ROPE) — Experiences accommodations as pure coordination mechanism. Legal requirement (ADA, AODA, Equality Act 2010) is frame-agnostic but seen through lens of cooperative obligation. Accommodations enable worker productivity without requiring systemic change. Net beneficiary through maintained labor supply and legal compliance. No experienced extraction — the constraint is coordination framing.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZATION / PRODUCTIVITY OPTIMIZATION (ROPE) — Experiences the exclusion as beneficial coordination. Neurotypical-optimized workflows (rapid context-switching, unstructured meetings, interruption-tolerant environments) maximize output for neurotypical employees. Neurodivergent workers who cannot adapt are naturally filtered out. The constraint solves the coordination problem of 'how to maintain a high-interruption, context-switching-intensive workflow': exclude workers whose neurology makes them poor at these tasks. Beneficiary through self-optimizing labor market.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NEURODIVERSITY ADVOCACY COALITION (SCAFFOLD) — Organized actors (disability organizations, neurodivergent worker cooperatives, inclusive-design advocates) are building alternative employment pathways with sunset logic: remote-first work norms, asynchronous communication, clear task specifications, and sensory-accessible environments make neurotypical optimization less necessary. As these norms mature (generational timescale), the exclusion mechanism loses force. The scaffold perspective sees the constraint as solvable through institutional design — not inherent to work.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CORPORATE D&I PROGRAM (PITON) — Disability and neurodiversity are now fixtures of D&I initiatives. However, the functional benefit of D&I programs on neurodivergent hiring and retention is minimal compared to the performative content: diversity statements, inclusive job descriptions, unconscious bias training, and neurodiversity awareness campaigns are largely theatrical. The underlying exclusion mechanisms (algorithm bias, interview format optimization for neurotypical performance, masking requirement) remain unchanged. Piton classification: high theater (0.62), minimal actual exclusion reduction.
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, workplace coordination requires some degree of synchronization, sensory tolerance, and real-time social performance. These requirements are genuinely necessary for *some* forms of work (real-time trading, synchronous surgery, live performance). But modern knowledge work and service work have shifted toward formats that maintain these neurotypical-optimized requirements despite having no structural necessity: emails that require immediate response, open-office sensory environments, synchronous meetings for asynchronous content, interview formats that test social performance rather than task capability. The constraint has both genuine coordination components (team synchronization) and contingent extractive components (optimization for neurotypical performance beyond synchronization need).
constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neurodivergent_workplace_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(neurodivergent_workplace_exclusion, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Moderately high. The constraint creates significant career and economic disadvantage for neurodivergent workers (reduced hiring, slower advancement, wage penalties), but not at maximum severity because: (1) some accommodations are available post-disclosure, (2) alternative employment paths exist (self-employment, specialized roles, remote-first companies), and (3) measurement reflects current state with emerging alternatives (remote work norms are weakening the constraint). If measurement were from 2010, extractiveness would be higher (0.68+); the trajectory shows slight improvement. Suppression (0.68): High. Barriers to exit are substantial: legal employment requirement (economic survival), stigma around disclosure (risk of discrimination), limited alternative job markets (most organizations replicate the constraint), and economic penalties for career gaps or self-employment. Suppression is primarily structural (external barriers) rather than purely internalized, though internalization amplifies the effect. Theater ratio (0.62): Moderate-high and increasing. Corporate diversity programs, inclusive job descriptions, and neurodiversity awareness training create visible institutional response but have minimal functional impact on hiring or retention of neurodivergent workers. The theater increased from 0.35 (2010: minimal D&I response) to 0.62 (2025: substantial D&I apparatus with limited actual change). This increase signals Piton dynamics: as the constraint became politically visible, the response was primarily theatrical rather than structural.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap is dramatic. The powerless neurodivergent job seeker perceives pure extraction (Snare) with no coordination benefit — the constraint only excludes. The employed neurodivergent worker (especially pre-disclosure) perceives the constraint as immutable and self-caused (identity-locked Snare) — they cannot see the structural mechanism, only their own inadequacy. The neurodivergent employee with accommodations sees mixed coordination and extraction (Tangled Rope) — accommodations enable function but create documentation risk. The organization's compliance perspective sees pure coordination (Rope) — legal requirement drives sensible inclusion policy. The organization's productivity perspective also sees coordination (Rope) — the constraint solves the problem of maintaining high-interruption workflows by filtering out workers incompatible with that style. The neurodiversity advocacy coalition sees a solvable temporary problem (Scaffold) — changing work norms (remote-first, asynchronous-first) will reduce exclusion as new defaults become normalized. The D&I program sees its own institutional response as adequate (Piton) — diversity statements and training are theater that obscures the unchanged core mechanisms. The analytical observer at civilizational scale sees a hybrid (Tangled Rope) with contingent components — some synchronization genuinely required, much is culturally conventional and changeable. The gap arises because: (1) the powerless agent cannot see alternatives, so the constraint appears inevitable; (2) the employed worker has internalized the constraint as identity, preventing perception of mutability; (3) the organization benefits from the status quo and perceives compliance as the maximal policy lever; (4) advocates see structural change as possible through design; (5) the institutional program mistakes visibility for function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from power level, exit options, and beneficiary/victim status. For the powerless job seeker (trapped): power amplifies victim status + zero exit options → high d (0.92) → high f(d) → high chi. They bear maximum extraction. For the employed worker with identity_locked exit: structurally mobile (could theoretically exit) but cognitively captured by internalized narratives → moderate d (0.80) relative to trapped agent but same classification (Snare) because the identity lock prevents exercising whatever structural mobility exists. For the moderate disclosed employee (constrained exit): moderate power + partial exit options (could change roles or self-employ at cost) + mixed victim/beneficiary → moderate d (0.58) → moderate chi. For the institutional beneficiary (arbitrage exit): beneficiary status + institutional power + ability to arbitrage (can shift to other labor markets, different work styles) → low d (0.15) → negative chi (extracted toward this agent). For the organized coalition (constrained): organized power + constrained exit (can build alternatives but at resource cost) → moderate-low d (0.35) → moderate chi downward. The directionality pipeline shows why the beneficiary sees coordination (Rope, low chi) while the victim sees extraction (Snare, high chi): the same constraint produces opposite directional values for different agents, which the f(d) sigmoid translates into different experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that neurodivergent workplace exclusion is genuinely a Tangled Rope, not a pure Snare or pure Rope. The coordination component is real: workplaces must coordinate synchronization, team communication, sensory environments, and task clarity. These are not purely extractive overlay — they are genuine constraints on work organization. The extraction component is also real: the degree of neurotypical optimization, the suppression of alternatives (remote work, asynchronous communication, job redesign), and the enforcement through hiring and performance management go beyond what coordination necessity requires. The Tangled Rope classification captures both. The false summit (mountain classification) would wrongly naturalize workplace norms as inevitable law. The false pure-extraction (snare-only) would ignore the genuine coordination requirements and miss why some optimization is legitimate. Tangled Rope correctly identifies the constraint as: (1) performing a necessary coordination function, (2) requiring active institutional enforcement to maintain, (3) asymmetrically extracting from neurodivergent workers beyond what coordination requires, and (4) maintainable only while alternatives (remote work, accommodation culture) are suppressed. As alternatives normalize (generational timescale), the extraction component weakens while the coordination component can persist in new forms — the scaffold sunset logic applies. The mandatrophy confirms that the classification must account for both the genuine and the contingent components simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_vs_convention,
    'How much of the neurotypical-optimized workplace requirement is structurally necessary vs. culturally conventional?',
    'Comparative analysis of productivity outcomes: same-task performance in synchronous vs. asynchronous, open vs. quiet, structured vs. unstructured, in-person vs. remote conditions. Neurotypical and neurodivergent performance across these conditions.',
    'If mostly necessary (>70%): constraint is closer to Mountain — coordination requirements genuinely exclude certain neuroligies. If mostly conventional (<40%): constraint is closer to pure Snare — the extraction is culture-driven, not structurally required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_convention, empirical, 'Necessity vs. convention in neurotypical workplace optimization').

omega_variable(
    identity_lock_reversibility,
    'Can neurodivergent workers who have internalized masking narratives shift their identity frame through cognitive intervention, or is the lock structurally irreversible within the current employment system?',
    'Longitudinal study of workers undergoing neurodivergence-affirming therapy or social transition; measurement of identity-lock indicators (self-blame, internalized deficit narratives) before and after environment change vs. cognitive intervention alone.',
    'If reversible through cognitive work: identity_locked classification is accurate and temporary. If reversible only through environment change: the ''identity lock'' is actually environmental capture requiring structural change, and the constraint''s extraction is more severe than identity_locked suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of internalized masking narratives').

omega_variable(
    remote_work_ceiling,
    'Do remote-first and asynchronous-first work norms actually reduce neurodivergent exclusion, or do they simply shift exclusion mechanisms from synchronous performance to documented productivity metrics and algorithmic monitoring?',
    'Comparison of neurodivergent hiring rates and retention rates in pre-pandemic fully remote organizations vs. transition-period remote-pivot organizations vs. modern synchronous-default organizations. Measurement of alternative exclusion mechanisms (time-tracking, productivity monitoring, async communication performance requirements).',
    'If remote reduces overall exclusion: scaffold sunset logic is valid and extraction mechanism weakens over time. If remote shifts but maintains exclusion: the constraint exhibits adaptation — Snare persists but changes enforcement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remote_work_ceiling, empirical, 'Whether remote work addresses root exclusion or shifts mechanism').

omega_variable(
    accommodation_as_documentation,
    'Do formal accommodations primarily enable neurodivergent worker function or primarily create documentation trails that enable subtle discrimination?',
    'Personnel file analysis comparing advancement rates, performance ratings, and termination reasons for workers with vs. without documented accommodations (controlling for actual disability severity and job fit). Manager interview analysis of how accommodation requests are perceived.',
    'If primarily enabling: Tangled Rope classification accurate — genuine coordination + limited extraction. If primarily documenting: accommodations are a Snare mechanism disguised as support — extraction is *increased* through formalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_as_documentation, empirical, 'Whether accommodations enable function or create exclusion trails').

omega_variable(
    suppression_internalization_ratio,
    'What proportion of measured suppression (0.68) is external (legal barriers, economic dependency, structural obstacles) vs. internal (internalized deficit narratives, masking requirement experienced as duty)?',
    'Post-disclosure and post-accommodation suppression change measurement: if suppression decreases significantly after removing legal barriers and obtaining support, it was primarily external. If suppression persists or decreases minimally, it is primarily internalized.',
    'If primarily external (>60%): Snare and Trapped classifications are most accurate. If primarily internal (>40%): identity_locked classification is more accurate; the constraint operates through cognitive capture rather than structural barrier.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_ratio, empirical, 'External vs. internal suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(neurodivergent_workplace_exclusion, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ndwe_tr_t0, neurodivergent_workplace_exclusion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ndwe_tr_t5, neurodivergent_workplace_exclusion, theater_ratio, 5, 0.52).
narrative_ontology:measurement(ndwe_tr_t10, neurodivergent_workplace_exclusion, theater_ratio, 10, 0.62).
narrative_ontology:measurement(ndwe_tr_t15, neurodivergent_workplace_exclusion, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(ndwe_be_t0, neurodivergent_workplace_exclusion, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ndwe_be_t5, neurodivergent_workplace_exclusion, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(ndwe_be_t10, neurodivergent_workplace_exclusion, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ndwe_be_t15, neurodivergent_workplace_exclusion, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(neurodivergent_workplace_exclusion, enforcement_mechanism).
narrative_ontology:affects_constraint(neurodivergent_workplace_exclusion, disability_identity_externalization).
narrative_ontology:affects_constraint(neurodivergent_workplace_exclusion, masking_burnout_cycle).
narrative_ontology:affects_constraint(neurodivergent_workplace_exclusion, workplace_accessibility_standards).

% DUAL FORMULATION NOTE:
% Neurodivergent workplace exclusion decomposes into three distinct constraints: (1) The hiring/screening mechanism (ε=0.72, pure Snare) that filters neurodivergent applicants — distinct from (2) The workplace accommodation system (ε=0.44, Tangled Rope) once hired — distinct from (3) The masking requirement and identity internalization (ε=0.65, Snare with identity_locked exit) for employees who remain undiagnosed. This story focuses on the overall structural constraint and perspectival decomposition. Related stories track the specific mechanisms separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(neurodivergent_workplace_exclusion, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
