% ============================================================================
% CONSTRAINT STORY: graduate_training_monoculture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_graduate_training_monoculture, []).

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
 *   constraint_id: graduate_training_monoculture
 *   human_readable: Graduate Training Monoculture
 *   domain: education/labor/institutional
 *
 * SUMMARY:
 *   The graduate training monoculture is a structural constraint in academic
 *   knowledge production where research-intensive universities have
 *   established a single normative pathway for advanced training: a 5-7 year
 *   doctoral apprenticeship in a disciplinary department, producing
 *   specialized researchers for research-track careers. This pathway
 *   dominates credential allocation, career opportunity access, and prestige
 *   distribution despite evidence that (a) many doctoral graduates pursue
 *   non-research careers for which the training is poorly designed, (b)
 *   alternative training models (industry apprenticeships, bootcamps,
 *   interdisciplinary centers, open-source communities) produce competent
 *   researchers and practitioners with lower suppression and lower theater,
 *   and (c) the gatekeeping apparatus (departmental structures,
 *   subdisciplinary journals, prerequisites) has become increasingly
 *   performative as research problems have become interdisciplinary. The
 *   constraint exhibits Tangled Rope structure: genuine coordination function
 *   (apprenticeship, knowledge transmission, research production) co-exists
 *   with asymmetric extraction (opportunity costs imposed on non-research
 *   pathways, lock-in to narrow fields, credential monopoly held by
 *   research-intensive universities). The theater ratio (0.64) reflects that
 *   substantial portions of graduate training (disciplinary requirements,
 *   specialized courses, departmental seminars) persist through institutional
 *   inertia despite diminished functionality for research production. The
 *   monoculture intensified during the 1970-2020 period (measurements 0-30)
 *   as the theater ratio rose from 0.42 to 0.64 and extractiveness from 0.38
 *   to 0.58, driven by credential inflation, increased specialization
 *   demands, and concentration of research funding in elite institutions.
 *
 * KEY AGENTS:
 *   - Graduate Students: Primary victims (powerless/identity_locked) — identity-fused with research pathway; experience high suppression through 5-7 year opportunity cost, debt, and identity lock
 *   - Non-Research Career Pathways: Secondary victims (moderate/constrained) — benefit from credential value but incur extraction through skill-credential mismatch, early lock-in, and opportunity cost
 *   - Research-Intensive Universities: Primary beneficiaries (institutional/arbitrage) — benefit from low-cost graduate student labor, prestige concentration, and research production; maintain monoculture through accreditation and hiring networks
 *   - Traditional Disciplinary Gatekeepers: Institutional beneficiaries (institutional/arbitrage) — benefit from credential monopoly and gatekeeping authority; maintain theater through departmental structures, journal gatekeeping, and hiring committees
 *   - Innovative Interdisciplinary Training: Victim agent (moderate/constrained) — suppressed by gatekeeping; lacks prestige and funding access compared to traditional programs
 *   - Regional and Teaching Institutions: Victims (powerful/constrained) — excluded from research prestige hierarchies; forced to adopt research-track training despite teaching mission
 *   - Alternative Training Programs: Organized agents (organized/constrained) — emerging scaffold: bootcamps, industry apprenticeships, open-source communities, interdisciplinary centers building alternative pathways with lower extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (long training is inherent to knowledge) as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(graduate_training_monoculture, 0.58).
domain_priors:suppression_score(graduate_training_monoculture, 0.68).
domain_priors:theater_ratio(graduate_training_monoculture, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(graduate_training_monoculture, extractiveness, 0.58).
narrative_ontology:constraint_metric(graduate_training_monoculture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(graduate_training_monoculture, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(graduate_training_monoculture, tangled_rope).
narrative_ontology:human_readable(graduate_training_monoculture, "Graduate Training Monoculture").
narrative_ontology:topic_domain(graduate_training_monoculture, "education/labor/institutional").

domain_priors:requires_active_enforcement(graduate_training_monoculture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(graduate_training_monoculture, incumbent_research_universities).
narrative_ontology:constraint_beneficiary(graduate_training_monoculture, traditional_disciplinary_gatekeepers).
narrative_ontology:constraint_beneficiary(graduate_training_monoculture, research_funding_agencies_with_legacy_portfolios).
narrative_ontology:constraint_victim(graduate_training_monoculture, graduate_students).
narrative_ontology:constraint_victim(graduate_training_monoculture, non_research_track_careers).
narrative_ontology:constraint_victim(graduate_training_monoculture, innovative_interdisciplinary_training).
narrative_ontology:constraint_victim(graduate_training_monoculture, regional_and_teaching_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRADUATE STUDENT (SNARE) — Structurally mobile (could abandon degree program) but identity-fused with the credentialing pathway. Identity as 'future researcher' or 'doctoral scholar' constitutes the agent's self-concept. Exit would require abandoning the professional identity frame, not just the program. High suppression through opportunity cost (5-7 years), debt dependency, and career-path lock-in. No perceived alternative to the traditional research training model.
constraint_indexing:constraint_classification(graduate_training_monoculture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: NON-RESEARCH CAREER PATHWAYS (TANGLED ROPE) — Moderate power agents (industry researchers, policy analysts, teaching-focused scholars, technical specialists) benefit from the legitimacy and credential value that doctoral training provides, but bear extraction costs: (a) training designed for research-track careers creates skill-credential mismatch for non-research roles; (b) excess specialization creates early lock-in to narrow disciplinary niches; (c) career opportunity loss during training years (opportunity cost). Exit is possible but costly — switching to non-research careers after doctoral investment incurs sunk-cost penalty.
constraint_indexing:constraint_classification(graduate_training_monoculture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RESEARCH-INTENSIVE UNIVERSITIES (ROPE) — Primary beneficiaries. The monoculture sustains the research production apparatus: graduate students provide low-cost research labor, mentorship of apprentices reproduces departmental culture, and credential prestige attracts talent and funding. These are genuine coordination functions — training researchers and building knowledge communities. The constraint enables these activities. Exit options are multiple: endowments, federal grants, industry partnerships. The universities experience the monoculture as beneficial coordination.
constraint_indexing:constraint_classification(graduate_training_monoculture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DISCIPLINARY GATEKEEPING (PITON) — The apparatus of disciplinary departments, specialized journals, narrow subdisciplinary identities, and prerequisite courses has become substantially performative. Much of the gatekeeping (what constitutes 'proper' training in physics, history, chemistry) persists through institutional inertia rather than functional necessity. The theater ratio is high because alternative knowledge-production pathways (interdisciplinary centers, industry labs, open-source communities) produce valid research with minimal gatekeeping. Yet departmental structures maintain the ritual — accreditation, hiring committees, journal editors — despite eroded functionality.
constraint_indexing:constraint_classification(graduate_training_monoculture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EMERGING ALTERNATIVE PROGRAMS (SCAFFOLD) — Organized agents (bootcamps, industry apprenticeships, open-source communities, interdisciplinary research collectives, teaching-track doctoral programs) are building parallel training pathways with lower theater and reduced lock-in. These alternatives provide genuine coordination (skill development, credentialing, mentorship) with lower extraction. Sunset logic applies: as alternative pathways mature and employers recognize non-traditional credentials, the monoculture's extraction mechanism weakens. Estimated sunset: 15-25 years as labor market norms shift.
constraint_indexing:constraint_classification(graduate_training_monoculture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ELITE RESEARCHERS / DEPARTMENT LEADERS (TANGLED ROPE) — High-power beneficiaries. These agents benefit substantially from the monoculture (access to graduate student labor, prestige concentration, disciplinary authority). They also experience genuine coordination benefits (training communities, knowledge transmission). However, they bear some extraction costs through administrative overhead, erosion of intellectual autonomy (pressure to train for external metrics), and institutional inflexibility. Exit options are mobile — can move institutions, start interdisciplinary centers, or transition to industry. Experienced extraction is moderate because power enables selective exit.
constraint_indexing:constraint_classification(graduate_training_monoculture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN CANDIDATE) — From a civilizational perspective, complex knowledge systems require long training, apprenticeship structures are inherent to knowledge transmission, and disciplinary specialization is necessary for depth. This framing naturalizes the monoculture as immutable law. However, the structural data contradicts the mountain classification: the beneficiary/victim declarations, the high theater ratio, and the observable emergence of functional alternatives (bootcamps, interdisciplinary centers) all reveal this as a false summit — naturalization of a contingent institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(graduate_training_monoculture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(graduate_training_monoculture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(graduate_training_monoculture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(graduate_training_monoculture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(graduate_training_monoculture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(graduate_training_monoculture, TR),
    TR >= 0.70.

:- end_tests(graduate_training_monoculture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The monoculture extracts through three mechanisms: (1) opportunity cost — 5-7 years of prime working years devoted to apprenticeship with stipend-level compensation; (2) credential monopoly — research-intensive universities control prestige allocation, forcing non-research pathways to adopt research training; (3) lock-in to narrow fields — early specialization creates path dependence into disciplinary niches, reducing career flexibility. These are real extraction mechanisms, not merely coordination costs. However, extraction is not maximal (ε ≈ 0.75) because genuine apprenticeship and knowledge transmission occur — the coordination function is real. Suppression (0.68): High but not total. Barriers to exiting the monoculture include: (a) identity fusion with research pathway (cognitive, portable); (b) credential monopoly — employers and academic institutions recognize only traditional degrees (structural, institution-dependent); (c) opportunity cost and financial dependency (structural, but time-bound); (d) social network concentration in research-track networks (structural, but permeable). Suppression has increased from 0.55 to 0.68 over the measurement interval due to credential inflation and reduced recognition of alternative pathways. Theater ratio (0.64): The monoculture's pedagogy contains substantial theater. Disciplinary prerequisites, journal-club seminars, specialized courses, and departmental requirements persist because they are institutionally required and status-signaling, not because they are necessary for research production. Alternative pathways (bootcamps, open-source communities) produce competent practitioners with minimal such theater. The theater ratio has drifted upward (0.42 → 0.64) as specialization requirements have intensified and as the constraint has become more self-sustaining (theater becomes institutional purpose rather than means to research end).
 *
 * PERSPECTIVAL GAP:
 *   Graduate students experience maximum extraction (snare) due to identity lock + trapped exit. Research universities experience the constraint as pure coordination benefit (rope) with arbitrage exit options. The gap reveals asymmetric power: beneficiaries perceive coordination; victims perceive extraction. Non-research career agents see tangled rope (mixed coordination and extraction) — they benefit from credential value but incur high costs from the training-to-career mismatch. Disciplinary gatekeepers see their own apparatus as degraded (piton) but maintain it through institutional inertia — diagnostic of theater accumulation. Alternative programs see the monoculture as a temporary coordination failure with a structural sunset (scaffold) — they experience constrained exit but see the constraint weakening as alternative credentials gain recognition. Elite researchers see tangled rope with mobile exit — they benefit from the system but constrain it through administrative overhead and intellectual inflexibility. The analytical observer risks the false summit: naturalizing this as 'knowledge transmission requires long training' rather than recognizing it as a contingent institutional structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derived from beneficiary/victim status and exit options. Graduate students: beneficiary status NO, victim status YES, exit identity_locked → high d (≈0.89) → high f(d) ≈ 1.28 → high experienced extraction. Research-intensive universities: beneficiary status YES, victim status NO, exit arbitrage → low d (≈0.05) → negative f(d) ≈ -0.12 → negative chi (constraint subsidizes these agents). Non-research pathways: beneficiary status MIXED (credential value) + victim status YES (skill-credential mismatch), exit constrained → moderate-high d (≈0.70) → high f(d) ≈ 1.10 → moderate-high chi. Elite researchers: beneficiary status YES, victim status PARTIAL (administrative burden, intellectual constraint), exit mobile → moderate d (≈0.50) → moderate f(d) ≈ 0.65 → moderate chi. The directionality chain reveals the asymmetry: power concentration among beneficiaries (research universities control credential and prestige allocation) combined with trapped or identity-locked exit options for victims (students, non-research pathways) produces high effective extraction despite moderate base extraction. Alternative programs with constrained exit still provide lower extraction than traditional programs because exit options, though costly, are present.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is partially unresolved (mandatrophy_resolved: false) because the primary ambiguity is whether the monoculture is a genuine coordination mechanism (justifying extraction as apprenticeship cost) or an extractive apparatus maintaining theater under the guise of training. The omegas address this: (1) identity_lock_persistence determines whether suppression is portable (internalized) or structural; (2) alternative_credential_recognition determines whether the scaffold is real or aspirational; (3) disciplinary_gatekeeping_functionality determines whether piton classification is correct (theater persists with eroded function); (4) research_capacity_versus_training_burden determines whether extractiveness should be higher (theater disguised as training) or justified as apprenticeship cost; (5) interdisciplinary_penalty determines whether the monoculture is intensifying or weakening. Resolution of these omegas would enable precise reclassification. Current state: tangled_rope is the most defensible type (genuine coordination + asymmetric extraction coexist), but the theater ratio (0.64) suggests creeping pitonization — if theater continues to rise while coordination function erodes, reclassification to snare becomes warranted. The constraint is at a decision point: alternative pathways are maturing (scaffold perspective gains strength), but the monoculture is intensifying its gatekeeping (theater rising, suppression rising). The outcome depends on labor market acceptance of alternative credentials and employer recognition — factors addressed by omega 2.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_persistence,
    'How much of the graduate student''s inability to exit stems from material barriers versus internalized identity fusion with the research pathway?',
    'Longitudinal study of career trajectories after program exit: do students who leave experience reduced suppression immediately (structural barrier model) or do they carry suppression costs related to abandoned identity (internalized model)? Analysis of dropout vs completion populations and post-program regret patterns.',
    'If primarily identity-locked: the classification remains snare, but the suppression is portable (exit would partially resolve it). If primarily trapped: suppression persists regardless of exit pathway. If mixed: omega resolution determines the weighting between structural and cognitive mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Identity lock versus material barrier in graduate student suppression').

omega_variable(
    alternative_credential_recognition,
    'To what extent do employers and academic institutions recognize and value non-traditional research credentials (bootcamp certificates, industry research experience, open-source contributions) as equivalent to doctoral degrees?',
    'Labor market analysis: hiring outcomes, salary parity, and career progression for traditional PhD vs alternative pathway credentials across 10-year timeframes in multiple sectors. Employer survey data on credential recognition and hiring preferences.',
    'If high recognition: scaffold sunset is real and accelerating — alternative pathways become viable exits. If low recognition: scaffold classification is aspirational rather than structural — the alternative pathways do not yet provide genuine exit capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credential_recognition, empirical, 'Employer recognition of non-traditional research credentials').

omega_variable(
    disciplinary_gatekeeping_functionality,
    'Does traditional disciplinary training produce superior research outcomes, broader understanding, or more innovative thinking compared to alternative pathways, or has the functionality eroded while the gatekeeping persists?',
    'Comparative citation analysis, innovation metrics, and interdisciplinary impact assessment for research produced by traditionally trained versus alternative-pathway researchers. Assessment of correlation between disciplinary depth and research contribution in current knowledge production.',
    'If disciplinary training produces superior outcomes: piton classification is incorrect — the gatekeeping has real function. If comparable or worse outcomes: piton classification confirmed — theater ratio is high and functionality has eroded, leaving inertial institutional structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disciplinary_gatekeeping_functionality, empirical, 'Functionality of traditional disciplinary gatekeeping in research outcomes').

omega_variable(
    research_capacity_versus_training_burden,
    'What proportion of graduate student time in traditional programs is dedicated to research production versus training/apprenticeship versus pedagogical labor (teaching), and how does this ratio compare to alternative pathways?',
    'Time-use analysis: detailed logs of graduate student activities across semesters in traditional programs versus bootcamps, industry labs, and open-source communities. Analysis of research output per unit of training time invested.',
    'If traditional programs have high training-to-research ratio: the suppression is partly justified as apprenticeship cost. If low ratio with high time burden: the suppression reflects extraction disguised as training. This affects whether extractiveness should be reduced (training justified) or increased (theater disguised as apprenticeship).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_capacity_versus_training_burden, empirical, 'Research capacity to training burden ratio in traditional versus alternative programs').

omega_variable(
    interdisciplinary_penalty,
    'Do interdisciplinary research trajectories suffer career disadvantage (fewer publications, lower prestige, reduced funding) compared to disciplinarily narrow trajectories, or is the disadvantage declining as research problems become increasingly interdisciplinary?',
    'Career trajectory analysis: funding success rates, publication metrics, and hiring outcomes for interdisciplinary versus disciplinary researchers across 10-year cohorts. Analysis of whether the penalty has diminished as problem landscapes shifted.',
    'If penalty persists and is increasing: monoculture extraction is growing (lock-in to narrow fields becomes more costly). If penalty is declining: alternative training pathways become more viable and scaffold sunset accelerates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interdisciplinary_penalty, empirical, 'Career penalty for interdisciplinary research trajectories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(graduate_training_monoculture, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gtm_tr_t0, graduate_training_monoculture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gtm_tr_t15, graduate_training_monoculture, theater_ratio, 15, 0.55).
narrative_ontology:measurement(gtm_tr_t30, graduate_training_monoculture, theater_ratio, 30, 0.64).
narrative_ontology:measurement(gtm_tr_t5, graduate_training_monoculture, theater_ratio, 5, 0.48).
narrative_ontology:measurement(gtm_tr_t10, graduate_training_monoculture, theater_ratio, 10, 0.51).
narrative_ontology:measurement(gtm_tr_t20, graduate_training_monoculture, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(gtm_be_t0, graduate_training_monoculture, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gtm_be_t15, graduate_training_monoculture, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(gtm_be_t30, graduate_training_monoculture, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(gtm_be_t5, graduate_training_monoculture, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(gtm_be_t10, graduate_training_monoculture, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(gtm_be_t20, graduate_training_monoculture, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(graduate_training_monoculture, resource_allocation).
narrative_ontology:affects_constraint(graduate_training_monoculture, academic_prestige_concentration).
narrative_ontology:affects_constraint(graduate_training_monoculture, research_funding_geographic_inequality).
narrative_ontology:affects_constraint(graduate_training_monoculture, disciplinary_knowledge_silos).
narrative_ontology:affects_constraint(graduate_training_monoculture, early_career_researcher_precarity).

% DUAL FORMULATION NOTE:
% Graduate training monoculture is structurally upstream of multiple downstream constraints: prestige concentration (monoculture concentrates prestige in research-intensive universities, which cascades into downstream prestige hierarchies), funding inequality (research funding agencies weight institutional prestige derived from research training capacity), disciplinary silos (monoculture enforces narrow specialization, preventing interdisciplinary knowledge integration), and early-career precarity (the 5-7 year training period delays career establishment and increases financial vulnerability). Each downstream constraint has its own ε value reflecting the specific mechanism. Network links enable contamination propagation: degradation of the monoculture (e.g., through alternative credential recognition) would reduce prestige concentration, which would weaken downstream funding inequality. Conversely, intensification of the monoculture (through increased gatekeeping) would amplify all downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(graduate_training_monoculture, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
