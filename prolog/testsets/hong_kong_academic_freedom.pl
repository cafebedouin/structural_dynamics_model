% ============================================================================
% CONSTRAINT STORY: hong_kong_academic_freedom
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hong_kong_academic_freedom, []).

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
 *   constraint_id: hong_kong_academic_freedom
 *   human_readable: Academic Freedom Constraint in Hong Kong Higher Education
 *   domain: political_economy/higher_education
 *
 * SUMMARY:
 *   Hong Kong's academic freedom constraint represents political extraction
 *   disguised as institutional governance. Since 2019 and accelerating
 *   post-2020, the constraint operates through multiple simultaneous
 *   mechanisms: national security legislation making research and teaching on
 *   sensitive topics legally risky, institutional vetting of faculty hiring
 *   and curriculum, funding allocation tied to political conformity, and
 *   surveillance of student activism. The constraint exhibits high
 *   suppression (0.75) through visa revocation threats, institutional
 *   sanctions, and legal liability. Extractiveness is high (0.68) because the
 *   extraction target is research autonomy and knowledge production on
 *   politically sensitive topics — topics that pose existential challenge to
 *   state authority. Theater ratio (0.58) reflects that universities maintain
 *   performative academic prestige (publishing, rankings, international
 *   reputation) while actual intellectual autonomy has been substantially
 *   constrained. The constraint is structurally a Snare from the perspectives
 *   of trapped researchers and powerless students, a Rope from complicit
 *   university administration, a Tangled Rope from the Hong Kong Executive
 *   (mixed governance and extraction), and a degraded Piton from the
 *   international academic community (which maintains nominal engagement
 *   while Hong Kong autonomy atrophies).
 *
 * KEY AGENTS:
 *   - Academic Researchers: Primary victims (powerless/trapped) — face career destruction and legal liability for sensitive research; self-censor to avoid institutional and state scrutiny
 *   - Graduate Students: Secondary victims (powerless/constrained) — thesis rejection and funding withdrawal threats; exit technically available but costly
 *   - University Administration: Primary beneficiaries (institutional/arbitrage) — experience constraint as coordination mechanism protecting institutional stability; maintain research prestige while ensuring political compliance
 *   - Hong Kong Executive: Institutional extractor (institutional/constrained) — enforces political control extraction through policy implementation; constrained by Beijing alignment requirements
 *   - Chinese Government: Ultimate beneficiary (institutional/arbitrage) — political monopoly on knowledge production regarding sensitive topics secured; maintains Hong Kong institutional prestige as soft power asset
 *   - International Academic Community: Performative observer (institutional/arbitrage) — maintains nominal engagement standards while Hong Kong institutional autonomy degrades
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hong_kong_academic_freedom, 0.68).
domain_priors:suppression_score(hong_kong_academic_freedom, 0.75).
domain_priors:theater_ratio(hong_kong_academic_freedom, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hong_kong_academic_freedom, extractiveness, 0.68).
narrative_ontology:constraint_metric(hong_kong_academic_freedom, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hong_kong_academic_freedom, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hong_kong_academic_freedom, snare).
narrative_ontology:human_readable(hong_kong_academic_freedom, "Academic Freedom Constraint in Hong Kong Higher Education").
narrative_ontology:topic_domain(hong_kong_academic_freedom, "political_economy/higher_education").

domain_priors:requires_active_enforcement(hong_kong_academic_freedom).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hong_kong_academic_freedom, chinese_government).
narrative_ontology:constraint_beneficiary(hong_kong_academic_freedom, hong_kong_executive).
narrative_ontology:constraint_victim(hong_kong_academic_freedom, academic_researchers).
narrative_ontology:constraint_victim(hong_kong_academic_freedom, students).
narrative_ontology:constraint_victim(hong_kong_academic_freedom, research_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACADEMIC RESEARCHER (SNARE) — Faculty members face career destruction, visa revocation, institutional sanctions, and potential legal liability for research or teaching deemed politically sensitive. No meaningful exit options within Hong Kong's institutional ecosystem. Research topics must be self-censored to avoid administrative scrutiny. Maximum extraction experienced by trapped agents with no alternative livelihood.
constraint_indexing:constraint_classification(hong_kong_academic_freedom, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GRADUATE STUDENT (SNARE) — Students pursuing sensitive research face thesis rejection, funding withdrawal, and surveillance. Exit is costly (relocation, program abandonment) but technically possible. However, the threat of institutional retaliation and political investigation creates effective entrapment. Research direction must conform to institutional safety guidelines. High extraction with asymmetric power imbalance.
constraint_indexing:constraint_classification(hong_kong_academic_freedom, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNIVERSITY ADMINISTRATION (ROPE) — Institutional leaders experience the constraint as coordination mechanism protecting institutional stability and funding. Viewed from administration, the constraint enables universities to operate within political constraints while maintaining research prestige (selective tolerance). Net beneficiary position with arbitrage options (compliance ensures autonomy in non-sensitive domains). Perceives constraint as necessary functional governance.
constraint_indexing:constraint_classification(hong_kong_academic_freedom, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: HONG KONG EXECUTIVE (TANGLED ROPE) — Government officials experience dual function: genuine coordination of education policy alongside political control extraction. The constraint serves both public governance (allocating research resources, setting academic standards) and political extraction (suppressing autonomous knowledge production that challenges state authority). Mixed beneficiary/enforcer position with constrained exit due to Beijing alignment requirements. Active enforcement of loyalty vetting and curriculum alignment.
constraint_indexing:constraint_classification(hong_kong_academic_freedom, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL ACADEMIC COMMUNITY (PITON) — Global academia maintains performative engagement with Hong Kong institutions (citing papers, collaborative protocols, rankings) while the institutions' actual autonomy has degraded. International standards for academic freedom remain nominally maintained; enforcement mechanisms are theatrical. Structural function (cross-border knowledge exchange) persists through ritual; actual independence has atrophied.
constraint_indexing:constraint_classification(hong_kong_academic_freedom, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a globalcivilizational perspective, Hong Kong's academic constraint is a snare disguised as institutional governance. The structure extracts intellectual autonomy, suppresses knowledge production on politically sensitive topics, and uses institutional legitimacy (universities continue to operate, rank, publish) to obscure the extraction mechanism. The analytical position reveals the asymmetry: researchers trapped, administrators complicit, government extracting political monopoly, international community performing engagement.
constraint_indexing:constraint_classification(hong_kong_academic_freedom, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hong_kong_academic_freedom_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hong_kong_academic_freedom, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hong_kong_academic_freedom, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hong_kong_academic_freedom, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hong_kong_academic_freedom, TR),
    TR >= 0.70.

:- end_tests(hong_kong_academic_freedom_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint's primary extraction target is research autonomy on politically sensitive topics (particularly Hong Kong identity, democratic governance, human rights, and comparative political systems). The measurement trajectory (0.32 → 0.52 → 0.68 over six years) reflects cumulative tightening through national security legislation (2020), institutional compliance mechanisms, and normalization of political screening. The extraction is not primarily financial but epistemic — control over knowledge production. Suppression (0.75): High. Multiple mechanisms create effective barriers to exit: visa threat for non-citizens, institutional sanctions including dismissal, legal liability under national security law, chilling effect on research funding, and social stigma. The suppression is both structural (external legal/administrative barriers) and internalized (researchers internalize risk assessment and pre-censor). Theater ratio (0.58): Moderate-high and increasing. Universities maintain academic legitimacy narratives (teaching excellence, research productivity, international collaboration) while actual autonomy constraints are obscured. The theater has increased as explicit policy constraints have shifted toward administrative implementation and institutional self-enforcement, making suppression less visible but more pervasive. International institutional rankings and publications perpetuate the fiction that Hong Kong universities operate under normal academic conditions.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Trapped researchers perceive it as extraction (Snare) — they are targets experiencing suppression and career risk. University administrators perceive it as functional coordination (Rope) — managing institutional stability and funding within political constraints while preserving selective autonomy. The Hong Kong Executive perceives mixed functions (Tangled Rope) — genuine education policy coordination alongside political extraction of knowledge monopoly control. The international academic community perceives theatrical engagement (Piton) — maintaining nominal academic standards while the constraint's actual effect atrophies Hong Kong institutional autonomy. The analytical observer perceives the integrated extraction mechanism (Snare) — institutional legitimacy deployed to obscure political suppression. The gap reveals how institutional power enables beneficiaries to frame extraction as coordination, while trapped agents experience the same structure as pure suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows structural relationships to the constraint's extraction target (research autonomy). Researchers experience maximum d ≈ 0.95 (full targets): trapped exit, victim status, structurally dependent on institutional affiliation. University administrators experience low d ≈ 0.20 (beneficiaries with arbitrage): arbitrage exit (can shift non-sensitive research focus, access government resources), beneficiary status (institutional funding and political protection). Hong Kong Executive experiences moderate d ≈ 0.45 (mixed): constrained exit (Beijing alignment non-negotiable), mixed beneficiary/enforcer role. International academic community experiences d ≈ 0.25 (beneficiaries with arbitrage): arbitrage exit (disengage from Hong Kong institutions if autonomy loss becomes untenable), beneficiary through nominal engagement maintained. The analytical observer experiences d ≈ 0.72 (structural analyst): analytical exit (can identify but not escape the constraint's logic from within institutional systems).
 *
 * MANDATROPHY ANALYSIS:
 *   POLITICAL EXTRACTION CONCEALMENT: The mandatrophy is resolved by recognizing that the constraint achieves political extraction while maintaining institutional legitimacy narrative. The problem it solves is political (securing knowledge production autonomy for state authorities); the solution it deploys is institutional (using university governance structures for political screening). This disguises pure extraction (Snare classification) as mixed governance. The analytical observer's snare classification is consistent with the powerless researcher's snare classification — both reveal that the constraint's primary function is extraction, not coordination. University administrators' rope perception reflects their structural position as beneficiaries with arbitrage options. The Hong Kong Executive's tangled rope reflects genuine mixed function (some real education policy + political extraction), distinguishing institutional enforcers from pure beneficiaries. The resolution: classify at multiple levels — institutional extraction at the administration/executive level (tangled rope governance), epistemic extraction at the researcher level (snare), and performative legitimacy at the international level (piton). No single type captures all dimensions; the presheaf over observation positions reveals the integrated extraction-legitimacy mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_censorship_boundary,
    'What proportion of observed topic avoidance reflects genuine institutional policy vs internalized anticipatory self-censorship by researchers?',
    'Longitudinal studies tracking research topic choices before/after explicit policy changes; anonymous surveys of researcher risk perception vs actual enforcement incidents; comparison of sensitive topic publication rates in pre-2020 vs post-2020 cohorts',
    'If primarily institutional policy: suppression metric should increase, extraction may be lower (external constraint). If primarily self-censorship: suppression is partially internalized, effective extraction higher than structural metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_censorship_boundary, empirical, 'Distinguishing explicit policy enforcement from internalized self-censorship').

omega_variable(
    research_funding_substitution,
    'Are researchers experiencing reduced funding access due to topic censorship, or are alternative funding sources (international grants, private foundations) compensating for political constraints?',
    'Analysis of Hong Kong research funding allocation by topic domain; tracking of international funding uptake by Hong Kong institutions; comparison with pre-2019 funding patterns',
    'If compensated: extraction is lower than suppression suggests (financial exit available). If not: extractiveness at suppression level, compound economic + political pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_funding_substitution, empirical, 'Whether alternative funding sources offset political constraints on research').

omega_variable(
    institutional_autonomy_measurement,
    'Does the distinction between ''Hong Kong Executive'' and ''Chinese Government'' represent two separate enforcement vectors or a single unified constraint?',
    'Analysis of policy origin and enforcement authority: Beijing directive vs Hong Kong SAR implementation; tracking of policy divergence or alignment over time; institutional interviews on authority hierarchy',
    'If unified: perspective simplification, single beneficiary (Chinese state apparatus). If separate: Hong Kong Executive has constrained exit (must align with Beijing), generating its own Tangled Rope dynamics at institutional level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_autonomy_measurement, empirical, 'Whether Hong Kong Executive autonomy is genuine or performative').

omega_variable(
    international_publication_suppression,
    'Are Hong Kong researchers able to publish sensitive research via international channels while maintaining institutional affiliations, or does institutional reputation damage prevent this exit route?',
    'Tracking of publication patterns by topic and venue; institutional response to off-shore publications; career outcomes for researchers publishing sensitive work internationally',
    'If exit route viable: exit_options upgrade to ''constrained'' rather than ''trapped'' for some researchers. If route closed: suppression remains total, extractiveness at high end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_publication_suppression, empirical, 'Whether international publication provides viable exit route for academic researchers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hong_kong_academic_freedom, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hong_tr_t0, hong_kong_academic_freedom, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hong_tr_t3, hong_kong_academic_freedom, theater_ratio, 3, 0.5).
narrative_ontology:measurement(hong_tr_t6, hong_kong_academic_freedom, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(hong_be_t0, hong_kong_academic_freedom, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hong_be_t3, hong_kong_academic_freedom, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(hong_be_t6, hong_kong_academic_freedom, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hong_kong_academic_freedom, enforcement_mechanism).
narrative_ontology:affects_constraint(hong_kong_academic_freedom, chinese_political_orthodoxy_in_universities).
narrative_ontology:affects_constraint(hong_kong_academic_freedom, hong_kong_civil_society_suppression).

% DUAL FORMULATION NOTE:
% Academic freedom constraint is structurally downstream of Chinese national security legislation but distinct in its enforcement mechanisms through institutional governance. Related to broader civil society suppression but operates through knowledge production channels specifically. Linked to political orthodoxy curriculum requirements as shared beneficiary (state political control).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hong_kong_academic_freedom, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
