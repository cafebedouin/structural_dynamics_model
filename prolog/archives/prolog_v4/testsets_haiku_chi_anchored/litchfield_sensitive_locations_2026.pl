% ============================================================================
% CONSTRAINT STORY: litchfield_sensitive_locations_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_litchfield_sensitive_locations_2026, []).

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
 *   constraint_id: litchfield_sensitive_locations_2026
 *   human_readable: Litchfield School Perimeter Crisis
 *   domain: political/social
 *
 * SUMMARY:
 *   The Litchfield School Perimeter Crisis represents a structural conflict
 *   between federal law enforcement jurisdiction and school community
 *   autonomy. On February 5, 2026, federal agents were spotted operating
 *   within a block of a school facility, establishing an investigative
 *   presence in a sensitive location where children, educators, and families
 *   congregate. The constraint emerges from the asymmetry between federal
 *   enforcement authority (institutional capacity to deploy near schools) and
 *   school community voice (powerless to challenge or negotiate placement).
 *   The crisis is classified as a Snare from most perspectives because it
 *   exhibits high suppression (school community has no viable exit options,
 *   no consultation mechanism, no formal appeals process) and extractive
 *   characteristics (operational burden, psychological externality,
 *   educational disruption imposed without consent). From the federal
 *   enforcement perspective, the constraint appears as Rope — a coordination
 *   mechanism for legitimate law enforcement. The theater ratio (0.65)
 *   reflects that much of the operational response to federal proximity is
 *   performative: school hardening measures, security protocols, and public
 *   communication about enforcement presence exceed what threat assessment
 *   would justify, indicating institutional theater masking the underlying
 *   extraction dynamic.
 *
 * KEY AGENTS:
 *   - School Students: Primary victims (powerless/trapped) — experience disruption, stress, and loss of educational normalcy without agency
 *   - School Staff and Administration: Secondary victims (moderate/constrained) — bear operational burden of managing federal presence while maintaining educational mission
 *   - School Families: Primary victims (powerless/trapped) — experience anxiety, disrupted schedules, and inability to influence federal operations
 *   - Federal Enforcement Agencies: Beneficiary (institutional/arbitrage) — obtain investigative proximity required for enforcement mandate; experience constraint as coordination mechanism
 *   - Civil Liberties and Education Advocacy Coalition: Organized responders (organized/constrained) — mobilize to constrain extraction through litigation, policy advocacy, and community organizing
 *   - Federal School Protection Policy Framework: Degraded institutional structure (institutional/arbitrage) — prescribed coordination protocols have become performative ritual
 *   - Analytical Observer: Universal rights perspective (analytical/analytical) — identifies structural subordination of school community to federal enforcement discretion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(litchfield_sensitive_locations_2026, 0.52).
domain_priors:suppression_score(litchfield_sensitive_locations_2026, 0.68).
domain_priors:theater_ratio(litchfield_sensitive_locations_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(litchfield_sensitive_locations_2026, snare).
narrative_ontology:human_readable(litchfield_sensitive_locations_2026, "Litchfield School Perimeter Crisis").
narrative_ontology:topic_domain(litchfield_sensitive_locations_2026, "political/social").

% --- Structural relationships ---
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, school_students).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, school_staff).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, school_families).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, educational_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCHOOL COMMUNITY (SNARE) — Trapped by federal enforcement proximity without control over the federal presence. Cannot exit the geographic constraint or modify federal operations. Bears full extraction cost: disrupted instruction, psychological stress, operational burden of security measures. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.57.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SCHOOL ADMINISTRATION (SNARE) — Constrained choice: must manage federal enforcement presence while maintaining educational operations. Cannot refuse federal operations; exit options limited to procedural accommodation or relocation (costly, disruptive). Bears extraction through operational burden and reputational risk. d≈0.78, f(d)≈1.08, σ=0.8 → χ≈0.45.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: FEDERAL ENFORCEMENT AGENCIES (ROPE) — Primary beneficiary of enforcement proximity. Coordination function: establish investigative presence near known activity areas. Experiences constraint as operational coordination mechanism: proximity enables execution of federal mandates. d≈0.15, f(d)≈0.05, σ=0.9 → χ≈0.02. Net beneficiary; low effective extraction from their perspective.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CIVIL LIBERTIES AND EDUCATION ADVOCACY COALITION (TANGLED ROPE) — Organized agents see mixed dynamics: genuine coordination function (legitimate law enforcement) overlaid with asymmetric extraction (school disruption). Coalition has agency to organize, litigate, and seek policy changes, but faces institutional barriers. Benefit from coordination (rule of law); bear costs of application (school impacts). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL SCHOOL PROTECTION POLICY (PITON) — Institutional framework for balancing law enforcement and school safety has degraded. Prescribed protocols (coordination function) are increasingly performative: school perimeter restrictions, facility hardening, and inter-agency coordination exist as ritual compliance rather than demonstrable protective mechanisms. theater_ratio=0.65 reflects performative elements (visible enforcement presence, public communication) exceeding functional protection. d≈0.20, f(d)≈0.12, σ=1.0 → χ≈0.06.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL RIGHTS FRAMEWORK (SNARE) — From a universal rights perspective, the constraint reveals structural subordination: school communities have no meaningful input into federal enforcement decisions affecting their immediate environment. The framework classifies this as snare because suppression is high (no exit options, no coordination mechanism, no appeals process visible to school community) and extraction is manifest (psychological externality, operational burden, educational disruption). d≈0.88, f(d)≈1.30, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(litchfield_sensitive_locations_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(litchfield_sensitive_locations_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(litchfield_sensitive_locations_2026, TR),
    TR >= 0.70.

:- end_tests(litchfield_sensitive_locations_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The federal enforcement presence imposes measurable costs on the school community (operational burden, psychological stress, educational disruption) without proportional benefit or consultation. The extractiveness is not maximal (0.66+) because the federal mandate may reflect legitimate law enforcement need; however, the lack of school community input and the absence of visible alternative locations suggest the extraction is substantive rather than incidental. The measurement trajectory shows extractiveness increasing from 0.35 to 0.52 over 10 time periods, indicating that initial federal presence (framed as temporary) has accumulated into sustained imposition. Suppression (0.68): High. The school community faces multiple layers of suppression: (1) no formal mechanism to challenge federal enforcement proximity, (2) geographic constraint — the school cannot relocate easily, (3) institutional constraint — federal authority supersedes school governance, (4) informational asymmetry — federal mandate scope and duration are not publicly disclosed. Theater ratio (0.65): Moderate-high. The institutional response to federal presence includes performative security measures that exceed threat mitigation: visible enforcement staging, heightened perimeter controls, and public communication about protection efforts. These reflect institutional theater — signaling security to parents and public — rather than proportional operational response. The trajectory shows theater increasing from 0.45 to 0.65, indicating mission creep from coordination (enforcement proximity) to performative security (institutional visibility).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a sharp perspectival gap between federal enforcement and school community. Federal agencies classify the constraint as Rope (coordination mechanism for legitimate law enforcement); they experience the school proximity as operationally efficient. School community classifies the constraint as Snare (extraction without consent); they experience it as imposition and threat. The analytical observer, using a structural rights framework, also classifies as Snare — the absence of school community input into federal decisions affecting their immediate environment is a form of structural subordination. The civil liberties coalition bridges the gap by providing organized voice (Tangled Rope perspective) — they acknowledge legitimate coordination functions while organizing to constrain extraction. The federal policy framework itself appears as Piton — the prescribed protocols for school-sensitive enforcement exist as performative ritual rather than functionally protective mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   School students, staff, families: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit options, no consultation mechanism, no appeal process. Federal enforcement agencies: Beneficiary + arbitrage → d≈0.15, f(d)≈0.05. Net beneficiary. High institutional capacity to exit (they choose the deployment location). Civil liberties coalition: Victim + constrained → d≈0.55, f(d)≈0.75. Organized resistance but institutional barriers. Federal policy framework: Institutional + arbitrage → d≈0.20, f(d)≈0.12. Piton classification comes from theater gate and institutional inertia, not high chi. Analytical observer: analytical → d≈0.88, f(d)≈1.30. Structural subordination from universal rights perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED: The constraint passes all snare gates (extractiveness 0.52 ≥ 0.46, suppression 0.68 ≥ 0.60, potential χ ≈ 0.68 ≥ 0.66 at analytical observer perspective). The mandatrophy is resolved by differentiating the federal enforcement perspective (Rope, coordination) from the school community perspective (Snare, extraction). The federal perspective legitimizes the coordination function (law enforcement mandate); the school community perspective reveals the extraction mechanism (imposition without consent). The constraint is snare because suppression is high (no exit, no consultation, no appeals) and extraction is manifest (disruption costs imposed unilaterally). The federal perspective does not override the snare classification; rather, it establishes that the coordination function exists but is not being shared with the most affected party (the school community). This is the defining feature of snares: the beneficiary perceives coordination; the victim perceives extraction. Mandatrophy is resolved by making the perspectival gap explicit: the federal constraint-as-coordination depends on the school's constraint-as-extraction. The two perspectives are structurally coupled but directionally opposite.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_enforcement_necessity,
    'Does the federal enforcement presence near Litchfield School represent a legitimate investigative requirement or speculative jurisdiction expansion?',
    'Public disclosure of federal enforcement mandate, threat assessment, and investigative scope relative to school proximity. Comparison with precedent cases of school-proximate federal operations.',
    'If legitimate necessity: constraint is snare with partial justification (extraction with structural rationale). If speculative expansion: constraint is pure snare with no coordination benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_enforcement_necessity, empirical, 'Whether federal enforcement proximity reflects genuine investigative necessity').

omega_variable(
    school_community_notice_and_process,
    'Did the school community receive advance notice and meaningful opportunity to challenge or negotiate federal enforcement proximity?',
    'Documentation of notice timelines, consultation procedures, formal objection processes. Comparison with federal protocols for sensitive-location enforcement.',
    'If notice and process existed: constraint becomes tangled rope (extraction with coordination function). If absent: constraint remains snare (extraction without consent mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(school_community_notice_and_process, empirical, 'Whether notice and consultation processes preceded federal enforcement presence').

omega_variable(
    enforcement_perimeter_externality_magnitude,
    'What is the quantifiable educational and psychological cost to the school community from federal enforcement proximity?',
    'Longitudinal tracking of student attendance, behavioral disruption, staff turnover, and psychological assessment data pre- and post-federal presence. Comparison with control schools without enforcement proximity.',
    'If costs exceed legitimate law enforcement benefits: justifies higher extraction classification. If costs are minimal: constraint may be reevaluated as coordination rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_perimeter_externality_magnitude, empirical, 'Quantified educational and psychological externalities from enforcement proximity').

omega_variable(
    alternative_enforcement_modalities,
    'Are there operationally feasible alternative enforcement approaches that achieve federal mandates with lower school community disruption?',
    'Technical analysis of federal enforcement objectives relative to geolocation options. Case studies of functionally equivalent operations with lower sensitive-location proximity.',
    'If alternatives exist: suppression metrics increase (artificial maintenance of school proximity despite feasible alternatives). If no alternatives: suppression reflects operational necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_enforcement_modalities, empirical, 'Availability of operationally feasible alternative enforcement approaches').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(litchfield_sensitive_locations_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litch_tr_t0, litchfield_sensitive_locations_2026, theater_ratio, 0, 0.45).
narrative_ontology:measurement(litch_tr_t5, litchfield_sensitive_locations_2026, theater_ratio, 5, 0.58).
narrative_ontology:measurement(litch_tr_t10, litchfield_sensitive_locations_2026, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(litch_be_t0, litchfield_sensitive_locations_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(litch_be_t5, litchfield_sensitive_locations_2026, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(litch_be_t10, litchfield_sensitive_locations_2026, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(litchfield_sensitive_locations_2026, sensitive_location_federal_jurisdiction).
narrative_ontology:affects_constraint(litchfield_sensitive_locations_2026, school_hardening_security_theater).
narrative_ontology:affects_constraint(litchfield_sensitive_locations_2026, family_enrollment_disruption).

% DUAL FORMULATION NOTE:
% The Litchfield perimeter crisis is downstream of broader federal sensitive-location enforcement policy and upstream of school-specific security theater and enrollment disruption. The constraint has ε=0.52 reflecting the specific imposition on this school community; the parent constraint (sensitive-location federal jurisdiction) has lower ε reflecting the general policy; the child constraints (school hardening, enrollment impacts) have higher ε reflecting accumulated extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(litchfield_sensitive_locations_2026, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
