% ============================================================================
% CONSTRAINT STORY: lakota_education_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lakota_education_access, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lakota_education_access
 *   human_readable: Lakota Education Access: Coordination and Extraction
 *   domain: indigenous_education/structural_inequality
 *
 * SUMMARY:
 *   Lakota education access represents a structural constraint operating at
 *   the intersection of geographic isolation, historical colonization,
 *   institutional dependency, and ongoing federal control. The constraint
 *   demonstrates how what appears as coordination (federal standardized
 *   education serving otherwise isolated communities) simultaneously
 *   functions as extraction mechanism (control of curriculum, language
 *   policy, cultural representation, and resource allocation). The
 *   theater_ratio (0.65) reflects that federal education programs perform
 *   commitment to Indian education through consultation processes, curriculum
 *   units, and policy statements while maintaining substantial
 *   assimilationist structural mechanisms. The constraint's evolution shows
 *   increasing extractiveness (0.42 → 0.58) as performative gestures
 *   substitute for substantive resource commitment and as indigenous
 *   education activism simultaneously raises visibility and articulates the
 *   depth of extraction mechanisms. The measurement interval spans roughly
 *   100 years (Carlisle boarding school closure era through contemporary
 *   tribal education movements), capturing the shift from explicit
 *   assimilation to implicit structural extraction through standardized
 *   coordination.
 *
 * KEY AGENTS:
 *   - Lakota Students: Primary victim (powerless/trapped) — bear costs of geographic isolation, resource scarcity, curriculum misalignment, and language subordination with minimal exit options within the system
 *   - Lakota Communities: Secondary victim (moderate/constrained) — structurally dependent on federal education funding while maintaining aspiration to cultural transmission and community control
 *   - Federal Education Administration: Primary beneficiary (institutional/arbitrage) — benefits from institutional legitimacy, budgetary control, and standardization that reduces coordination burden; high exit optionality to modify policy
 *   - Tribal Education Advocates: Organized agents (organized/constrained) — operate within constrained resource and regulatory environment; benefits from increased visibility and partial autonomy achievements but limited by continuing institutional barriers
 *   - BIA Historical Legacy: Institutional actor (institutional/arbitrage) — benefits from persistent bureaucratic structures, policy continuity, and performative commitment to Indian education
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-contingent inequality as inevitable feature of hierarchical societies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lakota_education_access, 0.58).
domain_priors:suppression_score(lakota_education_access, 0.72).
domain_priors:theater_ratio(lakota_education_access, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lakota_education_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(lakota_education_access, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lakota_education_access, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lakota_education_access, tangled_rope).
narrative_ontology:human_readable(lakota_education_access, "Lakota Education Access: Coordination and Extraction").
narrative_ontology:topic_domain(lakota_education_access, "indigenous_education/structural_inequality").

domain_priors:requires_active_enforcement(lakota_education_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lakota_education_access, federal_education_bureaucracy).
narrative_ontology:constraint_beneficiary(lakota_education_access, non_native_institutional_interests).
narrative_ontology:constraint_victim(lakota_education_access, lakota_students).
narrative_ontology:constraint_victim(lakota_education_access, lakota_communities).
narrative_ontology:constraint_victim(lakota_education_access, indigenous_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAKOTA STUDENT (SNARE) — Trapped by geographic isolation on reservations, poverty, underfunded schools, and language barriers. No meaningful exit options within the system; structural dependency on federal education apparatus. Bears full cost of extraction through constrained opportunity pathways, educational debt, and cultural erasure. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(lakota_education_access, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LAKOTA COMMUNITY (TANGLED ROPE) — Constrained by resource limitations, institutional dependency, and policy barriers, but also benefits from federally-funded education programs, resource coordination, and institutional infrastructure. Genuine coordination function (school systems coordinate community learning) exists alongside asymmetric extraction (control of curriculum, language policy, resource allocation). Moderate cost but also tangible benefit creates mixed experience.
constraint_indexing:constraint_classification(lakota_education_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEDERAL EDUCATION ADMINISTRATION (ROPE) — Experiences the constraint as coordination function: allocating resources, standardizing curricula, managing systems at scale. Benefits from institutional legitimacy and budgetary allocation. Minimal experienced extraction; sees compliance as solving collective action problem of national education standards. High exit optionality through policy adjustment.
constraint_indexing:constraint_classification(lakota_education_access, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LAKOTA TRIBAL EDUCATION ADVOCATES (TANGLED ROPE) — Organized resistance creates alternative pathways (tribal schools, language immersion programs, culturally-grounded curricula). Benefits from increased agency and institutional recognition, but constrained by limited funding, regulatory barriers, and need to maintain formal education credentials. Mixed experience: genuine coordination of tribal priorities alongside extraction of labor and cultural legitimacy by federal recognition frameworks.
constraint_indexing:constraint_classification(lakota_education_access, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: BIA EDUCATION LEGACY (PITON) — The historical assimilationist boarding school system (Carlisle, Chemawa, Pine Ridge schools) persists as institutional framework despite acknowledged harm and official policy reversals. Performative commitment to 'Indian education' masks continuation of structural assimilation through curriculum control and language subordination. Theater ratio high: ritual acknowledgment of Native languages without substantive curriculum integration; performative tribal consultation without genuine community control. Function has atrophied (assimilation goal is no longer officially stated) but the control structure persists through bureaucratic inertia.
constraint_indexing:constraint_classification(lakota_education_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, educational stratification appears as immutable feature of hierarchical societies: economically marginalized groups always have less access to quality education; geographic isolation always constrains opportunity; language minorities always lag in dominant-language academic metrics. This perspective naturalizes the constraint as inherent to human social organization. However, the structural data contradicts the mountain classification — the extractiveness (0.58) and active enforcement (true) reveal that this is a contingent institutional arrangement, not a law of nature. Engine's false summit detector will identify this as naturalization of what is actually a policy-contingent extraction mechanism.
constraint_indexing:constraint_classification(lakota_education_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lakota_education_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lakota_education_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lakota_education_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lakota_education_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lakota_education_access, TR),
    TR >= 0.70.

:- end_tests(lakota_education_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts cultural control (curriculum, language subordination), labor (student time in programs misaligned with community needs), and institutional legitimacy (communities must work within federal frameworks for resource access). But extraction is not total — federal funding provides genuine educational infrastructure, and tribal education initiatives create partial exit pathways. The trajectory (0.42 → 0.58 over 100 years) reflects accumulation: as explicit assimilation policies formally ended, their structural mechanisms persisted and became institutionalized, creating a more sophisticated extraction mechanism that hides under performative inclusion. Suppression (0.72): High. Multiple suppression mechanisms operate: geographic isolation (limited school choices), economic dependency (federal funding creates resource lock-in), regulatory barriers (tribal schools must meet federal accreditation standards), language barriers (Lakota speakers face penalties in English-dominant curricula), and career barriers (graduates face discrimination in dominant-culture labor markets). Theater ratio (0.65): Moderate-high and rising. Federal programs increasingly emphasize consultation, tribal language curriculum units, and indigenous perspectives in social studies. Yet substantive language integration remains minimal, tribal curriculum authority is limited, and the underlying resource allocation remains centralized. The theater has increased over time as performative commitment substitutes for structural reallocation. Claimed type (tangled_rope): Justified by presence of genuine coordination function (federal systems do solve school access and resource distribution problems) alongside asymmetric extraction (control structures subordinate tribal preferences and cultural transmission).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival gap between powerless victim and institutional beneficiary. Lakota students perceive snare (0.95 d-value, f(d)≈1.42 producing χ≈1.42×0.8×1.0 = 1.14 effective extraction). Federal administration perceives rope (0.05 d-value, f(d)≈-0.12 producing χ≈-0.12×0.8×1.0 = -0.10 effective extraction, i.e., benefit). The gap spans the full range: from maximum extraction experienced as pure coercion to negative extraction experienced as benefit through coordination. Intermediate perspectives (moderate Lakota communities at d≈0.65 seeing tangled rope; organized advocates at d≈0.45 seeing mixed rope/snare) fill the middle ground. The temporal evolution shows the gap widening: as performative inclusion increases without structural change, students and communities perceive increasing extraction while federal administration perceives stable or improving coordination. This divergence is the diagnostic signature of institutional degradation (piton dynamics) — the theater increases while function atrophies.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural position. Lakota students as powerless victims with no exit (trapped) receive maximum d (≈0.95), producing high f(d) and maximum experienced extraction chi. Federal bureaucracy as institutional beneficiary with policy arbitrage receives low d (≈0.05), producing negative f(d) and negative experienced extraction (benefits rather than costs). Lakota communities as moderate-power victims with constrained (not trapped) exit receive moderate d (≈0.65), producing moderate f(d) and proportional extraction chi. Tribal advocates as organized agents with constrained exit receive d ≈0.45, producing lower f(d) than powerless agents but higher than beneficiaries, reflecting their partial agency and partial extraction. The directionality derivation chain operates as: [structural_relationship (victim/beneficiary) + power_level + exit_options] → d → f(d) → chi × σ(S). No overrides are needed for primary perspectives; the canonical derivation captures the structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint demonstrates the mandatrophy resolution pattern where all six types are valid but from different structural positions. Snare (powerless/trapped agent) is the victim's structural reality. Tangled rope (moderate/constrained agents) is the hybrid experience of communities with partial agency. Rope (institutional/arbitrage beneficiary) is genuine from the federal perspective — they experience coordination. Scaffold (organized agents with sunset path) is realistic if tribal education autonomy movements succeed. Piton (institutional/arbitrage arbitrage at civilizational time) reveals that the BIA education legacy persists through inertia rather than function. Mountain (analytical/universal time) appears natural to systems-thinkers who see educational stratification as inevitable — but the structural data (active enforcement required, measurable extractiveness, rising theater ratio) reveals this as false naturalization. The mandatrophy is not 'which type is correct' but 'the constraint is genuinely all six from different positions; the full picture requires the whole presheaf.' The key insight for policy: moving the analytical observer's position from mountain (accepting inequality as inevitable) to snare (recognizing it as extraction mechanism) would enable policy reframing from 'improving access within the system' to 'restructuring control mechanisms' — a shift that the constraint's structural data supports.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geographic_isolation_vs_policy_choice,
    'How much of Lakota education disadvantage stems from geographic isolation as unavoidable structural fact versus policy choices about resource allocation to rural/reservation schools?',
    'Comparative analysis of similar geographic isolation cases with different policy outcomes (rural Alaska Native schools with robust funding vs. reservation schools with minimal funding); cost-benefit analysis of virtual/hybrid education infrastructure deployment',
    'If primarily geographic (unavoidable): more constraint features appear immutable (mountain characteristics increase). If primarily policy-driven: extractiveness interpretation stands (tangled rope / snare classification confirmed). Affects classification boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_isolation_vs_policy_choice, empirical, 'Whether disadvantage is geographic or policy-driven').

omega_variable(
    tribal_sovereignty_vs_federal_coordination,
    'Does federal education coordination genuinely solve a collective action problem (preventing lowest-common-denominator outcomes) or does it primarily impose external control on tribal education preferences?',
    'Historical comparison of tribal education systems pre-federal standardization; analysis of outcomes in communities that achieved educational autonomy (tribal colleges, autonomous tribal schools) versus federally-coordinated systems; measurement of alignment between federal standards and tribal learning goals',
    'If coordination genuine: rope perspective gains validity; community gains tangible benefit. If primarily control: snare/extraction perspective gains validity; coordination claim is cover story. Affects beneficiary/victim classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tribal_sovereignty_vs_federal_coordination, conceptual, 'Whether federal coordination is genuine or merely control').

omega_variable(
    language_suppression_structural_vs_unintended,
    'Is the subordination of Lakota language in education systems a deliberate extraction mechanism (cultural control) or an unintended consequence of resource constraints and standardization?',
    'Policy document analysis (historical intent in curriculum standardization); resource allocation patterns comparing Lakota-medium instruction funding vs. English-only instruction; comparison of resource availability for indigenous language programs in jurisdictions with explicit language preservation commitments vs. those without',
    'If deliberate: suppression interpretation (0.72) is accurate; extraction mechanism is cultural. If unintended: suppression may be lower than measured; classification shifts toward rope. Affects suppression metric interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(language_suppression_structural_vs_unintended, empirical, 'Whether language suppression is deliberate or unintended').

omega_variable(
    identity_lock_in_educational_aspiration,
    'Do Lakota students experience constrained mobility (external barriers) or identity_locked immobility (internalized framing of educational possibility)?',
    'Post-secondary educational pathway tracking; ethnographic analysis of student perception of possibility; comparison of educational aspirations (what students hope for) versus exit behaviors (actual pursuit of opportunities); measurement of persistence of low aspiration following barrier removal (scholarship programs, relocation opportunities)',
    'If constrained: barriers-focused policy interventions can shift outcomes (funding, infrastructure). If identity_locked: identity-frame interventions required (cultural relevance, community leadership in curriculum). Determines intervention design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_educational_aspiration, empirical, 'Whether student mobility is constrained or identity-locked').

omega_variable(
    tribal_education_autonomy_sufficiency,
    'If Lakota communities achieved complete educational autonomy and control, would resource constraints or systematic cultural/market disadvantages prevent comparable outcomes to dominant-culture schools?',
    'Analysis of tribal colleges outcomes; comparison of tribal school systems with maximum autonomy; historical analysis of pre-assimilation educational systems; cost analysis of truly equal-resource schools in comparable geographic contexts',
    'If autonomy sufficient: extraction mechanism is primarily control-based (tangled rope classification appropriate). If resource/market gaps persist: constraint includes coordination problem of historical disadvantage (rope component genuine). Affects mandatrophy resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tribal_education_autonomy_sufficiency, conceptual, 'Whether autonomy is sufficient for educational equity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lakota_education_access, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lako_tr_t0, lakota_education_access, theater_ratio, 0, 0.48).
narrative_ontology:measurement(lako_tr_t50, lakota_education_access, theater_ratio, 50, 0.6).
narrative_ontology:measurement(lako_tr_t100, lakota_education_access, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(lako_be_t0, lakota_education_access, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lako_be_t50, lakota_education_access, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(lako_be_t100, lakota_education_access, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lakota_education_access, resource_allocation).
narrative_ontology:affects_constraint(lakota_education_access, lakota_language_preservation).
narrative_ontology:affects_constraint(lakota_education_access, tribal_sovereignty_constraints).
narrative_ontology:affects_constraint(lakota_education_access, indigenous_student_debt).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
