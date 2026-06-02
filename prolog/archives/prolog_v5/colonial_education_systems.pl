% ============================================================================
% CONSTRAINT STORY: colonial_education_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colonial_education_systems, []).

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
 *   constraint_id: colonial_education_systems
 *   human_readable: Colonial Education Systems as Extractive Epistemic Control
 *   domain: political/educational/postcolonial
 *
 * SUMMARY:
 *   Colonial education systems represent a paradigmatic extractive
 *   constraint: the imposition of metropolitan epistemic hierarchies on
 *   colonized populations through mandatory schooling in colonial languages,
 *   colonial curricula, and colonial institutional structures. The constraint
 *   operates across two temporal regimes — the colonial period proper (active
 *   extraction with direct metropole benefit) and the post-colonial era
 *   (institutional persistence through inertia and internalized capture). The
 *   constraint is a snare because it combines maximal suppression
 *   (alternatives are systematically dismantled) with high extractiveness
 *   (cultural capital, labor capacity, epistemic resources flow toward the
 *   metropole and settler classes) and minimal coordination function (the
 *   constraint does not solve a mutual coordination problem; it solves the
 *   metropole's problem of controlling colonized populations and resources).
 *   The theater ratio increases over the colonial period as the system
 *   becomes normalized and ritualized, and remains high post-colonially as
 *   educational institutions continue to reproduce colonial epistemic
 *   hierarchies under the rhetoric of 'development' and 'modernization.'
 *
 * KEY AGENTS:
 *   - Colonized populations (students, families): Primary victims (powerless/trapped) — subject to mandatory education that extracts cultural identity and epistemic autonomy while presenting as opportunity
 *   - Indigenous knowledge holders and elders: Secondary victims (powerless/identity_locked) — structurally capable of teaching alternatives but psychologically convinced of the superiority of colonial knowledge through their own education
 *   - Colonial metropole administration: Primary beneficiary (institutional/arbitrage) — extracts labor capacity, political loyalty, and resource access through standardized educated colonial subjects
 *   - Settler intermediate class (teachers, administrators, traders): Secondary beneficiary (moderate/constrained) — gain access and upward mobility relative to colonized populations but remain subordinate to metropole interests
 *   - Post-colonial state educational apparatus: Institutional carrier (institutional/arbitrage) — maintains colonial structures through inertia despite formal decolonization; reproduces epistemic hierarchy under nationalist rhetoric
 *   - Analytical observer: Sees civilizational-scale epistemic destruction and knowledge commons depletion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colonial_education_systems, 0.68).
domain_priors:suppression_score(colonial_education_systems, 0.72).
domain_priors:theater_ratio(colonial_education_systems, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colonial_education_systems, extractiveness, 0.68).
narrative_ontology:constraint_metric(colonial_education_systems, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(colonial_education_systems, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colonial_education_systems, snare).
narrative_ontology:human_readable(colonial_education_systems, "Colonial Education Systems as Extractive Epistemic Control").
narrative_ontology:topic_domain(colonial_education_systems, "political/educational/postcolonial").

domain_priors:requires_active_enforcement(colonial_education_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colonial_education_systems, colonial_metropole).
narrative_ontology:constraint_beneficiary(colonial_education_systems, settler_administrative_class).
narrative_ontology:constraint_victim(colonial_education_systems, colonized_populations).
narrative_ontology:constraint_victim(colonial_education_systems, indigenous_knowledge_systems).
narrative_ontology:constraint_victim(colonial_education_systems, local_epistemic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COLONIZED STUDENT (SNARE) — Faces mandatory education in colonial language and epistemology with no alternative pathway to formal credential. Exit from the system means economic exclusion and social marginalization. The constraint extracts cultural capital, linguistic competence, and identity while presenting as opportunity. Suppression is total at local scope: alternatives are systematically dismantled.
constraint_indexing:constraint_classification(colonial_education_systems, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIGENOUS KNOWLEDGE HOLDERS (SNARE/IDENTITY_LOCKED) — Structurally mobile (could teach outside colonial schools) but identity-locked by generational enforcement of the colonial epistemic frame. The younger generation has internalized the colonial hierarchy of knowledge — indigenous languages are 'backwards,' traditional ecological knowledge is 'unscientific.' The binding mechanism is cognitive capture through childhood education. Suppression operates through institutional delegitimation of alternatives.
constraint_indexing:constraint_classification(colonial_education_systems, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: COLONIAL METROPOLE (ROPE) — Experiences the constraint as pure coordination: standardized education across colonies solves administrative coherence and legitimacy problems. The metropole extracts labor, loyalty, and tax capacity from educated colonials. This perspective sees the constraint as beneficial coordination, not extraction.
constraint_indexing:constraint_classification(colonial_education_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SETTLER INTERMEDIATE CLASS (TANGLED_ROPE) — Small traders, junior administrators, educators themselves. They benefit from education access but remain subordinate to metropole interests. Suppression is moderate — they have constrained mobility, some access to credentials, but face glass ceiling limiting advancement. Mixed extraction and coordination: education is both opportunity and control mechanism.
constraint_indexing:constraint_classification(colonial_education_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-COLONIAL INSTITUTIONAL RESIDUE (PITON) — After decolonization, the educational apparatus persists through institutional inertia. The curriculum, languages of instruction, institutional structure, and epistemic hierarchy were products of colonial extraction, but they remain embedded in post-colonial states. Theater ratio is high: education is performed as 'national development' while reproducing colonial knowledge hierarchies. The functional extraction has degraded (no direct metropole drain) but the constraint persists as theatrical maintenance.
constraint_indexing:constraint_classification(colonial_education_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational scope, colonial education systems represent systematic epistemic harm: the destruction of alternative knowledge systems, the devaluation of indigenous languages as vehicles of thought, and the imposition of a single epistemic hierarchy across territories with no alternatives. This is pure extraction at civilizational scale — knowledge commons are depleted, alternatives are eliminated, and the colonized bear the full cost.
constraint_indexing:constraint_classification(colonial_education_systems, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colonial_education_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colonial_education_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colonial_education_systems, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colonial_education_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(colonial_education_systems, TR),
    TR >= 0.70.

:- end_tests(colonial_education_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting that the primary function of colonial education is knowledge extraction and epistemic control rather than knowledge creation or genuine skill development. Measurement trajectory shows increase from 0.55 to 0.68 as the system matures and deepens its institutional embedding. Suppression (0.72): Very high. Indigenous schools are dismantled, indigenous languages are delegitimized, indigenous teachers are excluded from formal institutions, and alternatives are not merely suppressed but rendered invisible and 'unthinkable.' Suppression is structural and enforced. Theater ratio (0.58): Moderate-high and rising. During early colonialism, the educational function is relatively transparent — it is clearly a tool of control. By late colonialism and post-colonially, the theater increases as education is performed as 'civilization,' 'development,' 'progress,' and 'nation-building' while the underlying epistemic extraction persists. The claimed type is Snare because: (1) extractiveness >= 0.46 ✓, (2) suppression >= 0.60 ✓, (3) Chi will be >= 0.66 for trapped/powerless perspectives ✓, (4) minimal coordination function — colonized populations would be better off with alternative systems, and the constraint exists only because exit has been blocked.
 *
 * PERSPECTIVAL GAP:
 *   Colonial education systems exemplify how the same institutional structure can be legitimately classified as a coordination mechanism (Rope) from the metropole's perspective and pure extraction (Snare) from the colonized perspective. This gap reflects genuine structural reality, not subjective difference: the constraint solves the metropole's coordination problem while creating the colonized population's extraction problem. Post-colonially, the constraint persists not because it solves any coordination function (the metropole is no longer present to benefit) but through institutional inertia and internalized epistemic hierarchy — the constraint becomes Piton. This trajectory (Snare → Piton) is a typical pattern for extractive constraints that outlive their extraction engine.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values range from 0.95 (colonized student as full target/trapped) to 0.05 (colonial metropole as full beneficiary/arbitrage). The colonized student's high d reflects that they are structurally trapped with no exit option and bear maximal extraction. The metropole's low d reflects that they are the primary beneficiary of the extraction mechanism. The indigenous knowledge holder's d is moderately high (~0.80) despite having structural mobility (could choose to teach outside schools) because identity_locked exit options are much higher cost than the canonical trapped/constrained distinction captures — breaking from internalized belief in epistemic hierarchy requires identity reformation, not just material costs. The settler intermediate class's d is moderate (~0.55) because they are partially victimized (constrained to subaltern positions) but also partially beneficial (gain access to credentials). These directionality values produce the perspectival gap: beneficiaries experience low effective extraction, victims experience high effective extraction, and the analytical observer sees the sum.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE RESOLUTION: Colonial education systems are unambiguously extractive snares, not coordination mechanisms masquerading as extraction. The six perspectives confirm this: no perspective legitimately classifies the constraint as pure coordination. The metropole's Rope classification is from the metropole's beneficiary position, not from the constraint's actual function — the constraint does not solve a mutual coordination problem; it solves the metropole's problem of extracting resources and loyalty from colonized populations. The presence of a Rope perspective does not indicate mandatrophy; it indicates structural inequality. The absence of a Rope perspective from the colonized population's standpoint confirms that the constraint is not coordination. Mandatrophy would appear if: (1) all perspectives classified the constraint as Snare without exception, OR (2) the beneficiary perspective classified it as Rope but the empirical evidence showed mutual benefit — neither condition is met. The constraint legitimately exhibits perspectival variance because perspectives from different structural positions experience fundamentally different constraint functions. This variance is diagnostic, not problematic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_durability,
    'Is the epistemic capture of indigenous knowledge holders permanent (constitutive of postcolonial identity) or reversible through counter-education?',
    'Longitudinal study of decolonial education movements (such as language revitalization programs, indigenous curriculum development) to measure whether students raised in counter-hegemonic contexts recover indigenous knowledge frames or remain locked in colonial hierarchy',
    'If permanent: the snare classification is strengthened, and exit requires civilizational-scale identity reformation. If reversible: alternative pathways exist, elevating the constraint from Snare toward Tangled Rope for those with organized support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Whether indigenous knowledge suppression is reversible through counter-education').

omega_variable(
    functional_extraction_magnitude,
    'How much of the measurable economic extraction (wage differential, opportunity cost, resource drain) is attributable to colonial education systems versus other colonial institutions (land tenure, labor regulation, taxation)?',
    'Counterfactual analysis comparing colonies with varying educational penetration; econometric decomposition of colonial-era wealth extraction into components',
    'If education is dominant extraction mechanism (>40% of total): extractiveness estimate is accurate. If secondary (<20%): extractiveness should be revised downward to 0.45-0.55, elevating some perspectives from Snare to Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_extraction_magnitude, empirical, 'Proportion of colonial extraction attributable to education systems').

omega_variable(
    postcolonial_autonomy_gap,
    'To what extent do post-colonial states reconstitute educational sovereignty versus remaining institutionally dependent on former metropole models and epistemic standards?',
    'Comparative curriculum analysis (decolonization of content, languages of instruction, epistemological frameworks) across post-colonial states; measurement of pedagogical dependency on imported materials/methods',
    'If most states achieve full epistemic autonomy (>70%): the constraint degrades from Snare to Piton in the post-colonial period. If dependency persists (>50% institutional reliance on metropole frameworks): the Snare classification holds into post-colonial era.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postcolonial_autonomy_gap, empirical, 'Degree of post-colonial educational autonomy from metropole models').

omega_variable(
    suppression_mechanism_variation,
    'Does suppression of indigenous knowledge systems operate uniformly through curriculum exclusion, teacher training, institutional hierarchy, or through differential internalization by age cohorts?',
    'Ethnographic analysis of how suppression manifests across different institutional sites and age groups; measurement of indigenous language retention by generation and educational pathway',
    'If primarily institutional (curriculum/hierarchy): exit is possible with alternative schools. If primarily internalized (identity_locked): exit requires cognitive reframing and escape is structurally harder even without institutional barriers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_variation, empirical, 'Whether suppression operates through institutional or internalized mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colonial_education_systems, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colo_tr_t0, colonial_education_systems, theater_ratio, 0, 0.35).
narrative_ontology:measurement(colo_tr_t5, colonial_education_systems, theater_ratio, 5, 0.48).
narrative_ontology:measurement(colo_tr_t10, colonial_education_systems, theater_ratio, 10, 0.58).
narrative_ontology:measurement(colo_tr_t15, colonial_education_systems, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(colo_be_t0, colonial_education_systems, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(colo_be_t5, colonial_education_systems, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(colo_be_t10, colonial_education_systems, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(colo_be_t15, colonial_education_systems, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colonial_education_systems, identity_coordination).
narrative_ontology:affects_constraint(colonial_education_systems, linguistic_hegemony).
narrative_ontology:affects_constraint(colonial_education_systems, indigenous_land_dispossession).
narrative_ontology:affects_constraint(colonial_education_systems, postcolonial_dependency_structures).

% DUAL FORMULATION NOTE:
% Colonial education systems are part of a larger epistemic extraction apparatus that includes language policy, curriculum standards, and institutional legitimacy. The three downstream constraints represent specific mechanisms through which the educational constraint operates: linguistic hegemony enforces the use of colonial languages; land dispossession eliminates the material basis for indigenous knowledge transmission; postcolonial dependency perpetuates metropolitan epistemic standards even after formal political decolonization. Each downstream constraint has higher extractiveness (0.72-0.75) reflecting that education is a foundational mechanism enabling the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(colonial_education_systems, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
