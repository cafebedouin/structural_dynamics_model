% ============================================================================
% CONSTRAINT STORY: pathologization_of_poverty_and_marginalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pathologization_of_poverty_and_marginalization, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pathologization_of_poverty_and_marginalization
 *   human_readable: Pathologization of Poverty and Marginalization
 *   domain: social_policy/structural_inequality
 *
 * SUMMARY:
 *   The pathologization of poverty and marginalization is a structural
 *   constraint that reframes systemic inequality as individual dysfunction.
 *   Across the 70-year measurement interval, this constraint has intensified:
 *   extractiveness increased from 0.35 to 0.68, and theater ratio increased
 *   from 0.38 to 0.64. The constraint operates by shifting causal attribution
 *   from structural (distribution of resources, access to power, historical
 *   injustice) to individual (personal pathology, defective character,
 *   genetic predisposition, poor impulse control). Resources that might
 *   address structural inequality are instead allocated to diagnosis,
 *   treatment, and management of marginalized populations. The constraint
 *   exhibits features of all six types from different perspectives: for the
 *   powerless, it is a Snare with trapped and identity-locked exit routes;
 *   for organized social movements, it appears as a Scaffold with a sunset
 *   clause that could be activated through structural reform; for the medical
 *   establishment, it is a Rope providing coordination through standardized
 *   diagnostic frameworks; for the broader institutional system, it is a
 *   Piton maintained through professional and funding inertia; for the
 *   analytical observer, it is a Tangled Rope exhibiting both genuine
 *   coordination functions and extractive asymmetry. The increasing theater
 *   ratio indicates that diagnostic and treatment infrastructure has become
 *   progressively more elaborate while actual improvements in material
 *   outcomes for marginalized populations have plateaued or declined,
 *   suggesting the apparatus is increasingly performing legitimation rather
 *   than delivering care.
 *
 * KEY AGENTS:
 *   - Poor and marginalized populations: Primary victims (powerless/trapped and identity_locked) — bear full cost of pathology framing while structural alternatives are delegitimized
 *   - Affluent professional classes: Primary beneficiaries (institutional/arbitrage) — benefit from inequality legitimation without bearing costs of addressing structural causes
 *   - Medical and diagnostic establishment: Secondary beneficiaries (institutional/arbitrage) — expanded jurisdictional domain, revenue, and professional prestige through pathology framing
 *   - Harm-reduction and community health organizations: Constrained coordinators (organized/constrained) — provide genuine services while embedded in and forced to accept pathology framework
 *   - Social justice and structural reform movements: Scaffolding opposition (organized/constrained) — building alternative frameworks and pushing for sunset clauses through policy and cultural change
 *   - Institutional legacy systems: Inertial maintainers (institutional/arbitrage) — perpetuate pathology framing through professional structures, funding mechanisms, and diagnostic conventions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pathologization_of_poverty_and_marginalization, 0.68).
domain_priors:suppression_score(pathologization_of_poverty_and_marginalization, 0.72).
domain_priors:theater_ratio(pathologization_of_poverty_and_marginalization, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pathologization_of_poverty_and_marginalization, extractiveness, 0.68).
narrative_ontology:constraint_metric(pathologization_of_poverty_and_marginalization, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(pathologization_of_poverty_and_marginalization, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pathologization_of_poverty_and_marginalization, snare).
narrative_ontology:human_readable(pathologization_of_poverty_and_marginalization, "Pathologization of Poverty and Marginalization").
narrative_ontology:topic_domain(pathologization_of_poverty_and_marginalization, "social_policy/structural_inequality").

domain_priors:requires_active_enforcement(pathologization_of_poverty_and_marginalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pathologization_of_poverty_and_marginalization, affluent_professional_classes).
narrative_ontology:constraint_beneficiary(pathologization_of_poverty_and_marginalization, medical_diagnostic_industry).
narrative_ontology:constraint_beneficiary(pathologization_of_poverty_and_marginalization, institutional_gatekeepers).
narrative_ontology:constraint_victim(pathologization_of_poverty_and_marginalization, poor_and_marginalized_populations).
narrative_ontology:constraint_victim(pathologization_of_poverty_and_marginalization, social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PATHOLOGIZED POOR (SNARE) — Structurally trapped. Poverty itself is reframed as a result of individual pathology (poor impulse control, defective character, mental illness) rather than systemic distribution of resources. The victim bears the cost of this framing: resources flow to diagnosis and treatment rather than structural remediation. Exit requires either becoming non-poor (structurally blocked) or accepting externally-imposed diagnostic labels that further damage social position. Maximum suppression: alternatives (structural reform, redistribution, community control) are delegitimized as unrealistic while individual pathology is accepted as common sense.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (IDENTITY_LOCKED, GENERATIONAL) — Structurally mobile (could organize politically, could demand structural reform) but identity-locked by internalized pathology narratives. Community identity becomes fused with diagnostic categories: the community internalizes the framing that their marginalization reflects defective cultural values, poor parenting practices, or genetic predisposition. Exit from this constraint would require the community to reject these identity frames and assert structural causation — a cognitive and social reorganization more difficult than physical relocation. Generational view shows that children inherit both the material deprivation and the internalized pathology narrative, creating cyclical lock-in.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: HARM-REDUCTION ORGANIZATIONS (TANGLED ROPE) — Organized agents that provide genuine coordination function (meeting immediate health and social needs of marginalized populations) while embedded in an extractive framework. These organizations must accept the pathology framing to access funding and legitimacy; their work becomes both service delivery and stigma management. Constrained by funding dependence on donors who accept the pathology model; can reduce immediate harms but cannot challenge the constraint that creates the harms. Mixed classification reflects real service provision plus institutional capture — organizations become administrators of systemic injustice while believing they oppose it.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL AND DIAGNOSTIC ESTABLISHMENT (ROPE) — Institutional beneficiary with arbitrage options. The pathologization framework generates revenue, diagnostic categories, treatment protocols, and professional jurisdictional expansion. Psychiatry, psychology, social work, and public health all expand their domains by reframing social problems as mental health or behavioral disorders. From this perspective, the constraint appears as legitimate coordination: standardizing diagnostic criteria, enabling treatment access, building professional knowledge. The establishment experiences no suppression — the constraint is structured to their benefit and aligns with their institutional interests. They can exit (redirect to structural causation) but have no incentive to do so.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SOCIAL JUSTICE MOVEMENTS (SCAFFOLD) — Organized agents framing the constraint as temporary and solvable through structural reform, policy change, and institutional redistribution. These movements see a sunset clause: as material conditions improve, marginalization declines, and the pathology framing becomes empirically incoherent. Constrained by political barriers and institutional resistance but with increasing cultural and demographic momentum. The scaffold view requires active enforcement of counter-narratives and structural alternatives — mutual aid networks, participatory budgeting, decriminalization, wealth redistribution — that bypass the pathology framework entirely. High suppression reflects institutional resistance to this reframing; theater ratio reflects that social justice rhetoric often substitutes for structural change, creating performative solidarity without material redistribution.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL LEGACY (PITON) — From a civilizational view, pathologization represents a degraded institutional practice maintained through inertia: the constraint persists because no alternative has fully replaced it, despite mounting evidence of its dysfunction. Early 20th-century eugenic justifications have been abandoned; modern pathologization uses neuroscience and behavioral genetics as its theater, but the underlying structure (framing structural inequality as individual deficiency) has remained constant for centuries. The piton classification reflects high theater ratio: diagnostic language, treatment protocols, and research funding create the appearance of scientific progress while the core framing remains unchanged. Exit would require abandoning entire institutional and professional structures, not just policy changes.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the broadest perspective, the pathologization constraint exhibits both genuine coordination and extractive functions. Legitimate functions include: creating shared diagnostic language enabling treatment, building professional knowledge about conditions affecting marginalized populations, coordinating care delivery. Extractive functions include: legitimizing inequality as inevitable, shifting resource allocation from structural reform to individual treatment, protecting affluent populations' implicit theory of justice. The constraint is neither pure extraction (Snare) nor pure coordination (Rope) — it is genuinely hybrid. The classification reveals that accepting the hybrid structure means accepting ongoing extraction as a cost of coordination; rejecting the extraction requires rejecting the coordination framework and building alternatives.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pathologization_of_poverty_and_marginalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pathologization_of_poverty_and_marginalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pathologization_of_poverty_and_marginalization, TR),
    TR >= 0.70.

:- end_tests(pathologization_of_poverty_and_marginalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High and increasing. The constraint systematically extracts resources from vulnerable populations, redirecting them toward diagnosis and treatment administered by affluent professionals rather than toward structural remediation. The increase over time reflects diagnostic category expansion (new disorders continuously added to DSM and ICD), treatment intensification (prescription expansion, therapeutic mandates in schools and workplaces), and professionalization of poverty management across multiple disciplines. Suppression (0.72): Very high. Structural alternatives to pathology framing are actively delegitimized: talk of 'systemic inequality' is dismissed as naive or politically extreme; demands for redistribution are characterized as unrealistic; community self-determination is pathologized as 'lack of capacity' or 'poor governance.' Alternatives cannot be articulated, funded, or legitimized within institutional frameworks. Theater ratio (0.64): Moderately high and increasing. Diagnostic categories, treatment protocols, and therapeutic expertise create the appearance of rigorous scientific intervention while the underlying causal framework (individual pathology rather than structural deprivation) remains largely unexamined. Expansion of neuroscience, behavioral genetics, and neuroimaging provides contemporary theater for narratives that have persisted unchanged for centuries. The increase reflects growing gap between diagnostic sophistication and actual improvement in marginalized populations' material conditions.
 *
 * PERSPECTIVAL GAP:
 *   The pathologized poor perceive the constraint as a Snare with maximum suppression: they are trapped by poverty (structural barrier to exit) and identity-locked by internalized pathology narratives (cognitive barrier). They have no legitimate alternative framework within institutional domains. The medical establishment perceives the constraint as Rope: standardized diagnostic language enables care coordination, professional collaboration, and treatment access. They experience no suppression because the constraint aligns with their interests. Organized social movements perceive Scaffold: they see the pathology framing as temporary and solvable through structural reform, wealth redistribution, and community power-building — but they face severe suppression from institutional resistance. The analytical observer perceives Tangled Rope: the constraint exhibits both real coordination functions (shared diagnostic language, professional knowledge, care infrastructure) and extractive asymmetry (resource misdirection, legitimation of inequality, identity lock). The perspectival gap reveals that accepting the institutional framing (individual pathology) makes exit impossible; rejecting it requires building entirely alternative systems of meaning and resource allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction (d) is determined by each agent's structural position relative to the pathology framing. Poor and marginalized populations face maximum d (close to 1.0): they are targets of the pathology labeling apparatus, bear the costs of misdirected resources, and experience identity damage from internalized pathology narratives. Beneficiaries (affluent professionals, diagnostic industry) face minimum d (close to 0.0): the constraint protects their material position and professional interests; they have arbitrage options and high exit capacity but no incentive to exit. Organized movements face moderate-to-high d (0.55–0.75): they are partly targets of pathology framing (their structural reform demands are pathologized as unrealistic or utopian) but also have organizational capacity to contest the framing. The increasing extractiveness over the interval reflects growing d for powerless populations: diagnostic apparatus has expanded its reach and intensity, increasing the proportion of marginalized life subject to pathology interpretation. Mandatrophy is resolved by recognizing the extraction-coordination duality: the pathology constraint genuinely coordinates professional knowledge and care access while simultaneously extracting legitimacy from structural inequality. Accepting this duality means living with ongoing extraction; rejecting the extraction requires rejecting the coordination framework and building alternatives (community health workers, mutual aid, participatory budgeting, basic income).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint exhibits extraction (ε = 0.68, suppression = 0.72, χ computed at victims' perspective ≥ 0.66, satisfying snare gate) and coordination (measurable care provision, shared diagnostic language, professional knowledge-building). The mandatrophy is resolved by recognizing that these functions are structurally entangled: the coordination function depends on accepting the extraction mechanism. To accept the coordination (shared diagnostic language, professional standards, care access) requires accepting the extraction (pathology framing, resource misdirection, identity lock). To reject the extraction requires building an entirely alternative coordination framework that does not depend on individual pathology narratives — mutual aid networks, community health authority, structural reform, wealth redistribution. The analytical resolution: the constraint is not secretly 'just' extraction or 'just' coordination hiding under another name. It is genuinely both. The policy choice is whether to accept the extraction as a necessary cost of coordination or to incur the transition costs of building alternative coordination systems that do not require pathology framing. The mandatrophy is fully resolved when this duality is explicitly acknowledged and the choice is consciously made rather than naturalized as inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causation_direction_ambiguity,
    'Does poverty cause mental health problems and behavioral dysfunction (pathology model), or do systemic marginalization and structural deprivation cause psychological and social strain that manifests as diagnosable conditions (structural model)?',
    'Causal inference studies isolating structural variables (income, wealth, access to power) from individual pathology indicators; cross-national comparison of mental health outcomes controlled for structural inequality; intervention studies comparing structural reform vs. individual treatment outcomes',
    'If causation runs primarily from individual pathology to poverty: constraint is coordination mechanism for identifying and treating real dysfunction — reclassify as Rope. If causation runs from structural deprivation to psychological strain: pathology framing is extractive naturalization — reclassify as pure Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causation_direction_ambiguity, empirical, 'Whether poverty causes pathology or structural deprivation causes psychological strain').

omega_variable(
    diagnostic_label_externality,
    'Do psychiatric and behavioral diagnostic labels applied to marginalized populations function as treatment enablers or as stigma and identity locks?',
    'Longitudinal studies of diagnostic label effects: post-diagnosis social outcomes, employment, community trust, self-concept stability; comparison of outcomes for identical symptoms across economic classes (diagnostic bias detection); ethnographic evidence of how labels function in community contexts vs. clinical contexts',
    'If labels primarily enable treatment: pathologization represents coordination with side effects — modify classification to acknowledge dual function. If labels primarily function as stigma: the entire pathology framework is an identity lock mechanism — reclassify as pure Snare with identity_locked exit option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_label_externality, empirical, 'Whether diagnostic labels enable treatment or function primarily as stigma').

omega_variable(
    counterfactual_structural_intervention,
    'What would mental health and behavioral outcomes look like if the same resources allocated to diagnosis and treatment were instead allocated to unconditional income support, wealth redistribution, and structural equality?',
    'Natural experiments comparing regions/populations with different welfare-to-treatment ratios; pilot programs implementing basic income and measuring mental health outcomes; historical analysis of outcomes during periods of strong welfare provision vs. periods of market-driven diagnosis',
    'If structural intervention produces dramatically better outcomes: the pathology framing is extractive misdirection — reclassify as pure Snare with high mandatrophy. If outcomes are comparable: the pathology framework represents genuine coordination with real treatment value — reclassify as Rope or Tangled Rope with lower extraction estimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_structural_intervention, empirical, 'Mental health outcomes under structural intervention vs. diagnostic treatment').

omega_variable(
    identity_lock_mechanism_strength,
    'To what extent do marginalized populations accept and internalize pathology narratives as descriptions of their own nature vs. perceiving them as external impositions?',
    'Survey data on whether marginalized respondents attribute their circumstances to personal deficiency vs. systemic factors; experimental priming studies showing whether counter-narratives (structural attribution frames) temporarily shift self-concept and behavioral intentions; analysis of collective action — when communities reject pathology framing, do they successfully organize alternative frameworks?',
    'If internalization is deep and resistant to counter-narratives: identity_locked exit option is accurate and the lock is strong — extract requires identity transformation. If internalization is conditional and readily abandoned when counter-narratives are available: the lock is weaker and organizational resistance is more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_strength, empirical, 'Strength of internalized pathology identity vs. receptiveness to structural narratives').

omega_variable(
    theater_ratio_saturation,
    'Has the diagnostic and treatment apparatus become so elaborate that it now primarily performs legitimation rather than providing care?',
    'Measurement of resource allocation: what percentage of spending goes to diagnosis/labeling vs. actual treatment vs. structural change? Gap analysis between diagnostic capacity expansion and treatment outcome improvement; cost-benefit analysis of diagnosis infrastructure vs. direct material aid equivalents',
    'If theater ratio is increasing and outcomes are flat or declining: the constraint is becoming pure Piton (degraded and inertial). If theater ratio is declining and outcomes improve: constraint may be evolving from Snare toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_saturation, empirical, 'Whether diagnostic apparatus is saturation point of performative legitimation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pathologization_of_poverty_and_marginalization, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pathpov_tr_t0, pathologization_of_poverty_and_marginalization, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pathpov_tr_t35, pathologization_of_poverty_and_marginalization, theater_ratio, 35, 0.51).
narrative_ontology:measurement(pathpov_tr_t70, pathologization_of_poverty_and_marginalization, theater_ratio, 70, 0.64).
narrative_ontology:measurement(pathpov_tr_t10, pathologization_of_poverty_and_marginalization, theater_ratio, 10, 0.44).
narrative_ontology:measurement(pathpov_tr_t50, pathologization_of_poverty_and_marginalization, theater_ratio, 50, 0.57).

% Extraction over time
narrative_ontology:measurement(pathpov_be_t0, pathologization_of_poverty_and_marginalization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pathpov_be_t35, pathologization_of_poverty_and_marginalization, base_extractiveness, 35, 0.52).
narrative_ontology:measurement(pathpov_be_t70, pathologization_of_poverty_and_marginalization, base_extractiveness, 70, 0.68).
narrative_ontology:measurement(pathpov_be_t10, pathologization_of_poverty_and_marginalization, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(pathpov_be_t50, pathologization_of_poverty_and_marginalization, base_extractiveness, 50, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pathologization_of_poverty_and_marginalization, identity_coordination).
narrative_ontology:boltzmann_floor_override(pathologization_of_poverty_and_marginalization, 0.12).
narrative_ontology:affects_constraint(pathologization_of_poverty_and_marginalization, welfare_stigma_and_surveillance).
narrative_ontology:affects_constraint(pathologization_of_poverty_and_marginalization, criminalization_of_poverty).
narrative_ontology:affects_constraint(pathologization_of_poverty_and_marginalization, medical_gatekeeping_of_access).

% DUAL FORMULATION NOTE:
% Pathologization operates at the level of causal attribution — it reframes structural inequality as individual dysfunction. Downstream constraints (welfare stigma, criminalization, medical gatekeeping) inherit this causal framing and apply it through specific institutional mechanisms. Each downstream constraint has its own extractiveness value reflecting its specific operational domain, but all depend on the pathologization frame to justify their existence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pathologization_of_poverty_and_marginalization, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
