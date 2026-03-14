% ============================================================================
% CONSTRAINT STORY: developmental_disability_clustering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developmental_disability_clustering, []).

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
 *   constraint_id: developmental_disability_clustering
 *   human_readable: Developmental Disability Clustering in Educational and Social Systems
 *   domain: social_policy/disability_justice/education
 *
 * SUMMARY:
 *   Developmental disability clustering refers to the institutional practice
 *   of aggregating children with diverse developmental needs into categorical
 *   diagnostic buckets (autism, intellectual disability, specific learning
 *   disability, etc.) for purposes of educational placement, service
 *   allocation, and resource distribution. This clustering system emerged
 *   from genuine coordination needs — schools needed systematic ways to
 *   allocate scarce resources and adapted services. However, the clustering
 *   mechanism has evolved into a hybrid constraint combining coordination
 *   functions with significant extraction mechanisms. The diagnosed child and
 *   family enter a system where the diagnosis simultaneously opens access to
 *   services and closes alternative pathways. Schools benefit from funding
 *   formulas that reward documented disability counts. Medical and assessment
 *   providers expand diagnostic categories to maintain service volume. The
 *   constraint exhibits all six DR types from different structural positions.
 *   From the perspective of the diagnosed child, it appears as a snare —
 *   trapped in a categorized pathway with stigma and foreclosed opportunity.
 *   From the school system's perspective, it appears as tangled rope —
 *   genuine coordination of resources paired with revenue extraction through
 *   documentation. From organized disability justice movements, it appears as
 *   a scaffold — a temporary system being replaced by universal design and
 *   multi-tiered support with a generational sunset. The theater ratio (0.48)
 *   is moderate: while special education documentation and IEP processes
 *   contain performative elements, the coordination function is more
 *   substantial than in pure theaters. The extractiveness (0.58) reflects
 *   genuine asymmetry in who benefits and who bears costs, with families and
 *   children absorbing suppression while institutions consolidate authority
 *   and resources.
 *
 * KEY AGENTS:
 *   - Children with developmental disabilities: Primary victim (powerless/trapped) — locked into categorical pathways with stigma and foreclosed expectations
 *   - Families navigating systems: Primary victim (moderate/constrained) — face resource barriers, bureaucratic suppression, and dependence on the system extracting from them
 *   - Educational institutions: Primary beneficiary (institutional/arbitrage) — receive diagnosis-contingent funding, simplify resource allocation, reduce liability
 *   - Medical-diagnostic complex: Secondary beneficiary (institutional/arbitrage) — increase diagnostic volume, build research populations, expand service justification
 *   - Disability justice movements: Organized alternative (organized/constrained) — building sunset pathways through universal design, multi-tiered support, identity-affirming frameworks
 *   - Special education bureaucracy: Institutional actor (institutional/arbitrage) — maintains performative apparatus through legal entrenchment and path dependence
 *   - Neurodiversity movement: Alternative framing agent (powerful/mobile) — reframes clustering as contingent institutional arrangement, not inherent necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developmental_disability_clustering, 0.58).
domain_priors:suppression_score(developmental_disability_clustering, 0.65).
domain_priors:theater_ratio(developmental_disability_clustering, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developmental_disability_clustering, extractiveness, 0.58).
narrative_ontology:constraint_metric(developmental_disability_clustering, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(developmental_disability_clustering, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developmental_disability_clustering, tangled_rope).
narrative_ontology:human_readable(developmental_disability_clustering, "Developmental Disability Clustering in Educational and Social Systems").
narrative_ontology:topic_domain(developmental_disability_clustering, "social_policy/disability_justice/education").

domain_priors:requires_active_enforcement(developmental_disability_clustering).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developmental_disability_clustering, educational_institutions).
narrative_ontology:constraint_beneficiary(developmental_disability_clustering, service_bureaucracies).
narrative_ontology:constraint_beneficiary(developmental_disability_clustering, research_funding_structures).
narrative_ontology:constraint_victim(developmental_disability_clustering, children_with_developmental_disabilities).
narrative_ontology:constraint_victim(developmental_disability_clustering, families_navigating_systems).
narrative_ontology:constraint_victim(developmental_disability_clustering, alternative_support_pathways).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DIAGNOSED CHILD (SNARE) — Once clustered into disability diagnosis categories, the child faces structural entrapment. School placement, curriculum track, peer grouping, and resource allocation are locked by the diagnosis. Exit from the category means loss of accommodations and services, creating a false choice between classification trap and abandonment. The child bears maximum extraction through stigma, lowered expectations, and foreclosed opportunities, with minimal exit options.
constraint_indexing:constraint_classification(developmental_disability_clustering, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE FAMILY (SNARE/TANGLED ROPE HYBRID) — Families navigate a system where diagnosis is simultaneously a gateway to services and a mechanism of constraint. Obtaining diagnosis requires resources (assessment costs, specialist access), yet the diagnosis triggers system control. Families experience suppression through bureaucratic documentation requirements, service rationing, and paternalistic decision-making. Exit from the system means losing services; staying in means accepting the system's authority over educational and developmental decisions. High extraction paired with genuine dependence on the very system extracting from them.
constraint_indexing:constraint_classification(developmental_disability_clustering, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SCHOOL SYSTEM (TANGLED ROPE) — Schools benefit from disability clustering through funding formulas that reward documented disability counts, bureaucratic simplification (one diagnosis = predetermined resource allocation pathway), and liability reduction (documented accommodations shift responsibility). Schools simultaneously coordinate genuine peer grouping and resource allocation functions. The system experiences the constraint as coordination with asymmetric extraction — the clustering mechanism enables both genuine educational coordination AND revenue extraction through documentation systems.
constraint_indexing:constraint_classification(developmental_disability_clustering, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL-DIAGNOSTIC COMPLEX (TANGLED ROPE) — Clinicians, researchers, and assessment providers benefit from clustering through increased diagnostic volume, research populations, and service provision justifications. The diagnostic system genuinely coordinates clinical communication and enables intervention design. But it also extracts through diagnostic expansion creep, normalization of medicalized categories, and institutional dependence on disability prevalence. The extraction runs toward providers and researchers; the coordination function (shared diagnostic language) is genuine but subordinate to the extraction mechanism.
constraint_indexing:constraint_classification(developmental_disability_clustering, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NEURODIVERSITY MOVEMENT (ROPE) — Organized alternative framing (neurodiversity, identity-affirming models) sees the constraint as a pure coordination problem that has been solved better elsewhere. The movement experiences low extraction because it can exit to alternative frameworks (social model, strengths-based pedagogy, identity-first language). High mobility and organization enable reframing of the clustering mechanism as contingent, not natural. This perspective generates the scaffolding logic for sunset.
constraint_indexing:constraint_classification(developmental_disability_clustering, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DISABILITY JUSTICE ORGANIZATIONS (SCAFFOLD) — Organized disability justice movements see the clustering constraint as a temporary coordination failure with a sunset mechanism: multi-tiered support systems, universal design for learning, and identity-affirming frameworks are proving viable alternatives. The constraint persists through institutional inertia and funding path dependence, but organized pressure is building exit pathways. Exit options are constrained by institutional resistance, but the organizations have sufficient power and coordinated action to see the sunset as achievable within a generational timeline.
constraint_indexing:constraint_classification(developmental_disability_clustering, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: SPECIAL EDUCATION BUREAUCRACY (PITON) — The special education classification system persists substantially through institutional inertia and legal entrenchment (IDEA compliance, IEP documentation rituals). The functional rationale has weakened as inclusive education and universal design have proven effective, yet the bureaucratic apparatus expands. Theater ratio is high because compliance documentation (IEP meetings, mandatory evaluations, standardized classification forms) occupies vast institutional effort with questionable functional return. The system sees itself as degraded — administrative staff recognize that much of the special education apparatus is performative — but exit is blocked by legal requirements and path dependence.
constraint_indexing:constraint_classification(developmental_disability_clustering, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL VARIATION VIEW (MOUNTAIN) — From a civilizational perspective, developmental variation is inherent to human populations. Some clustering is necessary to coordinate support allocation. This perspective risks naturalizing the specific clustering mechanism (diagnostic categories, institutional enrollment pathways, funding formulas) as inevitable or natural. The engine's false summit detector will likely flag this as naturalization of a contingent institutional arrangement, revealing that the clustering mechanism is constructed, not inherent.
constraint_indexing:constraint_classification(developmental_disability_clustering, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developmental_disability_clustering_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(developmental_disability_clustering, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(developmental_disability_clustering, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(developmental_disability_clustering, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(developmental_disability_clustering, TR),
    TR >= 0.70.

:- end_tests(developmental_disability_clustering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The clustering mechanism concentrates benefits toward institutions (schools, providers, researchers) and costs toward families and children. The extracted value includes career trajectory alterations (lowered expectations, reduced peer integration affecting social development), identity costs (stigma, internalization of deficit framing), and opportunity costs (alternative educational pathways foreclosed by categorical placement). However, extractiveness is not maximal (0.72+) because some genuine coordination function persists — schools do allocate resources more effectively with diagnostic information, and some families gain meaningful access to support through the diagnostic gateway. The trajectory shows increasing extractiveness over the interval (0.32→0.58) reflecting diagnostic expansion creep, where more conditions are classified as disabilities, and behavioral expectations expand, pulling more children into the system. Suppression (0.65): High. Multiple barriers prevent exit or alternatives: (1) legal entrenchment — IDEA requirements, state special education regulations mandate diagnostic categorization; (2) resource concentration — alternative support pathways (universal design, private services) require resources beyond family reach; (3) bureaucratic dependence — obtaining services requires navigating diagnostic documentation systems; (4) social barriers — choosing non-categorical pathways signals rejection of available help, creating social friction. Theater ratio (0.48): Moderate. Special education compliance activities (IEP meetings, mandatory evaluations, documentation) are substantial but not purely performative — they do coordinate resource allocation and document accommodations. However, the expansion of documentation requirements outpaced genuine functional changes, indicating increasing theater. The trajectory shows rising theater (0.35→0.48) as compliance burden grows relative to functional changes in teaching practice.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap exists between the powerless diagnosed child (snare: trapped, maximum extraction) and the institutional school system (tangled rope: arbitrage mobility, beneficiary status). The child experiences the constraint as an inescapable categorization mechanism; the school experiences it as flexible coordination with beneficial funding implications. The family's constrained position produces a snare-tangled rope hybrid — they depend on the system while it extracts from them. The organized disability justice perspective generates the scaffolding logic: the constraint appears solvable within a generational timeframe through proven alternative mechanisms. The special education bureaucracy's piton perspective reveals institutional degradation — administrators recognize that much of the special education apparatus is performative compliance rather than functional change. The neurodiversity movement's rope perspective reframes the entire constraint as contingent, not natural — establishing that alternative framings (identity-affirming, social model) are viable. The false summit detection occurs at the analytical civilizational perspective: the risk is treating developmental variation as inherent necessity requiring categorical clustering, when in fact the clustering mechanism is an institutional choice with alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to extraction flow. Powerless/trapped children experience maximum extraction (d ≈ 0.95) because they have no exit options and bear all costs of the clustering mechanism. Constrained families (moderate power, constrained exit) experience high extraction (d ≈ 0.75) — they depend on services accessed through diagnosis while bearing suppression and control costs. Institutional beneficiaries (schools, providers) with arbitrage exit options experience low or negative extraction (d ≈ 0.20) — they can exit to alternative models (integrate into general education, reduce service volume) but choose not to because the current system extracts value. Organized agents (disability justice organizations) with constrained exit but growing power experience moderate extraction (d ≈ 0.55) — they are building alternative pathways and see a sunset. The analytical observer (analytical/analytical) at civilizational scope risks d ≈ 0.72 (treating variation as natural) but the structural data reveals this as false summit — the clustering is contingent on institutional design, not inherent.
 *
 * MANDATROPHY ANALYSIS:
 *   The developmental disability clustering constraint resolves the mandatrophy through perspectival decomposition. No single type is correct — the constraint IS a snare from the trapped child's view, IS a tangled rope from the school's view, IS a scaffold from organized disability justice movements, IS a piton from the bureaucratic system's internal view. The false summit at the civilizational analytical perspective is critical: if the constraint naturalizes clustering as inherent to human variation, it blocks recognition that the clustering mechanism is constructed and changeable. The mandatrophy resolution requires honoring the real experience of each perspective while recognizing that the tangled rope classification at the institutional level (schools + medical complex) is the locus of decision-making power. Schools and providers could shift to multi-tiered universal design systems; the fact that they have not indicates that the extraction benefits (funding, simplified allocation, research volume) outweigh coordination gains. The constraint persists because institutional actors benefit from it, not because it is inevitable or natural. The sunset mechanism (disability justice, universal design adoption) is structural and achievable — multiple countries have reduced diagnostic clustering while maintaining or improving educational outcomes. The mandatrophy is resolved by recognizing that the constraint is a tangled rope that could become a rope (pure coordination) or a scaffold (temporary support with sunset) if institutional beneficiaries' incentives shifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diagnostic_validity_versus_service_access,
    'Are diagnostic categories tracking real neurobiological differences, or are they constructed to match available service categories?',
    'Longitudinal natural history studies of diagnosed children; comparison of diagnostic category prevalence with predicted epidemiology from genetic and environmental risk factors; cross-cultural diagnostic variation analysis',
    'If categories track real differences: clustering may be inevitable coordination mechanism. If categories are service-contingent constructions: clustering is institutional extraction mechanism (Snare classification strengthened). If mixed: clustering is tangled rope with significant extractive overlay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_validity_versus_service_access, empirical, 'Whether diagnostic categories reflect neurobiological reality or service system architecture').

omega_variable(
    alternative_allocation_effectiveness,
    'Can multi-tiered universal design systems allocate resources as effectively as categorical diagnosis-based systems without diagnostic clustering?',
    'Randomized comparison of schools implementing universal design for learning versus traditional special education; outcome tracking (academic progress, inclusion rates, social integration, long-term employment); cost-effectiveness analysis of alternative allocation mechanisms',
    'If effective: scaffold perspective confirmed — sunset is achievable. If ineffective: clustering may be necessary coordination despite extraction costs. If partially effective: hybrid model may minimize extraction while maintaining coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_allocation_effectiveness, empirical, 'Whether universal design systems can replace categorical diagnosis for resource allocation').

omega_variable(
    stigma_persistence_mechanism,
    'Does diagnostic stigma persist because of inherent properties of categorical systems, or because of implementation and social attitudes that could change?',
    'Comparative analysis of stigma reduction interventions; longitudinal tracking of stigma change independent of diagnostic system change; identity-affirming implementation in existing diagnostic systems; cross-cultural comparison of societies with different diagnostic clustering norms',
    'If inherent: categorical systems require stigma acceptance cost. If implementational: stigma could be reduced while maintaining diagnosis-based resource allocation. If attitudinal: stigma reduction may require cultural change more than system change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stigma_persistence_mechanism, conceptual, 'Whether stigma is inherent to categorical diagnosis or implementational').

omega_variable(
    suppression_mechanism_structural_versus_internalized,
    'Is the suppression of families and children structural (external barriers to exit and alternatives) or internalized (identity fusion with the diagnosed label, reduced expectations internalized by families)?',
    'Post-exit suppression trajectory analysis: if family suppression persists after leaving the system, it indicates internalized components; identity interviews with families; comparison of post-diagnosis behavioral change in diagnosed versus undiagnosed children with equivalent support needs',
    'If primarily structural: system change can reduce suppression. If significantly internalized: suppression persists even after system reform; requires long-term cultural change and identity-work. If mixed: both system reform and identity work needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_versus_internalized, empirical, 'Structural versus internalized suppression mechanisms in disability clustering').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developmental_disability_clustering, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devdis_tr_t0, developmental_disability_clustering, theater_ratio, 0, 0.35).
narrative_ontology:measurement(devdis_tr_t15, developmental_disability_clustering, theater_ratio, 15, 0.42).
narrative_ontology:measurement(devdis_tr_t30, developmental_disability_clustering, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(devdis_be_t0, developmental_disability_clustering, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(devdis_be_t15, developmental_disability_clustering, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(devdis_be_t30, developmental_disability_clustering, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developmental_disability_clustering, resource_allocation).
narrative_ontology:affects_constraint(developmental_disability_clustering, school_segregation_infrastructure).
narrative_ontology:affects_constraint(developmental_disability_clustering, disability_service_rationing).
narrative_ontology:affects_constraint(developmental_disability_clustering, medicalization_of_developmental_variation).

% DUAL FORMULATION NOTE:
% Developmental disability clustering is upstream of specific service rationing and segregation mechanisms. The clustering system creates the structural basis for rationing and segregation by establishing categorical divisions. Separate constraint stories exist for school segregation infrastructure (which institutionalizes the clustering mechanism) and disability service rationing (which uses clustering for allocation). Those downstream constraints would be affected by changes to this clustering mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(developmental_disability_clustering, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
