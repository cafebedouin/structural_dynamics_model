% ============================================================================
% CONSTRAINT STORY: tropical_forest_tenure_insecurity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tropical_forest_tenure_insecurity, []).

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
 *   constraint_id: tropical_forest_tenure_insecurity
 *   human_readable: Tropical Forest Tenure Insecurity
 *   domain: environmental/political_economy/land_rights
 *
 * SUMMARY:
 *   Tropical forest tenure insecurity creates a structural lock that benefits
 *   extraction companies, land speculators, and rent-seeking governments
 *   while imposing catastrophic costs on indigenous communities, smallholder
 *   farmers, and forest ecosystems. The constraint operates by rendering
 *   indigenous and customary land rights legally invisible, recognizing only
 *   state title (which governments can allocate to preferred concession
 *   holders) and foreign corporate claims. This creates a coordination
 *   mechanism that solves specific actor problems — governments can allocate
 *   rents, companies can access resources cheaply, speculators can accumulate
 *   land optionality — while simultaneously trapping forest-dependent
 *   populations in a regime of resource insecurity and cultural degradation.
 *   The constraint exhibits classical snare characteristics: high extraction
 *   (beneficiaries capture values that would otherwise flow to forest
 *   communities), high suppression (legal systems actively exclude customary
 *   tenure from recognition), and increasing theater (international
 *   conservation governance, environmental impact assessments, and free prior
 *   informed consent protocols that operate as performative legitimation
 *   while tenure insecurity persists). The extractiveness has increased over
 *   40 years as access to forest resources has become economically more
 *   valuable and as land speculation has intensified.
 *
 * KEY AGENTS:
 *   - Indigenous Communities: Primary victims (powerless/trapped) — centuries of stewardship negated by legal non-recognition; relocation culturally and economically impossible
 *   - Smallholder Farmers and Forest-Dependent Populations: Secondary victims (moderate/constrained) — informal tenure offers no legal protection; formalization costs are extreme; productive capacity declining through environmental degradation
 *   - Extraction Companies and Concession Holders: Primary beneficiaries (institutional/arbitrage) — access valuable resources through low-cost state concessions; can exit to other jurisdictions if regulatory environment shifts
 *   - National Governments and State Rent-Seekers: Primary beneficiaries (institutional/arbitrage) — maintain weak tenure recognition to control concession allocation and extract rents through licensing fees and corruption
 *   - Land Speculation Investors: Secondary beneficiaries (powerful/mobile) — accumulate land optionality during uncertainty; profits from eventual consolidation and conversion
 *   - Environmental and Land Rights Organizations: Mixed (organized/constrained) — benefit from advocacy opportunities while bearing moral witnessing costs; coordinate information but may extract status from victimhood narratives
 *   - International Conservation Institutions: Institutional theater maintainers (institutional/arbitrage) — issue declarations and frameworks but lack enforcement capacity; may constitute secondary extraction layer through conditionality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tropical_forest_tenure_insecurity, 0.68).
domain_priors:suppression_score(tropical_forest_tenure_insecurity, 0.72).
domain_priors:theater_ratio(tropical_forest_tenure_insecurity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tropical_forest_tenure_insecurity, extractiveness, 0.68).
narrative_ontology:constraint_metric(tropical_forest_tenure_insecurity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tropical_forest_tenure_insecurity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tropical_forest_tenure_insecurity, snare).
narrative_ontology:human_readable(tropical_forest_tenure_insecurity, "Tropical Forest Tenure Insecurity").
narrative_ontology:topic_domain(tropical_forest_tenure_insecurity, "environmental/political_economy/land_rights").

domain_priors:requires_active_enforcement(tropical_forest_tenure_insecurity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tropical_forest_tenure_insecurity, extraction_companies).
narrative_ontology:constraint_beneficiary(tropical_forest_tenure_insecurity, land_speculation_investors).
narrative_ontology:constraint_beneficiary(tropical_forest_tenure_insecurity, national_governments_rent_seeking).
narrative_ontology:constraint_victim(tropical_forest_tenure_insecurity, indigenous_communities).
narrative_ontology:constraint_victim(tropical_forest_tenure_insecurity, smallholder_farmers).
narrative_ontology:constraint_victim(tropical_forest_tenure_insecurity, forest_dependent_populations).
narrative_ontology:constraint_victim(tropical_forest_tenure_insecurity, ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS COMMUNITIES (SNARE) — Trapped by lack of legal title despite centuries of occupancy and stewardship. Cannot exit because relocation is culturally catastrophic and economically impossible. Bear full extraction cost as land is converted to logging, mining, or agriculture. Suppression is structural: formal property regimes recognize only state title or foreign corporate concessions, not customary tenure. Zero degrees of freedom in this constraint.
constraint_indexing:constraint_classification(tropical_forest_tenure_insecurity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALLHOLDER FARMERS (SNARE) — Constrained by informal tenure with no legal recognition. High cost to formalize: titling requires capital, literacy, bureaucratic navigation. Extraction occurs through involuntary appropriation, environmental degradation reducing productive capacity, and resource conflicts. Can technically relocate but relocation costs are extreme. Suppression operates through legal exclusion — the tenure system recognizes their presence only as trespassers.
constraint_indexing:constraint_classification(tropical_forest_tenure_insecurity, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXTRACTION COMPANIES (ROPE) — Experience tenure insecurity as a coordination mechanism that solves their core problem: accessing valuable timber and mineral resources without paying landowners or negotiating complex indigenous rights. Low-cost title via government concessions creates the appearance of legitimacy. Can exit by investing elsewhere. Net beneficiary — extraction flows toward this agent. Theater involves environmental impact assessments and consultation protocols that are largely performative.
constraint_indexing:constraint_classification(tropical_forest_tenure_insecurity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NGOS AND ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized agents with constrained exit (funding and staffing dependencies). Benefit from tenure insecurity as it creates demand for their services and advocacy work, yet also bear costs through moral witnessing and resource constraints. Genuinely coordinate information sharing and rights documentation, while simultaneously extracting status and resources from victimhood narratives. Suppression is imposed by state actors hostile to land rights advocacy.
constraint_indexing:constraint_classification(tropical_forest_tenure_insecurity, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NATIONAL GOVERNMENTS (ROPE) — Experience tenure insecurity as a coordination solution for revenue generation and political patronage. Maintaining weak title recognition allows governments to allocate concessions to favored companies, extract bribes and licensing fees, and avoid costly land reforms. Can arbitrage between extraction revenue and conservation commitments. The constraint solves the government's core problem: controlling territory and extracting rents without confronting internal land redistribution politics.
constraint_indexing:constraint_classification(tropical_forest_tenure_insecurity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL INSTITUTIONS (PITON) — Conventions (CBD, UNDRIP, Paris Agreement) declare support for indigenous land rights and forest conservation, but implementation is largely theatrical. Institutions lack enforcement mechanisms, and signatories face no consequences for violating commitments. The theater persists through report submission and accountability theater while tenure insecurity continues. This represents institutional inertia — the international regime was created to address the problem but has become a performative substitute for real change.
constraint_indexing:constraint_classification(tropical_forest_tenure_insecurity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilization scale, tenure insecurity in tropical forests appears to be a natural consequence of conflicting economic systems and development imperatives. Land becomes valuable when extractive technologies emerge (logging, mining); insecure tenure is portrayed as an immutable feature of the modernization process. However, this perspective risks naturalizing what is actually a contingent institutional arrangement designed to benefit specific actors. Comparative analysis reveals that secure indigenous tenure systems produce superior forest conservation outcomes, suggesting the 'natural law' framing masks political choice.
constraint_indexing:constraint_classification(tropical_forest_tenure_insecurity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tropical_forest_tenure_insecurity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tropical_forest_tenure_insecurity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tropical_forest_tenure_insecurity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tropical_forest_tenure_insecurity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tropical_forest_tenure_insecurity, TR),
    TR >= 0.70.

:- end_tests(tropical_forest_tenure_insecurity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint directly transfers resource values from forest communities to extraction companies and state actors. Over 40 years, this extraction has intensified as timber, mineral, and land values have increased. Suppression (0.72): High. The suppression is structural: legal systems actively exclude customary tenure from recognition, creating a status where forest communities have use rights without ownership rights. Formalization pathways exist but are prohibitively expensive. International legal frameworks (UNDRIP, CBD) exist but lack enforcement against national governments. Theater ratio (0.58): Moderate-high. Environmental impact assessments, consultation protocols, and conservation certifications create appearance of legitimacy while tenure insecurity persists. Free prior informed consent becomes theater when consent cannot prevent concession allocation. International treaties commit to indigenous rights while implementation is abandoned post-signature. The theater has increased over time as international scrutiny has intensified, requiring more elaborate legitimation rituals while the underlying constraint remains unchanged.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the rope experience of extraction companies and the snare experience of indigenous communities is enormous and irreducible. No single metric bridges this gap because the agents occupy structurally incompatible positions within the constraint. The government's perspective (rope) depends on the powerlessness of forest communities (who then see snare). The beneficiary's coordination depends on the victim's extraction. This is not a measurement ambiguity or a perspective-relative framing — it is a structural conflict. The analytical observer who attempts to synthesize these perspectives risks naturalizing the beneficiary's framing ('tenure insecurity is an inevitable feature of economic development') rather than recognizing it as a contingent institutional arrangement that could be restructured to secure indigenous tenure while maintaining ordered forest resource management.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (extraction companies, governments, speculators) occupy positions of institutional power with arbitrage exit options — they can invest elsewhere if tenure regimes shift. Their directionality is low (d ≈ 0.15), producing negative or minimal effective extraction. They derive net benefit from the constraint. Victims (indigenous communities, smallholders) occupy positions of powerlessness with trapped or constrained exit — they cannot afford to relocate and face legal exclusion from formal tenure mechanisms. Their directionality is high (d ≈ 0.90 for trapped agents), producing maximum experienced extraction chi. The suppression metric (0.72) is particularly high for trapped agents: they face not just high exit costs but legal non-recognition that prevents formal claim-making. Environmental organizations occupy a middle position with constrained exit and mixed beneficiary/victim status — they benefit from advocacy opportunities but bear moral costs, producing d ≈ 0.60 and moderate experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that tenure insecurity genuinely exhibits both coordination and extraction functions, but the distribution is asymmetric. For extraction companies and governments, the constraint solves a real coordination problem: how to allocate valuable resources and generate state revenue without endless negotiations over land rights. From these beneficiaries' perspective, tenure insecurity appears as rope (pure coordination). For forest communities, the constraint exhibits zero coordination function — it creates conflict, insecurity, and resource loss with no offsetting benefit. From their perspective, it is snare (pure extraction). The constraint is not primarily coordination that has been corrupted by extraction; rather, it is an extractive arrangement dressed in coordination language ('efficient resource allocation,' 'economic development'). The mandatrophy resolves by distinguishing coordination-for-beneficiaries (real but asymmetric) from coordination-for-victims (nonexistent). This is the textbook structure of tangled rope seen from the beneficiary's perspective but snare seen from the victim's perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_tenure_formalization_paradox,
    'Does formalizing indigenous land titles into state legal systems actually protect indigenous communities or does it expose them to new extraction mechanisms (property taxation, foreclosure, commodification)?',
    'Longitudinal comparison of communities with formalized titles vs informal customary tenure; tracking of subsequent land loss, economic outcomes, and cultural practice changes',
    'If formalization protects: tenure insecurity classification shifts toward a solvable coordination problem. If formalization enables new extraction: the snare persists through different institutional channels, making the constraint fundamentally structural rather than documentary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_tenure_formalization_paradox, empirical, 'Whether land title formalization protects or exposes indigenous communities').

omega_variable(
    forest_dependent_populations_exit_capacity,
    'What proportion of forest-dependent populations have genuine exit capacity (ability to relocate and establish livelihood elsewhere) versus what proportion are genuinely trapped by cultural, economic, or geographic factors?',
    'Demographic analysis of migration patterns, livelihood diversification rates, secondary skills acquisition in forest-dependent communities; comparison with data on voluntary versus coerced displacements',
    'If exit capacity is high: classification shifts from trapped to constrained, reducing experienced extraction chi. If exit capacity is low: trapped classification confirmed, indicating maximum suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forest_dependent_populations_exit_capacity, empirical, 'Exit capacity of forest-dependent populations').

omega_variable(
    concession_allocation_corruption_mechanism,
    'To what extent does tenure insecurity serve as intentional policy (protecting government''s ability to allocate rents) versus unintended consequence of weak state capacity?',
    'Analysis of government decision-making processes; comparison of tenure security across countries with similar state capacity but different intentionality in tenure policies; examination of revenue flows from concession allocations',
    'If intentional policy: the snare is consciously maintained by state actors and constitutes a deliberate extraction mechanism. If unintended consequence: tenure insecurity is a byproduct of weak capacity rather than designed extraction, potentially subject to different remediation strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concession_allocation_corruption_mechanism, conceptual, 'Intentionality of government tenure insecurity maintenance').

omega_variable(
    international_conservation_conditionality_extraction,
    'Do international conservation agreements that condition development aid on forest protection constitute a secondary snare that extracts conservation labor from developing states while maintaining tenure insecurity?',
    'Analysis of conditionality terms, compliance burden, and outcomes; comparison of conservation outcomes under conditionality vs unconditional support; tracking of state revenues lost to conservation requirements',
    'If yes: tenure insecurity is embedded in a multi-layer extraction system where international institutions extract conservation work while national governments extract rents. If no: international support genuinely addresses tenure insecurity and forest protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_conservation_conditionality_extraction, conceptual, 'Whether international conservation conditionality constitutes secondary extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tropical_forest_tenure_insecurity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tfti_tr_t0, tropical_forest_tenure_insecurity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(tfti_tr_t20, tropical_forest_tenure_insecurity, theater_ratio, 20, 0.48).
narrative_ontology:measurement(tfti_tr_t40, tropical_forest_tenure_insecurity, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(tfti_be_t0, tropical_forest_tenure_insecurity, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(tfti_be_t20, tropical_forest_tenure_insecurity, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(tfti_be_t40, tropical_forest_tenure_insecurity, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tropical_forest_tenure_insecurity, resource_allocation).
narrative_ontology:affects_constraint(tropical_forest_tenure_insecurity, carbon_credit_extraction_from_tropical_forests).
narrative_ontology:affects_constraint(tropical_forest_tenure_insecurity, indigenous_cultural_degradation_through_dispossession).
narrative_ontology:affects_constraint(tropical_forest_tenure_insecurity, tropical_biodiversity_loss_acceleration).

% DUAL FORMULATION NOTE:
% Tenure insecurity is downstream of colonialism and state-formation processes but represents a distinct structural constraint. The upstream constraint (state monopoly on land title recognition) has its own extractive properties; tenure insecurity represents the application of that upstream constraint to specific forest contexts where valuable resources exist and customary claims are strong. Decomposition follows the ε-invariance principle: the general state land monopoly has different ε than tenure insecurity in specifically tropical forest contexts with high resource value and strong indigenous claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tropical_forest_tenure_insecurity, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
