% ============================================================================
% CONSTRAINT STORY: federal_resource_extraction_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_resource_extraction_authority, []).

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
 *   constraint_id: federal_resource_extraction_authority
 *   human_readable: Federal Resource Extraction Authority
 *   domain: political_economy/environmental_governance
 *
 * SUMMARY:
 *   Federal resource extraction authority is the institutional power to
 *   allocate public lands for mineral, fossil fuel, timber, and water
 *   extraction on behalf of the national interest. This constraint exhibits
 *   structurally distinct classification types from different perspectives,
 *   revealing how the same formal authority appears as coordination,
 *   extraction, degraded ritual, and natural law depending on the observer's
 *   structural position. The constraint operates through federal agencies
 *   (primarily Bureau of Land Management, U.S. Forest Service, Department of
 *   Interior) that hold statutory authority to permit extraction on public
 *   lands. The constraint's evolution shows increasing extractiveness (0.35 →
 *   0.58 over 40 years) and rising theater ratio (0.25 → 0.55), indicating
 *   that environmental coordination protections have become increasingly
 *   performative as extraction protocols have expanded.
 *
 * KEY AGENTS:
 *   - Indigenous Communities: Primary victim (powerless/trapped) — bear environmental and cultural costs of extraction on ancestral lands with no authority to refuse
 *   - Local Environmental Stewardship Communities: Secondary victim (moderate/constrained) — constrained by federal override of local environmental governance; some agency through coalition and litigation
 *   - Federal Extraction Agencies: Primary beneficiary (institutional/arbitrage) — constraint constitutes their statutory authority and budget; arbitrage between conservation mandates and extraction revenue
 *   - Resource Extraction Corporations: Primary beneficiary (institutional/arbitrage) — benefit from federal authority providing legal certainty, capital access, and standardized permitting
 *   - National Treasury: Beneficiary (institutional/arbitrage) — receives extraction revenues that fund federal operations
 *   - Environmental Advocacy Organizations: Powerful constrained actor (powerful/mobile) — coordinate conservation standards but face asymmetric litigation and agency capture
 *   - Historical Conservation Regime: Institutional actor in degradation (institutional/arbitrage) — early conservation purpose (ecosystem protection) has atrophied into theater masking extraction expansion
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent authority as immutable resource scarcity law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_resource_extraction_authority, 0.58).
domain_priors:suppression_score(federal_resource_extraction_authority, 0.62).
domain_priors:theater_ratio(federal_resource_extraction_authority, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_resource_extraction_authority, extractiveness, 0.58).
narrative_ontology:constraint_metric(federal_resource_extraction_authority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federal_resource_extraction_authority, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_resource_extraction_authority, tangled_rope).
narrative_ontology:human_readable(federal_resource_extraction_authority, "Federal Resource Extraction Authority").
narrative_ontology:topic_domain(federal_resource_extraction_authority, "political_economy/environmental_governance").

domain_priors:requires_active_enforcement(federal_resource_extraction_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_resource_extraction_authority, federal_extraction_agencies).
narrative_ontology:constraint_beneficiary(federal_resource_extraction_authority, resource_extraction_corporations).
narrative_ontology:constraint_beneficiary(federal_resource_extraction_authority, national_treasury).
narrative_ontology:constraint_victim(federal_resource_extraction_authority, indigenous_communities).
narrative_ontology:constraint_victim(federal_resource_extraction_authority, local_environmental_commons).
narrative_ontology:constraint_victim(federal_resource_extraction_authority, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS COMMUNITIES (SNARE) — Structurally trapped by federal land authority and historical treaty violations. Cannot exit resource extraction decisions on their lands. Bear maximum extraction costs (environmental destruction, cultural loss, health impacts) with minimal benefit. No meaningful coordination function — constraint operates through coercive federal override of local autonomy.
constraint_indexing:constraint_classification(federal_resource_extraction_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL ENVIRONMENTAL STEWARDSHIP (TANGLED ROPE) — Constrained by regulatory complexity, litigation costs, and political asymmetry. But some coordination function exists: environmental protection movements coordinate across jurisdictions to establish baseline standards. Extraction is real (imposed extraction protocols reduce local autonomy) but not total — some agency remains through coalition building and regulatory comment periods.
constraint_indexing:constraint_classification(federal_resource_extraction_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RESOURCE EXTRACTION CORPORATIONS (ROPE) — Experience constraint as coordination mechanism. Federal authority provides legal certainty, capital access, security of mineral claims, and standardized permitting. Effectively arbitrage between federal permitting and global commodity markets. Net beneficiary — constraint channels capital and legitimacy toward extraction entities.
constraint_indexing:constraint_classification(federal_resource_extraction_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FEDERAL EXTRACTION AGENCIES (ROPE) — Primary beneficiary. Constraint constitutes their statutory authority and budget justification. Perceive resource extraction as coordination: allocating public resources, managing competing uses, generating revenue for national treasury. No exit option — agencies ARE the constraint. Arbitrage between conservation mandates and extraction revenue.
constraint_indexing:constraint_classification(federal_resource_extraction_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL ADVOCACY ORGS (TANGLED ROPE) — Powerful actors (organized, well-funded) but constrained by federal agency capture and asymmetric litigation costs. Real coordination function: conservation standards, environmental impact assessment, species protection. But extraction mechanism persists: agencies systematically underweight environmental costs relative to extraction revenues. Coalition power provides agency and some exit options (litigation, regulatory comment, political pressure), so not snare-level, but asymmetric extraction is clear.
constraint_indexing:constraint_classification(federal_resource_extraction_authority, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL CONSERVATION REGIME (PITON) — Early conservation movement (1906 National Monuments Act through 1970s Endangered Species Act) established genuine coordination function: protecting ecosystems and species from unregulated extraction. But the functional purpose has atrophied — federal agencies now use conservation designations as theater masking underlying extraction expansion (permits issued within 'protected' lands, mitigation banking that displaces extraction, semantic redefinition of 'conservation'). The conservation apparatus persists through institutional inertia and provides legitimacy, but core function is degraded.
constraint_indexing:constraint_classification(federal_resource_extraction_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational horizon, resource extraction authority appears immutable: mineral wealth is finite, extraction from public lands funds national infrastructure, geopolitical competition for resources makes domestic extraction strategically necessary. This perspective naturalizes the constraint as law-like — extraction happens because resources are scarce and valuable. However, the structural data reveals this as false summit: the extractiveness (0.58) and suppression (0.62) indicate contingent institutional arrangements, not natural limits. The constraint is socially constructed authority, not natural law.
constraint_indexing:constraint_classification(federal_resource_extraction_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_resource_extraction_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_resource_extraction_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_resource_extraction_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_resource_extraction_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_resource_extraction_authority, TR),
    TR >= 0.70.

:- end_tests(federal_resource_extraction_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Federal extraction authority systematically transfers environmental and social costs (indigenous displacement, ecosystem damage, climate impacts) to powerless and constrained agents while concentrating revenues and strategic benefits among beneficiaries (federal agencies, extraction corporations, national treasury). The increase from 0.35 to 0.58 over 40 years reflects accumulating extraction expansion despite nominal conservation growth. Not maximum extraction (0.66+) because some coordination function remains: environmental impact assessment creates real constraints on agency discretion, and conservation designations do prevent some extraction. Suppression (0.62): High. Multiple barriers prevent agents from exiting or resisting extraction: federal statutory authority overrides local law, indigenous treaty violations create legal asymmetry, litigation costs exceed victim-group funding, political capture concentrates decision-making, and information asymmetries (agency expertise vs. community knowledge) favor extraction. Theater ratio (0.55): Moderate-high, increasing over time. Environmental assessment procedures, mitigation banking, conservation designations, and species protection appear to reduce extraction but often function as theater: permits issued within protected designations through reinterpretation, mitigation offsets displace extraction rather than prevent it, protected species do not prevent extraction in 95% of cases. The rise in theater from 0.25 to 0.55 reflects institutional growth in conservation bureaucracy without proportional reduction in extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint reveals inversion between institutional narrative and victim experience. Institutions narrate federal extraction as coordination (managing competing uses, funding public goods, maintaining strategic resource independence). Victims experience it as extraction (displacement, environmental destruction, cultural loss imposed without consent). Advocates experience hybrid (coordination standards exist but systematically underenforced; extraction expansion continues under conservation cover). Piton perspective reveals that conservation regime, originally designed to prevent extraction, now functions primarily to legitimize it. Analytical perspective risks naturalizing the constraint as immutable scarcity-driven necessity, which structural data contradicts: the constraint is enforced statutory authority, not natural law. The gap exposes how power asymmetry shapes which perspective is treated as 'realistic' (beneficiary institutional view) versus 'activist' (victim views).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural relationship to extraction flows. Indigenous communities (victims, trapped) experience maximum d ≈ 0.95, f(d) ≈ 1.42, producing maximum chi. Federal agencies (beneficiaries, arbitrage) experience minimum d ≈ 0.05, f(d) ≈ -0.12, producing negative chi (they are subsidized by the constraint). Environmental advocates (powerful, mobile, mixed beneficiary-victim) experience intermediate d ≈ 0.60, f(d) ≈ 0.95, producing moderate chi. Extraction corporations (beneficiaries, arbitrage) experience low d ≈ 0.10, f(d) ≈ -0.03, producing negative chi. National scope (σ(S) = 1.0) applies uniform scaling across all perspectives. The directionality chain reveals why beneficiaries perceive rope (low extraction experienced) while victims perceive snare (high extraction experienced) from identical structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits classic mandatrophy signature: the stated function (coordinate resource uses while protecting environment and indigenous rights) contradicts the measured function (systematically extract environmental costs, transfer to powerless agents, concentrate benefits to institutional and corporate beneficiaries). The constraint is not resolving a genuine coordination problem — environmental standards, indigenous sovereignty, and extraction are structurally opposed. Federal authority resolves this by subordinating the first two to the third through institutional mechanism (agency capture, treaty violation, litigation asymmetry, theater). The mandatrophy resolves by recognizing that extraction IS the primary function, and conservation is the cover story. The tangled rope classification is accurate: genuine coordination functions exist (environmental assessment does constrain some extraction, conservation designations do prevent some extraction) but extraction mechanism dominates. The constraint is not pure snare because some victims benefit from environmental standards and legal recognition. But the overall vector is extraction — toward concentrating benefits, dispersing costs, and expanding extraction over time. The measurement trajectory (extractiveness rising from 0.35 to 0.58, theater rising from 0.25 to 0.55) confirms: extraction is expanding while performance of coordination is increasing, indicating that theater is substituting for genuine function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_capture_mechanism,
    'To what extent is federal extraction authority captured by industry versus genuinely exercising independent conservation judgment?',
    'Comparative analysis: permit denial rates for similar environmental impacts across administered lands; career path analysis of agency personnel (post-government employment in extraction sector); cost-benefit analysis methodology (environmental valuation assigned in environmental impact statements)',
    'If captured (> 70% personnel revolving door): constraint reclassifies as pure snare from all non-beneficiary perspectives. If independent (< 30% capture): constraint legitimately coordinates multiple interests, tangled rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_capture_mechanism, empirical, 'Degree of federal agency capture by extraction industry').

omega_variable(
    indigenous_sovereignty_entanglement,
    'Is federal extraction authority on tribal lands a coordination problem (settling competing uses) or an extraction/colonization mechanism (imposing external authority over self-determination)?',
    'Historical analysis of treaty compliance; documentary evidence of tribal consultation that produced binding constraint (vs. performative consultation); comparison of extraction outcomes from tribally-managed lands versus federally-managed lands with indigenous populations',
    'If colonization: indigenous perspective reclassifies to most severe snare. If coordination: indigenous perspective classifies as tangled rope (extracted but benefiting from negotiated standards). Current classification assumes partial coordination, but historical evidence may contradict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_sovereignty_entanglement, conceptual, 'Whether federal extraction authority on tribal lands constitutes coordination or colonial imposition').

omega_variable(
    mitigation_banking_effectiveness,
    'Do environmental mitigation requirements and mitigation banking actually reduce net environmental extraction, or do they constitute theater that permits greater extraction under cover of ''offsets''?',
    'Comparison of ecological outcomes in fully protected areas versus mitigated extraction areas; analysis of mitigation banking success rates and whether offset sites achieve stated ecological goals',
    'If effective: suppression is lower than measured (agents have real alternatives through mitigation). If theater: suppression remains high and piton classification confirmed (conservation regime is degraded).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_banking_effectiveness, empirical, 'Whether mitigation banking reduces net environmental extraction or enables expansion').

omega_variable(
    generational_benefit_distribution,
    'Do extraction revenues to national treasury provide benefits to future generations that justify present extraction, or do intergenerational costs (climate, ecosystem collapse, resource depletion) exceed any benefit?',
    'Accounting for environmental externalities in federal revenue calculations; long-term ecological trajectory analysis under current extraction rates; comparison of per-capita intergenerational wealth transfers under extraction versus conservation scenarios',
    'If benefits exceed costs: extraction classifies as rope from generational perspective. If costs exceed benefits: extraction is pure intergenerational predation, snare from any future-oriented perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_benefit_distribution, preference, 'Intergenerational cost-benefit of federal resource extraction').

omega_variable(
    renewable_energy_transition_pathway,
    'Is the constraint (federal extraction authority) an obstacle to renewable energy transition or a necessary bridge providing transition capital?',
    'Comparative modeling: federal revenue from fossil fuels directed toward renewable infrastructure versus scenario without fossil extraction; analysis of whether extraction revenue accelerates or delays energy transition',
    'If obstacle: constraint''s theater (transition bridge narrative) is revealed as cover story; extraction is pure snare from environmental perspective. If bridge: constraint legitimately coordinates energy transition, tangled rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_energy_transition_pathway, empirical, 'Whether federal extraction authority enables or obstructs renewable energy transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_resource_extraction_authority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frea_tr_t0, federal_resource_extraction_authority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(frea_tr_t20, federal_resource_extraction_authority, theater_ratio, 20, 0.42).
narrative_ontology:measurement(frea_tr_t40, federal_resource_extraction_authority, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(frea_be_t0, federal_resource_extraction_authority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(frea_be_t20, federal_resource_extraction_authority, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(frea_be_t40, federal_resource_extraction_authority, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_resource_extraction_authority, resource_allocation).
narrative_ontology:boltzmann_floor_override(federal_resource_extraction_authority, 0.18).
narrative_ontology:affects_constraint(federal_resource_extraction_authority, indigenous_sovereignty_recognition).
narrative_ontology:affects_constraint(federal_resource_extraction_authority, climate_transition_pathway).
narrative_ontology:affects_constraint(federal_resource_extraction_authority, federal_environmental_regulation).

% DUAL FORMULATION NOTE:
% Federal extraction authority is upstream of specific extraction permits and environmental regulations. Sovereignty recognition and climate transition are downstream constraints that Federal extraction authority structurally influences through statutory authority and agency capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_resource_extraction_authority, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
