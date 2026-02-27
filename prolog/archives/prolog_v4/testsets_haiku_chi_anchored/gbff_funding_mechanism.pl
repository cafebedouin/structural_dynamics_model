% ============================================================================
% CONSTRAINT STORY: gbff_funding_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gbff_funding_mechanism, []).

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
 *   constraint_id: gbff_funding_mechanism
 *   human_readable: Global Biodiversity Framework Fund (GBFF) Funding Mechanism
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The Global Biodiversity Framework Fund represents a structural tension in
 *   multilateral environmental governance: it genuinely solves a coordination
 *   problem (wealthy nations pooling resources to prevent
 *   tragedy-of-the-commons biodiversity loss) while simultaneously enforcing
 *   asymmetric extraction through conditionality, reporting regimes, and
 *   knowledge hierarchies. Developing nations and indigenous stewards gain
 *   access to conservation capital but lose fiscal autonomy and
 *   decision-making authority over land governance. The constraint's
 *   theater_ratio (0.65) reflects that GBFF's governance apparatus is
 *   substantially performative: compliance is measured by audit procedures,
 *   grant disbursement rates, and reporting compliance rather than by
 *   biodiversity outcomes. Over the first decade (2021-2030), theater has
 *   increased as administrative overhead grew faster than conservation impact
 *   measurement capacity. The fund's beneficiaries (wealthy nations,
 *   conservation NGOs, fund administrators) experience genuine coordination
 *   benefits; the primary victims (indigenous land stewards, developing
 *   nations' fiscal autonomy) experience extraction through powerlessness and
 *   structural subordination. The constraint exemplifies mandatrophy at the
 *   institutional level: wealthy nations frame GBFF as pure moral
 *   coordination ('we share the burden of global conservation'), while
 *   indigenous stewards experience it as conditionality-based extraction
 *   ('you must accept our conservation definition to access funds'). The
 *   analytical observer resolves mandatrophy by recognizing both functions
 *   are structurally real—the constraint is a genuine hybrid (Tangled Rope),
 *   not a coordination mechanism mislabeled as extraction.
 *
 * KEY AGENTS:
 *   - Wealthy Nations (US, EU, Japan, Canada): Primary beneficiary (institutional/arbitrage) — share burden of global biodiversity financing while maintaining control over strategic priorities and leverage over recipient nations
 *   - Developing Nations (Brazil, Indonesia, Congo Basin states): Primary victim (moderate/constrained) — gain conservation funding but lose fiscal autonomy and must adopt externally-designed targets
 *   - Indigenous Land Stewards (Amazon communities, Central African forest managers, Southeast Asian territories): Secondary victim (powerless/trapped) — provide 80% of global biodiversity stewardship but trapped in conditionality regimes that subordinate indigenous knowledge to conservation science frameworks
 *   - Conservation NGOs (TNC, WWF, IUCN, BirdLife): Beneficiary and administrator (organized/constrained) — benefit from GBFF funding distribution but constrained by need to maintain donor relationships; pushing toward capacity-building sunset logic
 *   - GBFF Administrative Apparatus (World Bank trust fund management, UNDP oversight, technical secretariat): Institutional actor (institutional/arbitrage) — maintains governance infrastructure; sees own procedures as increasingly performative (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risk of naturalizing wealth-based governance as inevitable global conservation mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gbff_funding_mechanism, 0.52).
domain_priors:suppression_score(gbff_funding_mechanism, 0.58).
domain_priors:theater_ratio(gbff_funding_mechanism, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gbff_funding_mechanism, extractiveness, 0.52).
narrative_ontology:constraint_metric(gbff_funding_mechanism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gbff_funding_mechanism, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gbff_funding_mechanism, tangled_rope).
narrative_ontology:human_readable(gbff_funding_mechanism, "Global Biodiversity Framework Fund (GBFF) Funding Mechanism").
narrative_ontology:topic_domain(gbff_funding_mechanism, "geopolitical/economic").

domain_priors:requires_active_enforcement(gbff_funding_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gbff_funding_mechanism, wealthy_nations).
narrative_ontology:constraint_beneficiary(gbff_funding_mechanism, conservation_ngos).
narrative_ontology:constraint_beneficiary(gbff_funding_mechanism, fund_administrators).
narrative_ontology:constraint_victim(gbff_funding_mechanism, biodiversity_conservation_capacity).
narrative_ontology:constraint_victim(gbff_funding_mechanism, developing_nations_fiscal_autonomy).
narrative_ontology:constraint_victim(gbff_funding_mechanism, indigenous_land_stewards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS LAND STEWARDS (SNARE) — Trapped in conditionality regimes; must surrender land governance to conservation narratives to access funding. No exit without losing biodiversity stewardship capacity. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.81.
constraint_indexing:constraint_classification(gbff_funding_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATIONS (TANGLED ROPE) — Benefit from conservation funding but constrained by conditional aid, reporting requirements, and externally-set priorities. Lose fiscal autonomy while gaining resources. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(gbff_funding_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WEALTHY NATIONS (ROPE) — Experience GBFF as pure coordination mechanism: pool resources, align conservation targets, avoid unilateral defection. Benefits from shared burden and global biodiversity stabilization. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(gbff_funding_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSERVATION NGOS AND SCIENTIFIC COALITION (SCAFFOLD) — See GBFF as temporary institutional framework with sunset logic: as developing nations build domestic conservation capacity and global monitoring systems mature (satellite imagery, eDNA tracking), direct conditional financing should decline. Organized actors pushing toward capacity-building handoff. d≈0.38, f(d)≈0.38, σ=1.2 → χ≈0.23.
constraint_indexing:constraint_classification(gbff_funding_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GBFF ADMINISTRATIVE APPARATUS (PITON) — The fund's reporting, audit, and governance infrastructure is substantially performative: theater_ratio=0.65 reflects that compliance audits often measure procedural adherence (grant disbursement rates, reporting submissions) rather than biodiversity outcomes. The apparatus persists through institutional inertia; genuine impact measurement remains contested. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(gbff_funding_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, GBFF simultaneously solves a real coordination problem (wealthy nations would underinvest in global biodiversity without shared burden) AND creates asymmetric extraction through conditionality and knowledge asymmetry (developing nations cannot design their own conservation strategy; must adopt externally-defined targets). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.40.
constraint_indexing:constraint_classification(gbff_funding_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gbff_funding_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gbff_funding_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gbff_funding_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gbff_funding_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gbff_funding_mechanism, TR),
    TR >= 0.70.

:- end_tests(gbff_funding_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The GBFF extracts coordination surplus (wealthy nations benefit from shared burden) but this is legitimate. However, extraction through conditionality and knowledge hierarchy is real: developing nations must adopt external conservation definitions to access funds, indigenous stewards lose governance authority over ancestral lands. The value (0.52 vs earlier estimate of 0.38) reflects that conditionality mechanisms are more extensive than pure financing. Suppression (0.58): Moderate-high. Significant barriers to resistance include: fiscal dependency (developing nations cannot fund conservation alone), knowledge asymmetry (scientific frameworks dominate indigenous knowledge), exit costs (refusing GBFF means losing conservation capacity), and institutional norms (GBFF represents global consensus). But suppression is not total—some nations and indigenous groups resist conditionality, and alternative funding models (carbon markets, blue bonds) are emerging. Theater ratio (0.65): High. GBFF governance measures compliance (disbursement rates, audit completion, reporting submission) rather than biodiversity outcomes. The fund has spent considerable resources on monitoring systems while actual species-level impact attribution remains contested. Theater has increased over the interval as administrative apparatus grew.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Wealthy nations see pure coordination (Rope)—they genuinely benefit from pooled financing. Developing nations see mixed coordination and extraction (Tangled Rope)—they gain conservation resources but lose fiscal autonomy. Indigenous stewards see pure extraction (Snare)—trapped in conditionality regimes that subordinate their knowledge and governance. Conservation NGOs see a temporary scaffold—capacity building should enable transition to self-financed conservation. The GBFF apparatus sees its own governance as performative (Piton). The analytical observer sees the constraint as a structurally hybrid Tangled Rope: both the coordination function AND the extraction mechanism are real, not illusions. This perspectival gap is not a measurement error; it reflects genuine structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary (negative chi). Developing nations: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction; constrained exit prevents alternatives. Indigenous stewards: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; cannot refuse without losing stewardship capacity. Conservation NGOs: Beneficiary + constrained → d≈0.38, f(d)≈0.38. Moderate extraction upward; constrained by donor relationships but also benefit from mission alignment. GBFF apparatus: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification comes from theater gate, not from high chi. Analytical observer: analytical + analytical → d≈0.50, f(d)≈0.65. Sees both coordination and extraction as structural realities.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED through perspectival pluralism. The mandatrophy question is: 'Is GBFF a coordination mechanism or an extractive regime?' Answer: Both, from different structural positions. Wealthy nations experience coordination—they solve the collective action problem of underfunded global biodiversity protection. Developing nations and indigenous stewards experience extraction—they lose autonomy and knowledge authority in exchange for capital access. The constraint is NOT a coordination mechanism mislabeled as extraction (false negative). It is NOT an extraction regime falsely justified as coordination (false positive). It is a genuine hybrid (Tangled Rope) with real coordination function AND real asymmetric extraction. The mandatrophy resolves by recognizing that the constraint serves BOTH functions simultaneously: coordination for wealthy nations, extraction for developing nations. This is exactly what Tangled Rope classification captures: active enforcement (conditionality), beneficiaries (wealthy nations, NGOs), victims (developing nations, indigenous stewards), and an effective extraction chi that is neither pure coordination nor pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_extraction_threshold,
    'At what level of conditionality does conservation funding transition from coordination mechanism to extractive regime?',
    'Comparative analysis of GBFF conditions vs bilateral aid; measurement of developing nations'' ability to redirect funds toward locally-identified priorities; tracking of which priorities are donor-imposed vs recipient-selected',
    'If threshold is low (few conditions): GBFF appears as Rope from developing-nation perspective. If threshold is high (many conditions): GBFF appears as Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_extraction_threshold, empirical, 'Conditionality threshold distinguishing coordination from extraction').

omega_variable(
    fiscal_sovereignty_recovery_timeline,
    'Can developing nations realistically transition from conditional GBFF funding to self-financed conservation within 20-30 years, or does the fund structure entrench dependency?',
    'Longitudinal tracking of domestic conservation budget growth in recipient nations; correlation between GBFF funding and local budget allocation; historical analysis of aid-to-independence transitions in other domains',
    'If recovery is realistic: scaffold perspective is valid — fund has genuine sunset. If structural dependency emerges: scaffold is aspirational theater; constraint should classify as Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_sovereignty_recovery_timeline, empirical, 'Whether developing nations can achieve fiscal independence in conservation').

omega_variable(
    biodiversity_outcome_attribution,
    'Can improvements in global biodiversity metrics be reliably attributed to GBFF funding vs natural variation, other programs, or local effort?',
    'Causal inference analysis using satellite imagery and species distribution data; comparison of GBFF-funded regions vs matched non-funded regions; econometric decomposition of conservation drivers',
    'If attribution is strong: GBFF''s claimed coordination function is real. If attribution is weak: theater_ratio calculation may be understated; fund serves primarily as wealth transfer with conservation narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biodiversity_outcome_attribution, empirical, 'Whether GBFF outcomes can be reliably attributed to the fund').

omega_variable(
    knowledge_asymmetry_decayability,
    'Do indigenous and local knowledge systems provide genuine alternatives to externally-designed conservation strategies, or are they systematically devalued by scientific biodiversity frameworks?',
    'Comparative outcome analysis: conservation projects using indigenous knowledge design vs external expert design; documentation of conflict between indigenous fire management practices and global carbon/biodiversity targets; tracking of recognition by GBFF governance',
    'If indigenous knowledge is genuinely equivalent: suppression score should be lower; constraint appears more like coordination. If systematically devalued: suppression score should be higher; knowledge asymmetry is real extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_asymmetry_decayability, conceptual, 'Whether indigenous knowledge designs are systemically devalued').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gbff_funding_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gbff_tr_t0, gbff_funding_mechanism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gbff_tr_t5, gbff_funding_mechanism, theater_ratio, 5, 0.54).
narrative_ontology:measurement(gbff_tr_t10, gbff_funding_mechanism, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(gbff_be_t0, gbff_funding_mechanism, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gbff_be_t5, gbff_funding_mechanism, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(gbff_be_t10, gbff_funding_mechanism, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gbff_funding_mechanism, resource_allocation).
narrative_ontology:affects_constraint(gbff_funding_mechanism, biodiversity_conservation_sovereignty).
narrative_ontology:affects_constraint(gbff_funding_mechanism, indigenous_knowledge_valuation).
narrative_ontology:affects_constraint(gbff_funding_mechanism, global_carbon_finance_hierarchy).
narrative_ontology:affects_constraint(gbff_funding_mechanism, conservation_land_use_competition).

% DUAL FORMULATION NOTE:
% GBFF funding mechanism decomposes into two related but distinct constraints: (1) resource_allocation_coordination—the genuine coordination problem of financing global biodiversity protection, epsilon ≈ 0.15, Mountain-to-Rope depending on perspective; (2) conditionality_extraction—the mechanism through which wealthy nations enforce conservation definitions and fiscal control, epsilon ≈ 0.58, Snare-to-Tangled Rope. The primary story (gbff_funding_mechanism) treats the unified constraint at epsilon=0.52. The decomposition connects to biodiversity_conservation_sovereignty (the victim constraint) and indigenous_knowledge_valuation (an omega variable made structural by this constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gbff_funding_mechanism, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
