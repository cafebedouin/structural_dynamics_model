% ============================================================================
% CONSTRAINT STORY: uk_unpaid_care_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_unpaid_care_system, []).

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
 *   constraint_id: uk_unpaid_care_system
 *   human_readable: The UK's reliance on unpaid carers for social and healthcare
 *   domain: economic/social
 *
 * SUMMARY:
 *   The UK's social care system is structured around a fundamental extraction
 *   mechanism: the normalization of unpaid family caregiving as the primary
 *   response to elderly, disabled, and chronically ill dependents.
 *   Approximately 5.3 million unpaid carers (2023) provide care that would
 *   cost the public exchequer £100-150 billion annually if formalized. This
 *   creates a tangled coordination-extraction hybrid: the system genuinely
 *   needs care provided (coordination function), but that care is extracted
 *   from family members who bear the economic, health, and opportunity costs
 *   while receiving minimal statutory support. The constraint has intensified
 *   over 30 years as female labor force participation increased (reducing
 *   availability of unpaid female carers), while care demand grew due to
 *   aging populations and reduced availability of informal multigenerational
 *   households. The theater ratio (0.65) reflects that care is culturally
 *   narrativized as voluntary family duty and spiritual fulfillment, masking
 *   the structural coercion and extraction. The unpaid carer appears as a
 *   Snare from their own perspective (trapped, extracting wage labor), as
 *   tangled rope from the care-dependent relative's perspective (benefits
 *   from personalized care but loses autonomy), as pure rope (coordination)
 *   from the public exchequer's perspective (budgetary solution), and as
 *   temporary scaffolding from the carers' rights movement's perspective
 *   (awaiting statutory entitlements). The cultural narrative (piton
 *   perspective) naturalizes the system as immutable feature of kinship,
 *   defending it against reform through inertial power despite its atrophying
 *   function.
 *
 * KEY AGENTS:
 *   - Unpaid carers (typically women aged 25-65): Primary victims (powerless/trapped) — bear foregone wages, career loss, health deterioration, and permanent pension deficits with minimal statutory support
 *   - Care-dependent relatives (elderly, disabled, chronically ill): Secondary actors (moderate/constrained) — benefit from family-based personalized care but experience dependency, inadequate service quality, and guilt about burden
 *   - Public exchequer (NHS, local government social care budgets): Primary beneficiary (institutional/arbitrage) — avoids £100-150 billion annual formal care expenditure; system solves budgetary constraint through family obligation
 *   - Healthcare system management: Secondary beneficiary (institutional/constrained) — absorbs demand overflow through unpaid carer capacity; reduces formal service delivery burden
 *   - Carers' rights movement (Carers UK, campaign groups, local government): Organized advocates (organized/constrained) — push for partial reforms (carer's allowance, support services) while framing system as temporary scaffold awaiting statutory entitlements
 *   - Cultural narrative of family duty: Institutional actor (institutional/arbitrage) — naturalizes unpaid care as inevitable, voluntary, spiritually meaningful; defends system against reform through normative inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_unpaid_care_system, 0.58).
domain_priors:suppression_score(uk_unpaid_care_system, 0.72).
domain_priors:theater_ratio(uk_unpaid_care_system, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_unpaid_care_system, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_unpaid_care_system, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(uk_unpaid_care_system, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_unpaid_care_system, tangled_rope).
narrative_ontology:human_readable(uk_unpaid_care_system, "The UK's reliance on unpaid carers for social and healthcare").
narrative_ontology:topic_domain(uk_unpaid_care_system, "economic/social").

domain_priors:requires_active_enforcement(uk_unpaid_care_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_unpaid_care_system, public_exchequer).
narrative_ontology:constraint_beneficiary(uk_unpaid_care_system, healthcare_system).
narrative_ontology:constraint_beneficiary(uk_unpaid_care_system, care_dependent_relatives).
narrative_ontology:constraint_victim(uk_unpaid_care_system, unpaid_carers).
narrative_ontology:constraint_victim(uk_unpaid_care_system, economic_opportunity_cost).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNPAID CARER (SNARE) — Trapped by family obligation, lack of affordable alternatives, and absence of statutory entitlement to care leave or income replacement. Bears full extraction: foregone wages (average £5,000-£15,000 annually), career progression delays, pension deficits, and health deterioration. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CARE-DEPENDENT RELATIVE (TANGLED ROPE) — Benefits from continued family care (coordination function: preserves dignity, cultural continuity, personalized support) but also extracted from by dependency structure — loses autonomy, faces inadequate service quality, and experiences guilt about imposing burden. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(uk_unpaid_care_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC EXCHEQUER (ROPE) — Primary beneficiary. Unpaid care substitutes for ~£100-150 billion annually in formal service provision. Experiences constraint as pure coordination: family care networks solve collective action problem of matching care supply to demand without massive public expenditure. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(uk_unpaid_care_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEALTHCARE SYSTEM MANAGEMENT (TANGLED ROPE) — Coordination function: unpaid carers absorb demand that formal system cannot meet, enabling NHS to manage capacity. Extraction mechanism: system does not compensate carers for this absorption; depends on normative family obligation rather than service design. Management benefits from system stability while bearing responsibility for uncompensated dependency. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.19.
constraint_indexing:constraint_classification(uk_unpaid_care_system, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CARERS' RIGHTS MOVEMENT (SCAFFOLD) — Organized agents (Carers UK, local government, campaigning groups) see unpaid care system as temporary scaffolding awaiting transition to universal care entitlements. Pushes for carer's allowance (partial, inadequate), carer support, and eventual move toward comprehensive adult social care. χ≈0.20 — extraction is real but the movement constructs this as a transition mechanism with sunset logic: statutory entitlements will eventually replace family obligation. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.24.
constraint_indexing:constraint_classification(uk_unpaid_care_system, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CULTURAL NARRATIVE (PITON) — Philosophical/cultural framing naturalizes unpaid care as inherent to family duty ('caring is what families do'). Theater ratio=0.65: substantial performative content in how care is discussed as voluntary, inevitable, even spiritually meaningful. Institutional actors cite cultural continuity to defend system against reform. Constraint persists through normative inertia despite atrophied function — formal care alternatives exist but are subordinated to the narrative of familial obligation. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(uk_unpaid_care_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: GENERATIONAL ECONOMIC OPPORTUNITY (SNARE) — Abstract actor: the lost lifetime earning potential and capital accumulation of an entire cohort of unpaid carers. Those entering caring responsibility at 25 (typical for women in care roles) face 10-30 year extraction window with permanent earnings deficit. d≈1.0, f(d)≈1.50, σ=1.0 → χ≈0.87. This perspective reveals the constraint as extracting not just from current carers but from future economic participation of an entire demographic stratum.
constraint_indexing:constraint_classification(uk_unpaid_care_system, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_unpaid_care_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_unpaid_care_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_unpaid_care_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_unpaid_care_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_unpaid_care_system, TR),
    TR >= 0.70.

:- end_tests(uk_unpaid_care_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The system extracts significant economic value from unpaid carers (average £5,000-£15,000 annually in forgone wages, with lifetime costs reaching £300,000+ per carer). However, extraction is not maximal (ε would be >0.70 for pure snare) because the care-dependent relative does benefit from family-based care, and carers genuinely experience affection alongside obligation. The theater ratio and suppression reflect that the extraction is partly masked by cultural narrativization and lack of awareness. Suppression (0.72): High. Carers face substantial barriers to exit: legal/informal family obligation (normative suppression), lack of affordable formal alternatives (economic suppression), employment discrimination against those with caring responsibilities (institutional suppression), inadequate carer's allowance (£71.60/week, below minimum wage), and no statutory right to care leave (policy suppression). Theater ratio (0.65): Moderate-high. The system is substantially theatricalized through cultural narratives of familial duty, voluntary love, and intergenerational obligation. Government policy documents frame unpaid care as 'invaluable' while simultaneously relying on it to avoid expenditure. Carers are publicly praised as 'heroic' while receiving minimal material support. This performative recognition masks the extraction structure. Claimed type: Tangled Rope. The system exhibits both genuine coordination function (family care does provide personalized, culturally continuous support) and asymmetric extraction (carers bear costs, public exchequer benefits, enforcement is normative rather than formal).
 *
 * PERSPECTIVAL GAP:
 *   The unpaid carer and the public exchequer perceive radically different constraint structures. The carer sees a Snare: they are trapped by family obligation and lack of alternatives, bearing maximum extraction with no escape. The exchequer sees a Rope: the constraint elegantly solves the coordination problem of matching care supply to demand without enormous public expenditure. The care-dependent relative sees Tangled Rope: they benefit from personalized family care but are also trapped in dependency and guilt. The healthcare system sees Tangled Rope: coordination (unpaid care absorbs demand) mixed with structural dependency (system must maintain the normalization to avoid facing care funding crisis). The carers' rights movement sees Scaffold: the system is temporary, awaiting replacement with statutory entitlements and universal care services. The cultural narrative sees Piton: unpaid family care is an immutable feature of kinship and social structure, defended through normalized obligation despite diminishing function. This perspectival gap is the diagnostic signature of the constraint's structure: different actors experience the same system as having entirely different constraint types because they occupy fundamentally different positions within the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Unpaid carers: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction because they cannot exit (no legal right to care leave, no adequate income replacement, family obligation is normative prison). Public exchequer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Full beneficiary with options to exit if desired (though politically costly). Care-dependent relative: Mixed (beneficiary of personalized care + victim of dependency) + constrained → d≈0.55, f(d)≈0.75. Symmetric position: benefits from family care structure but loses autonomy. Healthcare system: Institutional + constrained → d≈0.35, f(d)≈0.32. Moderate extraction on the system because it is dependent on family care absorption to manage capacity, but also reaps benefit of reduced formal service delivery load. Carers' rights movement: Organized + constrained → d≈0.42, f(d)≈0.42. Low-moderate extraction because the movement has agency (can advocate, legislate, organize) and sees an exit path (statutory entitlements). Cultural narrative: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary (narrative defends system, derives legitimacy from it). Generational opportunity cost: Powerless + trapped → d≈1.0, f(d)≈1.50. Maximum extraction because entire demographic cohort is locked into lost earning potential.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question here is: 'Is unpaid care a natural coordination solution (rope/scaffold) that should be supported as cultural value, or is it an extractive snare that should be dismantled in favor of formal, funded care?' The constraint resolves the ambiguity through structural decomposition: (1) COORDINATION FUNCTION exists and is real: family care does provide personalized, culturally continuous support that formal systems struggle to replicate. This is the rope component. (2) EXTRACTION MECHANISM exists and is real: carers bear documented, measured economic costs (£5,000-15,000 annually, permanent earnings deficit) while the public exchequer avoids £100-150 billion in formal service provision. This is the snare component. (3) ENFORCEMENT is normative: the system persists through cultural obligation and lack of alternatives, not through formal legal requirement. This is the tangled rope signature: both functions are structural, not contingent. The mandatrophy is resolved by rejecting the binary (either 'family care is beautiful coordination' OR 'family care is exploitation') and accepting that the system genuinely is both simultaneously. The policy implication is that neither 'preserve family care as-is' nor 'abolish family care in favor of state provision' adequately addresses the constraint. The scaffold perspective (organized agents pushing for statutory entitlements while preserving family choice) attempts the proper resolution: maintain coordination function while removing extraction mechanism through formal compensation and universal care entitlements. The constraint is NOT mandatrophic if we accept that the solution is not to choose between rope and snare but to formalize and compensate the coordination that currently relies on extraction. However, this requires explicit rejection of the piton narrative (family obligation as natural/inevitable) and substantial public expenditure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    willingness_vs_coercion,
    'To what extent is unpaid care provision genuinely voluntary (driven by family affection and cultural values) versus structurally coerced (trapped by absence of alternatives)?',
    'Comparative policy natural experiments: regions with statutory care entitlements vs those without; longitudinal tracking of carer choice when legal leave or income support become available; psychological measurement of felt obligation vs felt agency among carers',
    'If predominantly voluntary: constraint is more rope-like (coordination) than snare-like (extraction). Policy implication: frame as cultural value to preserve. If predominantly coerced: constraint is snare/tangled rope. Policy implication: expand alternatives immediately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(willingness_vs_coercion, empirical, 'Degree to which care provision is voluntary versus structurally coerced').

omega_variable(
    substitution_vs_supplementation,
    'Would expansion of formal care services substitute for unpaid care (reducing family burden) or supplement it (adding formal on top of family obligation)?',
    'Historical analysis of care system expansion in other UK regions and countries (Germany, Denmark, Japan); measurement of caregiver workload in high-formal-care vs high-family-care regimes; qualitative interviews on perceived adequacy of formal services',
    'If substitution: formal care expansion genuinely reduces extraction on families. If supplementation: families add formal service navigation to unpaid care burden. Determines whether policy reform addresses or simply relocates extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_vs_supplementation, empirical, 'Whether formal care expands substitute for or supplement unpaid family care').

omega_variable(
    cultural_continuity_necessity,
    'Is cultural continuity (personalized, family-embedded care) actually dependent on unpaid family labor, or can it be preserved through professional caregiving relationships funded formally?',
    'Comparative ethnography of care provision in high-formal vs high-family-care societies; measurement of care quality, dignity, autonomy outcomes by model; cost analysis of preserving cultural continuity through formal employment of family members',
    'If dependent on unpaid labor: scaffold perspective is aspirational; genuine transition requires cultural adaptation. If achievable through formal relationships: scaffold is structural; statutory entitlements genuinely resolve mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_continuity_necessity, conceptual, 'Whether cultural continuity requires unpaid family care or can be achieved through formal relationships').

omega_variable(
    population_sustainability,
    'As demographics shift (aging population, declining birth rates, female labor force participation increases), can the unpaid care system sustain itself or will it collapse into demand crisis?',
    'Demographic modeling: ratio of care-dependent to working-age population; projection of female availability for unpaid care; comparison with care demand growth; historical precedent from other aging societies',
    'If unsustainable: system will transition regardless of policy (coerced by demographic fact). Extraction window may shorten as system fails. If sustainable: political choice whether to expand formal care or maintain family obligation model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(population_sustainability, empirical, 'Demographic sustainability of unpaid care system under aging population').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_unpaid_care_system, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukcare_tr_t0, uk_unpaid_care_system, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ukcare_tr_t15, uk_unpaid_care_system, theater_ratio, 15, 0.58).
narrative_ontology:measurement(ukcare_tr_t30, uk_unpaid_care_system, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(ukcare_be_t0, uk_unpaid_care_system, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ukcare_be_t15, uk_unpaid_care_system, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ukcare_be_t30, uk_unpaid_care_system, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_unpaid_care_system, resource_allocation).
narrative_ontology:affects_constraint(uk_unpaid_care_system, gender_wage_gap_uk).
narrative_ontology:affects_constraint(uk_unpaid_care_system, female_pension_poverty).
narrative_ontology:affects_constraint(uk_unpaid_care_system, social_care_funding_crisis).

% DUAL FORMULATION NOTE:
% The unpaid care system is upstream of gender wage gap and pension poverty because it structurally redirects female labor from paid work into unpaid care. It is also structurally coupled with social care funding crisis: the system avoids public expenditure, creating demand-side pressure that makes crisis inevitable as demographics shift. Decomposition: constraint family includes separate constraint stories for (a) cultural naturalization of family obligation (piton), (b) systemic extraction from carers (snare/tangled rope), and (c) population sustainability of unpaid care as percentage of total care provision (demographic constraint). These are linked by affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_unpaid_care_system, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
