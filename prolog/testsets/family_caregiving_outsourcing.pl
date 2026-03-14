% ============================================================================
% CONSTRAINT STORY: family_caregiving_outsourcing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_caregiving_outsourcing, []).

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
 *   constraint_id: family_caregiving_outsourcing
 *   human_readable: Family Caregiving Outsourcing: Coordination and Extraction in Household Labor Markets
 *   domain: social/economic/gender
 *
 * SUMMARY:
 *   Family caregiving outsourcing is the structural arrangement by which paid
 *   care workers (predominantly women, often migrants) provide childcare,
 *   elder care, and household services that were historically provided by
 *   unpaid family members (predominantly women). This constraint represents a
 *   complex decomposition problem — the natural-language concept 'family
 *   outsourcing' conflates at least three structurally distinct mechanisms:
 *   (1) the coordination problem of allocating care labor in dual-income
 *   households, (2) the extraction of labor value through wage suppression
 *   and legal precarity of care workers, and (3) the degradation of
 *   relational continuity in dependent care. The story focuses on the primary
 *   constraint: the tangled_rope mechanism by which household economic
 *   coordination depends on and perpetuates the extraction of care-worker
 *   labor. The constraint exhibits different classification from seven
 *   distinct perspectives spanning the care worker, the dependent child, the
 *   employing household, the institutional employer, care-work organizing
 *   movements, cultural ideology of family, and the analytical observer's
 *   natural law view. The extractiveness has increased over the 30-year
 *   interval as dual-income household dependence on outsourced care has
 *   become economicallly mandatory (suppression of alternatives), while
 *   care-worker wages have stagnated and precarity has increased (wages
 *   stagnated 1990-2020, then slight gains 2020-2025 from organizing
 *   pressure). Theater ratio has remained moderate — the constraint is
 *   primarily economic rather than performative, though cultural ideology of
 *   family maintains some performative function (the narrative that mothers
 *   'naturally' provide care persists even as care is outsourced).
 *
 * KEY AGENTS:
 *   - Migrant Care Workers: Primary victims (powerless/trapped) — bearers of suppressed wages, precarious legal status, emotional labor, geographic separation from own families
 *   - Children in Outsourced Care: Secondary victims (powerless/trapped) — bear relational extraction (disrupted attachment due to care-worker rotation)
 *   - Dual-Income Households: Primary beneficiaries + mixed agents (moderate/constrained) — benefit from enabled labor force participation; bear costs of relational compromise and care-market pricing constraints
 *   - Employers of Primary Earners: Institutional beneficiaries (institutional/arbitrage) — benefit from enabled full-time workforce; bear no visible care costs
 *   - Care Work Organizing (Unions, Advocacy): Organized agents (organized/constrained) — attempting to reframe constraint from snare toward balanced coordination; currently constrained by power imbalance
 *   - Family Ideology as Cultural Institution: Institutional performer (institutional/arbitrage) — maintains normative framing of family-based care even as function is outsourced; derives prestige/identity benefits from ideological maintenance
 *   - Analytical Observer (Natural Law view): Civilizational analyst (analytical/analytical) — risks naturalizing engineered extraction as biological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_caregiving_outsourcing, 0.58).
domain_priors:suppression_score(family_caregiving_outsourcing, 0.65).
domain_priors:theater_ratio(family_caregiving_outsourcing, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_caregiving_outsourcing, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_caregiving_outsourcing, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(family_caregiving_outsourcing, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_caregiving_outsourcing, tangled_rope).
narrative_ontology:human_readable(family_caregiving_outsourcing, "Family Caregiving Outsourcing: Coordination and Extraction in Household Labor Markets").
narrative_ontology:topic_domain(family_caregiving_outsourcing, "social/economic/gender").

domain_priors:requires_active_enforcement(family_caregiving_outsourcing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_caregiving_outsourcing, dual_income_households).
narrative_ontology:constraint_beneficiary(family_caregiving_outsourcing, employers_of_primary_earners).
narrative_ontology:constraint_beneficiary(family_caregiving_outsourcing, care_service_providers_as_class).
narrative_ontology:constraint_victim(family_caregiving_outsourcing, care_workers).
narrative_ontology:constraint_victim(family_caregiving_outsourcing, dependent_care_quality).
narrative_ontology:constraint_victim(family_caregiving_outsourcing, family_relational_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MIGRANT CARE WORKER (SNARE) — Trapped in the outsourcing system by visa restrictions, debt bondage, remittance obligations, and geographic distance from family. Bears full extraction: low wages, long hours, no labor protections, emotional labor uncompensated. Exit is materially blocked (visa status) and structurally impossible (family depends on remittances). Maximum suppression — legal status, language barriers, social isolation prevent organizing or negotiating. No coordination benefit flows to this agent.
constraint_indexing:constraint_classification(family_caregiving_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CHILD IN OUTSOURCED CARE (SNARE) — Trapped in contingent, unstable relationships with care workers who rotate due to low wages and burnout. Bears relational extraction: interrupted attachment, inconsistent discipline and norms, transactional rather than kin-based care. No exit option and no coordination function from the child's perspective. The constraint trades relational continuity for parental labor liberation.
constraint_indexing:constraint_classification(family_caregiving_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIMARY EARNER / DUAL-INCOME HOUSEHOLD (TANGLED ROPE) — Constrained by career demands and childcare market pricing, but also benefits from the system. Genuine coordination function: outsourcing enables both parents' labor force participation and economic household stability. Asymmetric extraction: benefits from suppressed care-worker wages (lower costs) and social invisibility of care labor. Suppression of alternatives (e.g., reduced work weeks, subsidized public childcare) maintains dependence on outsourced care.
constraint_indexing:constraint_classification(family_caregiving_outsourcing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYERS OF PRIMARY EARNERS (ROPE) — Net beneficiary. The outsourcing system enables full-time work at competitive wages without employers bearing care costs. Experiences the constraint as pure coordination: enables workforce stability and productivity. No extraction from this agent's perspective — the constraint solves their coordination problem (maintaining a full-time workforce) without visible cost to them.
constraint_indexing:constraint_classification(family_caregiving_outsourcing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CARE WORK ORGANIZING MOVEMENT (TANGLED ROPE) — Organized agents attempting to shift the constraint from snare toward balanced coordination. Pushing for labor standards, collective bargaining, and professionalization of care work. Constrained by the structural power imbalance (millions of care workers, diffuse employers, high employer arbitrage to other labor markets). See genuine coordination function (care work needs infrastructure) alongside active enforcement of extraction (wage suppression, regulatory capture by placement agencies). Current trajectory: slowly improving labor conditions in unionized sectors; backsliding in gig care platforms.
constraint_indexing:constraint_classification(family_caregiving_outsourcing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FAMILY AS NATURAL CARE UNIT (PITON) — The normative framing that family (especially mothers) 'naturally' provide childcare persists despite its function having been outsourced. Theater ratio high: family structures and norms remain organized around caregiving even as care is externalized. The constraint maintains ideological extraction — the cultural prestige and relational identity benefits of motherhood persist while the labor is outsourced. This perspective sees the theatrical maintenance of family-based care ideology even as economic reality is extraction via outsourcing.
constraint_indexing:constraint_classification(family_caregiving_outsourcing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / BIOLOGICAL NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, human children require care from some agent, and the constraints of human biology (gestational dependence, biological imprinting, developmental time windows) are immutable. The mountain view sees the outsourcing question as merely shifting WHO provides the care, not eliminating care requirements. However, structural data contradicts the mountain classification — the extractive asymmetry, the suppression mechanisms, and the institutional engineering of care labor markets are contingent, not laws of nature. This is a false summit: biological dependence is immutable, but the economic extraction structure around it is not.
constraint_indexing:constraint_classification(family_caregiving_outsourcing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_caregiving_outsourcing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_caregiving_outsourcing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_caregiving_outsourcing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_caregiving_outsourcing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_caregiving_outsourcing, TR),
    TR >= 0.70.

:- end_tests(family_caregiving_outsourcing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts care-worker labor through wage suppression (care workers earn 30-40% below comparable skill-level occupations), legal precarity (visa restrictions, lack of labor protections), and emotional labor uncompensated. However, the extraction is not maximal (0.72+) because organizing pressure (especially post-2020) has created wage floors in some jurisdictions, and some care work is shifting toward cooperative/unionized models. The extractiveness has increased over the interval due to gig-platform care (TaskRabbit, Care.com) reconstructing precarity. Suppression (0.65): Moderate-high. Significant barriers to care-worker exit include visa restrictions for migrant workers, remittance obligations to families in origin countries, language barriers, lack of credential portability, and social isolation. Also, suppression of alternative care arrangements (public childcare, employer-sponsored care, flexible scheduling) channels households toward outsourcing. Suppression has remained steady over the interval — it's structural rather than deteriorating or improving. Theater ratio (0.48): Moderate. The constraint is primarily economic (labor cost and availability) rather than performative. Performative elements include: cultural ideology of family-based care (families talk as if mothers 'naturally' provide care even while outsourcing it), performative professionalization (care certifications that don't meaningfully improve care quality), and therapeutic narratives (framing outsourcing as 'healthy work-life balance' rather than structural labor market extraction). Theater has slightly increased as cultural ideology work has intensified (motherhood marketing, 'quality time' narratives) to maintain legitimacy of outsourcing.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme across all dimensions. The beneficiary (dual-income household) experiences Tangled Rope (meaningful coordination function, some extraction). The victims (care worker, child) experience Snare (maximum extraction, minimal coordination). The institutional beneficiary (employer) experiences Rope (pure coordination). The organizing agents experience Tangled Rope with agency (can push back; constraint classification may shift to Rope with sufficient organizing power). This gap reflects a deep structural reality: the same constraint genuinely does coordinate household labor participation AND genuinely does extract care-worker labor. The classification difference is not observational bias — it's reflecting different structural relationships to the constraint's function.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the directionality computation. Dual-income households are declared beneficiaries because they gain labor-force participation and household stability. Care workers, children, and dependent-care quality are declared victims because they bear suppression and extraction. Employers benefit indirectly through enabled workforce. The pipeline computes d from these declarations plus exit options: migrant care workers' trapped status → d ≈ 0.95 (maximum). Dual-income households' constrained status + beneficiary status → d ≈ 0.45-0.55 (moderate). Employers' arbitrage status + beneficiary status → d ≈ 0.05 (minimal). These d values feed f(d), which feeds χ. The resultant χ values differ by a factor of 10+ across perspectives, even though base extractiveness ε is held constant. This demonstrates the power of the directionality derivation chain to reveal perspectival asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE ANALYSIS — How does the classification prevent mislabeling coordination as pure extraction (or vice versa)? The Tangled Rope classification captures what neither pure Rope (which would ignore the asymmetric extraction of care-worker labor) nor pure Snare (which would ignore the genuine coordination function enabling household labor participation) could explain. The constraint exhibits both mechanisms simultaneously because it operates on different agents with asymmetric power. The Rope perspective (employers) sees pure coordination. The Snare perspective (care workers) sees pure extraction. The Tangled Rope perspective (dual-income households and organizing agents) sees both. Mandatrophy is resolved not by choosing one classification but by recognizing that the constraint's true structure is asymmetrically distributed across perspectives. Claiming this as a Rope (pure coordination) would miss the extraction. Claiming it as a Snare would miss the coordination. Tangled Rope is the only type that captures the hybrid structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_harm_quantification,
    'What portion of negative developmental outcomes in outsourced childcare are attributable to care worker rotation versus parental work demands versus poverty/stress?',
    'Longitudinal studies comparing developmental trajectories by care stability (same provider >2 years vs rotating), parental work schedule intensity, and household income/stress; controlling for baseline factors',
    'If rotation is primary driver: constraint is fundamentally extractive (trading child welfare for parental labor). If work intensity is primary: constraint appears as coordination problem with poorly-managed side effects (could be mitigated). If poverty/stress is primary: constraint is artifact of insufficient household resources, not outsourcing per se.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_harm_quantification, empirical, 'Attribution of developmental harm to care worker rotation vs other factors').

omega_variable(
    counterfactual_household_alternative,
    'What portion of dual-income households could sustain themselves on one income plus reduced-hour work arrangements if childcare costs were not so high and work was structured more flexibly?',
    'Economic modeling: household budgets under scenarios of subsidized childcare vs wage requirement for two full-time jobs; labor market analysis of part-time and reduced-hour work availability',
    'If high percentage could switch (>40%): outsourcing is partially an engineered necessity (suppressed public childcare, inflexible work structures) rather than inevitable. If low percentage (<20%): outsourcing reflects genuine household economic requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_household_alternative, empirical, 'Feasibility of single-income or reduced-work household arrangements').

omega_variable(
    care_worker_voice_capture,
    'To what extent do care worker advocacy organizations represent care workers'' actual preferences vs advocates'' (often Western, middle-class) framings of what care work should be?',
    'Survey and interview data from care workers themselves about: (a) whether they want professionalization/credentialing vs flexibility, (b) whether wages vs scheduling/autonomy is primary concern, (c) what exit options they actually see as viable',
    'If advocates capture worker voice: policy responses may impose solutions workers don''t want (e.g., professionalization barriers that price out informal care). If worker voice is authentic: policy responses are more aligned with actual needs and constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(care_worker_voice_capture, conceptual, 'Whether care worker advocacy captures authentic worker preferences or advocate framings').

omega_variable(
    cultural_identity_lock,
    'For primary earners, especially women, to what extent is the choice to outsource care driven by actual economic necessity versus internalized identity-lock binding them to paid work identity and away from caregiving identity?',
    'Qualitative research: interviews with primary earners about counterfactuals (what if childcare were free? what if work were 4-day weeks?); analysis of subjective sense of choice and identity coherence',
    'If high identity-lock: the constraint appears as economic necessity but functions partly through cognitive capture (the constraint''s suppression is internalized). If low identity-lock: economic structuring alone drives the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_identity_lock, conceptual, 'Degree of identity-lock in paid work identity vs caregiving choice').

omega_variable(
    suppression_mechanism_composition,
    'What proportion of suppression of care-worker alternatives (subsidized public childcare, employer-sponsored on-site care, flexible scheduling) is structural (policy/regulation) versus internalized (care workers'' own belief that private market provision is inevitable)?',
    'Policy analysis of childcare funding mechanisms in comparable countries; care worker interviews about perceived viability of public/cooperative models; historical analysis of policy windows when public childcare expanded or contracted',
    'If structural: policy lever exists (public investment). If internalized: suppression persists even after regulatory barriers are removed (care workers internalize market logic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Composition of suppression between structural and internalized mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_caregiving_outsourcing, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fco_tr_t0, family_caregiving_outsourcing, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fco_tr_t15, family_caregiving_outsourcing, theater_ratio, 15, 0.43).
narrative_ontology:measurement(fco_tr_t30, family_caregiving_outsourcing, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(fco_be_t0, family_caregiving_outsourcing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fco_be_t15, family_caregiving_outsourcing, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(fco_be_t30, family_caregiving_outsourcing, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_caregiving_outsourcing, resource_allocation).
narrative_ontology:affects_constraint(family_caregiving_outsourcing, dual_income_household_stability).
narrative_ontology:affects_constraint(family_caregiving_outsourcing, care_worker_wage_suppression).
narrative_ontology:affects_constraint(family_caregiving_outsourcing, gender_labor_segregation).
narrative_ontology:affects_constraint(family_caregiving_outsourcing, migration_and_remittance_systems).

% DUAL FORMULATION NOTE:
% Family caregiving outsourcing is downstream of several structural constraints: (1) dual-income economic necessity (household requires two full incomes to reach middle-class stability), (2) inflexible work scheduling (employers expect full-time presence), (3) public childcare underfunding (no subsidized alternative), (4) gender norms around motherhood and caregiving. This story focuses on the primary constraint structure (the tangled rope of coordination + extraction); sibling stories address the upstream constraints that make outsourcing the default option.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_caregiving_outsourcing, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
