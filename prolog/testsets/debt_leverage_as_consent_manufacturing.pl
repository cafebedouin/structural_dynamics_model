% ============================================================================
% CONSTRAINT STORY: debt_leverage_as_consent_manufacturing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_leverage_as_consent_manufacturing, []).

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
 *   constraint_id: debt_leverage_as_consent_manufacturing
 *   human_readable: Debt Leverage as Consent Manufacturing in Labor Relations
 *   domain: labor_relations/institutional_control/debt_bondage
 *
 * SUMMARY:
 *   Debt-leverage as consent manufacturing operates through a structural
 *   transformation: financial obligations to household dependents convert
 *   what would otherwise be worker resistance or exit into institutional
 *   compliance, with the conversion justified through family-welfare framing.
 *   The mechanism exhibits genuine coordination (signing bonuses solve
 *   liquidity problems and enable household stability) alongside asymmetric
 *   extraction (debt obligations suppress labor organizing, reduce wage
 *   negotiation leverage, and shift economic risk from institution to
 *   individual household). The constraint's theater_ratio (0.58) reflects the
 *   growing professionalization of retention analytics and total rewards
 *   optimization — much of the HR consulting apparatus exists to obscure the
 *   core mechanism (debt creates compliance) behind complexity and
 *   legitimation rituals. The constraint is identity-sensitive: workers who
 *   have fused their self-concept with provider roles experience the debt as
 *   identity-lock (exit would require abandoning who they are), while workers
 *   with weaker provider identity or stronger alternative options experience
 *   it as a cost-benefit calculation with high but surmountable exit costs.
 *
 * KEY AGENTS:
 *   - Indebted Worker: Primary victim (powerless/identity_locked) — provider role fusion makes exit unthinkable despite structural mobility; every compliance decision justified through family welfare
 *   - Worker with Alternatives: Secondary victim (moderate/constrained) — faces real exit costs but not insurmountable; experiences mixed coordination and extraction
 *   - Employing Institution: Primary beneficiary (institutional/arbitrage) — captures compliance, reduced organizing, wage leverage through debt mechanism; experiences as pure coordination
 *   - Labor Organizing Coalition: Mixed position (organized/constrained) — benefits from household stability, victimized by fragmented collective action; sees dual structure clearly
 *   - Progressive Employer: Transitional actor (institutional/mobile) — uses debt-leverage now but building toward portable benefits and social safety nets; sees sunset path
 *   - HR Consulting Industry: Theatrical maintainer (institutional/arbitrage) — professional apparatus that obscures extraction through complexity; sees own process as degraded
 *   - Analytical Observer: Structural analyst (analytical/analytical) — identifies irreducible hybrid of coordination and extraction; maps perspectival gaps to structural positions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_leverage_as_consent_manufacturing, 0.38).
domain_priors:suppression_score(debt_leverage_as_consent_manufacturing, 0.62).
domain_priors:theater_ratio(debt_leverage_as_consent_manufacturing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_leverage_as_consent_manufacturing, extractiveness, 0.38).
narrative_ontology:constraint_metric(debt_leverage_as_consent_manufacturing, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(debt_leverage_as_consent_manufacturing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_leverage_as_consent_manufacturing, tangled_rope).
narrative_ontology:human_readable(debt_leverage_as_consent_manufacturing, "Debt Leverage as Consent Manufacturing in Labor Relations").
narrative_ontology:topic_domain(debt_leverage_as_consent_manufacturing, "labor_relations/institutional_control/debt_bondage").

domain_priors:requires_active_enforcement(debt_leverage_as_consent_manufacturing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_leverage_as_consent_manufacturing, employing_institution).
narrative_ontology:constraint_beneficiary(debt_leverage_as_consent_manufacturing, household_dependents).
narrative_ontology:constraint_victim(debt_leverage_as_consent_manufacturing, indebted_worker).
narrative_ontology:constraint_victim(debt_leverage_as_consent_manufacturing, labor_organizing_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEBTED WORKER (SNARE) — Identity-locked by provider role. Structurally mobile (could change jobs, relocate, default on signing bonus repayment) but identity-fused with family provider obligation. Exit would require abandoning self-concept as responsible parent/spouse. The debt is material but the lock is cognitive: 'I can't leave because my family depends on me' naturalizes institutional extraction as family duty. High suppression through internalized obligation plus material penalty (bonus clawback). Experiences maximum extraction — every compliance decision is justified through family welfare framing.
constraint_indexing:constraint_classification(debt_leverage_as_consent_manufacturing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: WORKER WITH ALTERNATIVES (TANGLED ROPE) — Has marketable skills and regional job market access. Faces real exit costs (bonus repayment, relocation, family disruption) but not insurmountable. Experiences the constraint as mixed: genuine coordination (signing bonus solves liquidity problem, enables household stability) alongside extraction (bonus structure locks them into compliance beyond what labor market would otherwise require). Can see the extraction but calculates that exit cost exceeds tolerance threshold. Not identity-locked — knows they could leave, just expensive.
constraint_indexing:constraint_classification(debt_leverage_as_consent_manufacturing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYING INSTITUTION (ROPE) — Experiences the constraint as pure coordination: signing bonuses solve the legitimate problem of retention in competitive labor markets and compensate workers for relocation/training costs. The institution sees family-welfare justifications as evidence the system works — workers stay because the arrangement benefits their households. Net beneficiary: extraction flows toward the institution (worker compliance, reduced organizing, wage negotiation leverage) but from the institution's perspective this is fair exchange for upfront capital and job security.
constraint_indexing:constraint_classification(debt_leverage_as_consent_manufacturing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR ORGANIZING COALITION (TANGLED ROPE) — Unions and worker advocacy groups see the dual structure clearly: signing bonuses provide genuine liquidity and household stability (coordination function) while simultaneously fragmenting collective action by tying individual workers to employer-specific debt obligations (extraction function). The coalition benefits from the coordination aspect (members' households are more stable) but is victimized by the extraction aspect (debt obligations suppress strike capacity and wage negotiation leverage). Organized power provides some agency but exit is constrained — can't eliminate signing bonuses without harming worker liquidity, can't accept them without accepting the compliance mechanism.
constraint_indexing:constraint_classification(debt_leverage_as_consent_manufacturing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE EMPLOYER (SCAFFOLD) — Institutions experimenting with alternative retention mechanisms see debt-leverage as a temporary coordination solution with a sunset: portable benefits, industry-wide training funds, and universal basic income proposals would provide household stability without employer-specific lock-in. These employers use signing bonuses now but are building toward structures that provide liquidity without manufacturing consent. Sunset timeline: 15-25 years as portable benefits and social safety nets mature. Low extraction because this perspective has exit options (can adopt alternative models) and sees the current structure as transitional.
constraint_indexing:constraint_classification(debt_leverage_as_consent_manufacturing, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: HR CONSULTING INDUSTRY (PITON) — The professional apparatus that designs and administers signing bonus structures has largely become theatrical: 'retention analytics,' 'employee engagement metrics,' and 'total rewards optimization' are performative rituals that obscure the core mechanism (debt creates compliance). The consulting industry maintains the complexity theater because simplification would reveal the extraction. High theater ratio — most of the professional activity is justification and obfuscation rather than functional design. The industry sees its own process as degraded but persists through institutional inertia and client demand for legitimation.
constraint_indexing:constraint_classification(debt_leverage_as_consent_manufacturing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a structural view, debt-leverage exhibits both genuine coordination (solves liquidity mismatch between worker need and employer retention interest) and asymmetric extraction (transforms economic dependency into manufactured consent, suppresses labor organizing, shifts risk from institution to individual household). The constraint requires active enforcement (bonus clawback provisions, legal mechanisms) and produces perspectival gaps that map to structural position. Not a natural law — the specific form (employer-specific debt rather than portable benefits or social insurance) is a contingent institutional arrangement. Tangled rope is the correct analytical classification: irreducible hybrid of coordination and extraction.
constraint_indexing:constraint_classification(debt_leverage_as_consent_manufacturing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_leverage_as_consent_manufacturing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_leverage_as_consent_manufacturing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_leverage_as_consent_manufacturing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_leverage_as_consent_manufacturing, TR),
    TR >= 0.70.

:- end_tests(debt_leverage_as_consent_manufacturing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The signing bonus structure captures worker compliance and suppresses labor organizing, but the extraction is not as severe as pure debt bondage because: (1) workers receive genuine upfront liquidity that solves real household problems, (2) exit is possible at a price (bonus repayment), and (3) some workers have sufficient alternatives to negotiate better terms. The value reflects that the career and compliance asymmetry is real but partly offset by the coordination function. Suppression (0.62): Moderate-high. Significant barriers to exit include bonus clawback provisions (often 1-2x annual salary), household dependency (mortgage, childcare, medical costs), identity fusion with provider role, and labor market segmentation. But suppression is not total — some workers do leave, some negotiate better terms, and some organize collectively despite the debt. The suppression has increased over the interval as signing bonuses have grown relative to household income and as HR practices have become more sophisticated at targeting workers with dependents. Theater ratio (0.58): Moderate-high. The HR consulting industry has built an elaborate apparatus of retention analytics, engagement metrics, and total rewards optimization that is substantially performative. Most of this activity obscures rather than optimizes — the core mechanism (debt creates compliance) is simple, but the professional apparatus exists to provide legitimation and complexity. The theater has increased as the mechanism has become more controversial and required more justification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural mechanism — debt obligations to dependents — produces radically different experiences depending on the agent's identity fusion, exit options, and relationship to the extraction flow. The indebted worker with provider identity fusion sees a snare: exit is unthinkable, every compliance decision is justified through family welfare, and the debt feels like an inescapable trap. The worker with alternatives sees tangled rope: genuine coordination (liquidity, stability) mixed with extraction (compliance pressure, reduced leverage). The employing institution sees pure rope: a fair exchange of upfront capital for retention and a solution to competitive labor market pressures. The labor organizing coalition sees the dual structure explicitly: coordination that enables household stability alongside extraction that fragments collective action. The progressive employer sees a scaffold: a temporary solution being replaced by portable benefits and social safety nets. The HR consulting industry sees a piton: a degraded professional apparatus maintained through complexity theater. The analytical observer synthesizes these perspectives into the structural reality: an irreducible hybrid where genuine coordination and asymmetric extraction are inseparable, and where the binding mechanism varies by agent (identity-lock for some, cost-benefit for others, pure coordination for beneficiaries).
 *
 * DIRECTIONALITY LOGIC:
 *   The indebted worker is identity-locked: structurally mobile (could change jobs, relocate, default) but cognitively trapped by provider role fusion. The identity lock is the binding mechanism — 'I can't leave because my family depends on me' naturalizes institutional extraction as family duty. This produces high directionality (d ≈ 0.89) and high experienced extraction despite moderate base extractiveness. The worker with alternatives is constrained rather than identity-locked: sees the extraction clearly, calculates exit cost, decides it exceeds tolerance. This produces moderate directionality (d ≈ 0.65) and moderate experienced extraction. The employing institution is the primary beneficiary with arbitrage exit options: can adjust bonus structures, shift to alternative retention mechanisms, or accept higher turnover. This produces low directionality (d ≈ 0.10) and experiences the constraint as coordination. The labor organizing coalition occupies a mixed position: benefits from household stability (coordination) but victimized by fragmented collective action (extraction). Organized power provides some agency but exit is constrained. The progressive employer has mobile exit options (can adopt alternative models) and sees the structure as transitional, producing low experienced extraction. The HR consulting industry maintains the theatrical apparatus and experiences low extraction (beneficiary of complexity demand). The analytical observer identifies the irreducible hybrid structure and maps the perspectival gaps to structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that the classification depends on the observer's structural position and the specific binding mechanism. For the identity-locked worker, it genuinely is a snare — exit is cognitively impossible despite structural mobility. For the worker with alternatives, it genuinely is tangled rope — mixed coordination and extraction with high but surmountable exit costs. For the employing institution, it genuinely is rope — a coordination mechanism that solves retention problems. For the labor organizing coalition, it genuinely is tangled rope — irreducible mixture of household stability and collective action suppression. For the progressive employer, it genuinely is scaffold — a temporary solution with a sunset path. For the HR consulting industry, it genuinely is piton — a degraded professional apparatus maintained through theater. The analytical classification is tangled rope because the constraint exhibits both genuine coordination (liquidity provision, household stability) and asymmetric extraction (compliance manufacturing, organizing suppression) that cannot be separated — eliminating the extraction would eliminate the coordination, and vice versa. The perspectival diversity is not measurement error; it is the structural reality of a constraint that operates through multiple mechanisms simultaneously (material debt, identity fusion, cost-benefit calculation, institutional legitimation) and produces different experiences for agents in different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liquidity_alternative_sufficiency,
    'Would portable benefits or social safety nets provide equivalent household liquidity without employer-specific lock-in?',
    'Comparative analysis of worker mobility and household stability in jurisdictions with strong social insurance vs signing-bonus-dependent labor markets; longitudinal tracking of workers who transition between systems',
    'If alternatives provide equivalent liquidity: the employer-specific debt structure is pure extraction disguised as coordination (Snare from more perspectives). If alternatives fail to match liquidity: the signing bonus structure has genuine irreducible coordination function (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liquidity_alternative_sufficiency, empirical, 'Whether alternative liquidity mechanisms can replace signing bonuses without household welfare loss').

omega_variable(
    identity_lock_prevalence,
    'What proportion of workers are identity-locked (provider role fusion) vs structurally trapped (no alternative employment) vs genuinely constrained (high but surmountable exit costs)?',
    'Survey data on exit decision framing: ''I can''t leave because...'' responses coded as identity (family duty), structural (no jobs), or cost-benefit (too expensive). Cross-reference with objective exit capacity (skills, local labor market, savings).',
    'If majority identity-locked: the constraint operates primarily through internalized obligation rather than material barriers, and the suppression mechanism is cognitive rather than structural. If majority structurally trapped: the constraint is material coercion with family-welfare framing as post-hoc justification. If majority constrained: the constraint is a cost-benefit calculation where the institution has set the exit price above most workers'' tolerance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_prevalence, empirical, 'Distribution of binding mechanisms across worker population').

omega_variable(
    household_dependency_causality,
    'Does financial obligation to dependents cause compliance, or does the institution select for workers with dependents to manufacture compliance?',
    'Hiring pattern analysis: do institutions preferentially hire workers with dependents? Longitudinal tracking: does compliance increase after workers acquire dependents? Experimental: do institutions offer larger signing bonuses to workers with dependents?',
    'If selection effect dominates: the constraint is institutional design (employers engineer the worker population to maximize debt-leverage effectiveness). If causal effect dominates: the constraint is opportunistic exploitation of pre-existing household structures. If both: the constraint exhibits feedback loops where selection and causation reinforce each other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(household_dependency_causality, empirical, 'Whether household dependency is cause, selection criterion, or both').

omega_variable(
    consent_authenticity_threshold,
    'At what debt-to-income ratio does manufactured consent become indistinguishable from coercion?',
    'Legal precedent analysis in debt bondage cases; psychological research on decision-making under financial duress; cross-cultural comparison of what debt levels are considered coercive vs acceptable',
    'If threshold is low (e.g., 0.3x annual income): most signing bonus structures exceed it and should be classified as coercive rather than consensual. If threshold is high (e.g., 2.0x annual income): most structures remain in the consent range and the ''manufactured'' framing overstates the extraction. The threshold determines whether the constraint is Tangled Rope (mixed consent and extraction) or Snare (coercion with consent theater).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_authenticity_threshold, preference, 'Debt-to-income threshold distinguishing manufactured consent from coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_leverage_as_consent_manufacturing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial, debt_leverage_as_consent_manufacturing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_early, debt_leverage_as_consent_manufacturing, theater_ratio, 3, 0.42).
narrative_ontology:measurement(theater_mid, debt_leverage_as_consent_manufacturing, theater_ratio, 6, 0.5).
narrative_ontology:measurement(theater_final, debt_leverage_as_consent_manufacturing, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(extract_initial, debt_leverage_as_consent_manufacturing, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(extract_early, debt_leverage_as_consent_manufacturing, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(extract_mid, debt_leverage_as_consent_manufacturing, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(extract_final, debt_leverage_as_consent_manufacturing, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_leverage_as_consent_manufacturing, resource_allocation).
narrative_ontology:affects_constraint(debt_leverage_as_consent_manufacturing, non_compete_enforcement).
narrative_ontology:affects_constraint(debt_leverage_as_consent_manufacturing, occupational_licensing_barriers).
narrative_ontology:affects_constraint(debt_leverage_as_consent_manufacturing, employer_sponsored_healthcare).

% DUAL FORMULATION NOTE:
% Debt-leverage as consent manufacturing is part of a constraint family that includes other employer-specific lock-in mechanisms. Non-compete agreements, occupational licensing, and employer-sponsored healthcare all operate through similar structural logic: tie individual worker welfare to employer-specific arrangements to suppress labor mobility and organizing capacity. Each has its own extractiveness value reflecting the specific mechanism, but all share the core pattern of fragmenting collective action by individualizing risk and dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(debt_leverage_as_consent_manufacturing, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
