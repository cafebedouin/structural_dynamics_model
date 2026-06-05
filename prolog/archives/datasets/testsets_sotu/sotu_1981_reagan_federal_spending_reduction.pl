% ============================================================================
% CONSTRAINT STORY: sotu_1981_reagan_federal_spending_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1981_reagan_federal_spending_reduction, []).

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
 *   constraint_id: sotu_1981_reagan_federal_spending_reduction
 *   human_readable: Federal Spending Growth Reduction via Direct Cuts and User Fees (Reagan 1981 SOTU)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The Federal Spending Growth Reduction constraint (Reagan 1981 SOTU)
 *   establishes a fiscal discipline mechanism requiring Congress to reduce
 *   federal spending growth by $49.1 billion in FY1982 through $41.4B in
 *   direct cuts and $7.7B in user fees. The constraint exemplifies a tangled
 *   rope structure: it functions as a coordination mechanism for supply-side
 *   recovery theory (disciplining inflation through reduced federal
 *   borrowing) while simultaneously extracting costs from non-protected
 *   discretionary programs, regional/local government subsidies, and
 *   arts/humanities funding. The constraint benefits deficit-reduction
 *   advocates and inflation-conscious taxpayers by anchoring Reagan's
 *   macroeconomic strategy; it bears costs on powerless institutional actors
 *   (arts organizations) with no alternative funding sources and moderately
 *   constrained actors (regional governments) with limited fiscal autonomy.
 *   The theater ratio (0.48) reflects that formal enforcement mechanisms (CBO
 *   target tracking, OMB controls) exist but are subject to legislative
 *   override through supplemental appropriations and definitional
 *   redefinition. The extractiveness trajectory (0.35 → 0.58) shows
 *   increasing extraction as legislative constituencies navigate the
 *   constraints and protective coalitions solidify around exempted programs
 *   (Social Security, Medicare), concentrating cuts on politically weaker
 *   programs.
 *
 * KEY AGENTS:
 *   - Deficit-Reduction Advocates: Primary beneficiary (institutional/arbitrage) — supply-side coalition benefits from reduced federal borrowing pressure and policy alignment with inflation-control strategy
 *   - Arts and Humanities Institutions: Primary victim (powerless/trapped) — face institutional collapse without federal funding; no alternative revenue mechanisms at scale
 *   - Regional and Local Governments: Secondary victim (moderate/constrained) — face revenue-sharing reductions but benefit from lower interest rates; constrained by state-level political choices
 *   - Protected Entitlements (Social Security, Medicare): Protected beneficiary (powerful/mobile) — structurally exempt from cuts; see constraint as pure coordination
 *   - Congress and Legislative Branch: Enforcer with agency (organized/constrained) — must allocate cuts but faces electoral pressure and budgetary constraints
 *   - Congressional Budget Office and OMB: Nominal enforcement actors (institutional/arbitrage) — track spending targets but lack final appropriations authority; piton classification reflects degraded enforcement function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing fiscal discipline as immutable law rather than political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1981_reagan_federal_spending_reduction, 0.58).
domain_priors:suppression_score(sotu_1981_reagan_federal_spending_reduction, 0.65).
domain_priors:theater_ratio(sotu_1981_reagan_federal_spending_reduction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1981_reagan_federal_spending_reduction, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1981_reagan_federal_spending_reduction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1981_reagan_federal_spending_reduction, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1981_reagan_federal_spending_reduction, tangled_rope).
narrative_ontology:human_readable(sotu_1981_reagan_federal_spending_reduction, "Federal Spending Growth Reduction via Direct Cuts and User Fees (Reagan 1981 SOTU)").
narrative_ontology:topic_domain(sotu_1981_reagan_federal_spending_reduction, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1981_reagan_federal_spending_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1981_reagan_federal_spending_reduction, deficit_reduction_advocates).
narrative_ontology:constraint_beneficiary(sotu_1981_reagan_federal_spending_reduction, inflation_conscious_taxpayers).
narrative_ontology:constraint_beneficiary(sotu_1981_reagan_federal_spending_reduction, supply_side_recovery_coalition).
narrative_ontology:constraint_victim(sotu_1981_reagan_federal_spending_reduction, discretionary_program_beneficiaries).
narrative_ontology:constraint_victim(sotu_1981_reagan_federal_spending_reduction, regional_local_governments).
narrative_ontology:constraint_victim(sotu_1981_reagan_federal_spending_reduction, arts_humanities_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARTS AND HUMANITIES INSTITUTIONS (SNARE) — Cannot exit the federal funding system without institutional collapse. Museums, humanities councils, and smaller cultural organizations face no alternative revenue mechanisms at scale. The $7.7B user fee and direct cut mechanisms apply disproportionately to arts/humanities budgets (non-protected discretionary). Maximum experienced extraction with zero exit capacity.
constraint_indexing:constraint_classification(sotu_1981_reagan_federal_spending_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL AND LOCAL GOVERNMENTS (TANGLED ROPE) — Constrained by federal revenue-sharing reductions and program subsidies cuts. However, also benefit from reduced federal borrowing pressure (lower interest rates) and some may capture state-level revenue opportunities. Significant extraction but not total — agency exists through state taxation and budget reallocation at cost.
constraint_indexing:constraint_classification(sotu_1981_reagan_federal_spending_reduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEFICIT-REDUCTION ADVOCATES AND SUPPLY-SIDE COALITION (ROPE) — Primary beneficiaries. Experience the constraint as coordination: federal spending discipline is the mechanism that enables supply-side recovery theory. The $49.1B reduction (direct cuts + fees) serves their policy objective of reducing inflationary pressure and restoring investment incentives. Minimal suppression, genuine policy coordination function.
constraint_indexing:constraint_classification(sotu_1981_reagan_federal_spending_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTECTED ENTITLEMENT PROGRAMS (ROPE) — Experience the constraint as pure coordination mechanism: the spending reduction is explicitly structured to exempt Social Security and Medicare from cuts. These programs see the constraint as a policy choice that protects them while disciplining the rest of the budget. Mobile options exist through legislative amendment; suppression is near zero.
constraint_indexing:constraint_classification(sotu_1981_reagan_federal_spending_reduction, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESS AND LEGISLATIVE BRANCH (TANGLED ROPE) — Constrained by budgetary arithmetic and electoral pressure (must balance deficit reduction against constituency demands). Benefits from reduced federal borrowing pressure (lower interest rates available for private investment). Organized agents with agency over cut allocation, but not over whether cuts happen. Active enforcement required through appropriations mechanisms.
constraint_indexing:constraint_classification(sotu_1981_reagan_federal_spending_reduction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONGRESSIONAL BUDGET OFFICE AND OMB ENFORCEMENT (PITON) — Nominal enforcement mechanism with degraded functional authority. The $49.1B target is largely performative because Congress retains final appropriations authority and can reallocate cuts through supplemental spending, emergency measures, and base-year redefinition. Theater ratio (0.48) reflects this: formal discipline mechanisms exist but are subject to legislative override. CBO compiles the data; Congress decides what counts.
constraint_indexing:constraint_classification(sotu_1981_reagan_federal_spending_reduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, fiscal constraints appear immutable: the zero-sum nature of budget allocation and the arithmetic of inflation vs growth appear as natural laws. The spending reduction is framed as an inevitable correction to unsustainable fiscal trajectories. However, this masks the contingent political choice: budget discipline is a policy instrument, not a law of nature. The engine will flag this as a false summit — the 'natural' deficit control is actually constructed through beneficiary coalition power and institutional choices.
constraint_indexing:constraint_classification(sotu_1981_reagan_federal_spending_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1981_reagan_federal_spending_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1981_reagan_federal_spending_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1981_reagan_federal_spending_reduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1981_reagan_federal_spending_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1981_reagan_federal_spending_reduction, TR),
    TR >= 0.70.

:- end_tests(sotu_1981_reagan_federal_spending_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint redistributes ~$49.1B in budget authority from discretionary and user-fee programs toward deficit reduction and protected entitlements. This is extraction in the sense that some actors (arts institutions, regional governments) bear concentrated costs while beneficiaries (deficit hawks, protected entitlement beneficiaries) see gains. However, extractiveness is not higher (e.g., 0.70+) because the extraction is mediated through democratic budgeting processes and some compensatory mechanisms exist (lower interest rates for constrained actors). Suppression (0.65): Moderate-high. Significant structural barriers prevent non-protected discretionary programs from resisting the constraint: arts organizations cannot exit the federal system without closure; regional governments cannot unilaterally replace lost revenue; Congress cannot ignore deficit arithmetic. However, suppression is not total (0.80+) because legislative mechanisms exist to renegotiate allocations, and protected programs demonstrate that exemptions can be secured through political organization. Theater ratio (0.48): Moderate. Formal enforcement mechanisms exist (CBO targets, OMB controls, appropriations processes) and have genuine functional effects on agency budgets. However, the mechanisms are partially theater because Congress retains final appropriations authority and can circumvent spending targets through supplemental spending, emergency declarations, and redefinitions of baseline spending. The ratio increases over the interval (0.35 → 0.48) as Congress develops workarounds and the nominal targets become increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence across power levels. The deficit-reduction coalition sees the spending reduction as necessary fiscal discipline and supply-side coordination — the constraint solves the macroeconomic problem of inflation-driven expectations. Arts institutions and regional governments see the same constraint as arbitrary extraction — they bear costs without participating in the policy choice. Congress sees the constraint as both: they benefit from the political framing ('fiscal responsibility') while bearing enforcement costs and facing electoral pressure from harmed constituencies. The protected entitlements (Social Security, Medicare) see pure coordination — the constraint protects them while disciplining others. The Congressional Budget Office sees the constraint as degraded — they maintain the nominal discipline mechanism while knowing Congress can override through supplemental spending. The analytical observer risks seeing the constraint as natural law (fiscal necessity) when it is actually a contingent political choice that reflects specific beneficiary coalition power. The perspectival gap is thus not just about different agents seeing different types, but about the constraint's framing as inevitable vs chosen, which is itself a core empirical question (resolved through omega variable investigation).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural relationship to the extraction flow. Deficit-reduction advocates occupy d ≈ 0.05 (beneficiaries with arbitrage options) — they benefit from the constraint's existence and can exit by supporting alternative fiscal frameworks, but the constraint aligns with their preferences so exit is not attractive. Arts institutions occupy d ≈ 0.95 (victims with trapped exit) — they bear concentrated costs and cannot exit without institutional failure. Regional governments occupy d ≈ 0.65 (victims with constrained exit) — they bear costs but can partially offset through state-level taxation and reallocation, so exit is expensive but not impossible. Protected entitlements occupy d ≈ 0.15 (beneficiaries with mobile exit) — they benefit from explicit protection and could legally change their status through legislative amendment, but legislative politics make amendment unlikely. Congress occupies d ≈ 0.50 (symmetric position) — they impose the constraint but also bear its enforcement costs and must navigate electoral pressure. These d values map to experienced extraction (χ) through the sigmoid function, producing the perspectival gap: beneficiaries see coordination (rope), trapped victims see pure extraction (snare), moderately constrained actors see mixed extraction (tangled rope), and protected groups see pure coordination (rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is not fully resolved (base_properties.mandatrophy_resolved: false) because the constraint's classification as tangled rope vs snare depends on empirical questions that remain unresolved at story generation time. The key uncertainties are: (1) whether the $7.7B user fees actually enforce spending discipline or are largely cosmetic; (2) whether the $41.4B direct cuts follow stated principles or reflect political power distribution; and (3) whether federal spending reduction is the primary driver of inflation reduction or a secondary factor. If resolution favors the supply-side coalition's narrative, the constraint solidifies as tangled rope with legitimate coordination function. If resolution favors the victim's narrative (fees are cosmetic, cuts reflect political power, inflation reduction is driven by Fed tightening), the constraint reclassifies toward pure snare (0.70+) for non-protected programs. The theater ratio (0.48) and increasing extractiveness (0.35 → 0.58) suggest increasing tension between nominal discipline and political override — a pattern consistent with mandatrophy where the constraint's legitimacy depends on whether its stated function (inflation discipline) is actually being performed. The presence of protected programs (Social Security, Medicare) and the use of user fees as a revenue mechanism rather than a true spending reduction both point toward a constraint that functions through political coalition building rather than neutral fiscal discipline. This is the core mandatrophy: is the constraint a Rope (genuine fiscal coordination) that happens to have beneficiaries, or a Snare (extraction) that uses fiscal discipline rhetoric to justify political reallocation?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    user_fee_revenue_realism,
    'Do the $7.7B in user fees actually materialize as programmatic discipline, or do they function as a revenue mechanism that obscures continued government spending?',
    'Retrospective audit of federal user fee collections (1982-1985): comparison of budgeted vs actual revenue; identification of programmatic reductions that directly correspond to fee implementation vs those that happen regardless',
    'If fees constitute real constraint: extractiveness (0.58) confirmed. If fees are largely cosmetic: extractiveness rises to 0.68+ and classification shifts toward pure snare for affected programs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_fee_revenue_realism, empirical, 'Whether $7.7B user fees enforce real spending discipline').

omega_variable(
    discretionary_cut_distribution,
    'How are the $41.4B direct cuts actually allocated across discretionary agencies? Does distribution follow stated principles (means-tested safety net protection) or reflect political power dynamics?',
    'Agency-by-agency budget analysis: comparison of proposed vs actual cuts by program category; identification of protected vs vulnerable constituencies; correlation between cuts and legislative district representation',
    'If distribution follows stated principles: constraint is genuinely tangled rope (coordination + asymmetric extraction). If distribution reflects political power: constraint is snare or pure extraction (0.70+) for politically weak programs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discretionary_cut_distribution, empirical, 'Whether direct cuts follow stated principles or political power distribution').

omega_variable(
    legislative_override_capacity,
    'What is Congress''s actual capacity and willingness to override the spending reduction through supplemental appropriations, emergency declarations, and budget manipulation?',
    'Legislative history 1982-1985: tracking of supplemental appropriations bills, emergency spending measures, and redefinitions of baseline spending; comparison of nominal vs actual spending trajectories',
    'If override capacity is high and used: suppression and theater_ratio both rise (enforcement is weaker than nominal). If Congress respects the constraint: suppression and theater_ratio remain stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_override_capacity, empirical, 'Congress''s capacity to override spending reduction mechanisms').

omega_variable(
    inflation_reduction_attribution,
    'To what degree does federal spending reduction contribute to inflation reduction vs other factors (Volcker Fed rate increases, oil price dynamics, OPEC production, global demand)?',
    'Econometric decomposition: VAR analysis isolating federal spending shock effects on inflation trajectory; comparison with counterfactual scenarios of unchanged spending growth',
    'If federal spending reduction is primary driver of inflation reduction: supply-side coalition''s beneficiary status and rope classification are strongly validated. If other factors dominate: beneficiary status is weaker and constraint may classify as scaffold (temporary mechanism pending monetary policy success).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inflation_reduction_attribution, empirical, 'Attribution of inflation reduction to federal spending vs other factors').

omega_variable(
    false_summit_contingency,
    'Is the spending constraint a natural law of fiscal necessity or a contingent political choice benefiting a specific coalition?',
    'Comparative institutional analysis: examination of alternative fiscal frameworks used by peer nations; identification of beneficiary coalition in Reagan administration; reconstruction of policy design choices that could have been different',
    'If natural law: mountain classification confirmed for analytical perspective. If contingent: false summit detected — engine reclassifies to tangled rope and flags beneficiary coalition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_contingency, conceptual, 'Whether spending reduction is natural law or contingent political choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1981_reagan_federal_spending_reduction, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1981_reagan_federal_spending_reduction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sotu_tr_t2, sotu_1981_reagan_federal_spending_reduction, theater_ratio, 2, 0.42).
narrative_ontology:measurement(sotu_tr_t4, sotu_1981_reagan_federal_spending_reduction, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1981_reagan_federal_spending_reduction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_be_t2, sotu_1981_reagan_federal_spending_reduction, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(sotu_be_t4, sotu_1981_reagan_federal_spending_reduction, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1981_reagan_federal_spending_reduction, resource_allocation).
narrative_ontology:affects_constraint(sotu_1981_reagan_federal_spending_reduction, supply_side_recovery_mechanism).
narrative_ontology:affects_constraint(sotu_1981_reagan_federal_spending_reduction, inflation_control_federal_borrowing).
narrative_ontology:affects_constraint(sotu_1981_reagan_federal_spending_reduction, social_safety_net_protection).

% DUAL FORMULATION NOTE:
% The federal spending reduction constraint decomposes into three structurally distinct claims: (1) direct cuts ($41.4B) which are snares or tangled ropes depending on program protection; (2) user fees ($7.7B) which function as coordination or revenue extraction depending on implementation; (3) protected entitlements (Social Security, Medicare) which are pure rope or scaffolds depending on whether protection is temporary or permanent. This story models the aggregate constraint. Decomposed stories would model individual program categories with distinct ε and suppression values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1981_reagan_federal_spending_reduction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
