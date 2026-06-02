% ============================================================================
% CONSTRAINT STORY: inheritance_obligation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inheritance_obligation_asymmetry, []).

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
 *   constraint_id: inheritance_obligation_asymmetry
 *   human_readable: Inheritance Obligation Asymmetry: Familial Extraction Through Transgenerational Duty
 *   domain: family/intergenerational/economic
 *
 * SUMMARY:
 *   Inheritance obligation asymmetry is the structural extraction of labor,
 *   deferred life opportunities, and asset control from heirs through the
 *   mechanism of familial duty and intergenerational succession. The
 *   constraint operates through multiple channels: legal framework (wills,
 *   intestacy, trusts), emotional mechanism (filial love, guilt, shame),
 *   economic mechanism (dependency, property rights, wealth concentration),
 *   and identity mechanism (family reputation, loyalty, selfhood constituted
 *   through obligation). The extractiveness has increased over the
 *   measurement interval from 0.35 to 0.58, driven primarily by increasing
 *   complexity of estate management, longer lifespans requiring extended
 *   eldercare, and concentration of wealth in fewer hands. The theater ratio
 *   remains relatively low (0.48), indicating that the obligation is
 *   functionally enforced rather than performative — heirs actually perform
 *   the labor (caregiving, management, emotional work), not merely enact
 *   obligation. The constraint exhibits all six DR types from different
 *   perspectives, revealing that inheritance obligation is genuinely a mixed
 *   coordination-extraction hybrid. For the dutiful child with
 *   identity-locked exit options, it is pure snare. For the organized reform
 *   movement (estate planning, professional services), it is a scaffolding
 *   constraint with a real sunset as transparent planning mechanisms mature.
 *
 * KEY AGENTS:
 *   - Dutiful Child / Obligated Heir: Primary victim (powerless/identity_locked) — identity fused with filial obligation; structurally mobile but psychologically trapped
 *   - Economically Dependent Heir: Secondary victim (moderate/constrained) — faces material barriers to exit; also receives inheritance benefit
 *   - Dispossessed Sibling: Victim (powerless/trapped) — excluded from inheritance but subject to obligation; experiences pure extraction
 *   - Wealth Accumulator / Testator: Primary beneficiary (institutional/arbitrage) — controls asset distribution; benefits from heir obligation
 *   - Estate Controller / Executor: Secondary beneficiary (institutional/arbitrage) — institutional actors who benefit from clear obligation framework
 *   - Estate Planning Reform Movement: Organized agents (organized/constrained) — professionals and family mediators building transparent planning tools; sunset mechanism
 *   - Primogeniture and Intestacy Law: Institutional constraint (institutional/arbitrage) — legal framework persisting through inertia despite functional replacement (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination (asset distribution, eldercare) and extraction (wealth concentration, uncompensated labor)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inheritance_obligation_asymmetry, 0.58).
domain_priors:suppression_score(inheritance_obligation_asymmetry, 0.65).
domain_priors:theater_ratio(inheritance_obligation_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inheritance_obligation_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(inheritance_obligation_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(inheritance_obligation_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inheritance_obligation_asymmetry, tangled_rope).
narrative_ontology:human_readable(inheritance_obligation_asymmetry, "Inheritance Obligation Asymmetry: Familial Extraction Through Transgenerational Duty").
narrative_ontology:topic_domain(inheritance_obligation_asymmetry, "family/intergenerational/economic").

domain_priors:requires_active_enforcement(inheritance_obligation_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inheritance_obligation_asymmetry, wealth_accumulator).
narrative_ontology:constraint_beneficiary(inheritance_obligation_asymmetry, estate_controller).
narrative_ontology:constraint_victim(inheritance_obligation_asymmetry, obligated_heirs).
narrative_ontology:constraint_victim(inheritance_obligation_asymmetry, dispossessed_siblings).
narrative_ontology:constraint_victim(inheritance_obligation_asymmetry, younger_generation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DUTIFUL CHILD (SNARE) — Identity fused with filial obligation. The obligated heir cannot imagine refusing parental expectations without becoming a different person (ungrateful, disloyal, unworthy). Structurally mobile — could refuse inheritance duties, has income, could relocate — but identity lock prevents exercise of exit. Experiences maximum extraction: defers life plans, absorbs parental caregiving, manages estate liquidation, resolves family conflicts, all with internalized framing that refusal is unthinkable.
constraint_indexing:constraint_classification(inheritance_obligation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: ECONOMICALLY DEPENDENT HEIR (TANGLED ROPE) — Constrained by material dependency: may live in family property, depend on parental financial support, face relocation costs. But also benefits from inheritance prospect and intergenerational wealth transfer. Mixed extraction and coordination: the obligation contains genuine family coordination (eldercare, estate management) alongside asymmetric extraction (uncompensated labor, deferred opportunities).
constraint_indexing:constraint_classification(inheritance_obligation_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTATE CONTROLLER (ROPE) — Sees obligation as pure coordination. The will, the executor role, the probate process all solve collective problems: determining distribution, settling debts, liquidating assets. For institutional actors (law firms, trust companies, executors with fiduciary duties), the obligation is a framework enabling their function with minimal extraction — they benefit from clear rules and enforceable duties, not from the asymmetry itself.
constraint_indexing:constraint_classification(inheritance_obligation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISPOSSESSED SIBLING (SNARE) — Trapped by birth order and parental preference. Excluded from inheritance obligation because deemed unworthy, unreliable, or wrong sex/gender. Experiences extraction without the beneficiary's promise: no inheritance to offset the family labor obligations, no estate control to compensate for exclusion. Pure extraction with high suppression — legal system enforces parental will, social norm of filial duty applies regardless of inclusion in bequest, no recourse.
constraint_indexing:constraint_classification(inheritance_obligation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: ESTATE PLANNING REFORM (SCAFFOLD) — Organized agents (estate lawyers, tax advisors, family wealth consultants) see obligation asymmetry as a temporary coordination failure being solved by transparent planning tools: trusts with explicit conditions, mediated inheritance meetings, documented family agreements, professional eldercare replacing filial caregiving. The sunset is real: as professional services and legal mechanisms mature, the informal obligation-based extraction mechanism loses force. Families with resources can now structure inheritance via contracts with explicit terms rather than implicit duty.
constraint_indexing:constraint_classification(inheritance_obligation_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PRIMOGENITURE AND INTESTACY REGIMES (PITON) — Historical frameworks (male primogeniture, automatic intestacy rules, spousal override provisions) persist through legal inertia despite reduced functional relevance. Formal law now allows flexible testamentary arrangements and gender-neutral distribution, but the theater of family obligation persists: families still reproduce the primogeniture hierarchy informally through parental preference, even when legal structures permit alternatives. The piton classification reflects that the formal constraint's original function (ensuring estate consolidation and clear succession) has been replaced by procedural mechanisms, yet the obligation machinery persists.
constraint_indexing:constraint_classification(inheritance_obligation_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, inheritance obligation serves genuine coordination (directing assets to those with highest future need, maintaining family continuity, distributing caregiving responsibilities) alongside extraction (concentrating wealth, binding heirs to parental preferences beyond death, enforcing uncompensated labor). The constraint has both functions, cannot be reduced to either, and the asymmetry is the mechanism by which it solves coordination via extraction.
constraint_indexing:constraint_classification(inheritance_obligation_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inheritance_obligation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inheritance_obligation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inheritance_obligation_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inheritance_obligation_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inheritance_obligation_asymmetry, TR),
    TR >= 0.70.

:- end_tests(inheritance_obligation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from obligated heirs through uncompensated eldercare labor (estimated at $5k-$15k annually per study), deferred career opportunities (opportunity cost during caregiving periods), and post-mortem asset control (testator's preferences enforced on heir's economic life for years after death). However, the extraction is not maximal (0.70+) because: (1) many heirs receive inheritance benefit that partially offsets obligations, (2) estate planning reforms provide exits for those with resources, (3) the constraint operates through identity and emotion rather than pure coercion. The measurement trajectory (0.35→0.58) reflects increasing extractiveness as lifespans extend, medical costs rise, and wealth concentration creates more complex estates. Suppression (0.65): Moderate-high. Barriers to refusing obligation include: legal enforceability of wills (structural), social shame and family ostracism (social), emotional guilt and identity fusion (psychological), economic dependency on parental property or income (structural). Suppression has both structural and internalized components in roughly equal measure — a heir with independent income and secular community might refuse obligation more easily than a heir embedded in conservative family culture despite identical legal framework. Theater ratio (0.48): Moderate-low. The obligation is functionally enforced rather than performative. Heirs actually provide eldercare, manage estates, attend probate hearings, not merely enact obligation symbolically. The theater emerges in specific moments (funeral rituals, will reading, family meetings) but the primary mechanism is real labor extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a perspectival gap between the dutiful child's experience (snare: identity-locked, maximum extraction) and the estate controller's experience (rope: pure coordination). The gap is not perceptual difference but structural — they literally occupy different positions in the extraction flow. The wealth accumulator sees rope (beneficiary/arbitrage); the obligated heir sees snare (victim/identity_locked); the reform movement sees scaffold (sunset mechanism real). The dispossessed sibling experiences snare with even higher extraction than the dutiful child — excluded from inheritance benefit but still subject to family obligation norms. The analytical observer sees tangled rope — the constraint genuinely coordinates family assets and eldercare AND asymmetrically extracts from heirs, both functions are real. The piton classification (primogeniture and intestacy law) reveals that formal legal obligation has been functionally replaced by transparent planning mechanisms, yet the informal obligation machinery persists through family culture and identity fusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declaration (wealth accumulator, estate controller) drives low directionality for institutional perspectives. Victim declaration (obligated heirs, dispossessed sibling, younger generation) drives high directionality for powerless and moderate perspectives. Identity-locked exit modulation prevents the dutiful child from escaping high d via mobility (structurally mobile but psychologically trapped). Constrained exit for economically dependent heir produces moderate d — material barriers exist but are surmountable at cost. Trapped exit for dispossessed sibling produces maximum d — legal system enforces exclusion, social norm enforces obligation, no structural escape. The measurement trajectory of increasing extractiveness (0.35→0.58) reflects accumulation of obligation without corresponding relief mechanisms — obligation tightens as estates grow complex and lifespans extend.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: Inheritance obligation asymmetry IS a genuine hybrid constraint. The mandatrophy is resolved by showing that the constraint performs both coordination and extraction simultaneously, not sequentially. Coordination function: directing assets to heirs, managing estate liquidation, distributing caregiving responsibilities, maintaining family continuity across generations — these are real coordination problems the obligation solves. Extraction function: concentrating wealth in testator's hands beyond death, binding heirs to deferred opportunities, extracting uncompensated labor from obligated children, creating hierarchy between heirs — these are real asymmetric extraction. The constraint is not 'coordination masquerading as extraction' nor 'extraction disguised as coordination' — it is genuinely both. The tangled rope classification captures that the obligation cannot be reduced to either pure function. The perspectival gaps (dutiful child sees snare, reform movement sees scaffold, analytical observer sees tangled rope) reveal that the functional emphasis shifts depending on structural position and power level. A structural observer recognizing both functions simultaneously arrives at tangled rope. A victim experiencing powerlessness arrives at snare. An institutional beneficiary arrives at rope. The mandatrophy dissolves when we stop asking 'which is the true classification?' and recognize that all five are legitimate readings from valid observation positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_ambiguity,
    'For obligated heirs with sufficient structural exit capacity (income, housing, legal standing), is the binding mechanism identity fusion (identity_locked) or high material cost (constrained)?',
    'Post-exit trajectory: if heir maintains guilt, self-recrimination, identity fragments after refusing obligation despite having structural capacity to refuse, classify as identity_locked. If heir feels relieved, reorganizes identity successfully after exiting, classify as constrained with high cost.',
    'If identity_locked: suppression is higher than structural metrics suggest — the heir carries the binding mechanism internally. If constrained: suppression is material and can be reduced by removing barriers (professional caregiving, transparent inheritance planning). Classification affects intervention strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_ambiguity, empirical, 'Mechanism of obligated heir''s binding: identity fusion vs. high material cost').

omega_variable(
    intergenerational_extraction_accumulation,
    'Does inheritance obligation escalate in severity across generations (younger cohorts experiencing higher extractiveness) or remain stable?',
    'Longitudinal family histories; comparison of obligation severity reported by grandparents vs. parents vs. children within same family; generational surveys on inheritance expectation vs. actual obligation experience.',
    'If escalating: constraint is accumulating like a snare tightening (classify higher extractiveness for younger cohorts). If stable: constraint maintains steady-state extraction (current 0.58 applies across time). If declining: open-science-style sunset is real (scaffold classification gains strength).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_extraction_accumulation, empirical, 'Whether obligation intensity escalates or declines across generations').

omega_variable(
    wealth_concentration_driver,
    'Is inheritance obligation primarily a mechanism for maintaining wealth concentration across generations, or a genuine coordination mechanism for family resource distribution?',
    'Comparative analysis: families with high wealth show higher obligation intensity than families with low wealth. Within families, heirs to large estates report higher obligation than heirs to small/middle-class estates. Intergenerational mobility: do high-obligation families show lower economic mobility for younger generation, or better coordinated resource distribution?',
    'If primarily wealth concentration: extractiveness should be classified higher (0.65+), beneficiary should emphasize wealth accumulator over estate controller. If primarily coordination: extractiveness justified at current level (0.58), emphasizing legitimate family function. Current classification treats both simultaneously (tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wealth_concentration_driver, empirical, 'Whether obligation primarily concentrates wealth or coordinates family resources').

omega_variable(
    suppression_structural_vs_internalized,
    'Is suppression of heir resistance primarily structural (legal enforceability, social sanction, economic dependency) or internalized (guilt, identity fusion, epistemic closure about alternatives)?',
    'Variation analysis: heirs in jurisdictions with weak inheritance enforcement report similar obligation intensity as those with strong enforcement (suggests internalized suppression). Heirs exposed to alternative family models report lower obligation intensity than those in homogeneous communities (suggests cognitive/cultural suppression is significant). Post-family therapy: do heirs who undergo identity work maintain reduced obligation intensity absent external reinforcement?',
    'If primarily structural: targeted legal/economic interventions (professional caregiving, transparent trusts, estate taxation) can reduce suppression. If primarily internalized: therapeutic and identity-level interventions required; structural changes alone insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Proportion of suppression that is structural vs. internalized').

omega_variable(
    gender_asymmetry_in_obligation,
    'Do female heirs report higher extractiveness and identity-lock intensity than male heirs across the same family structures?',
    'Gender-stratified family history analysis. Within families with multiple children, compare obligation intensity reported by daughters vs. sons in identical structural positions (same birth order, same economic position, same inheritance percentage). Cross-cultural comparison of gender asymmetry magnitude.',
    'If pronounced: consider separate constraint stories for gender-specific obligation mechanisms (they may have different ε values). Current story may average across two populations with meaningfully different extractiveness profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_asymmetry_in_obligation, empirical, 'Gender asymmetry in inheritance obligation intensity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inheritance_obligation_asymmetry, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inhob_tr_t0, inheritance_obligation_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(inhob_tr_t10, inheritance_obligation_asymmetry, theater_ratio, 10, 0.41).
narrative_ontology:measurement(inhob_tr_t20, inheritance_obligation_asymmetry, theater_ratio, 20, 0.48).
narrative_ontology:measurement(inhob_tr_t30, inheritance_obligation_asymmetry, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(inhob_be_t0, inheritance_obligation_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inhob_be_t10, inheritance_obligation_asymmetry, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(inhob_be_t20, inheritance_obligation_asymmetry, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(inhob_be_t30, inheritance_obligation_asymmetry, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inheritance_obligation_asymmetry, attachment_coordination).
narrative_ontology:boltzmann_floor_override(inheritance_obligation_asymmetry, 0.12).
narrative_ontology:affects_constraint(inheritance_obligation_asymmetry, intergenerational_wealth_concentration).
narrative_ontology:affects_constraint(inheritance_obligation_asymmetry, elder_care_debt_trap).
narrative_ontology:affects_constraint(inheritance_obligation_asymmetry, filial_piety_identity_lock).
narrative_ontology:affects_constraint(inheritance_obligation_asymmetry, probate_system_extraction).

% DUAL FORMULATION NOTE:
% Inheritance obligation asymmetry is part of a constraint family in intergenerational extraction. Upstream constraint: intergenerational wealth concentration (ε≈0.70, snare-type) — the macro-level wealth asymmetry that creates the conditions for obligation extraction. Current constraint: obligation asymmetry (ε≈0.58, tangled rope) — the mechanism by which obligation enforces concentration. Downstream constraints: specific elaborations like elder care as debt trap (ε≈0.75, snare) and identity-fusion mechanisms (identity_locked psychological binding, ε≈0.82). Each story has its own beneficiary/victim structure; they are linked causally and reinforcing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(inheritance_obligation_asymmetry, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
