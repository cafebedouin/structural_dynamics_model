% ============================================================================
% CONSTRAINT STORY: platonic_coparenting_decoupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platonic_coparenting_decoupling, []).

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
 *   constraint_id: platonic_coparenting_decoupling
 *   human_readable: The Platonic Co-Parenting Modularization
 *   domain: social/familial
 *
 * SUMMARY:
 *   The platonic co-parenting modularization represents a structural shift in
 *   how parenting partnerships are organized relative to romantic
 *   partnerships. Historically, parental and romantic bonds were integrated
 *   into a single institutional form (the nuclear family). The modularization
 *   decouples these bonds: parents can be romantically unpartnered while
 *   maintaining active parental partnership, or conversely maintain romantic
 *   partnership while parenting separately. This constraint exhibits high
 *   perspectival variance: the dependent child experiences pure extraction
 *   (Snare); cooperative co-parents experience mixed coordination and
 *   enforcement (Tangled Rope); the legal industry experiences market
 *   expansion (Rope); reform advocates see temporary scaffolding (Scaffold);
 *   traditional family ideology persists through institutional inertia
 *   (Piton); affluent households can arbitrage the constraint through
 *   purchased flexibility (powerful Tangled Rope); and the analytical
 *   observer risks naturalizing a contingent institutional arrangement as
 *   inevitable (false Mountain). The extractiveness value (0.38) reflects
 *   moderate asymmetric distribution of costs: parental binding obligations
 *   persist even as romantic partnership dissolves, creating asymmetric
 *   enforcement burdens. The theater ratio (0.62) reflects that
 *   modularization is rhetorically framed as 'in the child's best interest'
 *   while maintaining unstated assumptions about family legitimacy, creating
 *   performative rather than functional coordination.
 *
 * KEY AGENTS:
 *   - Dependent Child: Primary victim (powerless/trapped) — cannot exit arrangement; bears costs of fragmented attachments and coordination overhead; suppression through cultural normalization
 *   - Cooperative Co-Parent Dyad: Primary target (moderate/constrained) — benefits from romantic-parental decoupling but constrained by custody law, childcare logistics, economic dependencies; requires active enforcement
 *   - Divorce-Responsive Legal Industry: Primary beneficiary (institutional/arbitrage) — family law, mediation, custody evaluation benefit from modularization; can arbitrage to other domains
 *   - Family Structure Reform Coalition: Organized agents (organized/constrained) — therapists, parenting educators, advocates building alternative co-parenting norms; see modularization as transitional scaffold
 *   - Nuclear Family Ideology: Institutional actor (institutional/arbitrage) — internalized norm persisting through schools, welfare agencies, cultural narratives; degraded by function but maintained through inertia
 *   - Affluent Household Arbitrageur: Secondary beneficiary (powerful/mobile) — can exit suppression through purchased flexibility (separate housing, hired childcare); low experienced extraction
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks false summit by naturalizing contingent institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platonic_coparenting_decoupling, 0.38).
domain_priors:suppression_score(platonic_coparenting_decoupling, 0.48).
domain_priors:theater_ratio(platonic_coparenting_decoupling, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platonic_coparenting_decoupling, extractiveness, 0.38).
narrative_ontology:constraint_metric(platonic_coparenting_decoupling, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(platonic_coparenting_decoupling, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platonic_coparenting_decoupling, tangled_rope).
narrative_ontology:human_readable(platonic_coparenting_decoupling, "The Platonic Co-Parenting Modularization").
narrative_ontology:topic_domain(platonic_coparenting_decoupling, "social/familial").

domain_priors:requires_active_enforcement(platonic_coparenting_decoupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platonic_coparenting_decoupling, child_access_arbitrageurs).
narrative_ontology:constraint_beneficiary(platonic_coparenting_decoupling, exit_seekers_from_romantic_dyads).
narrative_ontology:constraint_victim(platonic_coparenting_decoupling, traditional_family_stability_norms).
narrative_ontology:constraint_victim(platonic_coparenting_decoupling, social_institutional_scaffolding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT CHILD (SNARE) — The child cannot exit the arrangement and lacks agency in the parental configuration. Bears extraction through fragmented attachments, uncertainty about parental availability, and emotional labor managing multiple parental households. No coordination benefit for the child; pure cost-bearing with suppression of alternatives (intact two-parent household, stable single-parent household) through cultural normalization.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COOPERATIVE CO-PARENT DYAD (TANGLED ROPE) — Experiences both coordination and extraction. Benefits from decoupling romantic and parental functions (can exit failed romantic partnerships while maintaining parenting role). Constrained by custody law, childcare logistics, economic dependency patterns, and social sanction against 'breaking up the family.' Enforcement mechanisms (custody arrangements, child support obligations) require active maintenance. Mixed extraction: freedom to exit romance coupled with binding parenting obligation.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DIVORCE-RESPONSIVE LEGAL INDUSTRY (ROPE) — Institutional beneficiary. Family law attorneys, family court infrastructure, mediation services, and custody evaluation experts all benefit from the modularization shift. Experiences the constraint as coordination that enables their market: decoupled parenting requires legal frameworks, custody agreements, support calculations. Low suppression because this actor can arbitrage to other legal domains if family law shrinks. Net beneficiary with minimal coercion — the extraction runs toward this actor.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FAMILY STRUCTURE REFORM COALITION (SCAFFOLD) — Organized actors (family therapists, parenting educators, co-parenting advocates) see platonic co-parenting as a transitional coordination mechanism with sunset logic. The arrangement is scaffolding toward genuine de-coupling norms where parental partnership has no romantic prerequisites. High suppression currently (stigma, institutional resistance) but declining over time as norms mature. Theater ratio declining as co-parenting becomes legitimized. Sunset clause: as cultural norms shift toward parental-romantic independence, the modularization constraint's enforcement needs decline.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NUCLEAR FAMILY IDEOLOGY (PITON) — The internalized norm that parenting should occur within romantic partnership persists through institutional inertia despite declining functional fit. Theater ratio (0.62) reflects performative framing: co-parenting is rhetorically positioned as 'doing what's best for the child' while maintaining stigma against platonic partnership. The ideology is degraded — it no longer functionally organizes parenting — but persists through schools, welfare agencies, and cultural narratives. Extraction mechanism weakens as institutional actors arbitrage away (therapists now offer 'co-parenting' services legitimizing the modular form).
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: AFFLUENT HOUSEHOLD ARBITRAGEUR (TANGLED ROPE) — High-income households can exit the constraint through purchased flexibility: separate housing, independent childcare, nannies, private schools with flexible attendance policies. Benefits from decoupling without bearing suppression costs (can maintain romantic partnership while outsourcing parental coordination to hired staff). Experiences low extraction due to mobility. But the arrangement requires active coordination enforcement (custody structures) even though enforcement is negotiable for affluent actors. Perspectival gap: same constraint, vastly different extraction profiles by economic access.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — Risk of false summit: viewing the parental-romantic decoupling as an immutable feature of human reproduction ('children need stable parenting regardless of parental romantic configuration'). This naturalizes what is actually a contingent institutional choice about which bonds are primary and which are optional. The structural data reveals institutional scaffolding, not natural law: suppression is real, theater ratio is high, and enforcement requires active maintenance.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platonic_coparenting_decoupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platonic_coparenting_decoupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platonic_coparenting_decoupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(platonic_coparenting_decoupling, TR),
    TR >= 0.70.

:- end_tests(platonic_coparenting_decoupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate baseline with upward trajectory. The modularization creates asymmetric obligations: parenting remains binding when romance dissolves, but romantic partnership can exit parenting through separation (with legal/financial friction). This asymmetry creates extraction value for whoever controls the parenting function and can credibly threaten disruption (typically the primary caregiver). The trajectory from 0.18 to 0.38 reflects increasing enforcement clarity as custody law and child support frameworks mature — what was informal extraction (guilt, social pressure) becomes formalized into legal obligation. Suppression (0.48): Moderate-high. Barriers to exiting include: legal enforcement mechanisms (custody orders, child support), childcare infrastructure dependencies, economic lock-in for lower-income households, and social sanction against 'abandoning' parenting role. Not total suppression because some households can arbitrage through wealth, and cultural legitimacy of co-parenting is rising. Theater ratio (0.62): Moderate-high and rising. Modularization is rhetorically framed as progressive ('doing what's best for the child,' 'healthy co-parenting') while maintaining underlying assumption that parenting should ideally occur within romantic partnership. Performance includes: framing enforcement as child-centered rather than adult-constraining; legitimacy-seeking through therapeutic language; stigma management through terminology ('conscious uncoupling'). Theater declines as norms mature toward genuine parental-romantic independence.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The dependent child sees pure extraction (Snare: no benefit, full suppression). The cooperative co-parent sees mixed coordination and enforcement (Tangled Rope: benefits from decoupling but constrained by enforcement). The legal industry sees pure coordination (Rope: benefits without suppression). Reform advocates see temporary scaffolding declining over time (Scaffold: high current suppression declining toward sunset). Traditional ideology sees naturalized necessity (Piton degraded form; or false Mountain if analytically incautious). Affluent households see negotiable constraint (Tangled Rope mobile: can arbitrage suppression through wealth). The analytical observer risks false summit (naturalizing institutional arrangement as inevitable). This perspectival spectrum reveals the constraint as structurally hybrid: neither pure extraction nor pure coordination, but institutionally contingent arrangement maintained through suppression of alternatives and performative legitimation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position: beneficiary status, power level, and exit options. The child (powerless/trapped) has d ≈ 0.95, experiencing near-maximal extraction. The cooperative co-parent (moderate/constrained) has d ≈ 0.58, experiencing mixed costs and benefits from decoupling itself, constrained by enforcement. The legal industry (institutional/arbitrage) has d ≈ 0.05, experiencing only benefits and minimal constraint. The reform coalition (organized/constrained) has d ≈ 0.45, organized enough to see alternatives but constrained by institutional resistance. The nuclear family ideology (institutional/arbitrage) has d ≈ 0.02, benefiting from default assumptions. The affluent arbitrageur (powerful/mobile) has d ≈ 0.35, able to exit suppression through wealth. The analytical observer (analytical/analytical) has d ≈ 0.72, seeing both structure and naturalization risk. These d values are NOT overridden; they flow from the beneficiary/victim declarations and exit option assignments in the base perspective definitions.
 *
 * MANDATROPHY ANALYSIS:
 *   CLASSIFICATION MANDATE: The tangled_rope classification resolves the mandate by requiring evidence of both genuine coordination function AND asymmetric extraction. The modularization does provide coordination benefit (enables parental partnership when romance fails) AND creates asymmetric extraction (parental binding obligations outlast romantic partnership, with enforcement concentrated on primary caregivers). The classification prevents misidentification as pure Rope (which would require suppression ≤ 0.40 and ignore the extraction asymmetry) or pure Snare (which would require χ ≥ 0.66 and ignore the coordination benefit of decoupling). The perspectives resolve the mandate's ambiguity: whether this is 'good' (Rope: enables freedom) or 'exploitative' (Snare: traps in obligation) depends entirely on observer position. The child's snare is genuine; the legal industry's rope is genuine; the affluent arbitrageur's mobile tangled rope is genuine. No single type is 'correct' — the perspective-indexed classification IS the answer, revealing that the constraint's nature is observer-dependent and that the highest-extractiveness experience (the child's snare perspective) is institutionally suppressed (theater ratio 0.62 reflects that child costs are narratively managed as 'normal family structure' rather than extracted burden).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    child_outcome_attribution,
    'Do measured child developmental outcomes diverge due to modularized parenting structure itself, or due to uncontrolled confounds (parental conflict, economic instability, selection effects)?',
    'Longitudinal studies controlling for pre-separation parental conflict, socioeconomic status, parental mental health, and parenting quality; matched-pair analysis of children in high-conflict intact vs cooperative platonic co-parenting arrangements',
    'If outcomes depend on parenting quality rather than structure: modularization is neutral coordination mechanism (Rope from many perspectives). If structure itself degrades outcomes: extraction is genuine and the tangled rope classification understates the snare component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(child_outcome_attribution, empirical, 'Whether child outcomes depend on parental romantic structure or parenting quality').

omega_variable(
    enforcement_asymmetry_persistence,
    'Will custody law and child support enforcement mechanisms remain asymmetrically enforceable against primary caregivers, or will they genuinely decouple from romantic partnership as the norm shifts?',
    'Analysis of custody case outcomes, child support calculation methods, and enforcement disparities across income levels and gender; comparison of enforcement rigor in platonic vs traditionally-partnered households',
    'If enforcement asymmetry persists: suppression (0.48) understates the extraction mechanism, and the constraint is closer to snare (0.60+). If enforcement genuinely decouples: modularization becomes pure coordination (Rope) with sunset as legal reform completes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_asymmetry_persistence, empirical, 'Whether child support enforcement remains asymmetric across family structures').

omega_variable(
    stigma_transmission_mechanism,
    'Does the (0.62) theater ratio reflect genuine norm transition or institutional conservatism locking in anti-modularization stigma despite shifting surface acceptance?',
    'Analysis of implicit bias, hiring discrimination, school treatment, and peer stigma toward children in platonic co-parenting arrangements; longitudinal tracking of stated vs behavioral acceptance across cohorts',
    'If stigma is genuinely declining: scaffold sunset is real and extractiveness will decline toward 0.15-0.25 over 15-20 years. If stigma is locked in by institutional structure: theater ratio will persist at 0.60+ and extracted value will remain hidden behind performative language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_transmission_mechanism, empirical, 'Whether anti-modularization stigma is genuine norm shift or institutional theater').

omega_variable(
    economic_access_threshold,
    'At what household income level does the powerful/mobile perspective (arbitrage exit) become available, and does this create a dual-constraint structure (high-income households in Rope, low-income in Snare)?',
    'Stratified analysis of co-parenting stability, enforcement disparities, and child outcome divergence across income quintiles; measurement of purchasing-power thresholds for childcare independence',
    'If threshold exists: the constraint actually bifurcates into two distinct stories (modular decoupling as Rope for affluent, as Snare for precarious households). Single story understates class-specific extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_access_threshold, empirical, 'Income threshold at which modularization shifts from constraint to coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platonic_coparenting_decoupling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plat_tr_t0, platonic_coparenting_decoupling, theater_ratio, 0, 0.48).
narrative_ontology:measurement(plat_tr_t10, platonic_coparenting_decoupling, theater_ratio, 10, 0.56).
narrative_ontology:measurement(plat_tr_t20, platonic_coparenting_decoupling, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(plat_be_t0, platonic_coparenting_decoupling, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(plat_be_t10, platonic_coparenting_decoupling, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(plat_be_t20, platonic_coparenting_decoupling, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platonic_coparenting_decoupling, resource_allocation).
narrative_ontology:affects_constraint(platonic_coparenting_decoupling, child_custody_asymmetry).
narrative_ontology:affects_constraint(platonic_coparenting_decoupling, romantic_partnership_exit_frictions).

% DUAL FORMULATION NOTE:
% The modularization constraint is downstream of specific custody law and child support systems (which have their own extractiveness values reflecting legal enforcement severity) but represents a distinct structural constraint about how parental and romantic bonds are institutionally organized. Upstream constraints on custody law affect how the modularization manifests; this constraint affects downstream family stability and relationship formation norms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platonic_coparenting_decoupling, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
