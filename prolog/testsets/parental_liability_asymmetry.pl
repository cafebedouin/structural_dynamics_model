% ============================================================================
% CONSTRAINT STORY: parental_liability_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parental_liability_asymmetry, []).

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
 *   constraint_id: parental_liability_asymmetry
 *   human_readable: Parental Liability Asymmetry
 *   domain: family_law/institutional_responsibility
 *
 * SUMMARY:
 *   Parental liability asymmetry describes the legal and institutional
 *   arrangement where parents bear unlimited personal and criminal liability
 *   for child safety, welfare, and conduct outcomes, while the state
 *   maintains capacity to prosecute parental 'neglect' without providing
 *   proportional material support for compliance or resources to prevent
 *   failure. This constraint exhibits a genuine but highly asymmetric
 *   coordination function: children do require protection, and parental
 *   incentive for vigilance serves that function. However, the allocation of
 *   responsibility is structured to minimize state spending and liability
 *   exposure while maximizing parental culpability — particularly for
 *   economically vulnerable families who lack the material and informational
 *   resources to meet middle-class adequacy standards that are encoded in
 *   law. The constraint has intensified over the past two decades
 *   (extractiveness rising from 0.42 to 0.58) as mandatory reporting laws
 *   have proliferated, criminal standards for parental conduct have expanded,
 *   and family court proceedings have become more adversarial, while public
 *   investment in family support systems has stagnated. Theater ratio (0.55)
 *   reflects that much enforcement is performative: high-profile cases
 *   receive intensive prosecution while systemic failures (school systems
 *   failing to detect abuse, welfare systems failing to intervene despite
 *   known risk) are treated as separate issues rather than as state liability
 *   failures.
 *
 * KEY AGENTS:
 *   - Economically Vulnerable Parents: Primary victims (powerless/trapped) — bear unlimited liability for outcomes beyond their material control; face criminal prosecution even when state systems fail to intervene
 *   - Working-Class Parents Managing Risk: Secondary victims (moderate/constrained) — navigate liability asymmetry through resource optimization and documentation; some agency but high cost of failure
 *   - Child Welfare State Apparatus: Primary beneficiary (institutional/arbitrage) — externalizes responsibility to parents; obtains compliance without maintaining universal monitoring or support systems
 *   - Child Protective Services: Secondary institutional actor (institutional/constrained) — implements liability regime while itself under-resourced; bears genuine coordination function (protecting children) alongside extraction (deflecting state liability)
 *   - Family Support and Advocacy Organizations: Organized reformers (organized/mobile) — building alternative models based on shared responsibility and structural support; mobilizing for sunset of pure parental liability regime
 *   - Patriarchal Family Doctrine: Institutional principle (institutional/arbitrage) — the underlying legal principle persists through ideological attachment and institutional inertia despite selective, class-stratified enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parental_liability_asymmetry, 0.58).
domain_priors:suppression_score(parental_liability_asymmetry, 0.62).
domain_priors:theater_ratio(parental_liability_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parental_liability_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(parental_liability_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(parental_liability_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parental_liability_asymmetry, tangled_rope).
narrative_ontology:human_readable(parental_liability_asymmetry, "Parental Liability Asymmetry").
narrative_ontology:topic_domain(parental_liability_asymmetry, "family_law/institutional_responsibility").

domain_priors:requires_active_enforcement(parental_liability_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parental_liability_asymmetry, institutional_child_welfare_apparatus).
narrative_ontology:constraint_beneficiary(parental_liability_asymmetry, state_liability_avoidance).
narrative_ontology:constraint_victim(parental_liability_asymmetry, economically_vulnerable_parents).
narrative_ontology:constraint_victim(parental_liability_asymmetry, child_developmental_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY VULNERABLE PARENT (SNARE) — Bears unlimited personal liability for child neglect/abuse outcomes even when state welfare systems fail to intervene despite known risk. Cannot exit parental role; faces criminal prosecution, asset seizure, incarceration. Suppression is structural: legal definitions of adequate care are indexed to middle-class material capacity; poverty itself becomes culpable negligence. Maximum extraction with minimal alternatives.
constraint_indexing:constraint_classification(parental_liability_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-CLASS PARENT (TANGLED ROPE) — Genuine coordination function exists: parents do need incentive to maintain child safety. But the liability assignment is asymmetric — parents bear full legal risk even when structural constraints (poverty, lack of childcare, underfunded schools) prevent meeting legal standards. High cost to exit (child separation, criminal record) but some agency through resource optimization and documentation. Mixed coordination and extraction.
constraint_indexing:constraint_classification(parental_liability_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHILD WELFARE STATE APPARATUS (ROPE) — Experiences the constraint as enabling coordination: parental liability allocates responsibility for child safety at the lowest enforcement cost. The state obtains compliance without maintaining universal monitoring, intervention, or support systems. Arbitrage exit: the state can reallocate liability burden upward (to parents) or downward (to communities/institutions) as budget cycles demand. Net beneficiary.
constraint_indexing:constraint_classification(parental_liability_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CHILD PROTECTIVE SERVICES (TANGLED ROPE) — Under-resourced institutional actor bearing genuine coordination function (preventing child abuse/neglect) alongside extraction (deflecting liability to parents). CPS must investigate parental conduct while itself operating under budget constraints that prevent adequate intervention or support. Constrained by caseload/funding limits; cannot exit responsibility. Experiences the constraint as both enabling and extractive.
constraint_indexing:constraint_classification(parental_liability_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FAMILY SUPPORT ORGANIZATIONS (SCAFFOLD) — Organized agents (legal aid, family service nonprofits, community child centers) see parental liability asymmetry as a temporary policy problem solvable through systemic reform: shifting from individual liability to shared responsibility models, expanding public childcare, redefining adequate care standards to account for structural poverty. Mobile exit: these organizations can build alternative service pathways. The sunset logic is real — some jurisdictions have shifted to family preservation models reducing liability extraction.
constraint_indexing:constraint_classification(parental_liability_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: PATRIARCHAL FAMILY DOCTRINE (PITON) — The underlying legal principle (parents are responsible for children's welfare/conduct) has largely atrophied into theater. Modern enforcement is selective, targeting economically vulnerable families while wealthy parents with access to expert childcare, private schools, and legal defense obtain de facto immunity. The principle persists through institutional inertia and ideological attachment to family autonomy rather than through functional enforcement of universal standards. Theater ratio indicates degradation.
constraint_indexing:constraint_classification(parental_liability_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the constraint reflects a structural tension between child protection (genuine coordination need) and cost externalization (extraction mechanism). The asymmetry is neither natural law nor pure coordination — it is a contingent institutional choice to allocate safety responsibility to parents while maintaining state capacity to criminalize parental conduct without providing material support for compliance. This is a tangled rope: genuine coordination function (children need protection), asymmetric extraction (burden on vulnerable agents), active state enforcement through liability law.
constraint_indexing:constraint_classification(parental_liability_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parental_liability_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parental_liability_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parental_liability_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parental_liability_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parental_liability_asymmetry, TR),
    TR >= 0.70.

:- end_tests(parental_liability_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The state captures significant benefit by externalizing child safety responsibility to parents while maintaining state authority to criminalize parental conduct. However, extraction is not total (0.66+) because genuine coordination function exists — parental vigilance does reduce child harm. The extraction reflects the *asymmetry* in burden allocation, not absence of coordination need. Suppression (0.62): Moderate-high. Multiple suppression mechanisms operate: (1) legal threat of prosecution, asset seizure, family separation; (2) definitional capture (poverty-indexed adequacy standards encode class assumptions); (3) selective enforcement (wealthy families obtain de facto immunity through expert childcare/legal defense); (4) informational asymmetry (parents often unaware of specific legal standards until violation occurs). Theater ratio (0.55): Moderate. Enforcement is selective and performative — high-profile cases receive intensive attention while systemic failures (school detection, welfare coordination) are treated as separate systems. The theater is lower than for piton-class constraints because genuine parental liability does occur; but it is significant because enforcement tracks class/visibility rather than actual child danger. Extractiveness rising over 20-year interval reflects proliferation of mandatory reporting, expansion of criminal standards, and intensified family court adversarialism without corresponding increase in support systems.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the vulnerable parent (Snare at d≈0.92) and the state apparatus (Rope at d≈0.08). The parent experiences unlimited liability and no exit; the state experiences cost-minimization and flexibility. At intermediate d values, intermediate classifications appear: working-class parents see Tangled Rope (mixed burden and benefit), CPS sees Tangled Rope (implementing extraction while under-resourced). Organized agents (family support orgs) see Scaffold because they have agency to build alternative pathways. The patriarchal doctrine classification as Piton indicates that the underlying principle has atrophied into theater — modern enforcement is selective and class-stratified, not universal, suggesting the functional coordination purpose (universal parental responsibility) has been displaced by extraction purpose (state liability avoidance for wealthy families while targeting vulnerable families).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness is computed from their d value via the sigmoid f(d). Vulnerable parents with d≈0.92 experience f(d)≈1.32 — their experienced extraction is amplified by their powerless position and trapped exit status, magnifying the base extractiveness. Institutional beneficiaries with d≈0.08 experience f(d)≈-0.10 — the constraint appears as negative extraction (net benefit/coordination) from their position. The analytical observer's d≈0.70 produces f(d)≈1.05 — moderate amplification. The spatial scope (national) applies a scope modifier σ(S)=1.0 (no amplification or dampening at national scope). Effective extraction chi = ε × f(d) × σ(S) ranges from ≈ 0.58 × (-0.10) × 1.0 ≈ -0.06 (beneficiary view) to ≈ 0.58 × 1.32 × 1.0 ≈ 0.77 (victim view). The gap between these experienced values is the perspectival gap that makes the constraint visible as extraction from victim position, coordination from beneficiary position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that parental liability is neither pure coordination (protection of children is genuinely needed) nor pure extraction (the liability mechanism does serve a protective function), but a tangled rope where coordination function has been preserved while extraction has intensified. The constraint is NOT a false positive for Tangled Rope — it genuinely exhibits both coordination (child safety incentives) and asymmetric extraction (burden on vulnerable agents, state liability avoidance). The rising extractiveness over 20 years indicates that enforcement has intensified without corresponding state investment in support systems, gradually shifting the constraint toward pure snare (extraction) from some perspectives (vulnerable parents). The theater ratio is moderate (0.55) because enforcement is selective rather than performative throughout — some parents do face real consequences, but the class stratification in enforcement indicates the coordination function is being displaced by extraction in practice. Mandatrophy is resolved by recognizing that the constraint is legitimately Tangled Rope at the analytical level (it exhibits both functions) while acknowledging that from the perspective of economically vulnerable parents, the extraction function dominates and the constraint appears as Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_poverty_vs_parental_negligence,
    'What portion of prosecuted parental ''neglect'' reflects genuine inability to provide adequate care (structural poverty) versus genuine indifference or incapacity to parent?',
    'Longitudinal data: follow families prosecuted for neglect post-prosecution; measure outcomes when material support is provided (housing, food security, childcare) vs. incarceration. Correlation between neglect charges and introduction of material support programs.',
    'If structural poverty is dominant cause: parental liability is mislabeled extraction (should shift to Snare). If genuine parental incapacity is dominant: classification as Tangled Rope is correct but extraction ratio ε is too high. If mixed: decompose into two distinct constraints (structural poverty barrier + genuine incapacity/indifference).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_poverty_vs_parental_negligence, empirical, 'Proportion of neglect attributable to structural poverty vs. parental incapacity').

omega_variable(
    state_liability_counterfactual,
    'If the state bore direct liability for child safety outcomes (rather than parents), would child welfare improve, degrade, or remain unchanged?',
    'Comparative jurisdictional analysis: jurisdictions with state-primary liability (e.g., some EU models emphasizing state support over parental prosecution) vs. parental-primary liability models. Measure child safety outcomes, cost of intervention, family preservation rates, and parent-state conflict.',
    'If state-primary improves outcomes: parental liability is extractive overhead, not coordination benefit — downgrade to Snare. If outcomes degrade: coordination function is real but asymmetry is necessary. If unchanged: the liability allocation is orthogonal to actual child safety (indicates piton degradation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_liability_counterfactual, empirical, 'Whether state-primary liability would improve child welfare outcomes').

omega_variable(
    identity_fusion_in_parental_liability,
    'To what extent is parental liability accepted because parents have internalized the narrative that bearing unlimited responsibility for child outcomes is constitutive of parenthood itself?',
    'Qualitative interviews and narrative analysis: do parents describe liability as external punishment or as intrinsic to their identity/role? Analysis of parental discourse before/after exposure to alternative models (shared responsibility, state support frameworks). Measurement of identity-based resistance to liability reform.',
    'If identity-locked: the constraint persists through cognitive capture even when material barriers are removed — the binding mechanism is internal (identity fusion), not external (legal/economic). This would elevate the classification from Snare/Tangled Rope to indicate a stronger hold. If not identity-locked: parents perceive liability as external punishment; reform faces only structural/political barriers, not psychological resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_parental_liability, empirical, 'Whether parental acceptance of liability reflects identity fusion with the parental role').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) primarily structural (legal barriers to exit, asset seizure, incarceration threat) or internalized (parents have accepted liability as deserved, moral obligation)?',
    'Post-liability-reform data: when legal suppression mechanisms are reduced (decriminalization, liability caps, statutory defenses for structural poverty), do parents seek exit or maintain engagement? Measure compliance persistence after legal threat is removed.',
    'If structural: suppression should decrease when legal mechanisms are reformed. If internalized: suppression persists after legal reform because parents carry the obligation internally — the constraint is more entrenched than structural measures suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in parental liability acceptance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parental_liability_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pla_tr_t0, parental_liability_asymmetry, theater_ratio, 0, 0.4).
narrative_ontology:measurement(pla_tr_t10, parental_liability_asymmetry, theater_ratio, 10, 0.48).
narrative_ontology:measurement(pla_tr_t20, parental_liability_asymmetry, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(pla_be_t0, parental_liability_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pla_be_t10, parental_liability_asymmetry, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(pla_be_t20, parental_liability_asymmetry, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parental_liability_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(parental_liability_asymmetry, mandatory_reporting_escalation).
narrative_ontology:affects_constraint(parental_liability_asymmetry, child_welfare_system_capacity).
narrative_ontology:affects_constraint(parental_liability_asymmetry, family_court_adversarialism).

% DUAL FORMULATION NOTE:
% Parental liability asymmetry is upstream of specific enforcement mechanisms (mandatory reporting, CPS intervention thresholds, family court standards). The separate constraint stories on mandatory reporting escalation and family court adversarialism represent downstream intensifications of the base liability asymmetry. This story captures the foundational institutional arrangement; downstream stories model how enforcement mechanisms have changed over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parental_liability_asymmetry, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
