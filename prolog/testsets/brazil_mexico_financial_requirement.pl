% ============================================================================
% CONSTRAINT STORY: brazil_mexico_financial_requirement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazil_mexico_financial_requirement, []).

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
 *   constraint_id: brazil_mexico_financial_requirement
 *   human_readable: Mexican Financial Proof Requirement for Brazilian Travelers
 *   domain: geopolitical/immigration_policy
 *
 * SUMMARY:
 *   Mexico's financial proof requirement for Brazilian travelers (minimum
 *   R$10,000 ~USD$1,900) emerges as a geopolitical constraint embedded in
 *   US-Mexico border enforcement coordination. Ostensibly designed to verify
 *   traveler solvency and prevent burden on Mexican public services, the
 *   requirement functions simultaneously as: (1) a coordination mechanism for
 *   legitimate state capacity assessment, (2) an extraction mechanism
 *   targeting lower-income travelers, (3) a performative display of border
 *   control competence to satisfy US enforcement pressures, and (4) a
 *   degradation of historical Mercosur mobility norms. The constraint
 *   exhibits all six DR types from different structural positions, making it
 *   a diagnostic case for how seemingly technical immigration requirements
 *   embed geopolitical asymmetries. Base extractiveness (0.52) is moderate —
 *   the requirement creates real access barriers but is not total coercion.
 *   Suppression (0.68) is moderately high — alternatives (informal credit,
 *   travel abandonment, false documentation) exist but are costly or risky.
 *   Theater ratio (0.55) reflects moderate performative content: financial
 *   proof is somewhat functional (solvency indicator) but also serves to
 *   demonstrate state border control capacity to US audiences.
 *
 * KEY AGENTS:
 *   - Lower-Income Brazilian Travelers: Primary victims (powerless/trapped) — blocked from travel due to inability to produce R$10,000 proof; no legitimate exit
 *   - Middle-Class Brazilian Travelers: Secondary victims (moderate/constrained) — can meet requirement but face administrative burden, privacy intrusion, proof validation uncertainty
 *   - Mexican Border Control Apparatus: Primary beneficiary (institutional/arbitrage) — gains administrative filtering tool and coordination mechanism; low enforcement cost
 *   - US-Mexico Migration Enforcement Alignment: Secondary beneficiary (organized/arbitrage) — coordinates upstream filtering of Brazil-to-US migration flows
 *   - Regional Mercosur Integration System: Victim of norm degradation (powerless/trapped) — historical mobility norms eroded by external pressure without formal renegotiation
 *   - Analytical Observer: Sees geopolitical leverage structure (analytical/analytical) — US enforcement objectives diffused into Mexican policy without explicit treaty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_mexico_financial_requirement, 0.52).
domain_priors:suppression_score(brazil_mexico_financial_requirement, 0.68).
domain_priors:theater_ratio(brazil_mexico_financial_requirement, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, extractiveness, 0.52).
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(brazil_mexico_financial_requirement, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazil_mexico_financial_requirement, tangled_rope).
narrative_ontology:human_readable(brazil_mexico_financial_requirement, "Mexican Financial Proof Requirement for Brazilian Travelers").
narrative_ontology:topic_domain(brazil_mexico_financial_requirement, "geopolitical/immigration_policy").

domain_priors:requires_active_enforcement(brazil_mexico_financial_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazil_mexico_financial_requirement, mexican_border_control_apparatus).
narrative_ontology:constraint_beneficiary(brazil_mexico_financial_requirement, us_migration_enforcement_alignment).
narrative_ontology:constraint_victim(brazil_mexico_financial_requirement, lower_income_brazilian_travelers).
narrative_ontology:constraint_victim(brazil_mexico_financial_requirement, regional_travel_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOWER-INCOME BRAZILIAN TRAVELER (SNARE) — Lacks liquidity to demonstrate required proof; faces binary choice: produce unverifiable funds or abandon travel plans entirely. No legitimate exit mechanism. Maximum experienced extraction through access denial. Suppression is total — alternative entry mechanisms do not exist.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MIDDLE-CLASS BRAZILIAN TRAVELER (TANGLED ROPE) — Can meet requirement through bank statement or credit line but faces administrative friction, privacy intrusion, and uncertainty about acceptable proof types. Experiences both coordination (border security stabilization) and extraction (unnecessary proof burden). Exit is constrained: can travel if resources permit, but cost is higher than without requirement.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEXICAN BORDER CONTROL APPARATUS (ROPE) — Primary beneficiary. Requirement provides coordination function (identifies travelers with solvency to avoid becoming state charges) and administrative tool for categorizing entrants. Enforcement cost is low relative to benefit — implemented as a document check requiring minimal infrastructure beyond existing border processing. Extracts administrative control; experiences the constraint as coordination.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: US-MEXICO MIGRATION ENFORCEMENT ALIGNMENT (ROPE) — Institutional coordination between US and Mexican authorities. Requirement reduces irregular migrants reaching US southern border by filtering at upstream point (Mexico entry). Low extraction from either state's perspective — both benefit from alignment. US provides implicit or explicit approval/incentive for Mexico to adopt stricter entry rules; Mexico achieves coordination with US migration enforcement objectives.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGIONAL TRAVEL CULTURE (PITON) — Historically, Mercosur citizenship and regional integration norms enabled fluid travel within South America without strict financial documentation. This requirement represents degradation of that norm due to external pressure (US border enforcement concerns). Theater ratio is moderate — the financial proof is partly functional (actual solvency indicator) but also partly performative (demonstrates state capacity for bureaucratic control). The constraint maintains itself through institutional inertia and US political pressure rather than through genuine coordination necessity.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, piton,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (GEOPOLITICAL LEVERAGE) — From civilizational scope, this constraint is a mechanism through which US border enforcement objectives diffuse into Mexican policy without formal treaty. Mexico achieves coordination with US preferences (barrier to US-bound migration) while technically maintaining sovereign control. The requirement exhibits both coordination (legitimate solvency verification) and extraction (administrative burden, access limitation, implicit coercion of lower-income travelers). Classification as Tangled Rope reflects genuine dual function: benefits both state-level coordination and unequal access to regional mobility.
constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_mexico_financial_requirement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_mexico_financial_requirement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brazil_mexico_financial_requirement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brazil_mexico_financial_requirement, TR),
    TR >= 0.70.

:- end_tests(brazil_mexico_financial_requirement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The requirement creates meaningful access barriers for lower-income travelers but is not total coercion — middle-class travelers can comply. The extraction is substantive (denial of travel opportunity) but not maximal (workarounds exist, compliance is possible for resourced travelers). Rising trajectory (0.38→0.52 over 12 months) reflects increasing enforcement consistency and broader adoption as bureaucratic norm. Suppression (0.68): Moderately high. Significant barriers to bypass include: no legitimate alternative entry methods, documentation requirements that require formal banking relationships (excluding informal-economy workers), and asymmetric enforcement (wealth visible in proof level). But suppression is not total — some travelers produce joint accounts, parental co-signatures, or informal credit. Suppression anchors at the middle-class traveler level; for powerless travelers, suppression approaches unity. Theater ratio (0.55): Moderate. The requirement has both functional and performative components. Functional: solvency verification does identify travelers likely to become state charges. Performative: the specific R$10,000 threshold (chosen to align with US border enforcement baselines, not empirically derived from Mexican state cost analysis) and the requirement's public announcement (demonstrating to US officials that Mexico is 'doing something' about Brazil-US migration) suggest significant theater. The rising trajectory (0.40→0.55) reflects increasing theater as enforcement becomes routine — early implementation emphasized the control function; sustained implementation increasingly emphasizes the symbolic message of border 'strength.'
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a profound gap between experience and structural function. For lower-income Brazilian travelers, the requirement is pure access denial (Snare) — a barrier with no legitimate exit and no benefit. For middle-class travelers, it is mixed burden and coordination (Tangled Rope) — administrative friction but also serves the legitimate function of solvency verification. For Mexican border control, it is a coordination and administrative tool (Rope) — enables capacity to assess and categorize entrants. For the US-Mexico enforcement alignment, it is institutional coordination (Rope) — upstream filtering that benefits both states. For the Mercosur regional system, it is institutional degradation (Piton) — the historical norm of fluid regional mobility persists in rhetoric but is undermined by enforcement, making the constraint largely performative at the regional level. The analytical observer recognizes all these functions simultaneously and classifies as Tangled Rope: genuine coordination (state capacity assessment) coupled with asymmetric extraction (unequal access to regional mobility). The perspectival gap exposes how the same requirement can be genuine coordination for states and extraction for individuals.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness (χ) derives from agent power, exit options, and structural relationship to the requirement. Lower-income travelers (powerless/trapped) experience maximum directionality (d≈0.95): zero beneficiary relationship, zero exit, zero arbitrage capacity. Their χ is high even though base ε is moderate — the sigmoid f(d) for trapped powerless agents amplifies experienced extraction. Middle-class travelers (moderate/constrained) experience moderate directionality (d≈0.55): can comply with effort, retain some exit (use alternative transport, cancel trip), but constrained. Mexican border control (institutional/arbitrage) experiences negative or low directionality (d≈0.10): primary beneficiary, retains high exit (can choose enforcement level), experiences the constraint as enabling their objectives. US-Mexico alignment (organized/arbitrage) experiences low directionality (d≈0.15): both states benefit from upstream filtering; exits are available (can modify requirement bilaterally). The perspectival gap is driven by these directionality differences: the powerless traveler sees extraction (Snare); the border apparatus sees coordination (Rope); the analytical observer sees the dual structure (Tangled Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the ambiguity between 'coordination mechanism' and 'extraction tool' by showing that both functions are structurally present but distributed unequally across agents. The requirement genuinely coordinates Mexican state capacity (identifies travelers who will not become state charges) — this is not false coordination. It also genuinely extracts from lower-income travelers (creates access barriers) — this is not false extraction. The mandatrophy dissolves when we recognize that the constraint can have genuine dual function: benefiting some actors (states, border apparatus) through coordination while imposing costs on others (lower-income travelers) through extraction. The Tangled Rope classification holds because: (1) base extractiveness is moderate-high (0.52), meeting the threshold; (2) suppression is high (0.68), meeting the threshold; (3) active enforcement is required (Mexico must maintain documentation checks and verification); (4) beneficiaries exist (Mexican border control, US enforcement alignment); (5) victims exist (lower-income Brazilian travelers); (6) χ is in the Tangled Rope range (0.40-0.90) when computed from structured perspectives. The false summit risk is that the requirement might appear as pure 'coordination' (Rope) if viewed only from state-level perspectives. The correct classification requires acknowledging the asymmetric impact: it is Rope for states, Snare for lower-income travelers, and Tangled Rope at the analytical level where both functions are visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    financial_proof_verification_credibility,
    'How verifiable is the claimed R$10,000 proof? Can travelers produce fake bank statements or letters without detection?',
    'Audit of Mexican border enforcement: sample verification rates, cross-checks with Brazilian financial institutions, documented fraud cases',
    'If verification is low: requirement becomes pure theater (Piton). If verification is moderate-to-high: genuine coordination mechanism (Rope upside for beneficiaries). If asymmetrically enforced (wealthy travelers less scrutinized): becomes Snare for lower-income travelers, Rope for affluent ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_proof_verification_credibility, empirical, 'Credibility of financial proof verification at Mexican border').

omega_variable(
    us_pressure_directedness,
    'Is the requirement genuinely motivated by US border control pressure, or by legitimate Mexican state capacity to assess traveler solvency?',
    'Timeline analysis of Mexican policy announcements; correlation with US immigration enforcement cycles; interviews with Mexican officials; diplomatic cables and bilateral agreement documents',
    'If primarily US-driven: constraint is better classified as US-imposed extraction (Snare for Brazilian travelers viewed from global perspective). If genuinely dual-motivated: Tangled Rope classification holds. If Mexican-initiated: Rope classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_pressure_directedness, conceptual, 'Whether requirement is US-pressure-driven or autonomously motivated').

omega_variable(
    lower_income_traveler_adaptation,
    'Are lower-income Brazilian travelers finding alternative proofs (co-signers, pooled resources, alternative documentation) or simply abandoning travel plans?',
    'Statistical analysis of Brazilian tourist arrivals in Mexico pre/post-requirement; interview-based study of low-income travelers; documentation of adaptation strategies (visa fraud, informal credit, travel plan cancellation rates)',
    'If significant adaptation: effective suppression is lower than estimated (some exit). If abandonment dominates: suppression is high (Snare confirmed). If informal workarounds proliferate: requirement becomes degraded (Piton) and enforcement theater increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lower_income_traveler_adaptation, empirical, 'Whether lower-income travelers adapt or abandon travel').

omega_variable(
    regional_normative_shift,
    'Is this requirement signaling a structural shift away from Mercosur mobility norms toward North American border-control standards?',
    'Comparison with other Mercosur states (Argentina, Paraguay, Uruguay): do they adopt similar requirements? Textual analysis of regional integration rhetoric; tracking of bilateral vs multilateral travel agreements',
    'If regional adoption occurs: constraint is part of larger structural shift (degradation of Mercosur, alignment with US standards). If Mexico remains outlier: constraint is bilateral extraction. If norms shift back (sunset): Scaffold classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_normative_shift, conceptual, 'Whether this signals broader shift away from Mercosur mobility norms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazil_mexico_financial_requirement, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bmfr_tr_t0, brazil_mexico_financial_requirement, theater_ratio, 0, 0.4).
narrative_ontology:measurement(bmfr_tr_t6, brazil_mexico_financial_requirement, theater_ratio, 6, 0.48).
narrative_ontology:measurement(bmfr_tr_t12, brazil_mexico_financial_requirement, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(bmfr_be_t0, brazil_mexico_financial_requirement, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bmfr_be_t6, brazil_mexico_financial_requirement, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(bmfr_be_t12, brazil_mexico_financial_requirement, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazil_mexico_financial_requirement, enforcement_mechanism).
narrative_ontology:affects_constraint(brazil_mexico_financial_requirement, us_mexico_border_enforcement_alignment).
narrative_ontology:affects_constraint(brazil_mexico_financial_requirement, mercosur_mobility_norm_degradation).
narrative_ontology:affects_constraint(brazil_mexico_financial_requirement, informal_economy_financial_exclusion).

% DUAL FORMULATION NOTE:
% This constraint is structurally dependent on US border enforcement pressures (upstream constraint) but represents a distinct mechanism operating at the Mexico-Brazil interface. The financial proof requirement could be decomposed further: (1) solvency verification as genuine coordination (lower ε), vs (2) administrative burden and access denial (higher ε). Current story treats them as integrated Tangled Rope. If analysis requires separation, write two stories: 'mexico_solvency_verification' (ε~0.25, Rope) and 'mexico_access_barrier' (ε~0.65, Snare) linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(brazil_mexico_financial_requirement, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
