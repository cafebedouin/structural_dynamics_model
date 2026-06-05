% ============================================================================
% CONSTRAINT STORY: cross_class_coalition_formation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cross_class_coalition_formation, []).

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
 *   constraint_id: cross_class_coalition_formation
 *   human_readable: Cross-Class Coalition Formation Constraint
 *   domain: political_economy/collective_action
 *
 * SUMMARY:
 *   Cross-class coalition formation represents a structural constraint on
 *   collective political action in which workers, professionals, and
 *   marginalized communities attempt to coordinate against concentrated
 *   capital. The constraint exhibits the classic hallmark of tangled_rope:
 *   genuine coordination function (unified political voice, mutual defense
 *   against capital accumulation) coupled with asymmetric extraction
 *   (organized working class and professional middle class benefit
 *   disproportionately; precariat workers and marginalized communities bear
 *   costs without equivalent access). The constraint's theater ratio (0.58)
 *   reflects increasing performative content (rallies, media campaigns,
 *   electoral theater) as the material coordination capacity has declined
 *   (strike rates, union density, membership participation). Suppression
 *   (0.65) operates through multiple mechanisms: isolation of precariat
 *   workers from formal membership structures, credential gatekeeping by
 *   professional class, and organizational discipline enforced through
 *   selective benefit distribution. The extractiveness trajectory (0.35 →
 *   0.58 over 50-year interval) shows accumulation of rent-seeking behaviors
 *   within the coalition structure itself — maintenance costs have become
 *   extraction.
 *
 * KEY AGENTS:
 *   - Organized Working Class: Primary beneficiary (organized/constrained) — captures union scale, workplace protections, political voice. Bears costs of professional-class dominance within coalition.
 *   - Professional Middle Class: Primary beneficiary (institutional/arbitrage) — captures regulatory access, counter-lobbying power, credential leverage. Can arbitrage between coalitions.
 *   - Precariat Workers: Primary victim (powerless/trapped) — structurally excluded from coalition infrastructure by gig economy conditions and isolation. Bears costs without access to protections.
 *   - Marginalized Communities: Secondary victim (powerless/trapped) — excluded through formal criteria and informal gatekeeping. Coalition interests often conflict with direct material interests of marginalized communities.
 *   - Precariat Aspirants: Tertiary victim (moderate/identity_locked) — identity-locked to working-class coalition by family/cultural heritage but structurally unable to access benefits. Trapped by cognitive frame rather than material barriers alone.
 *   - Coalition Apparatus: Institutional actor (institutional/arbitrage) — unions, professional associations, political machines that maintain coalition coordination. Experiences constraint as increasingly performative as material leverage declines.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cross_class_coalition_formation, 0.58).
domain_priors:suppression_score(cross_class_coalition_formation, 0.65).
domain_priors:theater_ratio(cross_class_coalition_formation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cross_class_coalition_formation, extractiveness, 0.58).
narrative_ontology:constraint_metric(cross_class_coalition_formation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cross_class_coalition_formation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cross_class_coalition_formation, tangled_rope).
narrative_ontology:human_readable(cross_class_coalition_formation, "Cross-Class Coalition Formation Constraint").
narrative_ontology:topic_domain(cross_class_coalition_formation, "political_economy/collective_action").

domain_priors:requires_active_enforcement(cross_class_coalition_formation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cross_class_coalition_formation, organized_working_class).
narrative_ontology:constraint_beneficiary(cross_class_coalition_formation, professional_middle_class).
narrative_ontology:constraint_victim(cross_class_coalition_formation, precariat_workers).
narrative_ontology:constraint_victim(cross_class_coalition_formation, marginalized_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIAT WORKER (SNARE) — Structurally excluded from coalition benefits. Trapped by gig economy conditions, lack of stable employment, and isolation from organized labor infrastructure. Bears costs of coalition formation (union dues pressure, credential requirements) without access to its protections. Maximum extraction with minimal exit.
constraint_indexing:constraint_classification(cross_class_coalition_formation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED WORKING CLASS (TANGLED ROPE) — Primary beneficiary of coalition structure (union scale, workplace protections) but faces extraction through professional-class gatekeeping within the coalition. Benefits from coordination mechanism while bearing asymmetric costs of maintaining coalition discipline. Constrained by mobility barriers within labor market stratification.
constraint_indexing:constraint_classification(cross_class_coalition_formation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROFESSIONAL MIDDLE CLASS (ROPE) — Net beneficiary (institutional/arbitrage). Experiences coalition as pure coordination mechanism: political voice, regulatory access, counter-lobbying power against capital. Can arbitrage between political coalitions if terms deteriorate. Minimal experienced extraction.
constraint_indexing:constraint_classification(cross_class_coalition_formation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRECARIAT ASPIRANT (TANGLED ROPE) — Structurally mobile (could exit into gig economy or informal labor) but identity-locked to working-class coalition by family history, cultural identity, and ideological commitment. Benefits from coalition protections theoretically but unable to access them due to precariat status. The identity lock prevents exit despite high structural mobility. Classification driven by identity_locked exit option producing rope at biographical horizon, converting to tangled_rope via inherited victim status.
constraint_indexing:constraint_classification(cross_class_coalition_formation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: COALITION APPARATUS (PITON) — Formal structures (unions, professional associations, political machines) that coordinate coalition activity. Theater ratio 0.65 reflects performative aspects: constituency theater (parades, rallies, media campaigns) that maintain coalition visibility despite declining material enforcement capacity. Coalition maintenance increasingly theatrical as material leverage (membership numbers, strike capacity) has declined. Institutional arbitrage options allow apparatus to shift emphasis between coalition partners.
constraint_indexing:constraint_classification(cross_class_coalition_formation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From civilizational/global scope, class coalition formation faces irreducible coordination problems: divergent material interests (workers vs professionals), information asymmetries (about real vs stated preferences), and commitment credibility issues (each class can benefit from defection). These constraints appear immutable across all historical contexts. However, the structural data contradicts mountain classification — the moderate extractiveness, suppression, and theater values reveal this as contingent institutional arrangement rather than natural law. False summit.
constraint_indexing:constraint_classification(cross_class_coalition_formation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cross_class_coalition_formation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cross_class_coalition_formation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cross_class_coalition_formation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cross_class_coalition_formation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cross_class_coalition_formation, TR),
    TR >= 0.70.

:- end_tests(cross_class_coalition_formation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The coalition extracts from precariat and marginalized members through membership expectations, credential requirements, and political discipline while distributing benefits asymmetrically toward organized working class and professionals. The measurement trajectory (0.35→0.58) reflects rent-seeking accumulation as coalition leadership developed mechanisms to extract resources from peripheral members. Not as severe as pure snare (ε≥0.66) because some genuine coordination benefit exists and some mobility options remain. Suppression (0.65): High. Multiple reinforcing mechanisms prevent exit: informal ostracism from working-class communities for coalition defectors, material dependency on union benefits, lack of alternative collective action infrastructure for precariat workers, identity-level integration into working-class identity. Information barriers also suppress: precariat workers may not understand that coalition leadership has shifted toward professional-class interests. Theater ratio (0.58): Moderate-high. Coalition visibility activities (electoral mobilization, public rallies, media campaigns) have become increasingly central to coalition maintenance as material enforcement capacity (ability to strike, mobilize membership) has declined. This suggests piton-trajectory degradation: coalition performing its function theatrically rather than materially.
 *
 * PERSPECTIVAL GAP:
 *   The perspective array shows the full range of experienced extractiveness across the class structure. Precariat workers experience snare: high extraction, trapped exit, no coordination benefit. Organized workers experience tangled_rope: genuine coordination benefit (union protections) coupled with extraction through professional-class dominance. Professionals experience rope: pure coordination, arbitrage options, minimal extraction. The precariat aspirant experiences tangled_rope differently: identity-locked to the coalition but structurally unable to access its benefits, creating a gap between identity and material position. The coalition apparatus experiences piton: sees its own function as increasingly performative. The analytical observer risks mountain (immutable conflict between class interests) but the structural data reveals this as naturalization: the extractiveness trajectory and theater growth show the constraint is contingent, not immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the coalition. Beneficiaries of the coalition structure (organized working class, professional middle class) experience low d-values reflecting their net positive position. Victims of the coalition structure (precariat workers, marginalized communities) experience high d-values reflecting extraction. The precariat aspirant presents a novel case: structurally mobile (could exit into informal economy, gig platforms, or individual precarity) but identity-locked to working-class coalition through family history and ideological commitment. This produces d-value around 0.50 at biographical horizon when identity-locked exit option is applied, which yields rope-type classification — the agent perceives the constraint as theoretically changeable (via identity shift) even though they cannot currently exercise that exit. This differs sharply from trapped precariat workers for whom d-value remains high (0.90+) regardless of identity considerations, yielding snare classification. The gap between identity_locked and trapped perspectives on the same precariat workers reveals the mechanism of suppression: structural isolation is reinforced by internalized identity commitment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through multiple classification types that each capture structural reality. The apparent contradictions (snare + rope + tangled_rope from different perspectives) resolve into a comprehensive picture: cross-class coalition formation genuinely solves collective action problems (rope coordination function) while creating new extraction mechanisms (tangled_rope asymmetry) that trap the most marginalized agents (snare from precariat perspective). The piton perspective (degraded function, performative maintenance) suggests the constraint has drifted from pure rope toward snare across its 50-year lifecycle. The analytical mountain perspective reveals the critical mandatrophy risk: naturalizing class conflict as immutable law ('workers and capital will always conflict') when the actual constraint is the contingent structure of coalition organization itself. Mandatrophy is not resolved — it is clarified: the constraint is NOT an immutable aspect of collective action but a specific institutional arrangement (union-professional partnership) that could be reorganized to reduce extraction or decomposed into separate coalitions. Current analysis leaves mandatrophy_resolved as false because the structural path to resolution is not yet crystallized in practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism_ambiguity,
    'Is the precariat aspirant''s identity lock to working-class coalition genuine cognitive fusion or rationalization of material dependency?',
    'Post-exit trajectory analysis: if precariat workers who exit coalition organizing maintain working-class identity and ideological commitment despite material non-participation, the identity lock is independent of access. If identity dissolves upon exit, the lock was rationalization of material status.',
    'If genuine identity lock: perspective correctly classifies as tangled_rope. If rationalization: should reclassify as snare — the exit option is constrained, not identity_locked. Changes directionality from 0.50 (identity_locked) to 0.85 (constrained victim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_ambiguity, empirical, 'Whether precariat aspirant identity lock is cognitive or material').

omega_variable(
    professional_class_extraction_visibility,
    'Do professional-class coalition members perceive themselves as extracting from organized workers or as equal partners in shared struggle?',
    'Survey/interview data on professional perception of wage ratios, credential gatekeeping, leadership concentration. Compare perceived vs actual distribution of coalition resources and decision-making power.',
    'If professionals perceive extraction: may reframe their perspective from rope to tangled_rope, creating perspectival gap within institutional power class. If professionals deny extraction: suggests identity-locked perspective among institutional actors themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_class_extraction_visibility, empirical, 'Professional class perception of extraction within coalition').

omega_variable(
    coalition_formation_prerequisite_costs,
    'What minimum mobilization costs (organizing infrastructure, communication channels, credential vetting) are inherent to cross-class coalition formation vs contingent to current institutional arrangements?',
    'Historical comparison across coalition formation efforts; analysis of costs that scale with coalition size vs fixed structural costs. Estimation of minimum viable coalition communication infrastructure.',
    'If costs are inherent/unavoidable: extractiveness should be classified as higher (ε > 0.70) and reclassified as snare from precariat perspective. If costs are contingent/reducible: current ε=0.58 is accurate; technological/organizational innovation could reduce extraction significantly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_formation_prerequisite_costs, empirical, 'Inherent vs contingent costs of cross-class coalition formation').

omega_variable(
    marginalized_community_exclusion_mechanism,
    'Is marginalized community exclusion from coalition due to structural incompatibility of interests or to institutional gatekeeping by organized class members?',
    'Analysis of formal coalition admission criteria vs informal veto power. Historical cases of attempted inclusion by marginalized groups with documented response from coalition leadership.',
    'If structural incompatibility: victim status may be inappropriate — different coalition may be more compatible. If institutional gatekeeping: confirms tangled_rope structure with asymmetric enforcement benefiting organized class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_community_exclusion_mechanism, empirical, 'Root cause of marginalized community coalition exclusion').

omega_variable(
    mandatrophy_classification_gap,
    'Is cross-class coalition formation primarily a coordination mechanism (rope) naturalizing into extraction (snare) or a mixed hybrid (tangled_rope) from inception?',
    'Historical analysis of early coalition formation: did material benefits initially distribute relatively evenly (rope origin) or was asymmetry present from founding? Longitudinal comparison of extraction ratios across coalition lifecycle.',
    'If rope→snare trajectory: explains piton perspective (degraded former function) and suggests reversibility. If tangled_rope from origin: suggests structural necessity of asymmetry and lower probability of significant reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_classification_gap, empirical, 'Whether coalition evolved from rope or was always tangled_rope').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cross_class_coalition_formation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccf_tr_t0, cross_class_coalition_formation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ccf_tr_t15, cross_class_coalition_formation, theater_ratio, 15, 0.48).
narrative_ontology:measurement(ccf_tr_t30, cross_class_coalition_formation, theater_ratio, 30, 0.58).
narrative_ontology:measurement(ccf_tr_t45, cross_class_coalition_formation, theater_ratio, 45, 0.65).

% Extraction over time
narrative_ontology:measurement(ccf_be_t0, cross_class_coalition_formation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ccf_be_t15, cross_class_coalition_formation, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ccf_be_t30, cross_class_coalition_formation, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(ccf_be_t45, cross_class_coalition_formation, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cross_class_coalition_formation, resource_allocation).
narrative_ontology:boltzmann_floor_override(cross_class_coalition_formation, 0.18).
narrative_ontology:affects_constraint(cross_class_coalition_formation, union_density_decline).
narrative_ontology:affects_constraint(cross_class_coalition_formation, professional_credentialing_gatekeeping).
narrative_ontology:affects_constraint(cross_class_coalition_formation, precariat_labor_market_segmentation).

% DUAL FORMULATION NOTE:
% Cross-class coalition formation is upstream of specific labor market constraints but represents a distinct structural constraint. Union density decline, professional credentialing gatekeeping, and precariat labor market segmentation are downstream manifestations of coalition structure failures, each with their own ε values. The coalition constraint coordinates (or fails to coordinate) these subsidiary mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cross_class_coalition_formation, organized, 0.35).
constraint_indexing:directionality_override(cross_class_coalition_formation, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
