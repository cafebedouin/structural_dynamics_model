% ============================================================================
% CONSTRAINT STORY: new_technology_adoption_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_new_technology_adoption_coordination, []).

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
 *   constraint_id: new_technology_adoption_coordination
 *   human_readable: New Technology Adoption Coordination
 *   domain: technology_adoption/economic_coordination
 *
 * SUMMARY:
 *   New technology adoption creates a structural coordination problem:
 *   network effects require critical mass participation, compatibility
 *   standards require mutual commitment, early adopters bear disproportionate
 *   risk, and late adopters face pressure to abandon legacy systems. This
 *   constraint exhibits mixed coordination and extraction dynamics. Vendors
 *   and early adopters benefit from momentum and market control. Legacy
 *   workers face mandatory obsolescence with uncertain retraining success.
 *   Small businesses are caught between supply-chain mandates and switching
 *   costs. Government transition programs provide scaffolding with explicit
 *   sunset logic. The constraint's extractiveness (0.35) reflects that
 *   genuine coordination benefits coexist with asymmetric burden distribution
 *   — solving the collective action problem of critical-mass adoption
 *   requires some agents to bear disruption costs. The theater ratio (0.38)
 *   is moderate: adoption has real efficiency gains but also includes
 *   performative compliance theater (vendor lock-in marketing as 'inevitable
 *   progress,' adoption certificates, compliance theater).
 *
 * KEY AGENTS:
 *   - Technology Vendors: Primary beneficiary (institutional/arbitrage) — capture market position, lock-in advantages, and timing control; can exit into adjacent markets
 *   - Legacy Workforce: Primary victim (powerless/trapped) — face mandatory retraining or workforce exit with no alternative paths; trapped in disruption they did not choose
 *   - Small Business Operators: Secondary victim (moderate/constrained) — face supply-chain adoption pressures and switching costs; benefit from efficiency gains but bear disproportionate transition burden
 *   - Early Adopters: Secondary beneficiary (powerful/mobile) — bear initial risk but capture first-mover advantages and influence standard-setting
 *   - Government Transition Programs: Organized third party (organized/constrained) — provide scaffolding through retraining, tax credits, adoption support with sunset as market matures
 *   - Large Incumbent Firms: Secondary actor (powerful/mobile) — navigate adoption timing strategically; can leverage position in legacy systems to control compatibility layers
 *   - Standardization Bodies: Institutional actor (institutional/arbitrage) — benefit from adoption through increased relevance and market influence; set compatibility standards that advantage early movers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(new_technology_adoption_coordination, 0.35).
domain_priors:suppression_score(new_technology_adoption_coordination, 0.42).
domain_priors:theater_ratio(new_technology_adoption_coordination, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(new_technology_adoption_coordination, extractiveness, 0.35).
narrative_ontology:constraint_metric(new_technology_adoption_coordination, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(new_technology_adoption_coordination, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(new_technology_adoption_coordination, tangled_rope).
narrative_ontology:human_readable(new_technology_adoption_coordination, "New Technology Adoption Coordination").
narrative_ontology:topic_domain(new_technology_adoption_coordination, "technology_adoption/economic_coordination").

domain_priors:requires_active_enforcement(new_technology_adoption_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(new_technology_adoption_coordination, technology_vendors).
narrative_ontology:constraint_beneficiary(new_technology_adoption_coordination, early_adopters).
narrative_ontology:constraint_beneficiary(new_technology_adoption_coordination, standardization_bodies).
narrative_ontology:constraint_victim(new_technology_adoption_coordination, late_adopters).
narrative_ontology:constraint_victim(new_technology_adoption_coordination, locked_in_workers).
narrative_ontology:constraint_victim(new_technology_adoption_coordination, small_competitors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGACY WORKFORCE (SNARE) — Workers in declining technology sectors face retraining costs, wage penalties, and geographic immobility. No alternative exit; adaptation is mandatory. Trapped in mandatory technology transitions with extraction running full force.
constraint_indexing:constraint_classification(new_technology_adoption_coordination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OPERATORS (TANGLED ROPE) — Face mandatory adoption pressures (supply chain requirements, customer demand) but benefit from network effects and efficiency gains once adopted. Constrained by switching costs and coordination requirements, yet genuine efficiency coordination exists. Mixed extraction and benefit.
constraint_indexing:constraint_classification(new_technology_adoption_coordination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TECHNOLOGY VENDORS (ROPE) — Primary beneficiary with full arbitrage exit (can pivot to different markets, customers, technologies). Benefits from adoption coordination; experiences constraint as solution to collective action problem. Net beneficiary.
constraint_indexing:constraint_classification(new_technology_adoption_coordination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GOVERNMENT DIGITAL TRANSITION PROGRAMS (SCAFFOLD) — Organized transitional support (retraining subsidies, adoption tax credits, migration assistance) with explicit sunset: programs designed to decrease as adoption reaches critical mass. Active enforcement through mandates, but with built-in sunset logic as market matures. Legitimate coordination with declining coercion.
constraint_indexing:constraint_classification(new_technology_adoption_coordination, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LARGE INCUMBENT FIRMS (TANGLED ROPE) — Powerful actors can choose adoption timing strategately (mobile exit). Genuine coordination function (supply chain integration, compatibility standards) alongside asymmetric extraction through early-mover control of compatibility layers. Significant agency reduces experienced extraction; some beneficiary dynamics present.
constraint_indexing:constraint_classification(new_technology_adoption_coordination, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From civilizational/analytical view, technology adoption is fundamentally a coordination problem: network effects require critical mass, compatibility standards require mutual commitment, collective risk-taking is necessary. The constraint solves real coordination failures. Pure coordination reading with low base extractiveness. Theater minimal.
constraint_indexing:constraint_classification(new_technology_adoption_coordination, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(new_technology_adoption_coordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(new_technology_adoption_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(new_technology_adoption_coordination, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(new_technology_adoption_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint solves a genuine coordination problem — technology adoption requires critical mass, and solving this problem requires some agents to bear transition costs. However, the distribution of costs is asymmetric: vendors and early adopters capture benefits while legacy workers bear disruption. The extractiveness is not low (not pure rope) because the transition is mandatory rather than voluntary, and transition support is insufficient to close wage gaps. Suppression (0.42): Moderate-high. Multiple barriers constrain alternatives: supply-chain dependencies force adoption, legacy skills depreciate rapidly, geographic immobility limits retraining access, and career switching penalties penalize late adaptation. Yet suppression is not total — some workers successfully transition, some firms maintain legacy systems, and some sectors resist adoption. Theater ratio (0.38): Low-moderate. Adoption has real efficiency gains but includes performative elements: vendor marketing framing adoption as inevitable progress, compliance certification theater, and adoption narratives that naturalize what are actually contestable technical choices. The theater has increased over time as vendors escalate marketing pressure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Legacy workers see a snare: mandatory disruption with inadequate support and no exit. Small business operators see tangled rope: genuine efficiency benefits mixed with adoption mandates and switching costs. Technology vendors see rope: solving a coordination problem they benefit from. Government sees scaffold: transitional coordination with sunset logic. Large firms see tangled rope with more agency: they coordinate supply chains while controlling compatibility advantage. The analytical observer risks seeing pure rope (efficiency coordination) but the structural data reveals extraction: mandatory adoption, asymmetric burden distribution, and insufficient transition support transform coordination into mixed extraction. The perspectival gap reveals how institutional power reshapes coordination into rent-extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors and early adopters have low directionality values (beneficiary + mobile/arbitrage exit) producing negative or minimal experienced extraction. Legacy workers have high directionality values (victim + trapped exit) producing maximum extraction. Small business operators occupy middle ground: they are victims of mandatory adoption but have constrained mobility, producing moderate extraction. Powerful incumbents have lower d values despite being partly victims because they retain significant exit options (strategic timing, leverage over standards). Suppression is structurally unscaled: it operates identically whether measuring from a local workforce or global technology market. Extractiveness scales with scope: a local adoption mandate (σ=0.8) has lower effective extraction than a global supply-chain requirement (σ=1.2) because global scope makes alternative exit paths more difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by separating coordination function from extraction mechanism. The coordination function is real: technology adoption does solve genuine problems of network effects and standards compatibility. The extraction mechanism is also real: vendors and early adopters capture asymmetric benefits while late adopters and legacy workers bear disruption costs. The constraint is not 'coordination disguised as extraction' (false rope that is actually snare) — it is genuine tangled rope with both functions coexisting. The government scaffold perspective reveals the intended resolution: transition support with sunset logic. If retraining programs successfully close wage gaps before workers are locked out (omega variable 2), the scaffold assumption holds and extraction is temporary. If retraining fails (workers remain permanently displaced), the constraint degrades toward pure snare, and the scaffold sunset is aspirational rather than structural. The mandatrophy asks: is this extraction justified by coordination benefit? The answer depends on whether transition support is adequate — if yes, mixed classification holds; if no, extraction dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disruption_vs_coordination_tradeoff,
    'Is adoption acceleration primarily coordination (solving collective action problems) or primarily disruption (concentrating rents through forced obsolescence)?',
    'Longitudinal analysis of wage trajectories, firm profitability distributions, and network effects before/after adoption windows; comparison of forced vs voluntary adoption regimes',
    'If primarily coordination: extractiveness should be 0.20-0.30 (rope-dominant). If primarily disruption: extractiveness should be 0.50-0.70 (snare-dominant). Current assessment (0.35) assumes mixed mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disruption_vs_coordination_tradeoff, empirical, 'Whether adoption constraints solve coordination or concentrate extraction').

omega_variable(
    retraining_efficacy_and_timing,
    'Do retraining programs close the wage gap before workers are locked out of labor markets, or does the temporal mismatch create permanent extraction?',
    'Cohort analysis of displaced workers: wage recovery timelines, employment rates 2/5/10 years post-displacement, program completion rates correlated with re-entry success',
    'If retraining closes gap within 2-3 years: scaffold sunset is real and justified. If gap persists beyond 5 years: suppression mechanism is structural, not temporary — reclassify toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retraining_efficacy_and_timing, empirical, 'Whether retraining programs effectively mitigate adoption disruption').

omega_variable(
    compatibility_standard_lock_in,
    'Do early-mover technology choices create path-dependent lock-in that extracts from later adopters, or do open standards prevent monopolistic compatibility extraction?',
    'Historical analysis of compatibility decisions, switching cost estimates, antitrust cases involving lock-in, comparison of open-standard vs proprietary adoption regimes',
    'If lock-in dominant: extractiveness rises toward 0.50+ and classification shifts toward snare from powerful/mobile perspective. If standards prevent lock-in: rope classification is appropriate and extraction is primarily transaction cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compatibility_standard_lock_in, empirical, 'Whether adoption coordination enables lock-in extraction').

omega_variable(
    adoption_rate_natural_vs_mandated,
    'Would the observed adoption rate occur without coordinating constraints, or do mandates/supply-chain pressures impose extraction beyond natural adoption economics?',
    'Comparison of adoption trajectories in regulated vs unregulated sectors, voluntary vs mandatory adoption programs, historical technology adoption S-curves with and without policy intervention',
    'If natural adoption rate would be similar: constraint is low-extraction coordination (rope). If mandates accelerate adoption significantly beyond natural curve: extraction is real (tangled_rope to snare depending on who bears transition costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adoption_rate_natural_vs_mandated, empirical, 'Whether adoption constraints accelerate beyond natural rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(new_technology_adoption_coordination, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ntac_tr_t0, new_technology_adoption_coordination, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ntac_tr_t5, new_technology_adoption_coordination, theater_ratio, 5, 0.32).
narrative_ontology:measurement(ntac_tr_t10, new_technology_adoption_coordination, theater_ratio, 10, 0.38).
narrative_ontology:measurement(ntac_tr_t15, new_technology_adoption_coordination, theater_ratio, 15, 0.35).

% Extraction over time
narrative_ontology:measurement(ntac_be_t0, new_technology_adoption_coordination, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ntac_be_t5, new_technology_adoption_coordination, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(ntac_be_t10, new_technology_adoption_coordination, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(ntac_be_t15, new_technology_adoption_coordination, base_extractiveness, 15, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(new_technology_adoption_coordination, resource_allocation).
narrative_ontology:affects_constraint(new_technology_adoption_coordination, skill_obsolescence_trap).
narrative_ontology:affects_constraint(new_technology_adoption_coordination, vendor_lock_in_extraction).

% DUAL FORMULATION NOTE:
% New technology adoption coordination is upstream of skill-specific constraints (skilled worker dependency on legacy systems) and vendor lock-in mechanisms (compatibility extraction). Adoption acceleration feeds downstream extraction through these mechanisms. Separate stories capture adoption-driven skill obsolescence (interaction of labor markets + technology disruption) and vendor lock-in (technical architecture + market power).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(new_technology_adoption_coordination, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
