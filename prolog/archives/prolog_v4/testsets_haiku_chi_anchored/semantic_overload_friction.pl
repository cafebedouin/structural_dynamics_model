% ============================================================================
% CONSTRAINT STORY: semantic_overload_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semantic_overload_friction, []).

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
 *   constraint_id: semantic_overload_friction
 *   human_readable: The Semantic Saturation Threshold
 *   domain: technological/social
 *
 * SUMMARY:
 *   As specialized domains mature — from software engineering to medicine to
 *   law to theoretical physics — their professional vocabularies become
 *   increasingly dense. This constraint represents the structural tension
 *   between precision (specialized language enables exact communication about
 *   complex topics) and accessibility (dense jargon excludes newcomers and
 *   blocks cross-domain collaboration). The semantic saturation threshold is
 *   the point at which jargon density exceeds functional necessity and begins
 *   serving primarily as a credentialing barrier. The constraint exhibits
 *   tangled rope classification: it coordinates within-domain expertise
 *   (genuine precision gains) while extracting from novices, outsiders, and
 *   cross-domain collaborators (forced apprenticeship, gatekeeping). Theater
 *   ratio (0.58) reflects that approximately 58% of specialized terminology
 *   serves performative credentialing rather than functional communication
 *   precision. Over the 50-year interval, extractiveness has increased from
 *   0.28 to 0.52, indicating that jargon accumulation has progressively
 *   exceeded functional requirements. Theater ratio has risen from 0.35 to
 *   0.58, showing that the proportion of terminological theater has grown as
 *   domains mature.
 *
 * KEY AGENTS:
 *   - Domain Specialists: Primary beneficiary (institutional/arbitrage) — capture status premium, precision gains, and credential value from specialized vocabulary
 *   - Domain Novices: Primary victim (powerless/trapped) — must invest years in lexical mastery; face permanent barriers to entry if cognitive load exceeds learning capacity
 *   - Junior Professionals: Secondary victim (moderate/constrained) — bear costs of credentialing apprenticeship while also benefiting from gatekeeping that reduces competition
 *   - Cross-Domain Collaboration: Victim (moderate/constrained) — semantic boundaries block integration of insights from adjacent fields; translation layers introduce friction and errors
 *   - Epistemic Accessibility: Abstract victim (powerless/trapped) — public understanding of specialized domains degrades; expertise becomes opaque to non-specialists
 *   - Cross-Domain Coalition: Organized agents (organized/constrained) — plain-language publishing, interdisciplinary translation initiatives, and open-science movements building alternative pathways
 *   - Historical Terminology System: Institutional actor (institutional/arbitrage) — Latin, Greek, and vestigial notation systems persist through tradition and authority rather than functional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semantic_overload_friction, 0.52).
domain_priors:suppression_score(semantic_overload_friction, 0.64).
domain_priors:theater_ratio(semantic_overload_friction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semantic_overload_friction, extractiveness, 0.52).
narrative_ontology:constraint_metric(semantic_overload_friction, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(semantic_overload_friction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semantic_overload_friction, tangled_rope).
narrative_ontology:human_readable(semantic_overload_friction, "The Semantic Saturation Threshold").
narrative_ontology:topic_domain(semantic_overload_friction, "technological/social").

domain_priors:requires_active_enforcement(semantic_overload_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semantic_overload_friction, domain_specialists).
narrative_ontology:constraint_beneficiary(semantic_overload_friction, credential_gatekeepers).
narrative_ontology:constraint_victim(semantic_overload_friction, domain_novices).
narrative_ontology:constraint_victim(semantic_overload_friction, cross_domain_collaboration).
narrative_ontology:constraint_victim(semantic_overload_friction, epistemic_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMAIN NOVICE (SNARE) — Cannot exit the lexical barriers without years of apprenticeship. Trapped by jargon density that increases faster than learning capacity. Theater ratio reflects that much specialized language serves credentialing rather than precision (e.g., Latin legal terminology). d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.75.
constraint_indexing:constraint_classification(semantic_overload_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JUNIOR PROFESSIONAL (TANGLED ROPE) — Constrained by requirement to master jargon for credentialing and collaboration, but also benefits from domain knowledge premium and gatekeeping that reduces competition. The semantic density both excludes competitors and enables precise communication within the field. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(semantic_overload_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMAIN SPECIALIST (ROPE) — Benefits from precision gains of specialized vocabulary. Experiences the constraint as coordination: dense language enables rapid communication among peers and establishes professional status. Can arbitrage between technical and lay contexts. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.07. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(semantic_overload_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CROSS-DOMAIN COALITION (SCAFFOLD) — Interdisciplinary collaboration initiatives, plain-language publishing movements, and open-science communities see semantic saturation as a temporary coordination failure with built-in sunset. Glossaries, translation layers (e.g., ArXiv abstracts for lay readers), and simplified notation systems are creating parallel pathways that bypass dense jargon. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.21. Low effective extraction because agents see alternative pathways forming.
constraint_indexing:constraint_classification(semantic_overload_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL TERMINOLOGY SYSTEM (PITON) — Vestigial Latin terminology in law, Greek in medicine, and obsolete notation systems persist through institutional inertia despite their low functional necessity. The constraint is maintained because tradition validates authority, not because the language precision justifies the accessibility cost. theater_ratio=0.58 reflects that approximately half the semantic density serves performative credentialing rather than functional precision.
constraint_indexing:constraint_classification(semantic_overload_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, semantic specialization is inherent to knowledge accumulation: as domains mature, precise language becomes necessary for capturing complex distinctions. This perspective frames jargon as an inescapable cost of technical depth. However, the structural data (ε=0.52, suppression=0.64, beneficiaries/victims explicitly declared, requires_active_enforcement=true) contradicts a mountain classification — the engine will detect this as a false summit, revealing that institutional gatekeeping (not inherent complexity) drives jargon accumulation.
constraint_indexing:constraint_classification(semantic_overload_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semantic_overload_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semantic_overload_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semantic_overload_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semantic_overload_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semantic_overload_friction, TR),
    TR >= 0.70.

:- end_tests(semantic_overload_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits genuine coordination benefits (precise technical communication within domains) but increasingly serves gatekeeping that exceeds functional requirements. The rise from 0.28 to 0.52 over the interval reflects accumulation of terminological theater beyond justified precision. Suppression (0.64): High. Barriers to entry include: (a) years of focused study required for fluency, (b) cognitive load that increases faster than domain complexity justifies, (c) publication norms that privilege technical terminology, (d) professional certification tied to vocabulary mastery, (e) absence of affordable translation layers. Suppression is structural but not absolute — some domains (software, contemporary fields) maintain lower semantic density. Theater ratio (0.58): Moderate-high. Approximately 58% of specialized terminology reflects credentialing theater rather than precision gain. Examples: Latin legal terminology (functionally equivalent to English alternatives), Greek medical terminology (precision no greater than English primitives), subscript notation in physics (convention rather than necessity). Theater has increased with domain maturity, suggesting that as genuine precision gains plateau, additional terminology increasingly serves status signaling.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp disagreement between specialists and novices. The specialist sees coordination (Rope) — dense language enables rapid, precise communication among trained peers. The novice sees pure extraction (Snare) — gatekeeping that forces costly apprenticeship. The junior professional experiences hybrid (Tangled Rope) — the system both requires and rewards lexical mastery. The cross-domain coalition sees a temporary problem with exit paths forming (Scaffold) — plain-language movements and translation layers are building alternative verification pathways. The vestigial terminology system (Piton) — Latin legal terms, Greek medical terms, obsolete notation — persists through institutional inertia despite low marginal precision benefit. The analytical observer risks seeing inherent necessity (Mountain) — specialized language must increase with domain complexity — but the rising theater ratio and extraction values reveal this as a false summit. The constraint is contingent on institutional credentialing structures, not inherent to knowledge itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Domain specialists: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Domain novices: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Junior professionals: Victim + constrained, but also have institutional connections → d≈0.68, f(d)≈1.05. High extraction. Cross-domain collaboration: Victim + constrained → d≈0.70, f(d)≈1.08. High extraction. Epistemic accessibility: Victim + trapped (abstract agent) → d≈0.95, f(d)≈1.42. Maximum extraction. Cross-domain coalition: Organized + constrained → d≈0.35, f(d)≈0.35. Low effective extraction; agents see alternative pathways. Historical terminology system: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate, not from beneficiary status.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that semantic saturation is NOT an inherent feature of knowledge complexity but a contingent institutional arrangement. The false summit (Mountain perspective) naturalizes what is actually a tangled rope. The mandatrophy is resolved by asking: 'Could the same domains function with lower semantic density?' The answer is yes — many fields (mathematics, computer science, contemporary physics) maintain lower jargon density than older domains (law, medicine, philosophy) despite equal or greater conceptual complexity. This reveals that terminological density is driven by institutional credentialing structures and historical accident, not functional necessity. The rising extractiveness (0.28 → 0.52) and theater ratio (0.35 → 0.58) over the interval confirm that jargon accumulation exceeds precision gains. The constraint is a tangled rope with a real coordination function (precise within-domain communication) and real extraction (novice gatekeeping, cross-domain friction), not a mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precision_necessity_threshold,
    'What fraction of specialized terminology serves functional precision vs credentialing theater?',
    'Comparative analysis: identical technical concepts explained in specialized vs plain language; measurement of comprehension loss when jargon is replaced with primitives',
    'If precision-functional ≥ 0.70: jargon is justified coordination (Rope/Tangled Rope). If < 0.70: jargon is primarily gatekeeping (Snare/Scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precision_necessity_threshold, empirical, 'Fraction of jargon serving precision vs credentialing').

omega_variable(
    lexical_growth_rate_ceiling,
    'Is semantic saturation self-limiting (growth rate slows as domain matures) or accelerating (new subdisciplines create new terminology)?',
    'Time-series analysis of new term adoption rates; tracking of terminology adoption cycles in mature vs emerging fields',
    'If self-limiting: saturation is temporary (scaffold perspective valid). If accelerating: saturation is structural (snare perspective deepens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lexical_growth_rate_ceiling, empirical, 'Whether semantic growth is self-limiting or accelerating').

omega_variable(
    cognitive_load_vs_precision_tradeoff,
    'Does increased jargon density improve precision of domain concepts or merely shift cognitive load to memorization?',
    'Controlled studies comparing expert comprehension and communication speed with vs without specialized terminology; error analysis on tasks requiring novel application',
    'If precision improved: dense language is functional (justifies tangled rope). If load merely shifted: density is performative (confirms snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_load_vs_precision_tradeoff, empirical, 'Whether jargon improves precision or merely shifts cognitive burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semantic_overload_friction, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semsat_tr_t0, semantic_overload_friction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(semsat_tr_t25, semantic_overload_friction, theater_ratio, 25, 0.47).
narrative_ontology:measurement(semsat_tr_t50, semantic_overload_friction, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(semsat_be_t0, semantic_overload_friction, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(semsat_be_t25, semantic_overload_friction, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(semsat_be_t50, semantic_overload_friction, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semantic_overload_friction, information_standard).
narrative_ontology:affects_constraint(semantic_overload_friction, credential_gatekeeping).
narrative_ontology:affects_constraint(semantic_overload_friction, interdisciplinary_integration_friction).
narrative_ontology:affects_constraint(semantic_overload_friction, knowledge_opacity_to_publics).

% DUAL FORMULATION NOTE:
% Semantic saturation can be decomposed into two structurally distinct constraints: (1) functional precision accumulation (ε≈0.15, Mountain for mature domains) and (2) credentialing theater accumulation (ε≈0.45, Tangled Rope). The constraint story treats them as unified (ε=0.52) to model the real institutional experience, where specialists cannot easily distinguish precision gains from status signaling. Network links to credential_gatekeeping (upstream cause) and interdisciplinary_integration_friction (downstream effect).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semantic_overload_friction, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
