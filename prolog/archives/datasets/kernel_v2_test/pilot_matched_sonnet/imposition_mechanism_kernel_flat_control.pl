% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_mechanism_kernel_flat_control
 *   human_readable: Legitimacy Grounding for Temporal and Sartorial Norms
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   The legitimacy grounding for temporal and sartorial norms represents a
 *   foundational question in state formation and cultural authority: by what
 *   mechanism does a particular calendar system or dress code become
 *   'correct' rather than merely conventional? This constraint operates
 *   through hybrid symbolic-coercive mechanisms that combine genuine
 *   coordination benefits (shared temporal reference enables commerce,
 *   administration, social coordination) with extraction from minority
 *   traditions and non-conforming communities. The state apparatus claims
 *   exclusive temporal sovereignty — the authority to define official time,
 *   mandate calendar systems, and enforce sartorial norms in public spaces
 *   and official contexts. This claim is partly vindicated by real
 *   coordination benefits (everyone using the same calendar reduces
 *   transaction costs) and partly maintained through suppression of
 *   alternatives (minority calendars are delegitimized, non-conforming dress
 *   is penalized). The constraint's extractiveness has increased over the
 *   interval as state administrative capacity expanded, reaching peak
 *   suppression during the modern state formation period (industrial era
 *   through mid-20th century) before declining slightly in the contemporary
 *   period as multicultural accommodation norms emerged. Theater ratio shows
 *   similar trajectory: the performative aspect of temporal/sartorial
 *   authority (ritual calendar reforms, official dress codes, symbolic
 *   enforcement) increased as states developed bureaucratic capacity to stage
 *   legitimacy performances, then declined modestly as the coordination
 *   function became more genuinely functional through network effects.
 *
 * KEY AGENTS:
 *   - State Apparatus: Primary beneficiary (institutional/arbitrage) — defines official calendar and dress standards, collects legitimacy from coordination function, exercises temporal sovereignty
 *   - Dominant Cultural Bloc: Secondary beneficiary (institutional/arbitrage) — their temporal and sartorial traditions become 'official', reducing their coordination costs while imposing costs on minorities
 *   - Minority Temporal Tradition Practitioners: Primary victim (powerless/identity_locked) — identity constituted through maintenance of minority calendar/dress traditions; exit requires abandoning identity frame
 *   - Non-Conforming Communities: Secondary victim (powerless/trapped or constrained) — face material penalties for non-conformity; may be materially trapped (employment discrimination, administrative barriers) rather than identity-locked
 *   - Urban Professional: Mixed position (moderate/constrained) — benefits from coordination function but bears conformity costs; navigates multiple norm systems
 *   - Religious Authority: Organized negotiator (organized/constrained) — maintains parallel calendar system, negotiates dual legitimacy with state temporal authority
 *   - International Standardization Coalition: Organized coordinator (organized/mobile) — sees temporal standardization as transitional scaffold toward voluntary coordination equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel_flat_control, 0.48).
domain_priors:suppression_score(imposition_mechanism_kernel_flat_control, 0.62).
domain_priors:theater_ratio(imposition_mechanism_kernel_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel_flat_control, extractiveness, 0.48).
narrative_ontology:constraint_metric(imposition_mechanism_kernel_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(imposition_mechanism_kernel_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel_flat_control, "Legitimacy Grounding for Temporal and Sartorial Norms").
narrative_ontology:topic_domain(imposition_mechanism_kernel_flat_control, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(imposition_mechanism_kernel_flat_control, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel_flat_control, state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel_flat_control, dominant_cultural_bloc).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel_flat_control, standardization_beneficiaries).
narrative_ontology:constraint_victim(imposition_mechanism_kernel_flat_control, minority_temporal_traditions).
narrative_ontology:constraint_victim(imposition_mechanism_kernel_flat_control, non_conforming_communities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel_flat_control, displaced_calendar_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY TEMPORAL TRADITION (SNARE) — Identity-locked rather than trapped: the practitioner is structurally mobile (could adopt dominant calendar/dress) but their identity is constituted through maintenance of the minority tradition. Exit would require abandoning not just practice but the identity frame that makes the tradition meaningful. Experiences the constraint as pure extraction: the state's temporal authority delegitimizes their calendar system, imposes coordination costs (translating between systems), and extracts compliance through administrative requirements (official documents, school schedules, legal proceedings all use dominant calendar). The coordination story (everyone needs a shared calendar) is cover for extraction from those whose traditions are displaced.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: URBAN PROFESSIONAL (TANGLED ROPE) — Constrained by professional dress codes and standardized work schedules but also benefits from the coordination function: shared temporal reference enables commerce, travel, professional networking across regions. Experiences both genuine coordination (can schedule meetings, plan travel, coordinate supply chains) and asymmetric extraction (must conform to dominant norms or face career penalty; minority traditions require extra translation labor). The constraint coordinates and extracts simultaneously through the same mechanism.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE APPARATUS (ROPE) — Primary beneficiary with arbitrage-level exit: the state defines the standard and can revise it (calendar reforms, official dress codes for state functions). Experiences the constraint as pure coordination: temporal and sartorial standardization enables administrative efficiency, tax collection, military mobilization, census operations. The state sees itself as solving a genuine collective action problem (how do we coordinate millions of people?) and collects legitimacy rather than rents. Extraction toward other agents is invisible from this seat.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RELIGIOUS AUTHORITY (TANGLED ROPE) — Organized agents (religious institutions maintaining parallel calendar systems) experience both coordination and extraction. Coordination: the religious calendar organizes ritual life, pilgrimage timing, fasting periods for the community. Extraction: must continuously negotiate with state temporal authority, translate between systems, defend legitimacy of parallel calendar against state claims of exclusive temporal sovereignty. Constrained exit: could abandon parallel calendar but at cost of religious authority and community identity. Generational horizon: sees the negotiation as multi-generational project of maintaining dual legitimacy.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INTERNATIONAL STANDARDIZATION COALITION (SCAFFOLD) — Organized agents (ISO, UTC, international bodies) see temporal standardization as transitional coordination with implicit sunset logic: once global coordination is achieved through voluntary adoption of standards (UTC, ISO 8601), the coercive enforcement mechanisms become unnecessary. The constraint is temporary support for achieving coordination equilibrium. Mobile exit: these agents can shift between competing standards and are not locked into any single system. The scaffold framing assumes that standardization will eventually be maintained by network effects rather than state coercion.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits both genuine coordination function (societies need shared temporal reference, some degree of sartorial legibility) and substantial extraction (state monopolization of calendar authority, suppression of minority traditions, symbolic violence through dress codes). The analytical observer sees the hybrid mechanism: endogenous adoption (network effects, coordination benefits) and exogenous mandate (state enforcement, administrative requirements) operate simultaneously. The constraint is structurally a tangled rope: it coordinates and extracts through the same institutional apparatus, and both functions are real rather than one being cover for the other.
constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imposition_mechanism_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from minority traditions and non-conforming communities through delegitimization of alternative calendar systems, administrative penalties for non-conformity, and symbolic violence through dress codes. However, extraction is not maximal because genuine coordination benefits exist: shared temporal reference does reduce transaction costs, and some degree of sartorial legibility does enable social coordination. The extractiveness value reflects that both functions are real — this is not pure extraction with coordination as cover (which would warrant 0.70+), nor is it pure coordination with trivial extraction (which would warrant 0.20-). The value increased over the interval as state administrative capacity expanded and enforcement mechanisms matured. Suppression (0.62): Moderate-high. Significant barriers to maintaining minority temporal traditions include administrative requirements (official documents use dominant calendar), employment discrimination (professional dress codes), educational system enforcement (school schedules follow official calendar), and legal system requirements (court dates, contract law, statute of limitations all reference official time). Suppression peaked during modern state formation (0.68) when bureaucratic capacity was high and multicultural accommodation norms were weak, then declined modestly (0.62) in contemporary period as accommodation norms emerged. Suppression is not total — some communities successfully maintain parallel calendar systems, and some jurisdictions accommodate minority traditions — but barriers remain substantial. Theater ratio (0.58): Moderate-high. Substantial performative content: calendar reforms are staged as expressions of state sovereignty (French Revolutionary calendar, Soviet calendar reform, various nationalist calendar projects), official dress codes for state functions serve symbolic rather than functional purposes, and enforcement of sartorial norms often targets symbolic non-conformity (religious dress, counter-cultural styles) rather than genuine coordination failures. The theater increased over the interval as states developed capacity to stage legitimacy performances, peaking during the modern state period (0.62) when bureaucratic ritual was most elaborate, then declining slightly (0.58) as the coordination function became more genuinely functional through network effects and digital standardization.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates classic tangled rope structure from the analytical perspective: genuine coordination function and asymmetric extraction operate through the same institutional mechanism. The state apparatus sees pure coordination (Rope) — they are solving the legitimate problem of temporal and sartorial standardization for millions of people. The international standardization coalition sees transitional coordination (Scaffold) — enforcement will sunset once network effects sustain voluntary adoption. The minority tradition practitioner sees pure extraction (Snare) — the coordination story is cover for cultural domination and identity erasure. The urban professional sees mixed coordination and extraction (Tangled Rope) — benefits from standardization but bears conformity costs. The religious authority sees ongoing negotiation (Tangled Rope) — must continuously defend parallel calendar legitimacy against state temporal sovereignty claims. These are not competing interpretations of ambiguous data — they are structurally accurate descriptions from different positions in the extraction/coordination flow. The beneficiary genuinely experiences coordination; the victim genuinely experiences extraction; both are true simultaneously because the constraint coordinates some agents by extracting from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. The state apparatus is the primary beneficiary with arbitrage-level exit — they define the standard and can revise it, so their directionality is low (near 0.0), producing negative or near-zero effective extraction (they experience the constraint as pure benefit). The dominant cultural bloc is a secondary beneficiary — their traditions become official, reducing their costs — so their directionality is also low. Minority tradition practitioners are primary victims with identity-locked exit — their directionality is high (near 1.0) because extraction flows away from them and they cannot exit without abandoning their identity frame, producing maximum effective extraction. Non-conforming communities are secondary victims with trapped or constrained exit — their directionality is high but modulated by whether barriers are material (trapped) or surmountable (constrained). The urban professional is mixed — benefits from coordination but bears conformity costs — so directionality is moderate (around 0.5), producing moderate effective extraction. The religious authority is an organized negotiator with constrained exit — directionality is moderate-high because they bear extraction (must defend parallel calendar legitimacy) but also derive some benefit (their calendar organizes community ritual life). The international standardization coalition has mobile exit and sees themselves as coordinators rather than targets, so directionality is low. The analytical observer computes these directionality values from the structural data and recognizes that all perspectives are simultaneously valid — the constraint genuinely coordinates and genuinely extracts, and which function dominates depends on where you sit in the flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope is not a compromise classification or a failure to choose between rope and snare — it is a structurally distinct type describing constraints that coordinate and extract through the same mechanism. The state's temporal sovereignty claim has a genuine coordination function: shared calendar reference does reduce transaction costs, enable commerce and administration, and solve real collective action problems. The coordination is not cover — it is real. Simultaneously, the constraint extracts from minority traditions through delegitimization, administrative penalties, and symbolic violence. The extraction is not incidental — it is structural. Both functions operate through the same institutional apparatus (state calendar authority, official dress codes, administrative requirements), and neither can be removed without destroying the other. This is the defining feature of tangled rope: you cannot separate the coordination from the extraction because they are the same mechanism viewed from different positions in the flow. The mandatrophy temptation is to classify this as either rope (if you emphasize coordination benefits) or snare (if you emphasize extraction from minorities). The tangled rope classification resists that temptation by insisting that both are true and both are structural rather than one being cover for the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_primacy,
    'Is the legitimacy grounding primarily endogenous (cultural adoption driven by coordination benefits) or exogenous (state mandate backed by coercion), and does the answer vary by domain (temporal vs sartorial) and historical period?',
    'Historical analysis of calendar and dress code adoption patterns: compare voluntary adoption rates in periods of weak state capacity vs strong state enforcement; examine persistence of minority traditions under varying enforcement regimes; analyze adoption curves for international standards (UTC, business dress) in contexts without state mandate.',
    'If primarily endogenous: constraint is closer to rope (genuine coordination) with extraction as secondary effect. If primarily exogenous: constraint is closer to snare (extraction with coordination as cover). If genuinely hybrid: tangled rope classification is structurally accurate. The answer likely varies: temporal norms may be more endogenous (coordination benefits are high), sartorial norms more exogenous (coordination benefits are lower, symbolic control is higher).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_primacy, empirical, 'Whether legitimacy grounding is primarily endogenous adoption or exogenous mandate').

omega_variable(
    minority_tradition_exit_cost,
    'What is the true exit cost for minority temporal tradition practitioners: material barriers (administrative penalties, employment discrimination) or identity barriers (abandoning tradition requires abandoning identity frame)?',
    'Ethnographic study of communities maintaining minority calendars: measure material costs (time spent translating between systems, administrative penalties, employment barriers) vs identity costs (community belonging, religious meaning, intergenerational transmission). Interview individuals who have exited minority traditions to assess whether the binding was structural or cognitive.',
    'If primarily material: exit_options should be ''trapped'' or ''constrained'' rather than ''identity_locked''. If primarily identity-based: ''identity_locked'' is accurate and the constraint''s suppression is partially internalized. The distinction matters for intervention design: material barriers can be reduced through accommodation policies; identity barriers require different approaches (dual-calendar systems, cultural recognition).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_tradition_exit_cost, empirical, 'Whether minority tradition practitioners are materially trapped or identity-locked').

omega_variable(
    standardization_sunset_realism,
    'Is the international standardization coalition''s scaffold framing realistic, or does temporal/sartorial authority require permanent enforcement?',
    'Longitudinal analysis of standardization trajectories: examine cases where international standards achieved dominance (metric system, UTC, ISO standards) and assess whether enforcement mechanisms actually sunset or persist indefinitely. Compare voluntary adoption rates in early vs late phases of standardization. Identify cases where standards collapsed after enforcement ended.',
    'If standards require permanent enforcement: scaffold perspective is aspirational rather than structural, and the constraint is better classified as tangled rope or snare with no sunset. If standards become self-sustaining through network effects: scaffold classification is accurate for the international standardization layer, though state-level enforcement may persist for other reasons (symbolic control, administrative convenience).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standardization_sunset_realism, empirical, 'Whether standardization can sunset enforcement or requires permanent coercion').

omega_variable(
    temporal_vs_sartorial_mechanism_divergence,
    'Do temporal and sartorial norms have structurally different legitimacy mechanisms, or are they parallel instances of the same state authority pattern?',
    'Comparative analysis of calendar reforms vs dress code enforcement across multiple state formation episodes: measure coordination benefits (network effects, transaction cost reduction) vs symbolic control benefits (legibility, status hierarchy maintenance) for each domain. Assess whether temporal norms show higher voluntary adoption rates and lower enforcement requirements than sartorial norms.',
    'If mechanisms diverge: the constraint should be decomposed into separate stories (temporal_authority_grounding and sartorial_authority_grounding) per the epsilon-invariance principle, as they would have different extractiveness values. If mechanisms are parallel: the unified story is structurally accurate. Preliminary assessment: temporal norms likely have higher genuine coordination function (epsilon ~0.35) while sartorial norms likely have higher extraction (epsilon ~0.60), suggesting decomposition is warranted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_vs_sartorial_mechanism_divergence, conceptual, 'Whether temporal and sartorial norms should be modeled as one constraint or decomposed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel_flat_control, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impos_theater_early_modern, imposition_mechanism_kernel_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(impos_theater_industrial, imposition_mechanism_kernel_flat_control, theater_ratio, 50, 0.48).
narrative_ontology:measurement(impos_theater_modern_state, imposition_mechanism_kernel_flat_control, theater_ratio, 100, 0.62).
narrative_ontology:measurement(impos_theater_contemporary, imposition_mechanism_kernel_flat_control, theater_ratio, 150, 0.58).

% Extraction over time
narrative_ontology:measurement(impos_extract_early_modern, imposition_mechanism_kernel_flat_control, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(impos_extract_industrial, imposition_mechanism_kernel_flat_control, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(impos_extract_modern_state, imposition_mechanism_kernel_flat_control, base_extractiveness, 100, 0.45).
narrative_ontology:measurement(impos_extract_contemporary, imposition_mechanism_kernel_flat_control, base_extractiveness, 150, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(impos_suppress_early_modern, imposition_mechanism_kernel_flat_control, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(impos_suppress_industrial, imposition_mechanism_kernel_flat_control, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(impos_suppress_modern_state, imposition_mechanism_kernel_flat_control, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(impos_suppress_contemporary, imposition_mechanism_kernel_flat_control, suppression_requirement, 150, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint may warrant decomposition into separate temporal_authority_grounding and sartorial_authority_grounding stories if empirical analysis confirms that coordination benefits and extraction mechanisms differ substantially between domains (omega: temporal_vs_sartorial_mechanism_divergence). Preliminary assessment suggests temporal norms have higher genuine coordination function (lower epsilon) while sartorial norms have higher symbolic control function (higher epsilon), which would violate epsilon-invariance if modeled as a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
