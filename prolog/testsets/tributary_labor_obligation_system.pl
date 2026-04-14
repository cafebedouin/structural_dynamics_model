% ============================================================================
% CONSTRAINT STORY: tributary_labor_obligation_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tributary_labor_obligation_system, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tributary_labor_obligation_system
 *   human_readable: Tributary Labor Obligation System
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   The tributary labor obligation system is a historical and contemporary
 *   constraint in which subordinate communities are required to provide labor
 *   service to dominant political authorities in exchange for nominal
 *   protection or administrative services. This constraint exists across
 *   multiple historical contexts (pre-Columbian empires, medieval serfdom,
 *   colonial systems, and modern labor trafficking networks) and creates a
 *   structural extraction mechanism with high suppression and
 *   identity-locking components. The system persists through legal
 *   enforcement, resource dependency, geographic immobility, and internalized
 *   cultural obligation. Extractiveness has increased over the measurement
 *   interval (0.55 to 0.71) as administrative capacity for enforcement
 *   improved, while theater ratio (0.35 to 0.58) increased as ideological
 *   justification became more elaborate. The system requires constant
 *   enforcement and shows no natural stability — it is a pure extraction
 *   mechanism disguised as cultural tradition or political necessity.
 *
 * KEY AGENTS:
 *   - Tributary Labor Population: Primary victims (powerless/trapped) — forced to provide labor service, bear full cost of extraction
 *   - Local Subject Communities: Secondary victims (powerless/identity_locked) — internalized obligation as cultural duty, cannot perceive exit despite some material mobility
 *   - Local Administrative Officials: Secondary beneficiaries (organized/constrained) — derive career benefits and privileges, maintain coordination function within extraction system
 *   - Tributary State Apparatus: Primary beneficiary (institutional/arbitrage) — collects and redistributes labor resources, maintains political order
 *   - Regional Hegemon: Ultimate beneficiary (institutional/arbitrage) — collects upper-tier tributary flows, maintains continental hierarchy
 *   - Reform Movements and Peasant Associations: Organized resistance (organized/constrained) — see system as degraded theater requiring coercive maintenance
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — assesses system as pure extraction mechanism without natural law justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tributary_labor_obligation_system, 0.68).
domain_priors:suppression_score(tributary_labor_obligation_system, 0.78).
domain_priors:theater_ratio(tributary_labor_obligation_system, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tributary_labor_obligation_system, extractiveness, 0.68).
narrative_ontology:constraint_metric(tributary_labor_obligation_system, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tributary_labor_obligation_system, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tributary_labor_obligation_system, snare).
narrative_ontology:human_readable(tributary_labor_obligation_system, "Tributary Labor Obligation System").
narrative_ontology:topic_domain(tributary_labor_obligation_system, "economic/political/social").

domain_priors:requires_active_enforcement(tributary_labor_obligation_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tributary_labor_obligation_system, tributary_state_apparatus).
narrative_ontology:constraint_beneficiary(tributary_labor_obligation_system, regional_hegemon).
narrative_ontology:constraint_victim(tributary_labor_obligation_system, tributary_labor_population).
narrative_ontology:constraint_victim(tributary_labor_obligation_system, local_subsistence_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRIBUTARY LABOR POPULATION (SNARE) — Structurally trapped by legal obligation, resource dependency, and geographic immobility. Labor is extracted through mandatory service with minimal compensation. No alternative livelihood paths exist. Maximum experienced extraction and suppression. Cannot organize or exit without severe material consequences.
constraint_indexing:constraint_classification(tributary_labor_obligation_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRIBUTARY SUBJECT COMMUNITIES (SNARE with identity_locked) — At generational scale, subjects who have internalized tributary obligations as cultural duty or divine obligation cannot perceive exit even where material barriers are surmountable. Identity is fused with subject status. The obligation persists through internalized framing ('this is how things are') rather than through external force alone. Structurally mobile but perceptually trapped.
constraint_indexing:constraint_classification(tributary_labor_obligation_system, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: LOCAL ADMINISTRATIVE OFFICIALS (TANGLED ROPE) — Constrained by career dependence on the tributary apparatus. Experience genuine coordination function: organizing labor rotation, managing dispute resolution within tributary system. Also extract personal benefit through administrative positions and privileges. Mixed coordination and extraction — cannot exit without career loss, but role has legitimacy components.
constraint_indexing:constraint_classification(tributary_labor_obligation_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: TRIBUTARY STATE APPARATUS (ROPE) — Benefits from labor extraction and sees the system as a coordination mechanism for maintaining political order and resource flows. Can exit the constraint through imperial dissolution or conquest, but current institutional interest is in maintaining the system. Net beneficiary experiencing the constraint as functional governance.
constraint_indexing:constraint_classification(tributary_labor_obligation_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: REGIONAL HEGEMON (ROPE) — Higher-level authority that collects tributary labor flows. Experiences system as pure coordination: collecting and redistributing labor resources, maintaining political hierarchy, preventing rebellion. Institutional beneficiary with arbitrage options (can maintain or dissolve the system). Zero experienced extraction from this position.
constraint_indexing:constraint_classification(tributary_labor_obligation_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: REFORM MOVEMENTS AND PEASANT ASSOCIATIONS (PITON) — Organized resistance movements see the tributary system as degraded and functional only through inertia and surveillance. Perceive the system as theater: the claimed necessity of labor obligations is performative, while the real mechanism is coercive enforcement and identity capture. Theater ratio high because the ideological justification ('natural order,' 'cultural tradition') is doing extraction work rather than the mechanism itself.
constraint_indexing:constraint_classification(tributary_labor_obligation_system, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — At civilizational scale, the tributary labor obligation system classifies as pure extraction mechanism. No inherent natural law component. The system requires active enforcement and collapses without coercion. Survives through suppression of alternatives (military, administrative control) and identity capture. High extractiveness (0.68) and high suppression (0.78) with sustained coercive apparatus.
constraint_indexing:constraint_classification(tributary_labor_obligation_system, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tributary_labor_obligation_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tributary_labor_obligation_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tributary_labor_obligation_system, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tributary_labor_obligation_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tributary_labor_obligation_system, TR),
    TR >= 0.70.

:- end_tests(tributary_labor_obligation_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The system extracts significant labor value from subordinate populations with minimal compensation. The extraction has grown over time (0.55→0.71) as administrative capacity improved, indicating this is not a stable equilibrium but an accumulating mechanism. Suppression (0.78): Very high. Multiple suppression mechanisms operate simultaneously: legal prohibition on exit, resource dependency through land access control, geographic immobility, military capacity to enforce collection, administrative surveillance of labor compliance, and internalized obligation through identity capture. The system explicitly prohibits alternatives and applies severe penalties for resistance. Theater ratio (0.52): Moderate and increasing. The system claims legitimacy through cultural tradition, divine sanction, or political necessity ('protection,' 'order'), but actual mechanism is enforcement. Theater has increased (0.35→0.52) as ideology elaborated, suggesting system visibility as pure extraction is forcing legitimacy work. Claimed type (Snare): Confirmed. High extractiveness (0.68 > 0.46), high suppression (0.78 > 0.60), effective extraction χ well above 0.66 threshold. No genuine coordination benefit — any coordination function (dispute resolution, security) is secondary to extraction goal and maintained primarily to enable continued extraction.
 *
 * PERSPECTIVAL GAP:
 *   Absolute perspectival divergence. Beneficiaries (institutional/arbitrage) experience the constraint as coordination and governance. Victims (powerless/trapped or identity_locked) experience it as pure extraction with no coordination benefit to them. Local officials occupy a genuine middle ground with mixed experience. The gap is not a measurement artifact but reflects real structural differences in how the system functions: it does provide some public goods (courts, security apparatus) that beneficiaries use and value, while simultaneously extracting labor value from populations who are excluded from benefits. The analytical observer recognizes this as a tangled structure where genuine coordination for the center is funded entirely by extraction from the periphery — what appears as Rope (coordination) from institutional perspective is Snare (extraction) from powerless perspective because the coordination benefits do not accrue to the extracted population. This is the canonical structure of colonialism and empire.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation: Labor population faces victim status (beneficiaries gain, victims lose) + trapped exit (d → 0.95 → high extraction). Identity-locked subjects: victim status + identity_locked exit (d → 0.89 → still high extraction despite perceptual mobility). Officials: mixed status (benefit from privilege but coerced by superiors) + constrained exit (d → 0.50 → moderate extraction). Apparatus: beneficiary status (extracts labor, maintains order) + arbitrage exit (d → 0.15 → low extraction). Hegemon: beneficiary status + arbitrage exit (d → 0.05 → maximum benefit). The directionality chain reveals that effective extractiveness scales with the agent's power deficit and exit barriers. Powerless + trapped produces the highest χ; institutional + arbitrage produces the lowest.
 *
 * MANDATROPHY ANALYSIS:
 *   The tributary labor obligation system shows no mandatrophy signal. All institutional perspectives (state apparatus, hegemon, officials) classify as Rope or Tangled Rope and see the system as functional. The snare classification emerges only from powerless/trapped and identity_locked perspectives. This is NOT a misclassification (institutional actors deceiving themselves about extraction) but a true perspectival gap: the system genuinely coordinates administrative functions (courts, security, resource distribution) for beneficiaries while simultaneously extracting from victims. The mandatrophy would only appear if the state apparatus or hegemon claimed the system was pure coordination while data showed it was primarily extractive — instead, they openly justify extraction as necessary for order. The absence of mandatrophy in beneficiary perspectives actually increases the snare classification confidence: the system is not disguised as something else within institutional hierarchies; it is explicitly legitimized as extraction justified by necessity. This is the defining feature of systemic snares — they require no false consciousness among beneficiaries, only suppression of victim agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_cultural_internalization,
    'What proportion of compliance derives from coercive enforcement versus internalized cultural obligation?',
    'Longitudinal data on rebellion rates following enforcement capacity reduction; interview data on compliance motivation; historical analysis of tributary system stability following external shocks to coercive apparatus',
    'If primarily coercive (>70%): reclassify suppression as structural, snare classification stable. If primarily internalized (>50%): identity_locked mechanism is primary, suggesting mountain-like stability despite low material barriers. Affects stability predictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_cultural_internalization, empirical, 'Proportion of tributary compliance driven by coercion versus cultural internalization').

omega_variable(
    alternative_livelihood_accessibility,
    'Are alternative livelihoods genuinely unavailable or suppressed through market control and legal prohibition?',
    'Historical analysis of labor market exclusions; comparison with adjacent regions without tributary obligations; data on penalties for exit attempts; feasibility studies of subsistence alternatives',
    'If alternatives unavailable: trapped classification confirmed, suppression reflects structural scarcity. If alternatives suppressed: suppression reflects active enforcement choice, reclassifying as behavioral control rather than material constraint. Changes policy intervention points.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_livelihood_accessibility, empirical, 'Whether alternative livelihoods are structurally absent or actively suppressed').

omega_variable(
    tributary_state_legitimacy_boundary,
    'Does the tributary state apparatus genuinely coordinate public goods (security, infrastructure, dispute resolution) or is legitimacy purely theater for extraction?',
    'Historical analysis of public goods provision correlated with tributary capacity; comparison with non-tributary governance structures in same region; data on rebel-provided alternatives to state-provided services',
    'If genuine public goods: tangled_rope classification for officials and apparatus may be correct. If purely theater: reclassify as snare from apparatus perspective. Affects institutional perspective directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tributary_state_legitimacy_boundary, empirical, 'Whether tributary state apparatus provides genuine public goods coordination').

omega_variable(
    resistance_coalition_power_threshold,
    'At what scale of organized resistance does the tributary system reach critical collapse, and how many critical mass threshold victims must coordinate to trigger system failure?',
    'Historical analysis of successful anti-tributary rebellions; data on revolt coordination costs and triggers; mathematical modeling of enforcement capacity versus rebellion probability',
    'If low threshold (<10% coordinated resistance): dynamic coalition extension applies, powerless agents may upgrade to organized power in perspectives. If high threshold (>50%): snare classification from powerless perspective is stable even with coalition potential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_coalition_power_threshold, empirical, 'Critical mass threshold for organized tributary resistance').

omega_variable(
    extractiveness_temporal_stability,
    'Does the tributary labor extraction rate remain stable over time or does it show accumulation/degradation patterns suggesting systemic lifecycle dynamics?',
    'Long-run data on labor obligations across centuries; comparison of extraction rates in early vs mature tributary systems; analysis of oscillation between reform periods and re-consolidation',
    'If stable: snare classification reflects steady-state mechanism. If degrading: piton classification from reform perspective may indicate natural atrophy. If accumulating: mandatrophy toward pure snare, intensifying mechanism suggests policy intervention failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_temporal_stability, empirical, 'Long-term trajectory of tributary extraction rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tributary_labor_obligation_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trib_tr_t0, tributary_labor_obligation_system, theater_ratio, 0, 0.35).
narrative_ontology:measurement(trib_tr_t3, tributary_labor_obligation_system, theater_ratio, 3, 0.42).
narrative_ontology:measurement(trib_tr_t6, tributary_labor_obligation_system, theater_ratio, 6, 0.52).
narrative_ontology:measurement(trib_tr_t9, tributary_labor_obligation_system, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(trib_be_t0, tributary_labor_obligation_system, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(trib_be_t3, tributary_labor_obligation_system, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(trib_be_t6, tributary_labor_obligation_system, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(trib_be_t9, tributary_labor_obligation_system, base_extractiveness, 9, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tributary_labor_obligation_system, enforcement_mechanism).
narrative_ontology:affects_constraint(tributary_labor_obligation_system, peasant_identity_fusion).
narrative_ontology:affects_constraint(tributary_labor_obligation_system, imperial_legitimacy_narrative).
narrative_ontology:affects_constraint(tributary_labor_obligation_system, administrative_capacity_ceiling).

% DUAL FORMULATION NOTE:
% The tributary labor obligation system is a historical archetype with contemporary instantiations in labor trafficking, indentured servitude, and forced labor regimes. Each historical manifestation has domain-specific metrics but shares the core structure: legal obligation, suppression of alternatives, extraction flow, identity capture. This story models the general mechanism; specific instances (colonial America, medieval manor, modern trafficking) would be decomposed as separate constraint stories with domain-specific beneficiary/victim groups and measurement data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
