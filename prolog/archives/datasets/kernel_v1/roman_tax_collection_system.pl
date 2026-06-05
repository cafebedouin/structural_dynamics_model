% ============================================================================
% CONSTRAINT STORY: roman_tax_collection_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_tax_collection_system, []).

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
 *   constraint_id: roman_tax_collection_system
 *   human_readable: Roman Tax Collection System (Publicani Extraction and Coordination)
 *   domain: economic_political/ancient_governance
 *
 * SUMMARY:
 *   The Roman tax collection system represents a quintessential tangled_rope
 *   constraint that exhibits genuine coordination benefits alongside
 *   systematic asymmetric extraction. From approximately 200 BCE to 200 CE,
 *   Rome lacked the administrative infrastructure to collect taxes directly,
 *   so it contracted with private tax farmers (publicani) who bid for
 *   collection rights in specific provinces. This mechanism solved a real
 *   structural problem: funding the military and state apparatus without
 *   building a permanent bureaucracy (which would be expensive, create
 *   competing power centers, and require trained administrators Rome did not
 *   yet possess). However, the publicani system enabled contractors to
 *   extract substantially more than legitimate administrative costs, crushing
 *   provincial populations through debt bondage, property seizure, and
 *   punitive assessments. The constraint exhibits all eight DR perspectives
 *   because different agents experience radically different structural
 *   relationships to the same institutional mechanism. Provincial subjects
 *   face a snare with no exit; local elites bargain constrained participation
 *   in exchange for partial exemptions; the state experiences pure
 *   coordination (delegation of collection risk); contractors experience
 *   legitimate profit extraction; the imperial bureaucracy eventually
 *   replaces the system with direct collection; and late imperial
 *   administrators maintain the degraded ritual long after bureaucratic
 *   capacity renders it unnecessary. The measurements capture the system's
 *   lifecycle: extractiveness rising from 0.42 to 0.62 as publicani firms
 *   perfect predatory assessment, theater rising from 0.35 to 0.62 as the
 *   imperial bureaucracy builds alternative capacity and the publicani system
 *   becomes increasingly performative, and suppression rising to 0.68 as
 *   enforcement becomes the binding constraint rather than coordination
 *   innovation.
 *
 * KEY AGENTS:
 *   - Provincial populations: Primary victim (powerless/trapped) — bear the full extraction burden with no legal recourse or exit option; subject to debt bondage and property seizure
 *   - Small landowners: Secondary victim (moderate/trapped) — vulnerable to assessment manipulation; often pushed into debt servitude or loss of property
 *   - Provincial aristocracy (local elites): Co-opted agent (moderate/constrained) — participate in extraction in exchange for exemptions and status; genuinely benefit from Roman military protection and trade access
 *   - Publicani contractors: Primary beneficiary (powerful/arbitrage) — capture substantial profits through competitive bidding; 5-year contracts with clear exit
 *   - Equestrian financial class: Organized beneficiary (organized/arbitrage) — finance and manage large publicani firms; face senatorial oversight and prosecution risk for excesses
 *   - Roman state apparatus: Primary beneficiary (institutional/arbitrage) — gains revenue without building expensive bureaucracy; can renegotiate or replace contracts
 *   - Imperial administrators (Augustus and successors): Architectural agent (institutional/arbitrage) — explicitly build alternative bureaucratic capacity; scaffold constraint with known sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_tax_collection_system, 0.58).
domain_priors:suppression_score(roman_tax_collection_system, 0.65).
domain_priors:theater_ratio(roman_tax_collection_system, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_tax_collection_system, extractiveness, 0.58).
narrative_ontology:constraint_metric(roman_tax_collection_system, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(roman_tax_collection_system, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_tax_collection_system, tangled_rope).
narrative_ontology:human_readable(roman_tax_collection_system, "Roman Tax Collection System (Publicani Extraction and Coordination)").
narrative_ontology:topic_domain(roman_tax_collection_system, "economic_political/ancient_governance").

domain_priors:requires_active_enforcement(roman_tax_collection_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_tax_collection_system, publicani_contractors).
narrative_ontology:constraint_beneficiary(roman_tax_collection_system, roman_state_apparatus).
narrative_ontology:constraint_victim(roman_tax_collection_system, provincial_populations).
narrative_ontology:constraint_victim(roman_tax_collection_system, small_landowners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL SUBJECT (SNARE) — The provincial population faces mandatory tax extraction with no exit or legal recourse. The publicani are private contractors maximizing extraction within their tenure (typically 5-year contracts). Suppression is structural: military occupation, legal authority granted to contractors to seize property, debt bondage for non-payment. The subject experiences maximum extractiveness — all benefits flow to the state and contractors; costs are entirely localized.
constraint_indexing:constraint_classification(roman_tax_collection_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PROVINCIAL ARISTOCRACY (TANGLED ROPE) — Local elites are partly co-opted into the extraction mechanism: they help identify taxable assets and collect quotas in exchange for exemptions or reduced obligations. This creates genuine coordination (provincial governance functions through local elite networks) alongside asymmetric extraction (elites benefit disproportionately while shouldering less burden). Exit is constrained by status dependence on Roman military protection and imperial recognition.
constraint_indexing:constraint_classification(roman_tax_collection_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ROMAN STATE APPARATUS (ROPE) — From the state's perspective, the publicani system solves a genuine coordination problem: how to extract tax revenue without building a permanent administrative apparatus (which would be expensive and create competing power centers). The state experiences the constraint as pure coordination — the publicani mechanism transfers costs of administration to private contractors while guaranteeing revenue. Net beneficiary with full arbitrage: can terminate contracts, renegotiate terms, shift collection to different contractors or eventually to imperial bureaucrats (as happened under Augustus).
constraint_indexing:constraint_classification(roman_tax_collection_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLICANI CONTRACTOR CLASS (ROPE) — For large-scale tax farming firms (especially the societates publicanorum — corporate partnerships of contractors), the constraint is a coordination mechanism that generates enormous private wealth. Contractors bid competitively for regional collection rights, which incentivizes accurate assessment and efficient collection (within exploitative bounds). They experience the system as a legitimate profit mechanism with a sunset (5-year contracts) and clear rules. Exit is mobile — contracts expire and can be replaced with commerce or other ventures. Beneficiary with arbitrage.
constraint_indexing:constraint_classification(roman_tax_collection_system, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EQUESTRIAN FINANCIAL CLASS (ORGANIZED) — The equestrian order (knights) who finance and manage large publicani firms are organized agents with significant but constrained power. They benefit from tax farming but face constraints: senatorial oversight, threat of prosecution for excesses, price competition from rival firms, and eventual transition to imperial bureaucracy. This perspective captures the hybrid nature clearly: genuine coordination (efficient revenue extraction) with significant asymmetric extraction (provincial populations bear the cost; equestrians capture the profit). Moderately constrained exit — they can diversify into other ventures but are deeply invested in the system.
constraint_indexing:constraint_classification(roman_tax_collection_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: IMPERIAL ADMINISTRATIVE REFORM (SCAFFOLD) — From the perspective of Augustus and later emperors, the publicani system is a temporary solution with a sunset: building imperial bureaucratic capacity (the fiscus, later imperial accounting systems) gradually replaces tax farming. The constraint has low theater under imperial administration because direct state collection becomes possible as bureaucratic infrastructure scales. The scaffold is genuine: the empire is explicitly building alternative mechanisms to eventually replace publicani extraction. Sunset clause: transition to professional tax collectors (logistai, tabularii) over 200 years.
constraint_indexing:constraint_classification(roman_tax_collection_system, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: LATE IMPERIAL DEGRADATION (PITON) — By the 3rd-4th century CE, the publicani system persists as institutional inertia long after the imperial bureaucracy has proven more efficient. Tax farming continues in peripheral provinces and for specialized taxes even as direct collection becomes standard. Theater ratio is high: the contracts are maintained for legitimacy and precedent rather than functional extraction efficiency. The system is sustained by path dependence, not by structural necessity — a degraded rope that persists through institutional momentum.
constraint_indexing:constraint_classification(roman_tax_collection_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / ECONOMIC NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, tax farming appears as an immutable economic law: pre-industrial states lack administrative capacity, so they must contract with private parties to extract revenue. This perspective naturalizes the publicani system as inherent to ancient governance. However, the structural data contradicts the mountain classification — the imperial transition to direct collection demonstrates that the 'necessity' was contingent on technological and organizational capacity, not an unchangeable feature of state finance.
constraint_indexing:constraint_classification(roman_tax_collection_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_tax_collection_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_tax_collection_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_tax_collection_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_tax_collection_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_tax_collection_system, TR),
    TR >= 0.70.

:- end_tests(roman_tax_collection_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. The publicani system extracted substantially beyond legitimate administration costs — historical evidence suggests 20-40% of assessed value went to contractors as profit beyond actual collection expenses. The value reflects that this is real extraction, not hypothetical, but also that the system served genuine state coordination functions that legitimated some overhead. Measurement trajectory shows rise to 0.62 during peak Imperial period (100-200 CE) as firms perfected predatory assessment techniques, then decline to 0.48 by 300 CE as imperial bureaucracy reduced reliance on private contractors. Suppression (0.65): High and structural. Enforcement mechanisms included military occupation, legal authority to seize property, debt bondage, and denial of legal recourse to appeal assessments. The provincials faced combined material barriers (no escape from territory) and legal barriers (no court recognized their claims against publicani). Suppression is asymmetric: elites faced lower suppression (could negotiate) while peasants faced nearly complete suppression (no alternatives). Theater ratio (0.48): Moderate, rising over time to 0.62. The system's early phase (0-100 CE) had genuinely functional coordination — publicani firms actually did solve the revenue problem efficiently. As imperial bureaucratic capacity grew (100-300 CE), the theater increased — the publicani system persisted as path dependence even as direct collection became more efficient. By 300 CE, the theater had risen substantially, indicating institutional inertia. The rise in theater mirrors the decline in extractiveness, suggesting the system shifted from functional extraction to degraded ritual (piton trajectory if extended beyond the imperial period).
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap separates the provincial victim (snare) from the state/contractor beneficiaries (rope). This gap is not attributable to disagreement about facts but to structural relationship: the victim bears costs the beneficiary does not. A secondary gap separates the imperial administrator's scaffold (system has sunset based on bureaucratic progress) from the late imperial observer's piton (system persists as ritual despite capacity to replace it). This gap reveals path dependence: the scaffold's actual sunset was delayed by 200+ years beyond when it technically became possible, suggesting institutional inertia or entrenched beneficiary interests prevented transition.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position. Provincial subjects are pure victims (trapped exit) with zero beneficiary status → d = 0.95, producing maximum f(d) and maximum experienced extraction (chi). Provincial elites are mixed: they benefit from status and protection but also bear some extraction burden and face constrained exit → d ≈ 0.60, producing moderate chi through f(d) sigmoid. The state is pure beneficiary (arbitrage exit) with no extraction burden → d ≈ 0.05, producing negative f(d) and effective subsidy relationship. Publicani contractors are beneficiaries (arbitrage exit) with upside-only structure → d ≈ 0.15, producing low/negative chi. The equestrian class occupies organized power with constrained exit and mixed beneficiary status (they profit but face oversight) → d ≈ 0.50, producing moderate chi. The imperial administrator is beneficiary with arbitrage (can transition system at will) → d ≈ 0.10. The analytical observer at civilizational scope risks naturalizing the constraint; if reframed as a false summit, d → institutional beneficiary of the naturalization narrative, not a neutral observer.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandatrophy through multiple mechanisms. First, the genuine coordination function (revenue extraction without permanent bureaucracy) is structurally distinct from asymmetric extraction — both are real and coexist. The tangled_rope classification captures this hybrid correctly. Second, the perspectival variation is not mandatrophy but legitimate structural difference: different agents occupy different positions (victim/beneficiary) and therefore experience different classification types. The snare perspective (victims) is not in conflict with the rope perspective (beneficiaries) — they are describing the same constraint from opposite positions. Third, the temporal trajectory (extractiveness rising, theater rising, suppression rising, then transition to alternative system) demonstrates the constraint operated as designed: solve coordination while enabling extraction, until the coordination function was superseded by bureaucratic capacity. The system's degradation into a piton (late imperial period) confirms the mandatrophy is resolved: the constraint became dysfunctional (theater > functional extraction), which prompted replacement by direct collection. The classical mandate trap (appears as both pure extraction and pure coordination depending on observer) is broken by recognizing that both are correct simultaneously — the constraint coordinated state revenue-raising while enabling extraction. The question was never 'which one is the constraint really?' but 'who benefits and who bears costs?' Once directionality is specified, classification becomes unambiguous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_magnitude_threshold,
    'What proportion of assessed tax wealth did the publicani system extract beyond legitimate administration costs?',
    'Historical reconstruction of tax quotas vs. actual collection amounts; comparison with later imperial direct taxation rates; analysis of provincial economic contraction during high publicani periods vs. lower rates under imperial bureaucratic collection',
    'If extraction >40% of assessed value: classification as snare for provincial victims is robust. If extraction <20%: system approaches rope classification (legitimate revenue mechanism with modest overhead). Determines whether suppression gates are breached.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_magnitude_threshold, empirical, 'Magnitude of extraction beyond administrative costs').

omega_variable(
    voluntary_participation_ambiguity,
    'Did provincial populations have any genuine exit option (migration, tax resistance, legal recourse) or was exit completely foreclosed?',
    'Historical evidence of successful tax resistance movements; patterns of forced migration or refuge-seeking; analysis of legal appeals mechanisms and their effectiveness; comparative data on voluntary imperial taxation in non-conquered territories',
    'If exit completely foreclosed: classification as trapped/snare is structural. If minimal exit available (voluntary debt servitude, migration to less-taxed regions): classification shifts toward constrained (higher d, lower experienced chi). Determines baseline suppression value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_ambiguity, empirical, 'Whether provincial exit from taxation was fully foreclosed or partially available').

omega_variable(
    coordination_function_reality,
    'Did the publicani system provide genuine coordination benefits (stable revenue, predictable administration) or was coordination merely the legitimizing narrative for pure extraction?',
    'Analysis of revenue stability under publicani vs. imperial bureaucratic collection; comparison of administrative efficiency metrics; examination of whether alternative revenue mechanisms (direct state collection, tribute-in-kind, fixed annual levies) would have been feasible; evidence of state preference continuation even after bureaucratic capacity was available',
    'If genuine coordination: tangled_rope classification is robust across perspectives. If coordination is post-hoc narrative: system reclassifies as pure snare from most perspectives; public-private partnership framing naturalizes extraction. Determines whether beneficiary + victim gate passes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_reality, conceptual, 'Whether coordination function is genuine or post-hoc legitimation').

omega_variable(
    imperial_transition_causality,
    'Did the shift from publicani to imperial bureaucratic collection occur because direct administration became more efficient, or because the publicani system was generating destabilizing social resistance and political instability?',
    'Chronological analysis of administrative capacity building; evidence of provincial revolts and anti-publicani movements; comparison of transition timing across provinces; analysis of imperial rhetoric justifying the transition',
    'If efficiency-driven: scaffold perspective is structural — the system had a genuine sunset based on technological/organizational progress. If resistance-driven: sunset was forced by accumulated extraction damage; system''s apparent sustainability masks precarious legitimacy. Affects both scaffold validity and mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_transition_causality, empirical, 'Whether imperial transition to bureaucratic collection was efficiency-driven or resistance-driven').

omega_variable(
    provincial_elite_collaboration_nature,
    'Did local elites genuinely consent to co-participation in extraction (trading enforcer role for exemptions), or were they coerced into collaboration under threat of military occupation?',
    'Evidence of elite bargaining power and negotiated exemptions; analysis of elite incentives (military protection, trade access, prestige); comparison with cases of elite resistance and resulting retaliation; examination of legal frameworks governing elite obligations',
    'If genuine bargaining: tangled_rope at the elite level is accurate — coordination + constrained extraction. If coerced collaboration: classification shifts to snare from elite perspective as well; suppression value increases. Determines whether the constraint''s apparent coordination is bottom-up or enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_elite_collaboration_nature, empirical, 'Whether elite collaboration in tax extraction was negotiated or coerced').

omega_variable(
    small_landowner_exclusion_mechanism,
    'Were small landowners systematically excluded from the tax base (through exemptions, administrative incapacity, or collusion with local elites), or did they bear proportional burden?',
    'Analysis of tax rolls and cadastral records; comparison of assessed value vs. actual taxable capacity across wealth classes; evidence of exemption patterns by social status; reconstruction of effective tax rates by landowner size',
    'If systematically excluded: snare operates as regressively as base metrics suggest — the poorest bear maximum suppression while the wealthy are partly protected. If proportional: extraction is harsh but less targeted; suppression value may be lower due to less precise enforcement apparatus. Affects directionality derivation for small landowner victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_landowner_exclusion_mechanism, empirical, 'Whether small landowners were systematically excluded or bore proportional tax burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_tax_collection_system, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rtcs_tr_t0, roman_tax_collection_system, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rtcs_tr_t100, roman_tax_collection_system, theater_ratio, 100, 0.48).
narrative_ontology:measurement(rtcs_tr_t200, roman_tax_collection_system, theater_ratio, 200, 0.55).
narrative_ontology:measurement(rtcs_tr_t300, roman_tax_collection_system, theater_ratio, 300, 0.62).

% Extraction over time
narrative_ontology:measurement(rtcs_be_t0, roman_tax_collection_system, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rtcs_be_t100, roman_tax_collection_system, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(rtcs_be_t200, roman_tax_collection_system, base_extractiveness, 200, 0.62).
narrative_ontology:measurement(rtcs_be_t300, roman_tax_collection_system, base_extractiveness, 300, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(rtcs_su_t0, roman_tax_collection_system, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(rtcs_su_t100, roman_tax_collection_system, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(rtcs_su_t200, roman_tax_collection_system, suppression_requirement, 200, 0.68).
narrative_ontology:measurement(rtcs_su_t300, roman_tax_collection_system, suppression_requirement, 300, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_tax_collection_system, resource_allocation).
narrative_ontology:affects_constraint(roman_tax_collection_system, provincial_resistance_dynamics).
narrative_ontology:affects_constraint(roman_tax_collection_system, elite_collaboration_mechanisms).
narrative_ontology:affects_constraint(roman_tax_collection_system, imperial_bureaucratic_capacity).

% DUAL FORMULATION NOTE:
% The roman_tax_collection_system is the upstream institutional mechanism that generates extraction visible in downstream constraints. provincial_resistance_dynamics shows the victim perspective's agency. elite_collaboration_mechanisms shows the co-opted agent perspective's constrained bargaining. imperial_bureaucratic_capacity shows the architectural replacement dynamic. All three are affected by changes in this constraint's extractiveness and suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_tax_collection_system, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
