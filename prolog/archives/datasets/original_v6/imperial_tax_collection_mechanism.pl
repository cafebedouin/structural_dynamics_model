% ============================================================================
% CONSTRAINT STORY: imperial_tax_collection_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_tax_collection_mechanism, []).

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
 *   constraint_id: imperial_tax_collection_mechanism
 *   human_readable: Imperial Tax Collection Mechanism
 *   domain: political_economy/state_extraction
 *
 * SUMMARY:
 *   The imperial tax collection mechanism represents a foundational
 *   extraction constraint in state formation. Operating across subject
 *   populations through land tenure systems, conscription obligations, and
 *   corvée labor requirements, it creates a structural separation between
 *   those who benefit from aggregated resources (imperial treasury, military
 *   apparatus, central bureaucracy) and those who bear the cost (peasants,
 *   merchants, local communities). The constraint exhibits the full range of
 *   DR classifications depending on perspective: the subject peasant trapped
 *   in the system sees pure extraction (Snare); the merchant class sees mixed
 *   coordination and extraction (Tangled Rope); the imperial treasury
 *   experiences it as pure coordination (Rope); the local tax assessor
 *   operates within an increasingly performative ritual (Piton);
 *   administrative reformers see a temporary problem solvable through
 *   bureaucratic rationalization (Scaffold); the civilizational observer
 *   risks naturalizing contingent institutional arrangements as inevitable
 *   (Mountain). The mechanism's extractiveness has increased over the
 *   measurement interval as imperial demands expand and local capacity
 *   becomes saturated, while the theater ratio has increased as formal
 *   assessment procedures become more elaborate relative to their actual role
 *   in determining collections.
 *
 * KEY AGENTS:
 *   - Subject Peasantry: Primary victim (powerless/trapped) — bears extraction via land taxation, corvée labor, and conscription; lacks exit options due to land tenure and legal disability
 *   - Merchant Classes: Secondary victim (moderate/constrained) — constrained by monopoly grants and licensing; face extraction via customs and arbitrary assessments; also benefit from imperial peace
 *   - Imperial Treasury and Central State: Primary beneficiary (institutional/arbitrage) — captures tax surplus for military, bureaucracy, and elite consumption; experiences taxation as pure coordination
 *   - Local Tax Assessors and Collectors: Institutional actor (institutional/constrained) — trapped between imperial quotas and local capacity; maintain increasingly performative assessment rituals
 *   - Administrative Reformers: Organized agents (organized/constrained) — see tax collection as improvable through standardization and cadastral surveys; building exit pathways via bureaucratic rationalization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating contingent institutional arrangements as natural laws of statecraft
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_tax_collection_mechanism, 0.68).
domain_priors:suppression_score(imperial_tax_collection_mechanism, 0.72).
domain_priors:theater_ratio(imperial_tax_collection_mechanism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_tax_collection_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_tax_collection_mechanism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imperial_tax_collection_mechanism, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_tax_collection_mechanism, snare).
narrative_ontology:human_readable(imperial_tax_collection_mechanism, "Imperial Tax Collection Mechanism").
narrative_ontology:topic_domain(imperial_tax_collection_mechanism, "political_economy/state_extraction").

domain_priors:requires_active_enforcement(imperial_tax_collection_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_tax_collection_mechanism, imperial_treasury).
narrative_ontology:constraint_beneficiary(imperial_tax_collection_mechanism, imperial_military).
narrative_ontology:constraint_beneficiary(imperial_tax_collection_mechanism, imperial_bureaucracy).
narrative_ontology:constraint_victim(imperial_tax_collection_mechanism, subject_peasantry).
narrative_ontology:constraint_victim(imperial_tax_collection_mechanism, merchant_classes).
narrative_ontology:constraint_victim(imperial_tax_collection_mechanism, local_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT PEASANT (SNARE) — Trapped by land tenure, legal disability, and geographic immobility. Bears extraction via corvée labor obligations, in-kind taxation, and confiscatory conscription. No exit option; maximum experienced extraction. The peasant's labor surplus is directly captured by the imperial machinery with minimal coordination benefit.
constraint_indexing:constraint_classification(imperial_tax_collection_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MERCHANT CLASS (TANGLED ROPE) — Constrained by monopoly grants and licensing requirements, but also benefits from imperial peace, standardized weights/measures, and infrastructure. Faces high extraction via customs duties and arbitrary assessments, but genuine coordination function exists (market stability, contract enforcement). Exit requires abandoning commercial networks built over generations.
constraint_indexing:constraint_classification(imperial_tax_collection_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IMPERIAL TREASURY (ROPE) — Primary beneficiary. Experiences tax collection as a pure coordination mechanism: aggregating dispersed resources into centralized stores. Zero experienced extraction — benefits flow entirely to this agent. Can arbitrage by redirecting collection mechanisms or adjusting tax rates.
constraint_indexing:constraint_classification(imperial_tax_collection_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: LOCAL TAX ASSESSOR (PITON) — Caught between imperial quota demands and local capacity. The assessment ritual (inspection tours, negotiation councils, written declarations) has become substantially performative — maintaining the fiction of accurate valuations while actual collections rest on power relations. Theater ratio high because formal procedures mask coercive extraction. Assessor is trapped by institutional inertia, not by functional necessity.
constraint_indexing:constraint_classification(imperial_tax_collection_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ADMINISTRATIVE REFORM MOVEMENT (SCAFFOLD) — Organized agents (reform edicts, cadastral surveys, centralized accounting) see tax extraction as a temporary coordination failure solvable through bureaucratic rationalization. The sunset clause operates via tax farming replacement or standardized assessment — moving from coercive extraction to transparent, predictable collection. Organized agents have agency and a visible exit path.
constraint_indexing:constraint_classification(imperial_tax_collection_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some taxation is logically necessary for statecraft: armies require funding, bureaucracies require resources, infrastructure requires maintenance. From this frame, imperial tax collection appears as an immutable law of political organization. However, the structural data contradicts this — the extractiveness and suppression values are contingent on specific institutional arrangements (land tenure, conscription, corvée), not inherent to taxation itself. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(imperial_tax_collection_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_tax_collection_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imperial_tax_collection_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imperial_tax_collection_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_tax_collection_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(imperial_tax_collection_mechanism, TR),
    TR >= 0.70.

:- end_tests(imperial_tax_collection_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The imperial treasury captures a substantial and growing portion of subject productive surplus via multiple mechanisms (land tax, corvée, conscription, customs duties, monopoly levies). The measurement trajectory shows increasing extractiveness as imperial demands expand and the constraint becomes more comprehensive. Initial value (0.45) reflects early-stage empire where collection mechanisms were less developed; terminal value (0.68) reflects mature empire where extraction is systematized and comprehensive. Suppression (0.72): High. Multiple structural barriers prevent exit: land tenure systems lock peasants to specific territories; legal disability prevents merchant competition; conscription is compulsory; corvée obligations are enforceable by military force. Escape routes (underground economy, banditry, geographic migration) are costly and risky. Theater ratio (0.55): Moderate and increasing. Early imperial tax collection relied on direct coercion and negotiation; as the system matured, elaborate assessment procedures (tax commissions, valuations, appeals processes) developed. These procedures create the appearance of systematic, legible taxation while actual collections rest primarily on power relations and coercive capacity. Increasing theater ratio reflects the growing gap between formal assessment processes and the extraction mechanism's actual operation.
 *
 * PERSPECTIVAL GAP:
 *   The gap between peasant/merchant experience (Snare/Tangled Rope) and imperial treasury experience (Rope) is maximal, revealing the constraint's purely extractive character. The reformer's Scaffold perspective offers a genuine exit path through administrative rationalization, but the current mechanism's classification as Snare from the powerless perspective is structurally confirmed. The piton classification of the assessor reveals institutional degradation — the formal procedures mask rather than enable collection. The false mountain from the civilizational observer reveals the risk of naturalizing oppressive institutional arrangements as inevitable features of statecraft.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value (d) for each agent is derived from their structural position relative to extraction flows. The peasant faces d ≈ 0.95 (trapped victim) producing maximum f(d) ≈ 1.42 effective extraction multiplier. The merchant faces d ≈ 0.65 (constrained victim with some coordination benefits) producing f(d) ≈ 1.00. The imperial treasury faces d ≈ 0.05 (institutional beneficiary with arbitrage options) producing f(d) ≈ -0.12 (negative effective extraction — benefits flow to this agent). The local assessor faces d ≈ 0.55 (institutional actor with constrained exit) producing f(d) ≈ 0.75. These directionality values flow from the beneficiary/victim declarations: the treasury is the primary beneficiary (d low); peasants and merchants are primary victims (d high); the assessor occupies an intermediate institutional position with genuine constraints despite moderate power level.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CONFIRMATION: The imperial tax collection mechanism satisfies all snare gates. Extractiveness (0.68) ≥ 0.46. Suppression (0.72) ≥ 0.60. The primary perspective (subject peasant: powerless/trapped) classifies as Snare with maximum experienced extraction. No coordination function serves the primary victim — the extraction is asymmetric and irreducible. The constraint exists because it serves extraction, not because it solves a coordination problem that could not be solved otherwise. The apparent public goods (roads, military, administration) primarily serve the imperial center; they are secondary to the primary function of extraction. Mandatrophy is resolved by demonstrating that the snare classification is not mislabeling a coordination mechanism but correctly identifying pure extraction wrapped in institutional procedures. The theater ratio (increasing from 0.35 to 0.55) reflects the growing gap between formal legibility (assessment procedures, cadastral records, written law) and actual mechanism (coercive extraction). High theater in a snare context indicates degradation — the formal procedures are becoming increasingly performative as the constraint matures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_necessity_vs_extractive_surplus,
    'What portion of imperial tax extraction is functionally necessary for legitimate state activity (defense, infrastructure, administration) versus extractive surplus captured by the imperial center and elites?',
    'Comparative analysis of tax rates across empires with varying defensive and infrastructural burdens; measurement of actual expenditure on public goods vs. imperial/elite consumption; examination of tax revenue levels required for functional statecraft vs. actual collection rates',
    'If functional necessity dominates (< 40% surplus): constraint might reclassify as Tangled Rope from powerless perspective. If surplus dominates (> 60%): classification as Snare is confirmed; suppression might increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_necessity_vs_extractive_surplus, empirical, 'Functional necessity versus extractive surplus ratio').

omega_variable(
    local_elite_complicity,
    'To what extent do local elites (landed gentry, merchant princes, village headmen) benefit from imperial tax extraction and thus become co-enforcers rather than fellow victims?',
    'Historical record of tax collector privileges, kickback arrangements, and local elite participation in assessment; correlation between local elite enrichment and imperial tax burden on peasantry; analysis of whether tax collection mechanisms distribute extraction rewards to local enforcers',
    'If local elite capture is significant: the constraint is more properly modeled as a network of nested extractions (local elite extracting on behalf of imperial center), requiring decomposition into separate constraint stories. If minimal: the clear beneficiary/victim distinction holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_elite_complicity, empirical, 'Degree of local elite complicity and benefit-sharing in tax extraction').

omega_variable(
    resistance_and_underground_economy_escape,
    'How much of the economic surplus escapes formal taxation through underground markets, barter, smuggling, and undeclared production? What portion of the subject population effectively exits the constraint?',
    'Demographic and economic analysis of reported vs actual productive capacity; reconstruction of underground market scale from period records (smuggling records, bandit activity, black market prices); comparison of formal tax base to estimated total wealth',
    'If escape is substantial (> 50%): effective suppression is lower than claimed (many agents have partial exit), and experienced extractiveness for some victims is lower. If escape is minimal (< 20%): suppression and extractiveness values are confirmed; trapping is near-total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_and_underground_economy_escape, empirical, 'Scale of underground economy and tax avoidance escape routes').

omega_variable(
    public_goods_provision_sufficiency,
    'Do the public goods provided by imperial taxation (road networks, postal system, military protection, legal framework) actually benefit the subject population sufficiently to constitute a legitimate coordination function, or are they primarily infrastructure for continued extraction?',
    'Reconstruction of public goods distribution across regions and social classes; analysis of whether infrastructure (roads, ports) primarily serves imperial interests or enables broader economic activity; measurement of security provision and banditry rates; comparison with alternative governance systems of similar scale',
    'If public goods are broadly beneficial: more perspectives would classify as Rope or Tangled Rope rather than Snare. If concentrated on elite infrastructure: Snare classification is reinforced; the constraint is pure extraction disguised as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_goods_provision_sufficiency, empirical, 'Whether public goods provision justifies extraction claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_tax_collection_mechanism, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imperial_tax_tr_t0, imperial_tax_collection_mechanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(imperial_tax_tr_t25, imperial_tax_collection_mechanism, theater_ratio, 25, 0.45).
narrative_ontology:measurement(imperial_tax_tr_t50, imperial_tax_collection_mechanism, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(imperial_tax_be_t0, imperial_tax_collection_mechanism, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(imperial_tax_be_t25, imperial_tax_collection_mechanism, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(imperial_tax_be_t50, imperial_tax_collection_mechanism, base_extractiveness, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_tax_collection_mechanism, resource_allocation).
narrative_ontology:affects_constraint(imperial_tax_collection_mechanism, imperial_bureaucratic_expansion).
narrative_ontology:affects_constraint(imperial_tax_collection_mechanism, peasant_servitude_lock_in).
narrative_ontology:affects_constraint(imperial_tax_collection_mechanism, merchant_monopoly_gatekeeping).

% DUAL FORMULATION NOTE:
% The imperial tax collection mechanism is upstream of several institutional constraints that it enables and reinforces: bureaucratic expansion requires funding (captured by taxation); peasant servitude is locked in by land tenure systems that also enable tax collection; merchant monopolies are created and maintained through imperial licensing requirements that also generate customs revenue. Each downstream constraint has its own extractiveness value but depends structurally on the tax mechanism's existence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_tax_collection_mechanism, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
