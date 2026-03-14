% ============================================================================
% CONSTRAINT STORY: wto_institutional_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_institutional_degradation, []).

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
 *   constraint_id: wto_institutional_degradation
 *   human_readable: WTO Institutional Degradation and Rule-Based Extraction
 *   domain: international_trade/institutional_governance
 *
 * SUMMARY:
 *   The WTO institutional degradation constraint tracks the progressive decay
 *   of the multilateral rules-based trade system from its functional peak
 *   (1995-2010) through institutional paralysis (2016-present). The Appellate
 *   Body's de facto non-functionality — resulting from systematic blocking of
 *   new judge appointments by major trading states — created a dispute
 *   resolution crisis where WTO rules cannot be authoritatively enforced.
 *   This constraint exhibits the classic Piton pattern: the WTO maintains
 *   institutional form (regular meetings, committee work, monitoring
 *   functions) while core enforcement mechanisms are inoperative.
 *   Simultaneously, dominant states benefit from this dysfunction: they can
 *   maintain agricultural subsidies, industrial policy, and intellectual
 *   property regimes that would not survive Appellate Body review, while
 *   smaller economies are locked into ineffective multilateral mechanisms
 *   they cannot exit. The rising theater ratio (0.35 → 0.74 over 30 years)
 *   reflects increasing performativity: WTO activities continue (reports,
 *   negotiations, dispute filings) but with declining actual dispute
 *   resolution. Regional trade agreements create scaffold-like alternatives
 *   with sunset logic — they fill the WTO's coordination function gap while
 *   the organization remains formally pre-eminent.
 *
 * KEY AGENTS:
 *   - Developing Economies: Primary victims (powerless/trapped) — cannot exit WTO membership; cannot access functioning dispute mechanisms; face locked-in negotiating positions under defunct rules
 *   - Mid-Tier Trading States: Secondary victims (moderate/constrained) — benefit from tariff coordination but constrained by inability to challenge dominant-state policies; lack resources for bilateral negotiations
 *   - Economically Dominant States (US, EU, China): Primary beneficiaries (institutional/arbitrage) — maintain preferential access to rule exceptions while blocking rule enforcement against their subsidies and policies
 *   - WTO Secretariat: Institutional actor (institutional/arbitrage) — maintains bureaucratic form through procedural work; benefits from continued membership fees and relevance despite declining functional authority
 *   - Regional Trade Coalitions (RCEP, CPTPP, AfCFTA): Organized agents (organized/mobile) — build alternative dispute mechanisms outside WTO framework; have exit options and functional substitutes for WTO coordination
 *   - Corporate Exporter Networks: Organized beneficiaries (organized/constrained) — leverage institutional paralysis to maintain preferential bilateral arrangements while coordination benefits remain in place
 *   - Multilateral Trade Rule System: Abstract victim (powerless/trapped) — cannot organize or advocate; bears cost through degraded legitimacy and reduced compliance with trade rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_institutional_degradation, 0.58).
domain_priors:suppression_score(wto_institutional_degradation, 0.62).
domain_priors:theater_ratio(wto_institutional_degradation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_institutional_degradation, extractiveness, 0.58).
narrative_ontology:constraint_metric(wto_institutional_degradation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(wto_institutional_degradation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_institutional_degradation, tangled_rope).
narrative_ontology:human_readable(wto_institutional_degradation, "WTO Institutional Degradation and Rule-Based Extraction").
narrative_ontology:topic_domain(wto_institutional_degradation, "international_trade/institutional_governance").

domain_priors:requires_active_enforcement(wto_institutional_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_institutional_degradation, economically_dominant_states).
narrative_ontology:constraint_beneficiary(wto_institutional_degradation, corporate_exporters).
narrative_ontology:constraint_victim(wto_institutional_degradation, developing_economies).
narrative_ontology:constraint_victim(wto_institutional_degradation, small_open_economies).
narrative_ontology:constraint_victim(wto_institutional_degradation, multilateral_trade_rule_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING ECONOMY (SNARE) — Trapped within WTO dispute resolution mechanisms that are gridlocked by Appellate Body dysfunction. Cannot exit (alternative trade systems require membership leverage); cannot reform (reform requires consensus of dominant states). Bears full cost of extraction through inability to challenge subsidy regimes in agriculture and manufacturing.
constraint_indexing:constraint_classification(wto_institutional_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER TRADING STATE (TANGLED ROPE) — Constrained by dependence on rules-based access to major markets but benefits from WTO membership's coordination function for reducing tariff barriers. Experiences mixed costs and benefits: coordination benefits (most-favored-nation treatment) coupled with extraction (unable to challenge dominant-state subsidies due to institutional paralysis).
constraint_indexing:constraint_classification(wto_institutional_degradation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ECONOMICALLY DOMINANT STATE (ROPE) — Benefits from institutional coordination: WTO's rules legitimize their preferred trade arrangements while Appellate Body dysfunction prevents challenges to their subsidy regimes. Experiences the constraint as functioning coordination that protects their strategic industries.
constraint_indexing:constraint_classification(wto_institutional_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WTO SECRETARIAT (PITON) — Maintains the institutional form and procedural theater of the organization despite fundamental dysfunction. Conducts reports, monitors compliance, and facilitates negotiations that produce little substantive reform. The organization persists through bureaucratic inertia and member-state reluctance to formally abandon multilateralism, not through functional rule enforcement.
constraint_indexing:constraint_classification(wto_institutional_degradation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL TRADE COALITION (SCAFFOLD) — Organized states (RCEP, CPTPP, AfCFTA signatories) building alternative coordination mechanisms outside the WTO framework. These platforms have sunset logic: they serve as functional substitutes for WTO dispute resolution and deeper integration until the WTO either reforms or becomes fully marginalized. Extraction is low because coalition members can exit to alternative networks.
constraint_indexing:constraint_classification(wto_institutional_degradation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: CORPORATE EXPORTER NETWORK (TANGLED ROPE) — Organized corporate actors benefit from WTO's tariff-reduction coordination but also extract value from the institutional paralysis: they exploit gaps in rule enforcement and maintain preferential market access through bilateral negotiations while formal WTO rules remain unenforced. Constrained by dependence on stable market access but strategically benefit from institutional weakness.
constraint_indexing:constraint_classification(wto_institutional_degradation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the constraint appears immutable: institutional decay is an inherent property of international consensus-based governance when enforcement mechanisms depend on member-state compliance and no supranational authority exists. The observer risks naturalizing what is actually a contingent institutional design choice — consensus-based decision-making without independent enforcement is a structural choice, not a law of nature.
constraint_indexing:constraint_classification(wto_institutional_degradation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_institutional_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wto_institutional_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wto_institutional_degradation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_institutional_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wto_institutional_degradation, TR),
    TR >= 0.70.

:- end_tests(wto_institutional_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through dual mechanisms: (1) dominant states avoid enforcement of rules against their subsidies and policies through Appellate Body blockade, creating de facto exemptions worth billions in maintained protections; (2) developing economies cannot effectively use dispute mechanisms, reducing their negotiating leverage in bilateral agreements. The extractiveness is not maximal (0.70+) because some coordination function persists — tariff bindings and MFN treatment remain partly operational, and the WTO still coordinates around agricultural trade and services. Suppression (0.62): Moderate-high. Developing economies face substantial barriers to exit (dependence on formal rules-based access to major markets; costs of bilateral renegotiation; lack of alternative platforms with equivalent coverage). Suppression is not total because emerging regional systems (RCEP, CPTPP, AfCFTA) provide alternative pathways, though with higher barriers to entry. Theater ratio (0.68): High and rising. The WTO increasingly conducts performative activities (dispute filings that cannot be adjudicated; negotiations that produce non-binding texts; compliance monitoring with weak enforcement) while core function (binding dispute resolution) remains broken. The rise from 0.35 to 0.74 over the interval reflects that the organization has become increasingly ritualistic — the form persists but the functional substance has degraded.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. The dominant state sees the WTO as successfully coordinating tariff reduction and market access (Rope) — the Appellate Body's non-functionality is either invisible to their analysis or strategically useful (maintains policy space). The developing economy sees it as inescapable extraction (Snare) — rules that cannot be enforced protect dominant-state subsidies while constraining their own policy space. The WTO Secretariat sees its own degradation (Piton) — maintaining bureaucratic form despite lost functional authority. The regional coalition sees a temporary window of opportunity (Scaffold) — building alternative systems while the WTO remains stuck. The corporate exporter sees mixed benefits (Tangled Rope) — coordination advantages coupled with opportunities to extract through bilateral arrangements outside the multilateral framework. The analytical observer risks a naturalizing false summit (Mountain) — treating rules-based system decay as inherent to international governance, when it reflects specific institutional design choices (consensus-based decision-making, no independent enforcement authority).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural relationship to the extraction flow. Dominant states occupy the beneficiary position (low d): they experience the constraint as enabling (exemptions from enforcement, policy space maintained) and can arbitrage into alternative arrangements (bilateral negotiation, domestic coalitions, regional blocs). Developing economies occupy the victim position (high d): they face material barriers to exit and cannot avoid the constraint's extraction cost (locked into ineffective dispute mechanisms, reduced leverage in bilateral negotiations). The WTO Secretariat occupies an institutional beneficiary position (low d): institutional inertia and continued member funding sustain the organization despite reduced functional authority. Regional coalitions occupy a constrained beneficiary position (moderate d): they benefit from alternative dispute mechanisms and have greater exit options than WTO members, but remain embedded in a fragmented global trade system.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through temporal decomposition: the constraint is a Tangled Rope from the perspective of states that benefit from both coordination (tariff reduction) and extraction (blocked enforcement against their policies), but it has degraded toward Snare/Piton over time as the enforcement mechanism became inoperative. The mandatrophy question ('is this coordination or extraction?') has a temporal answer: it started as Tangled Rope (genuine coordination coupled with extractive exemptions for dominant states) and is degrading toward Piton (form persists through inertia; function is lost). The constraint resolves mandatrophy by showing that classification is not static — an institution can shift types as its functional mechanisms degrade. The rising theater ratio documents this shift: as actual dispute resolution declined (theater rose), the effective classification shifted away from 'coordination with extraction' toward 'inertial maintenance of a broken form.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    appellate_body_reform_feasibility,
    'Can the WTO Appellate Body be reformed through consensus, or has institutional decay passed a point of irreversible paralysis?',
    'Temporal analysis of reform proposals and blocking coalitions; assessment of whether alternative dispute resolution mechanisms (Regional Comprehensive Economic Partnership arbitration, bilateral investor-state dispute) have created path-dependent lock-out of WTO reform.',
    'If reform is feasible: constraint reclassifies toward Scaffold (temporary institutional failure with sunset). If irreversible: constraint reclassifies toward Piton (permanently degraded but maintained through inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_reform_feasibility, empirical, 'Whether WTO Appellate Body reform remains feasible').

omega_variable(
    dominant_state_extraction_intentionality,
    'Is institutional degradation an unintended consequence of unilateral defection, or a strategically deliberate blocking mechanism by dominant states seeking to preserve policy space?',
    'Documentary evidence from negotiation records; analysis of correlation between dominant-state interests (agricultural subsidies, industrial policy space, intellectual property regimes) and positions on Appellate Body reform; pattern analysis of which states block appointments.',
    'If intentional: constraint reclassifies as pure Snare from developing economies'' perspective (deliberate suppression). If unintended consequence: reclassifies as degraded Tangled Rope (institutional coordination failure, not designed extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominant_state_extraction_intentionality, empirical, 'Whether institutional degradation is strategic or consequential').

omega_variable(
    alternative_system_substitutability,
    'Do emerging regional trade arrangements (RCEP, CPTPP, bilateral investor-state systems) provide functionally equivalent dispute resolution and rule enforcement, or do they fragment the global rules-based system and increase extraction for excluded developing economies?',
    'Comparative analysis of dispute resolution timelines, enforcement rates, and outcomes across WTO, RCEP, CPTPP, and bilateral mechanisms; mapping of which economies have access to which systems; measurement of rule compliance across trade blocs.',
    'If substitutable: regional systems reduce overall extraction (developing economies gain access to functioning dispute mechanisms through alternative platforms). If fragmentary: extraction increases (developing economies face divided, competing rule regimes with different enforcement mechanisms).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_system_substitutability, empirical, 'Whether alternative systems substitute or fragment global rules').

omega_variable(
    subsidy_enforcement_mechanism_sufficiency,
    'Even if the Appellate Body functioned perfectly, would the WTO''s subsidy rules (GATT Article XVI, SCM Agreement) provide sufficient enforcement mechanism against state industrial policies and agricultural supports, or are the rules themselves inadequate to constrain dominant-state extraction?',
    'Comparative legal analysis of subsidy definitions and exemptions across trade regimes; empirical measurement of major-power subsidies that would survive challenge even under functioning dispute mechanism; assessment of whether rule sufficiency is the primary constraint.',
    'If rules are sufficient: reform of Appellate Body would substantially restore constraint function (Tangled Rope classification holds, reform is feasible). If rules are inadequate: Appellate Body reform is necessary but not sufficient (constraint reclassifies toward permanent Snare for developing economies regardless of institutional fix).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_enforcement_mechanism_sufficiency, empirical, 'Whether WTO subsidy rules provide sufficient enforcement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_institutional_degradation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_tr_t0, wto_institutional_degradation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wto_tr_t12, wto_institutional_degradation, theater_ratio, 12, 0.52).
narrative_ontology:measurement(wto_tr_t24, wto_institutional_degradation, theater_ratio, 24, 0.68).
narrative_ontology:measurement(wto_tr_t30, wto_institutional_degradation, theater_ratio, 30, 0.74).

% Extraction over time
narrative_ontology:measurement(wto_be_t0, wto_institutional_degradation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(wto_be_t12, wto_institutional_degradation, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(wto_be_t24, wto_institutional_degradation, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(wto_be_t30, wto_institutional_degradation, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_institutional_degradation, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_institutional_degradation, agricultural_subsidy_regime).
narrative_ontology:affects_constraint(wto_institutional_degradation, intellectual_property_enforcement).
narrative_ontology:affects_constraint(wto_institutional_degradation, regional_trade_fragmentation).

% DUAL FORMULATION NOTE:
% WTO institutional degradation is upstream of specific trade policy constraints (agricultural subsidies, IP enforcement regimes). The WTO's Appellate Body paralysis enables these downstream constraints by removing the enforcement mechanism that would otherwise constrain them. Each downstream constraint has its own extractiveness value reflecting the specific policy domain; the WTO degradation enables the extraction in all of them by removing the rule-enforcing institution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_institutional_degradation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
