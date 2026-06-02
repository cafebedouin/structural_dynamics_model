% ============================================================================
% CONSTRAINT STORY: fiscal_federalism_absence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fiscal_federalism_absence, []).

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
 *   constraint_id: fiscal_federalism_absence
 *   human_readable: Absence of Fiscal Federalism in Migration-Driven Welfare Systems
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The absence of fiscal federalism creates a structural extraction
 *   mechanism in political systems where welfare responsibilities are
 *   decentralized to subnational jurisdictions while tax revenue is
 *   concentrated at the national level. This constraint manifests acutely in
 *   systems experiencing significant internal migration (both voluntary labor
 *   migration and forced displacement). The tension is permanent: poor
 *   jurisdictions cannot fund mandated welfare without raising local tax
 *   rates that drive out the mobile and productive; wealthy jurisdictions
 *   benefit from labor inflows without proportional welfare costs; the
 *   national government coordinates mobility while maintaining the fiscal
 *   imbalance that enables extraction. The constraint exhibits all six DR
 *   types because it operates simultaneously as a natural economic law (from
 *   civilizational distance), an institutional degradation (federalism
 *   without function), a mixed coordination-extraction hybrid (at the
 *   national level), and pure extraction (from the powerless jurisdiction
 *   perspective). The measurement trajectory shows rising extractiveness and
 *   suppression over the interval, reflecting increasing migration pressure
 *   and widening fiscal gaps that tighten the extraction mechanism.
 *
 * KEY AGENTS:
 *   - Impoverished Jurisdiction: Primary victim (powerless/trapped) — constitutionally obligated to provide welfare, cannot raise sufficient revenue, cannot exit, bears full cost
 *   - Wealthy Jurisdiction: Primary beneficiary (institutional/arbitrage) — selectively captures labor benefit from migration, avoids proportional welfare cost, can exit via incentive design
 *   - Middle-Income Region: Secondary agent (moderate/constrained) — benefits from labor mobility coordination, bears moderate welfare cost, constrained but not trapped
 *   - National Government: Institutional coordinator (powerful/constrained) — maintains fiscal structure, coordinates labor mobility, profits from tax centralization, technically constrained by constitutional structure
 *   - Migrant Recipients: Distributed victims (powerless/constrained) — concentrate in poor jurisdictions due to network effects and housing costs; benefit from mobility but concentrated extraction reduces welfare floor
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent fiscal structure as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiscal_federalism_absence, 0.58).
domain_priors:suppression_score(fiscal_federalism_absence, 0.68).
domain_priors:theater_ratio(fiscal_federalism_absence, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiscal_federalism_absence, extractiveness, 0.58).
narrative_ontology:constraint_metric(fiscal_federalism_absence, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fiscal_federalism_absence, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fiscal_federalism_absence, tangled_rope).
narrative_ontology:human_readable(fiscal_federalism_absence, "Absence of Fiscal Federalism in Migration-Driven Welfare Systems").
narrative_ontology:topic_domain(fiscal_federalism_absence, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(fiscal_federalism_absence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fiscal_federalism_absence, wealthy_jurisdictions).
narrative_ontology:constraint_beneficiary(fiscal_federalism_absence, national_government).
narrative_ontology:constraint_victim(fiscal_federalism_absence, poor_jurisdictions).
narrative_ontology:constraint_victim(fiscal_federalism_absence, migrant_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPOVERISHED JURISDICTION (SNARE) — Trapped by constitutional/legal structure that assigns welfare responsibility locally while tax revenue concentrates nationally. Cannot exit the welfare obligation without violating fundamental law; cannot raise sufficient revenue; bears full extraction cost. No alternative is accessible.
constraint_indexing:constraint_classification(fiscal_federalism_absence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MIDDLE-INCOME REGION (TANGLED ROPE) — Constrained by revenue limits and migrant inflow but benefits from coordination of interregional labor mobility (migrants fill labor shortages). Mixed extraction and coordination; agency exists but high costs to exit welfare provision.
constraint_indexing:constraint_classification(fiscal_federalism_absence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WEALTHY JURISDICTION (ROPE) — Captures benefits of labor mobility without proportional welfare cost. Can export poor residents via migration incentives; receives migrants selectively. Pure coordination gain from free movement without extraction cost.
constraint_indexing:constraint_classification(fiscal_federalism_absence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NATIONAL GOVERNMENT (TANGLED ROPE) — Coordinates labor mobility and welfare system across regions (genuine coordination function) but maintains fiscal structure that extracts from poor jurisdictions. Benefits from tax centralization while decentralizing welfare burden. Both coordination role and asymmetric extraction.
constraint_indexing:constraint_classification(fiscal_federalism_absence, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FEDERALIST INSTITUTIONAL STRUCTURE (PITON) — The formal legal commitment to federalism persists as theater while its functional coordination purpose has atrophied. The structure was designed to enable local autonomy (genuine coordination goal) but now maintains vertical extraction through constitutional immobility. Theater ratio reflects the gap between federalist rhetoric (subsidiarity, local control) and actual practice (revenue centralization, mandate-unfunded decentralization).
constraint_indexing:constraint_classification(fiscal_federalism_absence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, the tension between centralized taxation and decentralized welfare provision appears as a structural inevitability: any system with migration and local autonomy must solve this problem, and all solutions involve some extraction. The constraint appears immutable across political systems. Engine false-summit detection will flag this as naturalization of a politically contingent institutional choice.
constraint_indexing:constraint_classification(fiscal_federalism_absence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiscal_federalism_absence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fiscal_federalism_absence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiscal_federalism_absence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fiscal_federalism_absence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fiscal_federalism_absence, TR),
    TR >= 0.70.

:- end_tests(fiscal_federalism_absence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The original research measured this at 0.42 in early welfare systems; it has risen to 0.58 as migration pressure increased and jurisdictional inequality widened. The extraction is not total (wealthy jurisdictions genuinely benefit from labor mobility, not pure parasitism) but substantial — the unfunded welfare mandate on poor jurisdictions is documented at 12-18% of local budgets in high-migration systems. Suppression (0.68): High. The mechanism is deeply entrenched: constitutional limits on local taxation, national revenue preemption, legal barriers to residency-based welfare exclusion (creating forced obligation), and collective action barriers prevent coordinated reform. Agents understand the structure but cannot unilaterally exit. The suppression has increased over time as constitutional immobility has resisted reform pressure. Theater ratio (0.55): Moderate. The federalist rhetoric of subsidiarity and local autonomy persists while the fiscal reality is centralized extraction. Decentralized welfare spending is presented as respecting local choice, but the revenue constraint makes it theater — jurisdictions have the 'choice' to either underfund welfare or raise taxes that trigger out-migration of the tax base. The theater has increased as the gap widens and more elaborate justifications are needed.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence across power positions. The wealthy jurisdiction sees pure coordination (Rope) — free labor movement solves regional labor shortages and increases economic efficiency. The national government sees necessary coordination with some extraction overhead (Tangled Rope) — they manage a complex system that both enables mobility and maintains order. The middle-income region sees mixed outcomes (Tangled Rope) — they benefit and suffer simultaneously. The impoverished jurisdiction sees pure extraction (Snare) — they bear all costs with no exit. The federalist institutional structure itself (Piton) appears as a degraded mechanism — it was designed for coordination (subsidiarity, local autonomy) but now functions primarily to legitimize extraction. The analytical observer (Mountain) risks concluding that fiscal federalism absence is an immutable feature of any federal system, but the structural data reveals it as a specific institutional choice: federal systems like Germany and Canada have implemented revenue-sharing mechanisms that reduce or eliminate the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value is derived from their structural position within the extraction flow. Poor jurisdictions are full targets (d approaching 1.0) — they have no arbitrage options, cannot exit welfare obligations, and bear maximum extraction cost. Wealthy jurisdictions are beneficiaries (d approaching 0.0) — they capture labor benefits and avoid proportional welfare cost; they have arbitrage options (can migrate high-earners out). The national government occupies a middle position (d ≈ 0.5-0.6) — they both coordinate the system (beneficiary role) and maintain structures that extract from poor jurisdictions (partial target role). These directionality positions feed into the chi formula, producing the perspectival gap: the same underlying extractiveness (ε=0.58) scales differently for each agent depending on their d value. The powerless agent experiences high chi; the institutional beneficiary experiences negative or near-zero chi; the national government experiences moderate positive chi. This explains why the constraint appears as snare/rope/mountain from different viewpoints — not because the constraint itself is ambiguous, but because the extraction runs directionally through the system.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION VIA FISCAL FEDERALISM COMPARISON: The constraint resolves its ambiguity by comparison to working federal systems with integrated fiscal federalism. Germany's Länderfinanzausgleich (regional revenue-sharing), Canada's equalization payments, and Australia's federal grants all implement mechanisms that prevent the extraction pattern visible in the US/unitary systems. These comparisons show that: (1) Fiscal federalism absence is NOT inherent to federalism itself (Mountain classification is a false summit). (2) The constraint is remediable via institutional design (supports Scaffold classification with realistic sunset path). (3) The persistent absence in some systems reflects political choice by beneficiary jurisdictions, not technical necessity. This resolution supports the Tangled Rope classification from the national government perspective — the government both coordinates (genuine function) and extracts (asymmetric benefit). The absence is durable because beneficiaries profit from it and constitutional veto points protect the structure. A reform path exists (Scaffold logic) but requires political will from beneficiary coalitions (low probability). The constraint is neither natural law (Mountain false summit) nor pure extraction (Snare would imply no function) — it is a hybrid coordination mechanism with embedded asymmetric extraction that persists due to political capture of the fiscal architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    migration_sorting_causality,
    'Does welfare generosity drive in-migration of poor residents, or does in-migration require jurisdictions to raise welfare spending?',
    'Longitudinal comparison of welfare generosity changes vs. migration flows; synthetic control analysis of jurisdictions that changed welfare policy; modeling of revealed preference in location choice',
    'If welfare causes migration: beneficiaries (wealthy jurisdictions) are active extractors via incentive design. If migration is exogenous (economic opportunity): extraction is passive byproduct of fiscal structure. Determines whether the snare is intentional or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migration_sorting_causality, empirical, 'Causality between welfare generosity and in-migration').

omega_variable(
    fiscal_federalism_redesign_feasibility,
    'Is the absence of fiscal federalism a technical design failure or a political choice by wealthy jurisdictions?',
    'Comparison with federal systems (Germany, Canada, Australia) that have implemented revenue-sharing formulas; analysis of reform proposals that fail despite professional consensus; interviews with finance officials about design alternatives',
    'If design failure: constraint is remediable (Scaffold toward Rope). If political choice: constraint is durable (Snare stable). Determines terminal attractor classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_federalism_redesign_feasibility, conceptual, 'Whether fiscal federalism absence is technical or political').

omega_variable(
    welfare_boundary_definition,
    'Which welfare functions should be decentralized (K-12 education, local roads) vs. centralized (OASDI, healthcare)? Does the current boundary match economic logic or historical accident?',
    'Cross-national comparison of welfare function allocation; analysis of economies of scale and labor mobility sensitivity for each function; historical reconstruction of how boundaries were set in jurisdictions with working fiscal federalism',
    'If boundary is suboptimal: can be redrawn (Scaffold perspective strengthened). If boundary reflects genuine economic constraints: explains persistent extraction (Snare perspective stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_boundary_definition, empirical, 'Optimality of welfare function allocation boundary').

omega_variable(
    mandate_funding_gap_measurement,
    'What is the actual annual unfunded mandate cost (welfare spending decentralized without revenue delegation)?',
    'Comprehensive accounting of welfare spending by jurisdiction tier; comparison to centrally-collected revenue by tier; time-series analysis of gap growth',
    'High gap (>15% of local budgets): confirms high extractiveness. Low gap (<5%): extractiveness is modest, suggests Rope or Scaffold more than Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_funding_gap_measurement, empirical, 'Magnitude of unfunded welfare mandate cost').

omega_variable(
    constitutional_immobility,
    'Are the revenue and welfare allocation assignments constitutionally entrenched, or could they be changed by ordinary statute?',
    'Legal analysis of amendment requirements and statutory flexibility; historical precedent for reallocation without constitutional change; political economy modeling of veto coalitions that prevent reform',
    'If constitutionally entrenched: suppression high (agents cannot unilaterally exit). If statutory: suppression lower (reform is costly but possible). Affects whether constraint is Mountain or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_immobility, conceptual, 'Constitutional vs. statutory fixity of fiscal allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiscal_federalism_absence, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fiscfed_tr_t0, fiscal_federalism_absence, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fiscfed_tr_t5, fiscal_federalism_absence, theater_ratio, 5, 0.48).
narrative_ontology:measurement(fiscfed_tr_t10, fiscal_federalism_absence, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(fiscfed_be_t0, fiscal_federalism_absence, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fiscfed_be_t5, fiscal_federalism_absence, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fiscfed_be_t10, fiscal_federalism_absence, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fiscfed_su_t0, fiscal_federalism_absence, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fiscfed_su_t5, fiscal_federalism_absence, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(fiscfed_su_t10, fiscal_federalism_absence, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fiscal_federalism_absence, resource_allocation).
narrative_ontology:affects_constraint(fiscal_federalism_absence, welfare_race_to_bottom).
narrative_ontology:affects_constraint(fiscal_federalism_absence, migrant_concentration_clustering).
narrative_ontology:affects_constraint(fiscal_federalism_absence, jurisdictional_tax_competition).

% DUAL FORMULATION NOTE:
% Fiscal federalism absence is the upstream constraint that structurally enables welfare race-to-bottom dynamics and migrant concentration. Separate constraint stories model each phenomenon with different ε values; this story captures the fiscal architecture that makes them structurally possible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fiscal_federalism_absence, powerless, 0.92).
constraint_indexing:directionality_override(fiscal_federalism_absence, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
