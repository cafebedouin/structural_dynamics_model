% ============================================================================
% CONSTRAINT STORY: poverty_trap_asset_seizure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_poverty_trap_asset_seizure, []).

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
 *   constraint_id: poverty_trap_asset_seizure
 *   human_readable: Poverty Trap Asset Seizure via Debt and Fines
 *   domain: economic/legal/social
 *
 * SUMMARY:
 *   Poverty trap asset seizure via debt and fines operates as a pure
 *   extraction mechanism targeting economically trapped populations. The
 *   constraint works by converting the last resources that could enable exit
 *   — housing, transportation, income — into debt collection payments. This
 *   creates a self-reinforcing trap: seizing a vehicle eliminates
 *   transportation to employment; seizing wages reduces income available for
 *   living expenses, forcing additional borrowing; legal fines for failure to
 *   pay compound the original debt, creating a debt spiral where the original
 *   obligation becomes irretrievable. The constraint demonstrates the full
 *   structural signature of a snare: high base extractiveness (0.78), severe
 *   suppression (0.82), low theater (0.35 — the mechanism is brutally direct,
 *   not performative), and no genuine coordination function. The
 *   beneficiaries — debt collectors, creditor institutions, municipal fine
 *   collection systems — experience the constraint as a coordination
 *   mechanism (rope from their perspective) that reliably extracts assets.
 *   The victims — low-income debtors — have no alternatives within the
 *   constraint's operating parameters: income is garnished, vehicles are
 *   seized, fines compound, and the exit options that might enable escape
 *   (asset sale, relocation, income growth) are eliminated by the seizure
 *   itself.
 *
 * KEY AGENTS:
 *   - Low-income debtors: Primary victim (powerless/trapped) — bear full extraction cost; no exit options; assets seized eliminate pathways to higher income
 *   - Debt collectors and collection agencies: Primary beneficiary (institutional/arbitrage) — convert enforcement machinery into revenue stream; can exit constraint via market segmentation or volume adjustment
 *   - Creditor institutions (lenders, credit cards, auto loans): Secondary beneficiary (institutional/arbitrage) — asset seizure enables credit extension to uncreditworthy borrowers; enforcement machinery recovers assets; minimize default losses
 *   - Municipal fine collection systems: Secondary beneficiary (institutional/arbitrage) — generate municipal revenue through compound fines; enforcement creates escalating debt-to-income transfers
 *   - Courts and sheriff offices: Enforcement agents (institutional/arbitrage) — operate seizure machinery; maintain performative due process while processing default judgments; see process as degraded (piton perspective)
 *   - Organized debtor coalitions: Secondary victim (organized/constrained) — face legal liability and coordination costs; organized power modulates but does not eliminate experienced extraction
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing enforcement necessity as inherent to credit systems, generating false summit classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(poverty_trap_asset_seizure, 0.78).
domain_priors:suppression_score(poverty_trap_asset_seizure, 0.82).
domain_priors:theater_ratio(poverty_trap_asset_seizure, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(poverty_trap_asset_seizure, extractiveness, 0.78).
narrative_ontology:constraint_metric(poverty_trap_asset_seizure, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(poverty_trap_asset_seizure, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(poverty_trap_asset_seizure, snare).
narrative_ontology:human_readable(poverty_trap_asset_seizure, "Poverty Trap Asset Seizure via Debt and Fines").
narrative_ontology:topic_domain(poverty_trap_asset_seizure, "economic/legal/social").

domain_priors:requires_active_enforcement(poverty_trap_asset_seizure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(poverty_trap_asset_seizure, debt_collectors).
narrative_ontology:constraint_beneficiary(poverty_trap_asset_seizure, creditor_institutions).
narrative_ontology:constraint_beneficiary(poverty_trap_asset_seizure, municipal_fine_collection_systems).
narrative_ontology:constraint_victim(poverty_trap_asset_seizure, low_income_debtors).
narrative_ontology:constraint_victim(poverty_trap_asset_seizure, income_poverty_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED DEBTOR (SNARE) — Vehicle seized eliminates transportation to employment; wage garnishment reduces income available for living expenses; legal fines for missed payments compound the debt. Structural barriers to exit are total — the constraint uses enforcement machinery to eliminate the agent's exit capacity. No alternatives exist within the constraint's operating parameters.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-INCOME HOUSEHOLD / GENERATIONAL (TANGLED ROPE) — Over multiple generations, asset seizure creates a dynasty trap. Families accumulate debt across generations; education is deferred (parents work to service debt); intergenerational asset transfer is blocked. Some agency exists (income growth, debt forgiveness programs) but constrained. Coordination function exists (debt formalization provides access to credit that would otherwise be unavailable) alongside severe extraction. This perspective captures the multi-generational structural effect where the constraint appears less as pure extraction and more as a mixed system with coordinated credit access and embedded asymmetric extraction.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDITOR INSTITUTION (ROPE) — Access to enforcement machinery (courts, sheriffs, wage garnishment) solves the collective action problem of debt recovery. Institutional actors experience the constraint as pure coordination with minimal extraction perceived — the enforcement system reliably recovers assets, enabling credit extension to borrowers who would otherwise be uncreditworthy. The asymmetry is invisible from this perspective because exit options (arbitrage — can withdraw from lending or adjust terms) decouple the institution from the constraint's binding force.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIAL ENFORCEMENT SYSTEM / PITON — Debt collection courts are largely performative. Default judgments are entered in 95%+ of cases without defendant appearance; the courtroom theater of due process masks a unilateral seizure mechanism. The judicial system sees its own process as degraded — maintains strict adherence to procedure (notice, scheduling, hearings) despite knowing most debtors cannot afford to appear. Theater ratio is high because the procedural machinery creates an appearance of fairness while the structural outcome (asset seizure) is predetermined. Classification as piton reflects the institutional inertia: the system persists in its current form despite low functional legitimacy among practitioners.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED DEBTORS / COALITION (SNARE) — When debtors organize (mutual aid networks, debt resistance groups), the constraint still appears as extraction but with partial agency. Organized agents face constrained exit (legal liability, coordination costs) rather than total trapping. Coalition power modulates the experienced extraction from maximum (individual powerless agent) to substantial but navigable (organized group). This perspective demonstrates the dynamic coalition effect: even organization does not eliminate the snare's extractive force, but it creates agency and reduces perceived inevitability.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilization-scale perspective, some form of debt enforcement is necessary for any credit system to function. Without enforcement, lenders would not extend credit to low-income borrowers at all, and the entire structure of consumer finance would collapse. From this view, asset seizure appears as an immutable natural law: the price of access to credit. However, the structural beneficiary/victim declarations reveal this as a false summit. The constraint naturalizes what is contingent: the specific form of enforcement (asset seizure of exit-enabling property, wage garnishment, compound legal fines) and the asymmetric distribution of enforcement costs (targeting powerless agents, sparing institutional actors) are not inherent to credit systems — they are policy choices. The engine's FSM detector will identify this false summit.
constraint_indexing:constraint_classification(poverty_trap_asset_seizure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(poverty_trap_asset_seizure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(poverty_trap_asset_seizure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(poverty_trap_asset_seizure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(poverty_trap_asset_seizure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(poverty_trap_asset_seizure, TR),
    TR >= 0.70.

:- end_tests(poverty_trap_asset_seizure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The constraint extracts 78% of the target agent's remaining resources (net of subsistence) through compound mechanism: initial debt repayment + wage garnishment + asset seizure + compound fines. The baseline measurement accounts for the fact that some credit access (viewed from creditor perspective) legitimately requires some enforcement mechanism, but the specific form and intensity of this constraint far exceeds what credit system stability requires. The rising trajectory (0.55 → 0.78 over 10 years) reflects compound fines and cascading asset seizures, where each enforcement action increases debt faster than the debtor can service it. Suppression (0.82): Very high. Multiple mechanisms prevent exit: (1) Structural legal barriers — wage garnishment, asset seizure, default judgment procedures that operate without debtor participation; (2) Economic barriers — lost transportation eliminates employment access, forcing informal income that is harder to garnish (creating a perverse incentive structure); (3) Information asymmetries — debtors often do not understand the compounding mechanism and cannot effectively negotiate. The rising trajectory reflects that suppression mechanisms themselves intensify over time as the debtor's options narrow: early in the constraint, some negotiation may be possible; later, the machinery operates mechanically. Theater ratio (0.35): Low. Asset seizure is direct and functional, not performative. Courts maintain the theater of due process (scheduled hearings, notice, judgment forms), but the underlying mechanism is unilateral — 95%+ of debt collection cases result in default judgment because debtors cannot appear. The low theater reflects the brutal directness of the mechanism: vehicles are seized, wages are garnished, homes are sold at auction. No elaborate ritual disguises the extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival gap across institutional and individual boundaries. The creditor institution sees rope — a coordination mechanism that solves the collective action problem of lending to low-income borrowers. The trapped debtor sees snare — pure extraction with no alternatives. The courts see piton — performative due process theater disguising a unilateral seizure mechanism. The organized coalition of debtors sees a snare with partial agency — the extraction is real but navigable through coordination. The generational household perspective sees tangled rope — some coordination (access to credit) embedded in severe extraction. The analytical observer risks seeing mountain — naturalizing enforcement necessity as inherent to credit systems. The perspectival gap reflects how completely different institutional positions generate incommensurable interpretations of the same structural phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the extraction flow. Low-income debtors are victims + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. Creditor institutions are beneficiaries + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative effective extraction (they perceive benefit, not cost). Courts/sheriffs are enforcement agents + arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.08 → they experience coordination with minimal extraction perceived. Organized debtors are victims + constrained exit → d ≈ 0.85 → f(d) ≈ 1.15 → high but not maximum extraction. The household over generational time is victim + mobile exit (emigration, marriage, inheritance) → d ≈ 0.70 → f(d) ≈ 1.00 → moderate-to-high extraction. These derivations explain why the same structural arrangement produces entirely different classifications: the directionality function maps structural position to experienced intensity, and the classifications cluster around different intensity levels. Scope modifier σ(S) = 1.0 (national scope, no amplification/dampening).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint achieves >0.70 extractiveness while maintaining clarity on the beneficiary/victim structure and the coordination function boundary. Mandatrophy resolution here is straightforward: this is a pure snare with no genuine coordination function from the victim's perspective. The beneficiary (creditor) claims that asset seizure is necessary for credit access coordination, but this claim does not resolve the mandatrophy — it merely identifies where the false summit candidate lies. Alternative enforcement mechanisms (income-based repayment, debt restructuring, wage garnishment without asset seizure of exit-enabling property) could maintain credit access while reducing extraction intensity. The constraint's snare classification is stable: it combines high extractiveness, high suppression, and zero exit options, with a clear extraction direction from powerless to institutional. The analytical challenge is that the constraint naturalizes itself as inevitable ('credit systems require enforcement') when policy choices determine the specific extraction form. This naturalization is precisely what false summit detection targets: beneficiaries declare the constraint as mountain (natural law), but the structural data (clear victims, clear beneficiaries, manipulable enforcement mechanisms) reveal it as a contingent institutional arrangement. The engine's FSM detector will flag this false summit when it observes beneficiary declarations on a mountain-classified constraint with identical metric signatures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_necessity_ambiguity,
    'Is asset seizure of exit-enabling property (vehicles, primary residence) necessary for credit system stability, or are alternative enforcement mechanisms (wage garnishment without asset seizure, income-based repayment, debt restructuring) functionally equivalent?',
    'International comparative analysis of credit systems with different enforcement models; examination of countries with restrictive asset seizure rules and their credit availability for low-income borrowers; correlation between seizure intensity and credit access',
    'If alternative mechanisms are functionally equivalent: the constraint is policy-contingent, and the mountain classification is false summit. If asset seizure is necessary: some forms of extraction are inherent to credit systems, and the mandatrophy persists unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_necessity_ambiguity, empirical, 'Whether asset seizure is structurally necessary for credit system stability').

omega_variable(
    debt_spiral_exit_probability,
    'What is the probability that a low-income debtor in active asset seizure ever achieves sufficient income and asset accumulation to exit the constraint without external intervention (debt forgiveness, income floor, asset protection)?',
    'Longitudinal tracking of debtors subject to asset seizure; measurement of wage growth, debt reduction, and asset accumulation over 5, 10, 20-year horizons; comparison with control groups receiving temporary forbearance or debt restructuring',
    'If exit probability is near zero without intervention: the trap is structural and permanent (maximum snare severity). If exit probability is >30% within 10 years: some agents can self-exit (reduces to tangled rope from some perspectives). If exit probability is ~50%: the constraint is lifecycle-dependent rather than permanent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_spiral_exit_probability, empirical, 'Exit probability for debtors subject to asset seizure').

omega_variable(
    enforcement_machinery_discretion,
    'How much discretion do creditors and courts exercise in asset seizure decisions? Is enforcement mechanistic (all qualifying debtors seized) or selective (discretionary targeting of high-value assets or economically mobile individuals)?',
    'Analysis of court docket data examining correlation between debtor characteristics (income, asset type, prior seizures) and enforcement decisions; interviews with creditor collection managers on selection criteria; comparison of seizure rates across similar debtors with different institutional lenders',
    'If mechanistic: asset seizure is inevitable law-like enforcement with near-zero escape probability. If highly discretionary: some institutional actors are extracting more aggressively than others, and constraint variation reflects creditor choice rather than structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_machinery_discretion, empirical, 'Discretion in creditor enforcement decisions').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.82) predominantly structural (legal barriers, enforcement machinery, asset seizure capacity) or internalized (debtors believe the system is just, inevitable, or their own fault)?',
    'Post-exit suppression trajectory: track debtors who escape debt and measure whether suppression patterns persist (internalized) or dissolve (structural). Comparison with debtors receiving debt forgiveness while remaining in enforcement jurisdiction. Survey evidence on debtor beliefs about system legitimacy, fairness, and changeability.',
    'If predominantly structural: the suppression is real and external. If significantly internalized: the constraint''s effective suppression is higher than the measured structure suggests — debtors carry the suppression with them after exit and may resist future credit access even with legal barriers removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized suppression').

omega_variable(
    compound_fine_dynamics,
    'Do legal fines for nonpayment (failure to appear, contempt, collection costs) function as automatic escalation (compounding debt beyond the original obligation) or as settlement-based incentives (debtors can negotiate down)?',
    'Examination of fine trajectories in court records; measurement of original debt vs. final judgment amount; interviews with court administrators and creditor attorneys on fine application practices; comparison of final judgment amounts for similar initial debts across jurisdictions with different fine rules',
    'If automatic escalation: fines are a pure extraction mechanism with no coordination function, strengthening snare classification. If negotiable: fines create leverage for settlement, modulating extraction intensity and potentially enabling exit through negotiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compound_fine_dynamics, empirical, 'Whether fines escalate automatically or are negotiable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(poverty_trap_asset_seizure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pts_tr_t0, poverty_trap_asset_seizure, theater_ratio, 0, 0.32).
narrative_ontology:measurement(pts_tr_t10, poverty_trap_asset_seizure, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(pts_be_t0, poverty_trap_asset_seizure, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(pts_be_t3, poverty_trap_asset_seizure, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(pts_be_t6, poverty_trap_asset_seizure, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(pts_be_t10, poverty_trap_asset_seizure, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pts_su_t0, poverty_trap_asset_seizure, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(pts_su_t3, poverty_trap_asset_seizure, suppression_requirement, 3, 0.76).
narrative_ontology:measurement(pts_su_t6, poverty_trap_asset_seizure, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(pts_su_t10, poverty_trap_asset_seizure, suppression_requirement, 10, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(poverty_trap_asset_seizure, resource_allocation).
narrative_ontology:affects_constraint(poverty_trap_asset_seizure, predatory_lending_cycle).
narrative_ontology:affects_constraint(poverty_trap_asset_seizure, eviction_housing_instability).
narrative_ontology:affects_constraint(poverty_trap_asset_seizure, wage_garnishment_income_floor).

% DUAL FORMULATION NOTE:
% Asset seizure for debt recovery is one constraint in a family of interconnected mechanisms that comprise the poverty trap. Upstream constraints (predatory lending, discriminatory credit pricing) create the initial high-interest debt; asset seizure converts that debt into structural poverty. Downstream constraints (eviction, wage garnishment) amplify the trap. All three should be understood as a linked system where breaking one constraint (e.g., eliminating asset seizure of vehicles) would only partially escape the trap without addressing upstream predatory debt creation or downstream housing instability mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(poverty_trap_asset_seizure, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
