% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Gold-Fiat Transition: Automatic Constraint Elimination (Automatic Constraint Reading)
 *   domain: monetary_economics/political_economy/institutional_history
 *
 * SUMMARY:
 *   The transition from gold-standard monetary constraint to fiat discretion
 *   represents a fundamental structural shift in how money creation is
 *   governed. Under the gold standard, the monetary authority faced an
 *   automatic physical constraint: the money supply could not exceed gold
 *   reserves. This constraint was material — not dependent on institutional
 *   enforcement or creditor discipline, but on the simple fact that you
 *   cannot spend what you do not have. The transition to fiat money (1971,
 *   following decades of erosion through Bretton Woods instability)
 *   eliminated this automatic constraint. Central banks could now create
 *   money at will, bounded only by institutional rules (inflation targeting,
 *   central bank independence) and political pressure from creditors
 *   concerned about currency debasement. This constraint story instantiates
 *   ONE reading of this transition: the AUTOMATIC CONSTRAINT reading. This
 *   reading characterizes the core structural change as the elimination of
 *   material constraint and substitution of institutional discretion.
 *   Beneficiaries are monetary authorities (who gained discretionary power)
 *   and sovereign governments (who can now conduct expansionary fiscal
 *   policy). Victims are creditors (who lost automatic protection from
 *   debasement) and currency holders (whose purchasing power becomes subject
 *   to central bank policy choices). The constraint itself remains
 *   institutional but has changed character from automatic to discretionary.
 *
 * KEY AGENTS:
 *   - Central Banking Authority: Primary beneficiary (institutional/arbitrage) — gained discretionary control over money supply and inflation
 *   - Sovereign Fiscal Authority: Secondary beneficiary (organized/constrained) — gained ability to conduct countercyclical spending without gold constraint
 *   - Creditor Class: Primary victim (powerless/trapped) — lost automatic protection from debasement; now subject to central bank discretion
 *   - Currency Holders: Secondary victim (moderate/constrained) — purchasing power now subject to institutional policy rather than material constraint
 *   - Commercial Banking System: Mixed (moderate/constrained) — benefits from credit expansion but subject to central bank regulatory oversight
 *   - Gold Reserve Regime: Institutional residue (institutional/arbitrage) — maintains vestigial role as theater of legitimacy rather than functional constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.58).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.52).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Gold-Fiat Transition: Automatic Constraint Elimination (Automatic Constraint Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy/institutional_history").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, 'gfm-auto-2026-0226-001').
narrative_ontology:cs_kernel_codification('gfm-auto-2026-0226-001', fixed_text).
narrative_ontology:cs_authority_grounding('gfm-auto-2026-0226-001', extraction).
narrative_ontology:cs_interpretation_layer_present('gfm-auto-2026-0226-001').
narrative_ontology:cs_reading_relation('gfm-auto-2026-0226-001', creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('gfm-auto-2026-0226-001', composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('gfm-auto-2026-0226-001', foundational, gold_reserve_constraint_was_automatic).
narrative_ontology:cs_axiom_status(gold_reserve_constraint_was_automatic, holdable).
narrative_ontology:cs_axiom_grounding('gfm-auto-2026-0226-001', gold_reserve_constraint_was_automatic, empirically_contingent).
narrative_ontology:cs_axiom('gfm-auto-2026-0226-001', foundational, fiat_discretion_eliminated_automatic_constraint).
narrative_ontology:cs_axiom_status(fiat_discretion_eliminated_automatic_constraint, holdable).
narrative_ontology:cs_axiom_grounding('gfm-auto-2026-0226-001', fiat_discretion_eliminated_automatic_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('gfm-auto-2026-0226-001', gold_reserve_automatic_constraint).
narrative_ontology:cs_drift_state('gfm-auto-2026-0226-001', post_bretton_woods_fiat_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('gfm-auto-2026-0226-001', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_governments).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, currency_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CREDITOR CLASS (SNARE) — Under gold standard, creditors had automatic protection: money supply was mechanically constrained by gold reserves. The transition eliminated this external limit, replacing it with the discretionary authority of central banks. Creditors cannot exit the monetary system; they bear extraction (inflation risk) with no structural recourse. The constraint shifted from material (you cannot print more than you have gold) to institutional (you can print as much as the central bank chooses). Maximum extraction for this perspective.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__automatic_constraint_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMERCIAL BANKING SECTOR (TANGLED ROPE) — Banks benefit from the transition: fiat money creation allows credit expansion without gold reserve constraints. But banks also bear extraction through central bank reserve requirements, regulatory oversight, and the central bank's monopoly on base money creation. The constraint is hybrid: genuine coordination function (fractional reserve system requires lender-of-last-resort backstop) plus asymmetric extraction (central bank authority over credit conditions). Moderate effective extraction due to agency and regulatory negotiation capacity.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANKING AUTHORITY (ROPE) — The transition is a coordination mechanism from this perspective: the central bank now controls inflation and credit availability, solving the problem of how to manage money supply without material constraint. The central bank experiences the constraint as pure coordination — the institutional framework that enables monetary policy. Net beneficiary through discretionary authority. Low experienced extraction.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__automatic_constraint_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FISCAL AUTHORITY (SCAFFOLD) — Governments see the fiat transition as enabling fiscal coordination: the state can now conduct countercyclical spending without being constrained by gold reserves. This is a temporary support mechanism — the constraint is designed with a sunset: central bank independence and inflation targeting (Volcker era onward) reimpose implicit discipline, though now institutional rather than material. The state experiences moderate extraction through inflation targeting rules and central bank authority independence, but also experiences coordination benefit from money creation enabling full employment policies.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__automatic_constraint_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VESTIGIAL GOLD STANDARD REGIME (PITON) — From the vantage of post-1971 fiat, the gold standard itself now appears as a degraded institutional form. Some central banks maintain gold reserves as a theater of legitimacy (psychological anchor for currency value) rather than a functional constraint. The reserve requirement persists despite having no mechanical force. Theater ratio high because gold backing is now performative — it does not constrain money creation but symbolically reassures markets of central bank solvency.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__automatic_constraint_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the gold standard appears as an immutable natural law of monetary systems: 'you cannot spend what you do not have.' The transition appears to violate this law — fiat money seems to enable unlimited spending. This perspective risks naturalizing what is actually a contingent institutional arrangement (central bank authority backed by state taxing power, not by physical gold). The engine's false-summit detector will flag this as a naturalization of institutional discretion as immutable constraint.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__automatic_constraint_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__automatic_constraint_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__automatic_constraint_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, TR),
    TR >= 0.70.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The transition increased effective extraction on creditors by replacing an automatic material constraint with discretionary institutional authority. However, the extraction is not maximal (snare-level 0.66+) because alternative institutional disciplines emerged post-1980 (inflation targeting, central bank independence) that partially reimpose constraint-like discipline. The automatic removal of the gold ceiling allowed a period of high discretionary extraction (1971–1979), but Volcker's inflation control (1979–1985) and subsequent inflation-targeting frameworks reinstated disciplinary pressure. Suppression (0.52): Moderate. The gold standard's suppression was minimal because constraint was automatic. Fiat suppression is institutional: central banks must actively suppress inflationary expectations through interest rate policy, credibility maintenance, and communication theater. The suppression increased in the transition period but stabilized as institutional rules crystallized. Theater ratio (0.38): Moderate. Unlike the gold standard (where constraint is material, theater minimal), fiat requires performative legitimacy: central banks conduct open-market operations, publish forward guidance, and maintain inflation-targeting frameworks partly to reassure creditors that discretion is bounded. The theater is lower than in early fiat era (1971–1979) because inflation targeting provides rule-based rather than discretionary framework.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival divergence. The central bank sees pure coordination (rope): the transition solved the problem of how to manage credit and inflation without material constraint. Creditors see extraction (snare): the automatic protection they enjoyed is gone, replaced by institutional discretion they cannot control. The fiscal authority sees temporary support (scaffold): the transition enabled countercyclical spending, but new discipline rules (inflation targeting, central bank independence) reimpose constraints — the 'full employment' window is closing. The commercial banking sector sees mixed dynamics (tangled rope): they benefit from credit expansion but face regulatory constraints. The analytical observer risks naturalizing the fiat regime as immutable law (mountain): 'government money must be backed by state authority,' which naturalizes what is contingent institutional arrangement. The gold standard itself appears as piton: the vestigial reserve requirement persists performatively (central banks maintain gold reserves for credibility theater) despite having no mechanical force. Each perspective reads the same structural change (automatic constraint replaced by institutional discretion) differently, depending on their position relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective flows from structural position: Creditors are victims with trapped exit → high d → high f(d) → high experienced extraction (snare). Central banks are beneficiaries with arbitrage exit → low d → negative f(d) → low/negative experienced extraction (rope). Commercial banks face constrained exit and mixed benefit/victim status → moderate d → moderate f(d) → moderate extraction (tangled rope). Fiscal authorities face constrained exit but also benefit from discretion → moderate d → moderate extraction (scaffold). The analytical observer is structurally external (analytical exit) but at risk of identity-locking to the 'natural law' framing → high d → high f(d) → misleading extraction estimate (false summit mountain). The magnitude of the perspectival gap is unusually high for this constraint because the transition genuinely changes the type of constraint (material to institutional), not merely its severity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's extractiveness (0.58) places it in the high range, requiring mandatrophy resolution. The constraint is a hybrid: genuine coordination function (central bank discretion enables countercyclical monetary policy, managing credit and inflation) plus asymmetric extraction (creditors lose automatic protection, currencies subject to debasement risk). The mandatrophy is resolved by the tangled-rope classification: the constraint coordinates monetary policy while extracting from creditors. The perspectival gap (snare to the victim, rope to the beneficiary) is a diagnostic signal of hybrid structure. Post-1980 institutional disciplines (inflation targeting, central bank independence) do not eliminate extraction — they redistribute it, changing its character from discretionary to rule-bound. The constraint persists as tangled rope rather than degrading to piton because the coordination function (managing credit and inflation) remains genuine; institutional rules just constrain how much discretionary power remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automatic_vs_discretionary_boundary,
    'Is the gold standard''s constraint truly automatic and material, or was it always mediated by institutional decisions about reserve ratios, currency pegs, and monetary policy?',
    'Historical analysis of central bank policy decisions during the gold standard era (reserve ratio changes, peg adjustments, currency suspensions during crises). If numerous discretionary interventions exist, the ''automatic'' framing was always partial.',
    'If automatic: the transition is a clean shift from material to institutional constraint. If discretionary: both gold standard and fiat involve institutional mediation; the difference is degree, not kind. Changes classification of both readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(automatic_vs_discretionary_boundary, empirical, 'Whether gold standard constraint was automatic or discretionary').

omega_variable(
    creditor_discipline_alternative_constraint,
    'Does the fiat regime eliminate creditor discipline entirely, or replace the gold constraint with alternative institutional disciplines (inflation targeting, central bank independence, fiscal rules)?',
    'Comparison of inflation volatility and currency value stability under gold standard vs post-1980 inflation-targeting fiat. If new disciplines prove as effective, the constraint persists in different form (creditor_discipline_reading hypothesis). If volatility much higher, the constraint has genuinely weakened.',
    'If alternative disciplines effective: this reading (automatic_constraint) understates the continued extraction on creditors; the constraint type shifts toward snare (discretionary extraction replaces automatic constraint). If ineffective: extraction is genuine, extraction is higher, and this reading''s classification (tangled_rope) is too mild.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_discipline_alternative_constraint, empirical, 'Whether fiat regime substitutes institutional for material discipline').

omega_variable(
    kernel_reading_boundary,
    'Is this reading (automatic_constraint) describing the actual structural change that occurred (material constraint removed, institutional discretion substituted), or is it describing a cover story used to justify the transition?',
    'Historical comparison: do contemporary accounts of the transition (Bretton Woods breakdown, Nixon shock) emphasize the automatic constraint removal, or emphasize alternative rationales (creditor discipline erosion, fiscal needs, capital mobility)? If alternative rationales dominate, this reading may be post-hoc analytical framing, not the actual committer axis.',
    'If cover story: this reading''s classification stands but is revealed as aspirational naturalization rather than accurate description of lived constraint. If accurate: the reading accurately maps the structural change as agents experienced it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether automatic constraint reading describes actual change or cover story').

omega_variable(
    beneficiary_identity_shift,
    'Did the fiat transition benefit monetary authorities permanently, or only during the transition period before new institutional disciplines (central bank independence, inflation targeting) reimposed constraints?',
    'Longitudinal analysis of central bank discretion and fiscal outcomes: did discretion increase in post-1971 fiat era, or did new rules (inflation targeting, Basel accords, ECB independence mandates) reimpose constraints equivalent to gold standard? If reimposed, the permanent beneficiary is unclear.',
    'If permanent benefit: monetary_authorities is stable beneficiary; snare classification for creditors confirmed. If temporary: the transition enabled a brief period of discretion followed by reimposition of constraints; the constraint type shifts toward scaffold (temporary support with sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_shift, empirical, 'Duration of monetary authority benefit from fiat discretion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gfm_auto_tr_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gfm_auto_tr_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(gfm_auto_tr_t15, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 15, 0.38).

% Extraction over time
narrative_ontology:measurement(gfm_auto_be_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(gfm_auto_be_t5, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(gfm_auto_be_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(gfm_auto_be_t15, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 15, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(gfm_auto_su_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(gfm_auto_su_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(gfm_auto_su_t15, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.12).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, bretton_woods_asymmetric_reserve_obligation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, central_bank_independence_doctrine).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, inflation_targeting_framework).

% DUAL FORMULATION NOTE:
% The gold-fiat transition is decomposed into three constraint stories reflecting different readings of the same kernel. The automatic_constraint_reading (this file) emphasizes elimination of material constraint. The creditor_discipline_reading will emphasize that discipline mechanisms persist (gold standard: automatic; fiat: institutional). The composite_overdetermination_reading will emphasize that both mechanisms operated simultaneously. These stories are linked by network.affects_constraints and represent different epistemic framings of the same historical event, not different observables of one constraint. See kernel_context documentation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__automatic_constraint_reading, institutional, 0.08).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__automatic_constraint_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
