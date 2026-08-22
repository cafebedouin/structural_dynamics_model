% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility as Policy-Subordinated Obligation (Policy-Flexible Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This story instantiates the policy-flexible reading of the dollar-gold
 *   convertibility kernel: the U.S. Article IV commitment to convert dollars
 *   into gold at $35/oz is read as a conditional obligation, exercisable and
 *   deferrable at U.S. discretion when domestic economic stability
 *   (employment, growth, fiscal financing of the Vietnam War and Great
 *   Society) would be threatened by strict compliance. Under this reading the
 *   U.S. exits the victim set entirely — it is the seat that regains and
 *   exercises monetary autonomy — while dollar holders (foreign central
 *   banks, private holders, gold pool participants) enter the victim set,
 *   bearing the devaluation and non-conversion risk that the U.S. transfers
 *   outward. This is a distinct constraint from the
 *   strict_convertibility_reading (where the U.S. itself is bound and
 *   burdened) and from the triffin_structural_reading (where no party is at
 *   fault, only the system's design). Each reading has its own ε, victim set,
 *   and classification; they are linked only via the shared kernel_id.
 *
 * KEY AGENTS:
 *   - us_treasury: administers the gold window and decides when/whether conversion commitments bind
 *   - us_federal_reserve: sets domestic policy without external gold-drain constraint
 *   - foreign_central_banks_holding_dollars: bear reserve-value risk from deferred convertibility
 *   - foreign_private_dollar_holders: bear risk with no formal standing at all
 *   - international_monetary_fund: observes compliance gap without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.58).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.42).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility as Policy-Subordinated Obligation (Policy-Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, '3b7faa16-9a54-4ea9-8546-f0694d36744c').
narrative_ontology:cs_kernel_codification('3b7faa16-9a54-4ea9-8546-f0694d36744c', formalized).
narrative_ontology:cs_authority_grounding('3b7faa16-9a54-4ea9-8546-f0694d36744c', extraction).
narrative_ontology:cs_interpretation_layer_present('3b7faa16-9a54-4ea9-8546-f0694d36744c').
narrative_ontology:cs_reading_relation('3b7faa16-9a54-4ea9-8546-f0694d36744c', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b7faa16-9a54-4ea9-8546-f0694d36744c', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('3b7faa16-9a54-4ea9-8546-f0694d36744c', foundational, domestic_stabilization_takes_priority_over_external_commitment).
narrative_ontology:cs_axiom_status(domestic_stabilization_takes_priority_over_external_commitment, holdable).
narrative_ontology:cs_axiom_grounding('3b7faa16-9a54-4ea9-8546-f0694d36744c', domestic_stabilization_takes_priority_over_external_commitment, conventional).
narrative_ontology:cs_axiom('3b7faa16-9a54-4ea9-8546-f0694d36744c', secondary, convertibility_is_instrument_not_binding_rule).
narrative_ontology:cs_axiom_status(convertibility_is_instrument_not_binding_rule, holdable).
narrative_ontology:cs_axiom_grounding('3b7faa16-9a54-4ea9-8546-f0694d36744c', convertibility_is_instrument_not_binding_rule, instrumental).
narrative_ontology:cs_reference_frame('3b7faa16-9a54-4ea9-8546-f0694d36744c', bretton_woods_founding_compromise).
narrative_ontology:cs_drift_state('3b7faa16-9a54-4ea9-8546-f0694d36744c', gold_pool_collapse_1968, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3b7faa16-9a54-4ea9-8546-f0694d36744c', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_treasury).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_federal_reserve).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, domestic_us_labor_market).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_fiscal_policymakers).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks_holding_dollars).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_private_dollar_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, gold_pool_participant_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, gold_pool_participant_nations).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, domestic_stabilization_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the gold window and decides, in practice, when convertibility commitments will be honored at par, deferred, restricted (as with the 1963 Interest Equalization Tax and later capital controls), or suspended outright. Treats the Bretton Woods gold-conversion promise as a standing but conditional policy instrument, exercisable when convertibility would not force contractionary domestic policy. Bears no cost when it defers or narrows conversion access.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_treasury, agenda_setter,
    institutional, generational, arbitrage, global).

% Sets domestic interest rates and money supply according to U.S. employment and growth objectives without treating the external gold drain as a binding constraint on those choices. Runs an accommodative policy through the 1960s that domestic conditions would justify on their own terms, leaving external convertibility pressure to be managed by other means (swap lines, gold pool, moral suasion) rather than by monetary tightening.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_federal_reserve, beneficiary,
    institutional, generational, arbitrage, national).

% Benefits from the Fed's freedom to prioritize employment and growth over the gold-defense contraction that a strict convertibility reading would require. Workers and domestic firms experience looser credit conditions and lower unemployment than a gold-first policy would have produced; they have no direct stake in convertibility and do not bear its costs.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, domestic_us_labor_market, beneficiary,
    moderate, biographical, constrained, national).

% Finance deficit spending (Vietnam War outlays, Great Society programs) without external gold-reserve limits functioning as a hard fiscal constraint. Treat the convertibility commitment as a foreign-relations and reserve-currency management problem to be handled separately from the domestic budget, insulating fiscal choices from balance-of-payments discipline.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_fiscal_policymakers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, us_fiscal_policymakers, agenda_setter).

% Hold large dollar reserves accumulated through the reserve-currency role of the dollar, and depend on the U.S. honoring its $35/oz conversion commitment to preserve reserve value. As the U.S. treats conversion as conditional on its own domestic priorities, these banks absorb the devaluation and non-conversion risk: their reserves are only as good as a promise the issuer reserves the right to defer. Exit is politically and diplomatically constrained — dumping dollars risks currency disruption and alliance friction (as France discovered in 1965-1968).
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks_holding_dollars, payer,
    organized, biographical, constrained, global).

% Hold dollar-denominated assets and eurodollar deposits without access to the gold window (a central-bank-only privilege under Bretton Woods rules) and without the diplomatic leverage of a sovereign holder. They bear the erosion of dollar purchasing power and eventual devaluation risk with no formal recourse and no seat in the conversion decision at all.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_private_dollar_holders, payer,
    powerless, biographical, trapped, global).

% Contribute gold to the London Gold Pool to suppress the market price and defend the official $35 parity, effectively subsidizing continued U.S. policy autonomy by absorbing part of the defense cost themselves. They benefit from short-term system stability but pay in depleted reserves and eventual pool collapse (1968) when U.S. domestic priorities keep outpacing what collective gold defense can sustain.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, gold_pool_participant_nations, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, gold_pool_participant_nations, beneficiary).

% Monitors compliance with Article IV obligations and mediates disputes between the U.S. and dollar-holding members, but has no enforcement mechanism to compel conversion or discipline U.S. domestic monetary policy. Documents the growing gap between formal commitment and practiced flexibility without power to close it.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, us_treasury).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a nominally fixed exchange-rate anchor (gold-convertible dollar) that lets trading and reserve-holding nations coordinate on a common unit of account and store of value, avoiding the transaction costs and instability of freely floating rates in the postwar reconstruction period.
% TRANSFER_FUNCTION: Moves policy flexibility to the United States (which retains domestic monetary and fiscal discretion) at the expense of dollar-reserve value and predictability for foreign holders, who absorb the risk that the U.S. will treat its conversion commitment as subordinate to its own stabilization needs.
% ABSENT_VOICES: Private foreign dollar holders and smaller reserve-accumulating nations have no seat in the bilateral and G-10 negotiations (Roosa bonds, gold pool arrangements, swap lines) that manage the system's stresses; the arrangements are negotiated among major central banks and the U.S. Treasury, with the diffuse mass of dollar holders bearing outcomes they did not shape.
% DISAPPEARANCE_RATIONALE: If the U.S. had treated convertibility as strictly binding rather than conditional, the Fed would have been forced into contractionary policy far earlier (1958-1965), foreign reserve accumulation patterns would have differed, and the system might have transitioned to floating rates, or collapsed, years before 1971 actually occurred. The conditional-obligation practice is precisely what let the system persist as long as it did while accumulating the imbalances that eventually broke it.
% FOUNDING_PROBLEM: Postwar planners needed a credible nominal anchor to replace the interwar gold standard's failures and enable trade and capital reconstruction without repeating 1930s competitive devaluation and autarky.
% FOUNDING_PROBLEM_CORROBORATION: Foreign central bank officials (notably French and German monetary authorities in the mid-1960s) and later IMF staff assessments attest that by the mid-1960s the U.S. was managing convertibility as a discretionary instrument of its own policy rather than a binding external anchor — a reading corroborated by academic monetary historians (e.g. Eichengreen's account of Bretton Woods' terminal phase) writing from outside the U.S. policymaking apparatus that benefited from the flexibility.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.32 (1958, convertibility largely honored) to 0.58 (1971, effectively suspended) as the gap between formal commitment and practiced discretion widens — this tracks the accumulating divergence between U.S. gold reserves and outstanding dollar liabilities. Suppression is moderate (0.42 at end): the U.S. did not need coercive suppression of alternatives because foreign holders' exit options were themselves constrained by diplomatic and systemic considerations (a French or German dash for gold risked alliance rupture and currency disruption), not by direct U.S. coercion. Theater ratio rises through the gold pool era (peaking near 1968) as coordinated central-bank gold sales increasingly functioned as confidence-signaling rather than a sustainable price defense, then eases slightly as the pool's 1968 collapse ended the pretense.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, the U.S. Treasury and Federal Reserve sit at the beneficiary end: they retain the discretion to prioritize domestic stabilization and treat conversion as one policy lever among several, with essentially frictionless exit (arbitrage-grade — they can suspend, restrict, or defer conversion, as they ultimately did in August 1971, at will). Foreign central banks and private holders sit at the target end: they hold dollar claims whose value depends on a promise the issuer treats as conditional, and their exit options are constrained (sovereign holders, by diplomacy) or fully trapped (private holders, by lack of standing). Domestic U.S. labor and fiscal policymakers are secondary beneficiaries who gain from the policy space this reading of convertibility preserves, without directly administering it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a credible nominal anchor to prevent 1930s-style competitive devaluation — was substantially resolved by the mid-1960s once postwar reconstruction was complete and floating-rate alternatives were technically and institutionally feasible. Under the policy-flexible reading, the arrangement's persistence past that point reflects continued U.S. benefit from the reserve-currency privilege (financing deficits in its own currency) rather than continued necessity of the anchor function — a live mandatrophy candidate. The classification prevents mislabeling this as pure coordination breakdown or as villainous extraction: the coordination function was real in 1944-1958, but its subordination to U.S. domestic policy from the late 1950s onward layers extraction (externalized devaluation risk) onto a residual coordination shell (the dollar still served as a settlement medium).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_vs_default_boundary,
    'Is U.S. deferral of gold conversion in the 1960s better characterized as legitimate exercise of a conditional policy instrument (the reading this story instantiates) or as unilateral default on a binding treaty obligation (the strict_convertibility_reading)?',
    'Close textual and negotiating-history analysis of the Bretton Woods Articles of Agreement and contemporaneous U.S. Treasury/State Department internal memoranda regarding the perceived bindingness of the $35/oz commitment, cross-referenced against how other Article IV parties understood the commitment at the time of ratification.',
    'If the negotiating record shows convertibility was understood by all parties as unconditional and binding, this reading''s own claim of legitimate conditionality is substantially weakened and its extraction figure would need to rise toward the strict reading''s; if the record shows convertibility was always understood as policy-contingent even by other signatories, this reading''s ε is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_vs_default_boundary, conceptual, 'Whether the conditionality this reading asserts was a shared understanding or a unilateral reinterpretation.').

omega_variable(
    reserve_currency_privilege_scope,
    'How much of the measured extraction (0.58 by 1971) reflects deliberate policy choice to subordinate convertibility to domestic goals, versus how much reflects the structural ''exorbitant privilege'' of reserve-currency status that would have generated extraction regardless of any specific administration''s choices?',
    'Counterfactual analysis comparing U.S. monetary policy independence under the actual Bretton Woods arrangement against a hypothetical non-reserve-currency country facing equivalent balance-of-payments pressures, using comparative cases (e.g., UK sterling crises of the same period, which did force contractionary responses).',
    'If most of the extraction is attributable to reserve-currency structure rather than discretionary policy choice, this points toward the triffin_structural_reading capturing more of the true mechanism, with this reading''s agency-focused framing capturing a smaller share of the real story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reserve_currency_privilege_scope, conceptual, 'Disentangling structural privilege from discretionary policy choice as sources of the measured extraction.').

omega_variable(
    gold_pool_participant_consent_quality,
    'Did gold pool participant nations genuinely consent to subsidizing U.S. policy autonomy, or were they structurally coerced by the absence of a viable alternative reserve arrangement?',
    'Examine internal central bank records (Bundesbank, Banque de France) from 1961-1968 for evidence of explicit reluctance, negotiated concessions extracted in exchange for pool participation, or genuine belief in shared benefit.',
    'Genuine consent supports the coordination-function component of a tangled_rope reading; documented reluctance/coercion would push this element of the story closer to snare-like extraction with only cosmetic coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_pool_participant_consent_quality, empirical, 'Whether gold pool participation was voluntary coordination or structurally coerced subsidy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1958, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(doll_tr_t1961, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1961, 0.2).
narrative_ontology:measurement(doll_tr_t1963, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1963, 0.25).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(doll_tr_t1967, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1968, 0.42).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.4).

% Extraction over time
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1958, 0.32).
narrative_ontology:measurement(doll_be_t1961, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1961, 0.4).
narrative_ontology:measurement(doll_be_t1963, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1963, 0.45).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(doll_be_t1967, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1967, 0.53).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement(doll_su_t1961, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1961, 0.3).
narrative_ontology:measurement(doll_su_t1963, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1963, 0.35).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1965, 0.38).
narrative_ontology:measurement(doll_su_t1967, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1968, 0.42).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__policy_flexible_reading, 0.12).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the dollar_gold_convertibility kernel, each a distinct constraint with its own epsilon and victim set per the ε-invariance principle. strict_convertibility_reading treats the U.S. as bound (U.S. enters the victim set, bearing the cost of contractionary defense of parity); triffin_structural_reading treats no party as culpable (the design itself, not any actor's discretion, generates the eventual collapse). This policy_flexible_reading is the mirror of strict_convertibility_reading: where that reading's ε is driven by U.S.-borne policy constraint, this reading's ε is driven by externally-borne devaluation/non-conversion risk. Do not average or reconcile ε values across the three files; each is independently authored and internally coherent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
