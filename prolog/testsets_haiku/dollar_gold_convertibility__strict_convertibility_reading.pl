% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__strict_convertibility_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Article IV Gold Convertibility as Binding Legal Obligation
 *   domain: international_political_economy
 *
 * SUMMARY:
 *   Article IV of the Bretton Woods agreement (1944) bound the U.S. to
 *   maintain convertibility of dollars to gold at $35 per troy ounce. This
 *   reading interprets that commitment as a strict, binding legal obligation
 *   that constrains U.S. monetary policy and benefits foreign creditor
 *   nations with enforceable claims on U.S. gold reserves. Under this
 *   reading, the U.S. became a victim of its own currency's status as the
 *   international numeraire—losing autonomy over money supply, inflation, and
 *   countercyclical policy. Creditor nations (France, Germany, Belgium, and
 *   others) became beneficiaries, holding conversion rights that forced U.S.
 *   orthodoxy. This reading contrasts with two sibling interpretations:
 *   policy_flexible_reading (which treats convertibility as a conditional
 *   obligation subordinate to domestic stability) and
 *   triffin_structural_reading (which locates the problem not in the reading
 *   of the obligation but in its fundamental unsustainability). This story
 *   instantiates ONLY the strict_convertibility_reading, modeling the
 *   constraint as the strict reading implies: binding, extractive from the
 *   U.S., and benefiting foreign reserve holders.
 *
 * KEY AGENTS:
 *   - U.S. monetary policy authority (Federal Reserve, Treasury) — victim of binding obligation; constrained issuer
 *   - Creditor nations (France, Germany, Belgium, Netherlands, and others) — beneficiaries with enforceable conversion rights
 *   - U.S. domestic creditors (corporations, labor, Keynesian economists) — payers bearing the cost of constrained monetary policy
 *   - U.S. Congress — agenda-setter that codified convertibility but loses policy autonomy
 *   - International monetary order — beneficiary of the anchor mechanism, though distributed asymmetrically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.68).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.52).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Gold Convertibility as Binding Legal Obligation").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '1e419efc-2810-426d-82b2-06dc6ef87477').
narrative_ontology:cs_kernel_codification('1e419efc-2810-426d-82b2-06dc6ef87477', fixed_text).
narrative_ontology:cs_authority_grounding('1e419efc-2810-426d-82b2-06dc6ef87477', extraction).
narrative_ontology:cs_interpretation_layer_present('1e419efc-2810-426d-82b2-06dc6ef87477').
narrative_ontology:cs_reading_relation('1e419efc-2810-426d-82b2-06dc6ef87477', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('1e419efc-2810-426d-82b2-06dc6ef87477', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('1e419efc-2810-426d-82b2-06dc6ef87477', foundational, convertibility_is_legally_binding).
narrative_ontology:cs_axiom_status(convertibility_is_legally_binding, holdable).
narrative_ontology:cs_axiom_grounding('1e419efc-2810-426d-82b2-06dc6ef87477', convertibility_is_legally_binding, conventional).
narrative_ontology:cs_axiom('1e419efc-2810-426d-82b2-06dc6ef87477', foundational, us_domestic_policy_subordinate_to_gold_discipline).
narrative_ontology:cs_axiom_status(us_domestic_policy_subordinate_to_gold_discipline, holdable).
narrative_ontology:cs_axiom_grounding('1e419efc-2810-426d-82b2-06dc6ef87477', us_domestic_policy_subordinate_to_gold_discipline, deontological).
narrative_ontology:cs_reference_frame('1e419efc-2810-426d-82b2-06dc6ef87477', monetary_convertibility_discipline).
narrative_ontology:cs_drift_state('1e419efc-2810-426d-82b2-06dc6ef87477', gold_reserve_depletion_and_redemption_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1e419efc-2810-426d-82b2-06dc6ef87477', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, gold_reserve_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_policy_authority).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1944, honeymoon period) to 0.68 (1971, breakdown imminent) as gold reserves decline and redemption pressure mounts. In 1944, the U.S. held 70% of world gold and convertibility felt costless. By 1960, the London Gold Pool crisis and the Triffin problem show that convertibility is now binding—foreign creditors are demanding gold, the U.S. cannot expand money supply without triggering redemptions, and domestic policy is subordinated to external discipline. Theater ratio rises from near-zero (the constraint is functionally real) to 0.29 by 1971 (growing rhetorical emphasis on 'defending the dollar' masks the underlying loss of control). Suppression rises from 0.25 to 0.52 as the Fed and Treasury must actively defend convertibility—quarterly gold losses, monetary tightening in downturns, coordination with foreign banks to manage redemption demand. All measurements share one time grid (interval start and end = 1944 and 1971) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental divergence is between the issuer seat (U.S.) and the creditor seat (France, Germany, etc.). From the U.S. perspective, the constraint begins as a reasonable coordination mechanism (1944) but becomes progressively extractive as gold depletes and redemptions mount—by 1968, the London Gold Pool is dissolving and the U.S. is losing control. From the creditor perspective, the constraint is precisely what was negotiated: a discipline mechanism that forces the U.S. to run surpluses and maintain reserves, which benefits creditors who can redeem at will. Congress experiences the constraint as binding (domestically, labor and business lobby for expansion; internationally, allies enforce convertibility). The divergence is not about facts but about the structure of obligation: the strict reading DEFINES the constraint as binding precisely because it denies the issuer an exit from unilateral policy change. If the U.S. breaks the obligation, it breaches a treaty; if it honors it, it loses policy autonomy. This is the structural asymmetry the engine should detect.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. is structurally the victim: it authored the constraint (as agenda-setter in 1944), but by the strict reading, that authorship created a binding obligation from which exit is costly. U.S. monetary authorities lose directionality as the constraint operates—d moves toward the target end (1.0) as gold depletes and redemptions mount. Creditor nations are structurally beneficiaries: they hold conversion rights (d near 0.0), pay nothing, and arbitrage at the margin. U.S. domestic constituencies (workers, businesses) are secondary victims—they bear the cost of constrained monetary policy. The international monetary order itself is a beneficiary of the anchor, but that benefit is mediated through the discipline mechanism that harms the U.S. This structure produces seat divergence: from the U.S. policy authority's seat, the constraint is increasingly extractive and binding; from the creditor nations' seat, it is a coordination mechanism that also happens to subsidize them. The analytical observer seat sees both—the strict reading transforms a coordination problem into an asymmetric extraction mechanism as the U.S. gold stock declines.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve into mandatrophy—the founding problem (preventing 1930s-style competitive devaluation) remains substantively live throughout 1944–1971, which is why the constraint persists in force. The issue is not that the mandate has atrophied, but that the structural conditions that make the constraint binding have changed. In 1944, the U.S. holds sufficient gold that convertibility is costless theater. By 1968, the gold stock has halved, foreign demands for redemption are rising, and the constraint is actively binding. The claim/metric gap is deliberate: the CLAIM is tangled_rope (the constraint does coordinate the international system AND extract from the U.S. domestic policy space—it is both), while the METRICS show progressively higher extractiveness and suppression as the gold stock depletes. The engine should recognize this as a genuine tangled_rope undergoing metric drift toward snare-like conditions (by 1971, the U.S. is contemplating unilateral break). The constraint has not resolved into pure theater—it remains actively enforced by the threat of gold redemptions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strict_vs_flexible_reading_boundary,
    'Is Article IV convertibility a strict binding obligation, or a conditional obligation subordinate to U.S. domestic economic stability?',
    'Documentary analysis of Bretton Woods negotiating record, post-war policy statements from U.S. officials, and legal opinions from the State Department and Treasury. Comparison of actual policy decisions in crises (1957–58 recession, 1967 devaluation crisis, 1968 gold drain) against what each reading would permit.',
    'If strict reading prevails, the constraint extracts from the U.S. domestic policy space and benefits creditors with enforceable claims. If flexible reading prevails, the constraint is a coordination mechanism the U.S. can modify unilaterally in extremis, and extraction is lower. The readings produce different terminal classifications: strict → tangled_rope/snare; flexible → rope/scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_vs_flexible_reading_boundary, conceptual, 'Whether the commitment is binding or conditional, determining the constraint''s classification.').

omega_variable(
    implicit_vs_explicit_beneficiary_intent,
    'Did the U.S. authoring of convertibility at Bretton Woods intend to benefit creditor nations with enforceable claims, or intend a symmetrical coordination mechanism?',
    'Primary source analysis: private U.S. negotiating briefs, Henry Dexter White''s papers, Federal Reserve internal memoranda from 1944–1945 discussing the anticipated distribution of gains from convertibility. Comparison with creditor nations'' stated objectives (France''s preference for a gold standard, UK''s concern about sterling balances).',
    'If the U.S. intended to benefit creditors (benign asymmetry from the U.S. perspective, a cost of leadership), the constraint is intentionally extractive and the high extractiveness score is appropriate. If the U.S. intended symmetry and creditors extracted unintended benefits, the false_summit_mountain signature might apply (apparent coordination, hidden extraction). This omega addresses whether beneficiary presence on a tangled_rope is intentional or emergent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_vs_explicit_beneficiary_intent, empirical, 'Whether creditor nations'' benefit from convertibility was intentional U.S. strategy or an unintended consequence of the coordination design.').

omega_variable(
    gold_reserve_sufficiency_boundary,
    'What gold reserve level is required to maintain credible convertibility at $35/oz, and at what reserve level does the constraint become operationally unsustainable?',
    'Economic calculation: foreign dollar holdings vs. U.S. gold stock over time. Analysis of the Triffin problem—the mathematical incompatibility of fixed rates, growth in dollar-denominated claims, and finite gold stock. Observation of when redemption demand begins to exceed supply (London Gold Pool crisis, 1968).',
    'If a critical reserve level exists below which convertibility is unsustainable, then the constraint''s character changes from binding-legal to binding-empirical: it binds as long as gold lasts, then breaks by operation of physics, not by policy choice. This would suggest the constraint is better modeled as a finite-lived obligation (scaffold-like) rather than a permanent extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gold_reserve_sufficiency_boundary, empirical, 'The physical and mathematical limits of Bretton Woods convertibility.').

omega_variable(
    reading_foreclosure_by_1971_breakdown,
    'Does the actual breakdown of convertibility in August 1971 (Nixon Shock) foreclose the strict_reading within its own commitment framework?',
    'Post-hoc analysis: if the U.S. can unilaterally exit the obligation and survive the reputational cost, does that imply the strict reading was never as binding as claimed? Or does the cost of exit (currency devaluation, loss of U.S. credibility, inflation) vindicate the strict reading''s claim that exit is trapped?',
    'If exit is easy and costless, the reading is not strict—the U.S. could have left anytime. If exit is costly but possible, the reading describes a snare, not a binding obligation. If exit is blocked (which history suggests it was not), the reading is correct and the constraint is binding. The omega addresses whether ''strict binding obligation'' is descriptively accurate or aspirational cover for a constraint the U.S. could break but chose not to, until it chose to.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_by_1971_breakdown, conceptual, 'Whether the strict reading''s characterization of bindingness survives the historical fact that the U.S. broke the obligation in 1971.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(doll_tr_t1952, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1952, 0.08).
narrative_ontology:measurement(doll_tr_t1960, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1968, 0.27).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.29).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement(doll_be_t1952, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1952, 0.45).
narrative_ontology:measurement(doll_be_t1960, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1965, 0.64).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1968, 0.67).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1944, 0.25).
narrative_ontology:measurement(doll_su_t1952, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1952, 0.35).
narrative_ontology:measurement(doll_su_t1960, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1968, 0.51).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__strict_convertibility_reading, 0.12).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% The 'dollar_gold_convertibility' kernel decomposes into three structurally distinct constraints: strict_convertibility_reading (this file) interprets the obligation as binding and extractive from the U.S.; policy_flexible_reading interprets it as conditional and renegotiable; triffin_structural_reading treats the constraint as inherently unstable. Each reading has a different ε, different beneficiary/victim structure, and different persistence logic. The three readings are linked via this field because they share the same legal kernel (Bretton Woods Article IV) but instantiate different constraints from it. Decomposition follows DP-001 (ε-invariance): a single text (Article IV) does not yield a single constraint—the reading determines the constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__strict_convertibility_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
