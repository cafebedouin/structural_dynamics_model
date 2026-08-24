% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Gold Standard to Fiat Transition: Automatic Constraint Reading
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   The gold-to-fiat transition eliminated the automatic physical constraint
 *   that gold reserves imposed on money creation. Under gold, a central bank
 *   could not expand its balance sheet beyond its gold cover ratio without
 *   triggering redemption demands and gold outflows — a hard, self-enforcing
 *   limit. The transition replaced this with discretionary central bank
 *   authority: policy rates, open market operations, and
 *   lender-of-last-resort facilities became the binding constraints. This
 *   reading frames the transition as the replacement of a material constraint
 *   with an institutional one. The constraint weakened (gold's automatic
 *   discipline is gone) but remained institutional (central bank mandates,
 *   inflation targets, and international coordination provide a new, softer
 *   discipline). Extraction is high because monetary authorities gained
 *   seigniorage and crisis financing power; suppression is moderate because
 *   the new system relies on credibility and coordination rather than
 *   physical enforcement; resistance is high from creditors who lost their
 *   automatic veto.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.72).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.68).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Gold Standard to Fiat Transition: Automatic Constraint Reading").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '20b002d2-c759-45da-89b7-94d00ec82562').
narrative_ontology:cs_kernel_codification('20b002d2-c759-45da-89b7-94d00ec82562', formalized).
narrative_ontology:cs_authority_grounding('20b002d2-c759-45da-89b7-94d00ec82562', extraction).
narrative_ontology:cs_interpretation_layer_present('20b002d2-c759-45da-89b7-94d00ec82562').
narrative_ontology:cs_reading_relation('20b002d2-c759-45da-89b7-94d00ec82562', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_reading_relation('20b002d2-c759-45da-89b7-94d00ec82562', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('20b002d2-c759-45da-89b7-94d00ec82562', foundational, automatic_constraint_replacement_is_causal_node).
narrative_ontology:cs_axiom_status(automatic_constraint_replacement_is_causal_node, holdable).
narrative_ontology:cs_axiom_grounding('20b002d2-c759-45da-89b7-94d00ec82562', automatic_constraint_replacement_is_causal_node, empirically_contingent).
narrative_ontology:cs_axiom('20b002d2-c759-45da-89b7-94d00ec82562', secondary, institutional_discipline_substitutes_for_material_discipline).
narrative_ontology:cs_axiom_status(institutional_discipline_substitutes_for_material_discipline, holdable).
narrative_ontology:cs_axiom_grounding('20b002d2-c759-45da-89b7-94d00ec82562', institutional_discipline_substitutes_for_material_discipline, conventional).
narrative_ontology:cs_reference_frame('20b002d2-c759-45da-89b7-94d00ec82562', gold_standard_automatic_discipline).
narrative_ontology:cs_drift_state('20b002d2-c759-45da-89b7-94d00ec82562', post_1971_fiat_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('20b002d2-c759-45da-89b7-94d00ec82562', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_nations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, commercial_banks).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_nations).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, lender_of_last_resort_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks and treasury departments gained discretionary control over money creation, interest rates, and exchange rate policy after the gold constraint was removed. They administer the fiat system, set policy frameworks, and act as lenders of last resort. Their discretion is the core institutional gain from the transition.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Bondholders, banks, and surplus-nation savers lost the automatic redemption threat that disciplined debtor nations under gold. Their claims are now subject to inflation risk, financial repression, and unilateral restructuring. Exit options are limited to shifting asset composition or jurisdictions, but the fiat system is global and inescapable.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    organized, generational, constrained, global).

% Governments with fiscal deficits gained flexibility to finance spending without gold convertibility constraints. They benefit from seigniorage and the ability to inflate away domestic-currency debt. However, they also face market discipline through capital flight and currency crises, making them secondary payers when confidence collapses.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_nations, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, debtor_nations, payer).

% Banks operate in a system where central bank backstops and elastic reserves enable credit expansion without gold reserve ratios. They profit from maturity transformation and payment system rents. Their exit is mobile — they can relocate or restructure — but the fiat framework is the substrate of their business model.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, commercial_banks, beneficiary,
    powerful, biographical, mobile, global).

% IMF, BIS, and later G20 monitor the fiat system's stability, coordinate crisis responses, and publish surveillance reports. They do not set national monetary policy but shape the institutional architecture within which monetary authorities operate.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, international_organizations, observer,
    institutional, generational, analytical, global).

% Economists, policymakers, and political movements arguing for a return to metallic or rules-based monetary constraints. They are structurally excluded from operational decision-making; their proposals are treated as heterodox. Their exit is trapped — they cannot implement their preferred system without a systemic breakdown.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_standard_advocates, excluded,
    moderate, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a globally interoperable monetary framework that enables cross-border trade, credit, and investment without requiring physical gold flows for settlement. Solves the coordination problem of elastic money supply during crises, wars, and asymmetric shocks.
% TRANSFER_FUNCTION: Moves seigniorage income and crisis-time fiscal flexibility from the creditor class (who lose automatic protection) to monetary authorities and debtor nations (who gain discretionary issuance and lender-of-last-resort capacity). The transfer is continuous in peacetime (inflation tax) and acute in crises (financial repression, restructuring).
% ABSENT_VOICES: Gold standard advocates, surplus-nation creditor interests, and populations in hyperinflation episodes who bear the tail risk of discretionary policy. They are excluded from the institutional architecture that replaced gold — central bank mandates, IMF conditionality, and G20 coordination forums do not include hard-constraint advocates as veto players.
% DISAPPEARANCE_RATIONALE: If discretionary fiat authority vanished overnight, the global payment system would cease to clear at current volumes. A new anchor (gold, crypto, commodity basket, or multilateral clearing union) would have to be negotiated. Trade finance, sovereign debt markets, and derivative contracts denominated in fiat currencies would require massive restructuring. The world monetary order depends on this constraint's continued operation.
% FOUNDING_PROBLEM: The gold standard's inelasticity during WWI, the Great Depression, and WWII prevented governments from financing existential emergencies and forced deflationary adjustments that deepened crises. The founding problem was the need for a monetary system that could expand elastically during systemic emergencies without collapsing into competitive devaluations or trade war.
% FOUNDING_PROBLEM_CORROBORATION: Central bank historians (Eichengreen, Bordo) attest the founding problem was real and the transition solved it. Monetary historians of the Austrian and hard-money traditions (Rothbard, White, Selgin) attest the founding problem was overstated and the transition created worse instabilities. Independent economic historians (Kindleberger, Tooze) document both the emergency rationale and the subsequent drift toward discretionary excess.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.25 to 0.72 across the interval as the gold anchor weakened (1914 suspension, 1931 UK exit, 1944 Bretton Woods, 1971 Nixon Shock) and discretionary policy expanded. The theater ratio rises from 0.10 to 0.38 because early fiat regimes maintained gold convertibility rhetoric while operating discretionarily (theater), and later inflation targeting frameworks perform transparency while retaining full discretion. Suppression requirement rises from 0.30 to 0.68 because maintaining fiat credibility requires ever-more-sophisticated institutional machinery (independence statutes, forward guidance, swap lines, macroprudential tools). Accessibility collapse at 0.55 reflects that alternatives (gold, crypto, commodity standards) exist conceptually but are structurally inaccessible at global scale. Resistance at 0.71 reflects persistent creditor and hard-money opposition across the period.
 *
 * PERSPECTIVAL GAP:
 *   From the monetary authority seat, the fiat system is a genuine coordination achievement: elastic money, lender of last resort, stable inflation. From the creditor seat, it is an extraction mechanism: their claims are inflated away, their discipline veto removed. From the debtor nation seat, it is a fragile freedom: they can finance deficits but face sudden stops. The engine computes this seat divergence from the structural data — the claimed type (tangled_rope) captures the hybrid reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities are the primary beneficiaries (d near 0.0): they gained discretionary issuance, seigniorage, and crisis tools. Creditor class are the primary victims (d near 1.0): they lost automatic redemption protection and face inflation/financial repression risk. Debtor nations are secondary beneficiaries (d ~0.3) but also secondary payers when confidence fails (d ~0.7 in crises). Commercial banks are beneficiaries (d ~0.2) from elastic reserves and backstops. International organizations are analytical observers (d=0.5). Gold standard advocates are excluded (trapped, no directionality in the operational system). The engine derives these from the declared beneficiary/victim structure and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (elastic money for emergencies) was real and live through WWII and early Bretton Woods. By the 1970s, the emergency rationale had faded but the discretionary architecture persisted and expanded — a classic mandatrophy pattern. The constraint now extracts substantially (seigniorage, financial repression, crisis bailouts) while its original coordination justification has atrophied. The theater ratio rise tracks this: more performance (transparency, forward guidance) substitutes for the lost automatic discipline. The classification as tangled_rope (not snare) reflects that genuine coordination (payment system stability, crisis backstop) still operates alongside extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the automatic_constraint_reading a distinct structural claim about the gold-fiat transition, or a measurement-basis variant of the same underlying constraint?',
    'Compare epsilon and beneficiary/victim structures across the three declared readings. If epsilon differs substantially (this reading: high; creditor_discipline: moderate; composite: distributed), they are distinct constraints per epsilon-invariance.',
    'If distinct, each reading gets its own constraint story with independent classification. If variant, they collapse into one story with measurement-basis ambiguity — but the framework forbids measurement-basis parameters, so decomposition is required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading instantiates a separate constraint from its siblings per the epsilon-invariance principle.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression in the fiat system structural (capital controls, legal tender laws, regulatory barriers to alternatives) or internalized (creditors accept fiat because no viable alternative exists, belief in central bank credibility)?',
    'Post-1971 trajectory: if capital controls were liberalized (1980s-90s) but suppression persisted, internalized component is significant. Measure alternative-currency adoption rates and creditor portfolio behavior during inflation episodes.',
    'If substantially internalized, effective suppression exceeds the structural measure — creditors carry the constraint''s discipline internally. This would raise the constraint''s effective extraction for the creditor seat beyond what structural suppression alone predicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the fiat monetary system.').

omega_variable(
    naturalness_of_institutional_constraint,
    'Is the post-gold institutional constraint (central bank discretion + credibility) a genuine coordination solution that would persist without enforcement, or a constructed arrangement that requires active maintenance?',
    'Counterfactual: if central banks announced pure discretion with no inflation targets, no swap lines, no lender-of-last-resort commitment — would the system hold? Historical episodes (1970s stagflation, 2008 crisis, 2020 pandemic) test the coordination core under stress.',
    'If the coordination core is genuine, the constraint is a true tangled_rope (coordination + extraction). If the coordination story is cover for pure discretion, it trends toward snare. The mandated theater_ratio rise suggests the coordination core is real but eroding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_institutional_constraint, conceptual, 'Whether the institutional replacement constraint has genuine coordination function or is performative cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gft_acr_tr_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gft_acr_tr_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(gft_acr_tr_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(gft_acr_tr_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(gft_acr_tr_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(gft_acr_tr_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(gft_acr_tr_t60, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 60, 0.38).

% Extraction over time
narrative_ontology:measurement(gft_acr_be_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gft_acr_be_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(gft_acr_be_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(gft_acr_be_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(gft_acr_be_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(gft_acr_be_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 50, 0.69).
narrative_ontology:measurement(gft_acr_be_t60, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gft_acr_su_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gft_acr_su_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(gft_acr_su_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(gft_acr_su_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(gft_acr_su_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(gft_acr_su_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(gft_acr_su_t60, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.12).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the gold-fiat transition kernel by isolating the automatic-vs-discretionary mechanism. The creditor_discipline_reading isolates the creditor-veto-loss mechanism. The composite_overdetermination_reading isolates the multi-causal convergence mechanism. All three share the same historical interval but author different epsilon values and different beneficiary/victim structures, confirming they are distinct constraints per epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__automatic_constraint_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
