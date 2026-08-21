% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__punitive_liability_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Reparations: Punitive Liability Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'punitive liability' reading of
 *   the Versailles Reparations Clauses. This reading asserts that Germany
 *   bore unique moral and financial responsibility for the total costs of
 *   World War I, and that Article 231 of the Treaty of Versailles provided a
 *   legitimate legal basis for quasi-unlimited reparations claims. This
 *   perspective emphasizes the justice of compensation for Allied losses and
 *   the necessity of holding Germany accountable, subordinating German fiscal
 *   sovereignty to external claims.
 *
 * KEY AGENTS:
 *   - allied_creditor_states: Primary agenda_setter (institutional/arbitrage) — imposed and enforced the claims.
 *   - german_government: Primary payer (institutional/trapped) — bore the direct burden of collection and transfer.
 *   - german_workers_taxpayers: Primary payer (powerless/trapped) — bore the ultimate economic costs.
 *   - international_financial_markets: Secondary beneficiary (institutional/mobile) — profited from facilitating loans.
 *   - international_legal_scholars: Analytical observer (analytical/analytical) — analyzed the legal and economic implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.85).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.9).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Reparations: Punitive Liability Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, 'c0b4851f-162a-4002-8393-ee639cd09e0f').
narrative_ontology:cs_kernel_codification('c0b4851f-162a-4002-8393-ee639cd09e0f', fixed_text).
narrative_ontology:cs_authority_grounding('c0b4851f-162a-4002-8393-ee639cd09e0f', extraction).
narrative_ontology:cs_interpretation_layer_present('c0b4851f-162a-4002-8393-ee639cd09e0f').
narrative_ontology:cs_reading_relation('c0b4851f-162a-4002-8393-ee639cd09e0f', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0b4851f-162a-4002-8393-ee639cd09e0f', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('c0b4851f-162a-4002-8393-ee639cd09e0f', foundational, german_sole_war_guilt).
narrative_ontology:cs_axiom_status(german_sole_war_guilt, holdable).
narrative_ontology:cs_axiom_grounding('c0b4851f-162a-4002-8393-ee639cd09e0f', german_sole_war_guilt, conventional).
narrative_ontology:cs_axiom('c0b4851f-162a-4002-8393-ee639cd09e0f', foundational, unlimited_reparations_justified).
narrative_ontology:cs_axiom_status(unlimited_reparations_justified, holdable).
narrative_ontology:cs_axiom_grounding('c0b4851f-162a-4002-8393-ee639cd09e0f', unlimited_reparations_justified, instrumental).
narrative_ontology:cs_reference_frame('c0b4851f-162a-4002-8393-ee639cd09e0f', allied_punitive_justice).
narrative_ontology:cs_drift_state('c0b4851f-162a-4002-8393-ee639cd09e0f', interwar_period_end, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('c0b4851f-162a-4002-8393-ee639cd09e0f', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, international_financial_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The victorious powers (primarily France, Britain, Belgium) who imposed the Treaty of Versailles and sought to collect reparations. They set the terms, established enforcement mechanisms, and benefited directly from the financial transfers. Their position was one of legal and military superiority, allowing them to dictate terms and suppress alternatives.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, agenda_setter,
    institutional, generational, arbitrage, global).

% The Weimar Republic government, legally bound by the treaty to collect and transfer reparations. Faced immense internal political and economic pressure due to the burden, leading to hyperinflation and instability. Exit options were limited to default (with severe consequences like occupation) or negotiation under duress.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_government, payer,
    institutional, immediate, trapped, national).

% The ultimate bearers of the reparations burden through taxation, inflation, and reduced public services. Had no direct voice in the treaty negotiations or enforcement, and their economic well-being was directly subordinated to external claims. Their only 'exit' was emigration or political upheaval.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers, payer,
    powerless, biographical, trapped, national).

% Banks and investors who facilitated loans to Germany (e.g., Dawes and Young Plans) to enable reparations payments. They profited from interest on these loans, creating a circular flow of capital that benefited financial intermediaries while keeping Germany indebted.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, international_financial_markets, beneficiary,
    institutional, biographical, mobile, global).

% Academics and legal experts who analyzed the legality, morality, and economic impact of Article 231 and the reparations regime. Their analyses often diverged, with some supporting the Allied legal position and others critiquing its punitive nature and economic consequences.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a legal and financial framework for post-World War I peace, assigning responsibility for war damages and coordinating the transfer of compensation from Germany to the Allied powers.
% TRANSFER_FUNCTION: Transferred substantial financial resources, industrial assets, and territorial concessions from Germany to the Allied creditor states, primarily from the German national economy and its citizens.
% ABSENT_VOICES: The German delegation was largely excluded from the negotiation of the treaty terms, presenting a 'Diktat' rather than a negotiated settlement. German public opinion, which widely viewed the treaty as unjust, was also absent from the decision-making process.
% DISAPPEARANCE_RATIONALE: If the punitive reparations clauses had vanished overnight, the interwar global financial system would have been fundamentally different, Germany's economic recovery would have been significantly faster, and the political instability that fueled extremist movements in Germany would have been substantially mitigated, altering the course of 20th-century history.
% FOUNDING_PROBLEM: To hold Germany accountable for the immense human and material costs of World War I and to provide compensation to the victorious Allied nations for damages and war expenses.
% FOUNDING_PROBLEM_CORROBORATION: Allied governments and their populations consistently attested to the problem's live status and Germany's responsibility. However, German historians, economists (e.g., John Maynard Keynes), and some international observers provided extensive counter-arguments, asserting that the problem was either exaggerated, economically unfeasible, or politically unjust, leading to a contested status.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.85) because the claims were vast, largely decoupled from Germany's actual capacity to pay without severe economic distress, and designed to transfer significant wealth. `suppression` is very high (0.90) as Germany had virtually no exit options from the treaty's obligations, facing military occupation or further sanctions for non-compliance. `theater_ratio` is low (0.15) because the reparations regime was actively and coercively enforced, with real economic and political consequences, rather than being merely performative. `accessibility_collapse` is high (0.80) as alternatives to payment were severely restricted, and `resistance` is high (0.70) reflecting Germany's persistent, though largely unsuccessful, efforts to renegotiate or evade the terms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Allied creditor states (agenda_setter/beneficiary), the reparations were a just and necessary mechanism for accountability and compensation. From the German government and its citizens (payer/victim), the same structure was experienced as an unjust, punitive, and economically crippling imposition. The engine's per-seat classification will reflect this divergence, with the Allied seat computing a more 'rope-like' or 'tangled_rope' experience (coordination with benefit) and the German seats computing a 'snare' experience (pure extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   The Allied creditor states are clear beneficiaries, receiving the transfers and controlling the enforcement, placing their directionality near 0.0. The German government and its citizens are clear targets, bearing the costs with severely constrained exit options, placing their directionality near 1.0. International financial markets benefited from the intermediation of funds, placing them closer to the beneficiary end. International legal scholars, as analytical observers, have a directionality near 0.5, reflecting their detached analytical stance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reparations_economic_impact_ambiguity,
    'To what extent were the reparations truly crippling to the German economy, versus being a manageable burden exacerbated by political choices and the global economic climate?',
    'Counterfactual economic modeling comparing Germany''s actual economic trajectory with scenarios where reparations were lower or absent, alongside detailed historical analysis of German fiscal policy and international capital flows.',
    'If the impact was less severe than claimed, it would weaken the ''snare'' classification by reducing the effective extractiveness and suppression, potentially shifting it towards a ''tangled_rope'' or even ''rope'' from an economic perspective. If the impact was truly crippling, it reinforces the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reparations_economic_impact_ambiguity, empirical, 'Ambiguity regarding the actual economic severity of reparations on Germany.').

omega_variable(
    moral_responsibility_scope_ambiguity,
    'Was Article 231 primarily a legal formality to establish a basis for claims, or did it constitute a definitive moral judgment of Germany''s sole war guilt?',
    'Analysis of primary diplomatic correspondence, legal interpretations by the drafters, and contemporary public discourse from both Allied and German perspectives to discern the intended and perceived meaning of the clause.',
    'If primarily a legal formality, the moral justification for ''punitive'' reparations weakens, potentially reducing the perceived legitimacy of the extraction. If a definitive moral judgment, it strengthens the moral grounding of the punitive claims, even if the economic impact is debated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_responsibility_scope_ambiguity, conceptual, 'Ambiguity over the legal vs. moral interpretation of Article 231.').

omega_variable(
    treaty_legitimacy_ambiguity,
    'Given the circumstances of its imposition (a ''Diktat'' without German negotiation), was the Treaty of Versailles, and thus the reparations clauses, a legitimate international agreement?',
    'A conceptual analysis of international law principles regarding treaties imposed under duress, and a historical examination of the power dynamics at the Paris Peace Conference.',
    'If the treaty is deemed illegitimate, the entire basis for the reparations claims collapses, reclassifying the constraint as a pure ''snare'' or even a ''piton'' (if its persistence became purely inertial). If deemed legitimate despite the circumstances, the ''snare'' classification remains, but its legal foundation is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_legitimacy_ambiguity, conceptual, 'Ambiguity regarding the overall legitimacy of the Treaty of Versailles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement(vers_tr_t1922, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1922, 0.12).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1925, 0.15).
narrative_ontology:measurement(vers_tr_t1928, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1928, 0.18).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1932, 0.15).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.75).
narrative_ontology:measurement(vers_be_t1922, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1922, 0.8).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1925, 0.83).
narrative_ontology:measurement(vers_be_t1928, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1928, 0.86).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1932, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.8).
narrative_ontology:measurement(vers_su_t1922, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1922, 0.85).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1925, 0.88).
narrative_ontology:measurement(vers_su_t1928, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1928, 0.9).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1932, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
